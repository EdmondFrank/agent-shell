;;; agent-shell-fork-skill.el --- Complete a command name after a colon  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Edmond Frank

;; URL: https://github.com/EdmondFrank/agent-shell

;; This package is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3, or (at your option)
;; any later version.

;; This package is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this package.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; Fork feature: complete a command whose name carries a payload after a
;; colon -- AiderDesk's `/skill:<name>' trigger, for instance.
;;
;; Upstream completes one token at the very start of the input using the
;; word class "[:alnum:]_-", which has no colon in it, and it only fires
;; when `/` sits immediately before that word.  `/skill' therefore
;; completes to `skill:pdf' -- but as soon as the colon is typed the
;; trigger character is no longer next to the word, the bounds stop
;; matching, and nothing completes at all: the payload can only be typed
;; out in full, from memory.
;;
;; This module keeps the upstream region -- the command name starts right
;; after the `/` and ends at point -- and only narrows the candidates once
;; a colon is present: `/skill:p' then offers the advertised commands that
;; begin with `skill:', annotated with the description the agent sent for
;; each.  Because the region never changes shape at the colon, the
;; completion session lives across it: a popup opened by typing `/` keeps
;; filtering as `:pdf' is typed, and a bare `/skill:' pops the list.
;;
;; The region is deliberately not narrowed to the payload alone.  A UI
;; filters candidates by the text between the region bounds, so a region
;; of zero width right after the colon (`/skill:' with nothing typed yet)
;; has no prefix to filter by: Corfu quits such a session on the next
;; continuation check and does not pop again while one is active, which is
;; why typing the colon used to end completion instead of extending it.
;; Covering the whole `<prefix>:<payload>' token keeps the region non-empty
;; from the first character after the colon onwards, with the same start
;; before and after it.
;;
;; Nothing here knows what a skill is: the prefix is whatever the agent
;; advertised, so an agent with no `<prefix>:name' commands sees no
;; change at all.
;;
;; Upstream seams used, no upstream file edited:
;;
;;   agent-shell--command-completion-at-point  advice, :around
;;   agent-shell--trigger-completion-at-point  advice, :around
;;   agent-shell-completion--input-start       read, where input begins
;;   agent-shell-completion--source-buffer     read, whose session to ask
;;
;; Both wrappers call through to upstream unless the input is a
;; `/<prefix>:<partial>' token, so stock completion is untouched.

;;; Code:

(require 'cl-lib)
(require 'map)
(require 'subr-x)

(require 'agent-shell)
(require 'agent-shell-completion)

;; Upstream helpers this module reads.  Declared so the byte-compiler
;; does not warn, and so a rename degrades to stock completion rather
;; than an error (both readers are guarded with `fboundp').
(declare-function agent-shell-completion--input-start "agent-shell-completion" ())
(declare-function agent-shell-completion--source-buffer "agent-shell-completion" ())

(defvar agent-shell--state)
(defvar agent-shell-command-completion-enabled)

(defcustom agent-shell-fork-skill-completion-enabled t
  "Non-nil completes the name after a `prefix:' command token.

With this off, `/skill:p' falls back to upstream's single-token
completion, which cannot split a name at the colon."
  :type 'boolean
  :group 'agent-shell)

(defconst agent-shell-fork-skill-name-chars "[:alnum:]_-"
  "Characters a command payload may hold after the colon.

Mirrors the word class upstream uses for command names: a name that
survives `POST /commands' advertising is expected to fit it.")

(defun agent-shell-fork-skill--source-buffer ()
  "Return the buffer holding the session whose commands to complete.

Upstream keeps the session in the shell buffer and reads it from there
even when completion runs in the minibuffer reading a queued prompt;
`agent-shell-completion--source-buffer' answers which one that is."
  (or (and (fboundp 'agent-shell-completion--source-buffer)
           (ignore-errors (agent-shell-completion--source-buffer)))
      (and (fboundp 'agent-shell--shell-buffer)
           (ignore-errors (agent-shell--shell-buffer :no-error t :no-create t)))))

(defun agent-shell-fork-skill--commands ()
  "Return the session's advertised commands, or nil when there are none."
  (when-let* ((buffer (agent-shell-fork-skill--source-buffer))
              ((buffer-live-p buffer)))
    (ignore-errors
      (map-elt (buffer-local-value 'agent-shell--state buffer) :available-commands))))

(defun agent-shell-fork-skill--token ()
  "Return (PREFIX . (START . END)) for the `/<prefix>:<name>' token at point.

START and END delimit the command name, `/` excluded, exactly as
upstream delimits a command without a payload -- the colon is inside the
region, so it is filtered by like any other character and the region is
never empty.  Nil unless the token begins the input being composed, the
same rule upstream applies to the command name itself, since a slash
mid-message is plain text.

With point in `/skill:p', for example, the result is (\"skill\" . (2 . 9))."
  (when (fboundp 'agent-shell-completion--input-start)
    (when-let* ((input-start (save-excursion
                               (ignore-errors (agent-shell-completion--input-start))))
                ((integer-or-marker-p input-start))
                ((eq (char-after input-start) ?/)))
      (let* ((end (save-excursion
                    (skip-chars-forward agent-shell-fork-skill-name-chars)
                    (point)))
             (text (and (> end input-start)
                        (buffer-substring-no-properties input-start end)))
             (match (and text
                         (string-match "\\`/\\([[:alnum:]_-]+\\):\\([[:alnum:]_-]*\\)\\'"
                                       text))))
        (when match
          (cons (match-string 1 text) (cons (1+ input-start) end)))))))

(defun agent-shell-fork-skill--candidates (prefix commands)
  "Return (NAME . DESCRIPTION) for every COMMAND under PREFIX's colon.

NAME is the command name as advertised (`skill:pdf'), not its payload:
the completed region covers `<prefix>:<payload>', so the whole name is
what completion inserts.  Names are deduplicated, keeping the first
description advertised for one, and a command named exactly `<prefix>:'
is skipped: it carries no payload to complete.

COMMANDS is whatever the session stored: the ACP `session/update'
payload arrives as a vector, so iterate with `mapc' rather than a
list-only `dolist'."
  (let ((marker (concat prefix ":"))
        (seen nil)
        (result nil))
    (mapc (lambda (command)
            (let ((name (map-elt command 'name)))
              (when (and (stringp name)
                         (string-prefix-p marker name)
                         (> (length name) (length marker))
                         (not (member name seen)))
                (push name seen)
                (push (cons name (map-elt command 'description)) result))))
          commands)
    (nreverse result)))

(defun agent-shell-fork-skill--exit-with-space (_string status)
  "Insert a space after completion, if STATUS is `finished'.

The space ends the token: AiderDesk reads `/skill:<name> <prompt>', so
a typed prompt follows the name directly."
  (when (eq status 'finished)
    (insert " ")))

(defun agent-shell-fork-skill--capf ()
  "Return the capf completing a command payload at point, or nil.

Nil when the completion is switched off, when point is not inside a
`/<prefix>:<partial>' token, or when the agent advertised no command
under that prefix -- in every case upstream's own capf is left to
answer."
  (when agent-shell-fork-skill-completion-enabled
    (when-let* ((token (agent-shell-fork-skill--token))
                (commands (agent-shell-fork-skill--commands)))
      (let* ((prefix (car token))
             (bounds (cdr token))
             (candidates (agent-shell-fork-skill--candidates prefix commands))
             (names (mapcar #'car candidates))
             (descriptions (cl-loop for (name . description) in candidates
                                    when (stringp description)
                                    collect (cons name description))))
        (when names
          (list (car bounds) (cdr bounds) names
                :exclusive t
                ;; Typing the colon alone is enough for Company to pop.
                :company-prefix-length t
                :annotation-function
                (lambda (name)
                  (when-let* ((description (map-elt descriptions name)))
                    (concat "  " description)))
                :company-kind (lambda (_name) 'function)
                :exit-function #'agent-shell-fork-skill--exit-with-space))))))

(defun agent-shell-fork-skill--pop-enabled-p ()
  "Non-nil when completion may pop up on its own.

Follows the user's choice for upstream's `/` completion, since this is
the same picker: a payload popping up while command completion is off
would be surprising."
  (and agent-shell-fork-skill-completion-enabled
       (if (boundp 'agent-shell-command-completion-enabled)
           agent-shell-command-completion-enabled
         t)))

(defun agent-shell-fork-skill--completion-around (fn)
  "Call FN unless point sits in a command payload the fork completes.

FN is `agent-shell--command-completion-at-point'."
  (or (agent-shell-fork-skill--capf)
      (funcall fn)))

(defun agent-shell-fork-skill--trigger-around (fn)
  "Call FN, or pop the command payload when a colon was just typed.

FN is `agent-shell--trigger-completion-at-point', which pops only for
`@' and `/'; the colon is this module's trigger.

An active completion session is left alone: its region already covers
the colon, so it keeps filtering as the colon is typed, and restarting
it would only hide a popup the user is already reading."
  (if (and (eq (char-before) ?:)
           (not (bound-and-true-p completion-in-region-mode))
           (agent-shell-fork-skill--pop-enabled-p)
           (agent-shell-fork-skill--capf))
      (completion-at-point)
    (funcall fn)))

(advice-add 'agent-shell--command-completion-at-point :around
            #'agent-shell-fork-skill--completion-around)
(advice-add 'agent-shell--trigger-completion-at-point :around
            #'agent-shell-fork-skill--trigger-around)

(provide 'agent-shell-fork-skill)

;;; agent-shell-fork-skill.el ends here
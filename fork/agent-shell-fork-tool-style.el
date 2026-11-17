;;; agent-shell-fork-tool-style.el --- Compact tool-call presentation  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Edmond Frank

;; This package is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This package is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; Fork feature: present an AiderDesk tool call the way the agent CLIs do
;; -- the tool, the one argument that says what it is doing, how long it
;; took, and how it ended -- instead of upstream's kind label.
;;
;; AiderDesk reports ACP's catch-all kind `other' for most of its tools,
;; and the bridge's title carries the tool's name and argument.  Upstream
;; renders the kind, so a call reads
;;
;;   ✓ Other  Code_mode: local r = parallel_all({ …
;;
;; with the title truncated mid-token.  ACP has no slot for a duration or
;; a result summary either, so this module derives both in the label:
;;
;;   ✓ Code Mode `emacs__emacs_read_buffer' · 0.4s · 132 lines
;;
;; The reference is the agent TUIs: Claude Code's `⏺ Bash(…) / ⎿ exit 0',
;; Codex's `• Ran … └ exited 0' and OpenCode's `bash ✓ 1.2s' chips.  Each
;; keeps a call to one line with its outcome, and leaves the output to a
;; collapsed body -- which is what the label does here.
;;
;; The label is rendered as markdown, so the argument goes inside a code
;; span: unquoted, `[\"a\"]' loses its brackets to link syntax and
;; `snake_case' italicises.
;;
;; Upstream seams used, no upstream file edited:
;;
;; - `agent-shell--update-fragment' is advised with `:filter-args', the
;;   same seam `agent-shell-fork-params' uses: the labels and the body
;;   are arguments of that call.
;; - `agent-shell--save-tool-call' is advised with `:filter-args' to
;;   record when a call started, which is what the duration needs.
;;
;; Only the AiderDesk agent is affected.  Every wrapper degrades to
;; upstream's rendering when a seam moves: no tool call found means the
;; arguments pass through untouched.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'map)
(require 'seq)

(require 'agent-shell)
(require 'agent-shell-styles)
(require 'agent-shell-elicitation)

(defcustom agent-shell-fork-tool-style-compact-label t
  "Render an AiderDesk tool call as name, argument and outcome.

Nil keeps upstream's label: the tool call kind (`Other' for most
AiderDesk tools) followed by the bridge's title."
  :type 'boolean
  :group 'agent-shell)

(defcustom agent-shell-fork-tool-style-target-max-width 60
  "Columns the argument in a compact tool-call label may take.

The label shares its line with the status icon and the outcome suffix,
so a whole `code_mode' script would push both off screen."
  :type 'integer
  :group 'agent-shell)

(defcustom agent-shell-fork-tool-style-show-duration t
  "Append how long a finished tool call took to its label."
  :type 'boolean
  :group 'agent-shell)

(defcustom agent-shell-fork-tool-style-show-summary t
  "Append how a tool call ended to its label.

The outcome is the call's exit code when its output reports one,
`error' for a failed call, and how many lines it produced otherwise."
  :type 'boolean
  :group 'agent-shell)

(defcustom agent-shell-fork-tool-style-unwrap-json t
  "Show a JSON result envelope as the text it carries.

An older aiderdesk-acp-bridge forwarded a result as
`{\"content\": [{\"type\": \"text\", \"text\": …}], \"isError\": …}',
which upstream renders verbatim.  The current bridge unwraps it; this
handles a session replayed from an older one."
  :type 'boolean
  :group 'agent-shell)

(defconst agent-shell-fork-tool-style--agents '(aiderdesk)
  "Agents whose tool calls get the compact presentation.")

(defconst agent-shell-fork-tool-style--argument-keys
  '(command filePath file_path path query pattern code prompt url)
  "Arguments that say what a call is doing, most specific first.")

(defun agent-shell-fork-tool-style--tool-call (state block-id)
  "Return STATE's tool call BLOCK-ID, or nil.

A block id that is not a tool call (a message, a permission dialog,
a plan) has no entry, which is how the advice tells them apart."
  (when (and state block-id)
    (map-elt (map-elt state :tool-calls) block-id)))

(defun agent-shell-fork-tool-style--applies-p (state block-id)
  "Return the tool call this update should present, or nil.

Nil for another agent's call, for a fragment that is not a tool call,
and for a questionnaire: its input is the questions, which the tool
call's own content already spells out in prose."
  (when-let* ((tool-call (agent-shell-fork-tool-style--tool-call state block-id))
              ((memq (map-nested-elt state '(:agent-config :identifier))
                     agent-shell-fork-tool-style--agents))
              ((not (agent-shell-elicitation--questionnaire-p
                     (map-elt tool-call :raw-input)))))
    tool-call))

(defun agent-shell-fork-tool-style--name (tool-call)
  "Return TOOL-CALL's tool name, e.g. \"Code Mode\".

The name comes from the bridge's title (`Code_mode: <argument>'), which
is the only place AiderDesk names its tool: the ACP kind is `other' for
almost all of them."
  (let* ((title (or (map-elt tool-call :title) ""))
         (prefix (if (string-match "\\`\\([^:[:space:]]+\\):" title)
                     (match-string 1 title)
                   title)))
    (capitalize (string-replace "_" " " (string-trim prefix)))))

(defun agent-shell-fork-tool-style--raw-argument (tool-call)
  "Return the argument of TOOL-CALL that says what it is doing, or nil."
  (let ((raw-input (map-elt tool-call :raw-input)))
    (when (consp raw-input)
      (seq-some (lambda (key)
                  (let ((value (map-elt raw-input key)))
                    (and (stringp value)
                         (not (string-empty-p value))
                         value)))
                agent-shell-fork-tool-style--argument-keys))))

(defun agent-shell-fork-tool-style--target (tool-call)
  "Return TOOL-CALL's target on one line: what it works on.

The title already carries it (`Read: agent-shell.el'), so the title's
remainder is preferred and the raw argument is the fallback."
  (let* ((title (or (map-elt tool-call :title) ""))
         (target (or (when (string-match "\\`[^:[:space:]]+:[[:space:]]*\\(.+\\)\\'" title)
                       (match-string 1 title))
                     (agent-shell-fork-tool-style--raw-argument tool-call)
                     "")))
    (string-trim (replace-regexp-in-string "[[:space:]]+" " " target))))

(defun agent-shell-fork-tool-style--condense (text width)
  "Return TEXT shortened to WIDTH columns, marking the cut with an ellipsis."
  (let ((width (max width 4)))
    (if (<= (string-width text) width)
        text
      (concat (string-trim-right (truncate-string-to-width text (1- width)))
              "…"))))

(defun agent-shell-fork-tool-style--code (text)
  "Return TEXT as a markdown code span.

A label is rendered as markdown, so an unquoted `tools[\"a\"]' loses its
brackets to link syntax.  A code span keeps the text literal; text that
carries the delimiter itself is stripped of the characters markdown
would act on instead."
  (cond ((string-empty-p text) "")
        ((string-search "`" text)
         (replace-regexp-in-string "[`*_~]" "" text))
        (t (format "`%s`" text))))

(defun agent-shell-fork-tool-style--seconds (tool-call)
  "Return how long TOOL-CALL ran, or nil while it still runs."
  (when-let* ((started (map-elt tool-call :started-at))
              ((numberp started))
              ((member (map-elt tool-call :status) '("completed" "failed"))))
    (max 0 (- (float-time) started))))

(defun agent-shell-fork-tool-style--duration (tool-call)
  "Return TOOL-CALL's duration as a compact string, or nil."
  (when-let* ((seconds (agent-shell-fork-tool-style--seconds tool-call))
              ;; A replayed session sends a call's start and end back to
              ;; back; a zero here is the replay, not a fast tool.
              ((>= seconds 0.05)))
    (cond ((< seconds 1) (format "%.1fs" seconds))
          ((< seconds 60) (format "%.0fs" seconds))
          (t (format "%dm %ds" (floor seconds 60) (mod (floor seconds) 60))))))

(defun agent-shell-fork-tool-style--summary (tool-call body)
  "Return how TOOL-CALL ended, from its status and output BODY.

An exit code reported in the output wins: it says more than `error'.
A failed call with no exit code says so, and anything else reports how
many lines it produced."
  (when (stringp body)
    (let ((text (string-trim body)))
      (cond ((string-empty-p text)
             nil)
            ;; AiderDesk reports a command's exit code as a `key: value'
            ;; line of its output.
            ((string-match "^exit_code:[[:space:]]*\\([0-9]+\\)" text)
             (format "exit %s" (match-string 1 text)))
            ((equal (map-elt tool-call :status) "failed")
             "error")
            (t
             (let ((lines (length (split-string text "\n"))))
               (if (= lines 1) "1 line" (format "%d lines" lines))))))))

(defun agent-shell-fork-tool-style--label-left (tool-call)
  "Return TOOL-CALL's status icon, faced like upstream's.

The kind upstream prints beside it is dropped: every AiderDesk tool is
`other', and the tool's own name follows in the right label."
  (when-let* ((status (map-elt tool-call :status))
              (config (agent-shell--status-config status))
              (icon (map-elt config :icon)))
    (propertize icon 'font-lock-face (map-elt config :face))))

(defun agent-shell-fork-tool-style--label-right (tool-call body)
  "Return TOOL-CALL's compact label for a body of BODY."
  (let* ((name (agent-shell-fork-tool-style--name tool-call))
         (target (agent-shell-fork-tool-style--condense
                  (agent-shell-fork-tool-style--target tool-call)
                  agent-shell-fork-tool-style-target-max-width))
         (outcome (delq nil (list (and agent-shell-fork-tool-style-show-duration
                                       (agent-shell-fork-tool-style--duration tool-call))
                                  (and agent-shell-fork-tool-style-show-summary
                                       (agent-shell-fork-tool-style--summary tool-call body))))))
    (concat (propertize name 'font-lock-face 'agent-shell-section-heading)
            (unless (string-empty-p target)
              (concat " " (propertize (agent-shell-fork-tool-style--code target)
                                      'font-lock-face 'agent-shell-section-annotation)))
            (when outcome
              (propertize (concat " · " (string-join outcome " · "))
                          'font-lock-face 'agent-shell-section-annotation)))))

(defun agent-shell-fork-tool-style--unwrap (text)
  "Return TEXT with a JSON result envelope unwrapped, or TEXT unchanged.

Only a body that is nothing but the envelope is unwrapped: the whole
text has to parse, so a body that already carries output plus a diff is
left to upstream's renderer."
  (if (or (not (stringp text))
          (not (string-prefix-p "{" (string-trim text))))
      text
    (condition-case nil
        (let* ((data (json-parse-string text :object-type 'alist :array-type 'list))
               (blocks (map-elt data 'content)))
          (if (not (listp blocks))
              text
            (let ((texts (delq nil
                               (mapcar (lambda (block)
                                         (let ((inner (map-elt block 'content)))
                                           (or (map-elt block 'text)
                                               (and (consp inner) (map-elt inner 'text)))))
                                       blocks))))
              (if texts (string-join texts "\n\n") text))))
      (error text))))

(defun agent-shell-fork-tool-style--filter-args (args)
  "Present ARGS' tool call compactly.

ARGS is the advised `agent-shell--update-fragment' argument list: a
`cl-defun' with &key, so the list is the keyword list itself."
  (if-let* ((tool-call (agent-shell-fork-tool-style--applies-p (plist-get args :state)
                                                               (plist-get args :block-id))))
      (let ((body (plist-get args :body)))
        (when (and agent-shell-fork-tool-style-unwrap-json
                   (stringp body))
          (setq body (agent-shell-fork-tool-style--unwrap body))
          (setq args (plist-put args :body body)))
        (when agent-shell-fork-tool-style-compact-label
          ;; A call with neither a title nor an argument would render an
          ;; empty label, so upstream's label stays for it.
          (let ((label (agent-shell-fork-tool-style--label-right tool-call body)))
            (unless (string-empty-p label)
              (setq args (plist-put args :label-left
                                    (agent-shell-fork-tool-style--label-left tool-call)))
              (setq args (plist-put args :label-right label)))))
        args)
    args))

(defun agent-shell-fork-tool-style--stamp-start (args)
  "Record when the tool call ARGS is about to save started.

`agent-shell--save-tool-call' merges an update into the stored call,
so the stamp lands on the call itself and rides along with it.  ARGS is
its argument list: STATE, TOOL-CALL-ID and the TOOL-CALL update."
  (let* ((state (nth 0 args))
         (tool-call-id (nth 1 args))
         (tool-call (nth 2 args))
         (stored (and state tool-call-id
                      (map-elt (map-elt state :tool-calls) tool-call-id))))
    (when (and (consp tool-call)
               (member (map-elt tool-call :status) '("pending" "in_progress"))
               (not (map-elt tool-call :started-at))
               (not (map-elt stored :started-at)))
      (setf (nth 2 args) (append tool-call (list (cons :started-at (float-time))))))
    args))

(advice-add 'agent-shell--update-fragment :filter-args
            #'agent-shell-fork-tool-style--filter-args)
(advice-add 'agent-shell--save-tool-call :filter-args
            #'agent-shell-fork-tool-style--stamp-start)

(provide 'agent-shell-fork-tool-style)

;;; agent-shell-fork-tool-style.el ends here
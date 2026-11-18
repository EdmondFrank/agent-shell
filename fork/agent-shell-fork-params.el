;;; agent-shell-fork-params.el --- Show a tool call's arguments  -*- lexical-binding: t; -*-

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
;; along with this package.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; Fork feature: AiderDesk's bridge sends a tool call's arguments in
;; `rawInput', but the fragment body stays empty until the call ends --
;; upstream builds a body only on the terminal `tool_call_update'.  A
;; long `code_mode' script or a file edit is therefore opaque while it
;; runs: the label reads "Code_mode" and nothing else.
;;
;; This module gives a call two sections instead of one:
;;
;; - its arguments, as a fenced block, from the start and kept after the
;;   call finishes;
;; - its output, in a label-less fragment right below, written when the
;;   call ends.
;;
;; Upstream replaces the body with the call's output on the terminal
;; update, and its own argument block covers only the catch-all kind
;; `other'.  AiderDesk reports a real kind for most of its tools
;; (`execute` for `code_mode', `read`, `search`, ...), so a finished call
;; lost the parameters it showed while running.  Keeping the two in
;; separate fragments also gives each its own fold state and its own
;; share of `agent-shell-fork-tool-max-height': a long script no longer
;; pushes the output out of the capped view.
;;
;; Scoped to the AiderDesk agent: agents that already put the arguments
;; in the label (Pi's execute calls carry the whole command there) gain
;; nothing but a taller fragment.
;;
;; Loaded before `agent-shell-fork-tool-style', whose label advice sits
;; outside this one: the outcome it appends ("132 lines", "exit 0") is
;; read from the output body, which this module moves out of the
;; fragment it labels.
;;
;; Upstream seam used, no upstream file edited:
;; `agent-shell--update-fragment' is advised with `:around'.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'map)

(require 'agent-shell)

(declare-function agent-shell--format-tool-call-input "agent-shell")
(declare-function agent-shell--truncate-tool-output-lines "agent-shell")

(defcustom agent-shell-fork-tool-params-expanded t
  "Show a tool call's parameters without toggling them open.

The arguments become the fragment's body when the call starts and stay
there once it finishes, with the call's output in a fragment of its own
below.  Nil leaves upstream's default (folded), putting the parameters
one toggle away.

`agent-shell-fork-tool-max-height' still caps how much of each body is
shown."
  :type 'boolean
  :group 'agent-shell)

(defconst agent-shell-fork-params--agents '(aiderdesk)
  "Agents whose tool calls get their arguments rendered.")

(defconst agent-shell-fork-params--output-suffix "-output"
  "Suffix of the fragment holding a tool call's output.")

(defun agent-shell-fork-params--call (state block-id)
  "Return STATE's AiderDesk tool call BLOCK-ID, or nil.

A block id that is not a tool call -- a message, a permission dialog, a
plan, a questionnaire, the output fragment this module adds -- has no
entry, which is how the advice tells them apart."
  (when (and state block-id
             (memq (map-nested-elt state '(:agent-config :identifier))
                   agent-shell-fork-params--agents))
    (map-nested-elt state (list :tool-calls block-id))))

(defun agent-shell-fork-params--finished-p (tool-call)
  "Return non-nil when TOOL-CALL has ended."
  (member (map-elt tool-call :status) '("completed" "failed")))

(defun agent-shell-fork-params--arguments (tool-call)
  "Return TOOL-CALL's arguments as a fenced block, or nil.

Nil for a call with no arguments, and for an `rawInput' that is not an
alist (nothing to lay out)."
  (when-let* ((raw-input (map-elt tool-call :raw-input))
              ((consp raw-input)))
    (agent-shell--truncate-tool-output-lines
     (agent-shell--format-tool-call-input raw-input))))

(defun agent-shell-fork-params--arguments-prefix (tool-call)
  "Return the arguments block upstream prefaces TOOL-CALL's output with.

Upstream composes a finished body as arguments, a blank line and output,
for the two shapes it recognises: a `command' as a console fence, and the
catch-all kind `other' as the call's input.  Nil when upstream shows no
arguments of its own."
  (cond ((map-elt tool-call :command)
         (concat "```console\n" (map-elt tool-call :command) "\n```"))
        ((and (member (map-elt tool-call :kind) '(nil "other"))
              (consp (map-elt tool-call :raw-input)))
         (agent-shell--format-tool-call-input
          (map-elt tool-call :raw-input)))))

(defun agent-shell-fork-params--output (tool-call body)
  "Return BODY, TOOL-CALL's body, without the arguments it opens with.

The parameters sit in the call's own fragment, so repeating them above
the output would show them twice.  A body that does not open with them
-- diffs, a bare result -- is returned as is."
  (let ((prefix (agent-shell-fork-params--arguments-prefix tool-call)))
    (string-trim-left
     (if (and prefix (string-prefix-p prefix body))
         (substring body (length prefix))
       body))))

(defun agent-shell-fork-params--render-output (fn args)
  "Render ARGS' body as the output below the call it belongs to, via FN.

FN is the advised `agent-shell--update-fragment' and ARGS the keyword
list carrying the output body.  The output goes in a label-less
fragment, so it reads as the rest of the call without a second
heading."
  (apply fn (list :state (plist-get args :state)
                  :namespace-id (plist-get args :namespace-id)
                  :block-id (concat (plist-get args :block-id)
                                    agent-shell-fork-params--output-suffix)
                  :body (plist-get args :body)
                  :group-id (plist-get args :group-id)
                  :group-label (plist-get args :group-label)
                  :group-expanded (plist-get args :group-expanded)
                  :expanded t
                  :above-last-prompt (plist-get args :above-last-prompt))))

(defun agent-shell-fork-params--around (fn &rest args)
  "Keep a tool call's arguments in its body, and its output below.

FN is the advised `agent-shell--update-fragment' and ARGS its argument
list: a `cl-defun' with &key, so the list is the keyword list itself.

A running call gets its arguments as the body, which upstream leaves
empty until the call ends.  A finished call keeps them there, and the
output -- what upstream would have put in that body -- renders in a
label-less fragment right below, so each keeps its own fold state and
its own cap.

The update is forwarded to FN rather than reproduced: labels, groups,
and markdown rendering stay upstream's."
  (let* ((tool-call (agent-shell-fork-params--call (plist-get args :state)
                                                    (plist-get args :block-id)))
         (arguments (and tool-call
                         (agent-shell-fork-params--arguments tool-call)))
         (finished-p (and tool-call
                          (agent-shell-fork-params--finished-p tool-call)))
         (body (plist-get args :body))
         ;; A body arriving while the call still runs is streamed output,
         ;; which upstream already knows how to append to.
         (streamed-p (and (plist-member args :body) (not finished-p))))
    (if (or (null arguments) streamed-p)
        (apply fn args)
      (let ((call-args (plist-put (copy-sequence args) :body arguments)))
        (when (not (plist-member args :body))
          (setq call-args
                (plist-put call-args
                           :expanded
                           (or (plist-get args :expanded)
                               agent-shell-fork-tool-params-expanded))))
        (prog1 (apply fn call-args)
          (when finished-p
            (let ((output (and (stringp body)
                               (agent-shell-fork-params--output tool-call body))))
              (when (and output (not (string-empty-p output)))
                (agent-shell-fork-params--render-output
                 fn
                 (plist-put (copy-sequence args) :body output))))))))))

(advice-add 'agent-shell--update-fragment :around
            #'agent-shell-fork-params--around)

(provide 'agent-shell-fork-params)

;;; agent-shell-fork-params.el ends here

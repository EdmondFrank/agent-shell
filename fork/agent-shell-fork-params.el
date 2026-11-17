;;; agent-shell-fork-params.el --- Show a running tool call's arguments  -*- lexical-binding: t; -*-

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
;; Fork feature: AiderDesk's bridge sends a tool call's arguments in
;; `rawInput', but the fragment body stays empty until the call ends --
;; upstream builds a body only on the terminal `tool_call_update'.  A
;; long `code_mode' script or a file edit is therefore opaque while it
;; runs: the label reads "Code_mode" and nothing else.
;;
;; This module fills the gap by giving the fragment its arguments as a
;; fenced block when upstream first renders it (the `tool_call' update,
;; which passes no body), so the parameters are on screen from the
;; start.  Upstream's own rendering takes over on the terminal update,
;; which replaces the body with the call's output.  The block is an
;; ordinary tool body: `agent-shell-fork-cap' bounds its height and
;; `agent-shell-fork-width' its line width.
;;
;; Scoped to the AiderDesk agent: agents that already put the arguments
;; in the label (Pi's execute calls carry the whole command there) gain
;; nothing but a taller fragment.
;;
;; Upstream seam used, no upstream file edited:
;; `agent-shell--update-fragment' is advised with `:filter-args'.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'map)

(require 'agent-shell)

(declare-function agent-shell--format-tool-call-input "agent-shell")
(declare-function agent-shell--truncate-tool-output-lines "agent-shell")

(defcustom agent-shell-fork-tool-params-expanded t
  "Show a running tool call's parameters without toggling them open.

The arguments become the fragment's body when the call starts, and the
same fold state carries over to the output that replaces them.  Nil
leaves upstream's default (folded), putting the parameters one toggle
away.

`agent-shell-fork-tool-max-height' still caps how much of the body is
shown."
  :type 'boolean
  :group 'agent-shell)

(defconst agent-shell-fork-params--agents '(aiderdesk)
  "Agents whose running tool calls get their arguments rendered.")

(defun agent-shell-fork-params--running-call (state block-id)
  "Return STATE's in-flight tool call BLOCK-ID, or nil.

Only a call still `in_progress' is returned: a terminal update brings
upstream's own body, which must not be replaced by the arguments it
already carries."
  (when (and state block-id)
    (map-nested-elt state (list :tool-calls block-id))))

(defun agent-shell-fork-params--body (state block-id)
  "Return a fenced block of STATE's call BLOCK-ID arguments, or nil.

Nil for a call with no arguments, for one that already finished, and
for an `rawInput' that is not an alist (nothing to lay out)."
  (when-let* ((tool-call (agent-shell-fork-params--running-call state block-id))
              ((equal (map-elt tool-call :status) "in_progress"))
              (raw-input (map-elt tool-call :raw-input))
              ((consp raw-input)))
    (agent-shell--truncate-tool-output-lines
     (agent-shell--format-tool-call-input raw-input))))

(defun agent-shell-fork-params--filter-args (args)
  "Give a starting tool call its arguments as the fragment body.

ARGS is the advised `agent-shell--update-fragment' argument list.  The
function is a `cl-defun' with &key, so that list is the keyword list
itself.  An update that already carries a body — a completion, a
streamed chunk, a message, a permission dialog — is left alone."
  (let ((state (plist-get args :state))
        (block-id (plist-get args :block-id)))
    (if (or (null state)
            (null block-id)
            (plist-member args :body)
            (not (memq (map-nested-elt state '(:agent-config :identifier))
                       agent-shell-fork-params--agents)))
        args
      (if-let* ((body (agent-shell-fork-params--body state block-id)))
          (plist-put (plist-put args :body body)
                     :expanded
                     (or (plist-get args :expanded)
                         agent-shell-fork-tool-params-expanded))
        args))))

(advice-add 'agent-shell--update-fragment :filter-args
            #'agent-shell-fork-params--filter-args)

(provide 'agent-shell-fork-params)

;;; agent-shell-fork-params.el ends here
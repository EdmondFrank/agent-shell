;;; fork-seams.el --- Upstream seams the fork leans on  -*- lexical-binding: t; -*-

;;; Commentary:

;; The list of upstream definitions the overlay hooks into, plus the
;; hashing used to tell whether one of them moved.  Used by
;; `generate-overrides.el' (writes the recorded hashes) and
;; `check-drift.el' (compares them with what the checkout now has).

;;; Code:

(require 'subr-x)

(defconst fork-seam-specs
  ;; (SYMBOL KIND WRAPPER NOTE)
  '((agent-shell-ui-update-fragment
     function agent-shell-fork-cap--after-update
     "cap: re-derives the cut after every insert and streamed update")
    (agent-shell-ui-update-fragment
     function agent-shell-fork-stream--filter-args
     "stream: turns a grown body into an append")
    (agent-shell--update-fragment
     function agent-shell-fork-params--around
     "params: keeps a call's arguments in its body, its output below")
    (agent-shell--update-fragment
     function agent-shell-fork-tool-style--filter-args
     "tool-style: presents a call's name, argument, duration and outcome")
    (agent-shell--save-tool-call
     function agent-shell-fork-tool-style--stamp-start
     "tool-style: records when a tool call started, for its duration")
    (agent-shell--truncate-tool-output-lines
     function agent-shell-fork-width--around
     "width: shortens tool output lines before they render")
    (agent-shell-ui-toggle-fragment
     function agent-shell-fork-cap--toggle-around
     "cap: first toggle on a capped body reveals it")
    (agent-shell-ui--toggle-leaf-fragment-at-point
     function agent-shell-fork-cap--after-toggle
     "cap: expanding a folded body keeps it uncapped")
    (agent-shell-ui--cached-block
     function nil
     "util: locates a fragment's block by qualified id")
    (agent-shell-ui--body-range
     function nil
     "util: finds the body section inside a block")
    (agent-shell-ui--apply-trailing-whitespace-invisible
     function nil
     "util: re-hides the body's trailing newlines")
    (agent-shell-ui--set-group-collapsed
     function nil
     "cap: group folding re-applies each child's state")
    (agent-shell--make-header-model
     function agent-shell-fork-session-title--make-header-model
     "session-title: adds the session title to every header's model")
    (agent-shell--render-header-model-uncached
     function agent-shell-fork-session-title--render-header
     "session-title: splices the title into a text header")
    (agent-shell--svg-content-width
     function agent-shell-fork-session-title--svg-content-width
     "session-title: appends the title to the SVG's bottom row")
    (agent-shell--set-session-title
     function agent-shell-fork-session-title--after-set-title
     "session-title: repaints the header when a title lands")
    (agent-shell--command-completion-at-point
     function agent-shell-fork-skill--completion-around
     "skill: completes a `<prefix>:<payload>' command, colon included")
    (agent-shell--trigger-completion-at-point
     function agent-shell-fork-skill--trigger-around
     "skill: pops it when the colon is typed and nothing is active")
    (agent-shell-completion--input-start
     function nil
     "skill: finds where the prompt input being composed begins")
    (agent-shell-completion--source-buffer
     function nil
     "skill: finds the buffer whose session commands to complete"))
  "Upstream definitions the fork depends on, with what it uses them for.")

(defun fork-seam-source (symbol kind)
  "Return SYMBOL's current upstream definition as a string.

An advised function is unwrapped first: the manifest records what
upstream ships, not the fork's own wrapper around it."
  (if (eq kind 'variable)
      (prin1-to-string (symbol-value symbol))
    (if (fboundp symbol)
        (let ((fn (symbol-function symbol)))
          (when (advice--p fn)
            (while (advice--p fn)
              (setq fn (advice--cdr fn))))
          (prin1-to-string fn))
      "")))

(defun fork-seam-hash (symbol kind)
  "Return a stable hash of SYMBOL's upstream KIND definition."
  (secure-hash 'sha256 (fork-seam-source symbol kind)))

(provide 'fork-seams)

;;; fork-seams.el ends here
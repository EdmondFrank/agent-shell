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
     function agent-shell-fork-params--filter-args
     "params: gives a starting tool call its arguments as the body")
    (agent-shell--truncate-tool-output-lines
     function agent-shell-fork-width--around
     "width: shortens tool output lines before they render")
    (agent-shell-ui-toggle-fragment
     function agent-shell-fork-cap--toggle-around
     "cap: first toggle on a capped body reveals it")
    (agent-shell-ui--toggle-leaf-fragment-at-point
     function agent-shell-fork-cap--after-toggle
     "cap: expanding a folded body keeps it uncapped")
    (agent-shell--save-usage
     function agent-shell-fork-usage--after-save-usage
     "usage: refresh the thought group header when a turn's usage lands")
    (agent-shell--update-usage-from-notification
     function agent-shell-fork-usage--after-notification
     "usage: refresh the thought group header on a usage_update")
    (agent-shell-activity-group-header-label-function
     variable nil
     "usage: wrapped to append the usage summary")
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
     "cap: group folding re-applies each child's state"))
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
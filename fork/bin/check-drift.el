;;; check-drift.el --- Report upstream seams that moved under the fork  -*- lexical-binding: t; -*-

;;; Commentary:

;; Run after merging upstream (fork/bin/fork-sync does it):
;;
;;   emacs --batch -Q -L . -L fork -L fork/bin -L <acp> -L <shell-maker> \
;;     -l fork/bin/check-drift.el
;;
;; Exit status 0: every seam the overlay leans on still hashes as
;; recorded.  2: at least one upstream definition changed, so review the
;; wrapper for it (`fork/overrides.eld' names each one and its purpose).

;;; Code:

(require 'agent-shell)
(require 'agent-shell-fork)
(require 'fork-seams)

(defconst fork-drift-root
  (let ((dir default-directory))
    (or (getenv "FORK_ROOT") dir))
  "Repository root the manifest is read from.")

(load (expand-file-name "fork/overrides.eld" fork-drift-root) nil t)

(let ((drift 0)
      (total 0))
  (dolist (spec fork-seam-specs)
    (let* ((symbol (nth 0 spec))
           (kind (nth 1 spec))
           (note (nth 3 spec))
           (entry (seq-find (lambda (e)
                              (and (eq (nth 0 e) symbol)
                                   (eq (nth 1 e) kind)))
                            fork-overrides-manifest))
           (recorded (nth 3 entry))
           (current (fork-seam-hash symbol kind)))
      (setq total (1+ total))
      (unless (equal recorded current)
        (setq drift (1+ drift))
        (princ (format "DRIFT %s (%s)\n      %s\n"
                       symbol kind note)))))
  (princ (format "\n%d/%d seams unchanged\n" (- total drift) total))
  (kill-emacs (if (> drift 0) 2 0)))
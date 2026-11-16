;;; agent-shell-fork-width-tests.el --- Fork line width tests  -*- lexical-binding: t; -*-

;;; Commentary:

;; Behaviour tests for `fork/agent-shell-fork-width.el': tool output
;; reaching a fragment is shortened only where a line would cost more
;; to parse and display than it is worth, upstream's own limit still
;; applies underneath, and output that needs no work is handed back
;; untouched.

;;; Code:

(require 'ert)
(require 'cl-lib)
(add-to-list 'load-path
             (expand-file-name
              "../fork" (file-name-directory (or load-file-name
                                                 default-directory))))
(require 'agent-shell-fork)

(defun agent-shell-fork-width-tests--omitted-mark (line context)
  "Return the mark shortening LINE keeps CONTEXT characters at each end."
  (format "%s ... [%d characters omitted] ... %s"
          (substring line 0 context)
          (- (length line) (* 2 context))
          (substring line (- (length line) context))))

(ert-deftest agent-shell-fork-width-keeps-short-output-untouched-test ()
  "Output with no long line is returned as the same string."
  (let ((output "short\nlines\nonly\n"))
    (should (eq output (agent-shell--truncate-tool-output-lines output)))))

(ert-deftest agent-shell-fork-width-shortens-a-long-line-test ()
  "A long line keeps its ends and reports what it lost."
  (let* ((limit agent-shell-fork-tool-max-line-width)
         (context agent-shell-fork-tool-line-context)
         (line (make-string (* 2 limit) ?x))
         (result (agent-shell--truncate-tool-output-lines
                  (concat "before\n" line "\nafter"))))
    (should (string-prefix-p "before\n" result))
    (should (string-suffix-p "\nafter" result))
    (should (string-match-p
             (regexp-quote (agent-shell-fork-width-tests--omitted-mark line context))
             result))))

(ert-deftest agent-shell-fork-width-shortens-every-long-line-test ()
  "Each over-wide line is shortened, not just the first."
  (let* ((limit agent-shell-fork-tool-max-line-width)
         (context agent-shell-fork-tool-line-context)
         (long (make-string (* 2 limit) ?q))
         (result (agent-shell--truncate-tool-output-lines
                  (concat long "\n" long))))
    (should (= 2 (1- (length (split-string result "characters omitted" t)))))))

(ert-deftest agent-shell-fork-width-keeps-a-line-at-the-limit-test ()
  "A line exactly at the limit is left alone."
  (let ((line (make-string agent-shell-fork-tool-max-line-width ?y)))
    (should (equal line (agent-shell--truncate-tool-output-lines line)))))

(ert-deftest agent-shell-fork-width-falls-back-to-upstream-test ()
  "With the knob off, upstream's own limit is what applies."
  (let ((agent-shell-fork-tool-max-line-width nil)
        (line (make-string 5000 ?z)))
    ;; 5000 is under upstream's 32768, so nothing is shortened.
    (should (equal line (agent-shell--truncate-tool-output-lines line)))))

(ert-deftest agent-shell-fork-width-never-loosens-upstream-test ()
  "A fork limit above upstream's cannot lengthen a line upstream cuts."
  (let ((agent-shell-fork-tool-max-line-width 100000)
        (line (make-string (1+ agent-shell--tool-output-max-line-length) ?z)))
    (should (string-match-p "characters omitted"
                            (agent-shell--truncate-tool-output-lines line)))))

(ert-deftest agent-shell-fork-width-leaves-line-breaks-alone-test ()
  "Shortened output keeps the body's line count, so caps still apply."
  (let* ((limit agent-shell-fork-tool-max-line-width)
         (long (make-string (* 2 limit) ?w))
         (result (agent-shell--truncate-tool-output-lines
                  (concat long "\n" long "\n"))))
    (should (= 3 (length (split-string result "\n"))))))

(provide 'agent-shell-fork-width-tests)

;;; agent-shell-fork-width-tests.el ends here
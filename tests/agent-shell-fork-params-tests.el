;;; agent-shell-fork-params-tests.el --- Tests for parameters while running -*- lexical-binding: t; -*-

;;; Commentary:

;; The body a starting tool call gets from its `rawInput', and the cases
;; that must be left to upstream: an update that already carries a body,
;; another agent's calls, and a call that already finished.

;;; Code:

(require 'ert)
(require 'cl-lib)
;; Fork tests live beside upstream's, but their sources sit in fork/:
;; add it so a plain `-L .' run (upstream CI) still loads them.
(add-to-list 'load-path
             (expand-file-name
              "../fork" (file-name-directory (or load-file-name buffer-file-name
                                                 default-directory))))
(require 'agent-shell-fork-params)

(defun agent-shell-fork-params-tests--state (&optional identifier status raw-input)
  "Return a shell state whose single tool call carries RAW-INPUT.
IDENTIFIER defaults to the AiderDesk agent, STATUS to `in_progress'."
  (list (cons :agent-config (list (cons :identifier (or identifier 'aiderdesk))))
        (cons :tool-calls
              (list (cons "call-1"
                          (list (cons :title "Code_mode")
                                (cons :status (or status "in_progress"))
                                (cons :raw-input raw-input)))))))

(defun agent-shell-fork-params-tests--update (&optional identifier status raw-input)
  "Return the advised arguments of a starting tool call in such a state."
  (agent-shell-fork-params--filter-args
   (list :state (agent-shell-fork-params-tests--state identifier status raw-input)
         :block-id "call-1"
         :label-left "x"
         :label-right "y"
         :expanded nil)))

(ert-deftest agent-shell-fork-params-injects-arguments-test ()
  "A starting call gets its arguments as a fenced body."
  (let* ((keys (agent-shell-fork-params-tests--update
                nil nil '((command . "rtk git status"))))
         (body (plist-get keys :body)))
    (should (stringp body))
    (should (string-prefix-p "```" body))
    (should (string-match-p (regexp-quote "rtk git status") body))))

(ert-deftest agent-shell-fork-params-keeps-call-arguments-test ()
  "The injected body leaves the rest of the call untouched."
  (let ((keys (agent-shell-fork-params-tests--update
               nil nil '((path . "/a/b.ts")))))
    (should (equal (plist-get keys :block-id) "call-1"))
    (should (equal (plist-get keys :label-left) "x"))
    (should (equal (plist-get keys :label-right) "y"))))

(ert-deftest agent-shell-fork-params-json-block-test ()
  "Several arguments render as a JSON block, one argument bare."
  (let ((body (plist-get (agent-shell-fork-params-tests--update
                          nil nil '((code . "local r = 1") (path . "/a")))
                         :body)))
    (should (string-prefix-p "```json" body))
    (should (string-match-p (regexp-quote "\"code\"") body))))

(ert-deftest agent-shell-fork-params-expands-by-default-test ()
  "The parameters are visible without a toggle, unless the knob says not."
  (should (eq (plist-get (agent-shell-fork-params-tests--update
                          nil nil '((path . "/a")))
                         :expanded)
              t))
  (let ((agent-shell-fork-tool-params-expanded nil))
    (should-not (plist-get (agent-shell-fork-params-tests--update
                            nil nil '((path . "/a")))
                           :expanded))))

(ert-deftest agent-shell-fork-params-keeps-existing-body-test ()
  "An update that already carries a body is left alone."
  (let ((keys (agent-shell-fork-params--filter-args
               (list :state (agent-shell-fork-params-tests--state
                             nil nil '((path . "/a")))
                     :block-id "call-1"
                     :body "output"))))
    (should (equal (plist-get keys :body) "output"))))

(ert-deftest agent-shell-fork-params-skips-finished-call-test ()
  "A finished call keeps upstream's body (the output), not the arguments."
  (should-not (plist-member (agent-shell-fork-params-tests--update
                             nil "completed" '((path . "/a")))
                            :body)))

(ert-deftest agent-shell-fork-params-ignores-other-agents-test ()
  "Only the AiderDesk agent gets the parameters body."
  (should-not (plist-member (agent-shell-fork-params-tests--update
                             'pi nil '((command . "ls")))
                            :body)))

(ert-deftest agent-shell-fork-params-skips-empty-arguments-test ()
  "A call with no arguments gets no body."
  (should-not (plist-member (agent-shell-fork-params-tests--update) :body))
  (should-not (plist-member (agent-shell-fork-params-tests--update nil nil '()) :body)))

(provide 'agent-shell-fork-params-tests)
;;; agent-shell-fork-params-tests.el ends here
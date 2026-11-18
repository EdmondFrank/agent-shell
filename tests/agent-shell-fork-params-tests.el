;;; agent-shell-fork-params-tests.el --- Tests for tool-call parameters  -*- lexical-binding: t; -*-

;;; Commentary:

;; What a tool call's fragment shows: the body it gets from its
;; `rawInput' while running, the same arguments once it ends, and the
;; output that moves to a fragment of its own below them.  Also the cases
;; left to upstream: a streamed body, another agent's call, a call with
;; no arguments, and the output fragment this module adds.

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

(defun agent-shell-fork-params-tests--state (&optional identifier status raw-input extra)
  "Return a shell state whose single tool call carries RAW-INPUT.
IDENTIFIER defaults to the AiderDesk agent, STATUS to `in_progress',
and EXTRA is an alist of further entries for the call."
  (list (cons :agent-config (list (cons :identifier (or identifier 'aiderdesk))))
        (cons :tool-calls
              (list (cons "call-1"
                          (append (list (cons :title "Code_mode")
                                        (cons :status (or status "in_progress"))
                                        (cons :raw-input raw-input))
                                  extra))))))

(defun agent-shell-fork-params-tests--calls (args)
  "Return what `agent-shell-fork-params--around' forwards for ARGS.

Each element is the keyword list forwarded to
`agent-shell--update-fragment'."
  (let ((calls nil))
    (apply #'agent-shell-fork-params--around
           (lambda (&rest call) (push call calls))
           args)
    (nreverse calls)))

(defun agent-shell-fork-params-tests--update (&optional identifier status raw-input extra)
  "Return the advised argument list of a starting tool call.
IDENTIFIER, STATUS, RAW-INPUT and EXTRA go to
`agent-shell-fork-params-tests--state'."
  (list :state (agent-shell-fork-params-tests--state identifier status raw-input extra)
        :block-id "call-1"
        :label-left "x"
        :label-right "y"
        :group-id "group-1"
        :expanded nil))

(defun agent-shell-fork-params-tests--finished (body &optional status raw-input extra identifier)
  "Return the advised argument list of a finished call carrying BODY.
STATUS defaults to `completed'; RAW-INPUT, EXTRA and IDENTIFIER go to
`agent-shell-fork-params-tests--state'."
  (append (agent-shell-fork-params-tests--update identifier (or status "completed")
                                                 raw-input extra)
          (list :body body)))

(ert-deftest agent-shell-fork-params-injects-arguments-test ()
  "A starting call gets its arguments as a fenced body."
  (let* ((calls (agent-shell-fork-params-tests--calls
                 (agent-shell-fork-params-tests--update
                  nil nil '((command . "rtk git status")))))
         (body (plist-get (car calls) :body)))
    (should (= (length calls) 1))
    (should (stringp body))
    (should (string-prefix-p "```" body))
    (should (string-match-p (regexp-quote "rtk git status") body))))

(ert-deftest agent-shell-fork-params-keeps-call-arguments-test ()
  "The injected body leaves the rest of the call untouched."
  (let ((keys (car (agent-shell-fork-params-tests--calls
                    (agent-shell-fork-params-tests--update
                     nil nil '((path . "/a/b.ts")))))))
    (should (equal (plist-get keys :block-id) "call-1"))
    (should (equal (plist-get keys :label-left) "x"))
    (should (equal (plist-get keys :label-right) "y"))))

(ert-deftest agent-shell-fork-params-json-block-test ()
  "Several arguments render as a JSON block, one argument bare."
  (let ((body (plist-get (car (agent-shell-fork-params-tests--calls
                               (agent-shell-fork-params-tests--update
                                nil nil '((code . "local r = 1") (path . "/a")))))
                         :body)))
    (should (string-prefix-p "```json" body))
    (should (string-match-p (regexp-quote "\"code\"") body))))

(ert-deftest agent-shell-fork-params-expands-by-default-test ()
  "The parameters are visible without a toggle, unless the knob says not."
  (should (eq (plist-get (car (agent-shell-fork-params-tests--calls
                               (agent-shell-fork-params-tests--update
                                nil nil '((path . "/a")))))
                         :expanded)
              t))
  (let ((agent-shell-fork-tool-params-expanded nil))
    (should-not (plist-get (car (agent-shell-fork-params-tests--calls
                                 (agent-shell-fork-params-tests--update
                                  nil nil '((path . "/a")))))
                           :expanded))))

(ert-deftest agent-shell-fork-params-keeps-streamed-body-test ()
  "A body arriving while the call runs is left to upstream."
  (let ((calls (agent-shell-fork-params-tests--calls
                (append (agent-shell-fork-params-tests--update
                         nil nil '((code . "local r = 1")))
                        (list :body "partial output")))))
    (should (= (length calls) 1))
    (should (equal (plist-get (car calls) :body) "partial output"))))

(ert-deftest agent-shell-fork-params-keeps-arguments-after-finish-test ()
  "A finished call keeps its arguments, and its output lands below."
  (let* ((raw-input '((code . "local r = parallel_all({})")))
         (calls (agent-shell-fork-params-tests--calls
                 (agent-shell-fork-params-tests--finished "42" "completed" raw-input))))
    (should (= (length calls) 2))
    (should (string-match-p (regexp-quote "local r = parallel_all")
                            (plist-get (car calls) :body)))
    (should (equal (plist-get (cadr calls) :body) "42"))))

(ert-deftest agent-shell-fork-params-output-fragment-test ()
  "The output renders in its own label-less fragment, in the call's group."
  (let ((calls (agent-shell-fork-params-tests--calls
                (agent-shell-fork-params-tests--finished
                 "42" "completed" '((code . "local r = 1")))))
        (output nil))
    (setq output (cadr calls))
    (should (equal (plist-get output :block-id) "call-1-output"))
    (should (null (plist-get output :label-left)))
    (should (null (plist-get output :label-right)))
    (should (equal (plist-get output :group-id) "group-1"))
    (should (eq (plist-get output :expanded) t))))

(ert-deftest agent-shell-fork-params-output-drops-upstream-arguments-test ()
  "The arguments upstream prints above the output are not repeated."
  (let* ((raw-input '((path . "/a/b.el")))
         (input (agent-shell--format-tool-call-input raw-input))
         (calls (agent-shell-fork-params-tests--calls
                 (agent-shell-fork-params-tests--finished
                  (concat input "\n\ncontent: hi") "completed" raw-input
                  '((:kind . "other"))))))
    (should (= (length calls) 2))
    (should (equal (plist-get (cadr calls) :body) "content: hi"))))

(ert-deftest agent-shell-fork-params-output-drops-command-block-test ()
  "The console fence upstream prints for a command is not repeated."
  (let ((calls (agent-shell-fork-params-tests--calls
                (agent-shell-fork-params-tests--finished
                 "```console\nls -la\n```\n\nexit_code: 0"
                 "completed" '((command . "ls -la"))
                 '((:kind . "execute") (:command . "ls -la"))))))
    (should (= (length calls) 2))
    (should (equal (plist-get (cadr calls) :body) "exit_code: 0"))))

(ert-deftest agent-shell-fork-params-skips-empty-output-test ()
  "A call that produced nothing gets no output fragment."
  (let ((calls (agent-shell-fork-params-tests--calls
                (agent-shell-fork-params-tests--finished
                 "" "completed" '((code . "local r = 1"))))))
    (should (= (length calls) 1))
    (should (string-match-p (regexp-quote "local r = 1")
                            (plist-get (car calls) :body)))))

(ert-deftest agent-shell-fork-params-ignores-other-agents-test ()
  "Only the AiderDesk agent gets the parameters body."
  (let ((calls (agent-shell-fork-params-tests--calls
                (agent-shell-fork-params-tests--finished
                 "42" "completed" '((command . "ls")) nil 'pi))))
    (should (= (length calls) 1))
    (should (equal (plist-get (car calls) :body) "42"))))

(ert-deftest agent-shell-fork-params-ignores-other-fragments-test ()
  "A fragment that is not a tool call is left alone."
  (let* ((args (agent-shell-fork-params-tests--finished
                "42" "completed" '((code . "local r = 1"))))
         (calls (agent-shell-fork-params-tests--calls
                 (plist-put args :block-id "call-1-output"))))
    (should (= (length calls) 1))
    (should (equal (plist-get (car calls) :body) "42"))))

(ert-deftest agent-shell-fork-params-skips-empty-arguments-test ()
  "A call with no arguments keeps upstream's body."
  (let ((calls (agent-shell-fork-params-tests--calls
                (agent-shell-fork-params-tests--finished "42"))))
    (should (= (length calls) 1))
    (should (equal (plist-get (car calls) :body) "42")))
  (should (= (length (agent-shell-fork-params-tests--calls
                      (agent-shell-fork-params-tests--update nil nil '())))
             1)))

(provide 'agent-shell-fork-params-tests)
;;; agent-shell-fork-params-tests.el ends here

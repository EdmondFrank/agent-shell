;;; agent-shell-fork-tool-style-tests.el --- Tests for compact tool labels -*- lexical-binding: t; -*-

;;; Commentary:

;; What an AiderDesk tool call reads as: its name and argument instead of
;; the `other' kind, the duration and outcome that ACP does not carry,
;; and the cases the module must leave alone -- another agent's calls, a
;; questionnaire, a fragment that is not a tool call.

;;; Code:

(require 'ert)
(require 'cl-lib)
;; Fork tests live beside upstream's, but their sources sit in fork/:
;; add it so a plain `-L .' run (upstream CI) still loads them.
(add-to-list 'load-path
             (expand-file-name
              "../fork" (file-name-directory (or load-file-name buffer-file-name
                                                 default-directory))))
(require 'agent-shell-fork-tool-style)

(defun agent-shell-fork-tool-style-tests--call (&optional plist)
  "Return a tool call alist, with PLIST entries replacing the defaults.

The default call is a finished `code_mode' run whose title names the
script it ran."
  (let ((call (list (cons :title "Code_mode: local r = desktop.execute_command({...})")
                    (cons :status "completed")
                    (cons :raw-input '((code . "local r = desktop.execute_command({...})"))))))
    (while plist
      (setf (map-elt call (car plist)) (cadr plist))
      (setq plist (cddr plist)))
    call))

(defun agent-shell-fork-tool-style-tests--state (call &optional identifier)
  "Return shell state holding CALL, for IDENTIFIER (default AiderDesk)."
  (list (cons :agent-config (list (cons :identifier (or identifier 'aiderdesk))))
        (cons :tool-calls (list (cons "call-1" call)))))

(defun agent-shell-fork-tool-style-tests--label (&optional call body)
  "Return the compact label of CALL rendered with BODY."
  (agent-shell-fork-tool-style--label-right
   (or call (agent-shell-fork-tool-style-tests--call))
   body))

(defun agent-shell-fork-tool-style-tests--plain (string)
  "Return STRING without its faces."
  (substring-no-properties string))

(ert-deftest agent-shell-fork-tool-style-status-icon-test ()
  "The left label is the status icon alone, with no kind."
  (should (equal (agent-shell-fork-tool-style-tests--plain
                  (agent-shell-fork-tool-style--label-left
                   (agent-shell-fork-tool-style-tests--call)))
                 "✓"))
  (should (equal (agent-shell-fork-tool-style-tests--plain
                  (agent-shell-fork-tool-style--label-left
                   (agent-shell-fork-tool-style-tests--call '(:status "failed"))))
                 "✗")))

(ert-deftest agent-shell-fork-tool-style-label-names-the-tool-test ()
  "The label reads the tool's name, not the `other' kind upstream prints."
  (let ((label (agent-shell-fork-tool-style-tests--plain
                (agent-shell-fork-tool-style-tests--label))))
    (should (string-prefix-p "Code Mode " label))
    (should-not (string-match-p "Other" label))))

(ert-deftest agent-shell-fork-tool-style-label-carries-the-argument-test ()
  "The label carries the argument that says what the call is doing."
  (should (string-match-p "desktop\\.execute_command"
                          (agent-shell-fork-tool-style-tests--plain
                           (agent-shell-fork-tool-style-tests--label)))))

(ert-deftest agent-shell-fork-tool-style-argument-is-a-code-span-test ()
  "The argument is quoted so markdown leaves its punctuation alone."
  (let ((label (agent-shell-fork-tool-style-tests--plain
                (agent-shell-fork-tool-style-tests--label
                 (agent-shell-fork-tool-style-tests--call
                  '(:title "Code_mode"
                    :raw-input ((code . "local r = tools[\"a\"]"))))))))
    (should (string-match-p "`local r = tools\\[\"a\"\\]`" label))))

(ert-deftest agent-shell-fork-tool-style-argument-falls-back-to-raw-input-test ()
  "With no title to read, the argument comes from the raw input."
  (should (string-match-p "rtk git status"
                          (agent-shell-fork-tool-style-tests--plain
                           (agent-shell-fork-tool-style-tests--label
                            (agent-shell-fork-tool-style-tests--call
                             '(:title "Execute_command"
                               :raw-input ((command . "rtk git status")))))))))

(ert-deftest agent-shell-fork-tool-style-long-argument-is-truncated-test ()
  "A long argument is shortened rather than pushing the outcome off the line."
  (let* ((agent-shell-fork-tool-style-target-max-width 20)
         (label (agent-shell-fork-tool-style-tests--plain
                 (agent-shell-fork-tool-style-tests--label
                  (agent-shell-fork-tool-style-tests--call
                   '(:title "Code_mode: local r = parallel_all({ one, two, three })"))))))
    (should (string-match-p "…" label))
    (should-not (string-match-p "three" label))))

(ert-deftest agent-shell-fork-tool-style-duration-test ()
  "A finished call reports how long it took."
  (let ((label (agent-shell-fork-tool-style-tests--plain
                (agent-shell-fork-tool-style-tests--label
                 (agent-shell-fork-tool-style-tests--call
                  (list :started-at (- (float-time) 2)))))))
    (should (string-match-p " · 2s" label))))

(ert-deftest agent-shell-fork-tool-style-running-call-has-no-duration-test ()
  "A call still running reports neither a duration nor an outcome."
  (let ((label (agent-shell-fork-tool-style-tests--plain
                (agent-shell-fork-tool-style-tests--label
                 (agent-shell-fork-tool-style-tests--call
                  (list :status "in_progress"
                        :started-at (- (float-time) 5)))))))
    (should-not (string-match-p "5s" label))))

(ert-deftest agent-shell-fork-tool-style-replay-has-no-duration-test ()
  "A replayed call, whose start and end arrive together, shows no duration."
  (should-not (agent-shell-fork-tool-style--duration
               (agent-shell-fork-tool-style-tests--call
                (list :started-at (float-time))))))

(ert-deftest agent-shell-fork-tool-style-summary-reports-exit-code-test ()
  "An exit code in the output is the outcome."
  (should (string-match-p "exit 1"
                          (agent-shell-fork-tool-style-tests--plain
                           (agent-shell-fork-tool-style-tests--label
                            nil "cwd: /tmp\nexit_code: 1\nstdout: boom")))))

(ert-deftest agent-shell-fork-tool-style-summary-reports-lines-test ()
  "Output with no exit code reports how many lines it produced."
  (should (string-match-p "3 lines"
                          (agent-shell-fork-tool-style-tests--plain
                           (agent-shell-fork-tool-style-tests--label nil "a\nb\nc")))))

(ert-deftest agent-shell-fork-tool-style-summary-reports-failure-test ()
  "A failed call with no exit code says so."
  (should (string-match-p "error"
                          (agent-shell-fork-tool-style-tests--plain
                           (agent-shell-fork-tool-style-tests--label
                            (agent-shell-fork-tool-style-tests--call '(:status "failed"))
                            "No such file")))))

(ert-deftest agent-shell-fork-tool-style-summary-is-optional-test ()
  "Both suffixes can be turned off."
  (let* ((agent-shell-fork-tool-style-show-duration nil)
         (agent-shell-fork-tool-style-show-summary nil)
         (label (agent-shell-fork-tool-style-tests--plain
                 (agent-shell-fork-tool-style-tests--label nil "a\nb\nc"))))
    (should-not (string-match-p "lines" label))
    (should-not (string-match-p " · " label))))

(ert-deftest agent-shell-fork-tool-style-unwraps-a-result-envelope-test ()
  "A JSON envelope becomes the text it carries."
  (should (equal (agent-shell-fork-tool-style--unwrap
                  "{\"content\":[{\"type\":\"text\",\"text\":\"hello\"}],\"isError\":false}")
                 "hello")))

(ert-deftest agent-shell-fork-tool-style-leaves-other-bodies-alone-test ()
  "Anything that is not solely an envelope is untouched."
  (dolist (body '("plain output"
                  "{\"content\":[{\"type\":\"text\",\"text\":\"hello\"}]}\n\n```diff\n-a\n+b\n```"
                  "```console\nrtk git status\n```\n\nclean"))
    (should (equal (agent-shell-fork-tool-style--unwrap body) body))))

(ert-deftest agent-shell-fork-tool-style-filter-args-presents-the-call-test ()
  "The advised update carries the compact labels."
  (let* ((call (agent-shell-fork-tool-style-tests--call))
         (args (list :state (agent-shell-fork-tool-style-tests--state call)
                     :block-id "call-1"
                     :label-left "Other"
                     :label-right "Code_mode: local r = ..."
                     :body "a\nb")))
    (let ((result (agent-shell-fork-tool-style--filter-args args)))
      (should (equal (agent-shell-fork-tool-style-tests--plain
                      (plist-get result :label-left))
                     "✓"))
      (should (string-prefix-p "Code Mode " (plist-get result :label-right)))
      (should (string-match-p "2 lines" (plist-get result :label-right))))))

(ert-deftest agent-shell-fork-tool-style-filter-args-leaves-other-agents-alone-test ()
  "Another agent's tool call keeps upstream's rendering."
  (let ((args (list :state (agent-shell-fork-tool-style-tests--state
                           (agent-shell-fork-tool-style-tests--call)
                           'cursor)
                    :block-id "call-1"
                    :label-left "Other"
                    :label-right "Code_mode")))
    (should (equal (agent-shell-fork-tool-style--filter-args args) args))))

(ert-deftest agent-shell-fork-tool-style-filter-args-leaves-other-fragments-alone-test ()
  "A message or a permission dialog keeps its labels."
  (let ((args (list :state (agent-shell-fork-tool-style-tests--state
                           (agent-shell-fork-tool-style-tests--call))
                    :block-id "permission-call-1"
                    :label-left "Permission"
                    :label-right "Allow?")))
    (should (equal (agent-shell-fork-tool-style--filter-args args) args))))

(ert-deftest agent-shell-fork-tool-style-filter-args-leaves-questionnaires-alone-test ()
  "A questionnaire is a question, so upstream's prose rendering wins."
  (let* ((call (agent-shell-fork-tool-style-tests--call
                '(:raw-input ((questions . [((question . "Which colour?"))])))))
         (args (list :state (agent-shell-fork-tool-style-tests--state call)
                     :block-id "call-1"
                     :label-left "Question"
                     :label-right "Which colour?")))
    (should (equal (agent-shell-fork-tool-style--filter-args args) args))))

(ert-deftest agent-shell-fork-tool-style-stamps-a-starting-call-test ()
  "A call is stamped with the time it started, once."
  (let* ((call (agent-shell-fork-tool-style-tests--call '(:status "in_progress")))
         (state (agent-shell-fork-tool-style-tests--state call))
         (stamped (agent-shell-fork-tool-style--stamp-start (list state "call-1" call))))
    (should (numberp (map-elt (nth 2 stamped) :started-at)))
    ;; The start is stamped once: the stored call already carries it, so a
    ;; later update must not restart the clock.
    (let* ((finished (list (cons :status "completed")))
           (later (agent-shell-fork-tool-style--stamp-start
                   (list (agent-shell-fork-tool-style-tests--state (nth 2 stamped))
                         "call-1" finished))))
      (should (null (map-elt (nth 2 later) :started-at))))))

(ert-deftest agent-shell-fork-tool-style-does-not-stamp-a-finished-call-test ()
  "A call that arrives already finished never ran on this client."
  (let ((result (agent-shell-fork-tool-style--stamp-start
                 (list (agent-shell-fork-tool-style-tests--state
                        (agent-shell-fork-tool-style-tests--call))
                       "call-1"
                       (agent-shell-fork-tool-style-tests--call)))))
    (should (null (map-elt (nth 2 result) :started-at)))))

(ert-deftest agent-shell-fork-tool-style-labels-are-optional-test ()
  "Turning the compact label off leaves args untouched."
  (let* ((agent-shell-fork-tool-style-compact-label nil)
         (args (list :state (agent-shell-fork-tool-style-tests--state
                             (agent-shell-fork-tool-style-tests--call))
                     :block-id "call-1"
                     :label-left "Other"
                     :label-right "Code_mode")))
    (should (equal (agent-shell-fork-tool-style--filter-args args) args))))

(ert-deftest agent-shell-fork-tool-style-keeps-a-label-when-there-is-nothing-to-say-test ()
  "A call with neither a title nor an argument keeps upstream's label."
  (let* ((call (list (cons :status "completed")))
         (args (list :state (agent-shell-fork-tool-style-tests--state call)
                     :block-id "call-1"
                     :label-left "✓ Other"
                     :label-right "Tool")))
    (should (equal (agent-shell-fork-tool-style--filter-args args) args))))

(provide 'agent-shell-fork-tool-style-tests)

;;; agent-shell-fork-tool-style-tests.el ends here
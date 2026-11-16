;;; agent-shell-fork-usage-tests.el --- Fork usage summary tests  -*- lexical-binding: t; -*-

;;; Commentary:

;; Tests for the fork's usage summary on thought group headers
;; (`fork/agent-shell-fork-usage.el').  Fork tests live in their own
;; files so upstream's test files stay byte-identical.

;;; Code:

(require 'ert)
(require 'cl-lib)
(add-to-list 'load-path
             (expand-file-name
              "../fork" (file-name-directory (or load-file-name
                                                 default-directory))))
(require 'agent-shell-fork)

(ert-deftest agent-shell-fork-usage--summary-test ()
  "Format a compact usage summary with only the sections present."
  ;; Full data: context, tokens (only non-zero parts), cost with currency.
  (let ((summary (agent-shell-fork-usage--summary
                  '((:context-used . 45000)
                    (:context-size . 200000)
                    (:input-tokens . 92000)
                    (:output-tokens . 12000)
                    (:cached-read-tokens . 1048576)
                    (:total-tokens . 1048576)
                    (:cost-amount . 0.06)
                    (:cost-currency . "$")))))
    (should (equal (substring-no-properties summary)
                   ", Context: 45k/200k, Tokens: 92k in · 12k out · 1m cached (1m total), Cost: $0.06")))
  ;; Mid-turn: only context data so far.
  (should (equal (substring-no-properties
                  (agent-shell-fork-usage--summary
                   '((:context-used . 45000) (:context-size . 200000))))
                 ", Context: 45k/200k"))
  ;; Context size unknown renders "?".
  (should (equal (substring-no-properties
                  (agent-shell-fork-usage--summary
                   '((:context-used . 45000))))
                 ", Context: 45k/?"))
  ;; Zero cost and zero totals contribute no sections.
  (should (equal (substring-no-properties
                  (agent-shell-fork-usage--summary
                   '((:context-used . 45000)
                     (:context-size . 200000)
                     (:input-tokens . 0)
                     (:output-tokens . 0)
                     (:cost-amount . 0.0))))
                 ", Context: 45k/200k"))
  ;; No data at all -> nil.
  (should-not (agent-shell-fork-usage--summary '())))

(ert-deftest agent-shell-fork-usage--refresh-activity-group-header-test ()
  "Usage summary is appended only to groups holding a thought."
  (cl-flet ((label (thoughts usage &optional tool-calls)
              (let ((state (list (cons :activity-group-count 1)
                                 (cons :activity-thoughts thoughts)
                                 (cons :tool-calls tool-calls)
                                 (cons :usage usage)))
                    (captured nil))
                (cl-letf (((symbol-function 'agent-shell--update-fragment)
                           (lambda (&rest args) (push args captured))))
                  (agent-shell--refresh-activity-group-header state "activity-1"))
                (when captured
                  (substring-no-properties
                   (plist-get (car captured) :label-left))))))
    (let ((usage '((:context-used . 45000)
                   (:context-size . 200000)
                   (:input-tokens . 92000)
                   (:output-tokens . 12000)
                   (:cached-read-tokens . 1048576)
                   (:total-tokens . 1048576)
                   (:cost-amount . 0.06)
                   (:cost-currency . "$")))
          ;; A completed execute call so a thought-free group still renders
          ;; a header label to assert against.
          (tool-calls '(("t1" . ((:group-id . "activity-1")
                                 (:kind . "execute")
                                 (:status . "completed"))))))
      ;; A thought group carries the suffix after its descriptive label.
      (let ((result (label '(( "activity-1" . 1)) usage)))
        (should (string-prefix-p "Thought" result))
        (should (string-match-p
                 "Context: 45k/200k, Tokens: 92k in · 12k out · 1m cached (1m total), Cost: \\$0.06"
                 result)))
      ;; No thought -> label without the usage suffix.
      (let ((result (label nil usage tool-calls)))
        (should (stringp result))
        (should-not (string-match-p "Context:" result)))
      ;; Thought group without usage data -> no suffix either.
      (let ((result (label '(("activity-1" . 1)) '())))
        (should (string-prefix-p "Thought" result))
        (should-not (string-match-p "Context:" result))))))

(ert-deftest agent-shell-fork-usage--on-notification-usage-update-refreshes-header-test ()
  "A usage_update notification updates the current thought group's header.

Targets the live half of the feature: context data arriving mid-turn is
reflected in the thought group's header label."
  (let ((state (list (cons :activity-group-count 1)
                     (cons :activity-thoughts '(("activity-1" . 1)))
                     (cons :last-activity-time nil)
                     (cons :usage (list (cons :total-tokens 0)
                                        (cons :context-used 0)
                                        (cons :context-size 0)
                                        (cons :cost-amount 0.0)))))
        (labels nil))
    (cl-letf (((symbol-function 'agent-shell--update-header-and-mode-line)
               #'ignore)
              ;; `usage_update' also broadcasts a `usage-update' event, which
              ;; needs a real shell buffer; this test only covers the header.
              ((symbol-function 'agent-shell--emit-event)
               #'ignore)
              ((symbol-function 'agent-shell--update-fragment)
               (lambda (&rest args) (push (plist-get args :label-left) labels))))
      (agent-shell--on-notification
       :state state
       :acp-notification '((method . "session/update")
                           (params
                            (update
                             (sessionUpdate . "usage_update")
                             (used . 45000)
                             (size . 200000))))))
    (should (= (map-nested-elt state '(:usage :context-used)) 45000))
    (let ((label (substring-no-properties (car (last labels)))))
      (should (string-prefix-p "Thought" label))
      (should (string-match-p "Context: 45k/200k" label)))))

(ert-deftest agent-shell-fork-usage--send-command-refreshes-header-on-turn-end-test ()
  "The turn's usage lands in state and the thought group header refreshes.

Targets the other half of the feature: once `PromptResponse.usage'
arrives, the header suffix picks up the token breakdown."
  (let ((captured-on-success nil)
        (refreshed nil)
        (agent-shell--state (list (cons :buffer (current-buffer))
                                  (cons :event-subscriptions nil)
                                  (cons :client 'test-client)
                                  (cons :session (list (cons :id "test-session")
                                                       (cons :title nil)))
                                  (cons :last-entry-type nil)
                                  (cons :tool-calls nil)
                                  (cons :activity-group-count 1)
                                  (cons :activity-thoughts '(("activity-1" . 1)))
                                  (cons :usage (list (cons :total-tokens 0)))
                                  (cons :idle-timer nil)))
        (agent-shell-show-busy-indicator nil)
        (agent-shell-show-usage-at-turn-end nil))
    (cl-letf (((symbol-function 'agent-shell--state)
               (lambda () agent-shell--state))
              ((symbol-function 'agent-shell--send-request)
               (lambda (&rest args)
                 (setq captured-on-success (plist-get args :on-success))))
              ((symbol-function 'shell-maker-finish-output)
               (lambda (&rest _)))
              ((symbol-function 'agent-shell--prompt-queue-process-next)
               (lambda (&rest _)))
              ((symbol-function 'agent-shell--collapse-expanded-activity-group)
               (lambda (&rest _)))
              ((symbol-function 'agent-shell-fork-usage--refresh-header)
               (lambda (state) (setq refreshed state))))
      (agent-shell--send-command
       :prompt "Hello"
       :shell-buffer (current-buffer))
      (should captured-on-success)
      (funcall captured-on-success
               `((stopReason . "end_turn")
                 (usage . ((totalTokens . 1500)))))
      (should (= (map-nested-elt agent-shell--state '(:usage :total-tokens)) 1500))
      (should (eq refreshed agent-shell--state)))))

(provide 'agent-shell-fork-usage-tests)

;;; agent-shell-fork-usage-tests.el ends here
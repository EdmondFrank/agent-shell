;;; agent-shell-aiderdesk-tests.el --- Tests for AiderDesk integration -*- lexical-binding: t; -*-

;;; Commentary:

;; Tests for the AiderDesk input-history support in
;; `agent-shell-aiderdesk.el'.  History is fetched through the AiderDesk
;; ACP bridge (`session/list_input_history' -> the server's
;; `GET /api/project/input-history'), never parsed from disk.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'agent-shell-project)
(require 'agent-shell-aiderdesk)

;; Set buffer-locally by tests to stand in for comint's prompt marker.
(defvar comint-accum-marker)

(ert-deftest agent-shell-aiderdesk--input-history-request-test ()
  "The bridge request carries the normalized cwd."
  (should (equal
           (agent-shell-aiderdesk--input-history-request "/tmp/proj/")
           '((:method . "session/list_input_history")
             (:params . ((cwd . "/tmp/proj")))))))

(ert-deftest agent-shell-aiderdesk--input-history-request-relative-test ()
  "Relative cwds are expanded to absolute, trailing-slash-free paths."
  (should (equal
           (agent-shell-aiderdesk--input-history-request "proj")
           `((:method . "session/list_input_history")
             (:params . ((cwd . ,(directory-file-name
                                  (expand-file-name "proj")))))))))

(ert-deftest agent-shell-aiderdesk--fetch-input-history-test ()
  "Fetch sends the bridge request and maps its response to entries."
  (let ((sent-request nil)
        (sent-buffer nil))
    (cl-letf (((symbol-function 'agent-shell-cwd) (lambda () "/tmp/proj")))
      (cl-letf (((symbol-function 'acp-send-request)
                 (lambda (&rest args)
                   (setq sent-request (plist-get args :request)
                         sent-buffer (plist-get args :buffer))
                   '((inputs "fix the failing test" "review git staged diff")))))
        (with-temp-buffer
          (setq-local agent-shell--state (list (cons :client 'fake-client)))
          (should (equal (agent-shell-aiderdesk--fetch-input-history (current-buffer))
                         '("fix the failing test" "review git staged diff")))
          (should (equal sent-request
                         '((:method . "session/list_input_history")
                           (:params . ((cwd . "/tmp/proj"))))))
          (should (equal sent-buffer (current-buffer))))))))

(ert-deftest agent-shell-aiderdesk--fetch-input-history-empty-test ()
  "Fetch degrades to nil when the bridge returns no entries."
  (cl-letf (((symbol-function 'agent-shell-cwd) (lambda () "/tmp/proj")))
    (cl-letf (((symbol-function 'acp-send-request)
               (lambda (&rest _args) '((inputs)))))
      (with-temp-buffer
        (setq-local agent-shell--state (list (cons :client 'fake-client)))
        (should (null (agent-shell-aiderdesk--fetch-input-history (current-buffer))))))))

(ert-deftest agent-shell-aiderdesk--fetch-input-history-failure-test ()
  "Fetch degrades to nil when the bridge request fails."
  (cl-letf (((symbol-function 'agent-shell-cwd) (lambda () "/tmp/proj")))
    (cl-letf (((symbol-function 'acp-send-request)
               (lambda (&rest _args) (error "ACP request failed: boom"))))
      (with-temp-buffer
        (setq-local agent-shell--state (list (cons :client 'fake-client)))
        (should (null (agent-shell-aiderdesk--fetch-input-history (current-buffer))))))))

(ert-deftest agent-shell-aiderdesk--fetch-input-history-no-client-test ()
  "Fetch refuses to run when the shell has no ACP client."
  (cl-letf (((symbol-function 'agent-shell-cwd) (lambda () "/tmp/proj")))
    (with-temp-buffer
      (setq-local agent-shell--state nil)
      (should-error (agent-shell-aiderdesk--fetch-input-history (current-buffer))))))

(ert-deftest agent-shell-aiderdesk-input-history-busy-test ()
  "A busy shell refuses and never sends the fetch request."
  (cl-letf (((symbol-function 'agent-shell--current-shell)
             (lambda () (current-buffer)))
            ((symbol-function 'shell-maker-busy) (lambda () t))
            ((symbol-function 'agent-shell--input) (lambda () nil))
            ((symbol-function 'agent-shell-aiderdesk--fetch-input-history)
             (lambda (_shell-buffer) (error "fetch must not run"))))
    (with-temp-buffer
      (should-error (agent-shell-aiderdesk-input-history)
                    :type 'user-error))))

(ert-deftest agent-shell-aiderdesk-input-history-empty-input-test ()
  "Idle shells with no typed input still offer the history."
  (let (seen-seed)
    (cl-letf (((symbol-function 'agent-shell--current-shell)
               (lambda () (current-buffer)))
              ((symbol-function 'shell-maker-busy) (lambda () nil))
              ((symbol-function 'agent-shell--input) (lambda () nil))
              ((symbol-function 'agent-shell-aiderdesk--fetch-input-history)
               (lambda (_shell-buffer) '("prompt A" "prompt B")))
              ((symbol-function 'completing-read)
               (lambda (_prompt _collection _pred _require-match seed _hist)
                 (setq seen-seed seed)
                 "prompt B")))
      (with-temp-buffer
        (insert "typed input")
        (setq-local comint-accum-marker (copy-marker (point-min)))
        (agent-shell-aiderdesk-input-history)
        (should (equal (buffer-string) "prompt B"))
        (should (equal seen-seed ""))))))

(ert-deftest agent-shell-aiderdesk-history-cycling-test ()
  "M-<up> walks older entries; M-<down> walks back and restores input."
  (cl-letf (((symbol-function 'shell-maker-busy) (lambda () nil))
            ((symbol-function 'agent-shell-aiderdesk--fetch-input-history)
             (lambda (_shell-buffer) '("newest" "middle" "oldest"))))
    (with-temp-buffer
      (insert "typed input")
      (setq-local comint-accum-marker (copy-marker (point-min)))
      (agent-shell-aiderdesk-previous-input)
      (should (equal (buffer-string) "newest"))
      (agent-shell-aiderdesk-previous-input)
      (should (equal (buffer-string) "middle"))
      (agent-shell-aiderdesk-previous-input)
      (should (equal (buffer-string) "oldest"))
      (agent-shell-aiderdesk-next-input)
      (should (equal (buffer-string) "middle"))
      (agent-shell-aiderdesk-next-input)
      (should (equal (buffer-string) "newest"))
      (agent-shell-aiderdesk-next-input)
      (should (equal (buffer-string) "typed input"))
      (should (null agent-shell-aiderdesk--history-pos))
      (should (null agent-shell-aiderdesk--history-snapshot))
      (should (null agent-shell-aiderdesk--history-entries)))))

(ert-deftest agent-shell-aiderdesk-history-cache-until-walk-ends-test ()
  "The walk refetches history only after returning to the original input."
  (let ((fetch-count 0))
    (cl-letf (((symbol-function 'shell-maker-busy) (lambda () nil))
              ((symbol-function 'agent-shell-aiderdesk--fetch-input-history)
               (lambda (_shell-buffer)
                 (cl-incf fetch-count)
                 '("newest" "older"))))
      (with-temp-buffer
        (setq-local comint-accum-marker (copy-marker (point-min)))
        (agent-shell-aiderdesk-previous-input)
        (agent-shell-aiderdesk-previous-input)
        (should (equal fetch-count 1))
        (agent-shell-aiderdesk-next-input)
        (agent-shell-aiderdesk-next-input)
        (should (null agent-shell-aiderdesk--history-pos))
        ;; A fresh walk fetches again.
        (agent-shell-aiderdesk-previous-input)
        (should (equal fetch-count 2))))))

(ert-deftest agent-shell-aiderdesk-history-restores-empty-input-test ()
  "Navigating from an empty prompt restores the empty prompt."
  (cl-letf (((symbol-function 'shell-maker-busy) (lambda () nil))
            ((symbol-function 'agent-shell-aiderdesk--fetch-input-history)
             (lambda (_shell-buffer) '("newest"))))
    (with-temp-buffer
      (setq-local comint-accum-marker (copy-marker (point-min)))
      (agent-shell-aiderdesk-previous-input)
      (should (equal (buffer-string) "newest"))
      (agent-shell-aiderdesk-next-input)
      (should (equal (buffer-string) "")))))

(ert-deftest agent-shell-aiderdesk-history-beginning-test ()
  "M-<up> errors past the oldest entry."
  (cl-letf (((symbol-function 'shell-maker-busy) (lambda () nil))
            ((symbol-function 'agent-shell-aiderdesk--fetch-input-history)
             (lambda (_shell-buffer) '("newest" "older"))))
    (with-temp-buffer
      (setq-local comint-accum-marker (copy-marker (point-min)))
      (agent-shell-aiderdesk-previous-input)
      (agent-shell-aiderdesk-previous-input)
      (should-error (agent-shell-aiderdesk-previous-input)
                    :type 'user-error))))

(ert-deftest agent-shell-aiderdesk-history-no-newer-test ()
  "M-<down> without navigating first reports end of history."
  (with-temp-buffer
    (setq-local comint-accum-marker (copy-marker (point-min)))
    (should-error (agent-shell-aiderdesk-next-input)
                  :type 'user-error)))

(ert-deftest agent-shell-aiderdesk-history-empty-test ()
  "M-<up> reports when there is no history."
  (cl-letf (((symbol-function 'shell-maker-busy) (lambda () nil))
            ((symbol-function 'agent-shell-aiderdesk--fetch-input-history)
             (lambda (_shell-buffer) nil)))
    (with-temp-buffer
      (setq-local comint-accum-marker (copy-marker (point-min)))
      (should-error (agent-shell-aiderdesk-previous-input)
                    :type 'user-error))))

(ert-deftest agent-shell-aiderdesk-history-busy-test ()
  "A busy shell refuses history navigation."
  (cl-letf (((symbol-function 'shell-maker-busy) (lambda () t))
            ((symbol-function 'agent-shell-aiderdesk--fetch-input-history)
             (lambda (_shell-buffer) (error "fetch must not run"))))
    (with-temp-buffer
      (setq-local comint-accum-marker (copy-marker (point-min)))
      (should-error (agent-shell-aiderdesk-previous-input)
                    :type 'user-error))))

(ert-deftest agent-shell-aiderdesk-history-outside-input-test ()
  "History navigation is refused when point is above the prompt input."
  (cl-letf (((symbol-function 'shell-maker-busy) (lambda () nil))
            ((symbol-function 'agent-shell-aiderdesk--fetch-input-history)
             (lambda (_shell-buffer) (error "fetch must not run"))))
    (with-temp-buffer
      (insert "conversation output\n")
      (setq-local comint-accum-marker (copy-marker (point-max)))
      (goto-char (point-min))
      (should-error (agent-shell-aiderdesk-previous-input)
                    :type 'user-error))))

(provide 'agent-shell-aiderdesk-tests)
;;; agent-shell-aiderdesk-tests.el ends here
;;; agent-shell-fork-stream-tests.el --- Fork streamed delta tests  -*- lexical-binding: t; -*-

;;; Commentary:

;; Behaviour tests for `fork/agent-shell-fork-stream.el': a body that
;; strictly extends what is already on screen is appended, anything
;; else keeps upstream's full replace.  The tests observe which path
;; the update took (`agent-shell-ui--append-body' versus a replace),
;; not the shim's internals.

;;; Code:

(require 'ert)
(require 'cl-lib)
(add-to-list 'load-path
             (expand-file-name
              "../fork" (file-name-directory (or load-file-name
                                                 default-directory))))
(require 'agent-shell-fork)

(defun agent-shell-fork-stream-tests--insert (body)
  "Insert a label-bearing fragment with BODY."
  (agent-shell-ui-update-fragment
   (agent-shell-ui-make-fragment-model
    :namespace-id "ns" :block-id "1"
    :label-left "Ran" :label-right "cmd"
    :body body)
   :expanded t :navigation 'always))

(defun agent-shell-fork-stream-tests--update (body)
  "Update the test fragment's body to BODY without asking to append."
  (agent-shell-ui-update-fragment
   (agent-shell-ui-make-fragment-model
    :namespace-id "ns" :block-id "1" :body body)
   :expanded t))

(defmacro agent-shell-fork-stream-tests--record-appends (&rest body)
  "Run BODY, returning the chunks `agent-shell-ui--append-body' was given."
  (declare (indent 0))
  `(let ((appended nil))
     (cl-letf (((symbol-function 'agent-shell-ui--append-body)
                (lambda (_range chunk _qualified-id _collapsed)
                  (push chunk appended)
                  nil)))
       ,@body)
     (nreverse appended)))

(ert-deftest agent-shell-fork-stream-appends-a-grown-body-test ()
  "A body that extends the visible one reaches the append path."
  (with-temp-buffer
    (agent-shell-ui-mode 1)
    (agent-shell-fork-stream-tests--insert "line1\n")
    (let ((appended (agent-shell-fork-stream-tests--record-appends
                      (agent-shell-fork-stream-tests--update "line1\nline2\n"))))
      (should (= 1 (length appended)))
      (should (string-match-p "line2" (car appended))))))

(ert-deftest agent-shell-fork-stream-appends-only-the-new-tail-test ()
  "The appended chunk is the grown tail, not the whole accumulated body."
  (with-temp-buffer
    (agent-shell-ui-mode 1)
    (agent-shell-fork-stream-tests--insert "line1\n")
    (let ((appended (agent-shell-fork-stream-tests--record-appends
                      (agent-shell-fork-stream-tests--update
                       "line1\nline2\n"))))
      (should-not (string-match-p "line1" (car appended))))))

(ert-deftest agent-shell-fork-stream-replaces-when-body-is-not-a-extension-test ()
  "A re-derived body that is not an extension keeps the full replace."
  (with-temp-buffer
    (agent-shell-ui-mode 1)
    (agent-shell-fork-stream-tests--insert "line1\n")
    (should-not (agent-shell-fork-stream-tests--record-appends
                  (agent-shell-fork-stream-tests--update "different\n")))))

(ert-deftest agent-shell-fork-stream-replaces-when-content-follows-the-body-test ()
  "Output followed by rendered content (a diff) never appends.

The stored body then holds more than the output, so it is not a prefix
of the new output and appending would interleave the two."
  (with-temp-buffer
    (agent-shell-ui-mode 1)
    (agent-shell-fork-stream-tests--insert "line1\n\nhunk\n")
    (should-not (agent-shell-fork-stream-tests--record-appends
                  (agent-shell-fork-stream-tests--update
                   "line1\nline2\n\nhunk\n")))))

(ert-deftest agent-shell-fork-stream-appends-when-rendering-rewrote-the-body-test ()
  "A body the renderer rewrote still appends on its next chunk.

Markdown drops a heading's `## ' from the buffer, so comparing the
new body against the rendered text would fall back to a full replace
on every chunk -- the case that made a real session re-render tens of
KB per chunk.  The append decision reads the body that was sent, not
what the renderer left behind."
  (with-temp-buffer
    (agent-shell-ui-mode 1)
    (agent-shell-fork-stream-tests--insert "## Heading\n\nline1\n")
    ;; Stand in for the renderer: the heading markup is gone.  Rendering
    ;; runs with `inhibit-read-only' bound, since body chars are
    ;; read-only once inserted.
    (save-excursion
      (let ((inhibit-read-only t))
        (goto-char (point-min))
        (search-forward "## ")
        (delete-region (match-beginning 0) (match-end 0))))
    (let ((appended (agent-shell-fork-stream-tests--record-appends
                      (agent-shell-fork-stream-tests--update
                       "## Heading\n\nline1\nline2\n"))))
      (should (= 1 (length appended)))
      (should (string-match-p "line2" (car appended)))
      (should-not (string-match-p "line1" (car appended))))))

(ert-deftest agent-shell-fork-stream-replaces-when-the-delta-carries-a-fence-test ()
  "A grown tail holding a fence keeps the full replace.

The rendered body has no opening fence left for a closing one to pair
with, so the fence would land in the buffer as literal text."
  (with-temp-buffer
    (agent-shell-ui-mode 1)
    (agent-shell-fork-stream-tests--insert "intro\n")
    (should-not (agent-shell-fork-stream-tests--record-appends
                  (agent-shell-fork-stream-tests--update "intro\n```lua\n")))))

(ert-deftest agent-shell-fork-stream-replaces-when-the-body-did-not-grow-test ()
  "An identical body re-sent has nothing to append."
  (with-temp-buffer
    (agent-shell-ui-mode 1)
    (agent-shell-fork-stream-tests--insert "line1\n")
    (should-not (agent-shell-fork-stream-tests--record-appends
                  (agent-shell-fork-stream-tests--update "line1\n")))))

(ert-deftest agent-shell-fork-stream-keeps-buffers-apart-test ()
  "A fragment in one buffer never appends on another buffer's body.

The viewport mirror renders the same blocks as the shell, so a shared
fingerprint would let one strand the other."
  (with-temp-buffer
    (agent-shell-ui-mode 1)
    (agent-shell-fork-stream-tests--insert "line1\n")
    (with-temp-buffer
      (agent-shell-ui-mode 1)
      ;; Same block id, different buffer: nothing was sent here yet.
      (agent-shell-fork-stream-tests--insert "other\n")
      (should-not (agent-shell-fork-stream-tests--record-appends
                    (agent-shell-fork-stream-tests--update "line1\nline2\n"))))))

(ert-deftest agent-shell-fork-stream-appends-a-multibyte-body-test ()
  "A built body appends whether or not it holds non-ASCII text.

`secure-hash' does not slice a string by character positions, so
digesting a prefix without copying it first mismatches as soon as the
body holds a non-ASCII character -- a checkmark in a thought, a curly
quote in prose.  Real sessions hold both."
  (with-temp-buffer
    (agent-shell-ui-mode 1)
    (agent-shell-fork-stream-tests--insert "⧉ snippet\nline1\n")
    (let ((appended (agent-shell-fork-stream-tests--record-appends
                      (agent-shell-fork-stream-tests--update
                       "⧉ snippet\nline1\nline2\n"))))
      (should (= 1 (length appended)))
      (should (string-match-p "line2" (car appended))))))

(ert-deftest agent-shell-fork-stream-keeps-caller-supplied-append-test ()
  "An update that already asks to append is passed through untouched."
  (with-temp-buffer
    (agent-shell-ui-mode 1)
    (agent-shell-fork-stream-tests--insert "line1\n")
    (let ((appended (agent-shell-fork-stream-tests--record-appends
                      (agent-shell-ui-update-fragment
                       (agent-shell-ui-make-fragment-model
                        :namespace-id "ns" :block-id "1" :body "line2\n")
                       :append t :expanded t))))
      (should (= 1 (length appended)))
      (should (string-match-p "line2" (car appended))))))

(provide 'agent-shell-fork-stream-tests)

;;; agent-shell-fork-stream-tests.el ends here
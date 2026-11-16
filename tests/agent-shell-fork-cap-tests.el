;;; agent-shell-fork-cap-tests.el --- Fork body cap tests  -*- lexical-binding: t; -*-

;;; Commentary:

;; Behaviour tests for `fork/agent-shell-fork-cap.el'.  They assert
;; what the user sees (which lines are visible, where the ellipsis is,
;; what an expand does), not the shape of the fork's internals, so a
;; re-anchored overlay can keep passing them.

;;; Code:

(require 'ert)
(require 'cl-lib)
(add-to-list 'load-path
             (expand-file-name
              "../fork" (file-name-directory (or load-file-name
                                                 default-directory))))
(require 'agent-shell-fork)

(defun agent-shell-fork-cap-tests--visible-text ()
  "Return the buffer text that is not hidden with the `invisible' property."
  (let ((pos (point-min))
        (text ""))
    (while (< pos (point-max))
      (let ((next (next-single-property-change pos 'invisible nil (point-max))))
        (unless (get-text-property pos 'invisible)
          (setq text (concat text (buffer-substring-no-properties pos next))))
        (setq pos next)))
    text))

(defun agent-shell-fork-cap-tests--overlays ()
  "Return the cap ellipsis overlays in the current buffer."
  (seq-filter (lambda (overlay)
                (overlay-get overlay 'agent-shell-fork-cap-marker))
              (overlays-in (point-min) (point-max))))

(defun agent-shell-fork-cap-tests--fragment-state (namespace-id block-id)
  "Return NAMESPACE-ID/BLOCK-ID's fragment state alist."
  (let ((qualified-id (format "%s-%s" namespace-id block-id)))
    (save-mark-and-excursion
      (goto-char (point-min))
      (when-let* ((match (text-property-search-forward
                          'agent-shell-ui-state nil
                          (lambda (_ state)
                            (equal (map-elt state :qualified-id) qualified-id))
                          t)))
        (get-text-property (prop-match-beginning match)
                           'agent-shell-ui-state)))))

(defun agent-shell-fork-cap-tests--goto-fragment (namespace-id block-id)
  "Move point onto NAMESPACE-ID/BLOCK-ID's state-property range."
  (let ((qualified-id (format "%s-%s" namespace-id block-id)))
    (goto-char (point-min))
    (when-let* ((match (text-property-search-forward
                        'agent-shell-ui-state nil
                        (lambda (_ state)
                          (equal (map-elt state :qualified-id) qualified-id))
                        t)))
      (goto-char (prop-match-beginning match)))))

(defun agent-shell-fork-cap-tests--insert (&rest args)
  "Insert a two-member-capable group child from ARGS."
  (apply #'agent-shell-ui-update-fragment
         (list (apply #'agent-shell-ui-make-fragment-model
                      (append args
                              (list :namespace-id "ns" :block-id "1"
                                    :group-id "grp" :group-label "Tools"
                                    :label-left "Ran" :label-right "cmd")))
               :expanded t :navigation 'always)))

(ert-deftest agent-shell-fork-cap-caps-group-child-body-test ()
  "An expanded group child shows the cap, keeping the rest as text."
  (with-temp-buffer
    (agent-shell-ui-mode 1)
    (let ((agent-shell-fork-tool-max-height 2))
      (agent-shell-fork-cap-tests--insert :body "l1\nl2\nl3\nl4\n"))
    (let ((visible (agent-shell-fork-cap-tests--visible-text)))
      (should (string-match-p "l1" visible))
      (should (string-match-p "l2" visible))
      (should-not (string-match-p "l3" visible))
      (should-not (string-match-p "l4" visible)))
    ;; The tail is hidden, not dropped.
    (should (string-match-p "l4" (buffer-substring-no-properties
                                  (point-min) (point-max))))
    (should (= 1 (length (agent-shell-fork-cap-tests--overlays))))))

(ert-deftest agent-shell-fork-cap-leaves-short-bodies-alone-test ()
  "A body that fits under the cap is untouched."
  (with-temp-buffer
    (agent-shell-ui-mode 1)
    (let ((agent-shell-fork-tool-max-height 10))
      (agent-shell-fork-cap-tests--insert :body "l1\nl2\n"))
    (should (string-match-p "l2" (agent-shell-fork-cap-tests--visible-text)))
    (should-not (agent-shell-fork-cap-tests--overlays))))

(ert-deftest agent-shell-fork-cap-does-not-cap-conversation-fragments-test ()
  "A fragment outside an activity group streams unbounded."
  (with-temp-buffer
    (agent-shell-ui-mode 1)
    (let ((agent-shell-fork-tool-max-height 1))
      (agent-shell-ui-update-fragment
       (agent-shell-ui-make-fragment-model
        :namespace-id "ns" :block-id "msg"
        :body "l1\nl2\nl3\n")
       :expanded t :navigation 'always))
    (should (string-match-p "l3" (agent-shell-fork-cap-tests--visible-text)))
    (should-not (agent-shell-fork-cap-tests--overlays))))

(ert-deftest agent-shell-fork-cap-stays-capped-as-it-streams-test ()
  "Streaming past the cap keeps the tail hidden behind one ellipsis."
  (with-temp-buffer
    (agent-shell-ui-mode 1)
    (let ((agent-shell-fork-tool-max-height 2))
      (agent-shell-fork-cap-tests--insert :body "l1\n")
      (dolist (chunk '("l2\n" "l3\n" "l4\n"))
        (agent-shell-ui-update-fragment
         (agent-shell-ui-make-fragment-model
          :namespace-id "ns" :block-id "1" :body chunk)
         :append t :expanded t)))
    (let ((visible (agent-shell-fork-cap-tests--visible-text)))
      (should (string-match-p "l1" visible))
      (should (string-match-p "l2" visible))
      (should-not (string-match-p "l3" visible)))
    (should (= 1 (length (agent-shell-fork-cap-tests--overlays))))))

(ert-deftest agent-shell-fork-cap-toggling-a-capped-body-reveals-it-all-test ()
  "The first toggle on a capped body reveals everything, and it sticks."
  (with-temp-buffer
    (agent-shell-ui-mode 1)
    (let ((agent-shell-fork-tool-max-height 2))
      (agent-shell-fork-cap-tests--insert :body "l1\nl2\nl3\nl4\n"))
    (agent-shell-fork-cap-tests--goto-fragment "ns" "1")
    (agent-shell-ui-toggle-fragment)
    (let ((visible (agent-shell-fork-cap-tests--visible-text)))
      (should (string-match-p "l3" visible))
      (should (string-match-p "l4" visible)))
    (should-not (agent-shell-fork-cap-tests--overlays))
    (should (map-elt (agent-shell-fork-cap-tests--fragment-state "ns" "1")
                     :cap-expanded))
    ;; A later streamed update does not re-cap it.
    (agent-shell-ui-update-fragment
     (agent-shell-ui-make-fragment-model
      :namespace-id "ns" :block-id "1" :body "l1\nl2\nl3\nl4\nl5\n")
     :expanded t)
    (should (string-match-p "l5" (agent-shell-fork-cap-tests--visible-text)))
    (should-not (agent-shell-fork-cap-tests--overlays))))

(ert-deftest agent-shell-fork-cap-thought-and-tool-caps-are-separate-test ()
  "A thought section and a tool section each take their own cap."
  (with-temp-buffer
    (agent-shell-ui-mode 1)
    (let ((agent-shell-fork-tool-max-height 1)
          (agent-shell-fork-thought-max-height 3))
      (agent-shell-fork-cap-tests--insert :body "l1\nl2\nl3\n")
      (agent-shell-ui-update-fragment
       (cons (cons :render-body-images t)
             (agent-shell-ui-make-fragment-model
              :namespace-id "ns" :block-id "thought"
              :group-id "grp" :group-label "Tools"
              :body "t1\nt2\nt3\nt4\n"))
       :expanded t :navigation 'always))
    (should-not (string-match-p "l2" (agent-shell-fork-cap-tests--visible-text)))
    (should (string-match-p "t3" (agent-shell-fork-cap-tests--visible-text)))
    (should-not (string-match-p "t4" (agent-shell-fork-cap-tests--visible-text)))))

(provide 'agent-shell-fork-cap-tests)

;;; agent-shell-fork-cap-tests.el ends here
;;; agent-shell-fork-skill-tests.el --- Fork command payload completion tests  -*- lexical-binding: t; -*-

;;; Commentary:

;; Behaviour tests for `fork/agent-shell-fork-skill.el': a command whose
;; name carries a payload after a colon (AiderDesk's `/skill:<name>') is
;; completed across the colon, popping as soon as it is typed, while
;; every other input keeps the completion upstream would have offered.
;;
;; The region must stay the one upstream uses -- from just after the `/`
;; to point -- because a completion UI filters by the text between its
;; bounds: a zero-width region after the colon is what used to make Corfu
;; drop the session instead of filtering it.

;;; Code:

(require 'ert)
(require 'cl-lib)
(add-to-list 'load-path
             (expand-file-name
              "../fork" (file-name-directory (or load-file-name
                                                 default-directory))))
(require 'agent-shell-fork)

(defun agent-shell-fork-skill-tests--commands ()
  "Return the commands an AiderDesk session advertises with skills."
  '(((name . "help") (description . "Show help"))
    ((name . "compact") (description . "Compact history"))
    ((name . "skill:pdf") (description . "Read a PDF"))
    ((name . "skill:pdf-forms") (description . "Fill in PDF forms"))
    ((name . "skill:brainstorming") (description . "Explore an idea"))))

(defun agent-shell-fork-skill-tests--shell (&optional commands)
  "Return a buffer whose session advertises COMMANDS."
  (let ((shell (generate-new-buffer " *agent-shell-fork-skill-test*")))
    (with-current-buffer shell
      (setq-local agent-shell--state
                  (list (cons :available-commands
                              (or commands (agent-shell-fork-skill-tests--commands))))))
    shell))

(defmacro agent-shell-fork-skill-tests--with-input (shell input &rest body)
  "Run BODY in a temp buffer holding INPUT, sourced from SHELL."
  (declare (indent 2))
  `(unwind-protect
       (with-temp-buffer
         (setq-local agent-shell-completion--shell-buffer ,shell)
         (insert ,input)
         ,@body)
     (kill-buffer ,shell)))

(ert-deftest agent-shell-fork-skill-completes-the-whole-token-test ()
  "The completed region is the command name, `/` excluded, as upstream."
  (let ((shell (agent-shell-fork-skill-tests--shell)))
    (agent-shell-fork-skill-tests--with-input shell "/skill:p"
      (let ((capf (agent-shell--command-completion-at-point)))
        ;; `/skill:p' => the region holds `skill:p', so a candidate
        ;; replaces the name the colon is part of.
        (should (equal (nth 0 capf) 2))
        (should (equal (nth 1 capf) 9))
        (should (equal (nth 2 capf)
                       '("skill:pdf" "skill:pdf-forms" "skill:brainstorming")))))))

(ert-deftest agent-shell-fork-skill-keeps-the-region-across-the-colon-test ()
  "The region never shrinks to nothing when the colon is typed.

A zero-width region has no prefix to filter by, and Corfu quits such a
session on the next continuation check -- the bug this guards."
  (let ((shell (agent-shell-fork-skill-tests--shell)))
    (agent-shell-fork-skill-tests--with-input shell "/skill"
      (let* ((before (agent-shell--command-completion-at-point))
             (before-start (nth 0 before)))
        (goto-char (point-max))
        (insert ":")
        (let ((after (agent-shell--command-completion-at-point)))
          (should (= (nth 0 after) before-start))
          (should (< (nth 0 after) (nth 1 after)))
          (should (= (nth 1 after) (point))))))))

(ert-deftest agent-shell-fork-skill-offers-only-the-matched-prefix-test ()
  "Names of another `prefix:' command are never mixed in."
  (let* ((extra '(((name . "mode:fast") (description . "Faster answers"))
                  ((name . "mode:careful") (description . "Slower answers"))))
         (shell (agent-shell-fork-skill-tests--shell
                 (append (agent-shell-fork-skill-tests--commands) extra))))
    (agent-shell-fork-skill-tests--with-input shell "/mode:fa"
      (should (equal (nth 2 (agent-shell--command-completion-at-point))
                     '("mode:fast" "mode:careful"))))))

(ert-deftest agent-shell-fork-skill-offers-every-name-on-a-bare-colon-test ()
  "A colon alone is enough to offer the payload names."
  (let ((shell (agent-shell-fork-skill-tests--shell)))
    (agent-shell-fork-skill-tests--with-input shell "/skill:"
      (let ((capf (agent-shell--command-completion-at-point)))
        (should (equal (nth 0 capf) 2))
        (should (equal (nth 1 capf) 8))
        (should (equal (nth 2 capf)
                       '("skill:pdf" "skill:pdf-forms" "skill:brainstorming")))))))

(ert-deftest agent-shell-fork-skill-reads-commands-stored-as-a-vector-test ()
  "Commands arrive from ACP as a vector, which has no `cdr' chain.

A list-only iteration here signals inside `completion-at-point-functions',
and the popup dies the moment the colon asks the fork for candidates."
  (let ((shell (agent-shell-fork-skill-tests--shell
                (vconcat (agent-shell-fork-skill-tests--commands)))))
    (agent-shell-fork-skill-tests--with-input shell "/skill:"
      (should (equal (nth 2 (agent-shell--command-completion-at-point))
                     '("skill:pdf" "skill:pdf-forms" "skill:brainstorming"))))))

(ert-deftest agent-shell-fork-skill-skips-a-nameless-entry-test ()
  "An entry without a name does not add a nil candidate."
  (let ((shell (agent-shell-fork-skill-tests--shell
                (list (list (cons 'description "No name here"))
                      (list (cons 'name "skill:pdf")
                            (cons 'description "Read a PDF"))))))
    (agent-shell-fork-skill-tests--with-input shell "/skill:"
      (should (equal (nth 2 (agent-shell--command-completion-at-point))
                     '("skill:pdf"))))))

(ert-deftest agent-shell-fork-skill-annotates-with-descriptions-test ()
  "Each candidate is annotated with the description the agent sent."
  (let ((shell (agent-shell-fork-skill-tests--shell)))
    (agent-shell-fork-skill-tests--with-input shell "/skill:"
      (let* ((capf (agent-shell--command-completion-at-point))
             (annotate (plist-get (nthcdr 3 capf) :annotation-function)))
        (should (equal (funcall annotate "skill:brainstorming")
                       "  Explore an idea"))))))

(ert-deftest agent-shell-fork-skill-exit-inserts-a-space-test ()
  "Accepting a payload leaves the prompt ready for its arguments."
  (let ((shell (agent-shell-fork-skill-tests--shell)))
    (agent-shell-fork-skill-tests--with-input shell "/skill:"
      (let ((exit (plist-get (nthcdr 3 (agent-shell--command-completion-at-point))
                             :exit-function)))
        ;; The candidate is in the buffer by the time the exit function runs.
        (insert "pdf")
        (funcall exit "skill:pdf" 'finished)
        (should (equal (buffer-string) "/skill:pdf "))))))

(ert-deftest agent-shell-fork-skill-leaves-plain-commands-to-upstream-test ()
  "A command without a colon still completes the upstream way."
  (let ((shell (agent-shell-fork-skill-tests--shell)))
    (agent-shell-fork-skill-tests--with-input shell "/he"
      (let ((capf (agent-shell--command-completion-at-point)))
        (should (equal (nth 0 capf) 2))
        (should (equal (nth 1 capf) 4))
        (should (equal (nth 2 capf)
                       '("help" "compact" "skill:pdf" "skill:pdf-forms"
                         "skill:brainstorming")))))))

(ert-deftest agent-shell-fork-skill-leaves-unknown-prefixes-alone-test ()
  "An unadvertised prefix gets nothing from the fork, nor from upstream."
  (let ((shell (agent-shell-fork-skill-tests--shell)))
    (agent-shell-fork-skill-tests--with-input shell "/nope:pd"
      (should-not (agent-shell-fork-skill--capf))
      (should-not (agent-shell--command-completion-at-point)))))

(ert-deftest agent-shell-fork-skill-ignores-a-slash-mid-message-test ()
  "Agents read a slash mid-message as plain text, so no payload completes."
  (let ((shell (agent-shell-fork-skill-tests--shell)))
    (agent-shell-fork-skill-tests--with-input shell "please run /skill:p"
      (should-not (agent-shell-fork-skill--capf)))))

(ert-deftest agent-shell-fork-skill-pops-on-the-colon-test ()
  "Typing the colon pops the payload list when nothing is being completed."
  (dolist (case '(("/skill:" . t)
                  ("/skill:p" . nil)
                  ("/skill" . nil)
                  ("/he" . nil)
                  ("@src/" . nil)))
    (let ((shell (agent-shell-fork-skill-tests--shell))
          (popped nil))
      (agent-shell-fork-skill-tests--with-input shell (car case)
        (cl-letf (((symbol-function 'completion-at-point)
                   (lambda () (setq popped t))))
          (agent-shell--trigger-completion-at-point)
          (should (eq popped (cdr case))))))))

(ert-deftest agent-shell-fork-skill-leaves-a-live-session-alone-test ()
  "A popup already open keeps filtering; popping it again would hide it.

Corfu restarts (and hides) the popup on a fresh `completion-at-point',
so the colon must not restart a session whose region already covers it."
  (let ((shell (agent-shell-fork-skill-tests--shell))
        (completion-in-region-mode t)
        (popped nil))
    (agent-shell-fork-skill-tests--with-input shell "/skill:"
      (cl-letf (((symbol-function 'completion-at-point)
                 (lambda () (setq popped t))))
        (agent-shell--trigger-completion-at-point)
        (should-not popped)))))

(ert-deftest agent-shell-fork-skill-respects-the-upstream-knob-test ()
  "With upstream's command completion off, the colon pops nothing."
  (let ((shell (agent-shell-fork-skill-tests--shell))
        (agent-shell-command-completion-enabled nil)
        (popped nil))
    (agent-shell-fork-skill-tests--with-input shell "/skill:"
      (cl-letf (((symbol-function 'completion-at-point)
                 (lambda () (setq popped t))))
        (agent-shell--trigger-completion-at-point)
        (should-not popped)))))

(ert-deftest agent-shell-fork-skill-switch-off-falls-back-test ()
  "With the fork knob off, a payload completes as little as upstream leaves it."
  (let ((shell (agent-shell-fork-skill-tests--shell))
        (agent-shell-fork-skill-completion-enabled nil))
    (agent-shell-fork-skill-tests--with-input shell "/skill:p"
      (should-not (agent-shell--command-completion-at-point)))))

(provide 'agent-shell-fork-skill-tests)

;;; agent-shell-fork-skill-tests.el ends here
;;; agent-shell-fork-session-title-tests.el --- Session title header tests  -*- lexical-binding: t; -*-

;;; Commentary:

;; What a session title reads as before it reaches the header (a seeded
;; first prompt can span lines, a reported title can be long), where it
;; lands in the text and SVG headers, and that a title arriving in state
;; repaints the header.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'dom)
(require 'svg)
;; Fork tests live beside upstream's, but their sources sit in fork/: add
;; it so a plain `-L .' run (upstream CI) still loads them.
(add-to-list 'load-path
             (expand-file-name
              "../fork" (file-name-directory (or load-file-name buffer-file-name
                                                 default-directory))))
(require 'agent-shell-fork-session-title)

(defun agent-shell-fork-session-title-tests--state (&optional title)
  "Return shell state whose session carries TITLE."
  (list (cons :session (list (cons :title title)))))

(defun agent-shell-fork-session-title-tests--model (&rest plist)
  "Return a header model for the renderer, with PLIST entries overridden."
  (let ((model (list (cons :buffer-name "AiderDesk")
                     (cons :model-name "Sonnet")
                     (cons :mode-name "Edit")
                     (cons :project-name "agent-shell")
                     (cons :width 900)
                     (cons :font-height 20)
                     (cons :font-size 16)
                     (cons :font-family "Menlo")
                     (cons :background-mode 'dark)
                     (cons :busy-indicator-frame nil))))
    (while plist
      (setf (map-elt model (car plist)) (cadr plist))
      (setq plist (cddr plist)))
    model))

(defun agent-shell-fork-session-title-tests--render (model &optional style)
  "Render MODEL through the fork's advised header renderer.

STYLE is `agent-shell-header-style', graphical by default.  The mode
line menus read the shell the header belongs to, so they are stubbed
rather than started."
  (let ((agent-shell-header-style (or style 'graphical)))
    (with-temp-buffer
      (cl-letf (((symbol-function 'agent-shell--shell-buffer) #'current-buffer)
                ((symbol-function 'agent-shell--state)
                 (lambda () (list (cons :session nil))))
                ((symbol-function 'agent-shell--get-available-models)
                 (lambda (&rest _) nil))
                ((symbol-function 'agent-shell--current-model-id)
                 (lambda (&rest _) "model")))
        (agent-shell--render-header-model-uncached model)))))

(defun agent-shell-fork-session-title-tests--image-data (header)
  "Return HEADER's SVG image data, or nil when it is a text header."
  (when-let* ((index (next-single-property-change 0 'display header)))
    (plist-get (cdr (get-text-property index 'display header)) :data)))

(ert-deftest agent-shell-fork-session-title--display-test ()
  "A title is one clean line, shortened to the configured width."
  ;; A seeded first prompt spans lines; only the first is a title.
  (should (equal (agent-shell-fork-session-title--display
                  (agent-shell-fork-session-title-tests--state
                   "Fix   the\nlogin bug"))
                 "Fix the"))
  ;; A reported title can be longer than the header has room for.
  (let ((agent-shell-fork-session-title-max-width 10))
    (should (equal (agent-shell-fork-session-title--display
                    (agent-shell-fork-session-title-tests--state
                     "Fix the login bug"))
                   "Fix the l…")))
  ;; Nothing worth naming the session after.
  (should-not (agent-shell-fork-session-title--display
               (agent-shell-fork-session-title-tests--state nil)))
  (should-not (agent-shell-fork-session-title--display
               (agent-shell-fork-session-title-tests--state "  \n ")))
  ;; Off means off.
  (let ((agent-shell-fork-session-title nil))
    (should-not (agent-shell-fork-session-title--display
                 (agent-shell-fork-session-title-tests--state "Fix the bug")))))

(ert-deftest agent-shell-fork-session-title--make-header-model-test ()
  "The title joins the header model every header is rendered from."
  (should (advice-member-p #'agent-shell-fork-session-title--make-header-model
                           'agent-shell--make-header-model))
  (cl-letf (((symbol-function 'agent-shell-get-model-name) (lambda (&rest _) "Sonnet"))
            ((symbol-function 'agent-shell-get-mode-name) (lambda (&rest _) "Edit"))
            ((symbol-function 'agent-shell-get-thought-level-name) #'ignore)
            ((symbol-function 'agent-shell--current-thought-level-id) #'ignore)
            ((symbol-function 'agent-shell--more-config-options) #'ignore)
            ((symbol-function 'agent-shell--project-name) (lambda () "agent-shell"))
            ((symbol-function 'agent-shell--session-id-indicator) #'ignore)
            ((symbol-function 'agent-shell--context-usage-indicator) #'ignore)
            ((symbol-function 'agent-shell--cost-indicator) #'ignore)
            ((symbol-function 'agent-shell--busy-indicator-frame) #'ignore))
    (should (equal (map-elt (agent-shell--make-header-model
                             (agent-shell-fork-session-title-tests--state "Fix the bug"))
                            :session-title)
                   "Fix the bug"))
    (should-not (map-elt (agent-shell--make-header-model
                          (agent-shell-fork-session-title-tests--state nil))
                         :session-title))))

(ert-deftest agent-shell-fork-session-title-text-header-test ()
  "The text header carries the title after the project directory."
  (let* ((header (agent-shell-fork-session-title-tests--render
                  (agent-shell-fork-session-title-tests--model
                   :session-title "Fix the bug")
                  'text))
         (plain (substring-no-properties header)))
    (should (string-match-p "agent-shell ➤ Fix the bug" plain))
    (should (eq (get-text-property (string-match "Fix the bug" plain)
                                   'font-lock-face header)
                'agent-shell-session-title))))

(ert-deftest agent-shell-fork-session-title-absent-test ()
  "A session without a title keeps upstream's header."
  (let* ((header (agent-shell-fork-session-title-tests--render
                  (agent-shell-fork-session-title-tests--model)
                  'text))
         (plain (substring-no-properties header)))
    (should (equal plain " AiderDesk ➤ Sonnet ➤ Edit ➤ agent-shell"))
    (should-not (text-property-any 0 (length header) 'font-lock-face
                                   'agent-shell-session-title header))))

(ert-deftest agent-shell-fork-session-title-graphical-header-test ()
  "The graphical header draws the title."
  (skip-unless (image-type-available-p 'svg))
  (let ((data (agent-shell-fork-session-title-tests--image-data
               (agent-shell-fork-session-title-tests--render
                (agent-shell-fork-session-title-tests--model
                 :session-title "Fix the bug")))))
    (should (string-match-p "Fix the bug" data))))

(ert-deftest agent-shell-fork-session-title--svg-content-width-test ()
  "The title widens the measured header, exactly once."
  (skip-unless (image-type-available-p 'svg))
  (let ((svg (svg-create 900 60)))
    ;; The header's two rows: the title belongs on the second, short one.
    (svg--append svg (dom-node 'text '((x . "76") (font-size . "16"))
                               (dom-node 'tspan nil "AiderDesk ➤ Sonnet ➤ Edit")))
    (svg--append svg (dom-node 'text '((x . "76") (font-size . "16"))
                               (dom-node 'tspan nil "agent-shell")))
    (let* ((plain (agent-shell--svg-content-width svg))
           (agent-shell-fork-session-title--model
            '((:session-title . "Fix the bug")))
           (titled (agent-shell--svg-content-width svg)))
      (should (> titled plain))
      ;; Measuring the same row again must not draw the title twice.
      (should (= (agent-shell--svg-content-width svg) titled)))))

(ert-deftest agent-shell-fork-session-title-graphical-escapes-test ()
  "A title with markup characters still yields valid SVG."
  (skip-unless (image-type-available-p 'svg))
  (let ((data (agent-shell-fork-session-title-tests--image-data
               (agent-shell-fork-session-title-tests--render
                (agent-shell-fork-session-title-tests--model
                 :session-title "Fix <div> & co")))))
    (should (string-match-p "Fix &lt;div&gt; &amp; co" data))))

(ert-deftest agent-shell-fork-session-title--refresh-test ()
  "A shell buffer repaints its header, outside one nothing happens."
  (let (refreshed)
    (cl-letf (((symbol-function 'derived-mode-p)
               (lambda (&rest _) '(agent-shell-mode)))
              ((symbol-function 'agent-shell--update-header-and-mode-line)
               (lambda (&rest _) (setq refreshed t)))
              ((symbol-function 'agent-shell-viewport--buffer)
               (lambda (&rest _) nil)))
      (agent-shell-fork-session-title--refresh)
      (should refreshed)))
  (let (refreshed)
    (cl-letf (((symbol-function 'agent-shell--update-header-and-mode-line)
               (lambda (&rest _) (setq refreshed t))))
      (with-temp-buffer
        (agent-shell-fork-session-title--refresh))
      (should-not refreshed))))

(ert-deftest agent-shell-fork-session-title--after-set-title-test ()
  "Only a non-empty title, with the feature on, repaints the header."
  (let (refreshed)
    (cl-letf (((symbol-function 'agent-shell-fork-session-title--refresh)
               (lambda () (setq refreshed t))))
      (agent-shell-fork-session-title--after-set-title nil)
      (should-not refreshed)
      (agent-shell-fork-session-title--after-set-title "")
      (should-not refreshed)
      (agent-shell-fork-session-title--after-set-title "Fix the bug")
      (should refreshed)
      (let ((agent-shell-fork-session-title nil))
        (setq refreshed nil)
        (agent-shell-fork-session-title--after-set-title "Fix the bug")
        (should-not refreshed)))))

(provide 'agent-shell-fork-session-title-tests)

;;; agent-shell-fork-session-title-tests.el ends here
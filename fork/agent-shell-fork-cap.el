;;; agent-shell-fork-cap.el --- Cap expanded body height  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Edmond Frank

;; This package is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;;; Commentary:
;;
;; Fork feature: an expanded thought or tool-use section shows at most
;; N lines, so a long streaming body does not push the rest of the
;; conversation off screen.  The whole text stays in the buffer (copy,
;; search and the transcript are unaffected): the tail is hidden with
;; the `invisible' text property, the same mechanism folding uses, and
;; the elision is an empty overlay carrying an `after-string', so no
;; buffer text is added for the markdown renderer to rewrite.
;;
;; Upstream seams used, no upstream file edited:
;;
;; - `agent-shell-ui-update-fragment' is the single insert/update
;;   entry point for a fragment.  An `:after' advice re-derives the cap
;;   for the fragment it touched, which also covers streamed updates
;;   (markdown re-renders a body char and drops the property, so the
;;   cap has to be re-derived rather than assumed).
;; - `agent-shell-ui--toggle-leaf-fragment-at-point' expands a body.
;;   Advising it marks the fragment `:cap-expanded', so asking to see
;;   the whole body wins for the rest of that fragment's life.
;;
;; The fork sets its own cap knobs
;; (`agent-shell-fork-thought-max-height' / `-tool-max-height') rather
;; than the old inline ones, because the model no longer carries a
;; `:max-height' key: the fragment is classified here instead.

;;; Code:

(require 'cl-lib)
(require 'subr-x)

(require 'agent-shell)
(require 'agent-shell-ui)
(require 'agent-shell-fork-util)

(defcustom agent-shell-fork-thought-max-height 15
  "Maximum number of lines an expanded thought process section shows.

Thoughts can run long, and a body left unbounded pushes the rest of the
conversation off screen while it streams.  The body keeps its whole
text (copying it, searching it, and the transcript are unaffected):
lines past this many are hidden behind an ellipsis, and expanding the
section again reveals them all.

Set to nil to show thought bodies in full."
  :type '(choice (const :tag "No limit" nil)
                 (integer :tag "Lines"))
  :group 'agent-shell)

(defcustom agent-shell-fork-tool-max-height 15
  "Maximum number of lines an expanded tool use section shows.

Tool output can run long, and a body left unbounded pushes the rest of
the conversation off screen.  The body keeps its whole text (copying
it, searching it, and the transcript are unaffected): lines past this
many are hidden behind an ellipsis, and expanding the section again
reveals them all.

Set to nil to show tool bodies in full."
  :type '(choice (const :tag "No limit" nil)
                 (integer :tag "Lines"))
  :group 'agent-shell)

(defconst agent-shell-fork-cap--ellipsis " …"
  "Text shown at the end of a capped body's last visible line.")

(defun agent-shell-fork-cap--max-height-for (model)
  "Return the line cap that applies to MODEL's body, or nil.

Only activity-group members are capped: ordinary conversation
fragments stream unbounded.  A group child that renders body images is
a thought section, anything else is tool use."
  (when (or (map-elt model :group-id)
            (map-elt model :group-qualified-id))
    (if (map-elt model :render-body-images)
        agent-shell-fork-thought-max-height
      agent-shell-fork-tool-max-height)))

(defun agent-shell-fork-cap--cap-boundary (body-start body-end max-height)
  "Return where a MAX-HEIGHT-line view of a body ends.

BODY-START and BODY-END delimit the body.  Returns nil when the body
has no more than MAX-HEIGHT lines, or MAX-HEIGHT is not a positive
integer."
  (when (and (integerp max-height)
             (> max-height 0))
    (save-excursion
      (goto-char body-start)
      (forward-line max-height)
      (when (< (point) body-end)
        (point)))))

(defun agent-shell-fork-cap--marker-overlays (beg end)
  "Return the cap elision overlays within [BEG, END)."
  (seq-filter (lambda (overlay)
                (overlay-get overlay 'agent-shell-fork-cap-marker))
              (overlays-in beg end)))

(defun agent-shell-fork-cap--marker-at (position)
  "Return the cap elision overlay at POSITION, if any."
  (seq-find (lambda (overlay)
              (overlay-get overlay 'agent-shell-fork-cap-marker))
            (overlays-at position)))

(defun agent-shell-fork-cap--clear-markers (beg end)
  "Remove the cap elision overlays within [BEG, END)."
  (seq-do #'delete-overlay (agent-shell-fork-cap--marker-overlays beg end)))

(cl-defun agent-shell-fork-cap--set-marker (&key body-start body-end position)
  "Show a capped body's ellipsis at POSITION within [BODY-START, BODY-END).

With POSITION nil, drop the ellipsis instead.  The marker is an empty
overlay carrying `after-string', so it sits after the last visible line
of the body and moves with the text around it."
  (let ((markers (agent-shell-fork-cap--marker-overlays body-start body-end)))
    (if (null position)
        (seq-do #'delete-overlay markers)
      (let ((marker (or (seq-first markers)
                        (make-overlay position position))))
        (seq-do #'delete-overlay (seq-rest markers))
        (move-overlay marker position position)
        (overlay-put marker 'agent-shell-fork-cap-marker t)
        (overlay-put marker 'after-string
                     (propertize agent-shell-fork-cap--ellipsis
                                 'face 'agent-shell-section-annotation
                                 'help-echo "RET shows the rest of this section"))))))

(defun agent-shell-fork-cap--ensure-marker (body-start body-end position)
  "Show the cap ellipsis at POSITION within [BODY-START, BODY-END).

Reuses a marker already sitting there, which is the case on every
streamed chunk once the cut is in place."
  (unless (agent-shell-fork-cap--marker-at position)
    (agent-shell-fork-cap--set-marker :body-start body-start
                                      :body-end body-end
                                      :position position)))

(defun agent-shell-fork-cap--apply (body-start body-end max-height)
  "Show at most MAX-HEIGHT lines of the visible body [BODY-START, BODY-END).

Hides the rest and leaves an ellipsis at the cut.  With MAX-HEIGHT nil,
or a body that already fits, shows the whole body instead."
  (when (and body-start body-end (< body-start body-end))
    (let ((inhibit-read-only t)
          (buffer-undo-list t))
      (if-let* ((boundary (agent-shell-fork-cap--cap-boundary
                           body-start body-end max-height)))
          (progn
            (put-text-property boundary body-end 'invisible t)
            (agent-shell-fork-cap--ensure-marker body-start body-end boundary))
        (remove-text-properties body-start body-end '(invisible nil))
        (agent-shell-fork-cap--set-marker :body-start body-start
                                          :body-end body-end
                                          :position nil)
        (agent-shell-ui--apply-trailing-whitespace-invisible
         body-start body-end)))))

(defun agent-shell-fork-cap--body-cap (state)
  "Return the line cap a fragment with STATE still applies, or nil.

Nil when STATE carries no cap, or the user expanded past it for this
fragment (`:cap-expanded')."
  (when-let* ((max-height (map-elt state :max-height))
              ((not (map-elt state :cap-expanded))))
    max-height))

(defun agent-shell-fork-cap--state-put (state key value)
  "Set KEY to VALUE in the fragment state alist STATE, in place.

`map-put!' refuses a key the alist does not already hold, and the
fragment state is shared through a text property, so the alist itself
has to grow: callers write STATE back onto the block afterwards."
  (if (map-contains-key state key)
      (map-put! state key value)
    (setcdr (last state) (list (cons key value))))
  value)

(defun agent-shell-fork-cap--record (_model body-range max-height)
  "Store MAX-HEIGHT on BODY-RANGE's fragment state, for later re-caps."
  (when-let* ((state (agent-shell-fork-body-state body-range)))
    (let ((inhibit-read-only t)
          (buffer-undo-list t))
      (agent-shell-fork-cap--state-put state :max-height max-height)
      (put-text-property (map-elt body-range :start)
                         (map-elt body-range :end)
                         'agent-shell-ui-state state))))

(defun agent-shell-fork-cap--after-update (&rest args)
  "Cap the body of the fragment the advised update touched.

Re-derives the cut on every insert and streamed update: markdown
rendering rewrites body chars and drops the `invisible' the cap put
there, so an incremental shortcut would drift."
  (ignore-errors
    (let* ((model (car args))
           (body-range (agent-shell-fork-body-range model))
           (state (and body-range (agent-shell-fork-body-state body-range)))
           ;; Streamed updates may carry only the block id, so fall back
           ;; to the cap recorded for this fragment on its first insert.
           (max-height (or (agent-shell-fork-cap--max-height-for model)
                           (map-elt state :max-height))))
      (when (and body-range state)
        (if (map-elt state :cap-expanded)
            (agent-shell-fork-cap--apply (map-elt body-range :start)
                                         (map-elt body-range :end)
                                         nil)
          (agent-shell-fork-cap--record model body-range max-height)
          (when (not (eq (map-elt state :collapsed) t))
            (agent-shell-fork-cap--apply (map-elt body-range :start)
                                         (map-elt body-range :end)
                                         max-height)))))))

(defun agent-shell-fork-cap--fragment-at-point ()
  "Return (STATE . BODY-RANGE) for the leaf fragment at point, or nil.

Nil for a group header: only a leaf has a body to cap."
  (when-let* ((block (agent-shell-ui--block-range :position (point)))
              (start (map-elt block :start))
              (state (get-text-property start 'agent-shell-ui-state))
              ((not (eq (map-elt state :kind) 'group)))
              (body (agent-shell-ui--body-range start (map-elt block :end))))
    (cons state body)))

(defun agent-shell-fork-cap--reveal (state body-range)
  "Show BODY-RANGE's whole body and remember the choice in STATE.

The cap is only the default view, so asking to read the section wins:
`:cap-expanded' keeps later streamed updates from re-capping it."
  (agent-shell-fork-cap--state-put state :cap-expanded t)
  (let ((start (map-elt body-range :start))
        (end (map-elt body-range :end))
        (inhibit-read-only t)
        (buffer-undo-list t))
    (put-text-property start end 'agent-shell-ui-state state)
    (agent-shell-fork-cap--clear-markers start end)
    (remove-text-properties start end '(invisible nil))
    ;; Expanding unhides the body's trailing newlines too.  Left visible,
    ;; the next block counts them as its separator and lands on this
    ;; block's last line, so hide them again as folding does.
    (agent-shell-ui--apply-trailing-whitespace-invisible start end)))

(defun agent-shell-fork-cap--capped-expanded-p ()
  "Return non-nil when the leaf at point shows a capped body."
  (when-let* ((fragment (agent-shell-fork-cap--fragment-at-point)))
    (let* ((state (car fragment))
           (body-range (cdr fragment))
           (start (map-elt body-range :start))
           (end (map-elt body-range :end)))
      (and (not (eq (map-elt state :collapsed) t))
           (not (map-elt state :cap-expanded))
           (agent-shell-fork-cap--marker-overlays start end)))))

(defun agent-shell-fork-cap--toggle-around (fn &rest args)
  "Call FN with ARGS, unless that would fold a capped body.

A capped body takes two presses to read whole: the first reveals what
the cap hides, the next folds it away."
  (if (agent-shell-fork-cap--capped-expanded-p)
      (when-let* ((fragment (agent-shell-fork-cap--fragment-at-point)))
        (agent-shell-fork-cap--reveal (car fragment) (cdr fragment)))
    (apply fn args)))

(defun agent-shell-fork-cap--after-toggle (&rest _)
  "Mark the fragment at point as read in full once it expands.

Expanding a folded body reveals all of it, so the cap does not come
back on the next streamed update."
  (ignore-errors
    (when-let* ((fragment (agent-shell-fork-cap--fragment-at-point))
                (state (car fragment))
                ((not (map-elt state :collapsed))))
      (agent-shell-fork-cap--reveal state (cdr fragment)))))

(advice-add 'agent-shell-ui-update-fragment :after
            #'agent-shell-fork-cap--after-update)
(advice-add 'agent-shell-ui-toggle-fragment :around
            #'agent-shell-fork-cap--toggle-around)
(advice-add 'agent-shell-ui--toggle-leaf-fragment-at-point :after
            #'agent-shell-fork-cap--after-toggle)

(provide 'agent-shell-fork-cap)

;;; agent-shell-fork-cap.el ends here
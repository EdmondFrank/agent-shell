;;; agent-shell-fork-session-title.el --- Session title in the header  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Edmond Frank

;; This package is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This package is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; Fork feature: the shell header names the session, not just the agent
;; and its model.  ACP agents report a human-readable title with
;; `session_info_update' (AiderDesk sends its task name); upstream stores
;; it in `(:session :title)' for the session picker but never paints it.
;; An agent that reports no title has the first prompt seeded as one, so
;; that a session is named either way.
;;
;; Upstream seams used, no upstream file edited:
;;
;; - `agent-shell--make-header-model' builds the header model that both
;;   the shell buffer and the viewport render, so adding `:session-title'
;;   there reaches every header.  The render cache is keyed on the whole
;;   model (`agent-shell--header-cache-key'), so a new title invalidates
;;   it for free.
;; - `agent-shell--render-header-model-uncached' turns that model into a
;;   header: one string for the text style, an SVG image for the
;;   graphical one.  The SVG's rows are `text' elements in document
;;   order, so the title is appended to the second (bottom) row, which is
;;   the short one -- the top row already carries the agent, model, mode
;;   and usage figures and would leave the title no room.
;; - `agent-shell--svg-content-width' reports how far the drawn rows
;;   reach: the canvas is shrunk to that width, so the title is appended
;;   before it is measured and the image grows to hold it.
;; - `agent-shell--set-session-title' is where a title lands (a
;;   `session_info_update', a `session/list' refresh, or the seeded first
;;   prompt), so advice there repaints the header when it changes.
;;
;; See `agent-shell-fork-session-title--display' for what a title reads
;; as and `agent-shell-fork-session-title--refresh' for the repaint.

;;; Code:

(require 'cl-lib)
(require 'dom)
(require 'subr-x)

(require 'agent-shell)
(require 'agent-shell-faces)

(defcustom agent-shell-fork-session-title t
  "Non-nil to show the session title in the shell header.

The title comes from the agent over ACP (`session_info_update';
AiderDesk sends its task name), or from the first prompt when an
agent never reports one.  It is shown after the project directory on
the header's bottom row, in `agent-shell-session-title' face."
  :type 'boolean
  :group 'agent-shell)

(defcustom agent-shell-fork-session-title-max-width 40
  "Characters of the session title to show before shortening it."
  :type 'integer
  :group 'agent-shell)

(defvar agent-shell-fork-session-title--model nil
  "Header model being rendered, for the SVG row extender.")

(defvar agent-shell-fork-session-title--measured
  (make-hash-table :test #'eq :weakness 'key)
  "SVG rows that already carry the session title.

Keyed weakly on the row node, so measuring a row again cannot draw the
title twice while a rendered header is still alive.")

(defun agent-shell-fork-session-title--display (state)
  "Return STATE's session title as the header shows it, or nil.

A title seeded from the first prompt can span several lines, so only
its first line is shown; runs of whitespace collapse, and a title
longer than `agent-shell-fork-session-title-max-width' is shortened
with an ellipsis."
  (when agent-shell-fork-session-title
    (when-let* ((raw (map-nested-elt state '(:session :title)))
                ((stringp raw))
                ((not (string-empty-p (string-trim raw))))
                (line (car (split-string (string-trim raw) "\n")))
                (line (string-trim (replace-regexp-in-string "[ \t]+" " " line))))
      (unless (string-empty-p line)
        (let ((width (max 1 agent-shell-fork-session-title-max-width)))
          (if (> (length line) width)
              (concat (substring line 0 (1- width)) "…")
            line))))))

(defun agent-shell-fork-session-title--make-header-model (fn state &rest args)
  "Add the session title to the header model FN builds for STATE.

ARGS are FN's remaining arguments (its keyword arguments)."
  (let ((model (apply fn state args)))
    (if-let* ((title (agent-shell-fork-session-title--display state)))
        (append model (list (cons :session-title title)))
      model)))

(defun agent-shell-fork-session-title--xml-escape (text)
  "Return TEXT escaped for an SVG text node.

`svg-print' writes text nodes verbatim, so the entities are built
here.  The ampersand goes first, or the entities built afterwards
would be escaped a second time."
  (replace-regexp-in-string
   ">" "&gt;"
   (replace-regexp-in-string
    "<" "&lt;"
    (replace-regexp-in-string "&" "&amp;" text))))

(defun agent-shell-fork-session-title--svg-content-width (fn svg)
  "Append the session title to SVG's bottom row, then measure it with FN.

`agent-shell--svg-content-width' sizes the header image to the text it
finds, so the title is appended before FN counts.  The row remembers
the title it carries, so measuring it again cannot draw it twice."
  (when-let* ((title (map-elt agent-shell-fork-session-title--model
                              :session-title))
              (row (and (consp svg) (cadr (dom-by-tag svg 'text))))
              ((not (gethash row agent-shell-fork-session-title--measured))))
    (puthash row t agent-shell-fork-session-title--measured)
    (dom-append-child
     row
     (dom-node 'tspan
               `((fill . ,(agent-shell--svg-fill-color 'default))
                 (dx . "8"))
               "➤"))
    (dom-append-child
     row
     (dom-node 'tspan
               `((fill . ,(agent-shell--svg-fill-color 'agent-shell-session-title))
                 (dx . "8"))
               (agent-shell-fork-session-title--xml-escape title))))
  (funcall fn svg))

(defun agent-shell-fork-session-title--directory-end (header)
  "Return the position just past HEADER's project directory, or nil.

The directory is found by its `agent-shell-session-directory' face
rather than by matching its text, which could also appear in the
buffer name or model."
  (let* ((length (length header))
         (start (text-property-any 0 length 'font-lock-face
                                   'agent-shell-session-directory header)))
    (when start
      (or (text-property-not-all start length 'font-lock-face
                                 'agent-shell-session-directory header)
          length))))

(defun agent-shell-fork-session-title--text-header (header title)
  "Return HEADER with TITLE appended to its project directory."
  (if-let* ((end (agent-shell-fork-session-title--directory-end header)))
      (concat (substring header 0 end)
              " ➤ "
              (propertize title 'font-lock-face 'agent-shell-session-title)
              (substring header end))
    header))

(defun agent-shell-fork-session-title--image-header-p (header)
  "Return non-nil when HEADER is an SVG image rather than plain text."
  (and (stringp header)
       (next-single-property-change 0 'display header)))

(defun agent-shell-fork-session-title--render-header (fn model)
  "Render MODEL with its session title, using FN to build the header.

The graphical header is an SVG image whose rows
`agent-shell-fork-session-title--svg-content-width' extends while FN
draws it.  The text header is a single string, spliced here.  With no
SVG support the graphical style falls back to text, so the splice
covers that case too."
  (let* ((agent-shell-fork-session-title--model model)
         (header (funcall fn model))
         (title (map-elt model :session-title)))
    (if (and title (not (agent-shell-fork-session-title--image-header-p header)))
        (agent-shell-fork-session-title--text-header header title)
      header)))

(defun agent-shell-fork-session-title--refresh ()
  "Repaint the shell header and any viewport showing this session.

A full update is required: the heartbeat path reuses the last header
model and refreshes only the busy frame, so a changed title would not
otherwise reach the header.  A title that lands after the last turn
would likewise reach the viewport only when its next update arrives."
  (when (derived-mode-p 'agent-shell-mode)
    (ignore-errors (agent-shell--update-header-and-mode-line))
    (when (and (fboundp 'agent-shell-viewport--buffer)
               (fboundp 'agent-shell-viewport--update-header))
      (when-let* ((viewport (agent-shell-viewport--buffer
                             :shell-buffer (current-buffer)
                             :existing-only t)))
        (with-current-buffer viewport
          (ignore-errors (agent-shell-viewport--update-header)))))))

(defun agent-shell-fork-session-title--after-set-title (title)
  "Repaint the header after TITLE lands in state."
  (when (and agent-shell-fork-session-title
             (stringp title)
             (not (string-empty-p title)))
    (agent-shell-fork-session-title--refresh)))

(advice-add 'agent-shell--make-header-model :around
            #'agent-shell-fork-session-title--make-header-model)
(advice-add 'agent-shell--render-header-model-uncached :around
            #'agent-shell-fork-session-title--render-header)
(advice-add 'agent-shell--svg-content-width :around
            #'agent-shell-fork-session-title--svg-content-width)
(advice-add 'agent-shell--set-session-title :after
            #'agent-shell-fork-session-title--after-set-title)

(provide 'agent-shell-fork-session-title)

;;; agent-shell-fork-session-title.el ends here
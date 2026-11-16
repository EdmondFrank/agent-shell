;;; agent-shell-fork-util.el --- Shared helpers for the fork overlay  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Edmond Frank

;; This package is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;;; Commentary:
;;
;; Locating a fragment's body from the model it was built from, the way
;; upstream does it: a qualified id (`NAMESPACE-BLOCK-ID') is the block
;; cache key, and `agent-shell-ui--body-range' steps over the sections
;; above the body rather than walking a streamed body's intervals.

;;; Code:

(require 'cl-lib)
(require 'subr-x)

(require 'agent-shell-ui)

(defun agent-shell-fork-qualified-id (model)
  "Return MODEL's fragment qualified id."
  (format "%s-%s"
          (or (map-elt model :namespace-id) "global")
          (map-elt model :block-id)))

(defun agent-shell-fork-body-range (model)
  "Return MODEL's body range as an alist with `:start' and `:end'.

Nil when the block is not in the buffer's block cache (a fragment that
was erased, or a viewport rebuild), which callers treat as no-op."
  (when-let* ((block (agent-shell-ui--cached-block
                      (agent-shell-fork-qualified-id model)))
              (start (marker-position (map-elt block :start)))
              (end (marker-position (map-elt block :end)))
              ((< start end)))
    (agent-shell-ui--body-range start end)))

(defun agent-shell-fork-body-state (body-range)
  "Return the fragment state tagging BODY-RANGE's chars, or nil."
  (when-let* ((start (map-elt body-range :start))
              (end (map-elt body-range :end))
              ((< start end)))
    (get-text-property start 'agent-shell-ui-state)))

(provide 'agent-shell-fork-util)

;;; agent-shell-fork-util.el ends here
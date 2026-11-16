;;; agent-shell-fork-stream.el --- Append streamed tool output deltas  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Edmond Frank

;; This package is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;;; Commentary:
;;
;; Fork feature: agents stream tool output by re-sending the whole
;; accumulated text on every `tool_call_update'.  Upstream replaces the
;; fragment body with that text on each chunk, so the body is
;; re-rendered every time.  Upstream's `agent-shell-ui-update-fragment'
;; already supports `:append', used for streamed conversation text; the
;; fork extends the same cheap path to tool bodies.
;;
;; Upstream seam used, no upstream file edited:
;; `agent-shell-ui-update-fragment' is advised with `:filter-args'.
;; When the incoming body is a strict extension of the body already in
;; the buffer, the advice trims it to the grown tail and sets `:append',
;; which routes the update through `agent-shell-ui--append-body': only
;; the delta is rendered, at O(new-chunk) instead of O(body).
;;
;; Falling back to a full replace is always correct, so anything the
;; shim is unsure about -- a body that is not a strict extension (a
;; diff rendered after the output, a re-derived reset, a truncated
;; long line, an in-progress elicitation form) -- keeps upstream's
;; behaviour untouched.

;;; Code:

(require 'cl-lib)
(require 'subr-x)

(require 'agent-shell-ui)
(require 'agent-shell-fork-util)

(defun agent-shell-fork-stream--delta (model)
  "Return the grown tail of MODEL's body, or nil to keep a full replace.

Compares the body text already in the buffer with MODEL's `:body',
both trimmed: upstream wraps tool output in `\\n\\n' padding that the
body trims away, and tracking the raw text instead lets that trim eat
whitespace at a chunk seam (``line1\\n'' followed by ``line2\\n'' would
render ``line1line2\\n'')."
  (when-let* ((body (map-elt model :body))
              ((stringp body))
              (body-range (agent-shell-fork-body-range model))
              (start (map-elt body-range :start))
              (end (map-elt body-range :end))
              ((< start end))
              (stored (string-trim (buffer-substring-no-properties start end)))
              ((not (string-empty-p stored)))
              (new (string-trim body))
              ((> (length new) (length stored)))
              ((string-prefix-p stored new)))
    (substring new (length stored))))

(defun agent-shell-fork-stream--filter-args (args)
  "Turn a full-body update into an append of the grown tail when possible.

ARGS is the advised `agent-shell-ui-update-fragment' argument list:
MODEL followed by its keyword arguments.  Returns ARGS unchanged for
anything the append path cannot express."
  (let* ((model (car args))
         (keys (cdr args))
         (body (map-elt model :body)))
    (if (or (plist-get keys :append)
            (not (stringp body)))
        args
      (if-let* ((delta (agent-shell-fork-stream--delta model)))
          (progn
            ;; The body lives in the model, not in the keyword arguments:
            ;; replace it with the grown tail and ask for an append.
            ;; (`map-put!' returns the value, not the map.)
            (map-put! model :body delta)
            (cons model (plist-put (copy-sequence keys) :append t)))
        args))))

(advice-add 'agent-shell-ui-update-fragment :filter-args
            #'agent-shell-fork-stream--filter-args)

(provide 'agent-shell-fork-stream)

;;; agent-shell-fork-stream.el ends here
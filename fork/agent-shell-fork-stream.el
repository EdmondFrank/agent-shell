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
;; The append decision compares the new body with the body sent last
;; time, kept as a fingerprint in `agent-shell-fork-stream--sources',
;; not with the text in the buffer.  Rendering rewrites what it put on
;; screen: a fenced block loses its fences, a heading loses its `#',
;; a table gets padded.  Comparing against the buffer therefore
;; refuses to append for exactly the bodies that cost the most to
;; re-render -- a tool result carrying a snippet -- even though the
;; source text is a strict extension.  Measured on a real session
;; (7 bodies, 40 chunks each), that refusal alone kept the replace
;; path at ~7.0s where appending on the source text takes ~1.9s.
;;
;; Falling back to a full replace is always correct, so anything the
;; shim is unsure about keeps upstream's behaviour untouched:
;;
;; - a body that does not extend the previous one (a re-derived reset,
;;   a diff rendered after the output, a re-sent identical body);
;; - a grown tail that carries a code fence: the rendered body has no
;;   opening fence left for it to close, so the fence would land
;;   literally in the buffer;
;; - an update that asks to create a new block;
;; - an update that already asks to append.
;;
;; Upstream seam used, no upstream file edited:
;; `agent-shell-ui-update-fragment' is advised with `:filter-args'.
;; When the incoming body is a strict extension of the last one, the
;; advice trims it to the grown tail and sets `:append', which routes
;; the update through `agent-shell-ui--append-body': only the delta is
;; rendered, at O(new-chunk) instead of O(body).

;;; Code:

(require 'cl-lib)
(require 'subr-x)

(require 'agent-shell-ui)

(defconst agent-shell-fork-stream--fence-regexp "^[ \t]*```"
  "Regexp matching a line that carries a code fence.")

(defvar agent-shell-fork-stream--sources (make-hash-table :test #'eq :weakness 'key)
  "Fingerprint of the last body streamed into each buffer's fragments.

Keyed by buffer, each value is a table of BLOCK-KEY to (LENGTH .
SHA256): the length and digest of the body text last handed to that
fragment, trimmed the way `agent-shell-ui--replace-body' trims it.

Weak keys let a killed buffer's table go with it, so a long session
does not hold the last body of every fragment it ever showed.")

(defun agent-shell-fork-stream--key (model)
  "Return MODEL's fragment key, or nil when it has no block id.

The key is (BUFFER NAMESPACE-ID BLOCK-ID): the same block id can name
a different fragment in another buffer (the viewport mirror renders
the shell's blocks), and each needs its own fingerprint."
  (when-let* ((block-id (map-elt model :block-id)))
    (list (current-buffer)
          (map-elt model :namespace-id)
          block-id)))

(defun agent-shell-fork-stream--bucket (key)
  "Return the per-fragment fingerprint table of KEY's buffer.

Created on demand: `agent-shell-fork-stream--sources' starts empty so
loading the module allocates nothing."
  (or (gethash (car key) agent-shell-fork-stream--sources)
      (puthash (car key)
               (make-hash-table :test #'equal)
               agent-shell-fork-stream--sources)))

(defun agent-shell-fork-stream--remember (key text)
  "Record TEXT as the body last sent to KEY's fragment."
  (puthash (cdr key)
           (cons (length text) (secure-hash 'sha256 text))
           (agent-shell-fork-stream--bucket key))
  text)

(defun agent-shell-fork-stream--extends-p (text previous)
  "Return non-nil when TEXT begins with the body PREVIOUS fingerprints.

PREVIOUS is a (LENGTH . SHA256) pair from
`agent-shell-fork-stream--remember'.  Only TEXT's first LENGTH
characters are digested, so a body never has to be kept around to
answer this.

The prefix has to be copied before hashing: `secure-hash''s START and
END describe a *buffer* region, and are ignored for a string."
  (and previous
       (> (length text) (car previous))
       (string= (secure-hash 'sha256 (substring text 0 (car previous)))
                (cdr previous))))

(defun agent-shell-fork-stream--delta (key text)
  "Return the grown tail of TEXT for KEY's fragment, or nil.

TEXT is the incoming body, trimmed: upstream wraps tool output in
`\\n\\n' padding that the body trims away.  Nil means this update is
not expressible as an append and upstream's full replace should run."
  (let ((previous (gethash (cdr key) (agent-shell-fork-stream--bucket key))))
    (when (agent-shell-fork-stream--extends-p text previous)
      (let ((delta (substring text (car previous))))
        (unless (string-match-p agent-shell-fork-stream--fence-regexp delta)
          delta)))))

(defun agent-shell-fork-stream--filter-args (args)
  "Turn a full-body update into an append of the grown tail when possible.

ARGS is the advised `agent-shell-ui-update-fragment' argument list:
MODEL followed by its keyword arguments.  Returns ARGS unchanged for
anything the append path cannot express.  Every body seen here is
remembered, appended or not, so the next chunk is compared with what
this one actually sent."
  (let* ((model (car args))
         (keys (cdr args))
         (body (map-elt model :body)))
    (if (or (plist-get keys :append)
            (plist-get keys :create-new)
            (not (stringp body)))
        args
      (let* ((key (agent-shell-fork-stream--key model))
             (text (string-trim body))
             (delta (and key (agent-shell-fork-stream--delta key text))))
        (when key
          (agent-shell-fork-stream--remember key text))
        (if delta
            (progn
              ;; The body lives in the model, not in the keyword
              ;; arguments: replace it with the grown tail and ask for
              ;; an append.  (`map-put!' returns the value, not the
              ;; map.)
              (map-put! model :body delta)
              (cons model (plist-put (copy-sequence keys) :append t)))
          args)))))

(advice-add 'agent-shell-ui-update-fragment :filter-args
            #'agent-shell-fork-stream--filter-args)

(provide 'agent-shell-fork-stream)

;;; agent-shell-fork-stream.el ends here
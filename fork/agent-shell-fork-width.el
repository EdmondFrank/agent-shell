;;; agent-shell-fork-width.el --- Cap the width of a tool-output line  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Edmond Frank

;; This package is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;;; Commentary:
;;
;; Fork feature: upstream shortens tool output only past 32 768
;; characters on one line (issue #839), which leaves the band that
;; actually shows up untouched.  A real session held 22 lines over
;; 2 000 characters, the longest 18 866 -- and not one over upstream's
;; limit, so its cap never fired.
;;
;; A line that long costs twice over.  Parsing it is linear in its
;; length (a 1 MB line took 738ms to render and 5.6s to fontify as a
;; fenced block), and redisplaying it wraps it to hundreds of screen
;; lines, so the height cap on a section cannot hide it either.
;;
;; Capping the width before the body renders fixes both.  On the same
;; session, replaying 7 bodies in 40 cumulative chunks: 3.9s without
;; the cap, 2.5s with it (the rest is the streamed-delta append); the
;; single 18 866-character line went from 2.3s to 0.15s.
;;
;; Upstream seam used, no upstream file edited: an `:around' advice on
;; `agent-shell--truncate-tool-output-lines', the function every tool
;; body passes through before it reaches a fragment.  The fork's cap
;; only ever tightens: it applies when it is below upstream's, so
;; raising the knob cannot make a line longer than upstream allows.
;;
;; The shortened text is what the transcript records too, exactly as
;; upstream shortens it past its own limit: the middle of the line is
;; replaced by an omitted-character count, keeping
;; `agent-shell-fork-tool-line-context' characters of each end.

;;; Code:

(require 'cl-lib)
(require 'subr-x)

(require 'agent-shell)

;; Upstream owns the limits this module tightens.  Declared so the
;; byte-compiler does not warn when they are read below, and so a
;; rename upstream leaves the fork running on its own knob.
(defvar agent-shell--tool-output-max-line-length)

(defcustom agent-shell-fork-tool-max-line-width 4000
  "Maximum number of characters in a displayed tool output line.

A longer line keeps its beginning and its end, with a count of the
characters omitted between them (see
`agent-shell-fork-tool-line-context').

Lowering this bounds both the parsing work an output line costs and
the number of screen lines it can occupy.  4000 sits below every line
that made a real session crawl -- the longest was 18 866 -- while
still keeping an ordinary JSON or log line whole.

Set to nil to keep upstream's 32 768-character limit."
  :type '(choice (const :tag "Upstream limit" nil)
                 (integer :tag "Characters per line"))
  :group 'agent-shell)

(defcustom agent-shell-fork-tool-line-context 200
  "Number of characters kept at each end of a shortened tool output line.

Mirrors upstream's `agent-shell--tool-output-truncation-context-length',
which this module cannot reuse: the context belongs to the limit the
fork chose, not to upstream's."
  :type 'integer
  :group 'agent-shell)

(defun agent-shell-fork-width--long-line-p (output limit)
  "Return non-nil when OUTPUT holds a line longer than LIMIT characters.

In a string, `^' matches at the start and after every newline, so one
scan answers the question for the whole body.  It is a fast path worth
keeping: most tool bodies have no long line at all, and skipping the
split there is what makes an unchanged body cost nothing."
  (string-match-p (format "^[^\n]\\{%d,\\}" (1+ limit)) output))

(defun agent-shell-fork-width--shorten-line (line limit context)
  "Return LINE shortened to LIMIT characters, keeping CONTEXT at each end."
  (if (<= (length line) limit)
      line
    ;; Never let the two ends overlap, whatever the knob says.
    (let ((context (min context (/ limit 2))))
      (format "%s ... [%d characters omitted] ... %s"
              (substring line 0 context)
              (- (length line) (* 2 context))
              (substring line (- (length line) context))))))

(defun agent-shell-fork-width--truncate (output limit)
  "Return OUTPUT with every line longer than LIMIT characters shortened.

Lines that fit, and the line breaks themselves, are left exactly as
they were, so the body's shape -- and the line count the height cap
sees -- is unchanged.  OUTPUT itself is returned when no line is long
enough to shorten."
  (if (not (agent-shell-fork-width--long-line-p output limit))
      output
    (mapconcat (lambda (line)
                 (agent-shell-fork-width--shorten-line
                  line limit agent-shell-fork-tool-line-context))
               (split-string output "\n")
               "\n")))

(defun agent-shell-fork-width--limit ()
  "Return the line width to apply, or nil to leave upstream's limit.

Nil when the knob is off, and never above upstream's own limit: a
fork cap looser than upstream's would undo it."
  (when (and (integerp agent-shell-fork-tool-max-line-width)
             (> agent-shell-fork-tool-max-line-width 0))
    (if (and (boundp 'agent-shell--tool-output-max-line-length)
             (integerp agent-shell--tool-output-max-line-length))
        (min agent-shell-fork-tool-max-line-width
             agent-shell--tool-output-max-line-length)
      agent-shell-fork-tool-max-line-width)))

(defun agent-shell-fork-width--around (fn output)
  "Call FN on OUTPUT after capping its lines at the fork's width.

FN is `agent-shell--truncate-tool-output-lines'.  With the knob off,
OUTPUT is handed to FN untouched."
  (if-let* ((limit (agent-shell-fork-width--limit))
            ((stringp output)))
      (agent-shell-fork-width--truncate output limit)
    (funcall fn output)))

(advice-add 'agent-shell--truncate-tool-output-lines :around
            #'agent-shell-fork-width--around)

(provide 'agent-shell-fork-width)

;;; agent-shell-fork-width.el ends here
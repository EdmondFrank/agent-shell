;;; agent-shell-fork-usage.el --- Usage summary on thought group headers  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Edmond Frank

;; This package is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;;; Commentary:
;;
;; Fork feature: the header of an activity group that holds a thought
;; carries a compact usage suffix (context, tokens, cost) that tracks
;; the live `:usage' in state.
;;
;; Upstream seams used, no upstream file edited:
;;
;; - `agent-shell-activity-group-header-label-function' is a plain
;;   variable holding the label renderer, so wrapping it appends the
;;   suffix wherever a group label is produced.
;; - `agent-shell--save-usage' and
;;   `agent-shell--update-usage-from-notification' (both in
;;   agent-shell-usage.el) are the two places usage lands, so advice on
;;   them refreshes the header.
;;
;; See `agent-shell-fork-usage--summary' and
;; `agent-shell-fork-usage--refresh-header' (moved from the fork's old
;; inline edits).

;;; Code:

(require 'cl-lib)
(require 'subr-x)

(require 'agent-shell)
(require 'agent-shell-usage)

(defconst agent-shell-fork-usage--label-function
  agent-shell-activity-group-header-label-function
  "Label renderer to wrap.

Captured once, at load, so reloading this file never wraps the wrapper.")

(defun agent-shell-fork-usage--summary (usage)
  "Return a compact usage summary for appending to a header.

USAGE is the session usage alist stored in state's `:usage' (token
counts, context used/size, cost).  Returns a propertized string
with comma-separated sections for the data present -- Context,
Tokens, Cost -- prefixed with \", \", or nil when USAGE carries no
data.  Zero values contribute no sections: the token breakdown
shows only non-zero in/out/thought/cached counts (total in
parentheses when present), cost appears only for a non-zero
amount, and an unknown context size renders as \"?\".  With no
data at all, nil is returned."
  (let* ((context-used (or (map-elt usage :context-used) 0))
         (context-size (or (map-elt usage :context-size) 0))
         (input-tokens (or (map-elt usage :input-tokens) 0))
         (output-tokens (or (map-elt usage :output-tokens) 0))
         (thought-tokens (or (map-elt usage :thought-tokens) 0))
         (cached-tokens (or (map-elt usage :cached-read-tokens) 0))
         (total (or (map-elt usage :total-tokens) 0))
         (context (when (or (> context-used 0) (> context-size 0))
                    (format "Context: %s/%s"
                            (agent-shell--format-number-compact context-used)
                            (if (> context-size 0)
                                (agent-shell--format-number-compact context-size)
                              "?"))))
         (tokens
          (when-let* ((parts (delq nil
                                   (list
                                    (when (> input-tokens 0)
                                      (format "%s in" (agent-shell--format-number-compact input-tokens)))
                                    (when (> output-tokens 0)
                                      (format "%s out" (agent-shell--format-number-compact output-tokens)))
                                    (when (> thought-tokens 0)
                                      (format "%s thought" (agent-shell--format-number-compact thought-tokens)))
                                    (when (> cached-tokens 0)
                                      (format "%s cached" (agent-shell--format-number-compact cached-tokens)))))))
            (concat "Tokens: " (string-join parts " · ")
                    (when (> total 0)
                      (format " (%s total)" (agent-shell--format-number-compact total))))))
         (cost (when-let* ((amount (map-elt usage :cost-amount))
                           ((> amount 0)))
                 (format "Cost: %s%.2f" (or (map-elt usage :cost-currency) "$") amount)))
         (sections (delq nil (list context tokens cost))))
    (when sections
      (propertize (concat ", " (string-join sections ", "))
                  'font-lock-face 'agent-shell-secondary))))

(defun agent-shell-fork-usage--refresh-header (state)
  "Refresh the latest activity group's header in STATE when it holds a thought.

Keeps the usage suffix on the current group's header current: called
after usage lands, both from a streamed `usage_update' notification
and from a turn's final usage."
  (when-let* ((group-id (agent-shell--activity-group-latest-id state))
              ((agent-shell--group-has-thought-p state group-id)))
    (agent-shell--refresh-activity-group-header state group-id)))

(defun agent-shell-fork-usage--label (args)
  "Render an activity group label from ARGS, plus the usage suffix.

ARGS is the alist `agent-shell-activity-group-header-label-function'
is called with (`:state', `:group-id').  Delegates to the captured
upstream renderer so upstream's styling wins, then appends the
summary only for groups that hold a thought."
  (when-let* ((label (funcall agent-shell-fork-usage--label-function args)))
    (let ((state (map-elt args :state))
          (group-id (map-elt args :group-id)))
      (if (and state group-id
               (agent-shell--group-has-thought-p state group-id))
          (concat label
                  (or (agent-shell-fork-usage--summary (map-elt state :usage))
                      ""))
        label))))

(defun agent-shell-fork-usage--state-from-args (args)
  "Return the state the advised call was given, or the current shell's.

ARGS is the advised call's argument list.  `agent-shell--state' is
only a fallback: callers such as notification handlers pass their own
state explicitly, and tests drive those handlers with a bare alist."
  (or (and (plist-member args :state) (plist-get args :state))
      (ignore-errors (agent-shell--state))))

(defun agent-shell-fork-usage--after-save-usage (&rest args)
  "Refresh the thought group header after final usage lands."
  (when-let* ((state (agent-shell-fork-usage--state-from-args args)))
    (ignore-errors (agent-shell-fork-usage--refresh-header state))))

(defun agent-shell-fork-usage--after-notification (&rest args)
  "Refresh the thought group header after streamed usage lands."
  (when-let* ((state (agent-shell-fork-usage--state-from-args args)))
    (ignore-errors (agent-shell-fork-usage--refresh-header state))))

(unless (eq agent-shell-activity-group-header-label-function
            #'agent-shell-fork-usage--label)
  (setq agent-shell-activity-group-header-label-function
        #'agent-shell-fork-usage--label))

(advice-add 'agent-shell--save-usage :after
            #'agent-shell-fork-usage--after-save-usage)
(advice-add 'agent-shell--update-usage-from-notification :after
            #'agent-shell-fork-usage--after-notification)

(provide 'agent-shell-fork-usage)

;;; agent-shell-fork-usage.el ends here
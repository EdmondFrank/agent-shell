;;; agent-shell-fork.el --- Fork overlay for agent-shell  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Edmond Frank

;; URL: https://github.com/EdmondFrank/agent-shell

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
;; Entry point for this repository's local fork of agent-shell.
;;
;; Fork rule: no fork code lives in an upstream file.  Upstream-owned
;; files (agent-shell.el, agent-shell-ui.el, agent-shell-markdown.el,
;; agent-shell-usage.el, README.org, tests/agent-shell-*-tests.el) must
;; stay byte-identical to upstream/main, so merging upstream can never
;; conflict there.  Fork behaviour hangs off upstream's documented
;; extension points where they exist, and off advice where they don't.
;;
;; Layout:
;;
;;   fork/agent-shell-fork.el            this file: wiring and registration
;;   fork/agent-shell-fork-aiderdesk.el  AiderDesk agent definition
;;   fork/agent-shell-fork-cap.el        body height caps (advice)
;;   fork/agent-shell-fork-params.el     tool parameters while a call runs (advice)
;;   fork/agent-shell-fork-tool-style.el compact tool-call labels (advice)
;;   fork/agent-shell-fork-stream.el     streamed tool output deltas (advice)
;;   fork/agent-shell-fork-width.el      tool output line width cap (advice)
;;   fork/agent-shell-fork-markdown.el   tree-sitter fenced-block rendering
;;   fork/agent-shell-fork-usage.el      usage summary in group headers
;;   fork/overrides.eld                  advice/override manifest (drift)
;;
;; Load from your init:
;;
;;   (add-to-list 'load-path ".../site-lisp/agent-shell/fork")
;;   (require 'agent-shell-fork)
;;
;; See FORK.org for the divergence inventory and the upstream sync
;; procedure (`fork/bin/fork-sync').

;;; Code:

(require 'cl-lib)
(require 'subr-x)

(defconst agent-shell-fork-directory
  (file-name-directory (or load-file-name buffer-file-name))
  "Directory holding this fork's overlay files.")

(add-to-list 'load-path agent-shell-fork-directory)

(require 'agent-shell-fork-aiderdesk)

;; The agent registry is a plain list of maker functions and the mode
;; keymap is a `defvar-keymap', so the fork registers by appending data
;; after the package loads rather than editing upstream's definitions.
(with-eval-after-load 'agent-shell
  (add-to-list 'agent-shell-agent-configs
               #'agent-shell-aiderdesk-make-agent-config t)
  (keymap-set agent-shell-mode-map "M-<up>" #'agent-shell-aiderdesk-previous-input)
  (keymap-set agent-shell-mode-map "M-<down>" #'agent-shell-aiderdesk-next-input))

;; Behaviour patches.  Each module is optional so a broken or renamed
;; upstream seam degrades to stock behaviour instead of blocking startup.
(dolist (feature '(agent-shell-fork-cap
                   agent-shell-fork-params
                   agent-shell-fork-tool-style
                   agent-shell-fork-stream
                   agent-shell-fork-width
                   agent-shell-fork-markdown
                   agent-shell-fork-usage))
  (require feature nil t))

(provide 'agent-shell-fork)

;;; agent-shell-fork.el ends here
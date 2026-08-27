;;; agent-shell-aiderdesk.el --- AiderDesk agent configurations -*- lexical-binding: t; -*-

;; Copyright (C) 2024 Alvaro Ramirez

;; Author: Alvaro Ramirez https://xenodium.com
;; URL: https://github.com/xenodium/agent-shell

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
;; This file includes AiderDesk-specific configurations.
;;
;; AiderDesk is an Electron desktop application wrapping the Aider AI
;; coding assistant.  This integration requires the
;; aiderdesk-acp-bridge to be installed (an ACP adapter over stdio)
;; and an AiderDesk server running locally.
;;
;; See https://aiderdesk.com for details.

;;; Code:

(eval-when-compile
  (require 'cl-lib))
(require 'shell-maker)
(require 'acp)

(declare-function agent-shell--indent-string "agent-shell")
(declare-function agent-shell-make-agent-config "agent-shell")
(autoload 'agent-shell-make-agent-config "agent-shell")
(declare-function agent-shell--make-acp-client "agent-shell")
(declare-function agent-shell--dwim "agent-shell")

(defcustom agent-shell-aiderdesk-acp-command
  '("aiderdesk-acp-bridge")
  "Command and parameters for the AiderDesk ACP bridge.

The first element is the command name, and the rest are command parameters.

To use a non-default AiderDesk server port, add it as a parameter, for
example (\"aiderdesk-acp-bridge\" \"--port\" \"24337\")."
  :type '(repeat string)
  :group 'agent-shell)

(defcustom agent-shell-aiderdesk-environment
  nil
  "Environment variables for the AiderDesk ACP bridge.

This should be a list of environment variables to be used when
starting the AiderDesk ACP bridge process.

Example usage to set custom environment variables:

  (setq agent-shell-aiderdesk-environment
        (agent-shell-make-environment-variables
         \"AIDER_DESK_PORT\" \"24337\"))"
  :type '(repeat string)
  :group 'agent-shell)

(defun agent-shell-aiderdesk-make-agent-config ()
  "Create an AiderDesk agent configuration.

Returns an agent configuration alist using `agent-shell-make-agent-config'."
  (agent-shell-make-agent-config
   :identifier 'aiderdesk
   :mode-line-name "AiderDesk"
   :buffer-name "AiderDesk"
   :shell-prompt "AiderDesk> "
   :shell-prompt-regexp "AiderDesk> "
   :icon-name "aiderdesk.png"
   :welcome-function #'agent-shell-aiderdesk--welcome-message
   :client-maker (lambda (buffer)
                   (agent-shell-aiderdesk-make-client :buffer buffer))
   :install-instructions "Install the aiderdesk-acp-bridge (see https://github.com/AiderDesk/aiderdesk) and ensure it is on your PATH.  The AiderDesk server must be running (aiderdesk or aiderdesk start) before starting the shell."))

;;;###autoload
(defun agent-shell-aiderdesk-start-agent ()
  "Start an interactive AiderDesk agent shell."
  (interactive)
  (agent-shell--dwim :config (agent-shell-aiderdesk-make-agent-config)
                     :new-shell t))

(cl-defun agent-shell-aiderdesk-make-client (&key buffer)
  "Create an AiderDesk client using BUFFER as context.

The bridge connects to a locally running AiderDesk server, so no
authentication environment variables are required."
  (unless buffer
    (error "Missing required argument: :buffer"))
  (agent-shell--make-acp-client :command (car agent-shell-aiderdesk-acp-command)
                                :command-params (cdr agent-shell-aiderdesk-acp-command)
                                :environment-variables agent-shell-aiderdesk-environment
                                :context-buffer buffer))

(defun agent-shell-aiderdesk--welcome-message (config)
  "Return AiderDesk welcome message using `shell-maker' CONFIG."
  (let ((message (string-trim-left (shell-maker-welcome-message config) "\n")))
    (concat "\n\n"
            (agent-shell--indent-string 4 "AiderDesk")
            "\n\n"
            message)))

(provide 'agent-shell-aiderdesk)

;;; agent-shell-aiderdesk.el ends here
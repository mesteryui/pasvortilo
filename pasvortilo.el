;;; pasvortilo.el --- Password manager interface for pass/gopass -*- lexical-binding: t -*-

;; Author: Oscar
;; Version: 1.0
;; Package-Requires:  ((emacs "26.1") (transient "0.3.0"))
;; Homepage: https://codeberg.org/mester/pasvortilo
;; Keywords: unix, extensions, passwords

;; This file is not part of GNU Emacs

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Password manager using pass or gopass

;;; Code:

(require 'transient)

;; Declaring functions to avoid erros becuase might not be defined at runtime

(declare-function transient-define-prefix "transient" (&rest args))
(declare-function transient-setup "transient" (&rest args))
(declare-function transient-prefix "transient" (&rest args))
(declare-function transient--suffix-only "transient" (&rest args))
(declare-function transient--set-layout "transient" (&rest args))
(declare-function ansi-color-filter-apply "ansi-color" (string))


(defgroup pasvortilo nil
  "Password manager using `pass' or `gopass' as backend."
  :group 'applications
  :prefix "pasvortilo-"
  :link '(url-link :tag "Website" "https://codeberg.org/mester/pasvortilo")
  :version "1.0")

(defcustom pasvortilo-password-manager "pass"
  "Password manager to use between `gopass' and `pass'."
  :type '(choice (const :tag "Pass (Classic)" "pass")
                 (const :tag "Gopass (Modern)" "gopass"))
  :group 'pasvortilo)

(defun pasvortilo-obtain-password (service)
  "Return the password for SERVICE from the configured password manager."
  (let* ((case-fold-search t) (pass (string-trim (shell-command-to-string (format "%s show %s" pasvortilo-password-manager service)))))
    (if (or (string-match-p "not in the password store" pass) (string-match-p "no results" pass))
	(error "Doesn't exist a password for %s" service)
      pass)))

(defmacro pasvortilo-acts-with-pass (act)
  "Macro to define a simple action.
An action that is defined as an ACT."
  (let* ((actb (if (eq act 'copy) 'kill-new act)) (mes (if (eq act 'copy) `(message "Password copied successfully") `(message "Password %sed successfully" ,(symbol-name act)))))
    `(defun ,(intern (format "pasvortilo-%s-pass" (symbol-name act))) (password)
       ,(format "%s%s the PASSWORD in an easy way giving feedback." (capitalize (symbol-name act)) (if (eq act 'insert) " in a new buffer" ""))
       (if password
	   (progn (,actb password)
		  ,mes)
	 (error "A nil password isn't valid")))))

(pasvortilo-acts-with-pass insert)
(pasvortilo-acts-with-pass copy)

(defun pasvortilo-actions (password &optional act)
  "Actions to do with PASSWORD is possible to use ACT to use an action given by parameter."
  (let ((action (or act (completing-read "Accion: " '("Copy" "Insert")))))
    (pcase action
      ("Copy" (pasvortilo-copy-pass password))
      ("Insert" (pasvortilo-insert-pass password))
      ("Create" (pasvortilo-create-new-pass password)))))

(defun pasvortilo-generate-pass (&optional service length symbols?)
  "Generate and store a password.
Optional SERVICE, LENGTH, and SYMBOLS? arguments control the generation."
  (interactive)
  (let* ((entry (or service
                    (read-string "Enter password entry name: ")))
         (len (or length
                  (read-number "Enter desired password length: " 16)))
         (use-symbols (if (null symbols?)
                          (y-or-n-p "Include special symbols? ")
                        symbols?))
         (cmd (cond
               ;; gopass has a native 'generate=' command
               ((string= pasvortilo-password-manager "gopass")
                (format "%s generate -f %s %d %s"
                        pasvortilo-password-manager
                        entry
                        len
                        (if use-symbols "--symbols=true" "")))
               
               ;; pass delegates generation to pwgen internally via 'pass generate'
               ((string= pasvortilo-password-manager "pass")
                (format "%s generate -f %s %d %s"
                        pasvortilo-password-manager
                        entry
                        len
			(if use-symbols "" "--no-symbols")))

               (t (error "Unknown password manager: %s" pasvortilo-password-manager)))))
    (if (yes-or-no-p (format "Generate and store password for '%s'?" entry))
        (let ((exit-code (shell-command cmd "*Pasvortilo Output*" "*Pasvortilo Errors*")))
          (if (zerop exit-code)
              (message "Password successfully generated and saved for: %s" entry)
            (message "⚠️ Error: see *Pasvortilo Errors* buffer for details.")))
      (message "Operation cancelled."))))

(defun pasvortilo-pass-remove (&optional service)
  "Delete a password for SERVICE.If SERVICE is nil, prompt the user."
  (let* ((serv (or service (pasvortilo-select-service)))
         (conf (yes-or-no-p (format "Do you want to remove the password for %s? " serv))))
    (if conf
        (progn
          (shell-command (format "%s rm -f %s" pasvortilo-password-manager serv))
          (message "Password for %s deleted." serv))
      (message "Deletion canceled."))))

(defun pasvortilo-clean-entries (entries)
  "Clean ENTRIES output from `pass ls' or `gopass ls' to usable list."
  (let* ((lines (split-string entries "\n" t))
         (path-stack '())
         (entries '()))
    (dolist (line lines)
      (when (string-match "^\\([ │]*\\)\\(?:├──\\|└──\\) \\(.*\\)$" line)
        (let* ((indent (length (match-string 1 line)))
               (name (match-string 2 line))
               (level (/ indent 4)))
          ;; Acorta o alarga la pila para coincidir con el nivel
          (setq path-stack (seq-take path-stack level))
          (push name path-stack)
          (let ((full-path (string-join (reverse path-stack) "/")))
            (push full-path entries)))))
    (reverse entries)))

(defun pasvortilo-select-service ()
  "Prompt user to select a password entry."
  (let* ((password-entries (pasvortilo-clean-entries (ansi-color-filter-apply (shell-command-to-string (format "%s ls" pasvortilo-password-manager)))))
	 (password-entry (string-trim (completing-read "Password entry: " password-entries nil t))))
    (when password-entry
      password-entry)))

(defun pasvortilo-select-pass (&optional service)
  "Select password entry.and obtain the password.
Only if you have selected a SERVICE."
  (let* ((password-entry (or service (pasvortilo-select-service))))
    (when password-entry
      (pasvortilo-obtain-password password-entry))))

(transient-define-prefix pasvortilo-menu ()
  "Custom menu to do actions in Pasvortilo."
  [["Actions"
    ("c" "Copy password"
     (lambda () (interactive)
       (pasvortilo-actions (pasvortilo-select-pass) "Copy")))
    ("i" "Insert (in buffer) password"
     (lambda () (interactive)
       (pasvortilo-actions (pasvortilo-select-pass) "Insert")))
    ("n" "Insert and store a new password"
     (lambda () (interactive)
       (pasvortilo-create-new-pass)))
    ("r" "Remove password"
     (lambda () (interactive)
       (pasvortilo-pass-remove)))
    ("g" "Generate and store a new password"
     (lambda () (interactive) (pasvortilo-generate-pass)))]
   ["Exit"
    ("q" "Close menu" transient-quit-one)]])

(defun pasvortilo-create-new-pass (&optional service password)
  "Create a new password entry using the SERVICE and PASSWORD specified.
If they aren't given by user the function request them."
  (interactive)
  (let* ((service (or service (read-string "Insert the service you want a password for: ")))
         (pass (or password (read-passwd "Insert the password: ")))
         (proc (start-process pasvortilo-password-manager "*Pass Insert*"
                              pasvortilo-password-manager "insert" "-m" service)))
    (process-send-string proc (format "%s\n" pass))
    (process-send-eof proc)
    (set-process-sentinel
     proc
     (lambda (_ event)
       (when (string= event "finished\n")
         (message "Contraseña para %s guardada exitosamente." service))))))

(defun pasvortilo-about ()
  "Tell about pasvortilo in minibuffer."
  (interactive)
  (message "Version %s of Pasvortilo" (get 'pasvortilo 'custom-version)))

(provide 'pasvortilo)
;;; pasvortilo.el ends here

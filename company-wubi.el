;;; company-wubi.el ---

;; Copyright (C) 2012 Yevgnen Koh

;; Author: Yevgnen Koh <wherejoystarts@gmail.com>
;; Version: 1.0.0
;; Keywords:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;
;;
;; See documentation on https://github.com/yevgnen/company-wubi.

;;; Code:

(eval-when-compile (require 'cl-lib))
(require 'company)
(require 'dash)
(require 's)

(defgroup company-wubi nil
  "Completion back-end for WUBI."
  :group 'company)

(defcustom company-wubi-dict-path
  (expand-file-name "wubi.txt" (file-name-directory (buffer-file-name)))
  "Location of the wubi dict file.

A string containing the name or the full path of the dict."
  :group 'company-wubi
  :type '(file :must-match t)
  :risky t)

(defcustom company-wubi-idle-delay
  0
  "Overwrite the `company-idle-delay' variable."
  :group 'company-wubi)

(defcustom company-wubi-backends
  '(company-wubi)
  "Overwrite the `company-backends' variable."
  :group 'company-wubi
  :type 'list)

(defcustom company-wubi-minimum-prefix-length
  1
  "Overwrite the `company-minimum-prefix-length' variable."
  :group 'company-wubi
  :type 'number)

(defcustom company-wubi-tooltip-limit
  5
  "Overwrite the `company-tooltip-limit' variable."
  :group 'company-wubi
  :type 'number)

(defcustom company-wubi-tooltip-align-annotations
  nil
  "Overwrite the `company-tooltip-align-annotations' variable."
  :group 'company-wubi
  :type 'boolean)

(defcustom company-wubi-auto-complete-chars
  '((input " " output  nil command nil)
    (input "," output "，" command nil)
    (input "." output "。" command nil))
  "The auto complete char mapping in list form.

Use `input' to trigger insert `output' when completing.
The `command' field should be set to `nil'."
  :group 'company-wubi)

;; Internal variables
(defvar company-wubi-table nil)
(defvar company-wubi-enable-p nil)

(define-minor-mode wubi-indication-mode
  "Toggle Wubi indication mode on or off.
Turn Wubi indication mode on if ARG is positive, off otherwise."
  :group 'company-wubi
  :global t
  :lighter " 五")

(defun company-wubi--prefix ()
  "Get a prefix from current position."
  (when (looking-back "[a-z]+" 0 t)
    (let ((str (match-string 0)))
      str)))

(defun company-wubi--candidates (prefix)
  (-distinct
   (-sort (lambda (x y)
            (< (length (get-text-property 0 'code x))
               (length (get-text-property 0 'code y))))
          (-flatten
           (-map (lambda (c)
                   (let ((code (car c))
                         (chars (cdr c)))
                     (-map (lambda (char)
                             (propertize char 'code (substring code (length prefix))))
                           chars)))
                 (-take (* 2 company-tooltip-limit)
                        (-filter (lambda (l)
                                   (string-prefix-p prefix (car l)))
                                 company-wubi-table)))))))

(defun company-wubi--annotation (candidate)
  "Use the company annotation to show the left input codes."
  (get-text-property 0 'code candidate))

(defun company-wubi--load-dict ()
  "Load the WUBI dict."
  (with-temp-buffer
    (insert-file-contents company-wubi-dict-path)
    (setq company-wubi-table
          (-map
           (lambda (l) (s-split " " l))
           (-map
            #'s-trim
            (s-split "\n" (buffer-string)))))))

(defun company-wubi--unload-dict ()
  "Load the WUBI dict."
  (setq company-wubi-table nil))

(defun company-wubi--localize-variable (var val)
  "Use local company setting for better user experiences."
  (set (intern (format "company-wubi--%s" (symbol-name var))) (symbol-value var))
  (set (intern (symbol-name var)) val))

(defun company-wubi--delocalize-variable (var)
  "Restore the local buffer company settings."
  (set (intern (symbol-name var))
       (symbol-value (intern (format "company-wubi--%s" (symbol-name var))))))

(defun company-wubi--bind-auto-complete-keys ()
  "Bind the auto complete keys like `,', `.'."
  (setq company-wubi-auto-complete-chars
        (-map (lambda (mapping)
                (let ((in (plist-get mapping 'input))
                      (out (plist-get mapping 'output)))
                  ;; Save the original bindings
                  (setq mapping (plist-put mapping
                                           'command (lookup-key company-active-map in)))
                  ;; Define new bindings
                  (define-key company-active-map in
                    `(lambda ()
                       (interactive)
                       (company-complete-selection)
                       (if ,out
                           (insert ,out))))
                  mapping))
              company-wubi-auto-complete-chars)))

(defun company-wubi--unbind-auto-complate-keys ()
  "Unbind all auto complete keys."
  (-map (lambda (mapping)
          (let ((in (plist-get mapping 'input)))
            (define-key company-active-map in
              (plist-get company-wubi-auto-complete-chars 'command))
            (setq mapping (plist-put mapping 'command nil))))
        company-wubi-auto-complete-chars))

(defun company-wubi-enable ()
  "Enable the wubi input method."
  (interactive)
  ;; Load the input dict
  (unless company-wubi-enable-p
    (company-wubi--load-dict)

    ;; Set company for better input experience
    (company-wubi--localize-variable 'company-idle-delay company-wubi-idle-delay)
    (company-wubi--localize-variable 'company-backends company-wubi-backends)
    (company-wubi--localize-variable 'company-minimum-prefix-length company-wubi-minimum-prefix-length)
    (company-wubi--localize-variable 'company-tooltip-limit company-wubi-tooltip-limit)
    (company-wubi--localize-variable 'company-tooltip-align-annotations company-wubi-tooltip-align-annotations)

    ;; Set keys
    ;; TODO: Unbind the number keys
    (dotimes (i 10)
      (define-key company-active-map (read-kbd-macro (format "%d" i)) 'company-complete-number))
    (company-wubi--bind-auto-complete-keys)

    (setq company-wubi-enable-p t)
    (message "Wubi input enabled!")))

(defun company-wubi-disable ()
  "Disable the wubi input method."
  (interactive)
  (when company-wubi-enable-p
    ;; Unload the input dict
    (company-wubi--unload-dict)

    ;; Restore the company configs
    (company-wubi--delocalize-variable 'company-idle-delay)
    (company-wubi--delocalize-variable 'company-backends)
    (company-wubi--delocalize-variable 'company-minimum-prefix-length)
    (company-wubi--delocalize-variable 'company-tooltip-limit)
    (company-wubi--delocalize-variable 'company-tooltip-align-annotations)

    ;; Unbind keys
    (company-wubi--unbind-auto-complate-keys)

    (setq company-wubi-enable-p nil)
    (message "Wubi input disabled!")))

(defun company-wubi-toggle ()
  "Toggle the wubi input method."
  (interactive)
  (if company-wubi-enable-p
      (progn
        (company-wubi-disable)
        (wubi-indication-mode -1))
    (progn
      (company-wubi-enable)
      (wubi-indication-mode 1))))

(global-set-key (kbd "C-\\") #'company-wubi-toggle)

;;;###autoload
(defun company-wubi (command &optional arg &rest ignored)
  "`company-mode' completion back-end for `wubi-code.el'.
Provide completion info according to COMMAND and ARG.  IGNORED, not used."
  (interactive (list 'interactive))
  (cl-case command
    (interactive (company-begin-backend 'company-wubi))
    (prefix (or (company-wubi--prefix) 'stop))
    (candidates (company-wubi--candidates arg))
    (annotation (company-wubi--annotation arg))
    (sorted t)
    (no-cache t)))

(provide 'company-wubi)

;; company-wubi.el ends here

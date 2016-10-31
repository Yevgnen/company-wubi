;;; company-wubi.el ---

;; Copyright (C) 2016 Yevgnen Koh

;; Author: Yevgnen Koh <wherejoystarts@gmail.com>
;; Package-Requires: ((emacs "24") (cl-lib "0.5") (company "0.9.0") (dash "2.13.0") (s "1.10.0"))
;; Version: 0.0.1

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

;; A tiny Wubi Input Method(五笔输入法) for Emacs using company mode.
;; e.g.
;; ;; Basic usage.
;; (global-set-key (kbd "C-\\") #'wubi-mode)
;;
;; See documentation on https://github.com/yevgnen/company-wubi.

;;; Code:

(eval-when-compile (require 'cl-lib))
(require 'company)
(require 'dash)
(require 's)

(defgroup company-wubi nil
  "Completion back-end for wubi input."
  :group 'company)

(defcustom company-wubi-wb-dict-file
  (expand-file-name "wb_table.txt" (file-name-directory load-file-name))
  "Location of the wubi dict file.

A string containing the name or the full path of the dict."
  :group 'company-wubi
  :type '(file :must-match t)
  :risky t)

(defcustom company-wubi-py-dict-file
  (expand-file-name "py_table.txt" (file-name-directory load-file-name))
  "Location of the pinyin dict file.

A string containing the name or the full path of the dict."
  :group 'company-wubi
  :type '(file :must-match t)
  :risky t)

(defcustom company-wubi-wb-reverse-dict-file
  (expand-file-name "wb_reverse_table.txt" (file-name-directory load-file-name))
  "Location of the wubi reverse dict file.

A string containing the name or the full path of the dict."
  :group 'company-wubi
  :type '(file :must-match t)
  :risky t)

(defcustom company-wubi-idle-delay
  0
  "Hijack the `company-idle-delay' variable."
  :group 'company-wubi)

(defcustom company-wubi-minimum-prefix-length
  1
  "Hijack the `company-minimum-prefix-length' variable."
  :group 'company-wubi
  :type 'number)

(defcustom company-wubi-tooltip-limit
  5
  "Hijack the `company-tooltip-limit' variable."
  :group 'company-wubi
  :type 'number)

(defcustom company-wubi-tooltip-align-annotations
  nil
  "Hijack the `company-tooltip-align-annotations' variable."
  :group 'company-wubi
  :type 'boolean)

(defcustom company-wubi-auto-complete-chars
  '((" ")
    ("-" "－")
    ("=" "＝")
    (";" "；")
    ("'" "「」")
    ("," "，")
    ("." "。")
    ("~" "～")
    ("!" "！")
    ("@" "‧")
    ("#" "＃")
    ("$" "￥")
    ("%" "％")
    ("^" "……")
    ("&" "＆")
    ("*" "×")
    ("(" "（")
    (")" "）")
    ("_" "—")
    ("+" "＋")
    ("{" "｛")
    ("}" "｝")
    ("[" "［")
    ("]" "］")
    ("\\" "、")
    (":" "：")
    ("\"" "“”")
    ("|" "｜")
    ("?" "？")
    ("/" "、")
    ("<" "《》")
    (">" "〈〉"))
  "The auto complete char mapping in list form.

The `car' of each element is used to trigger auto completion
or controlling keys like page flipping or candidate selection.

Currently -/= is used for page flipping and SPC is used for auto completion,
while ;/' is used for 2nd and 3rd candidate selection.

When the `cdr' of the element is not `nil', the `car' of the
element will be inserted."
  :group 'company-wubi)

(defcustom company-wubi-show-codes-in-pinyin
  t
  "Show wubi codes when using pinyin.

Currently this will slow down the completion progress since
the wubi code of all the candidates will be lookup in realtime."
  :group 'company-wubi
  :type 'boolean)

;;; Internal variables
(defvar company-wubi-wb-table nil)
(defvar company-wubi-py-table nil)
(defvar company-wubi-wb-reverse-table nil)
(defvar company-wubi-punctuation-pos 0)
(defvar company-wubi-p t)

(defvar company-wubi-active-map (make-sparse-keymap))

(defvar company-wubi-backends nil)
(make-local-variable 'company-wubi-backends)

(defvar company-wubi-transformers nil)
(make-local-variable 'company-wubi-transformers)

;;; Dictionary management
(defun company-wubi--load-default-dict (file var)
  (with-temp-buffer
    (insert-file-contents file)
    (set (intern (symbol-name var))
         (-map (lambda (l)
                 (s-split " " l))
               (-map #'s-trim
                     (s-split "\n" (buffer-string)))))))

(defun company-wubi--load-dicts ()
  "Load all the dictionaries.

Including the `wb_table', `py_table' and `wb_reverse_table'."
  (company-wubi--load-default-dict company-wubi-wb-dict-file 'company-wubi-wb-table)
  (company-wubi--load-default-dict company-wubi-py-dict-file 'company-wubi-py-table)
  (company-wubi--load-default-dict company-wubi-wb-reverse-dict-file 'company-wubi-wb-reverse-table))

(defun company-wubi--load-dicts-when-necessary ()
  (unless
      (and company-wubi-wb-table
           company-wubi-py-table
           company-wubi-wb-reverse-table)
    (company-wubi--load-dicts)))

(defun company-wubi--unload-dict ()
  "Unload the dictionaries."
  (setq company-wubi-wb-table nil)
  (setq company-wubi-py-table nil)
  (setq company-wubi-wb-reverse-table nil))

;;; Company things
(defun company-wubi--prefix ()
  "Get a prefix from current position."
  (let ((case-fold-search nil))
    (when (looking-back "`?[a-z]+" 0 t)
      (let ((prefix (substring-no-properties (match-string 0))))
        (if (s-starts-with? "`" prefix)
            (progn
              (setq company-wubi-p nil)
              prefix)
          (progn
            (setq company-wubi-p t)
            (if (< (length prefix) 5)
                prefix)))))))

(defun company-wubi--py-candidates (prefix)
  "Return the candidates under pinyin input given `prefix'."
  (-flatten
   (-map #'cdr
         (-take 50
                (-filter (lambda (l)
                           (string-prefix-p prefix (car l)))
                         company-wubi-py-table)))))

(defun company-wubi--wb-candidates (prefix)
  "Return the candidates under wubi input given `prefix'."
  (-distinct
   (-flatten
    (-map (lambda (c)
            (let ((code (car c))
                  (chars (cdr c)))
              (-map (lambda (char)
                      (propertize char 'code (substring code (length prefix))))
                    chars)))
          (-take 50
                 (-filter (lambda (l)
                            (string-prefix-p prefix (car l)))
                          company-wubi-wb-table))))))

(defun company-wubi--candidates (prefix)
  "Return the candidates under given `prefix'."
  (if company-wubi-p
      (company-wubi--wb-candidates prefix)
    (company-wubi--py-candidates (substring-no-properties prefix 1))))

(defun company-wubi--wb-reverse-lookup-one (candidate)
  (let ((index (--find-index (equal candidate (car it)) company-wubi-wb-reverse-table)))
    (if index
        (car (last (nth index company-wubi-wb-reverse-table)))
      nil)))

(defun company-wubi--wb-reverse-lookup (candidate)
  "Return the wubi code of the `candidate'.

The wubi code is generated in the following rules.
(A-Z denotes chinese characters, a-z denote wubi codes in order.)

When the Chinese characters is AB, use code aabb,
when ABC use abcc, when ABCD use abcd, when ABC...Z, use abcz.
"
  (when company-wubi-show-codes-in-pinyin
    (or (company-wubi--wb-reverse-lookup-one candidate)
        (let ((len (length candidate)))
          (cond ((= len 2) (let ((c1 (company-wubi--wb-reverse-lookup-one (string (elt candidate 0))))
                                 (c2 (company-wubi--wb-reverse-lookup-one (string (elt candidate 1)))))
                             (and (> (length c1) 1)
                                  (> (length c2) 1)
                                  (concat
                                   (substring-no-properties c1 0 2)
                                   (substring-no-properties c2 0 2)))))
                ((= len 3) (let ((c1 (company-wubi--wb-reverse-lookup-one (string (elt candidate 0))))
                                 (c2 (company-wubi--wb-reverse-lookup-one (string (elt candidate 1))))
                                 (c3 (company-wubi--wb-reverse-lookup-one (string (elt candidate 2)))))
                             (and (> (length c1) 0)
                                  (> (length c2) 0)
                                  (> (length c3) 1)
                                  (concat
                                   (substring-no-properties c1 0 1)
                                   (substring-no-properties c2 0 1)
                                   (substring-no-properties c3 0 2)))))
                ((> len 3) (let ((c1 (company-wubi--wb-reverse-lookup-one (string (elt candidate 0))))
                                 (c2 (company-wubi--wb-reverse-lookup-one (string (elt candidate 1))))
                                 (c3 (company-wubi--wb-reverse-lookup-one (string (elt candidate 2))))
                                 (c4 (company-wubi--wb-reverse-lookup-one (string (elt candidate (1- len))))))
                             (and (> (length c1) 0)
                                  (> (length c2) 0)
                                  (> (length c3) 0)
                                  (> (length c4) 0)
                                  (concat
                                   (substring-no-properties c1 0 1)
                                   (substring-no-properties c2 0 1)
                                   (substring-no-properties c3 0 1)
                                   (substring-no-properties c4 0 1)))))
                (t ""))))))

(defun company-wubi--wb-annotation (candidate)
  "Use the company annotation to show the wubi input codes in wubi input."
  (get-text-property 0 'code candidate))

(defun company-wubi--py-annotation (candidate)
  "Use the company annotation to show the wubi input codes in py input."
  (company-wubi--wb-reverse-lookup candidate))

(defun company-wubi--annotation (candidate)
  "Use the company annotation to show the wubi input codes."
  (if company-wubi-p
      (company-wubi--wb-annotation candidate)
    (company-wubi--py-annotation candidate)))

(defun company-wubi-previous-page ()
  "Move to previous page of candidates."
  (interactive)
  (company-set-selection
   (- company-selection
      company-wubi-tooltip-limit
      (mod company-selection
           company-wubi-tooltip-limit))))

(defun company-wubi-next-page ()
  "Move to next page of candidates."
  (interactive)
  (company-set-selection
   (+ company-selection
      company-wubi-tooltip-limit
      (- company-wubi-tooltip-limit
         (1+ (mod company-selection
                  company-wubi-tooltip-limit)))))
  (company-set-selection
   (- company-selection
      (1- company-wubi-tooltip-limit))))

(defun company-wubi--insert-punctuation (chars)
  "Insert the punctuation."
  (let ((len (length chars)))
    (cond ((= len 0)
           nil)
          ((= len 1)
           (insert chars))
          ((= len 2)
           (progn
             (insert (elt chars company-wubi-punctuation-pos))
             (setq company-wubi-punctuation-pos (- 1 company-wubi-punctuation-pos)))))))

(defun company-wubi--bind-keys ()
  "Bind completion keys and other keys."
  (let ((oldmap company-active-map)
        (newmap (make-sparse-keymap)))
    (set-keymap-parent newmap oldmap)

    ;; Use digits to select candidates
    (dotimes (i 10)
      (define-key newmap (read-kbd-macro (format "%d" i)) 'company-complete-number))

    ;; Auto completion when more than 4 char codes
    (dotimes (i 26)
      (let ((char (+ ?a i)))
        (define-key newmap (read-kbd-macro (format "%s" (string char)))
          `(lambda ()
             (interactive)
             (and company-wubi-p
                  (>= (length (company-wubi--prefix)) 4)
                  (company-complete-selection))
             (insert ,char)
             (company-manual-begin)))))

    (-map (lambda (mapping)
            (let ((in (car mapping))
                  (out (cadr mapping)))
              ;; Define new bindings
              (define-key newmap in
                `(lambda ()
                   (interactive)
                   ;; Select the current candidate or 2nd/3rd candidate or change page
                   (cond ((string= ,in ";") (company-complete-number 2))
                         ((string= ,in "'") (company-complete-number 3))
                         ((string= ,in "-") (company-wubi-previous-page))
                         ((string= ,in "=") (company-wubi-next-page))
                         (t (company-complete-selection)))

                   ;; Insert the auto complete char unless it's not a controlling char
                   (unless (member ,in '("-" "=" ";" "'"))
                     (company-wubi--insert-punctuation ,out))))
              mapping))
          company-wubi-auto-complete-chars)
    newmap))

;;; Advices for hihacking company mode
(defun company-wubi--call-frontends (orig-fun &rest args)
  (if wubi-mode
      (let ((company-minimum-prefix-length company-wubi-minimum-prefix-length)
            (company-tooltip-limit company-wubi-tooltip-limit)
            (company-tooltip-align-annotations company-wubi-tooltip-align-annotations))
        (apply orig-fun args))
    (apply orig-fun args))

  (and wubi-mode
       company-wubi-p
       (let ((prefix (company-wubi--prefix)))
         (and (= (length prefix) 4)
              (= (length company-candidates) 1)
              (company-complete-selection)))))

(defun company-wubi--idle (orig-fun &rest args)
  (if wubi-mode
      (let ((company-idle-delay company-wubi-idle-delay))
        (apply orig-fun args))
    (apply orig-fun args)))

(defun company-wubi--good-prefix-p (orig-fun &rest args)
  (if wubi-mode
      (let ((company-minimum-prefix-length company-wubi-minimum-prefix-length))
        (apply orig-fun args))
    (apply orig-fun args)))

(defun company-wubi--perform (orig-fun &rest args)
  (if wubi-mode
      (let ((company-active-map company-wubi-active-map))
        (apply orig-fun args))
    (apply orig-fun args)))

;;; Wubi mode
;; Initialization and finalization
(defun company-wubi--localize-variable (var val)
  "Use local company setting for better user experiences."
  (make-local-variable var)
  (set (intern (format "company-wubi--%s" (symbol-name var))) (symbol-value var))
  (set (intern (symbol-name var)) val))

(defun company-wubi--delocalize-variable (var)
  "Restore the local buffer company settings."
  (set (intern (symbol-name var))
       (symbol-value (intern (format "company-wubi--%s" (symbol-name var))))))

(defun company-wubi-enable ()
  "Enable the wubi input method."
  ;; Load the input dict
  (company-wubi--load-dicts-when-necessary)

  ;; Hijack the company settings
  (company-wubi--localize-variable 'company-backends '(company-wubi))
  (company-wubi--localize-variable 'company-transformers (remove 'company-sort-by-statistics company-transformers))
  (advice-add 'company-call-frontends :around #'company-wubi--call-frontends)
  (advice-add 'company--idle-delay :around #'company-wubi--idle)
  (advice-add 'company--good-prefix-p :around #'company-wubi--good-prefix-p)
  (advice-add 'company--perform :around #'company-wubi--perform)

  ;; Bind keys
  (setq company-wubi-active-map (company-wubi--bind-keys)))

(defun company-wubi-disable ()
  "Disable the wubi input method."
  ;; Restore settings
  (company-wubi--delocalize-variable 'company-backends)
  (company-wubi--delocalize-variable 'company-transformers))

;; Keymap
(defvar wubi-mode-map (make-sparse-keymap))
(-each company-wubi-auto-complete-chars
  (lambda (mapping)
    (let ((in (car mapping))
          (out (cadr mapping)))
      (define-key wubi-mode-map (read-kbd-macro in)
        `(lambda ()
           (interactive)
           (company-wubi--insert-punctuation ,out))))))

;;;###autoload
(define-minor-mode wubi-mode
  "Toggle Wubi indication mode on or off.
Turn Wubi indication mode on if ARG is positive, off otherwise."
  :group 'company-wubi
  :lighter " 五"
  :keymap wubi-mode-map
  (if wubi-mode
      (company-wubi-enable)
    (company-wubi-disable)))

;;;###autoload
(defun company-wubi (command &optional arg &rest ignored)
  "`company-mode' completion back-end for wubi.
Provide completion info according to COMMAND and ARG.  IGNORED, not used."
  (interactive (list 'interactive))
  (cl-case command
    (interactive (company-begin-backend 'company-wubi))
    (prefix (company-wubi--prefix))
    (candidates (company-wubi--candidates arg))
    (annotation (company-wubi--annotation arg))
    (sorted t)
    (no-cache t)))

(provide 'company-wubi)

;; company-wubi.el ends here

;;; ysq-modeline.el --- put stock quotes in mode line

;; Copyright (C) 1999 Noah S. Friedman

;; Author: Noah Friedman <friedman@splode.com>
;; Maintainer: friedman@splode.com
;; Keywords: extensions
;; Created: 1999-03-31

;; $Id: ysq-modeline.el,v 1.1 2000/09/14 17:47:24 friedman Exp $

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, you can either send email to this
;; program's maintainer or write to: The Free Software Foundation,
;; Inc.; 59 Temple Place, Suite 330; Boston, MA 02111-1307, USA.

;;; Commentary:

;; This module is still heavily under development
;; and probably unusuably buggy.

;; Updates of this program may be available via the URL
;; http://www.splode.com/~friedman/software/emacs-lisp/

;;; Code:

(require 'ysq)

;;; Timer internals

(defconst ysq-xemacs-p
  (and (string-match "XEmacs\\|Lucid" (emacs-version)) t))

(if ysq-xemacs-p
    (require 'itimer)
  (require 'timer))

(defsubst ysq-timer-get-time     (timer)        (aref timer 0))
(defsubst ysq-timer-get-repeat   (timer)        (aref timer 1))
(defsubst ysq-timer-get-function (timer)        (aref timer 2))
(defsubst ysq-timer-get-args     (timer)        (aref timer 3))
(defsubst ysq-timer-get-data     (timer)        (aref timer 4))
(defsubst ysq-timer-set-time     (timer time)   (aset timer 0 time))
(defsubst ysq-timer-set-repeat   (timer repeat) (aset timer 1 repeat))
(defsubst ysq-timer-set-function (timer fn)     (aset timer 2 fn))
(defsubst ysq-timer-set-args     (timer args)   (aset timer 3 args))
(defsubst ysq-timer-set-data     (timer data)   (aset timer 4 data))

(defun ysq-timer-create (time repeat function &rest args)
  (let ((timer (make-vector 5 nil)))
    (ysq-timer-set-time     timer time)
    (ysq-timer-set-repeat   timer repeat)
    (ysq-timer-set-function timer function)
    (ysq-timer-set-args     timer args)
    timer))

(defun ysq-timer-schedule (timer)
  (ysq-timer-set-data timer
   (if ysq-xemacs-p
       (start-itimer (if (symbolp (ysq-timer-get-function timer))
                         (symbol-name (ysq-timer-get-function timer))
                       "anonymous function")
                     (ysq-timer-get-function timer)
                     (ysq-timer-get-time     timer)
                     (ysq-timer-get-repeat   timer))
     (apply 'run-at-time
            (ysq-timer-get-time     timer)
            (ysq-timer-get-repeat   timer)
            (ysq-timer-get-function timer)
            (ysq-timer-get-args     timer)))))

(defun ysq-timer-cancel (timer)
  (and (ysq-timer-get-data timer)
       (ysq-timer-set-data timer
        (if ysq-xemacs-p
            (delete-itimer (ysq-timer-get-data timer))
          (cancel-timer (ysq-timer-get-data timer))))))

(defun ysq-timer-reset (timer)
  (ysq-timer-cancel timer)
  (ysq-timer-schedule timer))


(defvar ysq-mode-line-tickers
  '((("aol") (last-price))))

(defvar ysq-mode-line-default-formatter-function
  'ysq-mode-line-default-formatter)

(defvar ysq-mode-line-format
  '(ysq-mode-line-string (" " ysq-mode-line-string)))

(defvar ysq-mode-line-string "")

(defvar ysq-mode-line-update-interval 10)
(defvar ysq-mode-line-timer nil)

(defun ysq-mode-line-install ()
  (interactive)
  (cond ((or (not (boundp 'global-mode-string))
             (null global-mode-string))
         (setq global-mode-string '("" ysq-mode-line-format)))
        ((listp global-mode-string)
         (or (member 'ysq-mode-line-format global-mode-string)
             (setq global-mode-string
                   (append global-mode-string '(ysq-mode-line-format))))))
  (ysq-mode-line-schedule))

(defun ysq-mode-line-schedule ()
  (setq ysq-mode-line-timer
        (ysq-timer-create 0
                          ysq-mode-line-update-interval
                          'ysq-mode-line-update))
  (ysq-timer-schedule ysq-mode-line-timer))

(defun ysq-mode-line-schedule-cancel ()
  (ysq-timer-cancel ysq-mode-line-timer))

(defun ysq-mode-line-update-sync ()
  (let ((alist (ysq-mode-line-get-sync ysq-mode-line-tickers))
        (tickers ysq-mode-line-tickers)
        (result "")
        formatter)
    (while alist
      (setq formatter (or (nth 2 (car tickers))
                          ysq-mode-line-default-formatter-function))

      (setq result (concat result (funcall formatter (car alist))))

      (setq alist (cdr alist))
      (setq tickers (cdr tickers)))
    (setq ysq-mode-line-string result)
    (force-mode-line-update)))

(defun ysq-mode-line-get-sync (&optional tickers)
  (or tickers
      (setq tickers ysq-mode-line-tickers))
  (let ((data nil)
        (ysq-query-fields ysq-query-fields))
    (while tickers
      (setq ysq-query-fields (nth 1 (car tickers)))
      (setq data
            (cons (car (apply 'ysq-ticker-collect (car (car tickers))))
                  data))
      (setq tickers (cdr tickers)))
    data))

(defun ysq-mode-line-default-formatter (alist &optional separator)
  (or separator
      (setq separator " "))
  (let ((result "")
        val)
    (while alist
      (setq val (cdr (car alist)))
      (setq result (concat result (if (string= "" result)
                                      ""
                                    separator)
                           (cond ((stringp val)
                                  val)
                                 ((integerp val)
                                  (number-to-string val))
                                 ((floatp val)
                                  (format "%.2f" val)))))
      (setq alist (cdr alist)))
    result))


(defun ysq-mode-line-get (&optional tickers)
  (or tickers
      (setq tickers ysq-mode-line-tickers))
  (let ((data nil)
        (ysq-query-fields ysq-query-fields))
    (while tickers
      (setq ysq-query-fields (nth 1 (car tickers)))

      (apply 'ysq-ticker-collect-async (car (car tickers)))
      (while (ysq-process-live-p ysq-ticker-async-process)
        (accept-process-output nil 1))
      (sit-for 0 1)

      (setq data (cons (car ysq-ticker-collected-async-data) data))
      (setq tickers (cdr tickers)))
    data))

;; obsolete
(defun ysq-mode-line-get-OBS (&optional tickers)
  (or tickers
      (setq tickers ysq-mode-line-tickers))
  (let ((data nil)
        (ysq-query-fields ysq-query-fields))
    (while tickers
      (setq ysq-query-fields (nth 1 (car tickers)))

      (apply 'ysq-ticker-collect-async (car (car tickers)))
      (while (ysq-process-live-p ysq-ticker-async-process)
        (accept-process-output nil 1))
      (sit-for 0 1)

      (setq data (cons (car ysq-ticker-collected-async-data) data))
      (setq tickers (cdr tickers)))
    data))


(provide 'ysq-modeline)

;; ysq-modeline.el ends here

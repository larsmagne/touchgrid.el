;;; libinput.el --- Executing actions based on input events
;; Copyright (C) 2018 Lars Magne Ingebrigtsen

;; Author: Lars Magne Ingebrigtsen <larsi@gnus.org>
;; Keywords: wordpress

;; Libinput is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; Libinput is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
;; or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public
;; License for more details.

;; You should have received a copy of the GNU General Public License
;; along with Emacs; see the file COPYING.  If not, write to the Free
;; Software Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA
;; 02111-1307, USA.

;;; Commentary:

;;; Code:

(require 'cl)

(defvar libinput--process nil)
(defvar libinput--prev-point nil)
(defvar libinput--callback nil)

(defun libinput-start (callback)
  "Start listening for events."
  (save-excursion
    (set-buffer (get-buffer-create " *libinput*"))
    (setq-local libinput--prev-point (point-max))
    (setq-local libinput--callback callback)
    (libinput-stop)
    (setq libinput--process
	  (start-process "libinput" (current-buffer) "libinput-debug-events"))
    (set-process-filter libinput--process 'libinput--filter)))

(defun libinput-stop ()
  "Stop libinput."
  (interactive)
  (when libinput--process
    (delete-process libinput--process)))

(defun libinput--filter (process string)
  (let ((lines nil)
	(callback nil))
    (save-excursion
      (set-buffer (process-buffer process))
      (setq callback libinput--callback)
      (goto-char (point-max))
      (insert string)
      (goto-char libinput--prev-point)
      (while (re-search-forward "^\\(.*\\)\n" nil t)
	(push (match-string 1) lines))
      ;; Keep the buffer trimmed.
      (save-excursion
	(forward-line -100)
	(delete-region (point) (point-min)))
      (setq libinput--prev-point (point)))
    (dolist (line (nreverse lines))
      (save-excursion
	(condition-case err
	    (funcall callback (libinput--parse-line line))
	  (error
	   (message "Got an error: %s" err)))))))

(defun libinput--parse-line (line)
  ;; The output is on the form below.  I don't know what the minus
  ;; means...
  ;; -event3   KEYBOARD_KEY     +501.91s	*** (-1) pressed
  ;;  event3   KEYBOARD_KEY     +502.02s	*** (-1) pressed
  (let* ((elem (split-string (substring line 1) "[ \n\t]+" t))
	 (type (cadr elem)))
    (append
     (list :device (car elem)
	   :type type)
     (cond
      ((equal type "POINTER_MOTION")
       (let ((delta (split-string (nth 3 elem) "[ /]+")))
	 (list :x-delta (string-to-number (car delta))
	       :y-delta (string-to-number (cadr delta)))))
      ((equal type "TOUCH_DOWN")
       (let ((pos (split-string (nth 5 elem) "[ /]+")))
	 (list :x (string-to-number (car pos))
	       :y (string-to-number (cadr pos)))))
      ((equal type "SWITCH_TOGGLE")
       (list :switch (nth 4 elem)
	     :state (nth 6 elem)))))))

(provide 'libinput)

;;; libinput.el ends here

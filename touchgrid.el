;;; touchgrid.el --- Executing actions based on input events
;; Copyright (C) 2018 Lars Magne Ingebrigtsen

;; Author: Lars Magne Ingebrigtsen <larsi@gnus.org>
;; Keywords: wordpress

;; Touchgrid is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; Touchgrid is distributed in the hope that it will be useful, but WITHOUT
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
(require 'libinput)

(defvar touchgrid-actions
  '(("event7"
     (backward-1m backward-10s pause forward-10s forward-1m)
     (backward-1m backward-10s pause forward-10s forward-1m)
     (delete      none         quit  none        undelete  ) 
     (grid        none         pause none        none      )
     (keyboard    none         none  none        play      ))))

(defun touchgrid-start ()
  "Start monitoring for touch events."
  (interactive)
  (libinput-start 'touchgrid--handle))

(defun touchgrid--handle (event)
  ;;(message "%s" event)
  (when-let ((grid (cdr (assoc (getf event :device) touchgrid-actions))))
    (when (equal (getf event :type) "TOUCH_DOWN")
      (message "%S" event)
      ;; The positions we get are percentages of width/height.
      (let* ((width (display-pixel-width))
	     (height (display-pixel-height))
	     (box-width (/ width (length (car grid))))
	     (box-height (/ height (length grid)))
	     (action (elt (elt grid (truncate
				     (/ (* (/ (getf event :y) 100.0) height)
					box-height)))
			  (truncate (/ (* (/ (getf event :x) 100.0) width)
				       box-width)))))
	(message "Doing action %s" action)
	(funcall (intern (format "touchgrid--%s" action) obarray))))))

(defun touchgrid--keyboard ()
  (call-process
   "qdbus" nil nil nil "org.onboard.Onboard"
   "/org/onboard/Onboard/Keyboard" "org.onboard.Onboard.Keyboard.Show"))

(defun touchgrid--backward-1m ()
  (movie-send-mpv-command '((command . ["seek" -60 "relative" "keyframes"]))))

(defun touchgrid--forward-1m ()
  (movie-send-mpv-command '((command . ["seek" 60 "relative" "keyframes"]))))

(defun touchgrid--backward-10s ()
  (movie-send-mpv-command '((command . ["seek" -10 "relative" "keyframes"]))))

(defun touchgrid--forward-10s ()
  (movie-send-mpv-command '((command . ["seek" 10 "relative" "keyframes"]))))

(defun touchgrid--quit ()
  (movie-send-mpv-command '((command . ["quit"]))))

(defun touchgrid--pause ()
  (movie-send-mpv-command '((command . ["pause"]))))

(provide 'touchgrid)

;;; touchgrid.el ends here

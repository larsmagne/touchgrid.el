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
(require 'svg)

(defvar touchgrid-actions
  '(("Wacom Pen and multitouch sensor Finger"
     ("emacs"
      (none        none         none  none        play      )
      (none        none         none  none        none      )
      (delete      none         none  none        undelete  ) 
      (grid        none         none  none        rotate    )
      (keyboard    none         none  none        play      ))
     ("mpv"
      (backward-1m backward-10s pause forward-10s forward-1m)
      (backward-1m backward-10s pause forward-10s forward-1m)
      (none        none         quit  none        none      ) 
      (grid        none         pause none        none      )
      (keyboard    none         pause none        show-progress))))
  "Alist of events/grid states/grids.")

(defvar touchgrid-event-transitions
  '((play "mpv"))
  "Alist of action/state transitions.
These actions make alternative grids active for the duration of
the command.")

(defvar touchgrid-debug nil
  "If non-nil, output debugging messages.")

(defvar touchgrid-other-actions
  '(((lambda (event)
       (and (equal (getf event :type) "SWITCH_TOGGLE")
	    (equal (getf event :switch) "tablet-mode")))
     toggle-rotation)))

(defvar touchgrid--state "emacs")

(defvar touchgrid--action-number 0)
(defvar touchgrid--rotation nil)

(defun touchgrid-start ()
  "Start monitoring for touch events."
  (interactive)
  (libinput-start 'touchgrid--handle))

(defun touchgrid--handle (event)
  (when touchgrid-debug
    (message "%S" event))
  (when-let ((grid (touchgrid--reorient-grid
		    (cdr (assoc touchgrid--state
				(cdr (assoc (getf event :device-name)
					    touchgrid-actions)))))))
    (when (equal (getf event :type) "TOUCH_DOWN")
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
	;; Disable all other commands when the keyboard is active.
	(when (or (not touchgrid--keyboard)
		  (eq action 'keyboard))
	  (unless (eq action 'grid)
	    (touchgrid--remove-grid))
	  (when touchgrid-debug
	    (message "%d: Doing action %s"
		     (incf touchgrid--action-number) action))
	  (let ((touchgrid--state
		 (or (cadr (assq action touchgrid-event-transitions))
		     touchgrid--state)))
	    (funcall (intern (format "touchgrid--%s" action) obarray)))))))
  (dolist (elem touchgrid-other-actions)
    (when (funcall (car elem) event)
      (funcall (intern (format "touchgrid--%s" (cadr elem)) obarray)))))

(defun touchgrid--call-process (program &optional infile destination display
					&rest args)
  (let ((default-directory "/"))
    (apply 'call-process program infile destination display args)))

(defvar touchgrid--grid-process nil)

(defun touchgrid--remove-grid ()
  (when (process-live-p touchgrid--grid-process)
    (delete-process touchgrid--grid-process)
    (when (file-exists-p "/tmp/grid.svg")
      (delete-file "/tmp/grid.svg"))))

(defun touchgrid--grid ()
  (if (process-live-p touchgrid--grid-process)
      (progn
	(touchgrid--remove-grid)
	(touchgrid--emacs-focus))
    (with-temp-buffer
      (let* ((width (display-pixel-width))
	     (height (display-pixel-height))
	     (grid (cdr (assoc touchgrid--state (cdar touchgrid-actions))))
	     (box-width (/ width (length (car grid))))
	     (box-height (/ height (length grid)))
	     (svg (svg-create width height)))
	(svg-rectangle svg 0 0 width height :fill "none"
		       :fill-opacity "0.5")
	(loop for y from 0 upto (length grid)
	      do (svg-line svg 0 (* y box-height) width (* y box-height)
			   :stroke-color "black"
			   :stroke-width "2px"))
	(loop for x from 0 upto (length (car grid))
	      do (svg-line svg (* x box-width) 0 (* x box-width) height
			   :stroke-color "black"
			   :stroke-width "2px"))
	(loop for (stroke color) in '((10 "#202020")
				      (5 "black")
				      (0 "white"))
	      do (loop for y from 0 upto (1- (length grid))
		       do (loop for x from 0 upto (1- (length (car grid)))
				for action = (elt (elt grid y) x)
				do (svg-text
				    svg (if (eq action 'none)
					    ""
					  (symbol-name action))
				    :text-anchor "middle"
				    :font-size 50
				    :stroke-width stroke
				    :stroke color
				    :fill color
				    :x (+ (* box-width x) (/ box-width 2.0))
				    :y (+ (* box-height y)
					  (/ box-height 2.0))))))
	(svg-print svg)
	(write-region (point-min) (point-max) "/tmp/grid.svg" nil 'silent))
      (let ((default-directory "/"))
	(setq touchgrid--grid-process
	      (start-process "qiv" nil "qiv" "-p" "/tmp/grid.svg"))))))

(defun touchgrid--reorient-grid (grid)
  (if (not touchgrid--rotation)
      grid
    (loop for line in (reverse grid)
	  collect (loop for elem in (reverse line)
			collect elem))))

(defvar touchgrid--keyboard nil)

(defun touchgrid--keyboard ()
  (if touchgrid--keyboard
      (touchgrid--call-process
       "qdbus" nil nil nil "org.onboard.Onboard"
       "/org/onboard/Onboard/Keyboard" "org.onboard.Onboard.Keyboard.Hide")
    (touchgrid--call-process
     "qdbus" nil nil nil "org.onboard.Onboard"
     "/org/onboard/Onboard/Keyboard" "org.onboard.Onboard.Keyboard.Show"))
  (setq touchgrid--keyboard (not touchgrid--keyboard))
  (touchgrid--emacs-focus))

(defun touchgrid--show-progress ()
  (movie-send-mpv-command '((command . ["show-progress"]))))

(defun touchgrid--backward-1m ()
  (movie-send-mpv-command '((command . ["seek" -60 "relative" "keyframes"])))
  (touchgrid--show-progress))

(defun touchgrid--forward-1m ()
  (movie-send-mpv-command '((command . ["seek" 60 "relative" "keyframes"])))
  (touchgrid--show-progress))

(defun touchgrid--backward-10s ()
  (movie-send-mpv-command '((command . ["seek" -10 "relative" "keyframes"])))
  (touchgrid--show-progress))

(defun touchgrid--forward-10s ()
  (movie-send-mpv-command '((command . ["seek" 10 "relative" "keyframes"])))
  (touchgrid--show-progress))

(defun touchgrid--quit ()
  (movie-send-mpv-command '((command . ["quit"]))))

(defun touchgrid--pause ()
  (movie-send-mpv-command '((command . ["cycle" "pause"]))))


(defun touchgrid--play ()
  (touchgrid--emacs-focus)
  (call-interactively 'movie-play-best-file))

(defun touchgrid--delete ()
  (touchgrid--emacs-focus)
  (call-interactively 'movie-delete-file))

(defun touchgrid--undelete ()
  (touchgrid--emacs-focus)
  (call-interactively 'movie-undo-delete))

(defun touchgrid--none ()
  )


(defun touchgrid--emacs-focus ()
  (touchgrid--call-process
   "xdotool" nil nil nil "windowfocus" (touchgrid--find-emacs)))

(defun touchgrid--find-emacs ()
  (with-temp-buffer
    (touchgrid--call-process
     "xdotool" nil (current-buffer) nil
     "search" "--name" "emacs")
    (goto-char (point-max))
    (when (re-search-backward "^\\([0-9]+\\)\n" nil t)
      (match-string 1))))

(defun touchgrid--toggle-rotation ()
  (setq touchgrid--rotation (equal (getf event :state) "1"))
  (touchgrid--call-process "xrandr" nil nil nil "--output" "eDP-1" "--rotate"
			   (if (not touchgrid--rotation)
			       "normal"
			     "inverted")))

(provide 'touchgrid)

;;; touchgrid.el ends here

;;; touchgrid.el --- Executing actions based on input events  -*- lexical-binding: t -*-
;; Copyright (C) 2018-2024 Lars Magne Ingebrigtsen

;; Author: Lars Magne Ingebrigtsen <larsi@gnus.org>
;; Keywords: input touch

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

;; apt install libinput-tools xdotool qiv
;; apt build-dep qiv
;; adduser larsi input

;;; Code:

(require 'cl-lib)
(require 'libinput)
(require 'svg)

(defconst touchgrid-actions
  '(("Wacom HID 5344 Finger"
     ("emacs"
      (reload      enter        prev  torrent     play-current )
      (last-seen   none         none  none        tv-series )
      (delete      none         one-window  none  undelete  ) 
      (grid        none         none  none        none      )
      (keyboard    exit         next  sort        play      ))
     ("keyboard"
      (none none none none k< k> kup kdown none none none none)
      (k1 k2 k3 k4 k5 k6 k7 k8 k9 k0 k- k= none)
      (kq kw ke kr kt ky ku ki ko kp k{ k} k|)
      (kshift ka ks kd kf kg kh kj kk kl k\; k\' kret)
      (kexit kz kx kc kv kb kn km k\, k. k/ none))
     ("keyboard-shift"
      (none none none none k< k> kup kdown none none none none)
      (k! k@ k\# k$ k% k^ k& k* k\( k\) k_ k+ none)
      (kQ kW kE kR kT kY kU kI kO kP k\[ k\] k\\)
      (kshift kA kS kD kF kG kH kJ kK kL k: k\" kret)
      (kexit kZ kX kC kV kB kN kM k< k> k? none))     
     ("mpv"
      (backward-1m backward-10s pause forward-10s forward-1m)
      (dec-sync    dec-volume   pause inc-volume  inc-sync  )
      (dec-speed   none         quit  none        inc-speed ) 
      (grid        none         pause none        audio     )
      (keyboard    subtitles    pause interlace   show-progress))))
  "Alist of events/grid states/grids.")

(keymap-global-set "<touchscreen-begin>" 'ignore)
(keymap-global-set "<touchscreen-update>" 'ignore)
(keymap-global-set "<touchscreen-end>" 'ignore)

(defvar touchgrid-debug nil
  "If non-nil, output debugging messages.")

(defvar touchgrid-other-actions
  '(
    ;; Toggle rotation when the screen is upside down (because the
    ;; Yoga is flipped over).
    ((lambda (event)
       (and (equal (cl-getf event :type) "SWITCH_TOGGLE")
	    (equal (cl-getf event :switch) "tablet-mode")))
     toggle-rotation)
    ;; Wake up xscreensaver when the user hits a key.  (xscreensaver
    ;; under Wayland seems to only wake up on mouse actions?)
    ((lambda (event)
       (equal (cl-getf event :type) "KEYBOARD_KEY"))
     wake-up)))

(defvar touchgrid--state "emacs")

(defvar touchgrid--action-number 0)
(defvar touchgrid--rotation nil)

(defun touchgrid-start ()
  "Start monitoring for touch events."
  (interactive)
  (server-start)
  (libinput-start 'touchgrid--handle))

(defun touchgrid--handle (event)
  (when touchgrid-debug
    (message "%S" touchgrid--state))
  (when-let ((grid (touchgrid--reorient-grid
		    (cdr (assoc touchgrid--state
				(cdr (assoc (cl-getf event :device-name)
					    touchgrid-actions)))))))
    (when (equal (cl-getf event :type) "TOUCH_DOWN")
      ;; The positions we get are percentages of width/height.
      (let* ((width (display-pixel-width))
	     (height (touchgrid--height))
	     (offset (- (display-pixel-height) height))
	     (box-width (/ width (length (car grid))))
	     (box-height (/ height (length grid)))
	     (action
	      (elt (elt grid
			(truncate
			 (/ (- (* (/ (cl-getf event :y) 100.0)
				  (display-pixel-height))
			       offset)
			    box-height)))
		   (truncate (/ (* (/ (cl-getf event :x) 100.0) width)
				box-width)))))
	;; Disable all other commands when the keyboard is active.
	(if (touchgrid--keyboard-p)
	    (touchgrid--handle-keyboard action)
	  (unless (eq action 'grid)
	    (touchgrid--remove-grid))
	  (when touchgrid-debug
	    (message "%d: Doing action %s"
		     (cl-incf touchgrid--action-number) action))
	  (funcall (intern (format "touchgrid--%s" action) obarray))))))
  (dolist (elem touchgrid-other-actions)
    (when (funcall (car elem) event)
      (funcall (intern (format "touchgrid--%s" (cadr elem)) obarray)
	       event))))

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

(defun touchgrid--keyboard-p ()
  (or (equal touchgrid--state "keyboard")
      (equal touchgrid--state "keyboard-shift")))

(defun touchgrid--height ()
  (let ((height (display-pixel-height)))
    (if (touchgrid--keyboard-p)
	(truncate (/ height 2.4))
      height)))

(defun touchgrid--grid ()
  (if (process-live-p touchgrid--grid-process)
      (progn
	(touchgrid--remove-grid)
	(touchgrid--emacs-focus))
    (with-temp-buffer
      (let* ((width (display-pixel-width))
	     (height (touchgrid--height))
	     (offset (- (display-pixel-height) height))
	     (grid (cdr (assoc touchgrid--state (cdar touchgrid-actions))))
	     (box-width (/ width (length (car grid))))
	     (box-height (/ height (length grid)))
	     (svg (svg-create width (display-pixel-height))))
	(svg-rectangle svg 0 0 width (display-pixel-height)
		       :fill "black"
		       :fill-opacity "0.5")
	(cl-loop for y from 0 upto (length grid)
		 do (svg-line svg 0 (+ offset (* y box-height))
			      width (+ offset (* y box-height))
			      :stroke-color "black"
			      :stroke-width "4px"))
	(cl-loop for x from 0 upto (length (car grid))
		 do (svg-line svg (* x box-width) offset
			      (* x box-width) (+ offset height)
			      :stroke-color "black"
			      :stroke-width "4px"))
	(cl-loop for (stroke color) in '((10 "#202020")
					 (5 "black")
					 (0 "white"))
		 do (cl-loop
		     for y from 0 upto (1- (length grid))
		     do (cl-loop for x from 0 upto (1- (length (car grid)))
				 for action = (elt (elt grid y) x)
				 do (svg-text
				     svg (cond
					  ((eq action 'none)
					   "")
					  ((touchgrid--keyboard-p)
					   (substring (symbol-name action) 1))
					  (t
					   (symbol-name action)))
				     :text-anchor "middle"
				     :font-size 100
				     :font-family "futura"
				     :font-weight "bold"
				     :stroke-width stroke
				     :stroke color
				     :fill color
				     :x (+ (* box-width x) (/ box-width 2.0))
				     :y (+ offset
					   ;; Make the text appear in
					   ;; the middle.
					   (truncate (* 100 0.3))
					   (* box-height y)
					   (/ box-height 2.0))))))
	(svg-print svg)
	(write-region (point-min) (point-max) "/tmp/grid.svg" nil 'silent))
      (let ((default-directory "/"))
	(setq touchgrid--grid-process
	      (start-process "qiv" nil
			     "~/src/pqiv/pqiv" "-c" "-f" "-i"
			     "/tmp/grid.svg"))
	(when t
	  (run-at-time 0.5 nil
		       (lambda ()
			 (start-process
			  "wmcrtl" nil
			  "wmctrl" "-r" "qiv" "-b" "add,above"))))))))

(defun touchgrid--reorient-grid (grid)
  (if (not touchgrid--rotation)
      grid
    (cl-loop for line in (reverse grid)
	     collect (cl-loop for elem in (reverse line)
			      collect elem))))

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

(defun touchgrid--inc-volume ()
  (movie-send-mpv-command '((command . ["add" "volume" "2"]))))

(defun touchgrid--dec-volume ()
  (movie-send-mpv-command '((command . ["add" "volume" "-2"]))))

(defun touchgrid--inc-sync ()
  (movie-send-mpv-command '((command . ["add" "audio-delay" "0.100"]))))

(defun touchgrid--dec-sync ()
  (movie-send-mpv-command '((command . ["add" "audio-delay" "-0.100"]))))

(defun touchgrid--inc-speed ()
  (movie-send-mpv-command '((command . ["multiply" "speed" "1.1"]))))

(defun touchgrid--dec-speed ()
  (movie-send-mpv-command '((command . ["multiply" "speed" "1/1.1"]))))

(defun touchgrid--subtitles ()
  (movie-send-mpv-command '((command . ["cycle" "sub"]))))

(defun touchgrid--interlace ()
  (movie-send-mpv-command '((command . ["cycle" "deinterlace"]))))

(defun touchgrid--audio ()
  (movie-send-mpv-command '((command . ["cycle" "audio"]))))



(defun touchgrid--play ()
  (touchgrid--emacs-focus)
  (touchgrid--remove-grid)
  (setq touchgrid--state "mpv")
  (unwind-protect
      (call-interactively 'movie-play-best-file)
    (setq touchgrid--state "emacs")
    (touchgrid--remove-grid)))

(defun touchgrid--one-window ()
  (touchgrid--emacs-focus)
  (delete-other-windows))

(defun touchgrid--delete ()
  (touchgrid--emacs-focus)
  (call-interactively 'movie-delete-file))

(defun touchgrid--undelete ()
  (touchgrid--emacs-focus)
  (call-interactively 'movie-undo-delete))

(defun touchgrid--tv-series ()
  (touchgrid--emacs-focus)
  (call-interactively 'movie-goto-last-series))

(defun touchgrid--last-seen ()
  (touchgrid--emacs-focus)
  (call-interactively 'movie-last-seen))

(defun touchgrid--reload ()
  (touchgrid--emacs-focus)
  (load "~/src/movie.el/movie.el")
  (load "~/src/touchgrid.el/touchgrid.el")
  (message "Reloaded"))

(defun touchgrid--prev ()
  (touchgrid--emacs-focus)
  (call-interactively 'scroll-down-command))

(defun touchgrid--next ()
  (touchgrid--emacs-focus)
  (call-interactively 'scroll-up-command))

(defun touchgrid--rescan ()
  (touchgrid--emacs-focus)
  (call-interactively 'movie-rescan))

(defun touchgrid--enter ()
  (touchgrid--emacs-focus)
  (call-interactively 'movie-find-file))

(defun touchgrid--exit ()
  (touchgrid--emacs-focus)
  (call-interactively 'bury-buffer))

(defun touchgrid--sort ()
  (touchgrid--emacs-focus)
  (call-interactively 'movie-toggle-sort))

(defun touchgrid--torrent ()
  (touchgrid--emacs-focus)
  (call-interactively 'movie-find-torrent))

(defun touchgrid--play-current ()
  (touchgrid--emacs-focus)
  (call-interactively 'movie-play-current)
  (touchgrid--emacs-focus))

(defun touchgrid--jump ()
  (touchgrid--emacs-focus)
  (call-interactively 'movie-jump-to-directory))


(defun touchgrid--none ()
  )

(defun touchgrid--emacs-focus ()
  (touchgrid--focus "emacs"))

(defun touchgrid--focus (string)
  (when-let ((id (touchgrid--find-window string)))
    (touchgrid--call-process
     "xdotool" nil nil nil "windowfocus" "--sync" id)))

(defun touchgrid--find-window (string)
  (with-temp-buffer
    (touchgrid--call-process
     "xdotool" nil (current-buffer) nil
     "search" "--onlyvisible" "--name" string)
    ;;(write-region (point-min) (point-max) "/tmp/find" nil 'silent)
    (goto-char (point-max))
    (when (re-search-backward "^\\([0-9]+\\)\n" nil t)
      (match-string 1))))

(defun touchgrid--display-name ()
  (with-temp-buffer
    (call-process "xrandr" nil t)
    (goto-char (point-min))
    (when (re-search-forward "^\\([^ ]+\\).*connected primary" nil t)
      (match-string 1))))

(defun touchgrid--toggle-rotation (event)
  (setq touchgrid--rotation (equal (cl-getf event :state) "1"))
  (touchgrid--call-process
   "/home/larsi/src/gnome-randr-rust/target/debug/gnome-randr"
   nil nil nil
   "modify" "eDP-1"
   "--rotate"
   (if (not touchgrid--rotation)
       "normal"
     "inverted")))

(defvar touchgrid--last-keyboard 0)

(defun touchgrid--wake-up (_event)
  (let ((time (float-time)))
    ;; If it's been more than a minute since last time, deactivate
    ;; xscreensaver.
    (when (> (- time touchgrid--last-keyboard) 60)
      (touchgrid--call-process
       "xscreensaver-command" nil nil nil "-deactivate")
      (setq touchgrid--last-keyboard time))))

(defvar touchgrid--pre-keyboard-state nil)

(defun touchgrid--keyboard ()
  (touchgrid--emacs-focus)
  (setq touchgrid--pre-keyboard-state touchgrid--state
	touchgrid--state "keyboard")
  (touchgrid--grid))

(defun touchgrid--handle-keyboard (action)
  (touchgrid--focus touchgrid--pre-keyboard-state)
  (with-suppressed-warnings ((interactive-only previous-line next-line))
    (let ((prev touchgrid--state))
      (setq action (substring (symbol-name action) 1))
      (pcase action
	("exit"
	 (touchgrid--remove-grid)
	 (setq touchgrid--state touchgrid--pre-keyboard-state))
	("one")
	("shift"
	 (setq touchgrid--state "keyboard-shift")
	 (touchgrid--remove-grid)
	 (touchgrid--grid))
	("<"
	 (left-char))
	(">"
	 (right-char))
	("up"
	 (previous-line))
	("down"
	 (next-line))
	(_
	 (let ((char (string (elt action 0))))
	   (if (equal touchgrid--pre-keyboard-state "emacs")
	       (setq unread-command-events
		     (append unread-command-events (listify-key-sequence char)))
	     (touchgrid--execute-mpv-key char)))))
      (when (and (equal prev "keyboard-shift")
		 (not (equal action "exit")))
        (setq touchgrid--state "keyboard")
	(touchgrid--remove-grid)
	(touchgrid--grid)))))

(defun touchgrid--execute-mpv-key (char)
  (let ((table (make-hash-table :test #'equal)))
    (touchgrid--parse-mpv-file table "~/.config/mpv/full-input.conf")
    (touchgrid--parse-mpv-file table "~/.config/mpv/input.conf")
    (when-let ((command (gethash char table)))
      (movie-send-mpv-command
       `((command . ,(cl-coerce (mapcar (lambda (c)
					  (if (string-match "^[-.0-9]+$" c)
					      (string-to-number c)
					    (intern c)))
					command)
				'array)))))))

(defun touchgrid--parse-mpv-file (table file)
  (with-temp-buffer
    (insert-file-contents file)
    (while (not (eobp))
      (when (looking-at "#?\\([^ ]\\) \\(.*\\)")
	(let ((key (match-string 1))
	      (command (match-string 2)))
	  (setq command (replace-regexp-in-string "#.*" "" command))
	  (setq command (replace-regexp-in-string "{encode}" "" command))
	  (setq command (split-string (string-trim command)))
	  (setf (gethash key table) command)))
      (forward-line 1))))

(provide 'touchgrid)

;;; touchgrid.el ends here

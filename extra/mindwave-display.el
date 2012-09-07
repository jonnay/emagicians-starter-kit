
;;; mindwave-display.el --- A simplified mindwave display file

;; Copyright (C) 2012 Jonathan Arkell

;; Author: Jonathan Arkell <jonnay@jonnay.net>
;; Created: 16 June 2012
;; Keywords: comint mindwave

;; This file is not part of GNU Emacs.
;; Released under the GPL     

  (require 'mindwave-emacs)


(defvar mw-display/timer nil
  "Timer responsible for updating the output buffer")

(defcustom mw-display/colors
  '((delta . ("RoyalBlue3" . "RoyalBlue4"))
    (theta . ("DeepSkyBlue3" . "DeepSkyBlue4"))
    (lowAlpha . ("cyan3" . "cyan4"))
    (highAlpha . ("aquamarine3" . "aquamarine4"))
    (lowBeta . ("yellow3" . "yellow4"))
    (highBeta . ("gold3" . "gold4"))
    (lowGamma . ("tan3" . "tan4"))
    (highGamma . ("firebrick3" . "firebrick4"))
    (attention . ("grey80" . "grey60"))
    (meditation . ("grey40" . "grey20")))
  "The colors to use when displaying the graph."
  :safe t
  :group 'mindwave)

(defun mw-display/show ()
  "Shows the output of the mindwave device in a nicely formatted buffer."
  (interactive)
  (mindwave-get-buffer)
  (let ((mwbuffer (get-buffer-create "*mindwave-status*")))
    (when (not (timerp mw-display/timer))
      (setq mw-display/timer (run-at-time t 1 'mw-display/write-values)))
    (save-excursion
      (buffer-disable-undo (set-buffer mwbuffer))
      (add-hook 'kill-buffer-hook 'mw-display/kill-timer nil t))
    (mw-display/write-values)
    (pop-to-buffer mwbuffer)))

(defun mw-display/kill-timer ()
  "Removes the timer"
  (when (timerp mw-display/timer)
    (cancel-timer mw-display/timer)
    (setq mw-display/timer nil)))

(defun mw-display/write-values ()
  "Actually write the values in the eeg buffer"
  (save-excursion
    (set-buffer "*mindwave-status*")
    (toggle-read-only 0)
    (erase-buffer)
    (insert (propertize "   Mindwave Status  \n" 
                        'face '(:background "white" :foreground "black")))
    (insert (format "%3d Signal\n\n" 
                    (cdr (assoc 'poorSignalLevel mindwave/current))))
    (mw-display/insert-eeg 'delta 'eegPower)
    (mw-display/insert-eeg 'theta 'eegPower)
    (mw-display/insert-eeg 'lowAlpha 'eegPower)
    (mw-display/insert-eeg 'highAlpha 'eegPower)
    (mw-display/insert-eeg 'lowBeta 'eegPower)
    (mw-display/insert-eeg 'highBeta 'eegPower)
    (mw-display/insert-eeg 'lowGamma 'eegPower)
    (mw-display/insert-eeg 'highGamma 'eegPower)
    (insert "\n")
    (mw-display/insert-eeg 'meditation 'eSense)
    (mw-display/insert-eeg 'attention 'eSense)
    (toggle-read-only 1)))

(defun mw-display/insert-eeg (band type)
  "Insert an eeg string.
If TYPE is eeg, the bargraph displayed will be out of 1 000 000"
  (let ((val (cdr (assoc band (cdr (assoc type mindwave/current))))))
    (insert (format "%-10s - %7d " band val)
            (if (equal type 'eegPower)
                (mw-display/graph val
                                  100000 
                                  band)
              (mw-display/graph val 
                                100 
                                band))
            "\n")))

(defun mw-display/graph (val total band)
  "Return a simple string bar graph from VAL and TOTAL"
  (let ((gsize (round (min (* (/ (float val) total) 
                               50)
                            50))))
    (propertize (format "%50s| %8s %12s " 
                        (propertize (make-string gsize ?#))
                        val
                        band)
                'face `(:background ,(car (cdr (assoc band mw-display/colors))) 
                        :foreground "grey1")
                        :weight "ultra-bold")))

(mw-display/write-values)
  (provide 'mindwave-display)

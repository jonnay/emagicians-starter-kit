
;;; solarized-mind.el --- changes emacs interface according to brainstate

;; Copyright (C) 2012 Jonathan Arkell

;; Author: Jonathan Arkell <jonnay@jonnay.net>
;; Created: 16 June 2012
;; Keywords: comint mindwave

;; This file is not part of GNU Emacs.
;; Released under the GPL     

(require 'mindwave-emacs)

(defun solarized-mind/brain-ring-full-hook (average)
  "Set up hook to solarize your mind, and set up the medicursor."
  (ring-insert mindwave/brain-ring average)
  (solarized-mind/set-medicursor (cdr (assoc 'meditation 
                                             (cdr (assoc 'eSense average)))))
  (solarized-mind/set-background (cdr (assoc 'attention
                                             (cdr (assoc 'eSense average))))))
(defun solarized-mind/start ()
  (interactive)
  (mindwave-get-buffer)
  (when (not (member 'solarized-mind/brain-ring-full-hook 'mindwave/brain-ring-full-hook))
    (message "Adding Mindwave hook")
    (add-hook 'mindwave/brain-ring-full-hook 'solarized-mind/brain-ring-full-hook)))

(defun solarized-mind/stop ()
  (interactive)
  (remove-hook 'mindwave/brain-ring-full-hook 'solarized-mind/brain-ring-full-hook))
(defun solarized-mind/set-medicursor (med)
  "Set the cursor to a value from the mindwave"
  (setq blink-cursor-interval
        (if ( = 0 med)
            0.25
            (+ 0.25
               (/ med 100.0)))))
    (require 'hexrgb)
  
  (defun solarized-mind/set-background (att)
    "Sets the background color"
    (set-background-color (solarized-mind/attention-to-rgb att))
    ;(set-frame-parameter nil 'background-color (solarized-mind/attention-to-rgb att))
    nil)
  
  ;(frame-parameter nil 'background-color)
  (defun solarized-mind/attention-to-rgb (att)
    "Takes an attention value (out of 100) and returns a color between #000000 and #002b36"
    (let ((h (hexrgb-hue "#002b36"))
          (s (hexrgb-saturation "#002b36"))
          (v (hexrgb-value "#002b36")))
      
                           (hexrgb-hsv-to-hex h 
                                              s 
                                              (* v (/ att 100.0)))))



;(set-face-attribute 'default nil :background (solarized-mind/attention-to-rgb 0))

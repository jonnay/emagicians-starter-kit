
;;; gander-into-org.el --- Gather Mindwave Data into an org file 

;; Copyright (C) 2012 Jonathan Arkell

;; Author: Jonathan Arkell <jonnay@jonnay.net>
;; Created: 16 June 2012
;; Keywords: comint mindwave

;; This file is not part of GNU Emacs.
;; Released under the GPL     

(provide 'gather-into-org)


(require 'mindwave-emacs)

(defvar dg-mindwave/org-buffer "Brain.org")
  
(defvar dg-mindwave/mark nil)

(defun dg-mindwave/generic-mark ()
  "Used to generically mark a section of the table"
  (interactive)
  (dg-mindwave/mark "mark"))

(defun dg-mindwave/mark (mark)
  "Set a mark on the section of a table"
  (interactive "sMark: ")
  (setq dg-mindwave/mark mark))
(defun dg-mindwave/if-assoc (key lst)
  (if (assoc key lst)
      (number-to-string (cdr (assoc key lst)))
      " "))

(defun dg-mindwave/get-in (lst key keylist)
  (let ((innerList (assoc key lst)))
    (mapconcat '(lambda (el)
                  (if (and innerList 
                           (assoc el innerList))
                       (number-to-string (cdr (assoc el innerList)))
                    "")) 
               keylist
               " | ")))

(defun dg-mindwave/collect-and-write (out)
  "Hook function to gather and write data to the table."
  (when (and (assoc 'eSense out)
             (assoc 'eegPower out))
    (let ((string-write (concat "| " 
                                (format-time-string "%s")
                                " | "
                                (dg-mindwave/if-assoc 'poorSignalLevel out) 
                                " | "
                                (dg-mindwave/get-in out 'eegPower '(highGamma lowGamma highBeta lowBeta highAlpha lowAlpha theta delta))
                                " | "
                                (dg-mindwave/get-in out 'eSense '(attention meditation))
                                " | "
                                (when dg-mindwave/mark
                                  (let ((m dg-mindwave/mark))
                                    (setq dg-mindwave/mark)
                                    m))
                                " | "                          
                                "\n")))
      (with-current-buffer dg-mindwave/org-buffer 
        (goto-char (point-max))
        (insert string-write)))))

(defun dg-mindwave/start-recording-session (name)
  "Sets up an entirely new mindwave session for recording." 
  (interactive "sMindwave Session Name: ")
  (with-current-buffer dg-mindwave/org-buffer
    (goto-char (point-max))
    (insert "\n\n")
    (insert "*** ")
    (insert (current-time-string))
    (insert "  ")
    (insert name)
    (insert "\n")
    (insert "#+TBLNAME: ")
    (insert name)
    (insert "\n")
    (insert "|------------+--------+-----------+----------+----------+---------+-----------+----------+--------+---------+------------+-----------+------|\n")
    (insert "|       time | signal | highGamma | lowGamma | highBeta | lowBeta | highAlpha | lowAlpha |  theta |   delta | meditation | attention | mark |\n")
    (insert  "|------------+--------+-----------+----------+----------+---------+-----------+----------+--------+---------+------------+-----------+------|\n"))
  (mindwave-get-buffer)
  (when (not (member 'dg-mindwave/collect-and-write 'mindwave-hook))
    (add-hook 'mindwave-hook 'dg-mindwave/collect-and-write)))

(defun dg-mindwave/stop-recording-session ()
  "Stops a recording session"
  (interactive)
  (remove-hook 'mindwave-hook 'dg-mindwave/collect-and-write))




(defun dg-mindwave/start-45-second-session (name) 
  "Start a 45 second session with appropriate marks.  NAME should be a simple name."
  (interactive "s45 Second Session Name:")
  (dg-mindwave/start-recording-session name)
  (run-at-time 15 nil '(lambda ()
                         (message "Close your Eyes and Relax")
                         (beep 1) 
                         (dg-mindwave/mark "relaxed")))
  (run-at-time 30 nil `(lambda ()
                         (message ,name)
                         (beep 1)
                         (dg-mindwave/mark ,name)))
  (run-at-time 45 nil '(lambda ()
                         (beep 1)
                         (message "stop")
                         (dg-mindwave/stop-recording-session))))

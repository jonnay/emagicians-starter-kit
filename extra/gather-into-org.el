
;;; mindwave-emacs.el --- Neurosky mindwave support

;; Copyright (C) 2012 Jonathan Arkell

;; Author: Jonathan Arkell <jonnay@jonnay.net>
;; Created: 16 June 2012
;; Keywords: comint mindwave

;; This file is not part of GNU Emacs.
;; Released under the GPL     


(require 'mindwave-emacs)

(defvar dg-mindwave/org-buffer "Brain.org")
  
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


(defun dg-mindwave/collect-and-write (output)
  "Hook function to gather and write data to the table."
      (let* ((out (json-read-from-string output))
             (string-write (concat "| " 
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
          (insert string-write))))

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
  (remove-hook 'mindwave-hook 'dg-mindwave/collect-and-write)
  )

(defun dg-mindwave/make-results-table (name)
  "Generate a results table for a mindwave session"
  (interactive "sMindwave Session Name: ")
  (insert "\n")
  (insert "#+TBLNAME: ")
  (insert name)
  (insert "_results")
  (insert "\n")
  (insert " |         |      signal | highGamma |  lowGamma |  highBeta |   lowBeta | highAlpha |  lowAlpha |     theta |     delta | meditation | attention |") (insert "\n")
  (insert " |---------+-------------+-----------+-----------+-----------+-----------+-----------+-----------+-----------+-----------+------------+-----------|") (insert "\n")
  (insert " | vmean   | 0.061611374 | 12192.720 | 15232.820 | 19399.642 | 15180.616 | 17033.287 | 22201.699 | 76134.531 | 270353.25 |  53.241706 | 53.424171 |") (insert "\n")
  (insert " | vmedian |           0 |    8132.5 |     10014 |   14247.5 |    9695.5 |    8411.5 |    9076.5 |   23773.5 |     62936 |         54 |        56 |") (insert "\n")
  (insert " | vmax    |          26 |     86970 |    152111 |    192200 |    260706 |    363667 |    799014 |    820033 |   2920134 |        100 |       100 |") (insert "\n")
  (insert " | vmin    |           0 |       303 |       378 |       638 |       342 |       436 |       311 |      2025 |       300 |          0 |         0 |") (insert "\n")
  (insert " | vsdev   |   1.2656602 | 12190.021 | 15797.156 | 17531.918 | 20699.664 | 29733.997 | 51731.083 | 124792.48 | 449634.67 |  22.641340 | 17.949459 |") (insert "\n")

(insert (concat "    #+TBLFM: @2$2=vmean(remote(" name ",@II$2..@III$2))::@3$2=vmedian(remote(" name ",@II$2..@III$2))::@4$2=vmax(remote(" name ",@II$2..@III$2))::@5$2=vmin(remote(" name ",@II$2..@III$2))::@6$2=vsdev(remote(" name ",@II$2..@III$2))::@2$3=vmean(remote(" name ",@II$3..@III$3))::@3$3=vmedian(remote(" name ",@II$3..@III$3))::@4$3=vmax(remote(" name ",@II$3..@III$3))::@5$3=vmin(remote(" name ",@II$3..@III$3))::@6$3=vsdev(remote(" name ",@II$3..@III$3))::@2$4=vmean(remote(" name ",@II$4..@III$4))::@3$4=vmedian(remote(" name ",@II$4..@III$4))::@4$4=vmax(remote(" name ",@II$4..@III$4))::@5$4=vmin(remote(" name ",@II$4..@III$4))::@6$4=vsdev(remote(" name ",@II$4..@III$4))::@2$5=vmean(remote(" name ",@II$5..@III$5))::@3$5=vmedian(remote(" name ",@II$5..@III$5))::@4$5=vmax(remote(" name ",@II$5..@III$5))::@5$5=vmin(remote(" name ",@II$5..@III$5))::@6$5=vsdev(remote(" name ",@II$5..@III$5))::@2$6=vmean(remote(" name ",@II$6..@III$6))::@3$6=vmedian(remote(" name ",@II$6..@III$6))::@4$6=vmax(remote(" name ",@II$6..@III$6))::@5$6=vmin(remote(" name ",@II$6..@III$6))::@6$6=vsdev(remote(" name ",@II$6..@III$6))::@2$7=vmean(remote(" name ",@II$7..@III$7))::@3$7=vmedian(remote(" name ",@II$7..@III$7))::@4$7=vmax(remote(" name ",@II$7..@III$7))::@5$7=vmin(remote(" name ",@II$7..@III$7))::@6$7=vsdev(remote(" name ",@II$7..@III$7))::@2$8=vmean(remote(" name ",@II$8..@III$8))::@3$8=vmedian(remote(" name ",@II$8..@III$8))::@4$8=vmax(remote(" name ",@II$8..@III$8))::@5$8=vmin(remote(" name ",@II$8..@III$8))::@6$8=vsdev(remote(" name ",@II$8..@III$8))::@2$9=vmean(remote(" name ",@II$9..@III$9))::@3$9=vmedian(remote(" name ",@II$9..@III$9))::@4$9=vmax(remote(" name ",@II$9..@III$9))::@5$9=vmin(remote(" name ",@II$9..@III$9))::@6$9=vsdev(remote(" name ",@II$9..@III$9))::@2$10=vmean(remote(" name ",@II$10..@III$10))::@3$10=vmedian(remote(" name ",@II$10..@III$10))::@4$10=vmax(remote(" name ",@II$10..@III$10))::@5$10=vmin(remote(" name ",@II$10..@III$10))::@6$10=vsdev(remote(" name ",@II$10..@III$10))::@2$11=vmean(remote(" name ",@II$11..@III$11))::@3$11=vmedian(remote(" name ",@II$11..@III$11))::@4$11=vmax(remote(" name ",@II$11..@III$11))::@5$11=vmin(remote(" name ",@II$11..@III$11))::@6$11=vsdev(remote(" name ",@II$11..@III$11))::@2$12=vmean(remote(" name ",@II$12..@III$12))::@3$12=vmedian(remote(" name ",@II$12..@III$12))::@4$12=vmax(remote(" name ",@II$12..@III$12))::@5$12=vmin(remote(" name ",@II$12..@III$12))::@6$12=vsdev(remote(" name ",@II$12..@III$12))")))


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

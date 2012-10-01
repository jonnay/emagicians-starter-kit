
;;; 23-million-erisian.el --- Gnosis quantified with Neurosky.
  
  ;; Copyright (C) 2012 Jonathan Arkell
  
  ;; Author: Jonathan Arkell <jonnay@jonnay.net>
  ;; Created: 16 June 2012
  ;; Keywords: mindwave
  ;; Version 0.1 
  
  ;; This file is not part of GNU Emacs.
  ;; Released under the GPL     
  
  ;;; Commentary: 
  ;; Please see the org-file that this was generated from. 

(defgroup 23-million-erisian nil 
  "23 Million Man.  An stats collector for mindwave")

(defcustom 23-million-erisian/store-in-org-file "~/Dropbox/org/Brain.org"
  "Full file path of where to store the data."
  :group '23-million-erisian
  :type 'file)

(defcustom 23-million-erisian/tblname-of-data "23million"
  "Name of the table where data is to be stored."
  :type 'symbol
  :group '23-million-erisian)

(defcustom 23-million-erisian/ring-averages-per-insertion 2
  "Number of insertions to put into the table every time the ring fills up.
This is your effective resolution.  Here are some general approximations and timing:

val  time
1    30 seconds
2    1 minute
30   15 minutes
60   30 minutes
120  1 hour"
  :group '23-million-erisian)

(defvar 23-million-erisian/running-average-data nil
  "Data structure to hold the running average.
It is in the format of:
 (total-as-int . average-brain-ring-so-far)")

(defun 23-million-erisian/running-average-hook (average-brain-ring)
  "Takes an AVERAGE-BRAIN-RING, and then updates the minute average."
  (let ((total (+ (if 23-million-erisian/running-average-data
                      (car 23-million-erisian/running-average-data)
                    0)
                  1))
        (average (if 23-million-erisian/running-average-data
                     (cdr 23-million-erisian/running-average-data)
                   (mindwave/make-single-val-brain-ring 0))))
    (setq 23-million-erisian/running-average-data
          (cons total
                (mindwave/brain-ring-apply 'mindwave/safe-div
                                           (mindwave/brain-ring-apply '+  
                                                                     average
                                                                     average-brain-ring)
                                           (mindwave/make-single-val-brain-ring total))))
    (when (>= total 23-million-erisian/ring-averages-per-insertion)
      (23-million-erisian/do-insertion-into-file (cdr 23-million-erisian/running-average-data))
      (setq 23-million-erisian/running-average-data nil))))


(defvar 23-million-erisian/tbl-buffer-pos nil)
(defconst 23-million-erisian/tbl-current-pos-marker "#mindwave-23million-pos")

(defun 23-million-erisian/do-insertion-into-file (data)
  "Inserts DATA into the 23-million-erisian table."
  (save-excursion
    (progn 
      (set-buffer (find-file-noselect 23-million-erisian/store-in-org-file))
      (23-million-erisian/find-buffer-pos)        
      (goto-char 23-million-erisian/tbl-buffer-pos)
      (when (not (string-equal (buffer-substring-no-properties (line-beginning-position) 
                                                               (line-end-position))
                               23-million-erisian/tbl-current-pos-marker))
        (error (concat "23-million-erisian: ARG, can't find the proper position to insert data! make sure you have '" 23-million-erisian/tbl-current-pos-marker "' at the bottom of your data table."))))
    (delete-region (line-beginning-position) 
                   (line-end-position))
    (23-million-erisian/write-running-average data)
    (setq 23-million-erisian/tbl-buffer-pos (line-beginning-position))
    (insert 23-million-erisian/tbl-current-pos-marker)
    (insert "\n"))) 

(defun 23-million-erisian/find-buffer-pos () 
  "find the current insert buffer position for the mindwave table.  
Start by opening the file if we have to."
  (save-excursion 
    (set-buffer (find-file-noselect 23-million-erisian/store-in-org-file))
    (goto-char (point-min))
    (if (re-search-forward (concat "^[ \t]*#\\+TBLNAME:[ \t]*" 
                                   23-million-erisian/tblname-of-data
                                   "[ \t]*$")
                           nil t)
        (progn 
          (goto-char (match-beginning 0))
          (if (re-search-forward 23-million-erisian/tbl-current-pos-marker nil t)
              (setq 23-million-erisian/tbl-buffer-pos (match-beginning 0))
            (error (concat "Cant find marker to insert data.  Make sure you have " 23-million-erisian/tbl-current-pos-marker " on your table."))))
      (error (concat "Can't find table data " 23-million-erisian/tblname-of-data)))))
  
(ert-deftest 23-million-erisian/find-buffer-pos () 
  ""
  (should (< 0 (23-million-erisian/find-buffer-pos)))
  (should-not (null 23-million-erisian/tbl-buffer-pos)))

(defun 23-million-erisian/write-running-average (brain)
  "Writes the running average.  used as part of a run-timer"
  (let ((time (decode-time)))
                    ;   y     m     d     h     m    s      g     g    b      b     a     a    t      d     m     a     
    (insert (format "| %4s | %5s | %3s | %4s | %6s | %6s | %9s | %8s | %8s | %7s | %9s | %8s | %8s | %10s | %10s | %9s | \n"
                    (nth 5 time)
                    (nth 4 time)
                    (nth 3 time)
                    (nth 2 time)
                    (nth 1 time)
                    (cdr (assoc 'poorSignalLevel brain))
                    (mindwave/access-in 'eegPower 'lowGamma brain)
                    (mindwave/access-in 'eegPower 'highGamma brain)
                    (mindwave/access-in 'eegPower 'lowBeta brain)
                    (mindwave/access-in 'eegPower 'highBeta brain)
                    (mindwave/access-in 'eegPower 'lowAlpha brain)
                    (mindwave/access-in 'eegPower 'highAlpha brain)
                    (mindwave/access-in 'eegPower 'theta brain)
                    (mindwave/access-in 'eegPower 'delta brain)
                    (mindwave/access-in 'eSense 'meditation brain)
                    (mindwave/access-in 'eSense 'attention brain)))))

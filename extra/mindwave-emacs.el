
;;; mindwave-emacs.el --- Neurosky mindwave support

;; Copyright (C) 2012 Jonathan Arkell

;; Author: Jonathan Arkell <jonnay@jonnay.net>
;; Created: 16 June 2012
;; Keywords: comint mindwave

;; This file is not part of GNU Emacs.
;; Released under the GPL     

;;; Commentary: 
;; Please see the org-file that this was generated from.

(require 'json)

(defvar mindwave-host "localhost")
(defvar mindwave-port 13854)

(defvar mindwave-appName "mindwave-emacs")
(defvar mindwave-appKey (sha1 mindwave-appName))

(defvar mindwave-buffer nil "Variable to store the buffer connected to the process")
(defvar mindwave-process nil "Process that mindwave is connected")

(defun mindwave-get-buffer ()
  "Returns the buffer for the mindwave connection"
  (if (and mindwave-process (process-live-p mindwave-process))
      mindwave-process
      (progn
  (setq mindwave-buffer (make-comint "mindwave" (cons mindwave-host mindwave-port)))
  (setq mindwave-process (get-buffer-process mindwave-buffer))
  (save-excursion
    (set-buffer mindwave-buffer)
    (sleep-for 1)
    (mindwave-authorize)
    (sleep-for 1)
    (mindwave-get-raw nil)
    (sleep-for 1)
    (add-hook 'comint-output-filter-functions 'mindwave-comint-filter-function nil t))
  mindwave-buffer)))

(defun mindwave-send-string (str)
  "Helper function to send STRING directly to the mindwave.
Please use `mindwave-authorize' or `mindwave-get-raw' for user-level configuration."
  (comint-send-string mindwave-process str))

(defvar mindwave-hook '() "Hooks to run when mindwave gets standard input")
(defvar mindwave-blink-hook '() "Hooks to run when mindwave gets blink input")
(defvar mindwave-raw-hook '() "Hooks to run when mindwave gets raw input")

(defun mindwave-comint-filter-function (output)
  "A helper hook to pass off output to the apropriate hooks"
  (let ((collected-raw '()))
    (loop for out 
          in (split-string output "\C-j" t)
          do
          (cond ((and (> (length out) 10) 
                      (string-equal (substring out 0 10) "{\"rawEeg\":"))
                 (setq collected-raw (cons (substring out 10 -1) collected-raw)))
                
                ((and (> (length out) 17) 
                      (string-equal (substring out 0 17) "{\"blinkStrength\":"))
                 (run-hook-with-args 'mindwave-blink-hook (substring out 17 -2)))
                
                ((string-equal "{" (substring out 0 1))
                 (run-hook-with-args 'mindwave-hook out))))
    (when (> (length collected-raw) 0)
      (run-hook-with-args 'mindwave-raw-hook collected-raw)))
  output)

(defvar mindwave-authorized-p nil "whether or not app is authorized")

(defun mindwave-authorize () 
  "provides an autorization request to the mindwave server"
  (mindwave-send-string (json-encode `(("appName" . ,mindwave-appName) 
                                       ("appKey" . ,mindwave-appKey)))))

(defun mindwave-get-raw (raw)
  "Return raw output from mindwave.
RAW is a boolean value as to whether or not to listen for raw values"
  (mindwave-send-string (json-encode `(("enableRawOutput" . ,(if raw t json-false))
                                      ("format" . "Json")))))

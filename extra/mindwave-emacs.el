
;;; mindwave-emacs.el --- Neurosky mindwave support

;; Copyright (C) 2012 Jonathan Arkell

;; Author: Jonathan Arkell <jonnay@jonnay.net>
;; Created: 16 June 2012
;; Keywords: comint mindwave
;; Version 0.1 

;; This file is not part of GNU Emacs.
;; Released under the GPL     

;;; Commentary: 
;; Please see the org-file that this was generated from.

(require 'json)

(defcustom mindwave-poor-signal-level 50
  "The signal level that mindwave-emacs should stop running hooks at.

The mindwave API sends a poorSignal level hook whenever it 
senses connection problems.  This is generally between 0 and
200.

  0   - Best connection
  200 - completely off the users head. (get it?)"
  :type 'integer
  :group 'mindwave-emacs)

(defvar mindwave-host "localhost")
(defvar mindwave-port 13854)

(defvar mindwave-appName "mindwave-emacs")
(defvar mindwave-appKey (sha1 mindwave-appName))

(defconst mindwave-serial-baud 57600)
(defconst mindwave-auth-key 0000)

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
          (buffer-disable-undo mindwave-buffer)
          (sleep-for 1)
          (mindwave-authorize)
          (sleep-for 1)
          (mindwave-get-raw nil)
          (sleep-for 1)
          (add-hook 'comint-output-filter-functions 'mindwave-comint-filter-function nil t))
        mindwave-buffer)))

(defun mindwave-make-serial-process ()
  "Creates a serial process for mindwave, or returns the current one if it exists.
Note that this function assumes that you'll only ever have one mindwave connected."
  (if (process-live-p mindwave-serial-process)
      mindwave-serial-process
    (setq mindwave-serial-process (make-serial-process :port mindwave-serial-port
                                                       :speed mindwave-serial-baud
                                                       :coding-system 'binary
                                                       :filter mindwave-serial-filter))))

(defun mindwave-send-string (str)
  "Helper function to send STRING directly to the mindwave.
Please use `mindwave-authorize' or `mindwave-get-raw' for user-level configuration."
  (comint-send-string mindwave-process str))

(defvar mindwave-hook '() "Hooks to run when mindwave gets standard input\nShould be a in a list that conforms to the json output.")
(defvar mindwave-blink-hook '() "Hooks to run when mindwave gets blink input")
(defvar mindwave-raw-eeg-hook '() "Hooks to run when mindwave gets raw eeg input.\n Note that you can get up to 512 of these events per second!")
(defvar mindwave-e-sense-hook '() "Hooks to run when mindwave gets an eSense(tm) reading")
(defvar mindwave-eeg-power-hook '() "Hooks to run when mindwave gets an eegPower reading")

(defun mindwave-if-in-list-run-hook (key list hook &rest funcs)
  (when (assoc key list)
    (when (not (null funcs))
      (dolist func funcs 
              (apply func (cdr (assoc key list)))))
    (run-hook-with-args hook (cdr (assoc key list)))))

(defmacro mindwave-if-in-list (key list &rest forms)
  "Helper macro to bind the mw-result to (assoc KEY LIST) and run FORMS"
  (declare (indent 2))
  `(let ((mw-result (assoc ,key ,list)))
     (if mw-result
         (progn 
           (setq mw-result  (cdr mw-result))
           ,@forms)
       nil)))

(ert-deftest mindwave/test-if-in-list ()
    ""
  (let ((r nil))
    (mindwave-if-in-list 'a '() (setq r 't))
    (should (not r)))
  (let ((r nil))
    (debug)
    (mindwave-if-in-list 'a '((a 1)) (setq r mw-result))
    (should r)))

(defun mindwave-comint-filter-function (output)
  "A helper hook to pass off output to the apropriate hooks"
  (when (and (stringp output) 
             (string= (substring output 0 1) "{"))   
    (loop for out 
          in (split-string output "\C-j" t)
          do
          (let ((brain (json-read-from-string out)))
            (run-hook-with-args 'mindwave-hook brain)
            (if (and (assoc 'poorSignalLevel brain)
                     (> (cdr (assoc 'poorSignalLevel brain))
                        mindwave-poor-signal-level))
                (run-hook-with-args 'mindwave-poor-signal-hook 
                                    (cdr (assoc 'poorSignalLevel brain)))
              (progn
                (mindwave-if-in-list-run-hook 'rawEeg brain 'mindwave-raw-hook)
                (mindwave-if-in-list 'poorSignalLevel brain
                  (mindwave/set-current 'poorSignalLevel mw-result)
                  (run-hook-with-args 'mindwave-poor-signal-hook mw-result))
                (mindwave-if-in-list 'eSense brain
                  (mindwave/set-current 'eSense mw-result)
                  (run-hook-with-args mindwave-e-sense-hook mw-result))
                (mindwave-if-in-list 'blink brain
                  (mindwave/set-current 'blink mw-result)
                  (run-hook-with-args 'mindwave-blink-hook mw-result))
                (mindwave-if-in-list 'eegPower brain
                  (mindwave/set-current 'eegPower mw-result)
                  (run-hook-with-args 'mindwave-eeg-power-hook mw-result)
                  (mindwave/brain-ring-update brain)))))))
  output)

(defvar mindwave/current '((poorSignalLevel . 200)
                           (eSense . ((attention . 0)
                                      (meditation . 0)))
                           (eegPower . ((delta      . 0)
                                        (theta      . 0)
                                        (lowAlpha   . 0)
                                        (highAlpha  . 0)
                                        (lowBeta    . 0)
                                        (highBeta   . 0)
                                        (lowGamma   . 0)
                                        (highGamma  . 0)))
                           (blink . 0))
  "The last known values from the mindwave headset.")

(defun mindwave/set-current (key val)
  (setq mindwave/current (list (if (equal key 'poorSignalLevel)
                                   (cons key val)
                                   (assoc 'poorSignalLevel mindwave/current))
                               (if (equal key 'eSense)
                                   (cons key val)
                                   (assoc 'eSense mindwave/current))
                               (if (equal key 'eegPower)
                                   (cons key val)
                                   (assoc 'eegPower mindwave/current))
                               (if (equal key 'blink)
                                   (cons key val)
                                   (assoc 'blink mindwave/current)))))

(ert-deftest mindwave/current-setters ()
  (setq mindwave/current '((poorSignalLevel . 200)
                           (eSense . ((attention . 0)
                                      (meditation . 0)))
                           (eegPower . ((delta      . 0)
                                        (theta      . 0)
                                        (lowAlpha   . 0)
                                        (highAlpha  . 0)
                                        (lowBeta    . 0)
                                        (highBeta   . 0)
                                        (lowGamma   . 0)
                                        (highGamma  . 0)))
                           (blink . 0)))
  (mindwave/set-current 'blink 255)
  (should (equal (assoc 'blink mindwave/current)
                 '(blink . 255)))

  (should (equal mindwave/current
                 '((poorSignalLevel . 200)
                   (eSense . ((attention . 0)
                              (meditation . 0)))
                   (eegPower . ((delta      . 0)
                                (theta      . 0)
                                (lowAlpha   . 0)
                                (highAlpha  . 0)
                                (lowBeta    . 0)
                                (highBeta   . 0)
                                (lowGamma   . 0)
                                (highGamma  . 0)))
                   (blink . 255)))))

(defconst mindwave/brain-ring-size 30)

(defvar mindwave/brain-ring (make-ring mindwave/brain-ring-size))
(defvar mindwave/brain-ring-reset-counter 0)

(defvar mindwave/brain-ring-full-hook '() "Hook to call when the brain ring is full")

(defun mindwave/access-in (outer-key inner-key list)
  "Access the value of INNER-KEY from OUTER-KEY of alist LIST"
  (cdr (assoc inner-key (cdr (assoc outer-key list)))))

(ert-deftest mindwave/test-access-in ()
  (should (equal (should (equal (mindwave/access-in 'outer 
                                                    'inner 
                                                    '((outer1 . (inner1 . 0))
                                                      (outer . ((inner . 23)))))
                              23)))))

(defun mindwave/brain-ring-make-entry (meditation attention delta theta lowAlpha highAlpha lowBeta highBeta lowGamma highGamma)
    "convenience function to make a valid brain ring entry"
    `((eSense . ((meditation  . ,meditation)
                 (attention   . ,attention)))
      (eegPower . ((delta     . ,delta) 
                   (theta     . ,theta)
                   (lowAlpha  . ,lowAlpha)
                   (highAlpha . ,highAlpha)
                   (lowBeta   . ,lowBeta)
                   (highBeta  . ,highBeta)
                   (lowGamma  . ,lowGamma)
                   (highGamma . ,highGamma)))))

(defun mindwave/brain-ring-apply (fn ring1 ring2)
  "Takes the \"brain-rings\" RING1 and RING2 and runs FN on it's guts"
  (mindwave/brain-ring-make-entry 
   (funcall fn (mindwave/access-in 'eSense 'meditation ring1)
               (mindwave/access-in 'eSense 'meditation ring2))
   (funcall fn (mindwave/access-in 'eSense 'attention ring1)
               (mindwave/access-in 'eSense 'attention ring2))
   (funcall fn (mindwave/access-in 'eegPower 'delta ring1)
               (mindwave/access-in 'eegPower 'delta ring2))
   (funcall fn (mindwave/access-in 'eegPower 'theta ring1)
               (mindwave/access-in 'eegPower 'theta ring2))
   (funcall fn (mindwave/access-in 'eegPower 'lowAlpha ring1)
               (mindwave/access-in 'eegPower 'lowAlpha ring2))
   (funcall fn (mindwave/access-in 'eegPower 'highAlpha ring1)
               (mindwave/access-in 'eegPower 'highAlpha ring2))
   (funcall fn (mindwave/access-in 'eegPower 'lowBeta ring1)
               (mindwave/access-in 'eegPower 'lowBeta ring2))
   (funcall fn (mindwave/access-in 'eegPower 'highBeta ring1)
               (mindwave/access-in 'eegPower 'highBeta ring2))
   (funcall fn (mindwave/access-in 'eegPower 'lowGamma ring1)
               (mindwave/access-in 'eegPower 'lowGamma ring2))
   (funcall fn (mindwave/access-in 'eegPower 'highGamma ring1)
               (mindwave/access-in 'eegPower 'highGamma ring2))))

(ert-deftest mindwave/test-brain-ring-add ()
  (should (equal (mindwave/brain-ring-make-entry 0 0 0 0 0 0 0 0 0 0)
                 (mindwave/brain-ring-apply '+ 
                                            (mindwave/brain-ring-make-entry 0 0 0 0 0 0 0 0 0 0)
                                            (mindwave/brain-ring-make-entry 0 0 0 0 0 0 0 0 0 0))))
  (should (equal (mindwave/brain-ring-make-entry 1 2 3 4 5 6 7 8 9 10)
                 (mindwave/brain-ring-apply '+
                                            (mindwave/brain-ring-make-entry 1 2 3 4 5 6 7 8 9 10)
                                            (mindwave/brain-ring-make-entry 0 0 0 0 0 0 0 0 0 0))))
  (should (equal (mindwave/brain-ring-make-entry 2 3 4 5 6 7 8 9 10 11)
                 (mindwave/brain-ring-apply '+
                                            (mindwave/brain-ring-make-entry 1 2 3 4 5 6 7 8 9 10)
                                            (mindwave/brain-ring-make-entry 1 1 1 1 1 1 1 1 1 1)))))

(defun mindwave/brain-ring-update (brain)
  "Keep a running tally of your neurological state."
  (when (and (assoc 'eSense brain)
             (assoc 'eegPower brain)
             (assoc 'poorSignalLevel brain)
             (> mindwave-poor-signal-level
                (cdr (assoc 'poorSignalLevel brain))))
    (ring-insert mindwave/brain-ring  brain)
    (when (>= (ring-length mindwave/brain-ring) 
              mindwave/brain-ring-size)
      (let ((new-ring (make-ring mindwave/brain-ring-size))
            (s mindwave/brain-ring-size)
            (collapsed-ring (reduce #'(lambda (brain total) 
                                        (mindwave/brain-ring-apply '+ brain total)) 
                                    (ring-elements mindwave/brain-ring)
                                    :initial-value (mindwave/brain-ring-make-entry 0 0 0 0 0 0 0 0 0 0))))
        (setq mindwave/brain-ring new-ring)
        (run-hook-with-args 'mindwave/brain-ring-full-hook
                            (mindwave/brain-ring-apply '/ 
                                                       collapsed-ring 
                                                       (mindwave/brain-ring-make-entry s s s s s s s s s s)))))))

(defun mindwave-get-raw (raw)
  "Return raw output from mindwave.
RAW is a boolean value as to whether or not to listen for raw values"
  (mindwave-send-string (json-encode `(("enableRawOutput" . ,(if raw t json-false))
                                      ("format" . "Json")))))

(defvar mindwave-authorized-p nil "whether or not app is authorized")

(defun mindwave-authorize () 
  "provides an autorization request to the mindwave server"
  (mindwave-send-string (json-encode `(("appName" . ,mindwave-appName) 
                                       ("appKey" . ,mindwave-appKey)))))

(provide 'mindwave-emacs)

;;; mindwave-emacs.el ends here

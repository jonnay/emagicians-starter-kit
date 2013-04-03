;; Various utilities

(require 'cl)

(defun format-expand (formatters format &optional arg)
  "Formats `format' according to `formatters' passing `arg' as an optional argument."
  (save-match-data
    (let ((regex (concat "%\\("
                         (reduce (lambda (a b)
                                   (concat a "\\|" b))
                                 (mapcar #'car formatters))
                         "\\)")))
      (replace-regexp-in-string regex
                                (lambda (str)
                                  (let ((fun (assoc (substring str 1)
                                                    formatters)))
                                    (if fun
                                        (funcall (cdr fun) arg)
                                        (error "Unrecognized format sequence: %s" str))))
                                format t t))))

(defvar notify-send-last-msg '())

;; Send notifications using libnotify.
(defun notify-send (summary &optional text icon)
  "Show a notification and ding for good measure."
  (interactive)
  (shell-command (concat "notify-send --hint=int:transient:1"
                         (if icon (concat " --icon=" (expand-file-name icon)) "")
                         " '" summary "'"
                         (if text (concat " '" text "'") "")))
  (my-play-sound (concat my-stuff-dir "ding.wav"))
  (setq notify-send-last-msg (list summary text icon)))

(defun notify-send-last ()
  "Show the last notification."
  (interactive)
  (if (null notify-send-last-msg)
    (message "No previous notification.")
    (apply #'notify-send notify-send-last-msg)))

(defun my-play-sound (sound)
  "Plays a sound."
  (start-process "play" nil "play" sound)
  "Done!")

(defun split-string-on-case (string &optional down-p)
  (let* ((len (length string))
         (start (- len 1))
         (end len)
         (matches ())
         (cas (if down-p
                  #'downcase
                  #'upcase)))
    (while (/= start -1)
      (let ((char (aref string start)))
        (when (= char (funcall cas char))
          (setq matches (cons (substring string start end)
                              matches))
          (setq end start)))
      (setq start (- start 1)))
    (if (/= end 0)
        (cons (substring string 0 end)
              matches)
        matches)))

(provide 'misc-utils)
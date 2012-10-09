
;; TODO: Documentation
;; TODO: Make it persistent by saving to a file
;; TODO: Make an `unlocks' attribute so that using org-mode will (require 'org-achievements)
;; TODO: easy way to show a random unearned achievement, perhaps on an idle timer

(require 'cl)

(defvar achievements-list nil
  "List of all possible achievements.")

(defvar achievement-score 0
  "Score of all earned achievements.")

;;{{{ Defining achievements

(defstruct
    (emacs-achievement
     (:constructor nil)
     (:constructor make-achievement
                   (name description
                         ;; &optional (predicate t)
                         &key
                         ;; slots
                         points transient min-score predicate
                         ;; convenience
                         package variable command
                         &aux (predicate
                               `(lambda ()
                                  ;; package
                                  (and
                                   ,@(when package
                                       (list (list 'featurep
                                                   (list 'quote package))))
                                   ,@(when variable
                                       (list (list 'achievements-variable-was-set
                                                   (list 'quote variable))))
                                   ,@(when command
                                       (list (list 'achievements-command-was-run
                                                   (list 'quote command))))
                                   ;; TODO: allow functions here not just forms
                                   ,@(when predicate
                                       (list predicate))))))))

  (name nil :read-only t)
  description
  (points 5)
  predicate ;; t if satisfied, nil if opted out, otherwise a function which should return non-nil on success
  transient ;; if non-nil then results won't be saved, but constantly re-evaluated.
  (min-score 0)
  )

(defmacro defachievement (name &rest body)
  `(add-to-list 'achievements-list
                ,(if (stringp (car-safe body))
                     `(make-achievement ,name ,@body)
                   `(make-achievement ,name nil ,@body)
                   )
                t
                ;; compare function
                ))

(defmacro defcommand-achievements (format-str body &rest arguments)
  (cons 'progn
       (loop for achiev in body
             collect (append
                      (list 'defachievement
                            (cadr achiev)
                            (format format-str
                                    (car achiev)
                                    (cddr achiev))
                            :command (list 'function (car achiev)))
                      arguments))))

(defmacro defvalue-achievements (var format-str body &rest arguments)
  (cons 'progn
        (loop for achiev in body
              collect (append
                       (list 'defachievement
                             (car achiev)
                             (format format-str
                                     (if (car-safe (cddr achiev))
                                         (car (cddr achiev))
                                       (cadr achiev))
                                     var)
                             :variable (list 'quote
                                             (list var (cadr achiev))))
                       arguments))))

;;}}}
;;{{{ Testing achievements

(defun achievements-variable-was-set (var)
  "Return non-nil if VAR has been set in customize or .emacs"
  (if (listp var)
      (equal (symbol-value (car var)) (cdr var))
    ;; it was set via customize etc.
    ))

(defun achievements-num-times-commands-were-run (command-list)
  (cond ((require 'command-frequency nil t)
         (let ((command-freq (cdr (command-frequency-list)))
               (total 0))
           (loop for com in command-freq
                 if (member (car com) command-list)
                 do (setq total (+ total (cdr com))))
           total))
        (t nil)))

(defun achievements-command-was-run (command)
  "Return non-nil if COMMAND has been run.
It can be a single command form or list of command forms.
If it's a list of forms, then all must be run.
Each form has one of the forms
 COMMAND -- must be run once
 (CMD1 CMD2 ...) -- any can be run
 (COMMAND . COUNT) -- must be run COUNT times
 ((CMD1 CMD2 ...) . COUNT) -- must be run COUNT times
symbol for a command which must be."
  (let (command-list)
    (cond
     ;; A symbol
     ((symbolp command)
      (>= (achievements-num-times-commands-were-run (list command))
          1))
     ;; cdr is a number
     ((numberp (cdr command))
      (>= (achievements-num-times-commands-were-run
           (if (listp (car command)) (car command) (list (car command))))
          (cdr command)))

     ;; A list of commands that are AND-ed
     ((or (symbolp (car-safe command))
          (numberp (cdr-safe (car-safe command))))
      (every 'achievements-command-was-run command))
     (t
      (>= (achievements-num-times-commands-were-run
           (car command))
          1)))))
;;}}}
;;{{{ Display

(defun achievements-update-score ()
  (setq achievement-score 0)
  (dolist (achievement achievements-list)
    (let ((pred (emacs-achievement-predicate achievement)))
      (when (achievements-earned-p achievement)
        (setq achievement-score (+ achievement-score
                                   (emacs-achievement-points achievement)))
        (unless (emacs-achievement-transient achievement)
          (setf (emacs-achievement-predicate achievement) t)))))
  achievement-score)

(defun achievements-earned-p (achievement)
  "Returns non-nil if the achievement is earned."
  (let ((pred (emacs-achievement-predicate achievement)))
    (or (eq pred t)
        (and (listp pred)
             (funcall pred)))))

;; TODO: Use `tabulated-list-mode' -- what package.el uses or ewoc
(defun achievements-list-achievements ()
  "Display all achievements including whether they have been achieved."
  (interactive)
  (pop-to-buffer "*Achievements*")
  (delete-region (point-min) (point-max))
  (achievements-update-score)
  (dolist (achievement achievements-list)
    (let ((pred (emacs-achievement-predicate achievement)))
      (when (>= achievement-score
                (emacs-achievement-min-score achievement))
          (insert (format "%s %20s | %s\n"
                      (cond ((eq pred nil) ":-|")
                            ((eq pred t) ":-)")
                            ((listp pred)
                             (if (funcall pred) ":-)" ":-("))
                            (t ":-?"))
                      (emacs-achievement-name achievement)
                      (emacs-achievement-description achievement)))))))

;;}}}

(require 'basic-achievements)

(provide 'achievements)

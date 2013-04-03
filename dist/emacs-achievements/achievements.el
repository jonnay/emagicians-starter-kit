;;; achievements.el --- achievements for emacs usage

;; Author: Ivan Andrus <darthandrus@gmail.com>
;; Maintainer: Ivan Andrus <darthandrus@gmail.com>
;; Created: 2012-10-07
;; Keywords: games

;;; Install:

;; A simple (require 'achievements).  However, currently it is also
;; highly recommended to install the command-frequency package in
;; order to get all the functionality.

;;; Commentary:

;; Running `achievements-list-achievements' will show a list of all
;; unlocked achievements.

;;; Code:

;; TODO: easy way to show a random unearned achievement, perhaps on an idle timer

(require 'cl)

(defconst achievements-file
  (expand-file-name ".achievements" user-emacs-directory)
  "File to store the achievements in.")

(defvar achievements-list nil
  "List of all possible achievements.")

(defvar achievement-score 0
  "Score of all earned achievements.")

(defvar achievement-total 0
  "Highest possible score of all unlocked achievements.")

(defcustom achievements-debug nil
  "If non-nil, various debug messages will be printed regarding achievements activity."
  :type 'bool
  :group 'achievements)

;;{{{ Persistence & startup

(defun achievements-save-achievements ()
  "Saves achievements to a super secret file."
  (interactive)
  (let ((print-level nil)
        (print-length nil))
	(with-temp-file achievements-file
	  (prin1 achievements-list (current-buffer)))))

(defun achievements-load-achievements ()
  "Load achievements from a super secret file.
This overwrites `achievements-list'."
  (interactive)
  (setq achievements-list
        (when (file-exists-p achievements-file)
          ;; Load sexp
          (let* ((l (condition-case nil
                        (with-temp-buffer
                          (insert-file-contents achievements-file)
                          (goto-char (point-min))
                          (read (current-buffer)))
                      ;; Catch empty file i.e., end of file during parsing
                      (error nil)))
                 (ll (and (listp l) l)))
            ;; Was it valid sexp?
            (and achievements-debug
                 (null ll)
                 (message "File %s does not contain valid data"
                          achievements-file))
            ll))))

;; Set up hooks and initialization
;;;###autoload
(defun achievements-init ()
  "Initialize achievements package."
  (when (null achievements-list)
    (achievements-load-achievements))
  (add-hook 'kill-emacs-hook #'achievements-save-achievements))

;; Set things up before we load any achievements files, otherwise the
;; definitions will populate achievements-list instead of the saved
;; values.
(achievements-init)

;;}}}
;;{{{ Defining achievements

(defstruct
    (emacs-achievement
     (:constructor nil)
     (:constructor make-achievement
                   (name description
                         ;; &optional (predicate t)
                         &key
                         ;; slots
                         points transient min-score predicate unlocks
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
  predicate ;; t if satisfied, nil if opted out, otherwise a function which should return non-nil on success
  transient ;; if non-nil then results won't be saved, but constantly re-evaluated.
  (points 5)
  (min-score 0)
  unlocks
  )

(defmacro defachievement (name &rest body)
  `(add-to-list 'achievements-list
                ,(if (stringp (car-safe body))
                     `(make-achievement ,name ,@body)
                   `(make-achievement ,name nil ,@body)
                   )
                t
                ;; We compare by name only, since the predicate will often be different
                (lambda (a b)
                  (equal (emacs-achievement-name a)
                         (emacs-achievement-name b)))))

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
  "If VAR is a cons, return non-nil if (car VAR) is equal to (cdr VAR).
If VAR is a symbol, return non-nil if VAR has been set in
customize or .emacs (not yet implemented)."
  (if (listp var)
      (equal (symbol-value (car var)) (cadr var))
    ;; it was set via customize etc.
    ))

(defun achievements-num-times-commands-were-run (command-list)
  "Return the number of times any one of the commands was run.
Right now this is checked it `command-frequency', but it is hoped
that in the future there will be other methods."
  (cond ((require 'command-frequency nil t)
         (let ((command-freq (cdr (command-frequency-list)))
               (total 0))
           (loop for com in command-freq
                 if (member (car com) command-list)
                 do (setq total (+ total (cdr com))))
           total))
        (t 0)))

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
     ;; Otherwise it's a list of commands, any of which could be run
     (t
      (>= (achievements-num-times-commands-were-run
           (car command))
          1)))))

;;}}}
;;{{{ Display

(defun achievements-update-score ()
  (let ((score 0)
        (total 0))
    (dolist (achievement achievements-list)
      (let ((points (emacs-achievement-points achievement)))
        (incf total points)
        (when (achievements-earned-p achievement)
          (incf score points)
          (when (emacs-achievement-unlocks achievement)
            (require (emacs-achievement-unlocks achievement) nil t))
          (unless (emacs-achievement-transient achievement)
            (setf (emacs-achievement-predicate achievement) t)))))
    ;; Save the updated list of achievements
    (achievements-save-achievements)
    (setq achievement-total total)
    (setq achievement-score score)))

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

;;; achievements.el ends here

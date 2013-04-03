;;; basic-achievements.el --- Some basic achievements

;; Author: Ivan Andrus <darthandrus@gmail.com>
;; Maintainer: Ivan Andrus <darthandrus@gmail.com>
;; Created: 2012-10-07
;; Keywords: games

;;; Commentary:

;; This is the main entry point for achievement defintions.  Other
;; files can be included from here using the `:unlocks' slot.

;;; Code:

;;{{{ meta

(defachievement "Achiever"
  "You used the achievements package."
  :predicate t)

(defachievement "Not All There"
  "You have a fractional achievement score."
  ;; Note: This is meant to be the only one that has fractional score
  :points .5
  :predicate '(/= achievement-score (round achievement-score)))

(defachievement "Unlocker"
  "You have earned over 50 points in Emacs achievements.  Not bad."
  :predicate '(>= achievement-score 50)
  :unlocks 'advanced-achievements)

(defachievement "Over Achiever"
  "You have earned 500 points in Emacs achievements.  Don't you have some real work to do?"
  :predicate '(>= achievement-score 500))

(defachievement "Cheater"
  "You have earned all Emacs achievements.  Actually that's impossible."
  :predicate '(every #'achievements-earned-p achievements-list))

;;}}}
;;{{{ Help

(defachievement "Free Software Zealot"
  "You've read the sales pitch."
  :command '(about-emacs
             describe-copying
             describe-distribution
             describe-gnu-project
             describe-no-warranty))

(defcommand-achievements
  "You learned new things by using `%s'."
  ((help-for-help "First things first.")
   (help-with-tutorial "Show me the way.") ;; Also one if finished the tutorial
   (info-emacs-manual "RTFM")
   (view-echo-area-messages "Log Auditor")
   (view-emacs-FAQ "FAQ")
   (view-emacs-news "What's new?")
   (view-emacs-problems "Am I the only one?")
   (view-emacs-debugging "Entomologist")
   (view-emacs-todo "Joining the cause")
   (view-external-packages "Where else can I look?")
   ((view-hello-file
     describe-language-environment
     describe-input-method
     describe-coding-system) "World Traveler")
   ((finder-by-keyword describe-package) "Package Hunter")))

(defcommand-achievements
  "You answered a question by using `%s'."
  ((apropos-documentation "I know I read it somewhere")
   (apropos "Apropos of Nothing")
   (apropos-value "Answer in search of a question")
   (describe-bindings "What to type?")
   ((describe-function
     Info-goto-emacs-command-node
     info-lookup-symbol
     describe-variable
     describe-mode) "What does this do?")
   ((describe-key
     describe-key-briefly
     Info-goto-emacs-key-command-node
     where-is) "What happens when I do this?")
   (describe-syntax "When is a word not a word?")
   ((command-history view-lossage) "What did I just do?")))

(defachievement "Shortcut genius"
  "You don't need to learn new shortcuts anymore."
  :variable '(suggest-key-bindings nil))

;; (display-local-help "")              ;; What is this?
;; (defachievement ""
;;   "You have read at least 10 pages of the read the manual.")
;; (defachievement "Book worm"
;;   "You have read at least 100 pages of the read the manual.")
;; eldoc

;;}}}
;;{{{ .emacs

(defachievement "Streamlined"
  "Your .emacs took less that 1 second to load."
  :predicate '(< (float-time (time-subtract after-init-time before-init-time)) 1)
  :transient t)

(defachievement "Oops"
  "Your .emacs file had an error."
  :predicate 'init-file-had-error)

(defachievement "Traditionalist"
  "You use .emacs instead of .emacs.d/init.el."
  :predicate '(string-match "/\\.emacs\\'" user-init-file))

(defachievement "Modernist"
  "You use .emacs.d/init.el instead of .emacs."
  :predicate '(string-match "/init\\.el\\'" user-init-file))

(defachievement "Post Modernist"
  "You don't use .emacs.d/init.el or .emacs."
  :predicate '(not (or (string-match "/init\\.el\\'" user-init-file)
                       (string-match "/\\.emacs\\'" user-init-file))))

(defachievement "Need for Speed"
  "Your .emacs is byte-compiled."
  :predicate '(or (file-exists-p (concat user-init-file "c"))
                  ;; TODO: I _think_ this is right for .emacs
                  (file-exists-p (concat user-init-file ".elc"))))

(defachievement "Last Year's Model"
  "Your byte-compiled .emacs is out of date."
  :predicate '(cond ((file-exists-p (concat user-init-file "c"))
                     (file-newer-than-file-p
                      user-init-file
                      (concat user-init-file "c")))
                    ((file-exists-p (concat user-init-file ".elc"))
                     (file-newer-than-file-p
                      user-init-file
                      (concat user-init-file ".elc")))))

(defachievement "Purest Vanilla"
  "You have no .emacs file.  How is that even possible?"
  :predicate '(not (file-exists-p user-init-file)))

;; Other sizes here
;; (defachievement "Jabba the Hutt"
;;   "Your .emacs is so big, it runs a crime syndicate on Tatooine."
;;   )

;;}}}
;;{{{ Fun and Games

(defcommand-achievements
  "You have enjoyed `%s'."
  ((5x5 "Twenty Five")
   (animate "The Future of Pixar")
   (artist-mode "Van Gogh")
   (blackbox "Hide and Seek")
   (bubbles "Blubb blubb")
   (butterfly "Change the world!")
   (decipher "Spy vs Spy")
   (dissociated-press "Tabloids")
   (doctor "I <3 Eliza")
   (dunnet "Adventure!")
   (gomoku "Connect 5")
   (handwrite "Penmanship")
   (hanoi "Saigon")
   ;; (landmark "") ;; I have no idea what this is
   (life "It's Alive!")
   (morse-region "Telegraph Operator")
   (mpuz "Arithmetician")
   (pong "Ping")
   (snake "Chase your tail")
   (solitaire "All alone?")
   (tetris "Tessellator")
   (yow "Yow!")
   (zone "The Matrix"))
  :min-score 50)

;; These probably need packages.  Also check out
;; http://www.emacswiki.org/emacs/CategoryGames
(defcommand-achievements
  "You have installed and enjoyed `%s'."
  ((emstar "Um, Star?" )
   (fliptext-flip-region "Can you read upside down?")
   (nethack "Hack, hack, hack.")
   (nyan-mode "Nyan, Nyan, Nyan"))
  :min-score 100)

;;}}}
;;{{{ Uptimes

(defachievement "Pretty Stable"
  "You have an uptime of over 1 day."
  :package 'uptimes
  :predicate `(> (let* ((uptime (car uptimes-top-n))
                        (seconds (- (cddr uptime) (cadr uptime))))
                   seconds)
                 ,(* 60 60 24 1)))

(defachievement "It keeps going and going..."
  "You have an uptime of over 1 week."
  :package 'uptimes
  :predicate `(> (let* ((uptime (car uptimes-top-n))
                        (seconds (- (cddr uptime) (cadr uptime))))
                   seconds)
                 ,(* 60 60 24 7)))

(defachievement "Marathon Hacker"
  "You have an uptime of over 30 days."
  :package 'uptimes
  :predicate `(> (let* ((uptime (car uptimes-top-n))
                        (seconds (- (cddr uptime) (cadr uptime))))
                   seconds)
                 ,(* 60 60 24 30)))

(defachievement "Methuselah"
  "You have an uptime of over 1 year!?"
  :package 'uptimes
  :predicate `(> (let* ((uptime (car uptimes-top-n))
                        (seconds (- (cddr uptime) (cadr uptime))))
                   seconds)
                 ,(* 60 60 24 365)))

;;}}}
;;{{{ Number of characters written

(defachievement "Short Story"
  "You've written the equivalent of a short story."
  :predicate `(> (achievements-num-times-commands-were-run
                 '(self-insert-command
                   org-self-insert-command))
                 ,(* 2000 6)))        ;; ~ 5/word + space

(defachievement "Nanowrimo"
  "You could have finished Nanowrimo by now."
  :predicate `(> (achievements-num-times-commands-were-run
                 '(self-insert-command
                   org-self-insert-command))
                 ,(* 50000 6)))

(defachievement "War and Peace"
  "You've written the equivalent of War and Peace."
  :predicate `(> (achievements-num-times-commands-were-run
                 '(self-insert-command
                   org-self-insert-command))
                 ,(* 587287 6)))

(defachievement "Proust"
  "You could have beaten Proust for longest novel."
  :predicate `(> (achievements-num-times-commands-were-run
                 '(self-insert-command
                   org-self-insert-command))
                 ,(* 1200000 6)))

;;}}}
;;{{{ Operating system

(defachievement "Loyalist"
  "You use GNU Emacs"
  :predicate '(not (string-match "XEmacs\\|Lucid" emacs-version)))

(defachievement "Patriot or Rebel?"
  "You use XEmacs"
  :predicate '(string-match "XEmacs\\|Lucid" emacs-version))

;; (defachievement "Switch Hitter"
;;   "You use both GNU Emacs and XEmacs")

;; What version of emacs you are using

(defvalue-achievements window-system
  "You have used the %s version Emacs."
  (("Green Glowing faces" nil "console")
   ("X marks the spot" x)
   ("MacPort or Aquamacs" mac)
   ("GNUStep or Cocoa" ns "nextstep")
   ("Windows" w32 "windows")
   ("DOS?" pc "DOS")))

(defvalue-achievements system-type
  "You have used Emacs on a %s system."
  (("Following the Hurd" gnu)
   ("Tux's Friend" gnu/linux)
   ("Beastie's Pal" gnu/kfreebsd)
   ("Friends with Hexley" darwin)
   ("DOS Box" ms-dos)
   ("Windows Machine" windows-nt)
   ("The Swan" cygwin)))

;;}}}
;;{{{ Identity

(defachievement "Anonymous"
  "You have `user-mail-address' set to nil."
  :predicate '(equal user-mail-address nil))

(defachievement "The One and Only"
  "You are Richard Stallman."
  :predicate '(equal user-mail-address "rms@gnu.org")
  :points 50)

;; TODO: need list of old maintainers, as well as for XEmacs
(defachievement "A Well Oiled Machine"
  "You help maintain Emacs."
  :predicate '(member user-mail-address
                      '("rms@gnu.org"
                        "cyd@gnu.org"
                        "monnier@iro.umontreal.ca"))
  :points 50)

;;}}}
;;{{{ packages

(defachievement "Tainted Love"
  "You have enabled non-GNU package repositories."
  :package 'package
  :predicate '(some (lambda (repo)
                      (not (string-match "elpa\\.gnu\\.org" (cdr repo))))
                    package-archives))

(defachievement "Vanilla"
  "You have no installed packages."
  :predicate '(= (length package-alist) 0))

(defachievement "Package Neophyte"
  "You have installed at least 1 package."
  :predicate '(>= (length package-alist) 1))

(defachievement "Package Apprentice"
  "You have installed over 10 packages."
  :predicate '(>= (length package-alist) 10))

(defachievement "Package Journeyman"
  "You have installed over 100 packages."
  :predicate '(>= (length package-alist) 100))

;; (defachievement "Drew Adams fanboy"
;;   "You are using 3 or more packages by Drew Adams.")

;; More package creators ...

;;}}}
;;{{{ Buffers & processess

(defachievement "Clean Desk"
  "You have less than 10 buffers open."
  :predicate '(<= (length (buffer-list)) 10))

(defachievement "Messy Desk"
  "You have over 100 buffers open."
  :predicate '(>= (length (buffer-list)) 100))

(defachievement "Papers to the ceiling"
  "You have over 1000 buffers open."
  :predicate '(>= (length (buffer-list)) 1000))

(defachievement "The Ol' Switcheroo"
  "You've switched to another buffer"
  :command '(switch-to-buffer ido-switch-buffer))

(defachievement "Buffer, buffers, everywhere"
  "You've seen all the buffers that can be seen."
  :command '(list-buffers ibuffer))

(defachievement "Top o' the morning"
  "You've used Emacs as a replacement for top."
  :command 'proced)

;;}}}

(provide 'basic-achievements)

;;; basic-achievements.el ends here

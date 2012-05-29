;; kitty-bliss.el --- a pink color theme for Emacs 24

;; Copyright (C) 2012 Jonathan Arkell
;; Copyright (C) 2005, 2008  Alex Schroeder <alex@gnu.org>

;; This file is not part of GNU Emacs. 

;; This is free software; you can redistribute it and/or modify it under
;; the terms of the GNU General Public License as published by the Free
;; Software Foundation; either version 2, or (at your option) any later
;; version.
;;
;; This is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
;; FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
;; for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330, Boston,
;; MA 02111-1307, USA.

;; Contains a bunch of stuff from PinBliss:

;; URL: http://www.emacswiki.org/cgi-bin/emacs/PinkBliss
;; pink-gnu.xpm: http://www.emacswiki.org/cgi-bin/emacs/download/pink-gnu.xpm

;;; Code:

(deftheme kitty-bliss-theme
  "Based off of pink-bliss, but emacs-24'ed and modified."

  (custom-theme-set-faces
   'kitty-bliss

   '((default ((t ( :background "misty rose" :foreground "magenta4"))))
     (button ((t (:bold t))))
     (fringe ((t (:background "hot pink"))))
     (menu ((t (:background "pink" :foreground "violet red"))))
     (modeline ((t (:background "pink" :foreground "purple"
				:box (:line-width 1 :style released-button)))))
     (mode-line-inactive ((t (:background "pink" :foreground "orchid"
					  :box (:line-width 1
							    :style released-button)))))
     (minibuffer-prompt ((t (:foreground "deep pink"))))
     (tool-bar ((t (:background "pink"
				:box (:line-width 1 :style released-button)))))
     (tooltip ((t (:background "lemon chiffon"
			       :foreground "violet red"))))
     (region ((t (:background "white"))))
     ;; isearch
     (isearch ((t (:foreground "pink" :background "red"))))
     (isearch-lazy-highlight-face ((t (:foreground "red"))))
     ;; info-mode
     (header-line ((t (:background "deep pink" :foreground "pink"))))
     ;; calendar
     (calendar-today-face ((t (:foreground "lemon chiffon"))))
     (diary-face ((t (:bold t :foreground "yellow"))))
     (holiday-face ((t (:bold t :foreground "peru"))))
     ;; font-lock
     (font-lock-builtin-face ((t (:foreground "orchid"))))
     (font-lock-comment-delimiter-face ((t (:foreground "coral"))))
     (font-lock-comment-face ((t (:foreground "salmon"))))
     (font-lock-constant-face ((t (:foreground "orchid"))))
     (font-lock-doc-face ((t (:foreground "coral"))))
     (font-lock-function-name-face ((t (:foreground "deep pink"))))
     (font-lock-keyword-face ((t (:foreground "purple"))))
     (font-lock-negation-char-face ((t (:foreground "red"))))
     (font-lock-preprocessor-face ((t (:foreground "pink"))))
     (font-lock-string-face ((t (:foreground "pale violet red"))))
     (font-lock-type-face ((t (:foreground "light slate blue"))))
     (font-lock-variable-name-face ((t (:foreground "hot pink"))))
     (font-lock-warning-face ((t (:bold t :foreground "red"))))
     ;; cperl
     (cperl-array-face ((t (:bold t :foreground "tomato"))))
     (cperl-hash-face  ((t (:bold t :foreground "chocolate"))))
     (cperl-nonoverridable-face  ((t (:foreground "red"))))
     ;; makefiles
     (makefile-shell-face  ((t (:background "linen")))))))

(defvar kitty-bliss-foreground-colors
  (let ((candidates)
	;; (red-limit #xe000)
	(green-limit #xed00)
	(both-limit #xa000))
    (dolist (item color-name-rgb-alist)
      (destructuring-bind (color red green blue) item
	(when (and (not (color-gray-p color))
		   ;; (< red red-limit)
		   (< green green-limit)
		   (not (and (> red both-limit)
			     (> green both-limit))))
	  (setq candidates (cons color candidates)))))
    candidates)
  "Colors to use for nicks in rcirc, for example.

To check out the list, evaluate
\(list-colors-display pink-bliss-foreground-colors).")

(provide-theme 'kitty-bliss)

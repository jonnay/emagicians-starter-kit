;;; org-beautify.el --- A sub-theme to make org-mode more beautiful.
;; Copyright (C) 2014 Jonathan Arkell

;; Author: Jonathan Arkell <jonnay@jonnay.net>
;; package-Version: 0.1
;; Created: 5 Oct 2012
;; Keywords: org theme

;; This file is not part of GNU Emacs.
;; Released under the GPL v3.0

;;; Commentary:
;; An attempt to improve the typography of an org-mode file.

;; Load this theme over top your existing theme, and you should
;; be golden.  If you find any incompatibilities, let me know
;; with what theme and I will try and fix it.

;; This is part of the Emagicians Starter kit--but available
;; separately.

;; When loading a whole new theme overtop, org-beautify will 
;; still be active with the old theme.  Just unload org-beautify
;; and then reload it, and everything will be fine again. 

;; The Source for this file is here:
;; https://github.com/jonnay/emagicians-starter-kit/blob/master/themes/org-beautify.org

;;; Code: 

(deftheme org-beautify "Sub-theme to beautify org mode")

(let* ((sans-font (cond ((x-list-fonts "Lucida Grande") '(:font "Lucida Grande"))
                        ((x-family-fonts "Sans Serif") '(:family "Sans Serif"))
                        (nil (warn "Cannot find a Sans Serif Font.  Please report at: https://github.com/jonnay/emagicians-starter-kit/issues"))))
       (base-font-color (face-foreground 'default  nil 'default))
       (background-color (face-background 'default nil 'default))
       (headline `(:inherit default :foreground ,base-font-color))
       (primary-color (face-foreground 'mode-line nil))
       (secondary-color (face-background 'secondary-selection nil 'region))
       (padding `(:line-width 5 :color ,background-color))
       (org-highlights `(:foreground ,base-font-color :background ,secondary-color)))
  (custom-theme-set-faces 'org-beautify
                          `(org-agenda-structure ((t (:inherit default ,@sans-font :height 2.0 :underline nil))))
                          `(org-level-8 ((t ,headline)))
                          `(org-level-7 ((t ,headline)))
                          `(org-level-6 ((t ,headline)))
                          `(org-level-5 ((t ,headline)))
                          `(org-level-4 ((t ,headline)))
                          `(org-level-3 ((t (,@headline  :box ,padding))))
                          `(org-level-2 ((t (,@headline ,@sans-font :height 1.25 :box ,padding))))
                          `(org-level-1 ((t (,@headline ,@sans-font :height 1.5 :box ,padding ))))
                          `(org-document-title ((t (:inherit org-level-1 :height 2.0 :underline nil :box ,padding))))

                          `(org-block ((t (:foreground ,base-font-color :background ,background-color :box nil))))
                          `(org-block-begin-line ((t ,org-highlights)))
                          `(org-block-end-line ((t ,org-highlights))) 

                          `(org-checkbox ((t (:foreground ,background-color :background "#93a1a1" :box (:line-width -3 :color "#93a1a1" :style "released-button")))))

                          `(org-headline-done ((t (:foreground "586e75" :strike-through t))))
                          `(org-done ((t (:foreground "586e75" :strike-through t))))))

(provide-theme 'org-beautify)

;;; org-beautify.el ends here

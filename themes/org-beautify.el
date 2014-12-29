
(deftheme org-beautify "Beautify org mode")

(let* ((base-font-color (face-foreground 'default  nil 'default))
       (background-color (face-background 'default nil 'default))
       (headline `(:inherit default :foreground ,base-font-color))
       (primary-color (face-foreground 'mode-line nil))
       (secondary-color (face-background 'secondary-selection nil 'region))
       (padding `(:line-width 5 :color ,background-color))
       (org-highlights `(:foreground ,base-font-color :background ,secondary-color)))
  (custom-theme-set-faces 'org-beautify
                          `(org-agenda-structure ((t (:inherit default :font "Lucida Grande" :height 2.0 :underline nil))))
                          `(org-level-8 ((t ,headline)))
                          `(org-level-7 ((t ,headline)))
                          `(org-level-6 ((t ,headline)))
                          `(org-level-5 ((t ,headline)))
                          `(org-level-4 ((t ,headline)))
                          `(org-level-3 ((t (,@headline  :box ,padding))))
                          `(org-level-2 ((t (,@headline :font "Lucida Grande" :height 1.25 :box ,padding))))
                          `(org-level-1 ((t (,@headline :font "Lucida Grande" :height 1.5 :box ,padding))))
                          `(org-document-title ((t (:inherit org-level-1 :height 2.0 :underline nil :box ,padding))))

                          `(org-block ((t (:foreground ,base-font-color :background ,background-color :box nil))))
                          `(org-block-begin-line ((t ,org-highlights)))
                          `(org-block-end-line ((t ,org-highlights))) 

                          `(org-checkbox ((t (:foreground ,background-color :background "#93a1a1" :box (:line-width -3 :color "#93a1a1" :style "released-button")))))

                          `(org-headline-done ((t (:foreground "586e75" :strike-through t))))
                          `(org-done ((t (:foreground "586e75" :strike-through t)))))
                            
    )

(provide-theme 'org-beautify)

;;; emagician-fix-spelling. --- Simple hack into ispell to fix (muscle) memory problems
 
;; Copyright (C) 2012 Jonathan Arkell

;; Author: Jonathan Arkell <jonnay@jonnay.net>
;; Created: 5 Oct 2012
;; Keywords: erc bitlbee bot
;; Version 0.1

;; This file is not part of GNU Emacs.
;; Released under the GPL v3.0

;;; Commentary:
;;   
;;   This file is part of the emagicians starter kit, which is available
;;   here: https://github.com/jonnay/emagicians-starter-kit
;;   
;;   Note that if you are using this .el file, you probably aren't using
;;   the emagicians sarter kit, which is fine. You dont need this for
;;   that.
;; 
;; * Motivation
;; 
;;   I used to type 'necessary' wrong... ALL THE TIME. I misspelled it so
;;   often that it became part of my muscle memory.  It is one of *THOSE*
;;   words for me.  There are others, that by muscle, or brain memory,
;;   are "burned in" as a particular pattern.
;;  
;;   This is an attempt to break that pattern, by forcing you to re-type
;;   your misspelled words 3 times.  This should help overcome any broken
;;   muscle and brain memory.
;; 
;; * Usage
;; 
;;   - Step 1 :: Require this file
;;   - Step 2 :: Use M-$ to check the spelling of your misspelled word
;;   - Step 3 :: follow the directions of the prompt
;; 
;; 
;; 

;;; Code:

(defadvice ispell-command-loop (after emagician/fix-muscle-memory last activate)
  "Force the user to type in the misspelled/mis-typoed word 5 times, to burn it into muscle memory."
  (let ((times 0)
        (total-times 3))
    (while (< times total-times)
      (setq times
            (+ times (if (string= (read-string (format "Re-type \"%s\" correctly (%d/%d): "  ad-return-value times total-times))
                              ad-return-value)
                     1
                   -1))))))

(provide 'emagician-fix-spell-memory)

;;; emagician-fix-spell-memory ends here

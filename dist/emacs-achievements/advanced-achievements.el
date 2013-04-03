;;; advanced-achievements.el --- More advanced achievements

;; Copyright (C) 2012  Ivan Andrus

;; Author: Ivan Andrus <darthandrus@gmail.com>
;; Keywords: games

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; These are meant to be slightly more advanced achievements than
;; basic-achievements.el and is unlocked with one of the achievements
;; there.  Nevertheless, what counts as "advanced" is quite arbitrary.

;;; Code:

;;{{{ Disabled functions

(defachievement "Inception"
  "You have used recursive editing and exited succesfully."
  :command '(recursive-edit exit-recursive-edit))

(defachievement "Narrow minded"
  "You have used narrowing."
  :command '(narrow-to-region narrow-to-page))

(defachievement "Forbidden Fruits"
  "You have used all disabled commands."
  :command (loop for s being the symbols
                 when (get s 'disabled) collect s))

(defachievement "Enabler"
  "You have enabled all commands."
  :predicate '(= 0 (length (loop for s being the symbols
                                 when (get s 'disabled) collect s))))

(defachievement "Case Changer"
  "You have changed the case of a few words."
  :command '(upcase-word downcase-word capitalize-word))

(defachievement "CASE CHANGER"
  "You have changed the case of vast amounts of text."
  :command '(upcase-region downcase-region))

(defachievement "The Great Destroyer"
  "You have laid waste to an entire buffer in one go."
  :command 'erase-buffer)

(defachievement "Goal Setter"
  "You have set the goal column."
  :command 'set-goal-column)

(defachievement "Wide Load"
  "You have scrolled to see an extra wide buffer."
  :command 'scroll-left)

(defachievement "Dired reuse"
  "You have reused a dired buffer to look at another file/directory."
  :command 'dired-find-alternate-file)

;;}}}

(provide 'advanced-achievements)

;;; advanced-achievements.el ends here

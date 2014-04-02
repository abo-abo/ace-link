;;; ace-link.el --- Ace jump to visible links

;; Copyright (C) 2014 Oleh Krehel

;; Author: Oleh Krehel <ohwoeowho@gmail.com>
;; URL: https://github.com/abo-abo/ace-link
;; Version: 0.1.0
;; Package-Requires: ((ace-jump-mode "2.0") (noflet "0.0.10"))
;; Keywords: convenience, links

;; This file is not part of GNU Emacs

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; For a full copy of the GNU General Public License
;; see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; This package offers an alternative to tabbing through links in
;; buffers, for instance, in an Info buffer.  `ace-jump-mode' is used
;; to turn opening a link from an O(N) operation into an O(1).

;;; Code:

(require 'noflet)
(require 'ace-jump-mode)

;; ——— Interactive —————————————————————————————————————————————————————————————
;;;###autoload
(defun ace-link-info ()
  "Ace jump to links in `Info-mode' buffers."
  (interactive)
  (noflet ((ace-jump-search-candidate (str va-list)
             (mapcar (lambda (x)
                       (make-aj-position
                        :offset (1- x)
                        :visual-area (car va-list)))
                     (ali--info-collect-references))))
    (setq ace-jump-mode-end-hook
      (list `(lambda ()
               (setq ace-jump-mode-end-hook)
               (Info-follow-nearest-node))))
    (ace-jump-do "")))

;;;###autoload
(defun ace-link-help ()
  "Ace jump to links in `help-mode' buffers."
  (interactive)
  (noflet ((ace-jump-search-candidate (str va-list)
             (mapcar (lambda (x)
                       (make-aj-position
                        :offset (1- x)
                        :visual-area (car va-list)))
                     (ali--help-collect-references))))
    (setq ace-jump-mode-end-hook
      (list `(lambda ()
               (setq ace-jump-mode-end-hook)
               (forward-char 1)
               (push-button))))
    (ace-jump-do "")))

;; ——— Utility —————————————————————————————————————————————————————————————————
(defun ali--info-collect-references ()
  "Collect the positions of visible links in the current `Info-mode' buffer."
  (let ((end (window-end))
        points)
    (save-excursion
      (goto-char (window-start))
      (when (ignore-errors (Info-next-reference) t)
        (push (point) points)
        (Info-next-reference)
        (while (and (< (point) end)
                    (> (point) (car points)))
          (push (point) points)
          (Info-next-reference))
        (nreverse points)))))

(defun ali--help-collect-references ()
  "Collect the positions of visible links in the current `help-mode' buffer."
  (let ((skip (text-property-any (point-min) (point-max)
                                 'button nil))
        candidates)
    (save-excursion
      (while (setq skip (text-property-not-all skip (point-max)
                                               'button nil))
        (goto-char skip)
        (push skip candidates)
        (setq skip (text-property-any (point) (point-max)
                                      'button nil))))
    (nreverse candidates)))

;;;###autoload
(defun ace-link-setup-default ()
  "Setup the defualt shortcuts."
  (require 'info)
  (define-key Info-mode-map "o" 'ace-link-info)
  (require 'help-mode)
  (define-key help-mode-map "o" 'ace-link-help))

(provide 'ace-link)

;;; ace-link.el ends here

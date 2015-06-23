;;; ace-link.el --- Quickly follow links

;; Copyright (C) 2014-2015 Oleh Krehel

;; Author: Oleh Krehel <ohwoeowho@gmail.com>
;; URL: https://github.com/abo-abo/ace-link
;; Version: 0.3.0
;; Package-Requires: ((avy "0.2.0"))
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
;; buffers, for instance, in an Info buffer.  `avy' is used to turn
;; opening a link from an O(N) operation into an O(1).
;;
;; Use `ace-link-setup-default' to set up the default bindings, which currently
;; bind e.g. `ace-link-info' to "o", which was previously unbound and is
;; close to "l" (which by default goes back).
;;
;; Supported modes: `Info-mode', `help-mode', `org-mode', `eww-mode',
;; `gnus-article-mode', `Custom-mode'.

;;; Code:
(require 'avy)

;;* Commands
;;** Info
(declare-function Info-follow-nearest-node "info")

;;;###autoload
(defun ace-link-info ()
  "Open a visible link in an `Info-mode' buffer."
  (interactive)
  (let ((res (avy--with-avy-keys ace-link-info
               (avy--process
                (ali--info-collect-references)
                #'avy--overlay-post))))
    (when res
      (push-mark)
      (goto-char res)
      (let ((we (window-end)))
        (while (not (ignore-errors
                      (Info-follow-nearest-node)))
          (forward-char 1)
          (when (> (point) we)
            (error "Could not follow link")))))))

;;** Help
;;;###autoload
(defun ace-link-help ()
  "Open a visible link in a `help-mode' buffer."
  (interactive)
  (let ((res (avy--with-avy-keys ace-link-help
               (avy--process
                (ali--help-collect-references)
                #'avy--overlay-post))))
    (when res
      (goto-char (1+ res))
      (push-button))))

;;** EWW
(declare-function eww-follow-link "eww")

;;;###autoload
(defun ace-link-eww ()
  "Open a visible link in an `eww-mode' buffer."
  (interactive)
  (let ((res (avy--with-avy-keys ace-link-eww
               (avy--process
                (ali--eww-collect-references)
                #'avy--overlay-post))))
    (when res
      (goto-char (1+ res))
      (eww-follow-link))))

;;;###autoload
(defun ace-link-compilation ()
  "Open a visible link in a `compilation-mode' buffer."
  (interactive)
  (let ((res (avy--with-avy-keys ace-link-compilation
               (avy--process
                (ali--eww-collect-references)
                #'avy--overlay-post))))
    (when res
      (goto-char (1+ res))
      (compile-goto-error))))

;;** GNUS
(declare-function gnus-summary-widget-forward "gnus-sum")
(declare-function widget-button-press "wid-edit")

;;;###autoload
(defun ace-link-gnus ()
  "Open a visible link in a `gnus-article-mode' buffer."
  (interactive)
  (when (eq major-mode 'gnus-summary-mode)
    (gnus-summary-widget-forward 1))
  (let ((res (avy--with-avy-keys ace-link-gnus
               (avy--process
                (ali--gnus-collect-references)
                #'avy--overlay-post))))
    (when res
      (goto-char (1+ res))
      (widget-button-press (point)))))

;;** Org
(declare-function org-open-at-point "org")

;;;###autoload
(defun ace-link-org ()
  "Open a visible link in an `org-mode' buffer."
  (interactive)
  (require 'org)
  (let ((res (avy--with-avy-keys ace-link-org
               (avy--process
                (ali--org-collect-references)
                #'avy--overlay-pre))))
    (when res
      (goto-char res)
      (org-open-at-point))))

;;;###autoload
(defun ace-link-custom ()
  "Open a visible link in an `Custom-mode' buffer."
  (interactive)
  (let ((res (avy--with-avy-keys ace-link-custom
               (avy--process
                (ali--custom-collect-references)
                #'avy--overlay-pre))))
    (when res
      (goto-char res)
      (Custom-newline (point)))))

;;* Internals
(declare-function widget-forward "wid-edit")
(defun ali--gnus-collect-references ()
  "Collect the positions of visible links in the current gnus buffer."
  (require 'wid-edit)
  (let (candidates pt)
    (save-excursion
      (save-restriction
        (narrow-to-region
         (window-start)
         (window-end))
        (goto-char (point-min))
        (setq pt (point))
        (while (progn (widget-forward 1)
                      (> (point) pt))
          (setq pt (point))
          (when (plist-get (text-properties-at (point)) 'gnus-string)
            (push (point) candidates)))
        (nreverse candidates)))))

(defun ali--custom-collect-references ()
  "Collect the positions of visible links in the current `Custom-mode' buffer."
  (let (candidates pt)
    (save-excursion
      (save-restriction
        (narrow-to-region
         (window-start)
         (window-end))
        (goto-char (point-min))
        (setq pt (point))
        (while (progn (widget-forward 1)
                      (> (point) pt))
          (setq pt (point))
          (when (get-char-property (point) 'button)
            (push (point) candidates)))))
    (nreverse candidates)))

(declare-function Info-next-reference "info")
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

(defun ali--eww-collect-references ()
  "Collect the positions of visible links in the current `eww' buffer."
  (save-excursion
    (save-restriction
      (narrow-to-region
       (window-start)
       (window-end))
      (goto-char (point-min))
      (let ((skip (text-property-any (point) (point-max)
                                     'help-echo nil))
            candidates)
        (while (setq skip (text-property-not-all
                           skip (point-max) 'help-echo nil))
          (goto-char skip)
          (push skip candidates)
          (setq skip (text-property-any (point) (point-max)
                                        'help-echo nil)))
        (nreverse candidates)))))

(declare-function outline-invisible-p "outline")
(defvar org-any-link-re)
(defun ali--org-collect-references ()
  (let ((end (window-end))
        points)
    (save-excursion
      (goto-char (window-start))
      (while (re-search-forward org-any-link-re end t)
        ;; Check that the link is visible. Look at the last character
        ;; position in the link ("...X]]") to cover links with and
        ;; without a description.
        (when (not (outline-invisible-p (- (match-end 0) 3)))
          (push (match-beginning 0) points)))
      (nreverse points))))

;;* Bindings
(defvar eww-link-keymap)
(defvar eww-mode-map)
(defvar custom-mode-map)

;;;###autoload
(defun ace-link-setup-default (&optional key)
  "Bind KEY to appropriate functions in appropriate keymaps."
  (setq key (or key "o"))
  (eval-after-load "info"
    `(define-key Info-mode-map ,key 'ace-link-info))
  (eval-after-load "compile"
    `(define-key compilation-mode-map ,key 'ace-link-compilation))
  (eval-after-load "help-mode"
    `(define-key help-mode-map ,key 'ace-link-help))
  (eval-after-load "eww"
    `(progn
       (define-key eww-link-keymap ,key 'ace-link-eww)
       (define-key eww-mode-map ,key 'ace-link-eww)))
  (eval-after-load 'cus-edit
    `(progn
       (define-key custom-mode-map ,key 'ace-link-custom))))

(provide 'ace-link)

;;; ace-link.el ends here

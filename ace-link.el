;;; ace-link.el --- Quickly follow links -*- lexical-binding: t -*-

;; Copyright (C) 2014-2020 Oleh Krehel

;; Author: Oleh Krehel <ohwoeowho@gmail.com>
;; URL: https://github.com/abo-abo/ace-link
;; Version: 0.5.0
;; Package-Requires: ((avy "0.4.0"))
;; Keywords: convenience, links, avy

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
;; `gnus-article-mode', `Custom-mode', `woman-mode', `goto-address-mode'.

;;; Code:
(require 'avy)

;;* `ace-link'
(defvar ace-link-fallback-function nil
  "When non-nil, called by `ace-link' when `major-mode' isn't recognized.")

;;;###autoload
(defun ace-link ()
  "Call the ace link function for the current `major-mode'"
  (interactive)
  (cond ((eq major-mode 'Info-mode)
         (ace-link-info))
        ((member major-mode '(help-mode
                              package-menu-mode geiser-doc-mode elbank-report-mode
                              elbank-overview-mode slime-trace-dialog-mode helpful-mode))
         (ace-link-help))
        ((eq major-mode 'Man-mode)
         (ace-link-man))
        ((eq major-mode 'woman-mode)
         (ace-link-woman))
        ((eq major-mode 'eww-mode)
         (ace-link-eww))
        ((eq major-mode 'w3m-mode)
         (ace-link-w3m))
        ((or (member major-mode '(compilation-mode grep-mode))
             (bound-and-true-p compilation-shell-minor-mode))
         (ace-link-compilation))
        ((memq major-mode '(gnus-article-mode gnus-summary-mode))
         (ace-link-gnus))
        ((eq major-mode 'mu4e-view-mode)
         (ace-link-mu4e))
        ((eq major-mode 'notmuch-show-mode)
         (ace-link-notmuch))
        ((memq major-mode '(org-mode
                            erc-mode elfeed-show-mode
                            term-mode vterm-mode
                            eshell-mode
                            telega-chat-mode))
         (ace-link-org))
        ((eq major-mode 'org-agenda-mode)
         (ace-link-org-agenda))
        ((eq major-mode 'Custom-mode)
         (ace-link-custom))
        ((eq major-mode 'sldb-mode)
         (ace-link-sldb))
        ((eq major-mode 'slime-xref-mode)
         (ace-link-slime-xref))
        ((eq major-mode 'slime-inspector-mode)
         (ace-link-slime-inspector))
        ((eq major-mode 'indium-inspector-mode)
         (ace-link-indium-inspector))
        ((eq major-mode 'indium-debugger-frames-mode)
         (ace-link-indium-debugger-frames))
        ((eq major-mode 'magit-commit-mode)
         (ace-link-commit))
        ((eq major-mode 'cider-inspector-mode)
         (ace-link-cider-inspector))
        ((and ace-link-fallback-function
              (funcall ace-link-fallback-function)))
        (t
         (error
          "%S isn't supported"
          major-mode))))

;;* `ace-link-info'
;;;###autoload
(defun ace-link-info ()
  "Open a visible link in an `Info-mode' buffer."
  (interactive)
  (let ((pt (avy-with ace-link-info
              (avy-process
               (mapcar #'cdr
                       (ace-link--info-collect))
               (avy--style-fn avy-style)))))
    (ace-link--info-action pt)))

(defun ace-link--info-action (pt)
  (when (numberp pt)
    (push-mark)
    (goto-char pt)
    (let ((we (window-end)))
      (while (not (ignore-errors
                    (Info-follow-nearest-node)))
        (forward-char 1)
        (when (> (point) we)
          (error "Could not follow link"))))))

(declare-function Info-follow-nearest-node "info")
(declare-function Info-next-reference "info")
(declare-function Info-try-follow-nearest-node "info")
(declare-function Info-goto-node "info")

(defun ace-link--info-current ()
  "Return the node at point."
  (cons (cl-letf (((symbol-function #'Info-goto-node)
                   (lambda (node _) node))
                  (browse-url-browser-function
                   (lambda (url &rest _) url)))
          (Info-try-follow-nearest-node))
        (point)))

(defun ace-link--info-collect ()
  "Collect the positions of visible links in the current `Info-mode' buffer."
  (let ((end (window-end))
        points)
    (save-excursion
      (goto-char (window-start))
      (when (ignore-errors (Info-next-reference) t)
        (push (ace-link--info-current) points)
        (Info-next-reference)
        (while (and (< (point) end)
                    (> (point) (cdar points)))
          (push (ace-link--info-current) points)
          (Info-next-reference))
        (nreverse points)))))

;;* `ace-link-help'
;;;###autoload
(defun ace-link-help ()
  "Open a visible link in a `help-mode' buffer."
  (interactive)
  (let ((pt (avy-with ace-link-help
              (avy-process
               (mapcar #'cdr (ace-link--help-collect))
               (avy--style-fn avy-style)))))
    (ace-link--help-action pt)))

(defun ace-link--help-action (pt)
  (when (numberp pt)
    (goto-char (1+ pt))
    (push-button)))

(defun ace-link--help-collect ()
  "Collect the positions of visible links in the current `help-mode' buffer."
  (let ((skip (text-property-any
               (window-start) (window-end) 'button nil))
        candidates)
    (save-excursion
      (while (setq skip (text-property-not-all
                         skip (window-end) 'button nil))
        (goto-char skip)
        (push (cons (button-label (button-at skip)) skip) candidates)
        (setq skip (text-property-any (point) (window-end)
                                      'button nil))))
    (nreverse candidates)))

;;* `ace-link-commit'
(defvar ivy-ffap-url-functions)
(defvar ffap-url-fetcher)
(declare-function magit-goto-next-section "ext:magit")
(declare-function magit-current-section "ext:magit")
(declare-function magit-section-end "ext:magit")

(defun ace-link-commit ()
  "Open an issue link in the browser."
  (interactive)
  (require 'counsel)
  (require 'ffap)
  (let* ((pts (save-excursion
                (goto-char (point-min))
                (when (eq major-mode 'magit-commit-mode)
                  (magit-goto-next-section))
                (let ((cands nil)
                      (end (if (eq major-mode 'magit-commit-mode)
                               (magit-section-end (magit-current-section))
                             (point-max))))
                  (while (re-search-forward "#\\([0-9]+\\)" end t)
                    (push (match-beginning 0) cands))
                  (nreverse cands))))
         (_pt (avy-with ace-link-commit
               (avy-process pts)))
         (url (cl-reduce
               (lambda (a b)
                 (or a (funcall b)))
               ivy-ffap-url-functions
               :initial-value nil)))
    (funcall ffap-url-fetcher url)))

;;* `ace-link-man'
;;;###autoload
(defun ace-link-man ()
  "Open a visible link in a `man' buffer."
  (interactive)
  (let ((pt (avy-with ace-link-man
              (avy-process
               (mapcar #'cdr (ace-link--man-collect))
               (avy--style-fn avy-style)))))
    (ace-link--man-action pt)))

(defun ace-link--man-action (pt)
  (when (number-or-marker-p pt)
    (goto-char (1+ pt))
    (if (button-at pt)
        (push-button pt)
      (call-interactively #'man-follow))))

(declare-function Man-default-man-entry "man")

(defun ace-link--man-collect ()
  "Collect all visible links in `Man-mode'.

There are two ways of following links interactively in
`Man-mode':

1. `push-button' (if there's a button overlay at point).
2. `man-follow' (if there's no button at point).

`man-follow' simply takes whatever text is at point and tries to
follow it as a manual page.  This logic can't be used by
`ace-link' since that would make every word a link.  However,
we'd miss actual links by only collecting button overlays.

The workaround for non-button links is to search for strings that
looks like manpages with a regular expression."
  (save-excursion
    (let ((end (window-end nil t))
          (pt (window-start))
          candidates)
      (while (and (setq pt (next-property-change pt))
                  (< pt end))
        (let ((entry (Man-default-man-entry pt)))
          (when (or (button-at pt)
                    (and (text-properties-at pt)
                         (string-match-p "^[^(]+([0-9]+)$" entry)))
            (push (cons entry pt) candidates))))
      (nreverse candidates))))

;;* `ace-link-woman'
;;;###autoload
(defun ace-link-woman ()
  "Open a visible link in a `woman-mode' buffer."
  (interactive)
  (let ((pt (avy-with ace-link-woman
              (avy-process
               (mapcar #'cdr (ace-link--woman-collect))
               (avy--style-fn avy-style)))))
    (ace-link--woman-action pt)))

(defun ace-link--woman-action (pt)
  (when (number-or-marker-p pt)
    (goto-char (1+ pt))
    (push-button)))

(defun ace-link--woman-collect ()
  "Collect all links visible in the current `woman-mode' buffer."
  (let ((end (window-end))
        candidates)
    (save-excursion
      (goto-char (window-start))
      (while (and (condition-case nil (forward-button 1)
                    (error nil))
                  (< (point) end))
        (push (cons (button-label (button-at (point))) (point))
              candidates))
      (nreverse candidates))))

;;* `ace-link-eww'
;;;###autoload
(defun ace-link-eww (&optional external)
  "Open a visible link in an `eww-mode' buffer.
If EXTERNAL is single prefix, browse the URL using
`browse-url-secondary-browser-function'.

If EXTERNAL is double prefix, browse in new buffer."
  (interactive "P")
  (let ((pt (avy-with ace-link-eww
              (avy-process
               (mapcar #'cdr (ace-link--eww-collect))
               (avy--style-fn avy-style)))))
    (ace-link--eww-action pt external)))

(declare-function eww-follow-link "eww")

(defun ace-link--eww-action (pt external)
  (when (number-or-marker-p pt)
    (goto-char pt)
    (eww-follow-link external)))

(defun ace-link--eww-collect (&optional property)
  "Collect the positions of visible links in the current `eww' buffer."
  (unless property
    (setq property 'shr-url))
  (save-excursion
    (save-restriction
      (narrow-to-region
       (window-start)
       (window-end))
      (goto-char (point-min))
      (let (beg end candidates)
        (setq end
              (if (get-text-property (point) property)
                  (point)
                (text-property-any
                 (point) (point-max) property nil)))
        (while (setq beg (text-property-not-all
                          end (point-max) property nil))
          (goto-char beg)
          ;; Skip leading newlines in the next link text.  They make things very
          ;; ugly when running `ace-link-eww' since the characters to jump to
          ;; each link will be displayed on the line before its visible text.
          (skip-chars-forward "\n")
          (setq beg (point))
          ;; Handle the case where a link is all newlines by skipping them.
          (if (get-text-property (point) property)
              (progn
                (setq end (next-single-property-change (point) property nil (point-max)))
                ;; When link at the end of buffer, end will be set to nil.
                (if (eq end nil)
                    (setq end (point-max)))
                (push (cons (buffer-substring-no-properties beg end) beg)
                      candidates))
            (setq end (point))))
        (nreverse candidates)))))

;;* `ace-link-w3m'
;;;###autoload
(defun ace-link-w3m ()
  "Open a visible link in an `w3m-mode' buffer."
  (interactive)
  (require 'w3m)
  (let ((pt (avy-with ace-link-w3m
              (avy-process
               (mapcar #'cdr (ace-link--w3m-collect))
               (avy--style-fn avy-style)))))
    (ace-link--w3m-action pt)))

(declare-function w3m-view-this-url "w3m")

(defun ace-link--w3m-action (pt)
  (when (numberp pt)
    (goto-char pt)
    (w3m-view-this-url)))

(defun ace-link--w3m-collect ()
  "Collect the positions of visible links in the current `w3m' buffer."
  (save-excursion
    (save-restriction
      (narrow-to-region
       (window-start)
       (window-end))
      (goto-char (point-min))
      (let ((anchor-prop 'w3m-anchor-sequence)
            (beg (point))
            (end 0)
            candidates)
        ;; property values for anchors in order are: 1 2 3 ...
        (unless (get-text-property beg anchor-prop)
          (setq beg (next-single-char-property-change beg anchor-prop)))
        (while (< beg (point-max))
          (setq end (next-single-char-property-change beg anchor-prop))
          (push (cons (buffer-substring-no-properties beg end) beg)
                candidates)
          (setq beg (next-single-char-property-change end anchor-prop)))
        (nreverse candidates)))))

;;* `ace-link-compilation'
;;;###autoload
(defun ace-link-compilation ()
  "Open a visible link in a `compilation-mode' buffer."
  (interactive)
  (let ((pt (avy-with ace-link-compilation
              (avy-process
               (mapcar #'cdr (ace-link--eww-collect 'help-echo))
               (avy--style-fn avy-style)))))
    (ace-link--compilation-action pt)))

(defun ace-link--compilation-action (pt)
  (when (number-or-marker-p pt)
    (goto-char (1+ pt))
    (compile-goto-error)))

(declare-function compile-goto-error "compile")

;;* `ace-link-gnus'
(defvar gnus-article-buffer)
(defvar mm-text-html-renderer)
(declare-function gnus-article-press-button "gnus-art")
(declare-function gnus-get-buffer-window "gnus-win")
(declare-function widget-button-press "wid-edit")
(declare-function widget-forward "wid-edit")

;;;###autoload
(defun ace-link-gnus ()
  "Open a visible link in a `gnus-article-mode' buffer."
  (interactive)
  (save-selected-window
    (when (eq major-mode 'gnus-summary-mode)
      (if-let ((win (gnus-get-buffer-window gnus-article-buffer 'visible)))
          (progn
            (select-window win)
            (select-frame-set-input-focus (window-frame win)))
        (user-error "No article window found")))
    (let ((pt (avy-with ace-link-gnus
                (avy-process
                 (ace-link--gnus-collect)
                 (avy--style-fn avy-style)))))
      (ace-link--gnus-action pt))))

(defun ace-link--gnus-action (pt)
  (when (number-or-marker-p pt)
    (goto-char (1+ pt))
    (cond ((< emacs-major-version 27)
           (widget-button-press (point)))
          ((and (eq mm-text-html-renderer 'shr)
             (get-text-property (point) 'shr-url))
           (shr-browse-url))
          ((get-text-property (point) 'gnus-callback)
           (gnus-article-press-button))
          (t (push-button)))))

(defun ace-link--gnus-collect ()
  "Collect the positions of visible links in the current gnus buffer."
  (if (<= 27 emacs-major-version)
      (mapcar #'cdr (ace-link--woman-collect))
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
            (when (or (plist-get (text-properties-at (point)) 'gnus-string)
                      (plist-get (text-properties-at (point)) 'shr-url))
              (push (point) candidates)))
          (nreverse candidates))))))

;;* Helper functions for `ace-link-mu4e' and `ace-link-notmuch'
(defun ace-link--email-view-plain-collect ()
  "Collect the positions of visible links in email-view buffer.
Only consider the links in 'text/plain'."
  (let (candidates pt)
    (save-excursion
      (save-restriction
        (narrow-to-region
         (window-start)
         (window-end))
        (goto-char (point-min))
        (while (re-search-forward "https?://" nil t)
          (setq pt (- (point) (length (match-string 0))))
          (push pt candidates))))
    (nreverse candidates)))

(defun ace-link--email-view-next-link (pos &optional mu4e)
  "Find next link from POS in current email-view buffer.
If MU4E is non-nil, also consider mu4e-links."
  (let* ((shr-link-pos (text-property-not-all pos (point-max) 'shr-url nil))
         (links (list (list 'shr-url shr-link-pos))))
    (when mu4e
      (let ((mu4e-link-pos (text-property-not-all pos (point-max) 'mu4e-url nil))
            (mu4e-att-link-pos (text-property-not-all pos (point-max) 'mu4e-attnum nil)))
        (push (list 'mu4e-url mu4e-link-pos) links)
        (push (list 'mu4e-attnum mu4e-att-link-pos) links)
        (setq links (nreverse links))))
    (setq links (cl-remove-if-not
                 (lambda (link) (nth 1 link))
                 links))
    (if links
        (car
         (sort links (lambda (x y)
                       (< (elt x 1) (elt y 1)))))
      nil)))

(defun ace-link--email-view-end-of-link (link)
  "Return end of LINK at point in current email-view buffer."
  (or (text-property-any (elt link 1) (point-max) (elt link 0) nil)
      (point-max)))

(defun ace-link--email-view-html-collect (&optional mu4e)
  "Collect positions of visible links in the current email-view buffer.
If MU4E is non-nil, or if the buffer is in `mu4e-view-mode', also
consider mu4e’s links."
  (save-excursion
    (save-restriction
      (narrow-to-region
       (window-start)
       (window-end))
      (goto-char (point-min))
      (let (link pos candidates)
        (setq pos (point))
        (while (setq link (ace-link--email-view-next-link pos mu4e))
          (goto-char (elt link 1))
          (setq pos (ace-link--email-view-end-of-link link))
          (push (cons (buffer-substring-no-properties (elt link 1) pos) (elt link 1)) candidates))
        (nreverse candidates)))))

;;* `ace-link-mu4e'
;;;###autoload
(defun ace-link-mu4e ()
  "Open a visible link in an `mu4e-view-mode' buffer."
  (interactive)
  (if (bound-and-true-p mu4e-view-use-gnus)
      (ace-link-gnus)
    (let ((pt (avy-with ace-link-mu4e
                (avy-process
                 (mapcar #'cdr (ace-link--email-view-html-collect t))
                 (avy--style-fn avy-style)))))
      (ace-link--mu4e-action pt))))

(declare-function shr-browse-url "shr")
(declare-function mu4e~view-browse-url-from-binding "ext:mu4e-view")
(declare-function mu4e~view-open-attach-from-binding "ext:mu4e-view")

(defun ace-link--mu4e-action (pt)
  "Open link at PT in a `mu4e-view' buffer."
  (when (number-or-marker-p pt)
    (goto-char (1+ pt))
    (cond ((get-text-property (point) 'shr-url)
           (shr-browse-url))
          ((get-text-property (point) 'mu4e-url)
           (mu4e~view-browse-url-from-binding))
          ((get-text-property (point) 'mu4e-attnum)
           (mu4e~view-open-attach-from-binding)))))

;;* `ace-link-notmuch'
;;;###autoload
(defun ace-link-notmuch-plain ()
  "Open a visible link in a `notmuch-show' buffer.
Only consider the 'text/plain' portion of the buffer."
  (interactive)
  (let ((pt (avy-with ace-link-notmuch-plain
              (avy-process
               (ace-link--email-view-plain-collect)
               #'avy--overlay-pre))))
    (when pt
      (ace-link--notmuch-plain-action pt))))

(defun ace-link--notmuch-plain-action (pt)
  "Open link at PT in a `notmuch-show' buffer.
Only works in 'text/plain'"
  (when (number-or-marker-p pt)
    (goto-char pt)
    (browse-url-at-point)))

;;;###autoload
(defun ace-link-notmuch-html ()
  "Open a visible link in a `notmuch-show' buffer.
Only consider the 'text/html' portion of the buffer."
  (interactive)
  (if (bound-and-true-p mu4e-view-use-gnus)
      (ace-link-gnus)
    (let ((pt (avy-with ace-link-mu4e
                (avy-process
                 (mapcar #'cdr (ace-link--email-view-html-collect))
                 (avy--style-fn avy-style)))))
      (ace-link--mu4e-action pt))))

(defun ace-link--notmuch-html-action (pt)
  "Open link at PT in a `notmuch-show' buffer.
Only works in 'text/html'"
  (when (number-or-marker-p pt)
    (when (get-text-property (point) 'shr-url)
      (shr-browse-url))))

;;;###autoload
(defun ace-link-notmuch ()
  "Open a visible link in `notmuch-show' buffer.
Consider both the links in 'text/plain' and 'text/html'."
  (interactive)
  (let ((match (avy-with ace-link-notmuch
                 (avy-process
                  (ace-link--notmuch-collect)
                  #'avy--overlay-pre))))
    (when match
      (let ((pt (car match))
            (fn (cdr match)))
        (funcall fn pt)))))

(defun ace-link--notmuch-collect ()
  "Collect the positions of visible links in `notmuch-show' buffer.
Considers the links in 'text/plain' and 'text/html'.
Returns a list of cons \( fn . pt ) where FN is the function to
call at PT."
  (append
   (mapcar (lambda (x)
             (cons x #'ace-link--notmuch-plain-action))
           (ace-link--email-view-plain-collect))
   (mapcar (lambda (x)
             (cons (cdr x) #'ace-link--notmuch-html-action))
           (ace-link--email-view-html-collect))))

;;* `ace-link-org'
;;;###autoload
(defun ace-link-org ()
  "Open a visible link in an `org-mode' buffer."
  (interactive)
  (require 'org)
  (let ((pt (avy-with ace-link-org
               (avy-process
                (mapcar #'cdr (ace-link--org-collect))
                (avy--style-fn avy-style)))))
    (ace-link--org-action pt)))

(declare-function org-open-at-point "org")
(declare-function outline-invisible-p "outline")
(defvar org-link-any-re)

(defun ace-link--org-action (pt)
  (when (numberp pt)
    (goto-char pt)
    (org-open-at-point)))

(defun ace-link--org-collect ()
  (let ((end (window-end))
        res)
    (save-excursion
      (goto-char (window-start))
      (while (re-search-forward org-link-any-re end t)
        ;; Check that the link is visible. Look at the last character
        ;; position in the link ("...X]]") to cover links with and
        ;; without a description.
        (when (not (outline-invisible-p (- (match-end 0) 3)))
          (push
           (cons
            (buffer-substring-no-properties
             (match-beginning 0)
             (match-end 0))
            (match-beginning 0))
           res)))
      (nreverse res))))

;;* `ace-link-org-agenda'
;;;###autoload
(defun ace-link-org-agenda ()
  "Open a visible link in an `org-mode-agenda' buffer."
  (interactive)
  (require 'org-agenda)
  (let ((pt (avy-with ace-link-org-agenda
              (avy-process
               (mapcar #'cdr (ace-link--org-agenda-collect))
               (avy--style-fn avy-style)))))
    (ace-link--org-agenda-action pt)))

(declare-function org-agenda-goto "org-agenda")

(defun ace-link--org-agenda-action (pt)
  (when (numberp pt)
    (goto-char pt)
    (org-agenda-goto)))

(defun ace-link--org-agenda-collect ()
  (let ((skip (text-property-any
               (window-start) (window-end) 'org-marker nil))
        candidates)
    (save-excursion
      (while (setq skip (text-property-not-all
                         skip (window-end) 'org-marker nil))
        (goto-char skip)
        (push (cons (get-char-property (point) 'txt) skip) candidates)
        (setq skip (text-property-any (point) (window-end)
                                      'org-marker nil))))
    (nreverse candidates)))

;;* `ace-link-xref'
;;;###autoload
(defun ace-link-xref ()
  "Open a visible link in an `xref--xref-buffer-mode' buffer."
  (interactive)
  (let ((pt (avy-with ace-link-xref
              (avy-process
               (ace-link--xref-collect)
               (avy--style-fn avy-style)))))
    (ace-link--xref-action pt)))

(declare-function xref-goto-xref "xref")

(defun ace-link--xref-action (pt)
  (when (numberp pt)
    (goto-char pt)
    (xref-goto-xref)))

(defun ace-link--xref-collect ()
  (let ((skip (text-property-any
               (window-start) (window-end) 'xref-item nil))
        candidates)
    (save-excursion
      (while (setq skip (text-property-not-all
                         skip (window-end) 'xref-item nil))
        (push (goto-char skip) candidates)
        (setq skip (text-property-any (point) (window-end)
                                      'xref-item nil))))
    (nreverse candidates)))

;;* `ace-link-custom'
;;;###autoload
(defun ace-link-custom ()
  "Open a visible link in an `Custom-mode' buffer."
  (interactive)
  (let ((pt (avy-with ace-link-custom
              (avy-process
               (ace-link--custom-collect)
               (avy--style-fn avy-style)))))
    (ace-link--custom-action pt)))

(declare-function Custom-newline "cus-edit")

(defun ace-link--custom-action (pt)
  (when (number-or-marker-p pt)
    (goto-char pt)
    (Custom-newline (point))))

(defun ace-link--custom-collect ()
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

;;* `ace-link-addr'
;;;###autoload
(defun ace-link-addr ()
  "Open a visible link in a goto-address buffer."
  (interactive)
  (let ((pt (avy-with ace-link-addr
               (avy-process
                (ace-link--addr-collect)
                (avy--style-fn avy-style)))))
    (ace-link--addr-action pt)))

(defun ace-link--addr-action (pt)
  (when (number-or-marker-p pt)
    (goto-char (1+ pt))
    (goto-address-at-point)))

(defun ace-link--addr-collect ()
  (let (candidates)
    (dolist (overlay (overlays-in (window-start) (window-end)))
      (if (overlay-get overlay 'goto-address)
          (push (overlay-start overlay) candidates)))
    (nreverse candidates)))

;;* `ace-link-sldb'
;;;###autoload
(defun ace-link-sldb ()
  "Interact with a frame or local variable in a sldb buffer."
  (interactive)
  (let ((pt (avy-with ace-link-sldb
              (avy-process
               (ace-link--sldb-collect)
               (avy--style-fn avy-style)))))
      (ace-link--sldb-action pt)))

(declare-function sldb-default-action "slime")

(defvar ace-link--sldb-action-fn #'sldb-default-action
  "Function to call after jump.")

(defun ace-link--sldb-action (pt)
  (when (number-or-marker-p pt)
    (goto-char pt)
    (funcall ace-link--sldb-action-fn)))

(defun ace-link--sldb-collect ()
  (let ((vars (list))
        (frames (list))
        (frame-prop 'frame)
        (var-face 'sldb-local-value-face))
    (save-excursion
      (goto-char (window-start))
      (while (< (point) (window-end))
        (when (get-text-property (point) frame-prop)
          (if (get-text-property (point) 'var)
              (push (text-property-any
                     (point)
                     (line-end-position)
                     'face var-face)
                    vars)
            (push (point) frames)
            (point)))
        (forward-visible-line 1)))
    ;; sort variables before frames
    (nreverse (nconc frames vars))))

;;* `ace-link-slime-xref'
;;;###autoload
(defun ace-link-slime-xref ()
  "Open a visible link in an `slime-xref-mode' buffer."
  (interactive)
  (let ((pt (avy-with ace-link-slime-xref
              (avy-process
               (ace-link--slime-xref-collect)
               (avy--style-fn avy-style)))))
      (ace-link--slime-xref-action pt)))

(declare-function slime-goto-xref "slime.el")

(defun ace-link--slime-xref-action (pt)
  (when (number-or-marker-p pt)
    (goto-char pt)
    (slime-goto-xref)))

(defun ace-link--slime-xref-collect ()
  (let ((candidates (list))
        (prop 'slime-location)
        (pt (window-start)))
    (while (and pt (< pt (window-end)))
      (when (get-text-property pt prop)
        (push pt candidates))
      (setq pt (next-single-property-change pt prop)))
    (nreverse candidates)))


;;* `ace-link-slime-inspector'
;;;###autoload
(defun ace-link-slime-inspector ()
  "Interact with a value, an action or a range button in a
`slime-inspector-mode' buffer."
  (interactive)
  (let ((pt (avy-with ace-link-slime-inspector
              (avy-process
               (ace-link--slime-inspector-collect)
               (avy--style-fn avy-style)))))
      (ace-link--slime-inspector-action pt)))

(declare-function slime-inspector-operate-on-point "slime.el")
(declare-function slime-inspector-copy-down-to-repl "slime.el")

(defun ace-link--slime-inspector-action (pt)
  (when (number-or-marker-p pt)
    (goto-char pt)
    (if (= pt 1)
        (call-interactively #'slime-inspector-copy-down-to-repl)
      (slime-inspector-operate-on-point))))

(defun ace-link--slime-inspector-collect ()
  (let ((candidates (list))
        (part 'slime-part-number)
        (range 'slime-range-button)
        (action 'slime-action-number)
        (pt (window-start)))
    (while (and pt (< pt (window-end)))
      (when (or (get-text-property pt part)
                (get-text-property pt range)
                (get-text-property pt action))
        (push pt candidates))
      (setq pt (next-property-change pt)))
    (nreverse candidates)))

;;* `ace-link-indium-inspector'
;;;###autoload
(defun ace-link-indium-inspector ()
  "Interact with a value, an action or a range button in a
`indium-inspector-mode' buffer."
  (interactive)
  (let ((pt (avy-with ace-link-indium-inspector
              (avy-process
               (ace-link--indium-inspector-collect)
               (avy--style-fn avy-style)))))
    (ace-link--indium-inspector-action pt)))

(defun ace-link--indium-inspector-action (pt)
  (when (numberp pt)
    (goto-char pt)
    (indium-follow-link)))

(defun ace-link--indium-inspector-collect ()
  "Collect the positions of visible links in the current `indium-inspector-mode' buffer."
  (let ((candidates)
        (old-position))
    (save-excursion
      (goto-char (point-max))
      (setq old-position (point))
      (indium-inspector-previous-reference)
      (while (not (= (point) old-position))
        (push (point) candidates)
        (setq old-position (point))
        (indium-inspector-previous-reference)))
    candidates))

;;* `ace-link-indium-debugger-frames'
;;;###autoload
(defun ace-link-indium-debugger-frames ()
  "Interact with a value, an action or a range button in a
`indium-debugger-frames-mode' buffer."
  (interactive)
  (let ((pt (avy-with ace-link-indium-debugger-frames
              (avy-process
               (ace-link--indium-debugger-frames-collect)
               (avy--style-fn avy-style)))))
    (ace-link--indium-debugger-frames-action pt)))

(defun ace-link--indium-debugger-frames-action (pt)
  (when (numberp pt)
    (goto-char pt)
    (indium-follow-link)))

(defun ace-link--indium-debugger-frames-collect ()
  "Collect the positions of visible links in the current `indium-debugger-frames-mode' buffer."
  (let ((candidates)
        (old-position))
    (save-excursion
      (goto-char (point-max))
      (setq old-position (point))
      (indium-debugger-frames-previous-frame)
      (while (and (not (= (point) old-position)) (not (= (point) (point-min))))
        (push (point) candidates)
        (setq old-position (point))
        (indium-debugger-frames-previous-frame)))
    candidates))

;;* `ace-link-cider-inspector'
;;;###autoload
(defun ace-link-cider-inspector ()
  "Open a visible link in a `cider-inspector-mode' buffer."
  (interactive)
  (let ((pt (avy-with ace-link-cider-inspector
              (avy-process
               (ace-link--cider-inspector-collect)
               (avy--style-fn avy-style)))))
    (ace-link--cider-inspector-action pt)))

(declare-function cider-inspector-operate-on-point "ext:cider-inspector")

(defun ace-link--cider-inspector-collect ()
  "Collect the positions of visible links in the current
`cider-inspector-mode' buffer."
  (let ((end (window-end))
        points)
    (save-excursion
      (goto-char (window-start))
      (while (< (point) end)
        (goto-char (next-single-property-change (point) 'cider-value-idx nil end))
        (when (get-text-property (point) 'cider-value-idx)
          (push (point) points)))
      (nreverse points))))

(defun ace-link--cider-inspector-action (pt)
  (when (number-or-marker-p pt)
    (goto-char pt)
    (cider-inspector-operate-on-point)))

;;* Bindings
(defvar eww-link-keymap)
(defvar eww-mode-map)
(defvar custom-mode-map)
(declare-function indium-follow-link "ext:indium")
(declare-function indium-inspector-previous-reference "ext:indium")
(declare-function indium-debugger-frames-previous-frame "ext:indium")

;;;###autoload
(defun ace-link-setup-default (&optional key)
  "Bind KEY to appropriate functions in appropriate keymaps."
  (setq key (or key "o"))
  (add-to-list 'avy-styles-alist
               '(ace-link-info . at))
  (add-to-list 'avy-styles-alist
               '(ace-link-help . post))
  (add-to-list 'avy-styles-alist
               '(ace-link-woman . post))
  (add-to-list 'avy-styles-alist
               '(ace-link-eww . post))
  (add-to-list 'avy-styles-alist
               '(ace-link-w3m . post))
  (add-to-list 'avy-styles-alist
               '(ace-link-compilation . post))
  (add-to-list 'avy-styles-alist
               '(ace-link-gnus . post))
  (add-to-list 'avy-styles-alist
               '(ace-link-mu4e . post))
  (add-to-list 'avy-styles-alist
               '(ace-link-org . pre))
  (add-to-list 'avy-styles-alist
               '(ace-link-org-agenda . pre))
  (add-to-list 'avy-styles-alist
               '(ace-link-custom . pre))
  (add-to-list 'avy-styles-alist
               '(ace-link-addr . pre))
  (add-to-list 'avy-styles-alist
               '(ace-link-xref . at))
  (add-to-list 'avy-styles-alist
               '(ace-link-sldb . pre))
  (add-to-list 'avy-styles-alist
               '(ace-link-slime-xref . pre))
  (add-to-list 'avy-styles-alist
               '(ace-link-slime-inspector . pre))
  (eval-after-load "xref"
    `(define-key xref--xref-buffer-mode-map ,key 'ace-link-xref))
  (eval-after-load "info"
    `(define-key Info-mode-map ,key 'ace-link-info))
  (eval-after-load "notmuch"
    `(define-key notmuch-show-mode-map ,key 'ace-link-notmuch))
  (eval-after-load "compile"
    `(define-key compilation-mode-map ,key 'ace-link-compilation))
  (eval-after-load "help-mode"
    `(define-key help-mode-map ,key 'ace-link-help))
  (eval-after-load "woman"
    `(define-key woman-mode-map ,key 'ace-link-woman))
  (eval-after-load "eww"
    `(progn
       (define-key eww-link-keymap ,key 'ace-link-eww)
       (define-key eww-mode-map ,key 'ace-link-eww)))
  (eval-after-load 'cus-edit
    `(progn
       (define-key custom-mode-map ,key 'ace-link-custom)))
  (eval-after-load "helpful"
    `(progn
       (define-key helpful-mode-map ,key 'ace-link-help)))
  (eval-after-load "elbank-overview"
    `(progn
       (define-key elbank-overview-mode-map ,key 'ace-link-help)))
  (eval-after-load "elbank-report"
    `(progn
       (define-key elbank-report-mode-map ,key 'ace-link-help)))
  (eval-after-load "indium-inspector"
    `(progn
       (define-key indium-inspector-mode-map ,key 'ace-link-indium-inspector)))
  (eval-after-load "indium-debugger"
    `(progn
       (define-key indium-debugger-frames-mode-map ,key 'ace-link-indium-debugger-frames)))
  (eval-after-load "cider-inspector"
    `(progn
       (define-key cider-inspector-mode-map ,key 'ace-link-cider-inspector))))

(provide 'ace-link)

;;; ace-link.el ends here

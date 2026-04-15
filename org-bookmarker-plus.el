;;; org-bookmarker-plus.el --- Configure bookmark+ for auto-complete and jump-to org-headers -*- lexical-binding: t -*-

;; Author: lordnik22
;; Version 1.0
;; Package-Requires: ((emacs "29.0"))

;;; Commentary
;; This file add a new company-bookmark-backend the
;; completion-candidates are taken from bookmark-list. With
;; bookmark-plus we add a new bookmark-type: org-id-links. We can use
;; these bookmarks not only as completion-candidates but also as
;; jump-to-candidates. The code advices org-store-link to add these
;; new bookmarks.

;;; Code:
(require 'org)
(require 'org-id)
(require 'bookmark+)
(require 'company)

(defvar org-bookmarker-plus-id-prefix "id:")

;;;###autoload
(defun org-bookmarker-plus-id-complete-link (&optional arg)
  "Create an org-id-link using completion."
  (concat org-bookmarker-plus-id-prefix (org-id-get-with-outline-path-completion '((org-agenda-files . (:maxlevel . 9))))))

;;;###autoload
(defun org-bookmarker-plus-id-description-function (link desc)
  "Generate description for links"
  (interactive)
  (cond (desc desc)
        ((region-active-p) (progn (buffer-substring (region-beginning) (region-end))))
        ((or (string-prefix-p "http" link)
             (string-prefix-p "https" link))
         (let ((title "")
               (url-b (url-retrieve link
                                    (lambda (_)
                                      (setq title (dom-text (dom-by-tag (libxml-parse-html-region (point-min) (point-max) nil t) 'title)))))))
           (let ((fallover 0))
             (while (or (< fallover 1) (ignore-errors (process-status url-b)))
               (setq fallover (+ fallover 1))
               (sleep-for 0 1)))
           (cond ((or (string-prefix-p "https://www.youtube.com/watch?v=" link)
                      (string-prefix-p "https://youtu.be/" link))
                  (concat "YOUTUBE: " title))
                 ((string-match-p "duden" link)
                  (let ((s (split-string title "|" t "[[:space:]]")))
                    (concat (upcase (car s)) ": " (cadr s))))
                 ((string-match-p "wikipedia" link) (concat "WIKI: " title))
                 (t title))))
        ((string-prefix-p org-bookmarker-plus-id-prefix link)
         (save-window-excursion
           (save-excursion
             (org-id-goto (string-remove-prefix org-bookmarker-plus-id-prefix link))
             (org-link-display-format (org-get-heading t t t t)))))
        (t (save-excursion (word-at-point)))))

;;;; Add new bookmark-type: org-id-link
(define-advice org-store-link (:after (&rest r))
  (cond ((derived-mode-p 'org-mode)
         (let* ((hid (org-id-get-create))
                (bn (org-link-display-format (org-get-heading t t t t)))
                (bm-bn (bmkp-get-bookmark bn 'NOERROR)))
           (when (not (null bm-bn))
             (setq bn (concat bn
                              " (" (file-name-nondirectory buffer-file-name)
                              ":" (number-to-string (line-number-at-pos))
                              ")")))
           (if (and bm-bn
                    (equal hid (cadr (caddr (bookmark-prop-get bm-bn 'function)))))
               bm-bn
             (bmkp-make-function-bookmark bn (backquote (lambda () (org-id-goto ,hid)))))))))

;;;; company-backend which uses the bookmark-list
(defvar company-org-bookmark-available 'unknown)

(defun company-org-bookmark-available ()
  "When bookmarks successfully loaded will return non-nil."
  (when (eq company-org-bookmark-available 'unknown)
    (condition-case err
        (progn
          (list-bookmarks)
          (setq company-org-bookmark-available (not (null bookmark-alist))))
      (error
       (message "Company-Org-Bookmark: %s" (error-message-string err))
       (setq company-org-bookmark-available nil))))
  company-org-bookmark-available)

(defun company-org-bookmark--lookup-words (word)
  "Filter bookmark-alist by WORD and return candidates."
  (all-completions word bookmark-alist))

(defun company-org-bookmark (command &optional arg &rest _ignored)
  "`company-mode' completion backend using bookmark-alist."
  (interactive (list 'interactive))
  (cl-case command
    (interactive (company-begin-backend 'company-org-bookmark))
    (prefix (when (company-org-bookmark-available) (company-grab-word)))
    (candidates
     (let ((words (company-org-bookmark--lookup-words arg))
           (completion-ignore-case t))
       words))
    (post-completion
     (kill-backward-chars (length arg))
     (insert (concat "[[" org-bookmarker-plus-id-prefix (cadr (caddr (bookmark-prop-get arg 'function))) "]" "[" arg "]]")))
    (kind 'text)
    (sorted t)
    (ignore-case 'keep-prefix)))

(defun company-org-bookmark-hook ()
  (if (boundp 'company-backends)
      (add-to-list 'company-backends 'company-org-bookmark)
    (set (make-local-variable 'company-backends) '(company-files company-org-bookmark))))
(provide 'org-bookmarker-plus)
;;; org-bookmarker-plus.el ends here

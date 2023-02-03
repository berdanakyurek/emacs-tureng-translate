;;; tureng-translate.el --- Tureng inside emacs

;; Copyright (C) 2011 Free Software Foundation, Inc.


;; Author: Berdan Akyurek <berdanakyurek17@gmail.com>
;; Version: 1.0
;; Package-Requires: org-fragtog
;; Keywords: tureng, translate, turkish, english
;; URL: https://github.com/berdanakyurekk/emacs-tureng-translate

;;; Commentary:

;; This package translates a word from English to Turkish
;; or from Turkish to English

(require 'org)
(require 'thingatpt)
(require 'pdf-view)

(defun convert-codepoint-to-char ()
  (while (re-search-forward "&#\\([0-9]+\\);" nil t)
    (replace-match (format "%c" (string-to-number (match-string 1))))))

(define-derived-mode tureng-view-mode
  org-mode "Tureng View Mode"
  "Major mode for displaying tureng search results"
  (local-set-key (kbd "q") 'delete-window)
  (local-set-key (kbd "m") 'tureng-trans-change-mode))

(defun tureng (word lang other-terms)
  "finds the definitions of the world in tureng with lang language"
  (defvar tureng-translation)
  (defvar tureng-arr-item)
  (let ((url (concat "https://tureng.com/en/" lang "/" word)))
    (let ((content-buffer (url-retrieve-synchronously url t) ))
      (switch-to-buffer content-buffer)
      (goto-char 1)

      (let ((wordfound t))
        (unless (search-forward "<table" nil t)
          (kill-buffer content-buffer)
          (setq tureng-translation nil)
          (setq wordfound nil))
        (if wordfound
            (progn
              (if other-terms
                  (search-forward "<table"))
              (end-of-line)
              (let ((start-index (point)))
                (search-forward "</table>")
                (beginning-of-line)
                (narrow-to-region start-index (point)))
              (goto-char (point-min))
              (point)
              ;;(search-forward "<tr>" (point-max) t)
              (setq tureng-translation nil)
              (while (search-forward "<tr>" (point-max) t)
                (setq tureng-arr-item nil)
                (search-forward-regexp "<td\\|<th")
                ;; loop
                (dotimes (i 3)
                  (search-forward-regexp "<td\\|<th")
                  (search-forward ">")
                  (if (equal (char-after (point)) 60)
                      (search-forward ">"))

                  (let ((strt-index (point)))
                    (search-forward "<")
                    (backward-char)
                    (add-to-ordered-list 'tureng-arr-item (buffer-substring strt-index (point)) i))
                  )
                (add-to-list 'tureng-translation tureng-arr-item -1))
              (kill-buffer content-buffer))
          (progn
            (kill-buffer content-buffer)
            nil)))))
  tureng-translation)

(defun tureng-trans-change-mode ()
  "Toggles between phrase mode and word mode"
  (interactive)
  (goto-char (point-min))
  (search-forward ": ")
  (let ((current-word-dd (word-at-point)))
    (goto-char (point-min))

    (search-forward " ")
    (forward-char)
    ;;(message (word-at-point))
    (if (string= (current-word) "Word")
        (tureng-translate-not-interactive current-word-dd t)
      (tureng-translate-not-interactive current-word-dd nil))))

(defun show-tureng-in-org-table (word arr mode)
  "show arr in buffer where arr is the result of tureng function"

  (if (not (eq arr nil))
      (progn
        (if (get-buffer "tureng-trans")
            (kill-buffer "tureng-trans"))
        (get-buffer-create "tureng-trans")
        (set-buffer "tureng-trans")
        (tureng-view-mode)
        (if (eq mode t)
            (insert "* Phrase Mode: ")
          (insert "* Word Mode: "))
        (insert word)
        (org-return)
        (org-cycle)
        ;; (concat "'" word "' translation")
        (dotimes (i (length arr))
          (dotimes (j 3)
            (insert "|")
            (insert (decode-coding-string (nth j (nth i arr)) 'utf-8)))
          (insert "|")
          (org-return)
          (if (equal i 0)
              (insert "|-\n")))
        (delete-char -1)
        (goto-char (point-min))
        (convert-codepoint-to-char)
        (goto-char (point-max))
        (org-cycle)
        (org-beginning-of-line)
        (org-kill-line)
        (goto-char (point-min))

        (pop-to-buffer "tureng-trans")
        (read-only-mode))
    (progn
      (message "Unknown Word!"))))

(defun tureng-translate-region (arg)
  "Translate the word in the selected region."
  (if (not (use-region-p))
      (message "No selected text!")
    (progn
      (let ((word (buffer-substring (region-beginning) (region-end)))
            (table (tureng (buffer-substring (region-beginning) (region-end)) "turkish-english" arg)))
        (show-tureng-in-org-table word table arg)))))

(defun tureng-translate-word-around-point (arg)
  "Translates the word around the cursor."
  (let ((word (current-word))
        (table (tureng (current-word) "turkish-english" arg)))
    (show-tureng-in-org-table word table arg)))

;;;###autoload
(defun tureng-translate ()
  "Translates the region, if any region is selected and translates
current-word otherwise"
  (interactive)
  (if (use-region-p)
      (tureng-translate-region nil)
    (tureng-translate-word-around-point nil)))

(defun tureng-translate-not-interactive (word arg)
  "Translates word not interactively"
  (let ((table (tureng word "turkish-english" arg)))
    (show-tureng-in-org-table word table arg)))


(defun tureng-translate-pdf ()
  "Translate the region from PDFs"
  (interactive)
  (tureng-translate-not-interactive (car (pdf-view-active-region-text)) nil))

;; TESTS
;; (tureng "kar" "turkish-english" nil)
;; (tureng "kar" "turkish-english" t)
;; (tureng "özür" "turkish-english" nil)
;; (tureng "ring" "turkish-english" nil)
;; (tureng "cep telefonu" "turkish-english" nil)
;; (show-tureng-in-org-table "kar topu" (tureng "kar topu" "turkish-english" nil))

;; (tureng-translate-not-interactive "ekmek" nil)

(provide 'tureng-translate)

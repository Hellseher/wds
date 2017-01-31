;;; init-local.el -- Custom local variables and functios.
;;; Created       : Thu 11 Aug 2016 22:32:01
;;; Modified      : <2017-1-31 Tue 20:56:18 GMT> sharlatan
;;; Author        : Sharlatan <sharlatanus@gmail.com>
;;; Maintainer(s) : Sharlatan
;;; Commentary:
;;;
;;; Combined with Purcell "A reasonable Emacs config" https://github.com/purcell/emacs.d
;;;
;;; Code:

(defvar exzellenz/required-packages '(evil
                                      org-beautify-theme
                                      org-bullets
                                      ggtags
                                      yasnippet
                                      multi-term
                                      dockerfile-mode)
  "Required custom packages for exzellenz set up.")
(dolist (pkg exzellenz/required-packages)
  (require-package pkg))


;;; Time stemtp in the header when save the file
;; <Yar-month-day weekday Time Zone> username
;; TODO: how to insert WEEK number?
(setq time-stamp-pattern
      "8/Modified[ \t]*:\\\\?[ \t]*<%04Y-%:m-%02d %03a %02H:%02M:%02S %Z> %u\\\\?$" )

(add-hook 'before-save-hook 'time-stamp)


;;; Evil-mode
(setq evil-toggle-key "C-'")

;;; Org-mode
;;
;; http://ehneilsen.net/notebook/orgExamples/org-examples.html
(add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))

(setq org-src-fontify-natively t)

;;; flycheck
;; http://www.flycheck.org/en/latest/


;;; GNU Global
;; https://github.com/leoliu/ggtags
(add-hook 'c-mode-common-hook
          (lambda ()
            (when (derived-mode-p 'c-mode 'c++-mode 'java-mode 'asm-mode)
              (ggtags-mode 1))))


;;;Yas-snipets
;; http://joaotavora.github.io/yasnippet/
(yas-global-mode 1)
;; Default snippet path
(setq yas-snippet-dirs (file-expand-wildcards "~/.emacs.d/elpa/yasnippet*/snippets"))
(setq yas-snippet-dirs (append yas-snippet-dirs
                               '("~/.emacs.d/snippets")))

;;; tramp
;; https://www.gnu.org/software/tramp/
(defvar tramp-default-method)
(after-load 'tramp
  (setq tramp-default-method "ssh"))


;;; Guix
;; https://www.gnu.org/software/guix/
                                        ;(add-to-list 'load-path "~/.guix-profile/share/emacs/site-lisp/")
                                        ;(require 'guix-autoload)

;;; multi-term
;; https://www.emacswiki.org/emacs/download/multi-term.el

(add-hook 'term-mode-hook
          (lambda ()
            (yas-minor-mode -1)
            (add-to-list 'term-bind-key-alist '("C-c C-n" . multi-term-next))
            (add-to-list 'term-bind-key-alist '("C-c C-p" . multi-term-prev))
            (add-to-list 'term-bind-key-alist '("C-c C-j" . term-line-mode))
            (add-to-list 'term-bind-key-alist '("C-c C-k" . term-char-mode))
            ))
(global-set-key (kbd "C-c T") 'multi-term)

;;; Docker
;;


;;; Custom functios
;;
(defun exzellenz/hl-insert ()
  "Insert dashed horisotnal line."
  (interactive)
  (progn
    (insert-char #x002D 78)
    (comment-region (line-beginning-position) (line-end-position) )))

(defun exzellenz/timestamp ()
  "Insert timestamp YmdHMS."
  (interactive)
  (insert (format-time-string "%y%m%d%H%M%S")))

(defun exzellenz/cix--find-cmd-file ()
  "Find a file name of the command/word under ther cursor."
  (shell-command-to-string
   (concat "find ./ -type f -name \"*org\" -exec grep -lP \"^\\*\\* "
           (thing-at-point 'word)
           "\\s\" {} \\;")))

(defun exzellenz/cix-create-link-to-cmd ()
  "Insert a link to command under cursor."
  (interactive)
  (progn
    (beginning-of-line)
    (insert "[[file:"(exzellenz/cix--find-cmd-file))
    (delete-char -1)
    (kill-word 1)
    (insert "::*")
    (yank)
    (insert "][")
    (yank)
    (insert "]]")))

(defun exzellenz/org-todo-move-to-top ()
  "Move TODO entery to the top ot the file when it is DONE."
  (interactive)
  (save-excursion
    (progn
      (org-cut-special)
      (goto-char (point-min))
      (if (search-forward "* COMPLETED" nil 't)
          (progn
            (beginning-of-line)
            (org-yank))
        (goto-char (point-min))
        (beginning-of-line)
        (forward-line 10)
        (org-yank)))))

(after-load 'org
  (define-key org-mode-map (kbd "C-c C-x t") 'exzellenz/org-todo-move-to-top))

(provide 'init-local)
;;; init-local.el ends here

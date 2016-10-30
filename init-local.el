;;; init-local.el -- Custom local variables and functios.
;;; Created       : Thu 11 Aug 2016 22:32:01
;;; Last Modified : <2016-10-30 Sun 21:11:12 GMT> sharlatan
;;; Author        : Sharlatan <sharlatanus@gmail.com>
;;; Maintainer(s) : Sharlatan
;;; Commentary:
;;;
;;; Combined with Purcell "A reasonable Emacs config" https://github.com/purcell/emacs.d
;;;
;;; Code:

;;; Time stemtp in the header when save the file
;; <Yar-month-day weekday Time Zone> username
;; TODO: how to insert WEEK number?
(setq time-stamp-pattern
      "8/Last Modified[ \t]*:\\\\?[ \t]*<%04Y-%:m-%02d %03a %02H:%02M:%02S %Z> %u\\\\?$" )

(add-hook 'before-save-hook 'time-stamp)


;; http://ergoemacs.org/emacs/modernization_upcase-word.html
(defun toggle-letter-case ()
  "Toggle the letter case of current word or text selection.
Toggles between: “all lower”, “Init Caps”, “ALL CAPS”."
  (interactive)
  (let (p1 p2 (deactivate-mark nil) (case-fold-search nil))
    (if (region-active-p)
        (setq p1 (region-beginning) p2 (region-end))
      (let ((bds (bounds-of-thing-at-point 'word) ) )
        (setq p1 (car bds) p2 (cdr bds)) ) )

    (when (not (eq last-command this-command))
      (save-excursion
        (goto-char p1)
        (cond
         ((looking-at "[[:lower:]][[:lower:]]") (put this-command 'state "all lower"))
         ((looking-at "[[:upper:]][[:upper:]]") (put this-command 'state "all caps") )
         ((looking-at "[[:upper:]][[:lower:]]") (put this-command 'state "init caps") )
         ((looking-at "[[:lower:]]") (put this-command 'state "all lower"))
         ((looking-at "[[:upper:]]") (put this-command 'state "all caps") )
         (t (put this-command 'state "all lower") ) ) )
      )

    (cond
     ((string= "all lower" (get this-command 'state))
      (upcase-initials-region p1 p2) (put this-command 'state "init caps"))
     ((string= "init caps" (get this-command 'state))
      (upcase-region p1 p2) (put this-command 'state "all caps"))
     ((string= "all caps" (get this-command 'state))
      (downcase-region p1 p2) (put this-command 'state "all lower")) )
    )
  )

;;set this to C-x M-c
(global-set-key (kbd "C-x M-c") 'toggle-letter-case)

(defun swap-windows ()
  "If you have 2 windows, it swaps them."
  (interactive)
  (cond ((/= (count-windows) 2)
         (message "You need exactly 2 windows to do this."))
        (t
         (let* ((w1 (first (window-list)))
                (w2 (second (window-list)))
                (b1 (window-buffer w1))
                (b2 (window-buffer w2))
                (s1 (window-start w1))
                (s2 (window-start w2)))
           (set-window-buffer w1 b2)
           (set-window-buffer w2 b1)
           (set-window-start w1 s2)
           (set-window-start w2 s1))))
  (other-window 1))
(global-set-key (kbd "C-c s") 'swap-windows)

;;; Evil-mode
(setq evil-toggle-key "C-'")
(require-package 'evil)

;;; Org-mode
;;
;; http://ehneilsen.net/notebook/orgExamples/org-examples.html
(require-package 'org-beautify-theme)
(require-package 'org-bullets)
(add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))

(defun exellenz/org-todo-move-to-top ()
  "Move TODO entery to the top ot the file when it is DONE."
  (interactive)
  (save-excursion
    (progn
      (org-cut-special)
      (goto-char (point-min))
      (if (search-forward "* DONE" nil 't)
          (progn
            (beginning-of-line)
            (org-yank))
        (goto-char (point-min))
        (beginning-of-line)
        (forward-line 10)
        (org-yank)))))

(after-load 'org
  (define-key org-mode-map (kbd "C-c C-x t") 'exellenz/org-todo-move-to-top))

;;; flycheck
;; http://www.flycheck.org/en/latest/
(after-load 'python
  (flychek-checker 'python-pylint))


;;;Yas-snipets
;; http://joaotavora.github.io/yasnippet/
(require-package 'yasnippet)
(yas-global-mode 1)
;; Default snippet path
(setq yas-snippet-dirs (file-expand-wildcards "~/.emacs.d/elpa/yasnippet*/snippets"))
(setq yas-snippet-dirs (append yas-snippet-dirs
                               '("~/.emacs.d/snippets")))

;;; tramp
;; https://www.gnu.org/software/tramp/

(after-load 'tramp
  (setq tramp-default-method "ssh"))


;;; Guix
;; https://www.gnu.org/software/guix/
                                        ;(add-to-list 'load-path "~/.guix-profile/share/emacs/site-lisp/")
                                        ;(require 'guix-autoload)

(provide 'init-local)
;;; init-local.el ends here

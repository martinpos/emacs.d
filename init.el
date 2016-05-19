;; init.el : my personal emacs setup
;;
;; Nov  8 2015  martin.pos@2lazy.nl  - .emacs -> init.el
;; Nov 12 2015  martin.pos@2lazy.nl  - re-grouped, less comments
;; Nov 14 2015  martin.pos@2lazy.nl  - whitespace-mode, ace mode
;; Nov 15 2015  martin.pos@nxp.com   - defuns, appearance, fixes
;; Nov 19 2015  martin.pos@2lazy.nl  - multiple cursors
;; Nov 20 2015  martin.pos@nxp.com   - ibuffer column width
;; Nov 23 2015  martin.pos@nxp.com   - insert-shell-command
;;                                   - auto-fill
;; Dec 03 2015  martin.pos@nxp.com   - visit-file-at-point
;;                                   - disable electric-pair-mode
;;                                   - insert-file-name
;; Dec 07 2015  martin.pos@nxp.com   - cua-selection-mode
;; Dec 09 2015  martin.pos@nxp.com   - cperl-mode
;; Feb 09 2016  martin.pos@nxp.com   - wrap-region
;; Feb 10 2016  martin.pos@nxp.com   - fix: ffap initialize
;; May 19 2016  martin.pos@nxp.com   - spelling, flyspell

;;
;; packages
;;
(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                         ("marmalade" . "http://marmalade-repo.org/packages/")
                         ("melpa" . "http://melpa.org/packages/")))
(package-initialize)
(require 's)
(require 'ffap)

;;
;; .emacs.d setup
;;
(setq backup-directory-alist
      `(("." . ,(expand-file-name
                 (concat user-emacs-directory "backups")))))
(setq savehist-file
      (expand-file-name
                 (concat user-emacs-directory "savehist")))
;;(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
;;(load custom-file)
(setq defuns-dir (expand-file-name "defuns" user-emacs-directory))
(dolist (file (directory-files defuns-dir t "\\w+"))
  (when (file-regular-p file)
    (load file)))

;;
;; appearance
;;
(set-background-color "gray95")
(global-hl-line-mode 1)
(set-face-background hl-line-face "gray92")
(set-face-attribute 'default nil :height 90)
(setq ibuffer-formats
      '((mark modified read-only " "
              (name 50 50 :left :elide) " "
              (size 9 -1 :right) " "
              (mode 16 16 :left :elide) " " filename-and-process)
        (mark " " (name 16 -1) " " filename)))
(add-to-list 'default-frame-alist '(background-color . "gray95"))
(add-to-list 'default-frame-alist '(foreground-color . "black"))

;;
;; whitespace-mode
;;
;;(require 'whitespace)
(progn
  (whitespace-mode -1)
  (global-whitespace-mode -1)
  (setq whitespace-line-column 130)
  ;; NB no space-mark, tab-mark or newline-mark in whitespace-style

  (setq whitespace-style '(face spaces tabs empty))
  (setq space-face (make-face 'space-face))
  (set-face-background 'space-face "gray90")
  (set-face-foreground 'space-face "white")
  (setq whitespace-space 'space-face)
  (setq tab-face (make-face 'tab-face))
  (set-face-background 'tab-face "gray75")
  (setq whitespace-tab 'tab-face)
  (whitespace-mode 1)
  (global-whitespace-mode 1)
  )

;;
;; generic
;;
(delete-selection-mode 1)
(electric-pair-mode 0)
(setq inhibit-splash-screen t)
(transient-mark-mode 0)
(setq column-number-mode t)
(display-time-mode 1)
(windmove-default-keybindings)
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)
(recentf-mode 1)
(setq recentf-max-menu-items 100)
(setq c-basic-offset 1)
(setq cperl-indent-level 1
      cperl-close-paren-offset -1
      cperl-continued-statement-offset 1
      cperl-indent-parens-as-block t
      cperl-tab-always-indent t)
(setq cperl-highlight-variables-indiscriminately t)
(defalias 'perl-mode 'cperl-mode)
(setq sentence-end-double-space nil)
(yas-global-mode 1)
(setq-default fill-column 130)
(add-hook 'text-mode-hook 'turn-on-auto-fill)
(savehist-mode 1)
(cua-selection-mode t)
(setq cua-auto-tabify-rectangles nil)
(setq cua-keep-region-after-copy t)
(wrap-region-mode t)

;;
;; spelling - May 19 2016  martin.pos@nxp.com
;;
;; see http://stackoverflow.com/questions/15891808/emacs-how-to-enable-automatic-spell-check-by-default
;;
(add-hook 'text-mode-hook 'flyspell-mode)
(add-hook 'prog-mode-hook 'flyspell-prog-mode)

;; Preset width nlinum
(add-hook 'nlinum-mode-hook
          (lambda ()
            (unless (boundp 'nlinum--width)
              (setq nlinum--width
                (length (number-to-string
                         (count-lines (point-min) (point-max))))))))

;;
;; key bindings
;;
(global-set-key (kbd "C-x C-r") 'recentf-open-files)
(global-set-key (kbd "C-=") 'er/expand-region)
(global-set-key (kbd "C-c n") 'cleanup-buffer)
(global-set-key [home] 'smart-beginning-of-line)
(global-set-key (kbd "M-%") 'query-replace-regexp)
(global-set-key (kbd "C-, F") 'copy-file-name-to-clipboard)
(global-set-key (kbd "C-, y") 'visit-file-at-point)
(global-set-key (kbd "C-, f") 'insert-file-name)

(global-set-key (kbd "<C-enter>") 'inline-shell-command)
(global-set-key (kbd "<M-enter>") 'filter-by-shell-command)
(global-set-key (kbd "<C-M-enter>") 'insert-shell-command)
;; resize window - find appropriate method for sizing the window
;;(global-set-key (kbd "S-M-C-<left>") 'shrink-window-horizontally)
;;(global-set-key (kbd "S-M-C-<right>") 'enlarge-window-horizontally)
;;(global-set-key (kbd "S-M-C-<down>") 'shrink-window)
;;(global-set-key (kbd "S-M-C-<up>") 'enlarge-window)
;; C-mousewheel scales font size
(global-set-key [C-mouse-4] 'text-scale-increase)
(global-set-key [C-mouse-5] 'text-scale-decrease)
;; move-text M-up, M-down
(move-text-default-bindings)
;; joins the following line onto this one (whattheemacsd.com)
(global-set-key (kbd "M-j") (lambda () (interactive) (join-line -1)))
;; jump
(global-set-key (kbd "C-+") 'ace-jump-mode)
(global-set-key (kbd "M-m") 'jump-char-forward)
(global-set-key (kbd "M-M") 'jump-char-backward)
(global-set-key (kbd "s-m") 'jump-char-backward)
;; multiple cursors
(global-unset-key (kbd "C-z"))
(global-set-key (kbd "C-z a") 'mc/mark-all-like-this)
(global-set-key (kbd "C-z A") 'mc/mark-all-in-region)
(global-set-key (kbd "C-z w") 'mc/mark-all-words-like-this)
(global-set-key (kbd "C-z n") 'mc/mark-next-like-this)
(global-set-key (kbd "C-z N") 'mc/unmark-next-like-this)
(global-set-key (kbd "C-z p") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-z P") 'mc/unmark-previous-like-this)
(global-set-key (kbd "C-z i") 'mc/insert-numbers)
(global-set-key (kbd "C-z q") 'mc/mark-all-in-region-regexp)
(global-set-key (kbd "C-z r") 'set-rectangular-region-anchor)

;; Org mode
(setq org-src-preserve-indentation t)
(setq org-src-fontify-natively t)
(add-hook 'doc-view-mode-hook 'auto-revert-mode)

;;
;; Oct 24 2015 martin.pos@2lazy.nl - inline-shell-command
;;
(defun inline-shell-command ()
  "execute region or current line as shell command"
  (interactive)
  (if (use-region-p)
      (setq
       start (region-beginning)
       end (region-end)
       )
    (setq
     start (point-at-bol)
     end (point-at-eol)
     )
    )
  (goto-char end)
  (setq command (concat (buffer-substring start end) "\n"))
  (insert (concat "\n" (shell-command-to-string command)))
  )

;;
;; Nov 22 2015  martin.pos@nxp.com - filter-by-shell-command
;;
;; inspired by: http://www.emacswiki.org/emacs/ExecuteExternalCommand
;;
(defun filter-by-shell-command ()
  "filter the region or current line by a execute region or current line as shell command"
  (interactive)
  (if (use-region-p)
      (setq
       start (region-beginning)
       end (region-end)
       )
    (setq
     start (point-at-bol)
     end (point-at-eol)
     )
    )
  (setq input (concat (buffer-substring start end) "\n"))
  (setq command (read-shell-command "Shell command: "))
  (goto-char end)
  (shell-command-on-region start end command t t)
  (exchange-point-and-mark)
  )

;;
;; Nov 23 2015  martin.pos@nxp.com - insert-shell-command
;;
(defun insert-shell-command ()
  "insert shell command in buffer"
  (interactive)
  (setq command (read-shell-command "Shell command: "))
  (insert (shell-command-to-string command))
  )

;;
;; Dec 03 2015  martin.pos@nxp.com - visit-file-at-point
;;
;; inspired by comment at http://stackoverflow.com/questions/3139970/open-a-file-at-line-with-filenameline-syntax
(defun visit-file-at-point ()
  "visit file under point, without prompt"
  (interactive)
  (find-file(ffap-guesser)))

;; from https://github.com/magnars/.emacs.d/blob/master/defuns/buffer-defuns.el
(defun cleanup-buffer ()
  "Perform a bunch of operations on the whitespace content of a buffer.
Including indent-buffer, which should not be called automatically on save."
  (interactive)
  (untabify-buffer)
  (delete-trailing-whitespace)
  (indent-buffer))
(defun untabify-buffer ()
  (interactive)
  (untabify (point-min) (point-max)))
(defun indent-buffer ()
  (interactive)
  (indent-region (point-min) (point-max)))

;; from http://stackoverflow.com/questions/145291/smart-home-in-emacs/145359
(defun smart-beginning-of-line ()
  "Move point to first non-whitespace character or beginning-of-line.
If point was already at that position, move point to beginning of line."
  (interactive "^")
  (let ((oldpos (point)))
    (back-to-indentation)
    (and (= oldpos (point))
         (beginning-of-line))))

;; from http://emacsredux.com/blog/2013/03/27/copy-filename-to-the-clipboard
(defun copy-file-name-to-clipboard ()
  "Copy the current buffer file name to the clipboard."
  (interactive)
  (let ((filename (if (equal major-mode 'dired-mode)
                      default-directory
                    (buffer-file-name))))
    (when filename
      (kill-new filename)
      (message "Copied buffer file name '%s' to the clipboard." filename))))

;; from http://stackoverflow.com/questions/16764502/insert-filename-using-ido
(defun insert-file-name (filename &optional args)
  "Insert name of file FILENAME into buffer after point.

  Prefixed with \\[universal-argument], expand the file name to
  its fully canocalized path.  See `expand-file-name'.

  Prefixed with \\[negative-argument], use relative path to file
  name from current directory, `default-directory'.  See
  `file-relative-name'.

  The default with no prefix is to insert the file name exactly as
  it appears in the minibuffer prompt."
  ;; Based on insert-file in Emacs -- ashawley 20080926
  (interactive `(,(ido-read-file-name "File Name: ")
                 ,current-prefix-arg))
  (cond ((eq '- args)
         (insert (expand-file-name filename)))
        ((not (null args))
         (insert filename))
        (t
         (insert (file-relative-name filename)))))

;; customize
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("4527ad80568d218b57e06ff1cab2e5391ab17e4c3252e74a3ea9d6db2d961db5" "5422b05b20c27caf9fe7a511baa8f3bcbaa3ea823cf54e7105fe759923047a26" default)))
 '(menu-bar-mode nil)
 '(scroll-bar-mode nil)
 '(send-mail-function (quote sendmail-send-it))
 '(tool-bar-mode nil)
 '(vhdl-basic-offset 1)
 '(vhdl-beautify-options (quote (t t t t t)))
 '(vhdl-upper-case-keywords t)
 '(vhdl-upper-case-types t))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

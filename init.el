;; init.el : my personal emacs setup
;;
;; Nov  8 2015  martin.pos@2lazy.nl  - .emacs -> init.el
;; Nov 12 2015  martin.pos@2lazy.nl  - re-grouped, less comments
;; Nov 14 2015  martin.pos@2lazy.nl  - whitespace-mode, ace mode
;; Nov 15 2015  martin.pos@nxp.com   - defuns, appearance, fixes
;; Nov 19 2015  martin.pos@2lazy.nl  - multiple cursors

;;
;; packages
;;
(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                         ("marmalade" . "https://marmalade-repo.org/packages/")
                         ("melpa" . "http://melpa.org/packages/")))
(package-initialize)
(require 's)

;;
;; .emacs.d setup
;;
;; Write backup files to own directory
(setq backup-directory-alist
      `(("." . ,(expand-file-name
                 (concat user-emacs-directory "backups")))))
;; Keep emacs Custom-settings in separate file
;;(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
;;(load custom-file)
;; Functions (load all files in defuns-dir)
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
(electric-pair-mode 1)
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
(setq cperl-indent-level 1)
(setq sentence-end-double-space nil)
(yas-global-mode 1)
(setq-default fill-column 130)

;;
;; key bindings
;;
(global-set-key (kbd "C-x C-r") 'recentf-open-files)
(global-set-key (kbd "C-=") 'er/expand-region)
(global-set-key (kbd "C-c n") 'cleanup-buffer)
(global-set-key [home] 'smart-beginning-of-line)
(global-set-key (kbd "M-%") 'query-replace-regexp)
(global-set-key (kbd "C-, f") 'copy-file-name-to-clipboard)
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
(global-set-key (kbd "<C-enter>") 'inline-shell-command)
;; jump
(global-set-key (kbd "C--") 'ace-jump-mode)
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
   min (region-beginning)
   max (region-end)
  )
  (setq
   min (point-at-bol)
   max (point-at-eol)
  )
 )
 (setq command (concat (buffer-substring min max) "\n"))
 (insert (concat "\n" (shell-command-to-string command)))
)

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

;; customize
(custom-set-variables
 '(custom-safe-themes
   (quote
    ("4527ad80568d218b57e06ff1cab2e5391ab17e4c3252e74a3ea9d6db2d961db5" "5422b05b20c27caf9fe7a511baa8f3bcbaa3ea823cf54e7105fe759923047a26" default)))
 '(menu-bar-mode nil)
 '(scroll-bar-mode nil)
 '(tool-bar-mode nil)
 '(vhdl-basic-offset 1)
 '(vhdl-beautify-options (quote (t t t t t)))
 '(vhdl-upper-case-keywords t)
 '(vhdl-upper-case-types t))

(custom-set-faces
 )


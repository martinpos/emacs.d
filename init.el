;; init.el : my personal emacs setup
;;
;; Nov  8 2015  martin.pos@2lazy.nl  - .emacs -> init.el
;;

;; package
(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                         ("marmalade" . "https://marmalade-repo.org/packages/")
                         ("melpa" . "http://melpa.org/packages/")))
(package-initialize)
(require 's)

;; generic
(global-hl-line-mode 1)
(electric-pair-mode 1)
(set-face-background 'hl-line "#e0e0e0")
(setq inhibit-splash-screen t)
(transient-mark-mode 0)
(setq column-number-mode t)
(display-time-mode 1)
(windmove-default-keybindings)
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)
;; Write backup files to own directory
(setq backup-directory-alist
      `(("." . ,(expand-file-name
                 (concat user-emacs-directory "backups")))))
;; recentfiles
(recentf-mode 1)
(setq recentf-max-menu-items 25)
(global-set-key (kbd "C-x C-r") 'recentf-open-files)
;; resize window
(global-set-key (kbd "S-C-<left>") 'shrink-window-horizontally)
(global-set-key (kbd "S-C-<right>") 'enlarge-window-horizontally)
(global-set-key (kbd "S-C-<down>") 'shrink-window)
(global-set-key (kbd "S-C-<up>") 'enlarge-window)
;; C-mousewheel scales font size
(global-set-key [C-mouse-4] 'text-scale-increase)
(global-set-key [C-mouse-5] 'text-scale-decrease)
;; default font size
(set-face-attribute 'default nil :height 95)
;; move-text M-up, M-down
(move-text-default-bindings)
;; jump-char
(global-set-key (kbd "M-m") 'jump-char-forward)
(global-set-key (kbd "M-M") 'jump-char-backward)
(global-set-key (kbd "s-m") 'jump-char-backward)
;; Languages
(setq c-basic-offset 1)
(setq cperl-indent-level 1)

;; Org mode
(setq org-src-preserve-indentation t)
(setq org-src-fontify-natively t)
(add-hook 'doc-view-mode-hook 'auto-revert-mode)

;; Keep emacs Custom-settings in separate file
;;(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
;;(load custom-file)

;; YASnippet
(yas-global-mode 1)

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
(global-set-key (kbd "<C-enter>") 'inline-shell-command)

;; joins the following line onto this one
;; from whattheemacsd.com
(global-set-key (kbd "M-j")
            (lambda ()
                  (interactive)
                  (join-line -1)))

;; customize
(custom-set-variables
 '(menu-bar-mode nil)
 '(scroll-bar-mode nil)
 '(tool-bar-mode nil)
 '(vhdl-basic-offset 1)
 '(vhdl-beautify-options (quote (t t t t t)))
 '(vhdl-upper-case-keywords t)
 '(vhdl-upper-case-types t))

;; Functions (load all files in defuns-dir)
(setq defuns-dir (expand-file-name "defuns" user-emacs-directory))
(dolist (file (directory-files defuns-dir t "\\w+"))
  (when (file-regular-p file)
    (load file)))


















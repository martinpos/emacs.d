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
;; Jun 09 2016  martin.pos@nxp.com   - ido settings, virtual buffers
;;                                   - hippie-expand settings
;; Jul 14 2016  martin.pos@nxp.com   - default keybindings for query-replace(-regexp)?
;; Nov 22 2016  martin.pos@nxp.com   - duplicate-line
;; Feb 27 2017  martin.pos@nxp.com   - evil-numbers
;; Jun 22 2017  martin.pos@nxp.com   - underline-text
;; Jun 26 2017  martin.pos@nxp.com   - zap-up-to-char (requires misc)
;;                                   - hippie-expand and ido
;; Jun 28 2017  martin.pos@nxp.com   - cursor-chg
;;                                   - force cperl mode over perl mode
;;                                   - hl-line-face darker
;;                                   - show-paren-mode
;;                                   - windmove on meta key
;; Jun 30 2017  martin.pos@nxp.com   - windmove on S-M-<arrow>
;; Jul 03 2017  martin.pos@nxp.com   - install packages
;; Jul 04 2017  martin.pos@nxp.com   - Buffer List column width
;; Sep 20 2017  martin.pos@nxp.com   - indent-tabs-mode nil
;; Oct 18 2017  martin.pos@nxp.com   - VHDL (removed from custom-set-variables)
;;                                   - vhdl-end-comment-column
;; Jun 22 2018  martin.pos@nxp.com   - advanced isearch
;;                                   - windresize
;; Jun 27 2018  martin.pos@nxp.com   - my-ffap (also open URL at point)
;; Jul 04 2018  martin.pos@nxp.com   - todo-run
;; May 28 2019  martin.pos@nxp.com   - octave-mode (matlab)
;; Jun 06 2019  martin.pos@nxp.com   - desktop-save-mode
;; Oct 16 2019  martin.pos@nxp.com   - todo: f5 -> f6
;;                                   - refresh: f5
;; Nov 14 2019  martin.pos@nxp.com   - whitespace-cleanup before-save
;; Feb 18 2020  martin.pos@nxp.com   - desktop-path, local directory
;; Feb 20 2020  martin.pos@nxp.com   - title (frame-title-format)
;; Feb 24 2020  martin.pos@nxp.com   - insert-file-name: rewrite for personal usage
;; Mar 20 2020  martin.pos@nxp.com   - browse-kill-ring
;; Apr 10 2020  martin.pos@nxp.com   - todo-jump-title
;; Aug 06 2020  martin.pos@nxp.com   - whitespace-mode updated
;;                                   - cleanup colors to avoid conflict with themes
;; Aug 12 2020  martin.pos@nxp.com   - diff-environment
;; Sep 09 2020  martin.pos@nxp.com   - use eww to open links (eww-browse-url))
;; Sep 10 2020  martin.pos@nxp.com   - my-ffap: fix opening file://<path> (addtional to file:////<path>)
;; Sep 23 2020  martin.pos@nxp.com   - smart-mode-line
;; Sep 30 2020  martin.pos@nxp.com   - yank-primary
;; Oct 01 2020  martin.pos@nxp.com   - help-window-select
;; Oct 15 2020  martin.pos@nxp.com   - flyspell-add-word, flyspell-check-next-highlighted-word
;;                                   - flyspell key bindings (F8)
;;



;;
;; packages
;;
(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                         ("marmalade" . "http://marmalade-repo.org/packages/")
                         ("melpa" . "http://melpa.org/packages/")))
(package-initialize)

;; install packages
;; see https://emacs.stackexchange.com/questions/408/synchronize-packages-between-different-machines
;; my-package-list compiled from variable package-activated-list
(setq my-package-list '(ace-jump-mode
                        better-defaults
                        cursor-chg
                        dired+
                        evil-numbers
                        expand-region
                        frame-cmds
                        frame-fns
                        htmlize
                        jump-char
                        linum-relative
                        magit
                        git-commit
                        magit-popup
                        move-text
                        multiple-cursors
                        nlinum
                        s
                        thing-cmds
                        hide-comnt
                        with-editor
                        async
                        wrap-region
                        dash
                        yasnippet
                        windresize)
      )
(mapc #'package-install my-package-list)

;; init packages
(require 's)
(require 'ffap)
(require 'misc)
(require 'cursor-chg)
(require 'windresize)
(require 'browse-kill-ring)
(require 'cl-lib)

;; desktop
(desktop-save-mode 1)
(setq desktop-path '("."))

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
;; proxy experiments for erc - May 30 2016  martin.pos@nxp.com
;;
(defvar http-proxy-host "http://emea.nics.nxp.com")
(defvar http-proxy-port 8080)
(defun open-http-proxy-stream (name buffer host service &rest parameters)
  "Open network stream via http proxy. Proxy is defined by variables http-proxy-host and http-proxy-port."
  (let ((tmp-process (apply 'open-network-stream name buffer http-proxy-host http-proxy-port parameters)))
  (process-send-string name (format "CONNECT %s:%d HTTP/1.1\n\n" host service))
  tmp-process))
(setq erc-server-connect-function 'open-http-proxy-stream)

;;
;; title
;;
;; NB: (substring str 0 -1)  removes trailing \n
(setq prj-name (substring (shell-command-to-string (format "prj -pn")) 0 -1))
(setq frame-title-format '("" prj-name " - %b"))

;;
;; appearance
;;
;; Aug 06 2020  martin.pos@nxp.com   - removed colors (avoid theme conflict)

(global-hl-line-mode 1)
(set-face-attribute 'default nil :height 90)
(setq ibuffer-formats
      '((mark modified read-only " "
              (name 50 50 :left :elide) " "
              (size 9 -1 :right) " "
              (mode 16 16 :left :elide) " " filename-and-process)
        (mark " " (name 16 -1) " " filename)))

;; sml - smart-mode-line
;;
;; Sep 23 2020  martin.pos@nxp.com  - smart-mode-line
(sml/setup)
(setq sml/name-width 70)
(setq sml/mode-width 30)

;;
;; mode
;;
(add-to-list 'auto-mode-alist '("\\.f\\'" . text-mode))
(add-to-list 'auto-mode-alist '("\\.m\\'" . octave-mode))
(setq octave-mode-hook
      (lambda () (progn (setq octave-comment-char ?%)
                        (setq comment-start "%")
                        (setq indent-tabs-mode nil)
                        (setq comment-add 0)
                        (setq tab-width 1)
                        (setq tab-stop-list (number-sequence 2 200 2))
                        (setq octave-block-offset 2)
                        (setq abbrev-mode 1)
                        (setq auto-fill-mode 1)
                        (if (eq window-system 'x)
                            (font-lock-mode 1))
                        (setq octave-block-offset 1)
                        (setq octave-continuation-offset 1))))

;;
;; whitespace-mode
;;
;; Nov 14 2015  martin.pos@2lazy.nl  - creation
;; Aug 06 2020  martin.pos@nxp.com   - removed colors (theme-compatibility)
;;(require 'whitespace)
(progn
 (whitespace-mode -1)
 (global-whitespace-mode -1)
 (setq whitespace-line-column 130)
 (setq whitespace-style '(face empty tabs spaces trailing newline empty))
 (whitespace-mode 1)
 (global-whitespace-mode 1)
)
(add-hook 'before-save-hook 'whitespace-cleanup)

;;
;; VHDL
;;
(setq
 vhdl-basic-offset 1
 vhdl-beautify-options (quote (t t t t t))
 vhdl-upper-case-keywords t
 vhdl-upper-case-types t
 vhdl-end-comment-column 130
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
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)
(setq c-basic-offset 1)
(setq sh-basic-offset 1
      sh-indentation 1)
(setq cperl-indent-level 1
      cperl-close-paren-offset -1
      cperl-continued-statement-offset 1
      cperl-indent-parens-as-block t
      cperl-tab-always-indent t)
(setq cperl-highlight-variables-indiscriminately t)
(fset 'perl-mode 'cperl-mode)
(setq sentence-end-double-space nil)
(yas-global-mode 1)
(setq-default fill-column 130)
(add-hook 'text-mode-hook 'turn-on-auto-fill)
(savehist-mode 1)
(cua-selection-mode t)
(setq cua-auto-tabify-rectangles nil)
(setq cua-keep-region-after-copy t)
(wrap-region-mode t)
(recentf-mode 1)
(setq recentf-max-menu-items 200
      recentf-max-menu-items 50)
(setq-default history-length 5000)
(setq ido-create-new-buffer (quote never)
      ido-enable-flex-matching t
      ido-enable-last-directory-history nil
      ido-enable-regexp nil
      ido-max-directory-size 300000
      ido-max-file-prompt-width 0.1
      ido-use-filename-at-point (quote guess)
      ido-use-url-at-point t
      ido-use-virtual-buffers t)
(ido-mode t)
(toggle-cursor-type-when-idle 1)
(change-cursor-mode 1)
(show-paren-mode 1)
(setq Buffer-menu-name-width 40)
(setq-default indent-tabs-mode nil)
(setq-default tab-width 1)
(setq tcl-indent-level 1)
(setq tcl-continued-indent-level 1)
(tool-bar-mode -1)
(diredp-toggle-find-file-reuse-dir 1)
(setq hippie-expand-try-functions-list '(try-complete-file-name-partially
    try-complete-file-name
    try-expand-all-abbrevs
    try-expand-list
    try-expand-line
    try-expand-dabbrev
    try-expand-dabbrev-all-buffers
    try-expand-dabbrev-from-kill
    try-complete-lisp-symbol-partially
    try-complete-lisp-symbol))
(setq browse-url-browser-function 'eww-browse-url)
(setq help-window-select t)

;;
;; spelling - May 19 2016  martin.pos@nxp.com
;;
;; see https://stackoverflow.com/questions/15891808/emacs-how-to-enable-automatic-spell-check-by-default
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
(global-set-key (kbd "C-x y") 'my-ffap)
(global-set-key (kbd "C-=") 'er/expand-region)
(global-set-key (kbd "C-c n") 'cleanup-buffer)
(global-set-key [home] 'smart-beginning-of-line)
(global-set-key (kbd "C-x M-f") 'insert-file-name)
(global-set-key (kbd "C-x M-F") 'copy-file-name-to-clipboard)
(global-set-key (kbd "M-\"") 'insert-pair)
(global-set-key (kbd "<C-enter>") 'inline-shell-command)
(global-set-key (kbd "<M-enter>") 'filter-by-shell-command)
(global-set-key (kbd "<C-M-enter>") 'insert-shell-command)
(global-set-key (kbd "C-c u") 'underline-text)
(global-set-key (kbd "M-z") 'zap-up-to-char)
(global-set-key (kbd "C-?") 'my-expand-file-name-at-point)
(global-set-key (kbd "<tab>") 'hippie-expand)
(global-set-key (kbd "<backtab>") (lambda () (interactive) (hippie-expand 0)))
(global-set-key (kbd "<C-mouse-1>") 'browse-url-at-point)
(global-unset-key [C-down-mouse-1])
(global-set-key (kbd "C-x g") 'magit-status)
(global-set-key (kbd "S-<insert>") 'yank-primary)
;; browse-kill-ring: M-y
(browse-kill-ring-default-keybindings)
;; to be done
(global-set-key (kbd "C-M-?") 't)
(global-set-key (kbd "M-?")  't)
;; windmove
(global-set-key (kbd "C-x <left>")  'windmove-left)
(global-set-key (kbd "C-x <right>") 'windmove-right)
(global-set-key (kbd "C-x <up>")    'windmove-up)
(global-set-key (kbd "C-x <down>")  'windmove-down)
;; windresize
(global-set-key (kbd "C-x S-<left>")  (lambda () (interactive) (windresize-left) (windresize)))
(global-set-key (kbd "C-x S-<right>") (lambda () (interactive) (windresize-right) (windresize)))
(global-set-key (kbd "C-x S-<up>")    (lambda () (interactive) (windresize-up) (windresize)))
(global-set-key (kbd "C-x S-<down>")    (lambda () (interactive) (windresize-down) (windresize)))
;; C-mousewheel scales font size
(global-set-key [C-mouse-4] 'text-scale-increase)
(global-set-key [C-mouse-5] 'text-scale-decrease)
;; move-text M-up, M-down
(move-text-default-bindings)
;; joins the following line onto this one (whattheemacsd.com)
(global-set-key (kbd "M-j") (lambda () (interactive) (join-line -1)))
(global-set-key (kbd "C-c C-d") 'duplicate-line)
(global-set-key (kbd "C-c d") 'duplicate-line)
;; jump
(global-set-key (kbd "C-x j") 'ace-jump-mode)
(global-set-key (kbd "M-m") 'jump-char-forward)
(global-set-key (kbd "M-M") 'jump-char-backward)
(global-set-key (kbd "s-m") 'jump-char-backward)
;; multiple cursors
(global-unset-key (kbd "C-z"))
(global-set-key (kbd "C-z a") 'mc/mark-all-like-this)
(global-set-key (kbd "C-z A") 'mc/mark-all-in-region)
(global-set-key (kbd "C-z e") 'mc/edit-lines)
(global-set-key (kbd "C-z w") 'mc/mark-all-words-like-this)
(global-set-key (kbd "C-z n") 'mc/mark-next-like-this)
(global-set-key (kbd "C-z N") 'mc/unmark-next-like-this)
(global-set-key (kbd "C-z p") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-z P") 'mc/unmark-previous-like-this)
(global-set-key (kbd "C-z i") 'mc/insert-numbers)
(global-set-key (kbd "C-z q") 'mc/mark-all-in-region-regexp)
(global-set-key (kbd "C-z r") 'set-rectangular-region-anchor)
(global-set-key (kbd "C-z s") 'mc/mark-next-like-this-symbol)
(global-set-key (kbd "C-z <right>") 'set-rectangular-region-anchor)
;; advanced isearch
(global-set-key (kbd "C-S-s") 'isearch-forward-symbol-at-point)
(global-set-key (kbd "C-S-r") (lambda () (interactive) (isearch-forward-symbol-at-point) (isearch-repeat-backward)))
(progn
  ;; set arrow keys in isearch. left/right is backward/forward, up/down is history. press Return to exit
  ;; from: http://ergoemacs.org/emacs/emacs_isearch_by_arrow_keys.html
  (define-key isearch-mode-map (kbd "C-<up>") 'isearch-ring-retreat )
  (define-key isearch-mode-map (kbd "C-<down>") 'isearch-ring-advance )
  (define-key isearch-mode-map (kbd "C-<left>") 'isearch-repeat-backward)
  (define-key isearch-mode-map (kbd "C-<right>") 'isearch-repeat-forward)
  (define-key minibuffer-local-isearch-map (kbd "<left>") 'isearch-reverse-exit-minibuffer)
  (define-key minibuffer-local-isearch-map (kbd "<right>") 'isearch-forward-exit-minibuffer)
  )
;; todo
(global-set-key (kbd "<f6>") 'todo-run)
(global-set-key (kbd "S-<f6>") 'todo-jump)
(global-set-key (kbd "C-<f6>") 'todo-ul)
(global-set-key (kbd "M-<f6>") 'todo-jump-title)
;; refresh, source: https://www.emacswiki.org/emacs/RevertBuffer, edited (messages)
(global-set-key
  (kbd "<f5>")
  (lambda (&optional force-reverting)
    "Interactive call to revert-buffer. Ignoring the auto-save
 file and not requesting for confirmation. When the current buffer
 is modified, the command refuses to revert it, unless you specify
 the optional argument: force-reverting to true."
    (interactive "P")
    (if (not (buffer-modified-p))
        (progn
        (message "Refresh buffer: %s" buffer-file-name)
        (revert-buffer :ignore-auto :noconfirm))
      (if (not force-reverting)
          (message "Not refreshed, modified buffer: %s" buffer-file-name)
        (progn
        (message "Force refresh modifed buffer: %s" buffer-file-name)
        (revert-buffer :ignore-auto :noconfirm))))))

;; evil-numbers
(global-set-key (kbd "<kp-add>") 'evil-numbers/inc-at-pt)
(global-set-key (kbd "<kp-subtract>") 'evil-numbers/dec-at-pt)
;; Org mode
(setq org-src-preserve-indentation t)
(setq org-src-fontify-natively t)
(add-hook 'doc-view-mode-hook 'auto-revert-mode)
;; flyspell (https://www.emacswiki.org/emacs/FlySpell)
(global-set-key (kbd "<f8>") 'ispell-word)
(global-set-key (kbd "C-S-<f8>") 'flyspell-mode)
(global-set-key (kbd "C-M-<f8>") 'flyspell-buffer)
(global-set-key (kbd "C-<f8>") 'flyspell-check-previous-highlighted-word)
(global-set-key (kbd "M-<f8>") 'flyspell-check-next-highlighted-word)

;;
;; Oct 24 2015 martin.pos@2lazy.nl - inline-shell-command
;; Aug 03 2020  martin.pos@nxp.com - local variables
;;
(defun inline-shell-command ()
 "execute region or current line as shell command"
 (interactive)
 (let (start end command)
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
)

;; yank-primary, based on
;;  https://stackoverflow.com/questions/28403647/emacs-25-yank-from-x-windows-primary-clipboard-buffer-with-keyboard
;;
;; Sep 30 2020  martin.pos@nxp.com  - creation
;;
(defun yank-primary ()
  (interactive)
  (insert
   (x-get-selection 'PRIMARY)))

;;
;; Nov 22 2015  martin.pos@nxp.com - filter-by-shell-command
;; Feb 25 2019  martin.pos@nxp.com - improved region handling
;; Aug 03 2020  martin.pos@nxp.com - local variables
;;
;; inspired by: http://www.emacswiki.org/emacs/ExecuteExternalCommand
;;
(defun filter-by-shell-command ()
 "filter the region or current line as shell command"
 (interactive)
 (let* (
   (start (region-beginning))
   (end (region-end))
   (input (concat (buffer-substring start end) "\n"))
   (command (read-shell-command "Shell command: "))
  )
  (goto-char end)
  (shell-command-on-region start end command t t)
  (exchange-point-and-mark)
 )
)

;;
;; Nov 23 2015  martin.pos@nxp.com - insert-shell-command
;; Aug 03 2020  martin.pos@nxp.com - local variable
;;
(defun insert-shell-command ()
 "insert shell command in buffer"
 (interactive)
 (let* (
   (command (read-shell-command "Shell command: "))
  )
  (insert (shell-command-to-string command))
 )
)

;;
;; Dec 03 2015  martin.pos@nxp.com - visit-file-at-point
;;
;; inspired by comment at http://stackoverflow.com/questions/3139970/open-a-file-at-line-with-filenameline-syntax
(defun visit-file-at-point ()
  "visit file under point, without prompt"
  (interactive)
  (find-file(ffap-guesser)))

;;
;; Jun 27 2018  martin.pos@nxp.com - ffap visits files and urls (!), prompt is annoying though
;; Mar 13 2020  martin.pos@nxp.com - substitute-in-file-name, avoids prompt in case of environment variables (e.g. $HOME/.bashrc.user)
;; Sep 10 2020  martin.pos@nxp.com - fix opening file://<path> (addtional to file:////<path>)
;;
;; based on: https://www.reddit.com/r/emacs/comments/676r5b/how_to_stop_findfileatprompting_when_there_is_a
(defun my-ffap (&optional name-input)
 (interactive)
 (let* (
   (name (or name-input (ffap-string-at-point 'file)))
   (name (replace-regexp-in-string "file://\\(\[^/\]\\)" "file:////\\1" name))
   (filename (expand-file-name name))
   (filename (substitute-in-file-name filename))
  )
  (message "my-ffap: name=\"%s\"" name)
  (if (and name filename (file-exists-p filename))
   (find-file filename)
   (find-file-at-point name)
  )
 )
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

;; from http://stackoverflow.com/questions/16764502/insert-filename-using-ido
;; Based on insert-file in Emacs -- ashawley 20080926
(defun insert-file-name (&optional args)
  "Insert buffer filename after point.
  Prefix \\[universal-argument], insert full filename.
  Prefix \\[negative-argument], insert true filename (no symlinks).
  Default only filename, without path"
  (interactive "P")
  (setq filename (buffer-file-name))
  (cond ((eq '- args)
         (insert (file-truename filename)))
        ((not (null args))
         (insert filename))
        (t
         (insert (file-name-nondirectory filename)))))

;; from https://www.emacswiki.org/emacs/RecentFiles#toc8
(defun recentf-interactive-complete ()
  "find a file in the recently open files using ido for completion"
  (interactive)
  (let* ((all-files recentf-list)
         (file-assoc-list (mapcar (lambda (x) (cons (file-name-nondirectory x) x)) all-files))
         (filename-list (remove-duplicates (mapcar 'car file-assoc-list) :test 'string=))
         (ido-make-buffer-list-hook
          (lambda ()
            (setq ido-temp-list filename-list)))
         (filename (ido-read-buffer "Find Recent File: "))
         (result-list (delq nil (mapcar (lambda (x) (if (string= (car x) filename) (cdr x))) file-assoc-list)))
         (result-length (length result-list)))
    (find-file
     (cond
      ((= result-length 0) filename)
      ((= result-length 1) (car result-list))
      ( t
        (let ( (ido-make-buffer-list-hook
                (lambda ()
                  (setq ido-temp-list result-list))))
          (ido-read-buffer (format "%d matches:" result-length))))
      ))))

;; from http://stackoverflow.com/questions/88399/how-do-i-duplicate-a-whole-line-in-emacs
(defun duplicate-line (arg)
  "Duplicate current line, leaving point in lower line."
  (interactive "*p")
  ;; save the point for undo
  (setq buffer-undo-list (cons (point) buffer-undo-list))
  ;; local variables for start and end of line
  (let ((bol (save-excursion (beginning-of-line) (point)))
        eol)
    (save-excursion
      ;; don't use forward-line for this, because you would have
      ;; to check whether you are at the end of the buffer
      (end-of-line)
      (setq eol (point))
      ;; store the line and disable the recording of undo information
      (let ((line (buffer-substring bol eol))
            (buffer-undo-list t)
            (count arg))
        ;; insert the line arg times
        (while (> count 0)
          (newline)         ;; because there is no newline in 'line'
          (insert line)
          (setq count (1- count)))
        )
      ;; create the undo information
      (setq buffer-undo-list (cons (cons eol (point)) buffer-undo-list)))
    ) ; end-of-let
  ;; put the point in the lowest line and return
  (next-line arg)
)

;; from http://www.nicholasvanhorn.com/posts/underline-text-in-emacs.html
(defun underline-text (arg)
  "Inserts a line under the current line, filled with a default
underline character `='. If point had been at the end of the
line, moves point to the beginning of the line directly following
the underlining. It does not underline the line's leading
whitespace, trailing whitespace, or comment symbols. With prefix `C-u'
prompts user for a custom underline character. With prefix `C-u
C-u', does not underline whitespace embedded in the line."

  ; Copyright 2015 Boruch Baum <boruch_baum@gmx.com>, GPL3+ license
  (interactive "p")
  (let* ((original-point (point))
         (underline-char
           (replace-regexp-in-string "[[:cntrl:][:space:]]" "="
           (if (= arg 1)
               "-"
             (char-to-string
           (read-char "What character to underline with?")))))
         (original-point-is-eol
           (when (looking-at "$") t))
         (original-point-is-eob
           (= original-point (point-max))))
    (beginning-of-line)
    (unless
      (when (looking-at "[[:space:]]*$")
        (beginning-of-line 0)
        (when (looking-at "[[:space:]]*$")
          (goto-char original-point)
          (message "nothing to do")))
      (insert
        (buffer-substring (line-beginning-position) (line-end-position))
        "\n")
      (save-restriction
        (narrow-to-region
          (progn
            (goto-char (1- (re-search-forward "[^[:space:]]" nil t)))
            (cond
              ((looking-at ";+")   (match-end 0))
              ((looking-at "#+")   (match-end 0))
              ((looking-at "//+")  (match-end 0))
              ((looking-at "/\\*+") (match-end 0))
              (t (point))))
          (1+ (progn
        (goto-char (line-end-position))
            (re-search-backward "[^[:space:]]" nil t))))
        (untabify (point-min) (point-max))
        (goto-char (point-min))
        (if (= arg 16)
          (while (re-search-forward "[^[:space:]]" nil t)
            (replace-match underline-char nil))
         (re-search-forward "[^[:space:]]" nil t)
         (goto-char (1- (point)))
         (while (re-search-forward "." nil t)
           (replace-match underline-char nil)))
        (widen))
      (if original-point-is-eob
        (goto-char (point-max))
       (if original-point-is-eol
         (goto-char (re-search-forward "^"))
        (goto-char original-point))))))

;; from https://superuser.com/questions/67170/how-do-i-complete-file-paths-in-emacs
(defun my-expand-file-name-at-point ()
  "Use hippie-expand to expand the filename"
  (interactive)
  (let ((hippie-expand-try-functions-list '(try-complete-file-name-partially try-complete-file-name)))
    (call-interactively 'hippie-expand)))

;; from https://www.emacswiki.org/emacs/HippieExpand
(defun my-hippie-expand-completions (&optional hippie-expand-function)
  "Return the full list of possible completions generated by `hippie-expand'.
The optional argument can be generated with `make-hippie-expand-function'."
  (let ((this-command 'my-hippie-expand-completions)
        (last-command last-command)
        (buffer-modified (buffer-modified-p))
        (hippie-expand-function (or hippie-expand-function 'hippie-expand)))
    (flet ((ding)) ; avoid the (ding) when hippie-expand exhausts its options.
      (while (progn
               (funcall hippie-expand-function nil)
               (setq last-command 'my-hippie-expand-completions)
               (not (equal he-num -1)))))
    ;; Evaluating the completions modifies the buffer, however we will finish
    ;; up in the same state that we began.
    (set-buffer-modified-p buffer-modified)
    ;; Provide the options in the order in which they are normally generated.
    (delete he-search-string (reverse he-tried-table))))

(defun my-ido-hippie-expand-with (hippie-expand-function)
  "Offer ido-based completion using the specified hippie-expand function."
  (let* ((options (my-hippie-expand-completions hippie-expand-function))
         (selection (and options
                         (ido-completing-read "Completions: " options))))
    (if selection
        (he-substitute-string selection t)
      (message "No expansion found"))))

(defun my-ido-hippie-expand ()
  "Offer ido-based completion for the word at point."
  (interactive)
  (my-ido-hippie-expand-with 'hippie-expand))

(defun my-ido-hippie-expand-filename ()
      "Offer ido-based completion for the filename at point."
      (interactive)
      (my-ido-hippie-expand-with
       (make-hippie-expand-function '(try-complete-file-name))))

;;
;; TODO
;;
;; Jul 06 2018  martin.pos@nxp.com  - creation
;;
(defun todo-run ()
  "run todo script to generate web-page"
  (interactive)
  (message(shell-command-to-string (format "
 p=$(prj -pn); \
 d=$HOME/public_html/TODO/$p; \
 mkdir -p $d; \
 fi=%s; \
 fo=$HOME/public_html/TODO/$p/index.html; \
 todo=$HOME/projects/BAP3_DIE2/data/aar_tdf8533_manager/aar_tdf8533_manager/bin/todo; \
 msg=$((time $todo -s -l ~/public_html/TODO/$p -u Uncategorized $fi > $fo) 2>&1 | perl -ne 'if (s/^real\\s+/run-todo, time: /) {print}'); \
 chmod -R o+rX $d; \
 echo -n \"$msg\"
" (buffer-file-name)))))

(defun todo-jump ()
  "jmup to latest TODO section"
  (interactive)
  (deactivate-mark)
  (goto-char (point-max))
  (re-search-backward "^TODO:" nil nil))

(defun todo-ul (&optional heading)
  "TODO heading"
  (interactive "P")
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
  (if heading
      (setq command (concat "UL -h" (number-to-string heading)))
    (setq command "UL")
    )
  (goto-char end)
  (shell-command-on-region start end command t t)
  (exchange-point-and-mark)
  )

 (defun todo-jump-title ()
   "jump to latest todo title"
   (interactive)
   (deactivate-mark)
   (let ((point-cur (point)))
     (if (and (boundp 'todo-point-title) (boundp 'todo-point-work) (eq point-cur todo-point-title))
         (goto-char todo-point-work)
       (progn
         (setq todo-point-work point-cur)
         (setq todo-point-title (re-search-backward "^ *title .*\"[^\"]+\" *$" ))))))

;; based on
;;  https://stackoverflow.com/questions/202803/searching-for-marked-selected-text-in-emacs
;;  https://stackoverflow.com/questions/10594208/how-do-i-get-the-region-selection-programmatically-in-emacs-lisp/10595146

;; (defun isearch-region (begin end)
;;   "Use region as the isearch text."
;;   (if (use_region-p)
;;     (let ((region (funcall region-extract-function nil)))
;;       (deactivate-mark)
;;       (isearch-push-state)
;;       (isearch-yank-string region))))
;; (remove-hook 'isearch-mode-hook #'isearch-region)

;; (defun get-search-term ()
;;   (interactive)
;;   (let (
;;         (selection (buffer-substring-no-properties (region-beginning) (region-end))))
;;     (if (= (length selection) 0)
;;         (message "empty string")
;;       (message selection))))
;;
;; (defun get-search-term (begin end)
;;   "message region or \"empty string\" if none highlighted"
;;   (interactive (if (use-region-p)
;;                    (list (region-beginning) (region-end))
;;                  (list nil nil)))
;;   (if (and begin end)
;;       (let ((selection (buffer-substring-no-properties begin end))))
;;       (message "empty string xx")
;;     )
;;          (message "empty string xx")
;;       (message selection))))
;;
;; (defun isearch-selection (start end)
;;   "use selection as search string"
;;   (interactive (if (use-region-p)
;;                    (list (region-beginning) (region-end))
;;                  (list nil nil)))
;;   (let (selection (buffer-substring start end) "\n"))
;;   (setq command (read-shell-command "Shell command: "))
;;   (goto-char end)
;;   (shell-command-on-region start end command t t)
;;   (exchange-point-and-mark)
;;   )

;; flyspell-add-word
;;
;; Oct 15 2020  martin.pos@nxp.com  - creation
;;
;; source: https://stackoverflow.com/questions/22107182/in-emacs-flyspell-mode-how-to-add-new-word-to-dictionary
;;
(defun flyspell-add-word ()
  (interactive)
  (let ((current-location (point))
         (word (flyspell-get-word)))
    (when (consp word)
      (flyspell-do-correct 'save nil (car word) current-location (cadr word) (caddr word) current-location))))

;; flyspell-check-next-highlighted-word
;;
;; Oct 15 2020  martin.pos@nxp.com  - creation
;;
;; source: https://www.emacswiki.org/emacs/FlySpell
;;
(defun flyspell-check-next-highlighted-word ()
  "Custom function to spell check next highlighted word"
  (interactive)
  (flyspell-goto-next-error)
  (ispell-word)
  )

;; diff-environment
;;
;; Aug 12 2020 martin.pos@nxp.com  - creation
;;
;; inspired by: https://gist.github.com/ffevotte/9345586
;;
(require 'cl-lib)
(cl-defun diff-environment (&key quiet shell (setenv t) debug summary command file)
 (let* (
   (env1 (make-hash-table :test 'equal))
   (env2 (make-hash-table :test 'equal))
   (re1 "^\\([^=]+\\)=\\(.*\\)$")
   (re2 "^declare -x \\([^\n=]+\\)\\(?:=\"\\(.*\\)\"\\)?$")
   (str "")
   (pos 0)
   n v1 v2
   (removed 0)
   (changed 0)
   (added 0)
   (unchanged 0)
   (report (not quiet))
  )
  (cl-loop for str in process-environment do
   (when (string-match re1 str '0)
    (puthash (match-string 1 str) (match-string 2 str) env1)
   )
  )
  (if file
   (if (file-readable-p file)
    (with-temp-buffer
     (insert-file-contents file)
     (setq str (buffer-string))
    )
    (insert "\n  " (format "ERROR cannot open file: %s\n" file))
    (cl-return-from diff-environment)
   )
   (if command
    (setq str (shell-command-to-string (concat command "; export | tr -d '\\'")))
    (setq str (shell-command-to-string "export | tr -d '\\'"))
   )
  )
  (while (string-match re2 str pos)
   (setq v (match-string 2 str))
   (setq v (replace-regexp-in-string (rx "\\" (group anything)) "\\1" v))
   (if (equal v nil) (setq v ""))
   (puthash (match-string 1 str) v env2)
   (setq pos (match-end 0))
  )
  (insert "\n")
  (maphash
   (lambda (n v1)
    (let (
      (v2 (gethash n env2))
     )
     (if (equal v2 nil)
      (progn
       (cl-incf removed)
       (if report
        (if shell
         (insert (format "  unset %s\n" n))
         (insert (format "  -%s=\"%s\"\n" n v1))
        )
       )
       (if setenv
        (setenv n nil)
       )
      )
      (if (string= v1 v2)
       (progn
        (cl-incf unchanged)
        (when (eq report 'all)
         (insert (format "   %s=\"%s\"\n" n v1))
        )
       )
       (cl-incf changed)
       (when report
        (if shell
         (insert (format "  declare -x %s=\"%s\"\n" n v2))
         (insert (format "  -%s=\"%s\"\n" n v1))
         (insert (format "  +%s=\"%s\"\n" n v2))
        )
       )
       (if setenv
        (setenv n v2)
       )
      )
     )
    )
   ) env1
  )
  (maphash
   (lambda (n v2)
    (when (not (gethash n env1))
     (cl-incf added)
     (if report
      (if shell
       (insert (format "  declare -x %s=\"%s\"\n" n v2))
       (insert (format "  +%s=%s\n" n v2))
      )
     )
     (if setenv
      (setenv n v2)
     )
    )
   ) env2
  )
  (when summary
   (insert "\n")
   (insert "  " "diff-environment\n")
   (insert "  " "----------------\n")
   (insert "  " (format "%-11s %s\n" "removed:" removed))
   (insert "  " (format "%-11s %s\n" "changed:" changed))
   (insert "  " (format "%-11s %s\n" "added:" added))
   (insert "  " (format "%-11s %s\n" "unchanged:" unchanged))
   (insert "  " (format "%-11s %s\n" "total:" (+ changed unchanged added)))
  )
  (when debug
   (insert "\n  " (make-string 30 ?#) "\n  debug\n  " (make-string 30 ?#) "\n" )
   (insert "  " (format "%s = %s\n" "command" command))
   (insert "  " (format "%s = %s\n" "file" file))
   (insert "  " (format "%s = %s\n" "report" report))
   (insert "  " (format "%s = %s\n" "quiet" quiet))
   (insert "  " (format "%s = %s\n" "setenv" setenv))
   (insert "  " (format "%s = %s\n" "debug" debug))
   (insert "  " (make-string 30 ?#) "\n")
  )
 )
)

;; customize
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(Tool-bar-mode nil)
 '(ansi-color-faces-vector
   [default bold shadow italic underline bold bold-italic bold])
 '(ansi-color-names-vector
   ["#3f3f3f" "#ea3838" "#7fb07f" "#fe8b04" "#62b6ea" "#e353b9" "#1fb3b3" "#d5d2be"])
 '(ansi-term-color-vector
   [unspecified "#FFFFFF" "#d15120" "#5f9411" "#d2ad00" "#6b82a7" "#a66bab" "#6b82a7" "#505050"])
 '(beacon-color "#c82829")
 '(company-quickhelp-color-background "#4F4F4F")
 '(company-quickhelp-color-foreground "#DCDCCC")
 '(custom-enabled-themes (quote (smart-mode-line-dark)))
 '(custom-safe-themes
   (quote
    ("60940e1f2fa3f4e61e7a7ed9bab9c22676aa25f927d5915c8f0fa3a8bf529821" "b9e9ba5aeedcc5ba8be99f1cc9301f6679912910ff92fdf7980929c2fc83ab4d" "84d2f9eeb3f82d619ca4bfffe5f157282f4779732f48a5ac1484d94d5ff5b279" "c74e83f8aa4c78a121b52146eadb792c9facc5b1f02c917e3dbb454fca931223" "a27c00821ccfd5a78b01e4f35dc056706dd9ede09a8b90c6955ae6a390eb1c1e" "3c83b3676d796422704082049fc38b6966bcad960f896669dfc21a7a37a748fa" "24714e2cb4a9d6ec1335de295966906474fdb668429549416ed8636196cb1441" "5e3fc08bcadce4c6785fc49be686a4a82a356db569f55d411258984e952f194a" "a0feb1322de9e26a4d209d1cfa236deaf64662bb604fa513cca6a057ddf0ef64" "ab04c00a7e48ad784b52f34aa6bfa1e80d0c3fcacc50e1189af3651013eb0d58" "04dd0236a367865e591927a3810f178e8d33c372ad5bfef48b5ce90d4b476481" "7356632cebc6a11a87bc5fcffaa49bae528026a78637acd03cae57c091afd9b9" "7153b82e50b6f7452b4519097f880d968a6eaf6f6ef38cc45a144958e553fbc6" "3d4df186126c347e002c8366d32016948068d2e9198c496093a96775cc3b3eaa" "57e3f215bef8784157991c4957965aa31bac935aca011b29d7d8e113a652b693" "d4f8fcc20d4b44bf5796196dbeabec42078c2ddb16dcb6ec145a1c610e0842f3" "afd761c9b0f52ac19764b99d7a4d871fc329f7392dfc6cd29710e8209c691477" "76b4632612953d1a8976d983c4fdf5c3af92d216e2f87ce2b0726a1f37606158" "e7ba99d0f4c93b9c5ca0a3f795c155fa29361927cadb99cfce301caf96055dfd" "9fbb62620cc371ccfb34fa17e8501a5aa53ce6e33b0837381d087d74cf829391" "87de2a48139167bfe19e314996ee0a8d081a6d8803954bafda08857684109b4e" "409e4d689f1e29e5a18f536507e6dc760ee9da76dc56481aaa0696705e6be968" "fd0396fcf5f148f85cd29dc0c2321582df3b55a395a0eb33d636b433bae49bba" "ae88c445c558b7632fc2d72b7d4b8dfb9427ac06aa82faab8d760fff8b8f243c" "f56eb33cd9f1e49c5df0080a3e8a292e83890a61a89bceeaa481a5f183e8e3ef" "b89a4f5916c29a235d0600ad5a0849b1c50fab16c2c518e1d98f0412367e7f97" "06f0b439b62164c6f8f84fdda32b62fb50b6d00e8b01c2208e55543a6337433a" "628278136f88aa1a151bb2d6c8a86bf2b7631fbea5f0f76cba2a0079cd910f7d" "bb08c73af94ee74453c90422485b29e5643b73b05e8de029a6909af6a3fb3f58" "1b8d67b43ff1723960eb5e0cba512a2c7a2ad544ddb2533a90101fd1852b426e" "82d2cac368ccdec2fcc7573f24c3f79654b78bf133096f9b40c20d97ec1d8016" "fa2b58bb98b62c3b8cf3b6f02f058ef7827a8e497125de0254f56e373abee088" "bffa9739ce0752a37d9b1eee78fc00ba159748f50dc328af4be661484848e476" "711efe8b1233f2cf52f338fd7f15ce11c836d0b6240a18fffffc2cbd5bfe61b0" "1623aa627fecd5877246f48199b8e2856647c99c6acdab506173f9bb8b0a41ac" "79278310dd6cacf2d2f491063c4ab8b129fee2a498e4c25912ddaa6c3c5b621e" "e1ef2d5b8091f4953fe17b4ca3dd143d476c106e221d92ded38614266cea3c8b" "7a994c16aa550678846e82edc8c9d6a7d39cc6564baaaacc305a3fdc0bd8725f" "82360e5f96244ce8cc6e765eeebe7788c2c5f3aeb96c1a765629c5c7937c0b5b" "6c3b5f4391572c4176908bb30eddc1718344b8eaff50e162e36f271f6de015ca" "37144b437478e4c235824f0e94afa740ee2c7d16952e69ac3c5ed4352209eefb" "5d09b4ad5649fea40249dd937eaaa8f8a229db1cec9a1a0ef0de3ccf63523014" "7b3d184d2955990e4df1162aeff6bfb4e1c3e822368f0359e15e2974235d9fa8" "54cf3f8314ce89c4d7e20ae52f7ff0739efb458f4326a2ca075bf34bc0b4f499" "7d708f0168f54b90fc91692811263c995bebb9f68b8b7525d0e2200da9bc903c" "c83c095dd01cde64b631fb0fe5980587deec3834dc55144a6e78ff91ebc80b19" "730a87ed3dc2bf318f3ea3626ce21fb054cd3a1471dcd59c81a4071df02cb601" "2cdc13ef8c76a22daa0f46370011f54e79bae00d5736340a5ddfe656a767fddf" "93ed23c504b202cf96ee591138b0012c295338f38046a1f3c14522d4a64d7308" "2f1518e906a8b60fac943d02ad415f1d8b3933a5a7f75e307e6e9a26ef5bf570" "99ea831ca79a916f1bd789de366b639d09811501e8c092c85b2cb7d697777f93" "b5fff23b86b3fd2dd2cc86aa3b27ee91513adaefeaa75adc8af35a45ffb6c499" "3c2f28c6ba2ad7373ea4c43f28fcf2eed14818ec9f0659b1c97d4e89c99e091e" "bf387180109d222aee6bb089db48ed38403a1e330c9ec69fe1f52460a8936b66" "e074be1c799b509f52870ee596a5977b519f6d269455b84ed998666cf6fc802a" "dde8c620311ea241c0b490af8e6f570fdd3b941d7bc209e55cd87884eb733b0e" "5b809c3eae60da2af8a8cfba4e9e04b4d608cb49584cb5998f6e4a1c87c057c4" "71e5acf6053215f553036482f3340a5445aee364fb2e292c70d9175fb0cc8af7" "d74c5485d42ca4b7f3092e50db687600d0e16006d8fa335c69cf4f379dbd0eee" "be9645aaa8c11f76a10bcf36aaf83f54f4587ced1b9b679b55639c87404e2499" "9efb2d10bfb38fe7cd4586afb3e644d082cbcdb7435f3d1e8dd9413cbe5e61fc" "cae81b048b8bccb7308cdcb4a91e085b3c959401e74a0f125e7c5b173b916bf9" "01cf34eca93938925143f402c2e6141f03abb341f27d1c2dba3d50af9357ce70" "5036346b7b232c57f76e8fb72a9c0558174f87760113546d3a9838130f1cdb74" "2899018e19d00bd73c10c4a3859967c57629c58a955a2576d307d9bdfa2fea35" "8d7684de9abb5a770fbfd72a14506d6b4add9a7d30942c6285f020d41d76e0fa" "f7216d3573e1bd2a2b47a2331f368b45e7b5182ddbe396d02b964b1ea5c5dc27" "3df5335c36b40e417fec0392532c1b82b79114a05d5ade62cfe3de63a59bc5c6" "fe00bb593cb7b8c015bb2eafac5bfc82a9b63223fbc2c66eddc75c77ead7c7c1" "57bd93e7dc5fbb5d8d27697185b753f8563fe0db5db245592bab55a8680fdd8c" "76bfa9318742342233d8b0b42e824130b3a50dcc732866ff8e47366aed69de11" "c4bdbbd52c8e07112d1bfd00fee22bf0f25e727e95623ecb20c4fa098b74c1bd" "a3b6a3708c6692674196266aad1cb19188a6da7b4f961e1369a68f06577afa16" "f2927d7d87e8207fa9a0a003c0f222d45c948845de162c885bf6ad2a255babfd" "4bca89c1004e24981c840d3a32755bf859a6910c65b829d9441814000cf6c3d0" "990e24b406787568c592db2b853aa65ecc2dcd08146c0d22293259d400174e37" "dd4db38519d2ad7eb9e2f30bc03fba61a7af49a185edfd44e020aa5345e3dca7" "79df7564981931c32b20391f8b46c70652a3d5bc64ab04b525e0760f3f79efd0" "074b3ab8695f742cc500fa9ace4b34ef7ced2451918d79ef0f596e04f2e1b101" "ac7dbc65ba1b73367d21179534a59a2b55215a22c1ce7e8e3d33e56cb740088d" "ac05beef308540043af99fafe5cbdf036586e9571f69c9ca48fee0605ee44759" "6d5e6704b62c25ef75f4c6ac25240f91f784b0e651e1c62b80ae24c6a67ad627" "eecdab5c7ce8f278a10274a173449d739226ffb3dcffdf27292be2f0a05df3c2" "4527ad80568d218b57e06ff1cab2e5391ab17e4c3252e74a3ea9d6db2d961db5" "5422b05b20c27caf9fe7a511baa8f3bcbaa3ea823cf54e7105fe759923047a26" default)))
 '(diary-entry-marker (quote font-lock-variable-name-face))
 '(emms-mode-line-icon-color "#1fb3b3")
 '(fci-rule-character-color "#d9d9d9")
 '(fci-rule-color "#222222")
 '(flycheck-color-mode-line-face-to-color (quote mode-line-buffer-id))
 '(frame-background-mode (quote light))
 '(gnus-logo-colors (quote ("#528d8d" "#c0c0c0")) t)
 '(gnus-mode-line-image-cache
   (quote
    (image :type xpm :ascent center :data "/* XPM */
static char *gnus-pointer[] = {
/* width height num_colors chars_per_pixel */
\"    18    13        2            1\",
/* colors */
\". c #1fb3b3\",
\"# c None s None\",
/* pixels */
\"##################\",
\"######..##..######\",
\"#####........#####\",
\"#.##.##..##...####\",
\"#...####.###...##.\",
\"#..###.######.....\",
\"#####.########...#\",
\"###########.######\",
\"####.###.#..######\",
\"######..###.######\",
\"###....####.######\",
\"###..######.######\",
\"###########.######\" };")) t)
 '(hl-paren-background-colors (quote ("#2492db" "#95a5a6" nil)))
 '(hl-paren-colors (quote ("#ecf0f1" "#ecf0f1" "#c0392b")))
 '(hl-sexp-background-color "#efebe9")
 '(hl-todo-keyword-faces
   (quote
    (("TODO" . "#dc752f")
     ("NEXT" . "#dc752f")
     ("THEM" . "#2d9574")
     ("PROG" . "#3a81c3")
     ("OKAY" . "#3a81c3")
     ("DONT" . "#f2241f")
     ("FAIL" . "#f2241f")
     ("DONE" . "#42ae2c")
     ("NOTE" . "#b1951d")
     ("KLUDGE" . "#b1951d")
     ("HACK" . "#b1951d")
     ("TEMP" . "#b1951d")
     ("FIXME" . "#dc752f")
     ("XXX+" . "#dc752f")
     ("\\?\\?\\?+" . "#dc752f"))))
 '(jdee-db-active-breakpoint-face-colors (cons "#1B2229" "#51afef"))
 '(jdee-db-requested-breakpoint-face-colors (cons "#1B2229" "#98be65"))
 '(jdee-db-spec-breakpoint-face-colors (cons "#1B2229" "#3f444a"))
 '(menu-bar-mode nil)
 '(nrepl-message-colors
   (quote
    ("#CC9393" "#DFAF8F" "#F0DFAF" "#7F9F7F" "#BFEBBF" "#93E0E3" "#94BFF3" "#DC8CC3")))
 '(objed-cursor-color "#ff6c6b")
 '(package-selected-packages
   (quote
    (smart-mode-line-atom-one-dark-theme smart-mode-line-powerline-theme smart-mode-line gratuitous-dark-theme dracula-theme alect-themes afternoon-theme abyss-theme flatui-theme hybrid-reverse-theme immaterial-theme material-theme matlab-mode twilight-bright-theme twilight-theme cyberpunk-theme doom-themes spacemacs-theme color-theme-sanityinc-tomorrow zenburn-theme browse-kill-ring auto-complete dash async with-editor hide-comnt magit-popup git-commit frame-fns yasnippet wrap-region windresize thing-cmds s nlinum multiple-cursors move-text magit linum-relative jump-char htmlize frame-cmds expand-region evil-numbers direx dired+ cursor-chg better-defaults ace-jump-mode)))
 '(pdf-view-midnight-colors (cons "#c6c6c6" "#282b33"))
 '(rustic-ansi-faces
   ["#282c34" "#ff6c6b" "#98be65" "#ECBE7B" "#51afef" "#c678dd" "#46D9FF" "#bbc2cf"])
 '(scroll-bar-mode nil)
 '(send-mail-function (quote sendmail-send-it))
 '(sml/active-background-color "#34495e")
 '(sml/active-foreground-color "#ecf0f1")
 '(sml/inactive-background-color "#dfe4ea")
 '(sml/inactive-foreground-color "#34495e")
 '(sml/mode-width
   (if
       (eq
        (powerline-current-separator)
        (quote arrow))
       (quote right)
     (quote full)))
 '(sml/pos-id-separator
   (quote
    (""
     (:propertize " " face powerline-active1)
     (:eval
      (propertize " "
                  (quote display)
                  (funcall
                   (intern
                    (format "powerline-%s-%s"
                            (powerline-current-separator)
                            (car powerline-default-separator-dir)))
                   (quote powerline-active1)
                   (quote powerline-active2))))
     (:propertize " " face powerline-active2))))
 '(sml/pos-minor-modes-separator
   (quote
    (""
     (:propertize " " face powerline-active1)
     (:eval
      (propertize " "
                  (quote display)
                  (funcall
                   (intern
                    (format "powerline-%s-%s"
                            (powerline-current-separator)
                            (cdr powerline-default-separator-dir)))
                   (quote powerline-active1)
                   (quote sml/global))))
     (:propertize " " face sml/global))))
 '(sml/pre-id-separator
   (quote
    (""
     (:propertize " " face sml/global)
     (:eval
      (propertize " "
                  (quote display)
                  (funcall
                   (intern
                    (format "powerline-%s-%s"
                            (powerline-current-separator)
                            (car powerline-default-separator-dir)))
                   (quote sml/global)
                   (quote powerline-active1))))
     (:propertize " " face powerline-active1))))
 '(sml/pre-minor-modes-separator
   (quote
    (""
     (:propertize " " face powerline-active2)
     (:eval
      (propertize " "
                  (quote display)
                  (funcall
                   (intern
                    (format "powerline-%s-%s"
                            (powerline-current-separator)
                            (cdr powerline-default-separator-dir)))
                   (quote powerline-active2)
                   (quote powerline-active1))))
     (:propertize " " face powerline-active1))))
 '(sml/pre-modes-separator (propertize " " (quote face) (quote sml/modes)))
 '(sml/theme (quote automatic))
 '(tab-width 1)
 '(tcl-continued-indent-level 1)
 '(tcl-indent-level 1)
 '(vc-annotate-background "#222222")
 '(vc-annotate-color-map
   (quote
    ((20 . "#fa5151")
     (40 . "#ea3838")
     (60 . "#f8ffa0")
     (80 . "#e8e815")
     (100 . "#fe8b04")
     (120 . "#e5c900")
     (140 . "#32cd32")
     (160 . "#8ce096")
     (180 . "#7fb07f")
     (200 . "#3cb370")
     (220 . "#099709")
     (240 . "#2fdbde")
     (260 . "#1fb3b3")
     (280 . "#8cf1f1")
     (300 . "#94bff3")
     (320 . "#62b6ea")
     (340 . "#30a5f5")
     (360 . "#e353b9"))))
 '(vc-annotate-very-old-color "#e353b9")
 '(verilog-auto-lineup (quote ignore))
 '(verilog-case-indent 1)
 '(verilog-cexp-indent 1)
 '(verilog-indent-level 1)
 '(verilog-indent-level-declaration 1)
 '(verilog-indent-level-module 1)
 '(verilog-indent-lists nil)
 '(window-divider-mode nil))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(sml/folder ((t (:inherit sml/global :foreground "gainsboro" :weight normal))))
 '(tcl-escaped-newline ((t nil))))
(put 'dired-find-alternate-file 'disabled nil)

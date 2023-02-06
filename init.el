; init.el : my personal emacs setup
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
;; Oct 18 2020  martin.pos@nxp.com   - todo-jump-end
;; Oct 21 2020  martin.pos@nxp.com   - whitespace-mode add colors
;;                                   - ibuffer instead of list-buffers
;;                                   - macros
;; Feb 18 2021  martin.pos@nxp.com   - todo-jump replaces all todo-jump-<>
;;                                   - ibuffer size-h (human readable)
;;                                   - my-dired-mode-hook
;; Mar 10 2021  martin.pos@nxp.com   - isearch-forward-region, isearch-backward-region
;; Apr 02 2021  martin.pos@nxp.com   - ibuffer filename-env (environment variables)
;; May 04 2021  martin.pos@nxp.com   - mpp-hilite
;; May 09 2021  martin.pos@nxp.com   - centaur-tabs
;; Jul 22 2021  martin.pos@nxp.com   - isearch-forward-region: special todo-search
;; Aug 31 2021  martin.pos@nxp.com   - load-theme, sml/theme
;; Sep 14 2021  martin.pos@nxp.com   - cru-header
;;                                   - mouse-autoselect-window
;; Dec 15 2021  martin.pos@nxp.com   - mpp-hilite, use region or thing-at-point
;; Jan 13 2022  martin.pos@nxp.com   - todo-jump last
;; Mar 12 2022  martin.pos@nxp.com   - override tcl-calculate-indent
;; Mar 22 2022  martin.pos@nxp.com   - find-file-ace-window (dired)
;; Jun 01 2022  martin.pos@nxp.com   - filter-by-perl-command
;; Feb 06 2023  martin.pos@nxp.com   - hungry-delete
;;

;;
;; packages
;;
;;                      ("marmalade" . "http://marmalade-repo.org/packages/")
(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                         ("melpa" . "http://melpa.org/packages/")))
(package-initialize)
(setq package-enable-at-startup nil)

;; install packages
;; see https://emacs.stackexchange.com/questions/408/synchronize-packages-between-different-machines
;; my-package-list compiled from variable package-activated-list
(setq my-package-list '(abyss-theme
ace-jump-mode
ace-window
afternoon-theme
alect-themes
auto-complete
avy better-defaults
browse-kill-ring
color-theme-sanityinc-tomorrow
cursor-chg
cyberpunk-theme
dired+
direx
doom-themes
dracula-theme
evil-numbers
expand-region
flatui-theme
frame-cmds
frame-fns
gratuitous-dark-theme
htmlize
hybrid-reverse-theme
immaterial-theme
jump-char
linum-relative
magit
git-commit
magit-popup
material-theme
matlab-mode
move-text
multiple-cursors
nlinum
popup
s
smart-mode-line
rich-minority
spacemacs-theme
thing-cmds
hide-comnt
transient
twilight-bright-theme
twilight-theme
windresize
with-editor
async
wrap-region
dash
yasnippet
zenburn-theme))

(mapc #'package-install my-package-list)

;; init packages
(require 's)
(require 'ffap)
(require 'misc)
(require 'cursor-chg)
(require 'windresize)
(require 'browse-kill-ring)
(require 'cl-lib)
(require 'ace-window)
;;(require 'centaur-tabs)

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
;; server-start
;;
;;
(server-start)

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

;;
;; centaur-tabs
;;
;; May 09 2021  martin.pos@nxp.com   - creation
;; (centaur-tabs-mode nil)
;; (global-set-key (kbd "C-<prior>")  'centaur-tabs-backward)
;; (global-set-key (kbd "C-<next>") 'centaur-tabs-forward)

;;
;; Feb 18 2021  martin.pos@nxp.com - size-h
;;                                 - auto-mode
;; Apr 02 2021  martin.pos@nxp.com - filename-env
;;
;; Use human readable Size column instead of original one
;; https://www.emacswiki.org/emacs/IbufferMode
(add-hook 'ibuffer-mode-hook (lambda () (ibuffer-auto-mode 1)))
(setq ibuffer-formats '(
 (mark
  (modified)
  (read-only)
  " "
  (name 45 45 :left :elide)
  " "
  (size-h 9 -1 :right)
  " "
  (mode 18 18 :left :elide)
  " "
  (filename-env)
 )
 (mark
  (modified)
  (read-only)
  " "
  (name 45 45 :left :elide)
  " "
  (size-h 9 -1 :right)
  " "
  (mode 18 18 :left :elide)
  " "
  (filename-and-process)
 )
 (mark
  " "
  (name 45 -1)
  " "
  (filename-env)
 )
))
;; ibuffer column size-h
(define-ibuffer-column size-h
 (:name "Size" :inline nil)
 (cond
  ((> (buffer-size) 1e6) (format "%7.1fM" (/ (buffer-size) 1.0e6)))
  ((> (buffer-size) 100e3) (format "%7.0fk" (/ (buffer-size) 1.0e3)))
  ((> (buffer-size) 1e3) (format "%7.1fk" (/ (buffer-size) 1.0e3)))
  (t (format "%8d" (buffer-size)))
 )
)
;; ibuffer column filename-env
(define-ibuffer-column filename-env
 (:name "Filename" :inline nil)
 (if (buffer-file-name buffer)
  (progn
   (setq vars '("WORK" "HOME" "PROJECT_ROOT"))
   (setq str (buffer-file-name buffer))
   (cl-loop for var in vars do
    (setq str (replace-regexp-in-string (regexp-quote (getenv var)) (concat "\$" var) str))
   )
   (setq str str)
  )
  (or dired-directory "")
 )
)

;; Feb 18 2021  martin.pos@nxp.com  - my-dired-mode-hook
;; Mar 12 2021  martin.pos@nxp.com  - details default enabled
;; Oct 27 2022  martin.pos@nxp.com  - dired-display-file
;;
(setq diredp-hide-details-initially-flag nil)
(add-hook 'dired-mode-hook (lambda () (setq truncate-lines t)))

;; Mar 12 2022  martin.pos@nxp.com  - find-file-ace-window
;; from: https://stackoverflow.com/questions/15441961/opening-a-file-from-dired-in-particular-window
(defun find-file-ace-window ()
 "Use ace window to select a window for opening a file from dired."
 (interactive)
 (let ((file (dired-get-file-for-visit)))
  (if (> (length (aw-window-list)) 1)
   (aw-select "" (lambda (window) (aw-switch-to-window window) (find-file file)))
   (find-file-other-window file)
  )
 )
)
(eval-after-load "dired" '(progn
 (define-key dired-mode-map (kbd "M-RET") 'find-file-ace-window)
 (define-key dired-mode-map (kbd "S-RET") 'dired-display-file)
 (define-key dired-mode-map (kbd "RET") 'dired-find-alternate-file)
 (define-key dired-mode-map (kbd "y") 'dired-display-file)
))

;;
;; load themes
;;

;; Feb 15 2021  martin.pos@nxp.com  - creation, custom-theme-load-path
;; from: https://www.emacswiki.org/emacs/CustomThemes
;;
(let ((basedir "~/.emacs.d/themes/"))
   (dolist (f (directory-files basedir))
     (if (and (not (or (equal f ".") (equal f "..")))
              (file-directory-p (concat basedir f)))
         (add-to-list 'custom-theme-load-path (concat basedir f)))))

;; Aug 31 2021  martin.pos@nxp.com  - disable-all-themes
;; from: https://www.greghendershott.com/2017/02/emacs-themes.html
;;
;; NB doesn't work yet when emacs is started
(progn
 (load-theme 'doom-dark+ t)
)

;; sml - smart-mode-line
;;
;; Sep 23 2020  martin.pos@nxp.com  - smart-mode-line
;; Apr 05 2021  martin.pos@nxp.com  - mode-line-format (experimenting still)
(setq sml/name-width 100)
(setq sml/mode-width 'full)
(setq sml/no-confirm-load-theme t)
(setq sml/theme 'mpp)
(sml/setup)
(setq mode-line-format '(
  "%e"
  mode-line-front-space
  mode-line-mule-info
  mode-line-client
  mode-line-modified
  mode-line-remote`
  mode-line-frame-identification
  mode-line-buffer-identification
  sml/pos-id-separator
  mode-line-position
  (vc-mode vc-mode)
  sml/pre-modes-separator
  mode-line-modes
  mode-line-misc-info
  mode-line-end-spaces
))

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
(add-to-list 'auto-mode-alist '("\\.sgdc\\'" . tcl-mode))
(add-to-list 'auto-mode-alist '("\\.sdc\\'" . tcl-mode))

(setq
 lisp-body-indent 1
 )

(setq
 verilog-align-ifelse t
 verilog-auto-delete-trailing-whitespace t
 verilog-auto-inst-param-value t
 verilog-auto-inst-vector nil
 verilog-auto-lineup nil
 verilog-auto-newline nil
 verilog-auto-save-policy nil
 verilog-auto-template-warn-unused t
 verilog-case-indent 1
 verilog-cexp-indent 1
 verilog-highlight-grouping-keywords t
 verilog-highlight-modules t
 verilog-indent-level 1
 verilog-indent-level-behavioral 1
 verilog-indent-level-declaration 1
 verilog-indent-level-module 1
 verilog-tab-to-comment t
)

(add-hook 'makefile-mode-hook
  (lambda ()
    (setq indent-tabs-mode t)
    (setq tab-width 2)))
(add-hook 'makefile-gmake-mode-hook
  (lambda ()
    (setq indent-tabs-mode t)
    (setq tab-width 2)))

;;
;; whitespace-mode
;;
;; Nov 14 2015  martin.pos@2lazy.nl  - creation
;; Aug 06 2020  martin.pos@nxp.com   - removed colors (theme-compatibility)
;; Oct 21 2020  martin.pos@nxp.com   - added colors (struggle)
;;(require 'whitespace)
(progn
 (whitespace-mode -1)
 (global-whitespace-mode -1)
 (setq whitespace-line-column 230)
 (setq whitespace-style '(face empty tabs spaces trailing newline))
 (whitespace-mode 1)
 (global-whitespace-mode 1)
 )
;; colors (dark theme)
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(whitespace-empty ((t (:background "gray20"))))
 '(whitespace-hspace ((t (:foreground "yellow" :background "red"))))
 '(whitespace-indentation ((t (:background "yellow" :foreground "red"))))
 '(whitespace-line ((t (:background "gray30" :foreground "orange"))))
 '(whitespace-newline ((t (:background "gray20" :foreground "gray30"))))
 '(whitespace-space ((t (:background "gray20" :foreground "gray30"))))
 '(whitespace-space-after-tab ((t (:background "yellow" :foreground "red"))))
 '(whitespace-space-before-tab ((t (:background "yellow" :foreground "red"))))
 '(whitespace-tab ((t (:background "orange"))))
 '(whitespace-trailing ((t (:background "gray20")))))

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
;; hungry-delete
;;
(require 'hungry-delete)
;;(global-hungry-delete-mode)
(setq hungry-delete-join-reluctantly t)
(global-set-key (kbd "C-c <backspace>") 'hungry-delete-backward)
(global-set-key (kbd "C-c <deletechar>") 'hungry-delete-forward)

;;
;;  size and position
;;
(when window-system
      (set-frame-position (selected-frame) 0 0)
      (set-frame-size (selected-frame) 130 90))

;; size and position new frames
(add-to-list 'default-frame-alist '(height . 70))
(add-to-list 'default-frame-alist '(width . 130))

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
(setq-default truncate-lines t)
(add-hook 'text-mode-hook 'turn-on-auto-fill)
(savehist-mode 1)
(cua-selection-mode t)
(setq cua-auto-tabify-rectangles nil)
(setq cua-keep-region-after-copy t)
(wrap-region-mode t)
(recentf-mode 1)
(setq recentf-max-menu-items 800)
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
(setq mouse-autoselect-window 0.7)
(setq tcl-indent-level 1
      tcl-continued-indent-level 1)

;; my-tcl-calculate-indent
;;
;; Mar 12 2022  martin.pos@nxp.com  - override tcl-calculate-indent
;;
(advice-add 'tcl-calculate-indent :override #'my-tcl-calculate-indent)
(defun my-tcl-calculate-indent (&optional parse-start)
  "Return appropriate indentation for current line as Tcl code.
In usual case returns an integer: the column to indent to.
Returns nil if line starts inside a string, t if in a comment."
  (save-excursion
    (beginning-of-line)
    (let* ((indent-point (point))
    (case-fold-search nil)
    (continued-line
     (save-excursion
       (if (bobp)
    nil
  (backward-char)
  (= ?\\ (preceding-char)))))
    (continued-indent-value (if continued-line
           tcl-continued-indent-level
         0))
    state
    containing-sexp
    found-next-line)
      (if parse-start
   (goto-char parse-start)
 (beginning-of-defun))
      (while (< (point) indent-point)
 (setq parse-start (point))
 (setq state (parse-partial-sexp (point) indent-point 0))
 (setq containing-sexp (car (cdr state))))
      (cond ((or (nth 3 state) (nth 4 state))
      ;; Inside comment or string.  Return nil or t if should
      ;; not change this line
      (nth 4 state))
     ((null containing-sexp)
      ;; Line is at top level.
      continued-indent-value)
     (t
      ;; Set expr-p if we are looking at the expression part of
      ;; an "if", "expr", etc statement.  Set commands-p if we
      ;; are looking at the body part of an if, while, etc
      ;; statement.  FIXME Should check for "for" loops here.
      (goto-char containing-sexp)
      (let* ((sexpr-type (tcl-figure-type))
      (expr-p (eq sexpr-type 'tcl-expr))
      (commands-p (eq sexpr-type 'tcl-commands))
      (expr-start (point)))
        ;; Find the first statement in the block and indent
        ;; like it.  The first statement in the block might be
        ;; on the same line, so what we do is skip all
        ;; "virtually blank" lines, looking for a non-blank
        ;; one.  A line is virtually blank if it only contains
        ;; a comment and whitespace.  FIXME continued comments
        ;; aren't supported.  They are a wart on Tcl anyway.
        ;; We do it this funky way because we want to know if
        ;; we've found a statement on some line _after_ the
        ;; line holding the sexp opener.
        (goto-char containing-sexp)
        (forward-char)
        (if (and (< (point) indent-point)
   (looking-at "[ \t]*\\(#.*\\)?$"))
     (progn
       (forward-line)
       (while (and (< (point) indent-point)
     (looking-at "[ \t]*\\(#.*\\)?$"))
         (setq found-next-line t)
         (forward-line))))
        (if (or continued-line
         (/= (char-after containing-sexp) ?{)
         expr-p)
     (progn
       ;; Line is continuation line, or the sexp opener
       ;; is not a curly brace, or we are looking at
       ;; an `expr' expression (which must be split
       ;; specially).  So indentation is column of first
       ;; good spot after sexp opener (with some added
       ;; in the continued-line case).  If there is no
       ;; nonempty line before the indentation point, we
       ;; use the column of the character after the sexp
       ;; opener.
       (if (>= (point) indent-point)
    (progn
      (goto-char containing-sexp)
      (forward-char))
         (skip-chars-forward " \t"))
     (- (current-column) 1))
   ;; Mar 12 2022  martin.pos@nxp.com - edit
   ;;(+ (current-column) continued-indent-value))

   ;; After a curly brace, and not a continuation line.
   ;; So take indentation from first good line after
   ;; start of block, unless that line is on the same
   ;; line as the opening brace.  In this case use the
   ;; indentation of the opening brace's line, plus
   ;; another indent step.  If we are in the body part
   ;; of an "if" or "while" then the indentation is
   ;; taken from the line holding the start of the
   ;; statement.
   (if (and (< (point) indent-point)
     found-next-line)
       (current-indentation)
     (if commands-p
         (goto-char expr-start)
       (goto-char containing-sexp))
     (+ (current-indentation) tcl-indent-level)))))))))

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
(global-set-key (kbd "C-x C-b") 'ibuffer)
(global-set-key (kbd "C-x C-d") 'dired-jump)
(global-set-key (kbd "C-x C-r") 'recentf-open-files)
(global-set-key (kbd "C-x y") 'my-ffap)
(global-set-key (kbd "C-=") 'er/expand-region)
(global-set-key (kbd "C-c n") 'cleanup-buffer)
(global-set-key [home] 'smart-beginning-of-line)
(global-set-key (kbd "C-x M-f") 'insert-file-name)
(global-set-key (kbd "C-x M-d") (lambda () (interactive) (insert (shell-command-to-string "me -d"))))
;; doesn't work yet:
;; (global-set-key (kbd "C-x M-m") (lambda (&optional arg) (interactive) (if arg (insert (shell-command-to-string "me -s")) (insert (shell-command-to-string "me"))) ))
(global-set-key (kbd "C-x M-m") (lambda () (interactive) (insert (shell-command-to-string "me"))))
(global-set-key (kbd "C-x M-M") (lambda () (interactive) (insert (shell-command-to-string "me -s"))))
(global-set-key (kbd "C-x M-n") (lambda () (interactive) (insert (shell-command-to-string "me -n"))))
(global-set-key (kbd "C-x M-y") (lambda () (interactive) (insert (shell-command-to-string "title -Y"))))
(global-set-key (kbd "C-x M-h") 'cru-header)
(global-set-key (kbd "C-x M-F") 'copy-file-name-to-clipboard)
(global-set-key (kbd "M-\"") 'insert-pair)
(global-set-key (kbd "<C-enter>") 'inline-shell-command)
(global-set-key (kbd "<M-enter>") 'filter-by-shell-command)
(global-set-key (kbd "<C-M-enter>") 'insert-shell-command)
(global-set-key (kbd "M-p") 'filter-by-perl-command)
(global-set-key (kbd "C-c u") 'underline-text)
(global-set-key (kbd "M-z") 'zap-up-to-char)
(global-set-key (kbd "C-?") 'my-expand-file-name-at-point)
(global-set-key (kbd "<tab>") 'hippie-expand)
(global-set-key (kbd "<backtab>") (lambda () (interactive) (hippie-expand 0)))
(global-set-key (kbd "<C-mouse-1>") 'browse-url-at-point)
(global-unset-key [C-down-mouse-1])
(global-set-key (kbd "C-x g") 'magit-status)
(global-set-key (kbd "S-<insert>") 'yank-primary)
(global-set-key (kbd "C-x n") 'display-line-numbers-mode)
(global-set-key (kbd "C-S-w") 'delete-region)

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
(global-set-key (kbd "C-x S-<down>")  (lambda () (interactive) (windresize-down) (windresize)))
;; C-mousewheel scales font size
(global-set-key [C-mouse-4] 'text-scale-increase)
(global-set-key [C-mouse-5] 'text-scale-decrease)
;; move-text M-up, M-down
(move-text-default-bindings)
;; joins the following line onto this one (whattheemacsd.com)
(global-set-key (kbd "M-j") (lambda () (interactive) (join-line -1)))
(global-set-key (kbd "C-c C-d") 'duplicate-line)
(global-set-key (kbd "C-c d") 'duplicate-line)
;; ace-window
(set-face-attribute 'aw-leading-char-face nil :height 400)
(global-set-key (kbd "C-c w") 'ace-window)
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
(global-set-key (kbd "C-z SPC") 'mc/mark-pop)
(global-set-key (kbd "C-z <right>") 'set-rectangular-region-anchor)
;; advanced isearch
(global-set-key (kbd "C-S-s") 'isearch-forward-region)
(global-set-key (kbd "C-S-r") 'isearch-backward-region)
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
(global-set-key (kbd "C-<f6>") 'todo-ul)
(global-set-key (kbd "S-<f6>") (lambda () (interactive) (todo-jump "todo")))
(global-set-key (kbd "C-S-<f6>") (lambda () (interactive) (todo-jump "last")))
(global-set-key (kbd "M-<f6>") (lambda () (interactive) (todo-jump "title")))
(global-set-key (kbd "M-S-<f6>") (lambda () (interactive) (todo-jump "end")))
(global-set-key (kbd "C-M-<f6>") (lambda () (interactive) (todo-jump "")))
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

;; browse-kill-ring: M-y
(browse-kill-ring-default-keybindings)
(setq
 browse-kill-ring-show-preview nil
 browse-kill-ring-highlight-current-entry t
 browse-kill-ring-separator "======================================"
)

;; autoscroll to end of Messages buffer
;;  https://stackoverflow.com/questions/4682033/in-emacs-can-i-set-up-the-messages-buffer-so-that-it-tails
;;
;; Feb 11 2022  martin.pos@nxp.com  - creation
(defadvice message (after message-tail activate)
  "goto point max after a message"
  (with-current-buffer "*Messages*"
    (goto-char (point-max))
    (let ((windows (get-buffer-window-list (current-buffer) nil t)))
      (while windows
        (set-window-point (car windows) (point-max))
        (setq windows (cdr windows))))))

;; cru-header
;;
;; Sep 14 2021  martin.pos@nxp.com  - todo-header
;;
(defun cru-header (&optional length)
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
  (if length
      (setq command (concat "header -n " (number-to-string length) input))
    (setq command (concat "header " input))
    )
  (goto-char end)
  (insert "\n" (shell-command-to-string command))
  (message "cru-header: %s" command)
  )

;; mpp-hilite
;;
;; May 04 2021  martin.pos@nxp.com  - creation
;; Dec 15 2021  martin.pos@nxp.com  - use region or thing-at-point
;;                                  - color updates
;;
(progn
 ;; faces
 (defface mpp-hilite-red '(
  (((background  dark)) :foreground "#FF0000" :weight bold)
  (((background light)) :foreground "#FF0000" :weight bold))
  "my highlighting face 1"
  :group 'mpp)
 (defface mpp-hilite-green '(
  (((background  dark)) :foreground "#00FF00" :weight bold)
  (((background light)) :foreground "#00FF00" :weight bold))
  "my highlighting face 1"
  :group 'mpp)
 (defface mpp-hilite-light-blue '(
  (((background  dark)) :foreground "#8ADEFF" :weight bold)
  (((background light)) :foreground "#8ADEFF" :weight bold))
  "my highlighting face 1"
  :group 'mpp)
 (defface mpp-hilite-yellow '(
  (((background  dark)) :foreground "#FFFF00" :weight bold)
  (((background light)) :foreground "#FFFF00" :weight bold))
  "my highlighting face 1"
  :group 'mpp)
 (defface mpp-hilite-orange '(
  (((background  dark)) :foreground "#FF8000" :weight bold)
  (((background light)) :foreground "#FF8000" :weight bold))
  "my highlighting face 1"
  :group 'mpp)
 (defface mpp-hilite-light-green '(
  (((background  dark)) :foreground "#CCFFCC" :weight bold)
  (((background light)) :foreground "#CCFFCC" :weight bold))
  "my highlighting face 1"
  :group 'mpp)
 (defface mpp-hilite-light-blue2 '(
  (((background  dark)) :foreground "#99FFFF" :weight bold)
  (((background light)) :foreground "#99FFFF" :weight bold))
  "my highlighting face 1"
  :group 'mpp)
 (defface mpp-hilite-light-yellow '(
  (((background  dark)) :foreground "#FFFFCC" :weight bold)
  (((background light)) :foreground "#FFFFCC" :weight bold))
  "my highlighting face 1"
  :group 'mpp)
 (defface mpp-hilite-magenta '(
  (((background  dark)) :foreground "#FF00FF" :weight bold)
  (((background light)) :foreground "#FF00FF" :weight bold))
  "my highlighting face 1"
  :group 'mpp)
 (defface mpp-hilite-white '(
  (((background  dark)) :foreground "#FFFFFF" :weight bold)
  (((background light)) :foreground "#FFFFFF" :weight bold))
  "my highlighting face 1"
  :group 'mpp)
 (defface mpp-hilite-1 '(
  (((background  dark)) :background "#FFFFFF" :foreground "#FF0000" :weight bold)
  (((background light)) :background "#000000" :foreground "#FF0000" :weight bold))
  "my highlighting face 1"
  :group 'mpp)
 (defface mpp-hilite-2 '(
  (((background  dark)) :background "#FFFFFF" :foreground "#00FF00" :weight bold)
  (((background light)) :background "#000000" :foreground "#00FF00" :weight bold))
  "my highlighting face 2"
  :group 'mpp)
 (defface mpp-hilite-3 '(
  (((background  dark)) :background "#FFFFFF" :foreground "#0000FF" :weight bold)
  (((background light)) :background "#000000" :foreground "#0000FF" :weight bold))
  "my highlighting face 3"
  :group 'mpp)
 (defface mpp-hilite-4 '(
  (((background  dark)) :background "#FFFFFF" :foreground "#FF00FF" :weight bold)
  (((background light)) :background "#000000" :foreground "#FF00FF" :weight bold))
  "my highlighting face 4"
  :group 'mpp)
 (defface mpp-hilite-5 '(
  (((background  dark)) :background "#FFFFFF" :foreground "#000000" :weight bold)
  (((background light)) :background "#000000" :foreground "#FFFFFF" :weight bold))
  "my highlighting face 1"
  :group 'mpp)
 ;; list with faces to use
 (setq
  hi-lock-face-defaults '(
   "mpp-hilite-red"
   "mpp-hilite-green"
   "mpp-hilite-light-blue"
   "mpp-hilite-yellow"
   "mpp-hilite-orange"
   "mpp-hilite-light-green"
   "mpp-hilite-light-blue2"
   "mpp-hilite-light-yellow"
   "mpp-hilite-magenta"
   "mpp-hilite-white"
   "mpp-hilite-1"
   "mpp-hilite-2"
   "mpp-hilite-3"
   "mpp-hilite-4"
   "hi-pink"
   "hi-green"
   "hi-blue"
   "hi-black-b"
   "hi-blue-b"
   "hi-red-b"
   "hi-green-b"
   "hi-black-hb"
   "hi-yellow"
  )
  mpp-hilite-last-index 0
 )
 (defun mpp-hilite (arg)
  "highlight selection or at point when no , prefix selects face, last face when no prefix"
  (interactive "P")
  (if arg (setq mpp-hilite-last-index arg))
  (let* (
    (face-name (nth (- mpp-hilite-last-index 1) hi-lock-face-defaults))
    regexp
   )
   (if (use-region-p)
    (setq regexp (regexp-quote (buffer-substring (region-beginning) (region-end))))
    (setq regexp (concat "\\_<" (thing-at-point 'symbol) "\\_>"))
   )
   (message "mpp-hilite: regexp: %s %s (%d)" regexp face-name mpp-hilite-last-index)
   (unhighlight-regexp regexp)
   (highlight-regexp regexp face-name)
  )
 )
 (defun mpp-unhilite (arg)
  "unhighlight at point, unhighlight all if prefix used"
  (interactive "P")
  (if arg
   (progn
    (message "mpp-unhilite: unhighlight all")
    (unhighlight-regexp t)
   )
   (let* (regexp)
    (if (use-region-p)
     (setq regexp (regexp-quote (buffer-substring (region-beginning) (region-end))))
     (setq regexp (concat "\\_<" (thing-at-point 'symbol) "\\_>"))
    )
    (message "mpp-unhilite: regexp: %s" regexp)
    (unhighlight-regexp regexp)
   )
  )
 )
 (global-set-key (kbd "C-<f1>") 'mpp-hilite)
 (global-set-key (kbd "M-<f1>") 'mpp-unhilite)
)

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
;; Mar 10 2021  martin.pos@nxp.com  - creation
;; Jul 22 2021  martin.pos@nxp.com  - special forward search
;; Oct 18 2022  martin.pos@nxp.com  - special backward search
;;                                  - fix: compare last-command
;;
(defun isearch-forward-region ()
 "search region-text forwards, if no active region: repeat or special search"
 (interactive)
 (let (start end line str)
  (if (use-region-p)
   (progn
    (setq
     start (region-beginning)
     end (region-end)
    )
    (deactivate-mark)
    (isearch-forward nil 1)
    (isearch-yank-string (buffer-substring start end))
   )
   (if (eq last-command 'isearch-forward-region)
    (isearch-repeat-forward)
    ; special search
    (setq line (thing-at-point 'line t))
    (save-match-data
     (and (string-match " *\\(?:[0-9.]+  +\\)?\\(.*?\\)\\(?:  +\\| *$\\)" line)
      (setq str (match-string 1 line))
     )
    )
    (message "special search forwards: %s" str)
    (forward-line)
    (deactivate-mark)
    (isearch-forward nil 1)
    (isearch-yank-string str)
   )
  )
 )
)

(defun isearch-backward-region ()
 "search region-text backwards, if no active region: repeat or special search"
 (interactive)
 (let (start end)
  (if (use-region-p)
   (progn
    (setq
     start (region-beginning)
     end (region-end)
    )
    (deactivate-mark)
    (isearch-backward nil 1)
    (isearch-yank-string (buffer-substring start end))
   )
   (if (eq last-command 'isearch-backward-region)
    (isearch-repeat-backward)
    ; special search
    (setq line (thing-at-point 'line t))
    (save-match-data
     (and (string-match " *\\(?:[0-9.]+  +\\)?\\(.*?\\)\\(?:  +\\| *$\\)" line)
      (setq str (match-string 1 line))
     )
    )
    (message "special search backwards: %s" str)
    (forward-line -1)
    (deactivate-mark)
    (isearch-backward nil 1)
    (isearch-yank-string str)
   )
  )
 )
)

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
;; Jun 01 2022  martin.pos@nxp.com  - creation
;;
(defun filter-by-perl-command ()
 "filter the region or current line as shell command"
 (interactive)
 (let* (
   (start (region-beginning))
   (end (region-end))
   (input (concat (buffer-substring start end) "\n"))
   (perl-command (read-from-minibuffer "Perl command: "))
   (command (concat "perl -pe '" perl-command "'"))
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
;; May 10 2021  martin.pos@nxp.com - prefix argument to open in other window
;; Dec 15 2022  martin.pos@nxp.com - prefix 5 open other frame
;;
;; based on: https://www.reddit.com/r/emacs/comments/676r5b/how_to_stop_findfileatprompting_when_there_is_a
(defun my-ffap (arg)
 (interactive "P")
 (let* (
   (name (ffap-string-at-point 'file))
   (name (replace-regexp-in-string "file://\\(\[^/\]\\)" "file:////\\1" name))
   (filename (expand-file-name name))
   (filename (substitute-in-file-name filename))
  )
  (message "my-ffap: name=\"%s\"" name)
  (message "arg: \"%s\"" arg)
  (if (and name filename (file-exists-p filename))
   (cond
    ((eq arg 5)
     (find-file-other-frame filename))
    (arg
     (find-file-other-window filename))
    (t
     (find-file filename)))
   (cond
    (arg
     (ffap-other-window name))
    (t
     (find-file-at-point name)))
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
;; Feb 18 2021  martin.pos@nxp.com  - todo-jump replaces all todo-jump-<>
;; Jul 23 2021  martin.pos@nxp.com  - todo-jump improved title and end points
;; Jan 13 2022  martin.pos@nxp.com  - todo-jump "last" added
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
 todo=/home/audiopwr/projects/AUDIOPWR/BIN/todo; \
 msg=$((time $todo -l ~/public_html/TODO/$p -u Uncategorized $fi > $fo) 2>&1 | perl -ne 'if (s/^real\\s+/run-todo, time: /) {print}'); \
 chmod -R o+rX $d; \
 echo -n \"$msg\"
" (buffer-file-name)))))

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

(defun todo-jump (where)
  "jump around"
  (interactive "swhere?")
  (deactivate-mark)
  (let ((point-cur (point)))
    (cond
     ((equal where "title")
      (if (and (boundp 'todo-point-title) (boundp 'todo-point-work) (eq point-cur todo-point-title))
        (goto-char todo-point-work)
       (progn
        (setq todo-point-work point-cur)
        (re-search-backward "^ *title .*\"[^\"]+\" *$" )
        (re-search-forward "^-+$" )
        (forward-line)
        (setq todo-point-title (point))
        )))
     ((equal where "end")
      (if (and (boundp 'todo-point-end) (boundp 'todo-point-work) (eq point-cur todo-point-end))
        (goto-char todo-point-work)
       (progn
          (setq todo-point-work point-cur)
          (re-search-forward "^ *title .*\"[^\"]+\" *$" nil t)
          (forward-line -1)
          (setq todo-point-end (point))
          (if (equal todo-point-end nil)
              (progn
                (setq todo-point-end (point-max))
                (goto-char todo-point-end))))))
     ((equal where "todo")
      (if (and (boundp 'todo-point-todo) (boundp 'todo-point-work) (eq point-cur todo-point-todo))
           (goto-char todo-point-work)
         (progn
           (setq todo-point-work point-cur)
           (goto-char (point-max))
           (setq todo-point-todo (re-search-backward "^TODO:" nil nil)))))
     ((equal where "last")
      (if (and (boundp 'todo-point-last) (boundp 'todo-point-work) (eq point-cur todo-point-last))
           (goto-char todo-point-work)
         (progn
           (setq todo-point-work point-cur)
           (goto-char (point-max))
           (setq todo-point-last (re-search-backward "^ *\n\\(?:-\\{80,\\}\n\\)\\{5,\\}" nil nil)))))
     (t
      (if (boundp 'todo-point-work)
          (goto-char todo-point-work))))))

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
;; source: https://www.emacswiki.org/emacs/FlySpellq
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
;; Mar 03 2021 martin.pos@nxp.com  - fix equal v nil
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
   (if (equal v nil) (setq v ""))
   (setq v (replace-regexp-in-string (rx "\\" (group anything)) "\\1" v))
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

;;
;; macros
;;
;; Oct 22 2020  martin.pos@nxp.com  - creation
;;
(fset 'm-tcl-1
   [?\C-\M-n ?\C-  ?\C-\M-p delete ?\C-u ?\C-  backspace])
(fset 'm-head
   (lambda (&optional arg) "Keyboard macro." (interactive "p") (kmacro-exec-ring-item (quote ([134217788 24 114 105 49 up up C-right C-left C-S-right C-S-right C-S-right delete 24 134217828 up up up up up up up C-right C-right C-right right home C-right C-right C-right right right 11 C-M-kp-enter 99 105 110 102 111 32 48 backspace 45 99 return home up up up up C-right C-right C-left 11 24 134217828 home up up up C-right C-right C-left 11 24 134217830 home 134217790] 0 "%d")) arg)))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default default default italic underline success warning error])
 '(ansi-term-color-vector
   [unspecified "#FFFFFF" "#d15120" "#5f9411" "#d2ad00" "#6b82a7" "#a66bab" "#6b82a7" "#505050"])
 '(beacon-color "#cc6666")
 '(company-quickhelp-color-background "#4F4F4F")
 '(company-quickhelp-color-foreground "#DCDCCC")
 '(custom-safe-themes
   (quote
    ("29d4d1527281b9aa07608d62556d9946a9e47d6e276d08493431bdef55ef010c" "08780b0f3811cc72df9d0be95c2ae7cf5a0f89991f75cbc331f901689e8e0b55" "e29a6c66d4c383dbda21f48effe83a1c2a1058a17ac506d60889aba36685ed94" "074b3ab8695f742cc500fa9ace4b34ef7ced2451918d79ef0f596e04f2e1b101" "eecdab5c7ce8f278a10274a173449d739226ffb3dcffdf27292be2f0a05df3c2" "79df7564981931c32b20391f8b46c70652a3d5bc64ab04b525e0760f3f79efd0" "f56eb33cd9f1e49c5df0080a3e8a292e83890a61a89bceeaa481a5f183e8e3ef" "e7ba99d0f4c93b9c5ca0a3f795c155fa29361927cadb99cfce301caf96055dfd" "76b4632612953d1a8976d983c4fdf5c3af92d216e2f87ce2b0726a1f37606158" "fa2b58bb98b62c3b8cf3b6f02f058ef7827a8e497125de0254f56e373abee088" "bffa9739ce0752a37d9b1eee78fc00ba159748f50dc328af4be661484848e476" "c74e83f8aa4c78a121b52146eadb792c9facc5b1f02c917e3dbb454fca931223" "3c83b3676d796422704082049fc38b6966bcad960f896669dfc21a7a37a748fa" "84d2f9eeb3f82d619ca4bfffe5f157282f4779732f48a5ac1484d94d5ff5b279" "b9e9ba5aeedcc5ba8be99f1cc9301f6679912910ff92fdf7980929c2fc83ab4d" "60940e1f2fa3f4e61e7a7ed9bab9c22676aa25f927d5915c8f0fa3a8bf529821" "d4f8fcc20d4b44bf5796196dbeabec42078c2ddb16dcb6ec145a1c610e0842f3" "afd761c9b0f52ac19764b99d7a4d871fc329f7392dfc6cd29710e8209c691477" "9fbb62620cc371ccfb34fa17e8501a5aa53ce6e33b0837381d087d74cf829391" "87de2a48139167bfe19e314996ee0a8d081a6d8803954bafda08857684109b4e" "409e4d689f1e29e5a18f536507e6dc760ee9da76dc56481aaa0696705e6be968" "fd0396fcf5f148f85cd29dc0c2321582df3b55a395a0eb33d636b433bae49bba" "ae88c445c558b7632fc2d72b7d4b8dfb9427ac06aa82faab8d760fff8b8f243c" "24714e2cb4a9d6ec1335de295966906474fdb668429549416ed8636196cb1441" "711efe8b1233f2cf52f338fd7f15ce11c836d0b6240a18fffffc2cbd5bfe61b0" "1623aa627fecd5877246f48199b8e2856647c99c6acdab506173f9bb8b0a41ac" "79278310dd6cacf2d2f491063c4ab8b129fee2a498e4c25912ddaa6c3c5b621e" "e1ef2d5b8091f4953fe17b4ca3dd143d476c106e221d92ded38614266cea3c8b" "7a994c16aa550678846e82edc8c9d6a7d39cc6564baaaacc305a3fdc0bd8725f" "82360e5f96244ce8cc6e765eeebe7788c2c5f3aeb96c1a765629c5c7937c0b5b" "6c3b5f4391572c4176908bb30eddc1718344b8eaff50e162e36f271f6de015ca" "37144b437478e4c235824f0e94afa740ee2c7d16952e69ac3c5ed4352209eefb" "5d09b4ad5649fea40249dd937eaaa8f8a229db1cec9a1a0ef0de3ccf63523014" "7b3d184d2955990e4df1162aeff6bfb4e1c3e822368f0359e15e2974235d9fa8" "54cf3f8314ce89c4d7e20ae52f7ff0739efb458f4326a2ca075bf34bc0b4f499" "7d708f0168f54b90fc91692811263c995bebb9f68b8b7525d0e2200da9bc903c" "c83c095dd01cde64b631fb0fe5980587deec3834dc55144a6e78ff91ebc80b19" "730a87ed3dc2bf318f3ea3626ce21fb054cd3a1471dcd59c81a4071df02cb601" "2cdc13ef8c76a22daa0f46370011f54e79bae00d5736340a5ddfe656a767fddf" "93ed23c504b202cf96ee591138b0012c295338f38046a1f3c14522d4a64d7308" "2f1518e906a8b60fac943d02ad415f1d8b3933a5a7f75e307e6e9a26ef5bf570" "99ea831ca79a916f1bd789de366b639d09811501e8c092c85b2cb7d697777f93" "b5fff23b86b3fd2dd2cc86aa3b27ee91513adaefeaa75adc8af35a45ffb6c499" "3c2f28c6ba2ad7373ea4c43f28fcf2eed14818ec9f0659b1c97d4e89c99e091e" "bf387180109d222aee6bb089db48ed38403a1e330c9ec69fe1f52460a8936b66" "e074be1c799b509f52870ee596a5977b519f6d269455b84ed998666cf6fc802a" "dde8c620311ea241c0b490af8e6f570fdd3b941d7bc209e55cd87884eb733b0e" "5b809c3eae60da2af8a8cfba4e9e04b4d608cb49584cb5998f6e4a1c87c057c4" "71e5acf6053215f553036482f3340a5445aee364fb2e292c70d9175fb0cc8af7" "d74c5485d42ca4b7f3092e50db687600d0e16006d8fa335c69cf4f379dbd0eee" "be9645aaa8c11f76a10bcf36aaf83f54f4587ced1b9b679b55639c87404e2499" "cae81b048b8bccb7308cdcb4a91e085b3c959401e74a0f125e7c5b173b916bf9" "01cf34eca93938925143f402c2e6141f03abb341f27d1c2dba3d50af9357ce70" "5036346b7b232c57f76e8fb72a9c0558174f87760113546d3a9838130f1cdb74" "2899018e19d00bd73c10c4a3859967c57629c58a955a2576d307d9bdfa2fea35" "8d7684de9abb5a770fbfd72a14506d6b4add9a7d30942c6285f020d41d76e0fa" "f7216d3573e1bd2a2b47a2331f368b45e7b5182ddbe396d02b964b1ea5c5dc27" "fe00bb593cb7b8c015bb2eafac5bfc82a9b63223fbc2c66eddc75c77ead7c7c1" "57bd93e7dc5fbb5d8d27697185b753f8563fe0db5db245592bab55a8680fdd8c" "76bfa9318742342233d8b0b42e824130b3a50dcc732866ff8e47366aed69de11" "c4bdbbd52c8e07112d1bfd00fee22bf0f25e727e95623ecb20c4fa098b74c1bd" "a3b6a3708c6692674196266aad1cb19188a6da7b4f961e1369a68f06577afa16" "f2927d7d87e8207fa9a0a003c0f222d45c948845de162c885bf6ad2a255babfd" "4bca89c1004e24981c840d3a32755bf859a6910c65b829d9441814000cf6c3d0" "990e24b406787568c592db2b853aa65ecc2dcd08146c0d22293259d400174e37" "b89a4f5916c29a235d0600ad5a0849b1c50fab16c2c518e1d98f0412367e7f97" "06f0b439b62164c6f8f84fdda32b62fb50b6d00e8b01c2208e55543a6337433a" "628278136f88aa1a151bb2d6c8a86bf2b7631fbea5f0f76cba2a0079cd910f7d" "bb08c73af94ee74453c90422485b29e5643b73b05e8de029a6909af6a3fb3f58" "1b8d67b43ff1723960eb5e0cba512a2c7a2ad544ddb2533a90101fd1852b426e" "82d2cac368ccdec2fcc7573f24c3f79654b78bf133096f9b40c20d97ec1d8016" "7153b82e50b6f7452b4519097f880d968a6eaf6f6ef38cc45a144958e553fbc6" "5e3fc08bcadce4c6785fc49be686a4a82a356db569f55d411258984e952f194a" "a0feb1322de9e26a4d209d1cfa236deaf64662bb604fa513cca6a057ddf0ef64" "ab04c00a7e48ad784b52f34aa6bfa1e80d0c3fcacc50e1189af3651013eb0d58" "04dd0236a367865e591927a3810f178e8d33c372ad5bfef48b5ce90d4b476481" "7356632cebc6a11a87bc5fcffaa49bae528026a78637acd03cae57c091afd9b9" "57e3f215bef8784157991c4957965aa31bac935aca011b29d7d8e113a652b693" "3d4df186126c347e002c8366d32016948068d2e9198c496093a96775cc3b3eaa" "9efb2d10bfb38fe7cd4586afb3e644d082cbcdb7435f3d1e8dd9413cbe5e61fc" "3df5335c36b40e417fec0392532c1b82b79114a05d5ade62cfe3de63a59bc5c6" "a27c00821ccfd5a78b01e4f35dc056706dd9ede09a8b90c6955ae6a390eb1c1e" default)))
 '(diary-entry-marker (quote font-lock-variable-name-face))
 '(emms-mode-line-icon-color "#358d8d")
 '(fci-rule-character-color "#d9d9d9")
 '(flycheck-color-mode-line-face-to-color (quote mode-line-buffer-id))
 '(frame-background-mode (quote dark))
 '(gnus-logo-colors (quote ("#259ea2" "#adadad")) t)
 '(gnus-mode-line-image-cache
   (quote
    (image :type xpm :ascent center :data "/* XPM */
static char *gnus-pointer[] = {
/* width height num_colors chars_per_pixel */
\"    18    13        2            1\",
/* colors */
\". c #358d8d\",
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
 '(hl-sexp-background-color "#1c1f26")
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
 '(jdee-db-active-breakpoint-face-colors (cons "#10151C" "#5EC4FF"))
 '(jdee-db-requested-breakpoint-face-colors (cons "#10151C" "#8BD49C"))
 '(jdee-db-spec-breakpoint-face-colors (cons "#10151C" "#41505E"))
 '(menu-bar-mode nil)
 '(nrepl-message-colors
   (quote
    ("#CC9393" "#DFAF8F" "#F0DFAF" "#7F9F7F" "#BFEBBF" "#93E0E3" "#94BFF3" "#DC8CC3")))
 '(objed-cursor-color "#D95468")
 '(package-selected-packages
   (quote
    (hungry-delete aggressive-indent ace-window dash async with-editor hide-comnt git-commit frame-fns zenburn-theme yasnippet wrap-region windresize twilight-theme twilight-bright-theme thing-cmds spacemacs-theme s nlinum multiple-cursors move-text matlab-mode material-theme magit-popup magit linum-relative jump-char immaterial-theme hybrid-reverse-theme htmlize gratuitous-dark-theme frame-cmds flatui-theme expand-region evil-numbers dracula-theme doom-themes direx dired+ cyberpunk-theme cursor-chg color-theme-sanityinc-tomorrow browse-kill-ring better-defaults auto-complete alect-themes afternoon-theme ace-jump-mode abyss-theme)))
 '(pdf-view-midnight-colors (cons "#A0B3C5" "#1D252C"))
 '(rustic-ansi-faces
   ["#1D252C" "#D95468" "#8BD49C" "#EBBF83" "#5EC4FF" "#E27E8D" "#70E1E8" "#A0B3C5"])
 '(scroll-bar-mode nil)
 '(send-mail-function (quote sendmail-send-it))
 '(vc-annotate-background "#1D252C")
 '(vc-annotate-color-map
   (list
    (cons 20 "#8BD49C")
    (cons 40 "#abcd93")
    (cons 60 "#cbc68b")
    (cons 80 "#EBBF83")
    (cons 100 "#e5ae6f")
    (cons 120 "#df9e5b")
    (cons 140 "#D98E48")
    (cons 160 "#dc885f")
    (cons 180 "#df8376")
    (cons 200 "#E27E8D")
    (cons 220 "#df7080")
    (cons 240 "#dc6274")
    (cons 260 "#D95468")
    (cons 280 "#b35365")
    (cons 300 "#8d5163")
    (cons 320 "#675160")
    (cons 340 "#56697A")
    (cons 360 "#56697A")))
 '(vc-annotate-very-old-color nil)
 '(window-divider-mode nil))

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
;;                                   - hippie-exand settings
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
                        yasnippet)
      )
(mapc #'package-install my-package-list)

;; init packages
(require 's)
(require 'ffap)
(require 'misc)
(require 'cursor-chg)

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
;; appearance
;;
(set-background-color "gray95")
(global-hl-line-mode 1)
(set-face-background hl-line-face "gray80")
(set-face-attribute 'default nil :height 90)
(setq ibuffer-formats
      '((mark modified read-only " "
              (name 50 50 :left :elide) " "
              (size 9 -1 :right) " "
              (mode 16 16 :left :elide) " " filename-and-process)
        (mark " " (name 16 -1) " " filename)))
(add-to-list 'default-frame-alist '(background-color . "gray95"))
(add-to-list 'default-frame-alist '(foreground-color . "black"))
(add-to-list 'auto-mode-alist '("\\.f\\'" . text-mode))

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
(tool-bar-mode -1)
(diredp-toggle-find-file-reuse-dir 1)

;;(setq hippie-expand-try-functions-list '(try-expand-dabbrev
;;                                         try-expand-dabbrev-all-buffers
;;                                         try-expand-dabbrev-from-kill
;;                                         try-complete-file-name-partially
;;                                         try-complete-file-name
;;                                         try-expand-all-abbrevs
;;                                         try-expand-list
;;                                         try-expand-line
;;                                         try-complete-lisp-symbol-partially
;;                                         try-complete-lisp-symbol))

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
  (define-key isearch-mode-map (kbd "<up>") 'isearch-ring-retreat )
  (define-key isearch-mode-map (kbd "<down>") 'isearch-ring-advance )

  (define-key isearch-mode-map (kbd "<left>") 'isearch-repeat-backward)
  (define-key isearch-mode-map (kbd "<right>") 'isearch-repeat-forward)

  (define-key minibuffer-local-isearch-map (kbd "<left>") 'isearch-reverse-exit-minibuffer)
  (define-key minibuffer-local-isearch-map (kbd "<right>") 'isearch-forward-exit-minibuffer)
  )

;; evil-numbers
(global-set-key (kbd "<kp-add>") 'evil-numbers/inc-at-pt)
(global-set-key (kbd "<kp-subtract>") 'evil-numbers/dec-at-pt)
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
  "filter the region or current line as shell command"
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

;;
;; Jun 27 2018  martin.pos@nxp.com - ffap visits files and urls (!), prompt is annoying though
;;
;; from https://www.reddit.com/r/emacs/comments/676r5b/how_to_stop_findfileatprompting_when_there_is_a
(defun my-ffap (&optional filename)
  (interactive)
  (let* ((name (or filename (ffap-string-at-point 'file)))
         (fname (expand-file-name name)))
    (if (and name fname (file-exists-p fname))
        (find-file fname)
      (find-file-at-point filename))))

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

;; from https://www.emacswiki.org/emacs/RecentFiles#toc8
(defun recentf-interactive-complete ()
  "find a file in the recently open file using ido for completion"
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



;; customize
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(Tool-bar-mode nil)
 '(custom-safe-themes
   (quote
    ("4527ad80568d218b57e06ff1cab2e5391ab17e4c3252e74a3ea9d6db2d961db5" "5422b05b20c27caf9fe7a511baa8f3bcbaa3ea823cf54e7105fe759923047a26" default)))
 '(menu-bar-mode nil)
 '(package-selected-packages
   (quote
    (windresize direx dash async with-editor hide-comnt magit-popup git-commit frame-fns cursor-chg yasnippet wrap-region thing-cmds s nlinum multiple-cursors move-text magit linum-relative jump-char htmlize frame-cmds expand-region evil-numbers dired+ better-defaults ace-jump-mode)))
 '(scroll-bar-mode nil)
 '(send-mail-function (quote sendmail-send-it))
 '(verilog-auto-lineup (quote ignore))
 '(verilog-case-indent 1)
 '(verilog-cexp-indent 1)
 '(verilog-indent-level 1)
 '(verilog-indent-level-declaration 1)
 '(verilog-indent-level-module 1)
 '(verilog-indent-lists nil))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
(put 'dired-find-alternate-file 'disabled nil)

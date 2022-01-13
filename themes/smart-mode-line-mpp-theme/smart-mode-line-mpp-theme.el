;;; smart-mode-line-mpp-theme.el --- my personal theme for smart-mode-line

;; Copyright (C) 2021 Martin Pos

;; Author: Martin Pos <martin.pos@nxp.com>
;; Maintainer: Martin Pos <martin.pos@nxp.com>
;; Version: 0.0
;; Keywords: mode-line themes faces

;;; Commentary:
;;
;;  mpp theme for smart-mode-line

;;; License:
;;
;; This file is NOT part of GNU Emacs.
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 3
;; of the License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;

;;; Change Log:
;; 0.0 - 2021/02/15 - creation
;;; Code:

(deftheme smart-mode-line-mpp
  "mpp theme for smart-mode-line.")

(custom-theme-set-faces
 'smart-mode-line-mpp
 '(mode-line     ((t :background "#000000" :foreground "#FFFFFF" :box (:line-width 3 :color "#FF0000"))))
 '(mode-line-inactive ((t :background "#000000")))
 '(sml/global    ((t :inherit font-lock-preprocessor-face :foreground "#A0A0A0")))
 '(sml/filename  ((t :inherit mode-line-buffer-id :foreground "#FFFFFF")))
 '(sml/prefix    ((t :inherit (font-lock-variable-name-face sml/global) :foreground "yellow")))
 '(sml/read-only ((t :background "red" :foreground "white" :weight bold)))
 '(sml/not-modified ((t :background "green" :foreground "white" :weight bold)))
 '(sml/modes     ((t :foreground nil :inherit sml/filename :weight normal)))
 '(sml/time      ((t :inherit mode-line-buffer-id :foreground "#FFFFFF")))
 ;; Helm
 '(helm-candidate-number ((t :background "#2C323C"))))

;;;###autoload
(when load-file-name
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory (file-name-directory load-file-name))))

(provide-theme 'smart-mode-line-mpp)
;;; smart-mode-line-mpp-theme.el ends here.

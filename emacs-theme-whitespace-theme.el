(deftheme emacs-theme-whitespace
  "Created 2020-08-06.")

(custom-theme-set-faces
 'emacs-theme-whitespace
 '(whitespace-space ((t (:foreground "grey80" :background "grey95"))))
 '(whitespace-tab ((t (:foreground "grey90" :background "grey75"))))
 '(whitespace-indentation ((t (:foreground "grey80" :background "lightgreen"))))
 '(whitespace-hspace ((t (:foreground "grey80" :background "grey95"))))
 '(whitespace-trailing ((t (:foreground "grey80" :background "grey95"))))
 '(whitespace-line ((t (:foreground "black" :background "grey85"))))
 '(whitespace-space-before-tab ((t (:foreground "grey90" :background "grey75"))))
 '(whitespace-space-after-tab ((t (:foreground "grey90" :background "grey75"))))
 '(whitespace-empty ((t (:foreground "grey90" :background "grey75")))))

(provide-theme 'emacs-theme-whitespace)

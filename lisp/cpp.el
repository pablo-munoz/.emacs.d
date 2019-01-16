(use-package rtags
  :ensure t
  :if (executable-find "clang")
  :config
  (setq rtags-autostart-diagnostics t)
  (rtags-enable-standard-keybindings))

(use-package company-rtags
  :after (rtags company)
  :config
  (setq rtags-completions-enabled t)
  (setq company-backends (delete 'company-semantic company-backends))
  (eval-after-load 'company
    '(add-to-list
      'company-backends 'company-rtags)))
(setq rtags-autostart-diagnostics t)
(rtags-enable-standard-keybindings)

(defun my-irony-mode-hook ()
  (define-key irony-mode-map [remap completion-at-point]
    'irony-completion-at-point-async)
  (define-key irony-mode-map [remap complete-symbol]
    'irony-completion-at-point-async))

(use-package irony
  :ensure t
  :config
  (progn
    (use-package company-irony
      :ensure t
      :config
      (add-to-list 'company-backends 'company-irony))
    (setq company-backends (delete 'company-semantic company-backends))
    (add-hook 'irony-mode-hook 'electric-pair-mode)
    (add-hook 'c++-mode-hook 'irony-mode)
    (add-hook 'c-mode-hook 'irony-mode)
    (add-hook 'objc-mode-hook 'irony-mode)
    (add-hook 'irony-mode-hook 'my-irony-mode-hook)
    (add-hook 'irony-mode-hook 'company-irony-setup-begin-commands)
    (add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options)))

(use-package flycheck-irony
  :ensure t)

(use-package company-irony-c-headers
  :ensure t
  :config
  '(add-to-list 'company-backends '(company-irony-c-headers company-irony)))

(eval-after-load 'company
  '(add-to-list
    'company-backends 'company-irony))

(setq company-idle-delay 0)
(define-key c-mode-map [(tab)] 'company-complete)
(define-key c++-mode-map [(tab)] 'company-complete)

(eval-after-load 'company
  '(add-to-list
    'company-backends 'company-irony))

(eval-after-load 'company
  '(add-to-list
    'company-backends '(company-irony-c-headers company-irony)))

(add-hook 'c++-mode-hook 'flycheck-mode)
(add-hook 'c-mode-hook 'flycheck-mode)

(require 'flycheck-rtags)

(defun my-flycheck-rtags-setup ()
  (flycheck-select-checker 'rtags)
  (setq-local flycheck-highlighting-mode nil) ;; RTags creates more accurate overlays.
  (setq-local flycheck-check-syntax-automatically nil))
;; c-mode-common-hook is also called by c++-mode
(add-hook 'c-mode-common-hook #'my-flycheck-rtags-setup)

(eval-after-load 'flycheck
  '(add-hook 'flycheck-mode-hook #'flycheck-irony-setup))

(use-package flycheck-clang-tidy
  :ensure t
  :config
  (eval-after-load 'flycheck
  '(add-hook 'flycheck-mode-hook #'flycheck-clang-tidy-setup)))

(cmake-ide-setup)

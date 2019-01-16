(require 'package)
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'package-archives
	     '("org" . "http://orgmode.org/elpa/"))
(package-initialize)

(defalias 'yes-or-no-p 'y-or-n-p)

(tool-bar-mode -1)
(scroll-bar-mode -1)
(setq inhibit-splash-screen t)

;; Start maximized
(toggle-frame-maximized)

;; Gpg password prompt in minibuffer
(setq epa-pinentry-mode 'loopback)

;; Line numbers
(when (version<= "26.0.50" emacs-version )
  (global-display-line-numbers-mode))

;; Transparency adjustment
(set-frame-parameter (selected-frame)'alpha '(90 . 90))
(add-to-list 'default-frame-alist'(alpha . (90 . 90)))

;; OS dependent configuration
(when (eq system-type 'darwin) ;; mac specific settings
  (setq mac-option-modifier 'alt)
  (setq mac-command-modifier 'meta)
  (global-set-key [kp-delete] 'delete-char) ;; sets fn-delete to be right-delete
  )

(set-language-environment "UTF-8")
(set-default-coding-systems 'utf-8-unix)

;; evil: Emulation of Vim in emacs
(use-package evil
  :ensure t ;; install evil package if not installed
  :init      ;; tweak evil's configuration BEFORE loading
  :config    ;; tweak evil AFTER loading
  (evil-mode)

  ;; example how to map a command in normal mode (called 'normal state' in evil)
  (define-key evil-normal-state-map (kbd ", w") 'evil-window-vsplit)
  (evil-define-key 'normal org-mode-map "<tab>" 'org-cycle)
  (evil-define-key 'normal org-mode-map "<TAB>" 'org-cycle)
  (evil-define-key 'normal 'global "gd" 'rtags-find-symbol-at-point)
  )


(use-package company
  :ensure t
  :config
  (add-hook 'after-init-hook 'global-company-mode)
  )

(use-package flycheck
  :ensure t
  :custom
  (flycheck-global-modes '(not org-mode))
  :config
  (global-flycheck-mode)

  ;; disable jshint since we prefer eslint checking
  (setq-default flycheck-disabled-checkers
                (append flycheck-disabled-checkers
                        '(javascript-jshint)))

  ;; use eslint with web-mode for jsx files
  (flycheck-add-mode 'javascript-eslint 'web-mode)

  ;; customize flycheck temp file prefix
  (setq-default flycheck-temp-prefix ".flycheck")

  ;; disable json-jsonlist checking for json files
  (setq-default flycheck-disabled-checkers
                (append flycheck-disabled-checkers
                        '(json-jsonlist)))

  ;; https://github.com/purcell/exec-path-from-shell
  ;; only need exec-path-from-shell on OSX
  ;; this hopefully sets up path and other vars better
  (when (memq window-system '(mac ns))
    (exec-path-from-shell-initialize))

  ;; use local eslint from node_modules before global
  ;; http://emacs.stackexchange.com/questions/21205/flycheck-with-file-relative-eslint-executable
  (defun my/use-eslint-from-node-modules ()
    (let* ((root (locate-dominating-file
                  (or (buffer-file-name) default-directory)
                  "node_modules"))
           (eslint (and root
                        (expand-file-name "node_modules/eslint/bin/eslint.js"
                                          root))))
      (when (and eslint (file-executable-p eslint))
        (setq-local flycheck-javascript-eslint-executable eslint))))
  (add-hook 'flycheck-mode-hook #'my/use-eslint-from-node-modules)

  ;; for better jsx syntax-highlighting in web-mode
  ;; - courtesy of Patrick @halbtuerke
  (defadvice web-mode-highlight-part (around tweak-jsx activate)
    (if (equal web-mode-content-type "jsx")
        (let ((web-mode-enable-part-face nil))
          ad-do-it)
      ad-do-it))
  )

(use-package magit
  :ensure t
  :defer 0.3
  :config
  (defhydra hydra-magit (:color blue)
    "
  ^
  ^Magit^             ^Do^
  ^─────^─────────────^──^────────
  _q_ quit            _b_ blame
  ^^                  _c_ clone
  ^^                  _i_ init
  ^^                  _s_ status
  ^^                  ^^
  "
    ("q" nil)
    ("b" magit-blame)
    ("c" magit-clone)
    ("i" magit-init)
    ("s" magit-status))
  )

(use-package git-gutter
  :ensure t
  :defer 0.3
  :diminish
  :init (global-git-gutter-mode +1)
  )

(use-package counsel
  :ensure t
  :after ivy
  :config (counsel-mode))

(use-package ivy
  :ensure t
  :defer 0.1
  :diminish
  :bind (("C-c C-r" . ivy-resume)
         ("C-x B" . ivy-switch-buffer-other-window))
  :custom
  (ivy-count-format "(%d/%d) ")
  (ivy-use-virtual-buffers t)
  :config (ivy-mode))

(use-package ivy-rich
  :ensure t
  :after (:all ivy counsel)
  :init (setq ivy-rich-parse-remote-file-path t)
  :config (ivy-rich-mode 1))

(use-package swiper
  :ensure t
  :after ivy
  :bind (("C-s" . swiper)
         ("C-r" . swiper))
  )

(use-package docker-tramp
  :ensure t
  :config
  ;; Open files in Docker containers like so: /docker:drunk_bardeen:/etc/passwd
  (push
   (cons
    "docker"
    '((tramp-login-program "docker")
      (tramp-login-args (("exec" "-it") ("%h") ("/bin/bash")))
      (tramp-remote-shell "/bin/sh")
      (tramp-remote-shell-args ("-i") ("-c"))))
   tramp-methods)

  (defadvice tramp-completion-handle-file-name-all-completions
      (around dotemacs-completion-docker activate)
    "(tramp-completion-handle-file-name-all-completions \"\" \"/docker:\" returns
    a list of active Docker container names, followed by colons."
    (if (equal (ad-get-arg 1) "/docker:")
        (let* ((dockernames-raw (shell-command-to-string "docker ps | perl -we 'use strict; $_ = <>; m/^(.*)NAMES/ or die; my $offset = length($1); while(<>) {substr($_, 0, $offset, q()); chomp; for(split m/\\W+/) {print qq($_:\n)} }'"))
               (dockernames (cl-remove-if-not
                             #'(lambda (dockerline) (string-match ":$" dockerline))
                             (split-string dockernames-raw "\n"))))
          (setq ad-return-value dockernames))
      ad-do-it))
  )

(use-package ledger-mode
  :ensure t
  :when (executable-find "ledger")
  :mode ("\\.dat\\'"
         "\\.ledger\\'")
  :custom (ledger-clear-whole-transactions t)
  :config
  (add-hook 'ledger-mode-hook #'ledger-flymake-enable)
  )

(use-package pyenv-mode
  :ensure t
  :config
  (setenv "WORKON_HOME" "~/virtualenvs")
  (setq python-shell-interpreter "ipython"
        python-shell-interpreter-args "--simple-prompt -i")
  )

(use-package ibuffer
  :ensure t
  :config
  )

(use-package dockerfile-mode
  :ensure t
  )

(use-package cmake-mode
  :ensure t
  )

(use-package try
  :ensure t
  )

(use-package smartparens
  :ensure t
  :config 
  (show-smartparens-global-mode +1)
  (smartparens-global-mode 1)

  ;; when you press RET, the curly braces automatically
  ;; add another newline
  (sp-with-modes '(c-mode c++-mode)
    (sp-local-pair "{" nil :post-handlers '(("||\n[i]" "RET")))
    (sp-local-pair "/*" "*/" :post-handlers '((" | " "SPC")
                                              ("* ||\n[i]" "RET"))))
  )

(use-package yaml-mode
  :ensure t
  )

;; Function to disable all themes, as emacs allows several themes to
;; be up at the same time
(defun pm/disable-all-themes()
  "Cleans up theme configurations."
  (interactive)
  (mapc #'disable-theme custom-enabled-themes))

(defvar pm/theme-hooks nil
  "((theme-id . function) ...)")

(defun pm/add-theme-hook (theme-id hook-func)
  (add-to-list 'pm/theme-hooks (cons theme-id hook-func)))

(defun pm/load-theme-advice (f theme-id &optional no-confirm no-enable &rest args)
  "Enhances `load-theme' in two ways:
1. Disables enabled themes for a clean slate.
2. Calls functions registered using `pm/add-theme-hook'."
  (unless no-enable
    (pm/disable-all-themes))
  (prog1
      (apply f theme-id no-confirm no-enable args)
    (unless no-enable
      (pcase (assq theme-id pm/theme-hooks)
        (`(,_ . ,f) (funcall f))))))

(advice-add 'load-theme
            :around
            #'pm/load-theme-advice)

(use-package material-theme
  :ensure t
  :defer t
  :init
  (defun pm/material-theme-hook ()
    (set-face-attribute 'which-key-key-face nil :foreground
                        (face-attribute 'error :foreground))
    (loop for n from 1 to 8
          do (set-face-attribute (intern-soft (format "org-level-%s" n))
                                 nil
                                 :height     'unspecified
                                 :background 'unspecified
                                 :box        'unspecified)))
  (pm/add-theme-hook 'material       #'pm/material-theme-hook)
  (pm/add-theme-hook 'material-light #'pm/material-theme-hook)
  )

(use-package plantuml-mode
  :ensure t)


(setq backup-directory-alist
	`(("." . ,(concat user-emacs-directory "backups"))))

;; Pablo Munoz's init.el

;; Install use-package, which is going to be used to install
;; other packages in an easy manner.

;; ======================================================================
;; Quality of life
;; ======================================================================

(defalias 'yes-or-no-p 'y-or-n-p)

(tool-bar-mode -1)
(scroll-bar-mode -1)
(setq inhibit-splash-screen t)

;; Change backups and autosaves to avoid pesky # and ~ files
(let ((backup-dir "~/.emacs.d/backups")
      (auto-saves-dir "~/.emacs.d/autosavedir/"))
  (dolist (dir (list backup-dir auto-saves-dir))
    (when (not (file-directory-p dir))
      (make-directory dir t)))
  (setq backup-directory-alist `(("." . ,backup-dir))
        auto-save-file-name-transforms `((".*" ,auto-saves-dir))
        auto-save-list-file-prefix (concat auto-saves-dir ".saves-")
        tramp-backup-directory-alist `((".*" . ,backup-dir))
        tramp-auto-save-directory auto-saves-dir))

(setq backup-by-copying t    ; Don't delink hardlinks
      delete-old-versions t  ; Clean up the backups
      version-control t      ; Use version numbers on backups,
      kept-new-versions 3    ; keep some new versions
      kept-old-versions 2)   ; and some old ones, too

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

;; Font
(set-language-environment "UTF-8")
(set-default-coding-systems 'utf-8-unix)

;; set a default font
(when (member "DejaVu Sans Mono" (font-family-list))
  (set-face-attribute 'default nil :font "DejaVu Sans Mono" :height 130))

;; specify font for all unicode characters
(when (member "Symbola" (font-family-list))
  (set-fontset-font t 'unicode "Symbola" nil 'prepend))

;; ======================================================================
;; OS dependent configuration
;; ======================================================================
;; key bindings
(when (eq system-type 'darwin) ;; mac specific settings
  (setq mac-option-modifier 'alt)
  (setq mac-command-modifier 'meta)
  (global-set-key [kp-delete] 'delete-char) ;; sets fn-delete to be right-delete
  )

;; ======================================================================
;; Package management
;; ======================================================================
;; This is only needed once, near the top of the file
;; load package manager, add the Melpa package registry
(require 'package)
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'package-archives
             '("org" . "http://orgmode.org/elpa/"))
(package-initialize)

(eval-when-compile
  ;; Following line is not needed if use-package.el is in ~/.emacs.d
  (let ((base (expand-file-name "third-party" user-emacs-directory)))
    (add-to-list 'load-path base)
    (dolist (f (directory-files base))
      (let ((name (concat base "/" f)))
	(when (and (file-directory-p name)
		   (not (equal f ".."))
		   (not (equal f ".")))
	  (add-to-list 'load-path name)))))
  
  (require 'use-package))

;; evil: Emulation of Vim in emacs
(use-package evil
  :ensure t ;; install evil package if not installed
  :init      ;; tweak evil's configuration BEFORE loading
  (setq evil-ex-complete-emacs-commands nil)
  (setq evil-vsplit-window-right t)
  (setq evil-split-window-below t)
  (setq evil-shift-round nil)
  (setq evil-want-C-u-scroll t)
  :config    ;; tweak evil AFTER loading
  (evil-mode)

  ;; example how to map a command in normal mode (called 'normal state' in evil)
  (define-key evil-normal-state-map (kbd ", w") 'evil-window-vsplit)
  )

;; org-mode: An awesome package for organization/notes
(use-package org
  :ensure t
  :mode ("\\.org\\'" . org-mode)
  :bind (("C-c l" . org-store-link)
         ("C-c c" . org-capture)
         ("C-c a" . org-agenda)
         ("C-c b" . org-iswitchb)
         ("C-c C-w" . org-refile)
         ("C-c j" . org-clock-goto)
         ("C-c C-x C-o" . org-clock-out))
  :config
  (progn
    ;; The GTD part of this config is heavily inspired by
    ;; https://emacs.cafe/emacs/orgmode/gtd/2017/06/30/orgmode-gtd.html
    (setq org-directory "~/my/org")
    (setq org-agenda-files
          (mapcar (lambda (path) (concat org-directory path))
                  '("/org.org"
                    "/gtd/gtd.org"
                    "/gtd/inbox.org"
                    "/gtd/tickler.org")))
    (setq org-log-done 'time)
    (setq org-src-fontify-natively t)
    (setq org-src-preserve-indentation t)
    (setq org-src-tab-acts-natively t)
    (setq org-use-speed-commands t)
    (setq org-capture-templates
          '(("t" "Todo [inbox]" entry
             (file+headline "~/org/gtd/inbox.org" "Tasks")
             "* TODO %i%?")
            ("T" "Tickler" entry
             (file+headline "~/org/gtd/tickler.org" "Tickler")
             "* %i%? \n %^t")))
    (setq org-refile-targets
          '(("~/org/gtd/gtd.org" :maxlevel . 3)
            ("~/org/gtd/someday.org" :level . 1)
            ("~/org/gtd/tickler.org" :maxlevel . 2)))
    (setq org-todo-keywords '((sequence "TODO(t)" "WAITING(w)" "|" "DONE(d)" "CANCELLED(c)")))
    (setq org-agenda-custom-commands
          '(("@" "Contexts"
             ((tags-todo "@email"
                         ((org-agenda-overriding-header "Emails")))
              (tags-todo "@phone"
                         ((org-agenda-overriding-header "Phone")))))))
    (setq org-clock-persist t)
    (org-clock-persistence-insinuate)
    (setq org-time-clocksum-format '(:hours "%d" :require-hours t :minutes ":%02d" :require-minutes t)))
  )

(use-package org-inlinetask
  :bind (:map org-mode-map
              ("C-c C-x t" . org-inlinetask-insert-task))
  :after (org)
  :commands (org-inlinetask-insert-task)
  )

(use-package org-bullets
  :ensure t
  :commands (org-bullets-mode)
  :init (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))
  )

;; Hydra for interactive pop up menus
(use-package hydra
  :ensure t
  :bind ("C-c m" . hydra-magit/body)
  :config
  (setq hydra-lv nil) ;use echo area
  )

(use-package major-mode-hydra
  :load-path "third-party/major-mode-hydra.el"
  :requires (dash s)
  :bind ("C-M-m" . major-mode-hydra)
  )

(use-package org-jira
  :ensure t
  :defer t
  :config
  (setq jiralib-token
	`("cookie" . "ajs_group_id=null; ajs_anonymous_id=%226b7dc79c-19a6-46a1-b042-9ee2011252a6%22; share_onboarding_seen=1; _csrf=8ALgWm4cHpPIEAurDLRbR3mb; atlassian.xsrf.token=BK6Q-G0KC-9DDN-GXU0_aff8083395d22f5cce535185b4a386fa14ccab52_lin; jira.mobile.native.experience.prompt=true; cloud.session.token=eyJraWQiOiJzZXNzaW9uLXNlcnZpY2VcL3Nlc3Npb24tc2VydmljZSIsImFsZyI6IlJTMjU2In0.eyJhc3NvY2lhdGlvbnMiOltdLCJzdWIiOiI1NTcwNTg6NGU3ZDBjYzItNmJmYi00MTg4LTk1OTItOTM4Yzk5NjY5MGU3IiwiYXVkIjoiYXRsYXNzaWFuIiwiaW1wZXJzb25hdGlvbiI6W10sIm5iZiI6MTUzNTk0Mzk0NiwicmVmcmVzaFRpbWVvdXQiOjE1MzU5NDQ1NDYsImlzcyI6InNlc3Npb24tc2VydmljZSIsInNlc3Npb25JZCI6IjY1ZmQ1NzI2LTUyNGItNDE2Ny05NGVmLWY5ZTk2ODdjYzA0YiIsImV4cCI6MTUzODUzNTk0NiwiaWF0IjoxNTM1OTQzOTQ2LCJlbWFpbCI6InBtdW5vekBza3ljYXRjaC5jb20iLCJqdGkiOiI2NWZkNTcyNi01MjRiLTQxNjctOTRlZi1mOWU5Njg3Y2MwNGIifQ.NIqiHOQBvixnBOIkV7qQHmSUEWlGb-WG9SQbUMJU3NSjFxVlrICmsZwVSz7XHbbGshFQPKE2RZ6WMereE7GgqsKUpQPB9AftQvS9trf49xyCOGkSdtWvcLbOsoSIMr6oYFSswiq_Aclg47jC4hFEdZuQgpB1oLNQ13OabhepW8DH7QhQEp2qrPtMWrjP_0fQXK5J227CujUNdKcHte7gTAxZIyRT3W2vvI6bcRHxvFgaBg0ORTc0VR4TxEQbaakUeixkFohTG9oYS824XA-g5NPPUr25QaeVI5bahQbILDr5_gSs9a4NjOqOOAGPhQ8cBg4yCX_i-_t-1Wzf1R5S2A"))
  (setq jiralib-url "https://skycatch.atlassian.net")
  )

(use-package elpy
  :ensure t
  :config
  (elpy-enable)
  ;; Fixing a key binding bug in elpy
  (define-key yas-minor-mode-map (kbd "C-c k") 'yas-expand)
  ;; Fixing another key binding bug in iedit mode
  (define-key global-map (kbd "C-c o") 'iedit-mode)
  (setenv "PYTHONPATH" "/usr/bin/python")
  )

(use-package flymake-cursor
  :ensure t
  )

(defun pm/hydra-jira ()
  (interactive)
  (funcall
      (pretty-hydra-define hydra-jira (:exit t :hint nil)
        ("Get" (("p" org-jira-get-projects                "Get Projects")
                ("g" org-jira-get-issues                  "Get Issues")
                ("G" org-jira-get-subtasks                "Get Subtasks")
                ("r" org-jira-refresh-issue               "Refresh Issue")
                ("R" org-jira-refresh-issues-in-buffer    "Refresh Issues in Buffer"))

         "Manage" (("b" org-jira-browse-issue             "Browse Issue")
                   ("c" org-jira-create-issue             "Create Issue")
                   ("s" org-jira-create-subtask           "Create Subtask")
                   ("P" org-jira-progress-issue           "Update Issue Progress")
                   ("a" org-jira-assign-issue             "Assign Issue"))

         "Push" (("u" org-jira-update-issue                "Update Issue")
                 ("y" org-jira-copy-current-issue-key      "Copy Current Issue Key")
                 ("U" org-jira-update-comment              "Update Comment")
                 ("t" org-jira-todo-to-jira                "Todo to Jira"))))))


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

(use-package git-timemachine
  ;; p Visit previous historic version
  ;; n Visit next historic version
  ;; w Copy the abbreviated hash of the current historic version
  ;; W Copy the full hash of the current historic version
  ;; g Goto nth revision
  ;; t Goto revision by selected commit message
  ;; q Exit the time machine.
  ;; b Run magit-blame on the currently visited revision (if magit available).
  :ensure t
  :defer 1
  :diminish
  )

(use-package which-key
  :ensure t
  :defer 0.2
  :diminish
  :config (which-key-mode)
  )

;; ======================================================================
;; Themes
;; ======================================================================

;; Function to disable all themes, as emacs allows several themes to
;; be up at the same time
(defun pm/disable-all-themes()
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

(use-package color-theme-sanityinc-tomorrow
  :ensure t
  :defer t
  )

(use-package toxi
  :ensure toxi-theme
  :defer t
  )

(use-package cherry-blossom
  :ensure cherry-blossom-theme
  :defer t
  )

(use-package solarized
  :ensure solarized-theme
  :defer t
  :init
  (defun pm/solarized-theme-hook ()
    (set-face-attribute 'font-lock-constant-face nil :weight 'normal)
    (set-face-attribute 'font-lock-function-name-face nil :weight 'bold)
    (set-face-attribute 'which-key-key-face nil :foreground
                        (face-attribute 'error :foreground)))
  (pm/add-theme-hook 'solarized-dark  #'pm/solarized-theme-hook)
  (pm/add-theme-hook 'solarized-light #'pm/solarized-theme-hook)
  :config
  (setq solarized-use-variable-pitch nil
        solarized-use-less-bold t
        solarized-use-more-italic nil
        solarized-distinct-doc-face t
        solarized-high-contrast-mode-line t
        ;; I find different font sizes irritating.
        solarized-height-minus-1 1.0
        solarized-height-plus-1 1.0
        solarized-height-plus-2 1.0
        solarized-height-plus-3 1.0
        solarized-height-plus-4 1.0)
  )

(use-package calmer-forest
  :ensure calmer-forest-theme
  :defer t
  )

(defhydra pm/themes-hydra (:hint nil :color pink)
  "
Themes
----------------------------------------------------
_s_: Sol Dark     _m_: Mat Dark      _x_: toxi    _DEL_: none
_S_: Sol Light    _M_: Mat Light     _c_: cherry
_f_: Calm For     _b_: Sanity Blue
"
  ("s" (load-theme 'solarized-dark  t))
  ("S" (load-theme 'solarized-light t))
  ("m" (load-theme 'material        t))
  ("M" (load-theme 'material-light  t))
  ("x" (load-theme 'toxi            t))
  ("c" (load-theme 'cherry-blossom  t))
  ("f" (load-theme 'calmer-forest   t))
  ("b" (load-theme 'sanityinc-tomorrow-blue))
  ("DEL" (pm/disable-all-themes))
  ("RET" nil "done" :color blue)
  )

(bind-keys ("C-c w t"  . pm/themes-hydra/body))

;; ================================================================================
;; Custom set variables
;; ================================================================================
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default bold shadow italic underline bold bold-italic bold])
 '(ansi-color-names-vector
   (vector "#212121" "#B71C1C" "#558b2f" "#FFA000" "#2196f3" "#4527A0" "#00796b" "#FAFAFA"))
 '(beacon-color "#cc6666")
 '(compilation-message-face (quote default))
 '(cua-global-mark-cursor-color "#2aa198")
 '(cua-normal-cursor-color "#657b83")
 '(cua-overwrite-cursor-color "#b58900")
 '(cua-read-only-cursor-color "#859900")
 '(custom-safe-themes
   (quote
    ("82d2cac368ccdec2fcc7573f24c3f79654b78bf133096f9b40c20d97ec1d8016" "06f0b439b62164c6f8f84fdda32b62fb50b6d00e8b01c2208e55543a6337433a" "04589c18c2087cd6f12c01807eed0bdaa63983787025c209b89c779c61c3a4c4" "8a97050c9dd0af1cd8c3290b061f4b6032ccf2044ddc4d3c2c39e516239b2463" default)))
 '(fci-rule-color "#ECEFF1")
 '(flycheck-color-mode-line-face-to-color (quote mode-line-buffer-id))
 '(frame-background-mode (quote dark))
 '(fringe-mode 6 nil (fringe))
 '(highlight-changes-colors (quote ("#d33682" "#6c71c4")))
 '(highlight-symbol-colors
   (--map
    (solarized-color-blend it "#fdf6e3" 0.25)
    (quote
     ("#b58900" "#2aa198" "#dc322f" "#6c71c4" "#859900" "#cb4b16" "#268bd2"))))
 '(highlight-symbol-foreground-color "#586e75")
 '(highlight-tail-colors
   (quote
    (("#eee8d5" . 0)
     ("#B4C342" . 20)
     ("#69CABF" . 30)
     ("#69B7F0" . 50)
     ("#DEB542" . 60)
     ("#F2804F" . 70)
     ("#F771AC" . 85)
     ("#eee8d5" . 100))))
 '(hl-bg-colors
   (quote
    ("#DEB542" "#F2804F" "#FF6E64" "#F771AC" "#9EA0E5" "#69B7F0" "#69CABF" "#B4C342")))
 '(hl-fg-colors
   (quote
    ("#fdf6e3" "#fdf6e3" "#fdf6e3" "#fdf6e3" "#fdf6e3" "#fdf6e3" "#fdf6e3" "#fdf6e3")))
 '(hl-paren-colors (quote ("#2aa198" "#b58900" "#268bd2" "#6c71c4" "#859900")))
 '(hl-sexp-background-color "#efebe9")
 '(linum-format (quote dynamic))
 '(magit-diff-use-overlays nil)
 '(nrepl-message-colors
   (quote
    ("#dc322f" "#cb4b16" "#b58900" "#546E00" "#B4C342" "#00629D" "#2aa198" "#d33682" "#6c71c4")))
 '(package-selected-packages
   (quote
    (which-key git-timemachine git-gutter magit flymake-cursor elpy org-jira major-mode-hydra color-theme-sanityinc-tomorrow calmer-forest-theme cherry-blossom-theme toxi-theme solarized-theme material-theme hydra org-bullets use-package evil)))
 '(pos-tip-background-color "#eee8d5")
 '(pos-tip-foreground-color "#586e75")
 '(smartrep-mode-line-active-bg (solarized-color-blend "#859900" "#657b83" 0.2))
 '(term-default-bg-color "#fdf6e3")
 '(term-default-fg-color "#657b83")
 '(vc-annotate-background nil)
 '(vc-annotate-background-mode nil)
 '(vc-annotate-color-map
   (quote
    ((20 . "#B71C1C")
     (40 . "#FF5722")
     (60 . "#FFA000")
     (80 . "#558b2f")
     (100 . "#00796b")
     (120 . "#2196f3")
     (140 . "#4527A0")
     (160 . "#B71C1C")
     (180 . "#FF5722")
     (200 . "#FFA000")
     (220 . "#558b2f")
     (240 . "#00796b")
     (260 . "#2196f3")
     (280 . "#4527A0")
     (300 . "#B71C1C")
     (320 . "#FF5722")
     (340 . "#FFA000")
     (360 . "#558b2f"))))
 '(vc-annotate-very-old-color nil)
 '(weechat-color-list
   (quote
    (unspecified "#fdf6e3" "#eee8d5" "#990A1B" "#dc322f" "#546E00" "#859900" "#7B6000" "#b58900" "#00629D" "#268bd2" "#93115C" "#d33682" "#00736F" "#2aa198" "#657b83" "#839496")))
 '(xterm-color-names
   ["#eee8d5" "#dc322f" "#859900" "#b58900" "#268bd2" "#d33682" "#2aa198" "#073642"])
 '(xterm-color-names-bright
   ["#fdf6e3" "#cb4b16" "#93a1a1" "#839496" "#657b83" "#6c71c4" "#586e75" "#002b36"]))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
(put 'dired-find-alternate-file 'disabled nil)

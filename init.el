;;; pacakge -- init.el
;;;; Commentary:

;;; Code:
;; This is only needed once, near the top of the file
(eval-when-compile
  ;; Following line is not needed if use-package.el is in ~/.emacs.d
  (add-to-list 'load-path
	       "/Users/pablo/.emacs.d/third-party/use-package"
	       "/Users/pablo/.emacs.d/lisp/cpp")
  (require 'use-package))

(add-to-list 'custom-theme-load-path "~/.emacs.d/themes")
(load-theme 'zerodark t)

(load-file "/Users/pablo/.emacs.d/lisp/common.el")
(load-file "/Users/pablo/.emacs.d/lisp/themes.el")
(load-file "/Users/pablo/.emacs.d/lisp/cpp.el")
(load-file "/Users/pablo/.emacs.d/lisp/js.el")
(load-file "/Users/pablo/.emacs.d/lisp/org.el")

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (zerodark-theme company-irony-c-headers yaml-mode which-key web-mode use-package try toxi-theme srcery-theme sr-speedbar solarized-theme smartparens pyenv-mode powerline plantuml-mode pdf-tools paganini-theme org-jira org-bullets multiple-cursors moody minions material-theme magit ledger-mode ivy-rich hydra gotham-theme google-c-style git-timemachine git-gutter ggtags flymake-cursor flycheck-rtags flycheck-clang-tidy exec-path-from-shell evil elpy dockerfile-mode docker-tramp cyberpunk-theme counsel company-rtags company-irony color-theme-sanityinc-tomorrow cmake-mode cmake-ide cherry-blossom-theme ccls calmer-forest-theme auto-complete-clang))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(global-set-key (kbd "C-c m") 'magit)

(put 'dired-find-alternate-file 'disabled nil)

(setq delete-old-versions 't)
(setq-default indent-tabs-mode nil)

(setq c-default-style "k&r"
      c-basic-offset 2)

    (defun my-indent-setup ()
      (c-set-offset 'arglist-intro '+)
      (c-set-offset 'innamespace 0))

    (add-hook 'java-mode-hook 'my-indent-setup)
(add-hook 'c-mode-hook 'my-indent-setup)
(add-hook 'c++-mode-hook 'my-indent-setup)

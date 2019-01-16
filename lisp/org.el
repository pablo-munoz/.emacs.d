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
	 ("C-c C-x C-o" . org-clock-out)
	 ("C-x C-l" . org-babel-remove-result))
  :config
  (progn
    ;; The GTD part of this config is heavily inspired by
    ;; https://emacs.cafe/emacs/orgmode/gtd/2017/06/30/orgmode-gtd.html
    (setq org-ditaa-jar-path "~/Dropbox/org-mode/contrib/scripts/ditaa.jar")
    (setq org-plantuml-jar-path "~/java/plantuml.jar")
    (add-hook 'org-babel-after-execute-hook 'bh/display-inline-images 'append)
    (remove-hook 'org-babel-after-execute-hook 'bh/display-inline-images)

    (org-babel-do-load-languages
     (quote org-babel-load-languages)
     (quote ((emacs-lisp . t)
	     (dot . t)
	     (ditaa . t)
	     (R . t)
	     (python . t)
	     (ruby . t)
	     (gnuplot . t)
	     (clojure . t)
	     (shell . t)
	     (ledger . t)
	     (org . t)
	     (plantuml . t)
	     (latex . t))))

    (setq org-startup-indented t)
    (setq org-directory "~/my/org")
    (setq org-agenda-files
	  (mapcar (lambda (path) (concat org-directory path))
		  '("/org.org"
		    "/gtd/gtd.org"
		    "/gtd/inbox.org"
		    "/gtd/tickler.org")))
    (setq org-log-done 'time)
    (setq org-src-fontify-natively t
	  org-src-preserve-indentation t
	  org-src-tab-acts-natively t)
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

  (defun my-org-screenshot ()
    "Take a screenshot into a time stamped unique-named file in the
same directory as the org-buffer and insert a link to this file."
    (interactive)
    (org-display-inline-images)
    (setq filename
	  (concat
	   (make-temp-name
	    (concat (file-name-nondirectory (buffer-file-name))
		    "_imgs/"
		    (format-time-string "%Y%m%d_%H%M%S_")) ) ".png"))
    (unless (file-exists-p (file-name-directory filename))
      (make-directory (file-name-directory filename)))
					; take screenshot
    (if (eq system-type 'darwin)
	(call-process "screencapture" nil nil nil "-i" filename))
    (if (eq system-type 'gnu/linux)
	(call-process "import" nil nil nil filename))
					; insert into file if correctly taken
    (if (file-exists-p filename)
	(insert (concat "[[file:" filename "]]"))))
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

(with-eval-after-load "org"
  ;; (add-to-list
  ;;  'org-src-lang-modes '("plantuml" . plantuml))
  (setq org-latex-listings 'minted
        org-latex-packages-alist '(("" "minted"))
        org-latex-pdf-process
        '("pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
          "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"))
  )

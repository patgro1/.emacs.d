(setq inhibit-startup-message t)
(menu-bar-mode -1)
(toggle-scroll-bar -1)
(tool-bar-mode -1)
(tooltip-mode -1)
(set-fringe-mode 10)
;; the t parameter apends to the hook, instead of prepending
;; this means it'd be run after other hooks that might fiddle
;; with the frame size
(add-hook 'window-setup-hook 'toggle-frame-maximized t)

(setq gc-cons-threshold 100000000)
(setq read-process-output-max (* 1024 1024)) ;; 1mb

(defun pg/turn-line-number-on ()
  (display-line-numbers-mode t)
  (setq display-line-numbers 'relative))
(add-hook 'prog-mode-hook 'pg/turn-line-number-on)
(column-number-mode t)

(setq backup-inhibited t)
(setq auto-save-default nil)

;; Install straight.el
  (defvar bootstrap-version)
  (let ((bootstrap-file
	 (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
	(bootstrap-version 5))
    (unless (file-exists-p bootstrap-file)
      (with-current-buffer
	  (url-retrieve-synchronously
	   "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
	   'silent 'inhibit-cookies)
	(goto-char (point-max))
	(eval-print-last-sexp)))
    (load bootstrap-file nil 'nomessage))
(setq package-enable-at-startup nil)

(straight-use-package 'use-package)
(use-package straight
       :custom (straight-use-package-by-default t))

(org-babel-do-load-languages
 'org-babel-load-languages
 '((emacs-lisp . t)
   (python . t)))
(setq org-confirm-babel-evaluate nil)

(use-package counsel)
(use-package ivy
  :config
  (setq ivy-use-virtual-buffers t)
  (setq ivy-count-format "(%d/%d) ")
  (ivy-mode 1)
  (global-set-key (kbd "M-x") 'counsel-M-x))
(use-package swiper)

(use-package which-key
  :ensure t
  :config
  (setq which-key-idle-delay 0.3)
  (which-key-mode))

(use-package flycheck
  :config
  (global-flycheck-mode))

(use-package general)

(set-face-attribute 'default nil :family "FiraCode Nerd Font" :height 150)

(use-package doom-themes
:config
(load-theme 'doom-material t))

(use-package doom-modeline
:init (doom-modeline-mode 1))

(use-package tree-sitter
  :straight (:host github :repo "patgro1/elisp-tree-sitter"))
(use-package tree-sitter-langs
  :straight (:host github :repo "patgro1/tree-sitter-langs"))
(add-to-list 'tree-sitter-load-path "~/.emacs.d/straight/repos/tree-sitter-langs/bin")
(tree-sitter-require 'verilog)
(add-hook 'python-mode-hook #'tree-sitter-hl-mode)
(add-hook 'verilog-mode-hook #'tree-sitter-hl-mode)
(add-hook 'cpp-mode-hook #'tree-sitter-hl-mode)

(use-package undo-tree
  :init
  (global-undo-tree-mode 1))
(use-package evil
  :init
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  (setq evil-undo-system 'undo-tree)
  :config
  (evil-mode 1))
  ;(evil-set-leader 'normal (kbd "SPC")))

(use-package evil-collection
  :after evil
  :config
  (evil-collection-init))

(general-create-definer leader-def
  ;; :prefix my-leader
  :states '(normal visual motion insert emacs)
  :prefix "SPC"
  :keymaps 'override
  :non-normal-prefix "M-SPC")

(leader-def
"b" '(:ignore t :which-key "buffers")
"bb" 'switch-to-buffer
"bk" 'kill-current-buffer)

(use-package magit
  :commands (magit-status magit-log)
  :general
  (leader-def
  "g" '(:ignore t :which-key "git")
  "gg" 'magit-status
  "gl" 'magit-log))

(use-package tramp)

;(setq vc-ignore-dir-regexp
;      (format "\\(%s\\)\\|\\(%s\\)"
;              vc-ignore-dir-regexp
;              tramp-file-name-regexp))

(setq vc-handled-backends '(SVN Git))

(setq tramp-default-method "ssh")

; FIXME: when tangling this in, projectile is acting weird with tramp
;(defadvice projectile-project-root (around ignore-remote first activate)
;  (unless (file-remote-p default-directory 'no-identification) ad-do-it))

(use-package projectile
  :ensure t
  :init
  (projectile-mode +1)
  :config
  (setq projectile-indexing-method 'alien)
  (setq projectile-completion-system 'ivy)
  (setq projectile-project-search-path '("~/workspace"))
  ;(projectile-mode))
  :general
  (leader-def
  "p" '(:ignore t :which-key "projectile")
  "<SPC>" 'projectile-find-file
  "pf" 'projectile-find-file
  "pp" 'projectile-switch-project
  "pb" 'projectile-switch-to-buffer))

(use-package dockerfile-mode
:config
(add-to-list 'auto-mode-alist '("Dockerfile\\'" . dockerfile-mode)))

(use-package company-jedi
	 :config
	 (add-to-list 'company-backends 'company-jedi)
	 (setq jedi:complete-on-dot t)
	 :hook
	 (inferior-python-mode . jedi:setup)
	 (python-mode . jedi:setup)
	 )

       (defun my/python-mode-hook ()
	 (add-to-list 'company-backends 'company-jedi))
(use-package virtualenvwrapper)
(venv-set-location "~/virtualenvs")
(venv-workon "emacs")
(setq lsp-python-executable-cmd "python3")

(use-package company
:config
(setq company-global-modes '(not comint-mode
				    eshell-mode
				    help-mode
				    message-mode))
(setq company-tooltip-align-annotations t ; aligns annotation to the right
		      company-tooltip-limit 24            ; bigger popup window
		      company-idle-delay .2               ; decrease delay before autocompletion popup shows
		      company-echo-delay 0                ; remove annoying blinking
		      company-minimum-prefix-length 2
		      company-require-match nil
		      company-dabbrev-ignore-case nil
		      company-dabbrev-downcase nil)
(company-tng-configure-default)
(add-hook 'after-init-hook 'global-company-mode))
(use-package company-box
	:after company
	:diminish
	:hook (company-mode . company-box-mode)
	:custom (company-box-icons-alist 'company-box-icons-all-the-icons))

(use-package lsp-mode
  :hook (
	 (python-mode . lsp)
	 (lsp-mode . lsp-enable-which-key-integration))
  :commands lsp
  :general
  (leader-def
    :keymaps 'lsp-mode-map
    "l" '(:ignore t :which-key "lsp")
    "lg" 'lsp-find-definition
    "lr" 'lsp-ui-peek-find-references
    "ld" 'lsp-ui-peek-find-definitions))
(use-package lsp-ui
  :commands lsp-ui-mode
  :custom
  (lsp-ui-sideline-show-diagnostics t)
  (lsp-ui-sideline-show-hover t)
  (lsp-ui-peek-enable t)
  (lsp-ui-flycheck-enable t))
(use-package lsp-ivy :commands lsp-ivy-workspace-symbol)

(defun setup_env ()
	  (interactive)
	  (venv-deactivate)
	  (message projectile-project-root)
	  (setenv "TOOLS_PATH" (concat (projectile-project-root) "/tools"))
	  ;; (setq tags-table-list (list (concat (projectile-project-root) "/rtl" )))
	  (setenv "PYTHONPATH" (concat (projectile-project-root) ":" (getenv "TOOLS_PATH") "/themis_fw:" (concat  (projectile-project-root) "/registers/auto_gen/python")))
	  (venv-set-location "~/virtualenvs")
	  (venv-workon)
	  (lsp-restart-workspace)
;	  (jedi:stop-server)
;	  (jedi:setup)
	  (setq projectile-tags-command (concat (projectile-project-root)"scripts/etags/verilog_etags " (projectile-project-root) "rtl"))
	  (setq projectile-tags-file-name (concat (projectile-project-root) "rtl/TAGS"))
	  (setq jedi:complete-on-dot t)
	  )

(use-package org-bullets
  :after org
  :hook (org-mode . org-bullets-mode)
  :config
  (setq org-bullets-bullet-list '("○" "☉" "◎" "◉" "○" "◌" "◎" "●" "◦" "◯")))

(with-eval-after-load 'org
(require 'org-tempo)
(add-to-list 'org-structure-template-alist '("sh" . "src shell"))
(add-to-list 'org-structure-template-alist '("el" . "src emacs-lisp"))
(add-to-list 'org-structure-template-alist '("py" . "src python")))

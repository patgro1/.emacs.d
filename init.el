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
  (evil-mode 1)
  (evil-set-leader 'normal (kbd "SPC")))


(use-package evil-collection
  :after evil
  :config
  (evil-collection-init))

(evil-define-key 'normal 'global (kbd "<leader>bb") 'switch-to-buffer)
(evil-define-key 'normal 'global (kbd "<leader>bk") 'kill-current-buffer)

(use-package magit
  :commands (magit-status))
(evil-define-key 'normal 'global (kbd "<leader>gg") 'magit-status)

(use-package tramp)

;(setq vc-ignore-dir-regexp
;      (format "\\(%s\\)\\|\\(%s\\)"
;              vc-ignore-dir-regexp
;              tramp-file-name-regexp))

(setq vc-handled-backends '(SVN Git))

(setq tramp-default-method "ssh")

(defadvice projectile-project-root (around ignore-remote first activate)
  (unless (file-remote-p default-directory 'no-identification) ad-do-it))

(use-package projectile
  :ensure t
  :init
  (projectile-mode +1)
  :config
  (setq projectile-indexing-method 'alien)
  (setq projectile-project-search-path '("~/workspace")))
  ;(projectile-mode))
(evil-define-key 'normal 'global (kbd "<leader><SPC>") 'projectile-find-file)
(evil-define-key 'normal 'global (kbd "<leader>pp") 'projectile-switch-project)
(evil-define-key 'normal 'global (kbd "<leader>pb") 'projectile-switch-to-buffer)

(use-package dockerfile-mode
:config
(add-to-list 'auto-mode-alist '("Dockerfile\\'" . dockerfile-mode)))

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

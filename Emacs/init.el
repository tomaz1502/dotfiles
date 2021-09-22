;;; package -- Summary
;;; Code:
;;; Commentary:
;; Remove welcome message
(setq inhibit-startup-message t)

(load-theme 'doom-Iosvkem t)

(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)

(set-face-attribute 'default nil :height 160)
(set-frame-font "JetBrains Mono" nil t)

(setq scroll-conservatively 101)
(setq mouse-wheel-scroll-amount '(3 ((shift) . 3))) ;; one line at a time
(setq mouse-wheel-progressive-speed nil) ;; don't accelerate scrolling
(setq mouse-wheel-follow-mouse 't) ;; scroll window under mouse
(setq scroll-step 1) ;; keyboard scroll one line at a time

(global-display-line-numbers-mode)
;; (setq display-line-numbers-type 'relative)

(require 'package)
(setq package-enable-at-startup nil)

(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)

(package-initialize)

(unless (package-installed-p 'use-package)
	(package-refresh-contents)
	(package-install 'use-package))

(setq use-package-always-ensure t)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("a9a67b318b7417adbedaab02f05fa679973e9718d9d26075c6235b1f0db703c8" "59ba50f24540958f33699a5247255d10f34dd812f3975837e3eddccdc4caa32e" "0c3b1358ea01895e56d1c0193f72559449462e5952bded28c81a8e09b53f103f" "bffa9739ce0752a37d9b1eee78fc00ba159748f50dc328af4be661484848e476" "16dd114a84d0aeccc5ad6fd64752a11ea2e841e3853234f19dc02a7b91f5d661" "6b5c518d1c250a8ce17463b7e435e9e20faa84f3f7defba8b579d4f5925f60c1" default))
 '(global-flycheck-mode t)
 '(helm-completion-style 'helm)
 '(lsp-dired-mode t nil (lsp-dired))
 '(package-selected-packages
   '(lsp-haskell vterm eterm-256color flycheck lsp-mode general company doom-themes rust-mode evil-smartparens smartparens pdf-tools projectile-ripgrep projectile spacemacs-theme lean-mode haskell-mode doom-modeline eshell-syntax-highlighting helm base16-theme magit key-chord evil use-package)))


(global-hl-line-mode t)
(global-company-mode t)
(smartparens-global-mode t)
(evil-mode 1)

(use-package company
  :defer
)

(use-package rust-mode
  :defer
)

(use-package smartparens
  :defer
)

(use-package evil-smartparens
  :defer
)

(use-package doom-themes
  :defer
)

(use-package pdf-tools
  :defer
)

(use-package evil)

(use-package org
  :defer
)

(use-package magit
  :defer
)

(use-package base16-theme
  :defer
)

(use-package recentf
  :config (setq recentf-max-menu-items 25)
  :init (recentf-mode 1)
)

(use-package eshell-syntax-highlighting
  :defer
  :after esh-mode
  :config (eshell-syntax-highlighting-global-mode +1)
)

(use-package helm
  :init
    (require 'helm-config)
    (setq helm-split-window-inside-p t
          helm-move-to-line-cycle-in-source t
	  helm-mode-line-string nil
	  helm-display-header-line nil)
  :config
    (helm-mode 1) ;; Most of Emacs prompts become helm-enabled
    (helm-autoresize-mode 1) ;; Helm resizes according to the number of candidates
    (global-set-key (kbd "C-x b") 'helm-buffers-list) ;; List buffers ( Emacs way )
    (global-set-key (kbd "C-x C-f") 'helm-find-files) ;; Finding files with Helm
    (global-set-key (kbd "M-x") 'helm-M-x)  ;; Improved M-x menu
    (global-set-key (kbd "C-x C-r") 'helm-recentf)
    (define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action)
)

(use-package all-the-icons)

(use-package doom-modeline
  :config (doom-modeline-mode 1)
)

(use-package projectile
  :defer
)

(use-package projectile-ripgrep
  :defer
)

(use-package which-key
  :config (which-key-mode)
)

(use-package key-chord
  :config (setq key-chord-two-keys-delay 0.5)
  :init
    (progn
      (key-chord-define evil-insert-state-map "jk" 'evil-normal-state)
      (key-chord-mode 1)
    )
)

(use-package lsp-mode
  :commands (lsp lsp-deferred)
  :hook (lsp-mode . efs/lsp-mode-setup)
  :init
  (setq lsp-keymap-prefix "C-l")
)

(use-package flycheck
  :config (global-flycheck-mode)
)

(use-package eterm-256color
  :hook (term-mode . eterm-256color-mode)
)

(use-package vterm
  :config (setq vterm-shell 'bash))

(use-package lsp-haskell)
(add-hook 'haskell-mode-hook #'lsp)
(add-hook 'haskell-literate-mode-hook #'lsp)
(add-hook 'lsp-mode-hook
    (define-key evil-normal-state-map (kbd "K") 'lsp-describe-thing-at-point)
)


;; Key Bindings
(global-set-key (kbd "C-x C-x") 'evil-mode)

(global-set-key (kbd "C-=") 'text-scale-increase)
(global-set-key (kbd "C--") 'text-scale-decrease)

(global-set-key (kbd "M-l") 'windmove-right)
(global-set-key (kbd "M-h") 'windmove-left)
(global-set-key (kbd "M-k") 'windmove-up)
(global-set-key (kbd "M-j") 'windmove-down)

(global-set-key (kbd "M-L") 'enlarge-window-horizontally)
(global-set-key (kbd "M-H") 'shrink-window-horizontally)
(global-set-key (kbd "M-K") 'enlarge-window)
(global-set-key (kbd "M-J") 'shrink-window)

(load-file (let ((coding-system-for-read 'utf-8))
                (shell-command-to-string "agda-mode locate")))

(setq initial-buffer-choice "~/Desktop/Main.org")

(add-hook 'pdf-view-mode-hook
  (lambda ()
    (local-set-key (kbd "j")  'pdf-view-next-line-or-next-page)
    (local-set-key (kbd "k")  'pdf-view-previous-line-or-previous-page)
    (local-set-key (kbd "gg") 'pdf-view-first-page)
    (local-set-key (kbd "G")  'pdf-view-last-page)
    (local-set-key (kbd "{")  'pdf-view-previous-page-command)
    (local-set-key (kbd "}")  'pdf-view-next-page-command)
  )
)

(add-hook 'doc-view-mode-hook
  (lambda ()
    (pdf-tools-install)
  )
)

(define-key evil-normal-state-map (kbd "-")
  (lambda () (interactive)
    (find-file default-directory)
  )
)

(add-hook 'dired-mode-hook
  (lambda ()
    (local-set-key (kbd "-") 'dired-up-directory)
  )
)
	    
(defun org-force-open-current-window ()
  "Force to open link in the current window, instaad of splitting."
  (interactive)
  (let ((org-link-frame-setup (quote
                               ((vm . vm-visit-folder)
                                (vm-imap . vm-visit-imap-folder)
                                (gnus . gnus)
                                (file . find-file)
                                (wl . wl)))
                              ))
    (org-open-at-point)))

(add-hook 'org-mode-hook
  (lambda ()
    (local-set-key (kbd "C-c C-o") 'org-force-open-current-window)
  )
)

(use-package general)

(general-evil-setup t)
(general-nvmap :states '(normal visual) :keymaps 'override :prefix "SPC"
  "e r"  '(lambda () (interactive) (load-file "~/.emacs.d/init.el"))
  "e f"  '(lambda () (interactive) (find-file "~/.emacs.d/init.el"))
  "SPC"  'projectile-find-file
  "f s"  'save-buffer
  "f f"  'helm-find-files
  "f d"  '(lambda () (interactive) (find-file default-directory))
  "f r"  'helm-recentf
  "b b"  'helm-buffers-list
  "b i"  'ibuffer
  "b d"  'kill-this-buffer
  "w \\" 'split-window-horizontally
  "w -"  'split-window-vertically
  "w d"  'delete-window
  "c c"  'comment-or-uncomment-region
)

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
)
(provide 'init)
;;; init.el ends here

;; Remove welcome message
;; (setq inhibit-startup-message t)

(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)

(set-face-attribute 'default nil :height 160)
(set-frame-font "Fira Mono" nil t)

(setq mouse-wheel-scroll-amount '(1 ((shift) . 1))) ;; one line at a time
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

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("16dd114a84d0aeccc5ad6fd64752a11ea2e841e3853234f19dc02a7b91f5d661" "6b5c518d1c250a8ce17463b7e435e9e20faa84f3f7defba8b579d4f5925f60c1" default))
 '(package-selected-packages
   '(eshell-syntax-highlighting helm base16-theme magit key-chord auto-complete evil use-package)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(global-hl-line-mode t)

(use-package evil
  :ensure t
  :config (evil-mode)
)

(use-package which-key
  :ensure t
  :config (which-key-mode)
)

(use-package auto-complete
  :ensure t
  :init
    (progn
	(ac-config-default)
	(global-auto-complete-mode t)
    )
)

(use-package key-chord
  :config (setq key-chord-two-keys-delay 0.5)
  :init
    (progn
      (key-chord-define evil-insert-state-map "jk" 'evil-normal-state)
      (key-chord-mode 1)
    )
  :ensure t)

(use-package magit
  :ensure t)

(use-package base16-theme
  :ensure t
  :config (load-theme 'base16-default-dark)
)

(use-package recentf
  :ensure t
  :config (setq recentf-max-menu-items 25)
  :init (recentf-mode 1)
)

(use-package eshell-syntax-highlighting
  :after esh-mode
  :demand t ;; Install if not already installed.
  :config
  ;; Enable in all Eshell buffers.
  (eshell-syntax-highlighting-global-mode +1)
)

(use-package helm
  :init
    (require 'helm-config)
    (setq helm-split-window-in-side-p t
          helm-move-to-line-cycle-in-source t
	  helm-mode-line-string nil
	  helm-display-header-line nil)
  :config 
    (helm-mode 1) ;; Most of Emacs prompts become helm-enabled
    (helm-autoresize-mode 1) ;; Helm resizes according to the number of candidates
    (global-set-key (kbd "C-x b") 'helm-buffers-list) ;; List buffers ( Emacs way )
    (define-key evil-ex-map "b" 'helm-buffers-list) ;; List buffers ( Vim way )
    (global-set-key (kbd "C-x r b") 'helm-bookmarks) ;; Bookmarks menu
    (global-set-key (kbd "C-x C-f") 'helm-find-files) ;; Finding files with Helm
    (global-set-key (kbd "M-x") 'helm-M-x)  ;; Improved M-x menu
    (global-set-key (kbd "C-x C-r") 'helm-recentf)
    (define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action)
  :ensure t
)

;; Key Bindings
(global-set-key (kbd "C-x C-x") 'evil-mode)
(global-set-key (kbd "M-a")     'neotree-toggle)
(global-set-key (kbd "C-x C-l") (lambda () (interactive) (find-file "~/.emacs.d/init.el")))

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

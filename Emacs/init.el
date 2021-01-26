;; Remove welcome message
(setq inhibit-startup-message t)

;; Remove menus
(tool-bar-mode -1)
(menu-bar-mode -1)

;; Remove scrolling bar
(scroll-bar-mode -1)

(set-face-attribute 'default nil :height 120)

(set-default-font "Fira Mono 14")
;; (set-frame-font "Source Code Pro" nil t)

;; theme
;;(load-theme 'base16-default-dark)

;; line numbers
(global-display-line-numbers-mode)
(setq display-line-numbers-type 'relative)

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
   (quote
    ("16dd114a84d0aeccc5ad6fd64752a11ea2e841e3853234f19dc02a7b91f5d661" "6b5c518d1c250a8ce17463b7e435e9e20faa84f3f7defba8b579d4f5925f60c1" default)))
 '(package-selected-packages
   (quote
    (base16-theme gruvbox-theme emacs-gruvbox-themes magit key-chord all-the-icons neotree auto-complete evil use-package))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

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

(use-package neotree
  :config (setq neo-theme (if (display-graphic-p) 'icons 'arrow))
  :ensure t)

(use-package all-the-icons
  :ensure t)

(use-package key-chord
  :ensure t)

(use-package magit
  :ensure t)

(use-package gruvbox-theme
  :ensure t)

(use-package base16-theme
  :ensure t
  :config
  (load-theme 'base16-default-dark)
)

;; Key Bindings
(global-set-key (kbd "C-x C-x") 'evil-mode)
(global-set-key (kbd "M-a")     'neotree-toggle)
(global-set-key (kbd "C-x C-l") (lambda () (interactive) (find-file "~/.emacs.d/init.el")))

(global-set-key (kbd "M-l") 'windmove-right) 
(global-set-key (kbd "M-h") 'windmove-left) 
(global-set-key (kbd "M-k") 'windmove-up) 
(global-set-key (kbd "M-j") 'windmove-down) 

(global-set-key (kbd "M-L") 'enlarge-window-horizontally)
(global-set-key (kbd "M-H") 'shrink-window-horizontally)
(global-set-key (kbd "M-K") 'enlarge-window)
(global-set-key (kbd "M-J") 'shrink-window)

;; jk to enter normal mode in evil
(setq key-chord-two-keys-delay 0.5)
(key-chord-define evil-insert-state-map "jk" 'evil-normal-state)
(key-chord-mode 1)

;; Unable evil-mode mappings while in neotree
(evil-define-key 'normal neotree-mode-map (kbd "TAB") 'neotree-enter)
(evil-define-key 'normal neotree-mode-map (kbd "SPC") 'neotree-quick-look)
(evil-define-key 'normal neotree-mode-map (kbd "q") 'neotree-hide)
(evil-define-key 'normal neotree-mode-map (kbd "RET") 'neotree-enter)
(evil-define-key 'normal neotree-mode-map (kbd "g") 'neotree-refresh)
(evil-define-key 'normal neotree-mode-map (kbd "n") 'neotree-next-line)
(evil-define-key 'normal neotree-mode-map (kbd "p") 'neotree-previous-line)
(evil-define-key 'normal neotree-mode-map (kbd "A") 'neotree-stretch-toggle)
(evil-define-key 'normal neotree-mode-map (kbd "H") 'neotree-hidden-file-toggle)

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
(load-theme 'deeper-blue)

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
 '(package-selected-packages
   (quote
    (key-chord all-the-icons neotree auto-complete evil use-package))))
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


(setq key-chord-two-keys-delay 0.5)
(key-chord-define evil-insert-state-map "jk" 'evil-normal-state)
(key-chord-mode 1)

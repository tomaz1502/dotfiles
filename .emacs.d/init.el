;; Basic
;; 
(setq inhibit-startup-message t)

(load-theme 'wombat t)

(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)			;

(set-face-attribute 'default nil :height 160)
(set-frame-font "Fira Mono" nil t)

(setq scroll-conservatively 101)
(setq mouse-wheel-scroll-amount '(3 ((shift) . 3))) ;; one line at a time
(setq mouse-wheel-progressive-speed nil) ;; don't accelerate scrolling
(setq mouse-wheel-follow-mouse 't) ;; scroll window under mouse
(setq scroll-step 1) ;; keyboard scroll one line at a time

(global-display-line-numbers-mode 1)

;; Package

(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)

(unless (package-installed-p 'use-package)
	(package-install 'use-package))

(setq use-package-always-ensure t)

(use-package evil
  :config (evil-mode)
)

(use-package smartparens
  :config (smartparens-global-mode t)
)

(use-package ivy
  :config
    (ivy-mode)
    (setq ivy-re-builders-alist '((t . ivy--regex-fuzzy)))
)

(use-package flx
  :defer) ; help sorting results in Ivy

(defun smart-comment ()
  "Comment or uncomment region if selected, otherwise comment or uncomment current line."
  (interactive)
  (if (use-region-p)
      (comment-or-uncomment-region (region-beginning) (region-end))
    (comment-line 1)))

(use-package general
  :config
    (general-evil-setup t)
    (general-nvmap :states '(normal visual) :keymaps 'override :prefix "SPC"
      "e r"  '(lambda () (interactive) (load-file "~/.emacs.d/init.el"))
      "e f"  '(lambda () (interactive) (find-file "~/.emacs.d/init.el"))
      ;; "SPC"  'projectile-find-file
      "f s"  'save-buffer
      "."  'find-file
      ;; "f r"  'helm-recentf
      "b l"  'evil-switch-to-windows-last-buffer
      "b b"  'ivy-switch-buffer
      "b d"  'kill-this-buffer
      "w s"  'split-window-horizontally
      "w v"  'split-window-vertically
      "w d"  'delete-window
      "w l"  'windmove-right
      "w k"  'windmove-up
      "w j"  'windmove-down
      "w h"  'windmove-left
      "c c"  'smart-comment
    )
    (general-nvmap :states '(normal visual) :keymaps 'override :prefix "g"
      "c c" 'smart-comment
    )
)

;; Keymaps

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

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ispell-dictionary nil)
 '(package-selected-packages '(general flx ivy smartparens use-package evil cmake-mode)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

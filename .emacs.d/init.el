;; Basic

(setq inhibit-startup-message t)

; (load-theme 'wombat t)

(tool-bar-mode -1)
(scroll-bar-mode -1)			;
(menu-bar-mode -1)

(set-face-attribute 'default nil :height 160)
(set-frame-font "Fira Mono" nil t)

(setq scroll-conservatively 101)
(setq mouse-wheel-scroll-amount '(3 ((shift) . 3)))
(setq mouse-wheel-progressive-speed nil)
(setq mouse-wheel-follow-mouse 't)
(setq scroll-step 1)

(setq make-backup-files nil)
(global-display-line-numbers-mode 1)

;; Auxiliary Functions

(setq notes-dir "~/Tomaz/org/")

;; Surprisingly, can't declare both variables in the same let-statement, `files`
;; would not be in scope.
(defun find-note ()
  (interactive)
  (let ((files (directory-files-recursively notes-dir ".*.org")))
    (let ((rel-files (mapcar (lambda (file) (file-relative-name file notes-dir)) files)))
	(ivy-read "Find note: " rel-files
	  :action (lambda (file) (find-file (expand-file-name file notes-dir))))
    )
  )
)

;; Note: Creates the directory if it does not exist. Does not create the
;; file if you do not save it. This is to avoid replacing an existing file.
(defun create-note ()
  (interactive)
  (let ((filename (read-string "New note name: ")))
    (let ((filedir (concat notes-dir (file-name-directory filename))))
        (unless (file-directory-p filedir) (make-directory filedir t))
	(find-file (expand-file-name (concat filename ".org") notes-dir))
      )
  )
)

(defun open-notes-dir ()
    (interactive)
    (find-file notes-dir)
)

;; Package

(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)

(unless (package-installed-p 'use-package)
	(package-install 'use-package))

(setq use-package-always-ensure t)

(use-package evil
  :config (evil-mode)
)

(use-package company
  :bind (:map company-active-map
	      ("<tab>" . company-select-next)
	      ("<backtab>" . company-select-previous)))
(global-company-mode t) ; buggy if I do this and the binding withing use-package?

(use-package smartparens
  :config (smartparens-global-mode)
)

(use-package ivy
  :config
    (ivy-mode)
    (setq ivy-re-builders-alist '((t . ivy--regex-fuzzy))) ;; Allow fuzzy-finding on Ivy
)

(use-package flx
  :defer) ; help sorting results in Ivy

(use-package key-chord
  :config
    (key-chord-mode 1)
    (add-hook 'evil-mode-hook
      (lambda ()
	(if evil-mode
	  (key-chord-mode 1)
	  (key-chord-mode -1)
	)))
    (key-chord-define-global "jk" 'evil-normal-state)
  )

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
      "."    'find-file
      "f f"  'find-file
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
      "n n"  'find-note
      "n c"  'create-note
      "n o"  'open-notes-dir
    )
    (general-nvmap :states '(normal visual) :keymaps 'override :prefix "g"
      "c c" 'smart-comment
    )
)

(use-package pdf-tools
  :defer
  :config
    (add-hook 'doc-view-mode-hook
      (lambda ()
        (pdf-tools-install)
      )
    )

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

(global-set-key (kbd "M-;") 'eval-expression) ; "M-:" is not working, why?

; (add-to-list 'load-path "/home/tomazgomes/.emacs.d/nano-emacs")
; (require 'nano)

; (menu-bar-mode -1) ;; why does nano changes this back?


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ispell-dictionary nil)
 '(package-selected-packages
   '(company key-chord key-combo pdf-tools general flx ivy smartparens use-package evil cmake-mode)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

  (setq user-full-name "Tomaz Gomes Mascarenhas"
        user-mail-address "tomgm1502@gmail.com")

  (set-face-attribute 'default nil :height 180)
  (set-frame-font "Fira Mono" nil t)
  (setq doom-theme 'doom-Iosvkem)
  (after! vertico
    (setq vertico-count 10)
  )

(map! "M-l" #'windmove-right
      "M-h" #'windmove-left
      "M-j" #'windmove-down
      "M-k" #'windmove-up
      "M-L" #'enlarge-window-horizontally
      "M-H" #'shrink-window-horizontally
      "M-K" #'enlarge-window
      "M-J" #'shrink-window
      "C-x C-x" #'evil-mode
)

(map! :leader
      "f P" #'(lambda () (interactive) (find-file "~/.doom.d/setup.org"))
      "r a" #'align-regexp
      "T"   #'neotree-toggle
)

(define-key evil-normal-state-map (kbd "-")
  (lambda () (interactive)
    (find-file default-directory)
  )
)

(setq vterm-shell "/usr/bin/zsh")

(load-file (let ((coding-system-for-read 'utf-8))
                (shell-command-to-string "agda-mode locate")))

  (add-hook! pdf-view-mode
    (lambda ()
        (local-set-key (kbd "j")  'pdf-view-next-line-or-next-page)
        (local-set-key (kbd "k")  'pdf-view-previous-line-or-previous-page)
        (local-set-key (kbd "gg") 'pdf-view-first-page)
        (local-set-key (kbd "G")  'pdf-view-last-page)
        (local-set-key (kbd "{")  'pdf-view-previous-page-command)
        (local-set-key (kbd "}")  'pdf-view-next-page-command)
      )
    )

;; (add-to-list 'load-path "/home/tomazgomes/Tools/lean-mode")
;; (require 'lean-mode)
;; (require 'company-lean)
;; (require 'helm-lean)
(setq lean4-autodetect-lean3 t)

(add-hook! LaTeX-mode
  (setq compile-command
    (format "pdflatex %s"
      (file-name-nondirectory (kill-new buffer-file-name))))
  (display-line-numbers-mode 0)
)

  (add-hook! org-mode
    (display-line-numbers-mode 0)   )

  (setq doom-scratch-initial-major-mode 'org-mode)
  (setq initial-buffer-choice "~/Desktop/Org")
  (global-visual-line-mode t)
  (add-to-list 'custom-theme-load-path "~/.emacs.d/themes")

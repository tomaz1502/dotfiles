;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets. It is optional.
;; (setq user-full-name "John Doe"
;;       user-mail-address "john@doe.com")

;; Doom exposes five (optional) variables for controlling fonts in Doom:
;;
;; - `doom-font' -- the primary font to use
;; - `doom-variable-pitch-font' -- a non-monospace font (where applicable)
;; - `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;; - `doom-symbol-font' -- for symbols
;; - `doom-serif-font' -- for the `fixed-pitch-serif' face
;;
;; See 'C-h v doom-font' for documentation and more examples of what they
;; accept. For example:
;;
;;(setq doom-font (font-spec :family "Fira Code" :size 12 :weight 'semi-light)
;;      doom-variable-pitch-font (font-spec :family "Fira Sans" :size 13))
;;
;; If you or Emacs can't find your font, use 'M-x describe-font' to look them
;; up, `M-x eval-region' to execute elisp code, and 'M-x doom/reload-font' to
;; refresh your font settings. If Emacs still can't find your font, it likely
;; wasn't installed correctly. Font issues are rarely Doom issues!

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'doom-one)

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type t)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/org/")


;; Whenever you reconfigure a package, make sure to wrap your config in an
;; `after!' block, otherwise Doom's defaults may override your settings. E.g.
;;
;;   (after! PACKAGE
;;     (setq x y))
;;
;; The exceptions to this rule:
;;
;;   - Setting file/directory variables (like `org-directory')
;;   - Setting variables which explicitly tell you to set them before their
;;     package is loaded (see 'C-h v VARIABLE' to look up their documentation).
;;   - Setting doom variables (which start with 'doom-' or '+').
;;
;; Here are some additional functions/macros that will help you configure Doom.
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c c k').
;; This will open documentation for it, including demos of how they are used.
;; Alternatively, use `C-h o' to look up a symbol (functions, variables, faces,
;; etc).
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.


(setq user-full-name "Tomaz Gomes Mascarenhas"
      user-mail-address "tomgm1502@gmail.com")

(set-face-attribute 'default nil :height 120)
(set-frame-font "Fira Mono" nil t)
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
      "1"   #'(lambda () (interactive) (+workspace/switch-to-0)) ; alt-n does not work well in vterm
      "2"   #'(lambda () (interactive) (+workspace/switch-to-1))
      "3"   #'(lambda () (interactive) (+workspace/switch-to-2))
      "4"   #'(lambda () (interactive) (+workspace/switch-to-3))
      "5"   #'(lambda () (interactive) (+workspace/switch-to-4))
      "6"   #'(lambda () (interactive) (+workspace/switch-to-5))
)

(define-key evil-normal-state-map (kbd "-")
  (lambda () (interactive)
    (find-file default-directory)
  )
)

(setq vterm-shell "/usr/bin/bash")

(load-file (let ((coding-system-for-read 'utf-8))
               (shell-command-to-string "agda-mode locate")))

(after! pdf-tools (setq pdf-view-use-scaling nil)) ; for high resolution display
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

(add-hook! LaTeX-mode
  (setq compile-command
    (format "pdflatex -shell-escape %s"
      (file-name-nondirectory (kill-new buffer-file-name))))
  (display-line-numbers-mode 0)
)

(add-hook! org-mode
  (display-line-numbers-mode 0)
)
(setq org-agenda-span 30)

(setq org-startup-with-inline-images t)
(setq doom-scratch-initial-major-mode 'org-mode)
(global-visual-line-mode t)
(setq org-todo-keyword-faces
      (quote (("TODO" :foreground "red" :weight bold)
              ("NEXT" :foreground "light blue" :weight bold)
              ("DONE" :foreground "forest green" :weight bold)
              ("WAITING" :foreground "orange" :weight bold)
              ("HOLD" :foreground "magenta" :weight bold)
              ("CANCELLED" :foreground "#666666" :weight bold)
              ("TEST" :foreground "yellow" :weight bold)
              ("STALLED" :foreground "dark yellow" :weight bold)
              ("CTE" :foreground "lightslategray")
              ("ASK" :foreground "forest blue" :weight bold))))

(defun eval-print-region ()
  (interactive)
  (if (use-region-p)
      (eval-region (region-beginning) (region-end) standard-output)
      (message "No region selected"))
)

;;; Settings
;; Window Settings
;(menu-bar-mode -1)  ; Disable the menu bar
(scroll-bar-mode -1) ; Disable visible scrollbar
(tooltip-mode -1)    ; Disable tooltips
(set-fringe-mode 4)  ; Decrease sidebar width
(tool-bar-mode -1)   ; Disable the toolbar

;; Feature Settings
(setq inhibit-startup-message t)              ; Disable landing page
(setq ring-bell-function 'ignore)             ; Disable alarms
(setq gc-cons-threshold 20000000)             ; Slower garbage collection
(setq make-backup-files nil)                  ; Do not backup files
(setq large-file-warning-threshold 100000000) ; Warn for large files limit

;; Appearance
(load-theme 'wombat)
(set-face-attribute 'default nil :font "Consolas") ; :height (int)

;;; Behavior Tweaks

;; Esc to C-g
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

;; Y/N instead of Yes/No
(fset 'yes-or-no-p 'y-or-n-p)

;; Custom.el
(setq custom-file "~/.emacs.d/custom-settings.el")
(load custom-file t)

;; Auto Save to /tmp
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

;; Dired no new buffer on 'a'
(put 'dired-find-alternate-file 'disabled nil)

;;; Load Package Sources
(require 'package)
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("org" . "https://orgmode.org/elpa/")
			 ("marmalade" . "https://marmalade-repo.org/packages/")
                         ("gnu" . "https://elpa.gnu.org/packages/")))
(package-initialize)
(unless package-archive-contents
 (package-refresh-contents))

;;; Packages
;; Install List
(defvar my-packages '(all-the-icons
		      company
		      doom-modeline
		      flycheck
		      ivy
		      meghanada
		      use-package
		      yasnippet))
;; Install Packages
(dolist (p my-packages)
  (unless (package-installed-p p)
    (package-refresh-contents)
    (package-install p)))
;; Init use-package
(require 'use-package)
(setq use-package-always-ensure t)

;;; Configure Packages

;; Common Lisp Compatability
(require 'cl-lib)

;; All-The-Icons
(use-package all-the-icons)
(unless (member "all-the-icons" (font-family-list))
  (all-the-icons-install-fonts t))

;; Company-Mode
(use-package company
  :diminish
  :init (global-company-mode))

;; Doom-Modeline
(use-package doom-modeline
  :init (doom-modeline-mode 1)
  :custom ((doom-modeline-height 15)))

;; Ivy
(use-package ivy
  :diminish
  :config (ivy-mode 1))

;; Meghanada
(use-package meghanada)

;; Yasnippet
(use-package yasnippet
  :diminish
  :config
  (add-to-list 'load-path
              "~/.emacs.d/plugins/yasnippet")
  (yas-global-mode 1))

;;; Language Hooks
(add-hook 'java-mode-hook
          (lambda ()
            (meghanada-mode t)
            (flycheck-mode +1)
            (setq c-basic-offset 2)
            (add-hook 'before-save-hook 'meghanada-code-beautify-before-save)))

;;; End init.el
(provide 'init)

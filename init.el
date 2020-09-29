;; Startup Optimizations
(setq gc-cons-threshold most-positive-fixnum)
(setq gc-cons-percentage 0.6)

;; Window Settings
(menu-bar-mode -1)        ; Disable the menu bar
(scroll-bar-mode -1)      ; Disable visible scrollbar
(tooltip-mode -1)         ; Disable tooltips
(set-fringe-mode 4)       ; Decrease sidebar width
(tool-bar-mode -1)        ; Disable the toolbar
(toggle-frame-fullscreen) ; Start in fullscreen
(set-frame-parameter (selected-frame) 'alpha '(95 . 95))

;; Feature Settings
(setq inhibit-startup-message t)              ; Disable landing page
(setq ring-bell-function 'ignore)             ; Disable alarms
(setq make-backup-files nil)                  ; Do not backup files
(setq large-file-warning-threshold 100000000) ; Warn for large files limit

;; Variable Fonts
(set-face-attribute 'default nil :font "Fira Code Retina:antialias=subpixel")
(set-face-attribute 'fixed-pitch nil :font "Fira Code Retina")
(set-face-attribute 'variable-pitch nil :font "Source Sans Pro" :height 120)

;; Line numbers
(column-number-mode)
(global-display-line-numbers-mode t)
(dolist (mode '(org-mode-hook
                term-mode-hook
		shell-mode-hook
                eshell-mode-hook
		pdf-view-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

;; Esc to C-g
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

;; Y/N instead of Yes/No
(fset 'yes-or-no-p 'y-or-n-p)

;; Custom.el
(setq custom-file "~/.emacs.d/custom.el")
(load custom-file t)

;; Auto Save to /tmp
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

;; Dired no new buffer on 'a'
(put 'dired-find-alternate-file 'disabled nil)

;; Package
(require 'package)
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("org" . "https://orgmode.org/elpa/")
                         ("gnu" . "https://elpa.gnu.org/packages/")))
(package-initialize)
(unless package-archive-contents
 (package-refresh-contents))     ; Refresh archive, run periodically

;; Use-Package
(unless (package-installed-p 'use-package) ; Check if use-package is installed
   (package-install 'use-package))         ; Install use-package
(require 'use-package)                     ; Init use-package
(setq use-package-always-ensure t)         ; Make sure packages install

;; Quelpa
(use-package quelpa
  :custom (quelpa-update-melpa-p nil))
(use-package quelpa-use-package
  :config (quelpa-use-package-activate-advice))

;;; Packages
;; All-The-Icons
(use-package all-the-icons)
(unless (member "all-the-icons" (font-family-list))
  (all-the-icons-install-fonts t))

;; Company
(use-package company
  :diminish
  :init (global-company-mode)
  :config (add-to-list 'company-backends 'company-powershell)
  :custom (company-idle-delay 0.3))

;; Company-Powershell
(use-package company-powershell
  :ensure quelpa
  :quelpa (company-powershell
	    :fetcher github
	    :repo "N8-Doge/company-powershell"
	    :files ":defaults" "*.ps1"))

;; Cl-Lib
(use-package cl-lib)

;; Counsel
(use-package counsel)

;; Doom-Modeline
(use-package doom-modeline
  :init (doom-modeline-mode 1)
  :custom (doom-modeline-height 15))

;; Doom-Themes
(use-package doom-themes
  :init (load-theme 'doom-palenight))

;; Elcord
(use-package elcord
  :init (elcord-mode)
  :config
    (defun elcord--details-and-state ()
      "No line numbers"
      (let ((activity (list
        (cons "details" (format "Editing %s" (buffer-name)))
        (cons "state" (format "Major mode: %s" (elcord--mode-text))))))
        (when elcord-display-elapsed
          (push (list "timestamps" (cons "start" elcord--startup-time)) activity))
        activity))
  :custom ((elcord-use-major-mode-as-main-icon 't)))

;; Flycheck
(use-package flycheck
  :hook (java-mode . flycheck-mode))

;; GCMH
(use-package gcmh
  :diminish
  :hook (emacs-startup . gcmh-mode))

;; Helpful
(use-package helpful
  :custom
    (counsel-describe-function-function #'helpful-callable)
    (counsel-describe-variable-function #'helpful-variable))

;; Ivy
(use-package ivy
  :diminish
  :init (ivy-mode 1)
  :bind (("C-s" . swiper))
  :custom (ivy-count-format "(%d/%d) "))

;; Ivy-Rich
(use-package ivy-rich
  :init (ivy-rich-mode 1))

;; Meghanada
(use-package meghanada
  :hook (java-mode . meghanada-mode))

;; Mixed-Pitch
(use-package mixed-pitch
  :custom (mixed-pitch-set-height 12)
  :hook (org-mode . mixed-pitch-mode))

;; Org
(use-package org
  :custom (org-hide-emphasis-markers t))

;; Org-Bullets
(use-package org-bullets
  :hook (org-mode . org-bullets-mode))

;; PDF-Continuous-Scroll-Mode
;(use-package pdf-continuous-scroll-mode
; :after (pdf-tools)
; :ensure quelpa
; :quelpa (pdf-continuous-scroll-mode
;    :fetcher github
;    :repo "dalanicolai/pdf-continuous-scroll-mode.el")
; :hook (pdf-view-mode-hook . pdf-continuous-scroll-mode))

;; PDF-tools
(use-package pdf-tools
  :config (pdf-loader-install))

;; Powershell
(use-package powershell
  :custom (powershell-indent 2))

;; Rainbow Delimiters
(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

;; Which-Key
(use-package which-key
  :diminish
  :init (which-key-mode)
  :custom (which-key-idle-delay 5))

;; Yasnippet
(use-package yasnippet
  :diminish
  :init (yas-global-mode 1)
  :config (add-to-list 'load-path "~/.emacs.d/plugins/yasnippet"))

;; Java Hook
(add-hook 'java-mode-hook
	  (lambda ()
	    (setq c-basic-offset 2)
	    (add-hook 'before-save-hook 'meghanada-code-beautify-before-save)))

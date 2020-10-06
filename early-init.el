;; Prevent package from initializing
(setq package-enable-at-startup nil)

;; Disable visual components
(setq tooltip-mode nil
      tool-bar-mode nil
      menu-bar-mode nil
      scroll-bar-mode nil)

;; Set other GUI components
(set-fringe-mode 4)

;; Set garbage collection high
(setq gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 0.6)

;; Don't resize frame before font load
(setq frame-inhibit-implied-resize t)

;; Don't compile packages at runtime
(setq comp-deferred-compilation nil)

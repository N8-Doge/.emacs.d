;; Startup Optimizations
(setq gc-cons-threshold most-positive-fixnum)

;; Do not initialise the package manager until (package-initialize)
(setq package-enable-at-startup nil)

;; Allow loading from the package cache.
(setq package-quickstart t)

;; Do not resize the frame at this early stage.
(setq frame-inhibit-implied-resize t)

;; DisableUnnecessaryInterface
(menu-bar-mode -1)
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)

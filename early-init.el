;; Startup Optimizations
(setq gc-cons-threshold most-positive-fixnum)

;; DisableUnnecessaryInterface
(menu-bar-mode -1)
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)

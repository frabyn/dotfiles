;;; init.el ---  -*- lexical-binding: t; -*-

;; Author: Franklin Bynum

;;; Commentary:

;; This is the early init file

;;; Code:

;; Suppress UI elements before first frame renders
(push '(fullscreen . maximized) default-frame-alist)
(push '(menu-bar-lines . 0) default-frame-alist)
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)

;; Prevent GC during init for faster startup
(setq gc-cons-threshold most-positive-fixnum)

;;; _
(provide 'early-init)
;;; early-init.el ends here

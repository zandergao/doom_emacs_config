;;; lisp/init-org.el -*- lexical-binding: t; -*-

(use-package! pangu-spacing
  :config
  (global-pangu-spacing-mode 1)
  ;; 在中英文符号之间, 真正地插入空格
  (setq pangu-spacing-real-insert-separtor t))

(use-package! org-roam
  :config
  (setq org-roam-directory "/home/gaozhan/org/roam")
  )

(provide 'init-org)

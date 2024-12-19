;;; lisp/init-org.el -*- lexical-binding: t; -*-

(add-hook 'org-mode-hook #'org-num-mode)
(require 'org-bars)
(add-hook 'org-mode-hook #'org-bars-mode)

;; 在 Source Block 中像在语言 mode 中一样的缩进
(after! org
  (setq org-src-tab-acts-natively t)
  (setq org-src-preserve-indentation nil))

(use-package! pangu-spacing
  :hook (org-mode . pangu-spacing-mode)  ;; 仅在 org-mode 中启用 pangu-spacing
  :config
  ;;(global-pangu-spacing-mode 1)
  ;; 在中英文符号之间, 真正地插入空格
  (setq pangu-spacing-real-insert-separtor t))

(use-package! org-roam
  :config
  (setq org-roam-directory "/home/gaozhan/org/roam")
  (setq org-roam-capture-templates
      '(("d" "default" plain "%?"
         :target (file+head "%<%Y%m%d%H%M%S>-${slug}.org"
                            "#+author: %(user-full-name)\n#+date: %U\n#+title: ${title}\n")
         :unnarrowed t)))
  )

(provide 'init-org)

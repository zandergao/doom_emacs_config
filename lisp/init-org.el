;;; lisp/init-org.el -*- lexical-binding: t; -*-

(add-hook 'org-mode-hook #'org-num-mode)
(require 'org-bars)
(add-hook 'org-mode-hook #'org-bars-mode)

(use-package! org
  :config
  (setq org-hide-emphasis-markers t) ;; 不显示强调符
  ;; 启用自动显示图片
  (setq org-startup-with-inline-images t)
  (add-hook 'org-mode-hook #'org-display-inline-images)
  ;; 在 Source Block 中像在语言 mode 中一样的缩进
  (setq org-src-tab-acts-natively t)
  (setq org-src-preserve-indentation nil)
  )

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

(defun my/wrap-with-char (char)
  "Wrap the word at point or the selected region with CHAR for org-mode emphasis."
  (interactive "cEnter character to wrap with: ")
  (save-excursion
    (if (use-region-p)
        ;; Wrap the selected region
        (let ((start (region-beginning))
              (end (region-end)))
          (goto-char start)
          (insert char)
          (goto-char (+ end 1))
          (insert char))
      ;; Wrap the word at point
      (let ((bounds (bounds-of-thing-at-point 'word)))
        (when bounds
          (let ((start (car bounds))
                (end (cdr bounds)))
            (goto-char start)
            (insert char)
            (goto-char (1+ end))
            (insert char)))))))

(use-package org-html-themify
  :hook (org-mode . org-html-themify-mode)
  :custom
  (org-html-themify-themes
   '((light . doom-nord-light)
     (dark . doom-vibrant)))
  )

(use-package! websocket
    :after org-roam)

(use-package! org-roam-ui
    :after org-roam ;; or :after org
;;         normally we'd recommend hooking orui after org-roam, but since org-roam does not have
;;         a hookable mode anymore, you're advised to pick something yourself
;;         if you don't care about startup time, use
;;  :hook (after-init . org-roam-ui-mode)
    :config
    (setq org-roam-ui-sync-theme t
          org-roam-ui-follow t
          org-roam-ui-update-on-save t
          org-roam-ui-open-on-start t))

(provide 'init-org)
;;; init-org.el ends here

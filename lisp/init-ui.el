;;; lisp/init-ui.el -*- lexical-binding: t; -*-

(cnfonts-mode 1)

(add-hook! 'window-setup-hook #'toggle-frame-maximized)

(display-time-mode t)             ; 开启时间状态栏

;; 彩虹猫 UI
(use-package! nyan-mode
  :hook (after-init . nyan-mode)
  :config
  (setq nyan-animate-nyancat t
        nyan-wavy-trail t
        nyan-cat-face-number 4
        nyan-bar-length 16
        nyan-minimum-window-width 64)
  (add-hook! 'doom-modeline-hook #'nyan-mode))

;; 标题
(setq! frame-title-format
      '("%b – Doom Emacs"
        (:eval
         (let ((project-name (projectile-project-name)))
           (unless (string= "-" project-name)
             (format "  -  [%s]" project-name))))))

;; 窗口标题, 被修改过显示*
(setq frame-title-format
      '("Doom Emacs: "
        (:eval
         (if (string-match-p (regexp-quote (or (bound-and-true-p org-roam-directory) "\u0000"))
                             (or buffer-file-name ""))
             (replace-regexp-in-string
              ".*/[0-9]*-?" "☰ "
              (subst-char-in-string ?_ ?\s buffer-file-name))
           "%b"))
        (:eval
         (when-let ((project-name (and (featurep 'projectile) (projectile-project-name))))
           (unless (string= "-" project-name)
             (format (if (buffer-modified-p)  " * %s" " ● %s") project-name))))))

(provide 'init-ui)

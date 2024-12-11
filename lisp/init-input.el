;;; lisp/init-input.el -*- lexical-binding: t; -*-

(use-package! rime
  :config
  (setq rime-user-data-dir "~/.local/share/fcitx5/rime/")
  (setq rime-posframe-properties
      (list :background-color "#333333"
            :foreground-color "#dcdccc"
            :internal-border-width 10))
  (setq default-input-method "rime"
      rime-show-candidate 'posframe)
  (setq rime-translate-keybindings
        '("C-f" "C-b" "C-n" "C-p" "C-g" "<left>" "<right>" "<up>" "<down>" "<prior>" "<next>" "<delete>"))
  :bind
  (:map rime-mode-map
        ("C-`" . 'rime-send-keybinding))
  )


(provide 'init-input)

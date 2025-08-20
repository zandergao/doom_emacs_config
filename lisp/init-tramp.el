;;; init-tramp.el --- Tramp configuration -*- lexical-binding: t; -*-

;; tramp 基本配置

(setq tramp-use-ssh-controlmaster-options nil)
(setq tramp-chunksize 2000) ; 增大数据块大小，加快传输速度
(with-eval-after-load 'tramp
  (add-to-list 'tramp-remote-path 'tramp-own-remote-path) ; 使用远程主机的默认 PATH
  (add-to-list 'tramp-remote-path "/home/gaozhan/.local/bin")) ; 添加自定义路径

(setq remote-file-name-inhibit-locks t
      tramp-use-scp-direct-remote-copying t
      remote-file-name-inhibit-auto-save-visited t)
(setq tramp-copy-size-limit (* 1024 1024) ;; 1MB
      tramp-verbose 2)

(connection-local-set-profile-variables
 'remote-direct-async-process
 '((tramp-direct-async-process . t)))

(connection-local-set-profiles
 '(:application tramp :protocol "scp")
 'remote-direct-async-process)

(setq magit-tramp-pipe-stty-settings 'pty)

(setq magit-remote-git-executable "/home/gaoz/.local/git/bin/git")

(with-eval-after-load 'tramp
  (with-eval-after-load 'compile
    (remove-hook 'compilation-mode-hook #'tramp-compile-disable-ssh-controlmaster-options)))

(remove-hook 'evil-insert-state-exit-hook #'doom-modeline-update-buffer-file-name)
(remove-hook 'find-file-hook #'doom-modeline-update-buffer-file-name)
(remove-hook 'find-file-hook 'forge-bug-reference-setup)

(provide 'init-tramp)
;;; init-tramp.el ends here

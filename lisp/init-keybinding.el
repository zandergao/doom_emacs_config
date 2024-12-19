;;; lisp/init-keybinding.el -*- lexical-binding: t; -*-

(global-set-key (kbd "<f12>") 'my/yank-image-from-win-clipboard-through-powershell)

(map! :leader
      :desc "sdcv search word" "s w" #'sdcv-search-pointer+)

(provide 'init-keybinding)

;;; lisp/init-keybinding.el -*- lexical-binding: t; -*-

;;; Code:
(global-set-key (kbd "<f12>") 'my/yank-image-from-win-clipboard-through-powershell)

(map! :leader
      :desc "sdcv search word" "s w" #'sdcv-search-pointer+
      ;; buffer
      :desc "Previous Buffer" "b <left>" #'switch-to-prev-buffer
      :desc "Next Buffer" "b <right>" #'switch-to-next-buffer
      ;; my function
      :desc "searche with path" "m s" #'my/consult-rg-with-path
      :desc "finf file with path" "m f" #'my/consult-fd-with-path
      )

(provide 'init-keybinding)
;;; init-keybinding.el ends here

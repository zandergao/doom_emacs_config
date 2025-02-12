;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets. It is optional.
;; (setq user-full-name "John Doe"
;;       user-mail-address "john@doe.com")

;; Doom exposes five (optional) variables for controlling fonts in Doom:
;;
;; - `doom-font' -- the primary font to use
;; - `doom-variable-pitch-font' -- a non-monospace font (where applicable)
;; - `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;; - `doom-symbol-font' -- for symbols
;; - `doom-serif-font' -- for the `fixed-pitch-serif' face
;;
;; See 'C-h v doom-font' for documentation and more examples of what they
;; accept. For example:
;;
;;(setq doom-font (font-spec :family "Fira Code" :size 12 :weight 'semi-light)
;;      doom-variable-pitch-font (font-spec :family "Fira Sans" :size 13))
;;
;; If you or Emacs can't find your font, use 'M-x describe-font' to look them
;; up, `M-x eval-region' to execute elisp code, and 'M-x doom/reload-font' to
;; refresh your font settings. If Emacs still can't find your font, it likely
;; wasn't installed correctly. Font issues are rarely Doom issues!

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'doom-vibrant)

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type 'relative)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/org/")


;; Whenever you reconfigure a package, make sure to wrap your config in an
;; `after!' block, otherwise Doom's defaults may override your settings. E.g.
;;
;;   (after! PACKAGE
;;     (setq x y))
;;
;; The exceptions to this rule:
;;
;;   - Setting file/directory variables (like `org-directory')
;;   - Setting variables which explicitly tell you to set them before their
;;     package is loaded (see 'C-h v VARIABLE' to look up their documentation).
;;   - Setting doom variables (which start with 'doom-' or '+').
;;
;; Here are some additional functions/macros that will help you configure Doom.
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c c k').
;; This will open documentation for it, including demos of how they are used.
;; Alternatively, use `C-h o' to look up a symbol (functions, variables, faces,
;; etc).
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.

;; personal information
(setq user-full-name "gaozhan"
      user-mail-address "gaozhanwk@163.com")

(global-unset-key (kbd "C-z")) ; 关闭 "C-z" 最小化
(global-auto-revert-mode 1) ; 自动加载外部修改过的文件
(setq auto-revert-remote-files t) ; 启用对远程文件的自动检测
(setq auto-save-default t) ; 自动保存文件
(setq make-backup-files nil) ; 禁用备份文件

(setq tramp-chunksize 2000) ; 增大数据块大小，加快传输速度
(with-eval-after-load 'tramp
  (add-to-list 'tramp-remote-path 'tramp-own-remote-path) ; 使用远程主机的默认 PATH
  (add-to-list 'tramp-remote-path "/home/gaozhan/.local/bin")) ; 添加自定义路径

(setq read-file-name-completion-ignore-case t) ; 读取文件名时，忽略大小写进行补全
(setq read-buffer-completion-ignore-case t) ; buffer 忽略大小写进行补全

;; rust
(setq rustic-cargo-bin "/home/gaozhan/.cargo/bin/cargo")

(use-package! color-rg
  :config
  (defun my/rg-search-in-directory ()
    "Prompt for search term and directory, then search using `counsel-rg`."
    (interactive)
    (let* ((search-term (read-string "Search for: "))
           (directory (read-directory-name "Select directory: ")))
      (color-rg-search-input search-term directory)))
  )

(add-to-list 'load-path "~/.doom.d/lisp/")

;;; Package Management
;; -----------------------------------------------------------------------------
(use-package init-ui)
(use-package init-wsl)
(use-package init-org)
(use-package init-input)
(use-package init-keybinding)
(use-package init-sdcv)
(use-package my-fun)

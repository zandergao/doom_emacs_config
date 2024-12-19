;;; lisp/my-fun.el -*- lexical-binding: t; -*-

(defun my/consult-fd-with-path (dir)
  "使用 fd 在指定路径 DIR 下搜索文件，默认为 projectile 的根目录。"
  (interactive
   (let* ((root (or (projectile-project-root) default-directory)) ;; 项目根目录或当前目录
          (chosen-dir (read-directory-name "Search for: " root root t)))
     (list chosen-dir)))
  (consult-fd dir))

(defun my/consult-ripgrep-with-path (dir)
  "使用 rg 在指定路径 DIR 下搜索文件，默认为 projectile 的根目录。"
  (interactive
   (let* ((root (or (projectile-project-root) default-directory)) ;; 项目根目录或当前目录
          (chosen-dir (read-directory-name "Search for: " root root t)))
     (list chosen-dir)))
  (consult-ripgrep dir))

(provide 'my-fun)

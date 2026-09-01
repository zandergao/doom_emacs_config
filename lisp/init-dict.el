;;; init-dict.el -*- lexical-binding: t; -*-

(defvar +dict-data-dir (expand-file-name "~/.stardict/dic")
  "Directory of local StarDict dictionaries used by sdcv.")

(use-package! sdcv
  :commands (sdcv-search-pointer sdcv-search-input)
  :init
  (setq sdcv-program (or (executable-find "sdcv")
                         "/opt/homebrew/bin/sdcv"
                         "/usr/local/bin/sdcv")
        sdcv-dictionary-data-dir +dict-data-dir
        sdcv-only-data-dir t
        sdcv-env-lang "zh_CN.UTF-8"
        sdcv-say-word-p nil
        sdcv-dictionary-simple-list '("朗道英汉字典5.0"
                                      "朗道汉英字典5.0")
        sdcv-dictionary-complete-list '("朗道英汉字典5.0"
                                        "朗道汉英字典5.0"))
  :config
  (set-popup-rule! "^\\*SDCV\\*" :side 'right :size 0.38 :select t :quit t))

(map! :leader
      :desc "Search word (offline dict)" "s w" #'+dict/search-word)

(defun +dict--format-float-text (raw)
  "Strip sdcv markers from RAW for a compact floating tooltip."
  (replace-regexp-in-string
   "^-->" ""
   (string-trim (or raw ""))))

(defun +dict--show-float-text (text &optional hint)
  "Show TEXT in a posframe tooltip. HINT is shown in the echo area."
  (require 'posframe)
  (let* ((buf (get-buffer-create " *+dict-float*"))
         (max-w (min 72 (max 36 (/ (frame-width) 2)))))
    (with-current-buffer buf
      (erase-buffer)
      (setq-local truncate-lines nil)
      (insert text)
      (goto-char (point-min)))
    (posframe-show
     buf
     :position (point)
     :poshandler #'posframe-poshandler-point-bottom-left-corner
     :max-width max-w
     :min-width 28
     :internal-border-width 12
     :internal-border-color (or (face-foreground 'font-lock-comment-face nil t)
                                "gray50")
     :background-color (or (face-background 'tooltip nil t)
                           (face-background 'default nil t))
     :foreground-color (or (face-foreground 'default nil t) "white")
     :left-fringe 8
     :right-fringe 8)
    (message (or hint "按 q / ESC / 空格关闭"))
    (unwind-protect
        (let ((event (read-event)))
          (unless (memq event '(?q ?Q ?\s ?\r ?\e escape return space))
            (push event unread-command-events)))
      (posframe-hide buf)
      (message nil))))

(defun +dict--show-float (word)
  "Show WORD's offline dictionary result in a posframe tooltip."
  (require 'sdcv)
  (+dict--show-float-text
   (+dict--format-float-text
    (sdcv-search-with-dictionary word sdcv-dictionary-simple-list))
   "按 q / ESC / 空格关闭词典浮窗"))

(defun +dict/search-word (&optional arg)
  "Search a word in the local offline dictionary.
Use the active region or the word at point.  With prefix ARG, or
when there is no word at point, prompt for a word.
Show the result in a floating tooltip; fall back to a buffer on TTY."
  (interactive "P")
  (unless (executable-find "sdcv")
    (user-error "sdcv is not installed. Run: brew install sdcv"))
  (unless (file-directory-p +dict-data-dir)
    (user-error "Dictionary directory not found: %s" +dict-data-dir))
  (require 'sdcv)
  (let ((word (cond
               (arg (sdcv-prompt-input))
               ((use-region-p)
                (buffer-substring-no-properties (region-beginning) (region-end)))
               ((thing-at-point 'word t))
               (t (sdcv-prompt-input)))))
    (if (and (display-graphic-p) (require 'posframe nil t))
        (+dict--show-float word)
      (sdcv-search-detail word))))

;;; init-dict.el -*- lexical-binding: t; -*-

(defvar +dict-data-dir (expand-file-name "~/.stardict/dic")
  "Directory of local StarDict dictionaries used by sdcv.")

(defvar +dict-translate-ollama-url "http://127.0.0.1:11434/api/chat"
  "Ollama chat API endpoint used for sentence translation.")

(defvar +dict-translate-ollama-model "qwen2.5-coder:7b"
  "Ollama model used for sentence translation.")

(defvar +dict-translate-timeout 60
  "Seconds to wait for a sentence translation response.")

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
      (:desc "Search word (offline dict)" "s w" #'+dict/search-word)
      (:desc "Translate sentence" "s y" #'+dict/translate-sentence))

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

(defun +dict--mostly-cjk-p (text)
  "Return non-nil if TEXT is mostly Chinese characters."
  (let ((cjk 0)
        (total 0))
    (dolist (ch (string-to-list text))
      (unless (memq ch '(?\s ?\t ?\n ?\r))
        (setq total (1+ total))
        (when (aref (char-category-set ch) ?c)
          (setq cjk (1+ cjk)))))
    (and (> total 0)
         (>= (/ (float cjk) total) 0.3))))

(defun +dict--sentence-at-point ()
  "Return selected text, the sentence at point, or the current line."
  (cond
   ((use-region-p)
    (buffer-substring-no-properties (region-beginning) (region-end)))
   ((thing-at-point 'sentence t))
   ((thing-at-point 'line t))))

(defun +dict--url-json (url &optional payload timeout)
  "Fetch URL and parse the JSON body.
If PAYLOAD is a string, send it as a JSON POST body."
  (require 'json)
  (require 'url)
  (let ((url-request-method (if payload "POST" "GET"))
        (url-request-extra-headers
         (when payload '(("Content-Type" . "application/json"))))
        (url-request-data
         (when payload (encode-coding-string payload 'utf-8))))
    (with-current-buffer
        (or (url-retrieve-synchronously url t t (or timeout +dict-translate-timeout))
            (error "No response from %s" url))
      (goto-char (point-min))
      (unless (re-search-forward "\n\n" nil t)
        (error "Invalid HTTP response from %s" url))
      (json-parse-buffer :object-type 'alist :array-type 'list))))

(defun +dict--translate-ollama (text target)
  "Translate TEXT to TARGET language via local Ollama."
  (require 'json)
  (let* ((prompt (format "将下面文本翻译成%s。只输出译文，不要解释，不要引号：\n%s"
                         target text))
         (payload (json-encode
                   `((model . ,+dict-translate-ollama-model)
                     (stream . :json-false)
                     (messages . [((role . "user")
                                   (content . ,prompt))]))))
         (json (+dict--url-json +dict-translate-ollama-url payload))
         (result (alist-get 'content (alist-get 'message json))))
    (unless (and (stringp result) (not (string-empty-p (string-trim result))))
      (error "Ollama returned an empty translation"))
    (string-trim result)))

(defun +dict--translate-google (text target-code)
  "Translate TEXT to TARGET-CODE via the public Google Translate endpoint."
  (let* ((url (concat "https://translate.googleapis.com/translate_a/single"
                      "?client=gtx&sl=auto&dt=t"
                      "&tl=" target-code
                      "&q=" (url-hexify-string text)))
         (json (+dict--url-json url nil 8))
         (result (mapconcat #'car (car json) "")))
    (unless (and (stringp result) (not (string-empty-p (string-trim result))))
      (error "Google Translate returned an empty translation"))
    (string-trim result)))

(defun +dict--translate-text (text)
  "Translate TEXT between Chinese and English.
Prefer local Ollama; fall back to Google Translate."
  (let* ((to-en (+dict--mostly-cjk-p text))
         (target-name (if to-en "英文" "简体中文"))
         (target-code (if to-en "en" "zh-CN"))
         (ollama-err nil)
         (result nil))
    (setq result
          (condition-case err
              (+dict--translate-ollama text target-name)
            (error
             (setq ollama-err err)
             nil)))
    (unless result
      (setq result
            (condition-case err
                (+dict--translate-google text target-code)
              (error
               (user-error "翻译失败。Ollama: %s；Google: %s"
                           (error-message-string ollama-err)
                           (error-message-string err))))))
    result))

(defun +dict/translate-sentence (&optional arg)
  "Translate the selected region or the sentence at point.
With prefix ARG, prompt for the text to translate.
Chinese text is translated to English, and vice versa.
Result is shown in a floating tooltip."
  (interactive "P")
  (let* ((text (string-trim
                (or (and arg (read-string "Translate: "))
                    (+dict--sentence-at-point)
                    (read-string "Translate: ")))))
    (when (string-empty-p text)
      (user-error "没有可翻译的文本"))
    (message "正在翻译...")
    (let ((result (+dict--translate-text text)))
      (if (and (display-graphic-p) (require 'posframe nil t))
          (+dict--show-float-text
           (format "原文\n%s\n\n译文\n%s" text result)
           "按 q / ESC / 空格关闭翻译浮窗")
        (message "%s" result)))))

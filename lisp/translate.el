;;; translate.el -*- lexical-binding: t; -*-

(defvar +trans-dict-dir (expand-file-name "~/.stardict/dic")
  "Directory of local StarDict dictionaries used by sdcv.")

(defvar +trans-ollama-url "http://127.0.0.1:11434/api/chat"
  "Ollama chat API endpoint used for sentence translation.")

(defvar +trans-ollama-model "qwen2.5-coder:7b"
  "Ollama model used for sentence translation.")

(defvar +trans-timeout 60
  "Seconds to wait for a sentence translation response.")

(use-package! sdcv
  :commands (sdcv-search-pointer sdcv-search-input)
  :init
  (setq sdcv-program (or (executable-find "sdcv")
                         "/opt/homebrew/bin/sdcv"
                         "/usr/local/bin/sdcv")
        sdcv-dictionary-data-dir +trans-dict-dir
        sdcv-only-data-dir t
        sdcv-env-lang "zh_CN.UTF-8"
        sdcv-say-word-p nil
        sdcv-dictionary-simple-list '("朗道英汉字典5.0"
                                      "朗道汉英字典5.0")
        sdcv-dictionary-complete-list '("朗道英汉字典5.0"
                                        "朗道汉英字典5.0"))
  :config
  (set-popup-rule! "^\\*SDCV\\*" :side 'right :size 0.38 :select t :quit t))

(set-popup-rule! "^\\*Translate\\*" :side 'right :size 0.5 :select t :quit t)

(map! :leader
      (:desc "Search word (offline dict)" "s w" #'+trans/search-word)
      (:prefix ("y" . "translate")
       :desc "Translate sentence" "s" #'+trans/translate-sentence
       :desc "Translate paragraph" "p" #'+trans/translate-paragraph
       :desc "Translate buffer" "f" #'+trans/translate-buffer
       :desc "Chinese to English" "e" #'+trans/zh-to-en))

(defun +trans--format-float-text (raw)
  "Strip sdcv markers from RAW for a compact floating tooltip."
  (replace-regexp-in-string
   "^-->" ""
   (string-trim (or raw ""))))

(defvar-local +trans--float-parent-frame nil
  "Parent frame to restore when the dictionary float is closed.")

(defun +trans--float-hidehandler (info)
  "Hide the float when focus leaves its child frame."
  (let* ((buf-info (plist-get info :posframe-buffer))
         (buf (if (consp buf-info) (cdr buf-info) buf-info))
         (frame (and (buffer-live-p buf)
                     (buffer-local-value 'posframe--frame buf))))
    (not (and (frame-live-p frame)
              (eq (selected-frame) frame)))))

(defun +trans-float-quit ()
  "Hide the dictionary float and return focus to the parent frame."
  (interactive)
  (let ((parent +trans--float-parent-frame)
        (buf (current-buffer)))
    (posframe-hide buf)
    (when (frame-live-p parent)
      (select-frame-set-input-focus parent))))

(defun +trans-float-copy-all ()
  "Copy the whole float buffer to the kill ring and clipboard."
  (interactive)
  (let ((text (buffer-substring-no-properties (point-min) (point-max))))
    (kill-new text)
    (when (fboundp 'gui-set-selection)
      (gui-set-selection 'CLIPBOARD text))
    (message "已复制全部内容")))

(define-derived-mode +trans-float-mode special-mode "Trans"
  "Read-only mode for translation/dictionary floats.
Select text and copy it with Evil visual + y, or M-w / ⌘C."
  (setq-local truncate-lines nil)
  (setq-local cursor-type 'box))

(when (boundp 'evil-normal-state-map)
  (evil-set-initial-state '+trans-float-mode 'normal))

(map! :map +trans-float-mode-map
      :g "C-g" #'+trans-float-quit
      :n "q" #'+trans-float-quit
      :n "Y" #'+trans-float-copy-all
      :n "C-c C-c" #'+trans-float-copy-all)

(defun +trans--show-float-text (text &optional hint)
  "Show TEXT in a focusable posframe so it can be selected and copied."
  (require 'posframe)
  (let* ((buf (get-buffer-create " *+trans-float*"))
         (parent (selected-frame))
         (max-w (min 72 (max 36 (/ (frame-width) 2))))
         (max-h (min 18 (max 8 (/ (frame-height) 3)))))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert text)
        (goto-char (point-min)))
      (+trans-float-mode)
      (setq-local +trans--float-parent-frame parent))
    (posframe-show
     buf
     :position (point)
     :poshandler #'posframe-poshandler-point-bottom-left-corner
     :accept-focus t
     :cursor t
     :max-width max-w
     :max-height max-h
     :min-width 28
     :internal-border-width 12
     :internal-border-color (or (face-foreground 'font-lock-comment-face nil t)
                                "gray50")
     :background-color (or (face-background 'tooltip nil t)
                           (face-background 'default nil t))
     :foreground-color (or (face-foreground 'default nil t) "white")
     :left-fringe 8
     :right-fringe 8
     :hidehandler #'+trans--float-hidehandler)
    (when-let ((frame (buffer-local-value 'posframe--frame buf)))
      (when (frame-live-p frame)
        (select-frame-set-input-focus frame)
        (select-window (frame-root-window frame))
        (when (fboundp 'evil-normal-state)
          (with-current-buffer buf
            (evil-normal-state 1)))))
    (message (or hint "v 选中  y 复制  Y 全部复制  q 关闭"))))

(defun +trans--show-float (word)
  "Show WORD's offline dictionary result in a posframe tooltip."
  (require 'sdcv)
  (+trans--show-float-text
   (+trans--format-float-text
    (sdcv-search-with-dictionary word sdcv-dictionary-simple-list))
   "v 选中  y 复制  Y 全部复制  q 关闭"))

(defun +trans/search-word (&optional arg)
  "Search a word in the local offline dictionary.
Use the active region or the word at point.  With prefix ARG, or
when there is no word at point, prompt for a word.
Show the result in a floating tooltip; fall back to a buffer on TTY."
  (interactive "P")
  (unless (executable-find "sdcv")
    (user-error "sdcv is not installed. Run: brew install sdcv"))
  (unless (file-directory-p +trans-dict-dir)
    (user-error "Dictionary directory not found: %s" +trans-dict-dir))
  (require 'sdcv)
  (let ((word (cond
               (arg (sdcv-prompt-input))
               ((use-region-p)
                (buffer-substring-no-properties (region-beginning) (region-end)))
               ((thing-at-point 'word t))
               (t (sdcv-prompt-input)))))
    (if (and (display-graphic-p) (require 'posframe nil t))
        (+trans--show-float word)
      (sdcv-search-detail word))))

(defun +trans--mostly-cjk-p (text)
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

(defun +trans--in-comment-p ()
  "Return non-nil if point is inside a comment."
  (or (nth 4 (syntax-ppss))
      (save-excursion (comment-beginning))))

(defun +trans--sentence-span (text pos)
  "Return (START END) of the sentence in TEXT around 0-based index POS."
  (let* ((len (length text))
         (pos (max 0 (min pos (max 0 (1- len)))))
         (sep "[。！？；;.!?]")
         (start 0)
         (end len)
         i)
    (setq i (1- pos))
    (while (and (>= i 0)
                (not (string-match-p sep (substring text i (1+ i)))))
      (setq i (1- i)))
    (setq start (if (>= i 0) (1+ i) 0))
    (setq i pos)
    (while (and (< i len)
                (not (string-match-p sep (substring text i (1+ i)))))
      (setq i (1+ i)))
    (setq end (if (< i len) (1+ i) len))
    (while (and (< start end) (memq (aref text start) '(?\s ?\t)))
      (setq start (1+ start)))
    (while (and (< start end) (memq (aref text (1- end)) '(?\s ?\t)))
      (setq end (1- end)))
    (list start end)))

(defun +trans--sentence-in-string (text pos)
  "Extract the sentence in TEXT around 0-based index POS."
  (pcase-let ((`(,start ,end) (+trans--sentence-span text pos)))
    (string-trim (substring text start end))))

(defun +trans--comment-line-sentence-bounds ()
  "Return (BEG END TEXT) for the sentence on the current comment line."
  (let* ((line-beg (line-beginning-position))
         (line (buffer-substring-no-properties line-beg (line-end-position)))
         (prefix (if (string-match
                      "\\`\\s-*\\(//+\\|;+\\|#+\\|--\\|/\\*+\\|\\*+/?\\)\\s-*"
                      line)
                     (match-end 0)
                   (if (string-match "\\`\\s-*" line)
                       (match-end 0)
                     0)))
         (suffix (if (string-match "\\s-*\\*/\\s-*\\'" (substring line prefix))
                     (- (length line) prefix (match-beginning 0))
                   0))
         (body-beg (+ line-beg prefix))
         (body-end (- (line-end-position) suffix))
         (body (buffer-substring-no-properties body-beg body-end))
         (rel (- (point) body-beg)))
    (unless (string-empty-p (string-trim body))
      (pcase-let ((`(,start ,end) (+trans--sentence-span body (max 0 rel))))
        (when (< start end)
          (list (+ body-beg start)
                (+ body-beg end)
                (substring body start end)))))))

(defun +trans--comment-line-sentence ()
  "Return the sentence on the current comment line, without comment markers."
  (nth 2 (+trans--comment-line-sentence-bounds)))

(defun +trans--chinese-run-char-p (ch)
  "Return non-nil if CH is a Chinese character or mid-sentence punctuation."
  (and ch
       (not (+trans--chinese-end-punct-p ch))
       (or (aref (char-category-set ch) ?c)
           (memq ch '(?， ?、 ?： ?（ ?） ?「 ?」 ?“ ?” ?《 ?》)))))

(defun +trans--chinese-end-punct-p (ch)
  "Return non-nil if CH ends a Chinese sentence."
  (memq ch '(?。 ?！ ?？ ?；)))

(defun +trans--chinese-run-at-point ()
  "Return (BEG END TEXT) for a contiguous Chinese sentence around point.
This matches the common workflow of typing Chinese, then invoking
a command to turn it into English.  Sentence-ending punctuation
is not crossed, so two Chinese sentences stay separate."
  (save-excursion
    (let ((bol (line-beginning-position))
          (eol (line-end-position))
          beg end)
      (cond
       ((+trans--chinese-run-char-p (char-after))
        (while (and (< (point) eol) (+trans--chinese-run-char-p (char-after)))
          (forward-char 1))
        (when (and (< (point) eol) (+trans--chinese-end-punct-p (char-after)))
          (forward-char 1)))
       ((+trans--chinese-end-punct-p (char-after))
        (forward-char 1)))
      (setq end (point))
      (when (and (> (point) bol) (+trans--chinese-end-punct-p (char-before)))
        (forward-char -1))
      (while (and (> (point) bol) (+trans--chinese-run-char-p (char-before)))
        (forward-char -1))
      (setq beg (point))
      (when (and (< beg end)
                 (+trans--mostly-cjk-p (buffer-substring-no-properties beg end)))
        (list beg end (buffer-substring-no-properties beg end))))))

(defun +trans--zh-bounds ()
  "Return (BEG END TEXT) of Chinese text to translate to English."
  (cond
   ((use-region-p)
    (list (region-beginning) (region-end)
          (buffer-substring-no-properties (region-beginning) (region-end))))
   ((+trans--chinese-run-at-point))
   ((and (derived-mode-p 'prog-mode) (+trans--in-comment-p))
    (+trans--comment-line-sentence-bounds))
   ((let ((line (string-trim (or (thing-at-point 'line t) ""))))
      (when (+trans--mostly-cjk-p line)
        (list (line-beginning-position)
              (line-end-position)
              line))))))

(defun +trans--sentence-at-point ()
  "Return text to translate around point.
In code comments, only the current line's sentence is used so a
whole comment block is not sent.  In other source code, use the
current line.  In prose, use the sentence at point.  An active
region always wins."
  (cond
   ((use-region-p)
    (buffer-substring-no-properties (region-beginning) (region-end)))
   ((and (derived-mode-p 'prog-mode) (+trans--in-comment-p))
    (or (+trans--comment-line-sentence)
        (string-trim (or (thing-at-point 'line t) ""))))
   ((derived-mode-p 'prog-mode)
    (string-trim (or (thing-at-point 'line t) "")))
   (t
    (or (let ((sentence-end-double-space nil))
          (thing-at-point 'sentence t))
        (thing-at-point 'line t)))))

(defun +trans--comment-line-p ()
  "Return non-nil if the current line is a comment line."
  (save-excursion
    (let ((eol (line-end-position))
          (bol (line-beginning-position)))
      (or (nth 4 (syntax-ppss (max bol (1- eol))))
          (progn
            (goto-char bol)
            (looking-at-p "\\s-*\\(//+\\|;+\\|#+\\|--\\|/\\*+\\|\\*\\)"))))))

(defun +trans--comment-line-body ()
  "Return current line's comment text without markers, or nil if empty."
  (let* ((line (buffer-substring-no-properties
                (line-beginning-position) (line-end-position)))
         (body (replace-regexp-in-string
                "\\`\\s-*\\(//+\\|;+\\|#+\\|--\\|/\\*+\\|\\*+/?\\)\\s-*"
                ""
                line)))
    (setq body (replace-regexp-in-string "\\s-*\\*/\\s-*\\'" "" body))
    (setq body (string-trim body))
    (unless (string-empty-p body)
      body)))

(defun +trans--comment-paragraph-at-point ()
  "Return adjacent comment lines around point, without comment markers.
A blank comment line ends the paragraph."
  (save-excursion
    (let (lines)
      (beginning-of-line)
      (while (and (not (bobp))
                  (save-excursion
                    (forward-line -1)
                    (and (+trans--comment-line-p)
                         (+trans--comment-line-body))))
        (forward-line -1))
      (while (and (not (eobp))
                  (+trans--comment-line-p)
                  (let ((body (+trans--comment-line-body)))
                    (when body
                      (push body lines)
                      t)))
        (forward-line 1))
      (when lines
        (string-join (nreverse lines) "\n")))))

(defun +trans--paragraph-at-point ()
  "Return the paragraph to translate around point.
An active region always wins.  In comments, adjacent comment
lines are collected.  Otherwise use the text paragraph."
  (cond
   ((use-region-p)
    (buffer-substring-no-properties (region-beginning) (region-end)))
   ((or (+trans--in-comment-p) (+trans--comment-line-p))
    (+trans--comment-paragraph-at-point))
   (t
    (let ((sentence-end-double-space nil))
      (string-trim (or (thing-at-point 'paragraph t) ""))))))

(defun +trans--url-json (url &optional payload timeout)
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
        (or (url-retrieve-synchronously url t t (or timeout +trans-timeout))
            (error "No response from %s" url))
      (goto-char (point-min))
      (unless (re-search-forward "\n\n" nil t)
        (error "Invalid HTTP response from %s" url))
      (json-parse-buffer :object-type 'alist :array-type 'list))))

(defun +trans--translate-ollama (text target)
  "Translate TEXT to TARGET language via local Ollama."
  (require 'json)
  (let* ((prompt (format "将下面文本翻译成%s。只输出译文，不要解释，不要引号：\n%s"
                         target text))
         (payload (json-encode
                   `((model . ,+trans-ollama-model)
                     (stream . :json-false)
                     (messages . [((role . "user")
                                   (content . ,prompt))]))))
         (json (+trans--url-json +trans-ollama-url payload))
         (result (alist-get 'content (alist-get 'message json))))
    (unless (and (stringp result) (not (string-empty-p (string-trim result))))
      (error "Ollama returned an empty translation"))
    (string-trim result)))

(defun +trans--translate-google (text target-code)
  "Translate TEXT to TARGET-CODE via the public Google Translate endpoint."
  (let* ((url (concat "https://translate.googleapis.com/translate_a/single"
                      "?client=gtx&sl=auto&dt=t"
                      "&tl=" target-code
                      "&q=" (url-hexify-string text)))
         (json (+trans--url-json url nil 8))
         (result (mapconcat #'car (car json) "")))
    (unless (and (stringp result) (not (string-empty-p (string-trim result))))
      (error "Google Translate returned an empty translation"))
    (string-trim result)))

(defun +trans--translate-text (text &optional lang)
  "Translate TEXT between Chinese and English.
LANG is `en or `zh; nil means auto-detect from TEXT.
Prefer local Ollama; fall back to Google Translate."
  (let* ((to-en (pcase lang
                  ('en t)
                  ('zh nil)
                  (_ (+trans--mostly-cjk-p text))))
         (target-name (if to-en "英文" "简体中文"))
         (target-code (if to-en "en" "zh-CN"))
         (ollama-err nil)
         (result nil))
    (setq result
          (condition-case err
              (+trans--translate-ollama text target-name)
            (error
             (setq ollama-err err)
             nil)))
    (unless result
      (setq result
            (condition-case err
                (+trans--translate-google text target-code)
              (error
               (user-error "翻译失败。Ollama: %s；Google: %s"
                           (error-message-string ollama-err)
                           (error-message-string err))))))
    result))

(defun +trans/translate-sentence (&optional arg)
  "Translate the selected region or the sentence at point.
With prefix ARG, prompt for the text to translate.
Chinese text is translated to English, and vice versa.
Result is shown in a floating tooltip."
  (interactive "P")
  (let* ((text (string-trim
                (or (and arg (read-string "Translate: "))
                    (+trans--sentence-at-point)
                    (read-string "Translate: ")))))
    (when (string-empty-p text)
      (user-error "没有可翻译的文本"))
    (message "正在翻译...")
    (let ((result (+trans--translate-text text)))
      (if (and (display-graphic-p) (require 'posframe nil t))
          (+trans--show-float-text
           (format "原文\n%s\n\n译文\n%s" text result)
           "v 选中  y 复制  Y 全部复制  q 关闭")
        (message "%s" result)))))

(defun +trans/translate-paragraph (&optional arg)
  "Translate the selected region or the paragraph at point.
With prefix ARG, prompt for the text to translate.
Chinese text is translated to English, and vice versa.
Show the translation first, then the original text."
  (interactive "P")
  (let* ((text (string-trim
                (or (and arg (read-string "Translate paragraph: "))
                    (+trans--paragraph-at-point)
                    (read-string "Translate paragraph: ")))))
    (when (string-empty-p text)
      (user-error "没有可翻译的段落"))
    (message "正在翻译整段...")
    (let ((result (+trans--translate-text text)))
      (if (and (display-graphic-p) (require 'posframe nil t))
          (+trans--show-float-text
           (format "译文\n%s\n\n原文\n%s" result text)
           "v 选中  y 复制  Y 全部复制  q 关闭")
        (message "译文: %s" result)))))

(defun +trans/zh-to-en (&optional arg)
  "Translate Chinese at point into English and replace it.
Use the active region, the Chinese just typed on this line, or
the current comment sentence.  With prefix ARG, prompt for
Chinese and insert the English at point."
  (interactive "P")
  (if arg
      (let ((text (string-trim (read-string "中文: "))))
        (when (string-empty-p text)
          (user-error "没有可翻译的文本"))
        (unless (string-match-p "\\cc" text)
          (user-error "没有检测到中文"))
        (message "正在翻译成英文...")
        (let ((result (+trans--translate-text text 'en)))
          (insert result)
          (message "%s" result)))
    (pcase-let ((`(,beg ,end ,text) (+trans--zh-bounds)))
      (unless (and beg end text (not (string-empty-p (string-trim text))))
        (user-error "没有找到可翻译的中文，请先输入或选中中文"))
      (unless (string-match-p "\\cc" text)
        (user-error "没有检测到中文，请先输入中文"))
      (message "正在翻译成英文...")
      (let ((result (+trans--translate-text text 'en)))
        (atomic-change-group
          (delete-region beg end)
          (goto-char beg)
          (insert result))
        (when (use-region-p)
          (deactivate-mark))
        (message "%s" result)))))

(defun +trans--split-paragraphs (text)
  "Split TEXT into paragraphs separated by blank lines."
  (split-string text "\n[ \t]*\n+" t "[ \t\n\r]+"))

(defun +trans--show-result-buffer (text &optional title)
  "Show TEXT in a normal *Translate* buffer, not a float."
  (let ((buf (get-buffer-create "*Translate*")))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert text)
        (goto-char (point-min)))
      (text-mode)
      (setq-local header-line-format (or title "译文")))
    (pop-to-buffer buf)))

(defun +trans/translate-buffer ()
  "Translate the current buffer and show the result in a side buffer.
The original file is left unchanged.  Long text is translated
paragraph by paragraph.  Confirm before translating source code
or very large buffers."
  (interactive)
  (let ((text (string-trim (buffer-substring-no-properties (point-min) (point-max))))
        (src (or (and buffer-file-name (file-name-nondirectory buffer-file-name))
                 (buffer-name))))
    (when (string-empty-p text)
      (user-error "当前文件没有可翻译的内容"))
    (when (and (derived-mode-p 'prog-mode)
               (not (y-or-n-p "当前是代码文件，整文件翻译可能把代码也译掉。继续？")))
      (user-error "已取消"))
    (when (and (> (length text) 8000)
               (not (y-or-n-p (format "文本约 %d 字，翻译会较久。继续？" (length text)))))
      (user-error "已取消"))
    (let* ((parts (+trans--split-paragraphs text))
           (total (max 1 (length parts)))
           (i 0)
           translated)
      (dolist (part (or parts (list text)))
        (setq i (1+ i))
        (message "正在翻译整文件 %d/%d..." i total)
        (redisplay t)
        (push (+trans--translate-text part) translated))
      (+trans--show-result-buffer
       (string-join (nreverse translated) "\n\n")
       (format "译文 · %s" src))
      (message "整文件翻译完成（%d 段）" total))))


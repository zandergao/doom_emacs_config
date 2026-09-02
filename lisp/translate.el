;;; translate.el -*- lexical-binding: t; -*-

(defvar +trans-dict-dir (expand-file-name "~/.stardict/dic")
  "Directory of local StarDict dictionaries used by sdcv.")

(defvar +trans-ollama-url "http://127.0.0.1:11434/api/chat"
  "Ollama chat API endpoint used for sentence translation.")

(defvar +trans-ollama-model "qwen2.5-coder:7b"
  "Ollama model used for sentence translation.")

(defvar +trans-timeout 60
  "Seconds to wait for a sentence translation response.")

(defvar +trans-batch-items 16
  "Max readable pieces to send in one translation request.")

(defvar +trans-batch-chars 2400
  "Max source characters to send in one translation request.")

(defvar +trans--job nil
  "Hash table for the current background buffer-translation job, or nil.")

(defvar +trans-worker-program
  (expand-file-name
   "bin/trans-worker.py"
   (or (bound-and-true-p doom-user-dir)
       (expand-file-name "~/.doom.d")))
  "Python worker that talks to local Ollama outside Emacs.")

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
       :desc "Cancel buffer translation" "q" #'+trans/translate-buffer-cancel
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
                (+trans--org-strip-markers
                 (buffer-substring-no-properties (region-beginning) (region-end))))
               ((+trans--org-inner-at-point))
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

(defun +trans--org-p ()
  "Return non-nil if the current buffer is Org."
  (derived-mode-p 'org-mode))

(defun +trans--looks-like-org-p (text)
  "Return non-nil if TEXT contains common Org markup."
  (or (+trans--org-p)
      (string-match-p
       "\\(?:\\*\\*[^*\n]+\\*\\*\\|=[^= \t\n][^=\n]*=\\|~[^~ \t\n][^~\n]*~\\|^\\*+ \\|#\\+\\)"
       text)))

(defun +trans--org-atomic-ranges (text)
  "Return ((START END) ...) spans in TEXT that must not be split.
Covers Org verbatim, code, and links."
  (let (ranges)
    (dolist (re '("=\\(?:[^= \t\n]\\|[^= \t\n][^=]*?[^= \t\n]\\)="
                  "~\\(?:[^~ \t\n]\\|[^~ \t\n][^~\n]*?[^~ \t\n]\\)~"
                  "\\[\\[[^][\n]+\\]\\(?:\\[[^][\n]+\\]\\)?\\]"))
      (let ((start 0))
        (while (string-match re text start)
          (push (list (match-beginning 0) (match-end 0)) ranges)
          (setq start (match-end 0)))))
    ranges))

(defun +trans--pos-in-ranges-p (pos ranges)
  "Return non-nil if 0-based POS is inside any (START END) in RANGES."
  (let (hit)
    (dolist (r ranges hit)
      (when (and (>= pos (car r)) (< pos (cadr r)))
        (setq hit t)))))

(defun +trans--org-strip-markers (text)
  "Strip a single pair of surrounding Org emphasis or verbatim markers."
  (setq text (string-trim (or text "")))
  (cond
   ((string-match "\\`\\*\\*\\([^*].*?\\)\\*\\*\\'" text)
    (match-string 1 text))
   ((string-match "\\`\\*\\([^*].*?\\)\\*\\'" text)
    (match-string 1 text))
   ((string-match "\\`=\\([^=].*?\\)=\\'" text)
    (match-string 1 text))
   ((string-match "\\`~\\([^~].*?\\)~\\'" text)
    (match-string 1 text))
   ((string-match "\\`/\\([^/].*?\\)/\\'" text)
    (match-string 1 text))
   ((string-match "\\`_\\([^_].*?\\)_\\'" text)
    (match-string 1 text))
   ((string-match "\\`\\+\\([^+].*?\\)\\+\\'" text)
    (match-string 1 text))
   ((string-match "\\`\\[\\[\\(?:[^][]+\\)\\]\\[\\([^][]+\\)\\]\\]\\'" text)
    (match-string 1 text))
   ((string-match "\\`\\[\\[\\([^][]+\\)\\]\\]\\'" text)
    (match-string 1 text))
   (t text)))

(defun +trans--org-inner-at-point ()
  "Return inner text of Org emphasis, verbatim, code, or link at point."
  (when (and (+trans--org-p) (fboundp 'org-element-context))
    (require 'org-element)
    (let* ((ctx (org-element-context))
           (type (org-element-type ctx)))
      (pcase type
        ((or 'verbatim 'code)
         (org-element-property :value ctx))
        ((or 'bold 'italic 'underline 'strike-through)
         (let ((b (org-element-property :contents-begin ctx))
               (e (org-element-property :contents-end ctx)))
           (when (and b e) (buffer-substring-no-properties b e))))
        ('link
         (or (let ((b (org-element-property :contents-begin ctx))
                   (e (org-element-property :contents-end ctx)))
               (when (and b e) (buffer-substring-no-properties b e)))
             (org-element-property :raw-link ctx)))))))

(defun +trans--org-heading-title-bounds (&optional el)
  "Return (BEG END) of the headline title, without stars, TODO, or tags."
  (require 'org-element)
  (let ((el (or el (org-element-at-point))))
    (save-excursion
      (goto-char (org-element-property :begin el))
      (let ((eol (line-end-position)))
        (skip-chars-forward "*")
        (skip-chars-forward " \t")
        (when (and (boundp 'org-todo-regexp) org-todo-regexp
                   (looking-at (concat org-todo-regexp "\\s-+")))
          (goto-char (match-end 0)))
        (when (looking-at "\\[#[A-Za-z0-9]\\]\\s-*")
          (goto-char (match-end 0)))
        (let ((start (point))
              (stop eol))
          (when (re-search-forward "[ \t]+\\(:[[:alnum:]_@#%:]+:\\)[ \t]*$" eol t)
            (setq stop (match-beginning 0)))
          (list start stop))))))

(defun +trans--org-container-bounds ()
  "Return (BEG END) of the Org element that should be translated."
  (require 'org-element)
  (save-excursion
    (let* ((el (org-element-at-point))
           (type (org-element-type el)))
      (when (memq type '(section org-data))
        (setq el (or (org-element-lineage
                      (org-element-context)
                      '(paragraph headline item verse-block
                        quote-block table-cell src-block)
                      t)
                     el))
        (setq type (org-element-type el)))
      (pcase type
        ('headline (+trans--org-heading-title-bounds el))
        ('src-block
         (list (line-beginning-position) (line-end-position)))
        ((or 'paragraph 'verse-block 'quote-block 'table-cell)
         (list (org-element-property :contents-begin el)
               (org-element-property :contents-end el)))
        ('item
         (list (org-element-property :contents-begin el)
               (org-element-property :contents-end el)))
        (_
         (let ((cb (org-element-property :contents-begin el))
               (ce (org-element-property :contents-end el)))
           (cond
            ((and cb ce) (list cb ce))
            ((org-element-property :begin el)
             (list (org-element-property :begin el)
                   (or (org-element-property :end el)
                       (line-end-position)))))))))))

(defun +trans--org-sentence-bounds ()
  "Return (BEG END TEXT) for the Org sentence at point."
  (when (+trans--org-p)
    (pcase-let ((`(,beg ,end) (+trans--org-container-bounds)))
      (when (and beg end (< beg end))
        (let* ((text (buffer-substring-no-properties beg end))
               (rel (max 0 (min (- (point) beg)
                                (max 0 (1- (length text)))))))
          (pcase-let ((`(,s ,e) (+trans--sentence-span text rel)))
            (when (< s e)
              (list (+ beg s) (+ beg e) (substring text s e)))))))))

(defun +trans--org-paragraph-at-point ()
  "Return the current Org paragraph, heading title, or list item."
  (when (+trans--org-p)
    (pcase-let ((`(,beg ,end) (+trans--org-container-bounds)))
      (when (and beg end (< beg end))
        (string-trim (buffer-substring-no-properties beg end))))))

(defun +trans--merge-ranges (ranges)
  "Sort and merge overlapping (START END) RANGES."
  (setq ranges (sort (copy-sequence ranges)
                     (lambda (a b)
                       (or (< (car a) (car b))
                           (and (= (car a) (car b))
                                (< (cadr a) (cadr b)))))))
  (let (merged)
    (dolist (r ranges)
      (if (null merged)
          (push (list (car r) (cadr r)) merged)
        (let ((last (car merged)))
          (if (<= (car r) (cadr last))
              (setcar (cdr last) (max (cadr last) (cadr r)))
            (push (list (car r) (cadr r)) merged)))))
    (nreverse merged)))

(defun +trans--org-todo-re ()
  "Regexp alternative of Org TODO keywords to keep untranslated."
  (mapconcat #'identity
             (or (and (boundp 'org-todo-keywords-1) org-todo-keywords-1)
                 '("TODO" "DONE" "WAIT" "HOLD" "KILL" "PROJ" "NEXT" "STRT"))
             "\\|"))

(defun +trans--org-keep-add (ranges beg end len)
  "Push (BEG END) onto RANGES when it is a valid span of LEN."
  (if (and beg end (< beg end) (<= end len))
      (cons (list beg end) ranges)
    ranges))

(defun +trans--org-keep-ranges (text)
  "Return ((START END) ...) spans of Org markup that must not be translated."
  (let ((case-fold-search t)
        (len (length text))
        (ranges nil)
        (start 0))
    (setq start 0)
    (while (string-match
            "#\\+BEGIN_\\(SRC\\|EXAMPLE\\|EXPORT\\|COMMENT\\)\\(?:.\\|\n\\)*?#\\+END_\\(?:SRC\\|EXAMPLE\\|EXPORT\\|COMMENT\\)"
            text start)
      (setq ranges (+trans--org-keep-add ranges (match-beginning 0) (match-end 0) len)
            start (match-end 0)))
    (setq start 0)
    (while (string-match
            ":\\(?:PROPERTIES\\|LOGBOOK\\):\\(?:.\\|\n\\)*?:END:"
            text start)
      (setq ranges (+trans--org-keep-add ranges (match-beginning 0) (match-end 0) len)
            start (match-end 0)))
    (setq start 0)
    (while (string-match "^[ \t]*#\\+\\(.*\\)$" text start)
      (let ((lb (match-beginning 0))
            (le (match-end 0))
            (body (match-string 1 text)))
        (setq ranges
              (if (string-match "\\`\\(TITLE\\|SUBTITLE\\|DESCRIPTION\\):[ \t]*" body)
                  (+trans--org-keep-add ranges lb (+ lb 2 (match-end 0)) len)
                (+trans--org-keep-add ranges lb le len))
              start (min len (1+ le)))))
    (setq start 0)
    (let ((re (format "^\\*+\\s-+\\(?:\\(?:%s\\)\\s-+\\)?\\(?:\\[#[A-Za-z0-9]\\]\\s-+\\)?"
                      (+trans--org-todo-re))))
      (while (string-match re text start)
        (setq ranges (+trans--org-keep-add ranges (match-beginning 0) (match-end 0) len)
              start (match-end 0))))
    (setq start 0)
    (while (string-match "[ \t]+\\(:[[:alnum:]_@#%:]+:\\)[ \t]*$" text start)
      (setq ranges (+trans--org-keep-add ranges (match-beginning 0) (match-end 0) len)
            start (match-end 0)))
    (setq start 0)
    (while (string-match "^[ \t]*\\(?:[-+]\\|[0-9]+[.)]\\)\\s-+" text start)
      (setq ranges (+trans--org-keep-add ranges (match-beginning 0) (match-end 0) len)
            start (match-end 0)))
    (+trans--merge-ranges ranges)))

(defun +trans--span-covers-p (pos spans)
  "Return non-nil if POS is inside any (BEG END ...) in SPANS."
  (let (hit)
    (dolist (sp spans hit)
      (when (and (>= pos (car sp)) (< pos (cadr sp)))
        (setq hit t)))))

(defun +trans--org-inline-collect (text)
  "Return ((BEG END KIND ...) ...) for inline Org objects in TEXT."
  (let (spans start)
    (setq start 0)
    (while (string-match
            "\\[\\[\\([^][\n]+\\)\\]\\[\\([^][\n]+\\)\\]\\]"
            text start)
      (push (list (match-beginning 0) (match-end 0) 'link
                  (match-string 1 text) (match-string 2 text))
            spans)
      (setq start (match-end 0)))
    (setq start 0)
    (while (string-match "\\[\\[\\([^][\n]+\\)\\]\\]" text start)
      (unless (+trans--span-covers-p (match-beginning 0) spans)
        (push (list (match-beginning 0) (match-end 0) 'link-bare
                    (match-string 1 text))
              spans))
      (setq start (match-end 0)))
    (setq start 0)
    (while (string-match
            "=\\(?:[^= \t\n]\\|[^= \t\n][^=]*?[^= \t\n]\\)="
            text start)
      (unless (+trans--span-covers-p (match-beginning 0) spans)
        (push (list (match-beginning 0) (match-end 0) 'raw
                    (+trans--org-raw-oneline (match-string 0 text)))
              spans))
      (setq start (match-end 0)))
    (setq start 0)
    (while (string-match
            "~\\(?:[^~ \t\n]\\|[^~ \t\n][^~\n]*?[^~ \t\n]\\)~"
            text start)
      (unless (+trans--span-covers-p (match-beginning 0) spans)
        (push (list (match-beginning 0) (match-end 0) 'raw
                    (match-string 0 text))
              spans))
      (setq start (match-end 0)))
    (setq start 0)
    (while (string-match "\\*\\*\\([^*\n]+\\)\\*\\*" text start)
      (unless (+trans--span-covers-p (match-beginning 0) spans)
        (push (list (match-beginning 0) (match-end 0) 'wrap
                    "**" "**" (match-string 1 text))
              spans))
      (setq start (match-end 0)))
    (setq start 0)
    (while (string-match
            "\\+\\(?:[^+ \t\n]\\|[^+ \t\n][^\n]*?[^+ \t\n]\\)\\+"
            text start)
      (unless (+trans--span-covers-p (match-beginning 0) spans)
        (push (list (match-beginning 0) (match-end 0) 'wrap
                    "+" "+" (substring (match-string 0 text) 1 -1))
              spans))
      (setq start (match-end 0)))
    spans))

(defun +trans--org-inline-protect (text)
  "Replace inline Org objects with <tN> tags.
Return (PROTECTED . ALIST) where ALIST maps N to a restore spec."
  (let* ((spans (sort (copy-sequence (+trans--org-inline-collect text))
                      (lambda (a b) (> (car a) (car b)))))
         (n 0)
         (alist nil)
         (out text))
    (dolist (sp spans)
      (setq n (1+ n))
      (let ((beg (car sp))
            (end (cadr sp))
            (kind (nth 2 sp)))
        (pcase kind
          ('link
           (push (cons n (list 'link (nth 3 sp) (nth 4 sp))) alist)
           (setq out (concat (substring out 0 beg)
                             (format "<t%d>%s</t%d>" n (nth 4 sp) n)
                             (substring out end))))
          ('link-bare
           (push (cons n (list 'link-bare (nth 3 sp))) alist)
           (setq out (concat (substring out 0 beg)
                             (format "<t%d/>" n)
                             (substring out end))))
          ('raw
           (push (cons n (list 'raw (nth 3 sp))) alist)
           (setq out (concat (substring out 0 beg)
                             (format "<t%d/>" n)
                             (substring out end))))
          ('wrap
           (push (cons n (list 'wrap (nth 3 sp) (nth 4 sp))) alist)
           (setq out (concat (substring out 0 beg)
                             (format "<t%d>%s</t%d>" n (nth 5 sp) n)
                             (substring out end)))))))
    (cons out alist)))

(defun +trans--org-desc-blown-p (inner orig)
  "Return non-nil if INNER is far longer than original link text ORIG.
The model sometimes stuffs the rest of the sentence into a link tag."
  (let ((a (length (string-trim (or inner ""))))
        (b (length (or orig ""))))
    (and (> b 0) (> a (* 3 b)))))

(defun +trans--org-inline-restore-one (text n spec)
  "Restore tag N in TEXT from SPEC, or nil if the tag is missing."
  (let ((open (format "<t%d>" n))
        (close (format "</t%d>" n))
        (empty (format "<t%d/>" n)))
    (cond
     ((string-match (regexp-quote open) text)
      (let ((beg (match-beginning 0))
            (after (match-end 0)))
        (when (string-match (regexp-quote close) text after)
          (let ((inner (substring text after (match-beginning 0)))
                (head (substring text 0 beg))
                (rest (substring text (match-end 0))))
            (pcase spec
              (`(link ,url . ,maybe-orig)
               (let* ((orig (car maybe-orig))
                      (desc (if (and orig (+trans--org-desc-blown-p inner orig))
                                orig
                              (string-trim inner))))
                 (concat head (format "[[%s][%s]]" url desc) rest)))
              (`(wrap ,a ,b)
               (concat head a inner b rest))
              (`(link-bare ,url)
               (concat head (format "[[%s]]" url) rest))
              (`(raw ,s)
               (concat head s rest)))))))
     ((string-match (regexp-quote empty) text)
      (let ((repl (pcase spec
                    (`(link ,url . _) (format "[[%s][%s]]" url url))
                    (`(link-bare ,url) (format "[[%s]]" url))
                    (`(raw ,s) s)
                    (`(wrap ,a ,b) (concat a b)))))
        (when repl
          (replace-regexp-in-string (regexp-quote empty) repl text t t)))))))

(defun +trans--org-raw-oneline (s)
  "Collapse fill-wrapped verbatim S onto one line so Org can fontify it."
  (replace-regexp-in-string "[ \t]*\n[ \t]*" " " s))

(defun +trans--org-emphasis-pre-ok-p (text pos)
  "Return non-nil if TEXT[POS] can start Org verbatim/emphasis."
  (or (zerop pos)
      (memq (aref text (1- pos)) '(?\s ?\t ?\n ?\r ?- ?\( ?' ?\" ?{))))

(defun +trans--org-emphasis-post-ok-p (text pos)
  "Return non-nil if TEXT[POS] can follow Org verbatim/emphasis."
  (or (>= pos (length text))
      (memq (aref text pos)
            '(?\s ?\t ?\n ?\r ?- ?. ?, ?: ?! ?? ?\; ?' ?\" ?\) ?} ?\[ ?\\))))

(defun +trans--org-cjk-punct-ascii (ch)
  "ASCII stand-in for fullwidth punct CH, or nil."
  (pcase ch
    (?（ "(")
    (?） ")")
    (?， ",")
    (?。 ".")
    (?； ";")
    (?： ":")
    (?！ "!")
    (?？ "?")))

(defun +trans--org-fix-emphasis-borders (text)
  "Insert or normalize borders so `=verbatim=` next to CJK still fontifies.
Org only treats a few ASCII characters as valid neighbors of `=` / `~`."
  (let ((re (concat "=\\(?:[^= \t\n]\\|[^= \t\n][^=\n]*?[^= \t\n]\\)="
                    "\\|"
                    "~\\(?:[^~ \t\n]\\|[^~ \t\n][^~\n]*?[^~ \t\n]\\)~"))
        (start 0))
    (while (string-match re text start)
      (let ((beg (match-beginning 0))
            (end (match-end 0)))
        (unless (+trans--org-emphasis-pre-ok-p text beg)
          (let ((ascii (and (> beg 0)
                            (+trans--org-cjk-punct-ascii (aref text (1- beg))))))
            (if ascii
                (setq text (concat (substring text 0 (1- beg))
                                   ascii
                                   (substring text beg)))
              (setq text (concat (substring text 0 beg) " "
                                 (substring text beg))
                    beg (1+ beg)
                    end (1+ end)))))
        (unless (+trans--org-emphasis-post-ok-p text end)
          (let ((ascii (+trans--org-cjk-punct-ascii (aref text end))))
            (cond
             ((equal ascii "(")
              (setq text (concat (substring text 0 end) " ("
                                 (substring text (1+ end)))
                    end (+ end 2)))
             (ascii
              (setq text (concat (substring text 0 end) ascii
                                 (substring text (1+ end)))
                    end (+ end (length ascii)))
              (when (and (< end (length text))
                         (aref (char-category-set (aref text end)) ?c)
                         (not (+trans--org-cjk-punct-ascii (aref text end))))
                (setq text (concat (substring text 0 end) " "
                                   (substring text end))
                      end (1+ end))))
             (t
              (setq text (concat (substring text 0 end) " "
                                 (substring text end))
                    end (1+ end))))))
        (setq start end)))
    text))

(defun +trans--org-inline-restore (text alist)
  "Restore inline Org objects in TEXT using ALIST.
Return nil if any tag is missing so the caller can keep the original."
  (let ((out text)
        (ok t))
    (dolist (item (sort (copy-sequence alist)
                        (lambda (a b) (> (car a) (car b)))))
      (let ((next (+trans--org-inline-restore-one out (car item) (cdr item))))
        (if next
            (setq out next)
          (setq ok nil))))
    (when ok (+trans--org-fix-emphasis-borders out))))

(defun +trans--org-tag-note (text)
  "Prompt note telling the model to keep inline <tN> tags."
  (if (string-match-p "<t[0-9]+" text)
      "保留所有 <t数字>、</t数字>、<t数字/> 标签，一个字符都不要改；只翻译标签内外的普通文字。"
    ""))

(defun +trans--org-pack-core (core)
  "Return (PROTECTED ALIST ORIGINAL) for CORE."
  (pcase-let ((`(,prot . ,alist) (+trans--org-inline-protect core)))
    (list prot alist core)))

(defun +trans--org-unpack-core (translated alist original)
  "Restore TRANSLATED using ALIST, or return ORIGINAL if tags were lost."
  (if (null alist)
      (or translated original)
    (or (and translated (+trans--org-inline-restore translated alist))
        original)))

(defun +trans--org-partition (text)
  "Split TEXT into ((STR . keep|translate) ...) using Org markup ranges."
  (let ((ranges (+trans--org-keep-ranges text))
        (pos 0)
        (len (length text))
        parts)
    (dolist (r ranges)
      (when (< pos (car r))
        (push (cons (substring text pos (car r)) 'translate) parts))
      (push (cons (substring text (car r) (cadr r)) 'keep) parts)
      (setq pos (cadr r)))
    (when (< pos len)
      (push (cons (substring text pos) 'translate) parts))
    (nreverse parts)))

(defun +trans--has-words-p (s)
  "Return non-nil if S contains letters to translate."
  (string-match-p "[[:alpha:][:nonascii:]]" s))

(defun +trans--split-ws (text)
  "Return (LEAD CORE TRAIL) whitespace split of TEXT."
  (let* ((lead (if (string-match "\\`[ \t\n]+" text) (match-string 0 text) ""))
         (trail (if (string-match "[ \t\n]+\\'" text) (match-string 0 text) ""))
         (core (string-trim text)))
    (list lead core trail)))

(defun +trans--translate-preserving-space (text lang)
  "Translate TEXT but keep its leading and trailing whitespace."
  (if (not (+trans--has-words-p text))
      text
    (pcase-let ((`(,lead ,core ,trail) (+trans--split-ws text)))
      (if (string-empty-p core)
          text
        (concat lead (+trans--translate-raw core lang) trail)))))

(defun +trans--group-batches (jobs)
  "Group JOBS into request batches.
Each job is (IDX CORE LEAD TRAIL TO-EN)."
  (let (batches cur cur-en cur-chars)
    (dolist (job jobs)
      (let ((en (nth 4 job))
            (n (length (nth 1 job))))
        (when (or (null cur)
                  (not (eq en cur-en))
                  (>= (length cur) +trans-batch-items)
                  (> (+ cur-chars n) +trans-batch-chars))
          (when cur
            (push (nreverse cur) batches))
          (setq cur nil
                cur-en en
                cur-chars 0))
        (push job cur)
        (setq cur-chars (+ cur-chars n))))
    (when cur
      (push (nreverse cur) batches))
    (nreverse batches)))

(defun +trans--parse-batch (text n)
  "Parse numbered batch TEXT into N strings, or nil on mismatch.
Expected lines look like 1||translation."
  (let ((items (make-vector n nil))
        (re "^[ \t]*\\([0-9]+\\)||")
        (pos 0)
        (found 0))
    (while (and (< pos (length text))
                (string-match re text pos))
      (let* ((idx (1- (string-to-number (match-string 1 text))))
             (beg (match-end 0))
             (next (or (and (< beg (length text))
                            (string-match re text beg))
                       (length text)))
             (val (string-trim (substring text beg next))))
        (when (and (>= idx 0) (< idx n) (null (aref items idx)))
          (aset items idx val)
          (setq found (1+ found)))
        (setq pos next)))
    (when (= found n)
      (append items nil))))

(defun +trans--translate-cores (cores lang)
  "Translate CORES (list of strings) in as few requests as possible."
  (let ((n (length cores)))
    (cond
     ((= n 0) nil)
     ((= n 1) (list (+trans--translate-raw (car cores) lang)))
     (t
      (or (condition-case nil
              (let* ((to-en (eq lang 'en))
                     (target (if to-en "英文" "简体中文"))
                     (body (let ((i 0)
                                 lines)
                             (dolist (c cores)
                               (setq i (1+ i))
                               (push (format "%d||%s" i c) lines))
                             (string-join (nreverse lines) "\n")))
                     (prompt (format
                              "将下面编号片段分别翻译成%s。每条格式必须是 数字||译文 ，不要解释，不要改编号。%s\n%s"
                              target (+trans--org-tag-note body) body))
                     (raw (+trans--ollama-chat prompt)))
                (+trans--parse-batch raw n))
            (error nil))
          (mapcar (lambda (c) (+trans--translate-raw c lang)) cores))))))

(defun +trans--org-translate (text &optional lang progress)
  "Translate readable text in Org TEXT, leaving markup untouched.
PROGRESS if non-nil is called as (FN N TOTAL) before each API call."
  (let* ((parts (+trans--org-partition text))
         (jobs nil)
         (idx 0)
         (results (make-vector (length parts) nil)))
    (dolist (p parts)
      (when (and (eq (cdr p) 'translate) (+trans--has-words-p (car p)))
        (pcase-let ((`(,lead ,core ,trail) (+trans--split-ws (car p))))
          (unless (string-empty-p core)
            (pcase-let ((`(,prot ,alist ,orig) (+trans--org-pack-core core)))
              (push (list idx prot lead trail
                          (pcase lang
                            ('en t)
                            ('zh nil)
                            (_ (+trans--mostly-cjk-p core)))
                          alist orig)
                    jobs)))))
      (setq idx (1+ idx)))
    (setq jobs (nreverse jobs))
    (let* ((batches (+trans--group-batches jobs))
           (total (max 1 (length batches)))
           (done 0))
      (dolist (batch batches)
        (setq done (1+ done))
        (when progress
          (funcall progress done total))
        (let* ((lang (if (nth 4 (car batch)) 'en 'zh))
               (translated (+trans--translate-cores
                            (mapcar (lambda (j) (nth 1 j)) batch)
                            lang)))
          (let ((trs translated))
            (dolist (job batch)
              (aset results (car job)
                    (concat (nth 2 job)
                            (+trans--org-unpack-core
                             (car trs) (nth 5 job) (nth 6 job))
                            (nth 3 job)))
              (setq trs (cdr trs)))))))
    (let ((i 0)
          out)
      (dolist (p parts)
        (push (or (aref results i) (car p)) out)
        (setq i (1+ i)))
      (apply #'concat (nreverse out)))))

(defun +trans--in-comment-p ()
  "Return non-nil if point is inside a comment."
  (or (nth 4 (syntax-ppss))
      (save-excursion (comment-beginning))))

(defun +trans--sentence-sep-at-p (text pos atomic)
  "Return non-nil if TEXT at POS is a real sentence separator."
  (and (string-match-p "[。！？；;.!?]" (substring text pos (1+ pos)))
       (not (+trans--pos-in-ranges-p pos atomic))))

(defun +trans--sentence-span (text pos)
  "Return (START END) of the sentence in TEXT around 0-based index POS.
Periods inside Org verbatim, code, or links are not treated as
sentence boundaries."
  (let* ((len (length text))
         (pos (max 0 (min pos (max 0 (1- len)))))
         (atomic (+trans--org-atomic-ranges text))
         (start 0)
         (end len)
         i)
    (setq i (1- pos))
    (while (and (>= i 0)
                (not (+trans--sentence-sep-at-p text i atomic)))
      (setq i (1- i)))
    (setq start (if (>= i 0) (1+ i) 0))
    (setq i pos)
    (while (and (< i len)
                (not (+trans--sentence-sep-at-p text i atomic)))
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

(defun +trans--org-marker-char-p (ch)
  "Return non-nil if CH is an Org emphasis or verbatim marker."
  (memq ch '(?* ?= ?~ ?/ ?_ ?+)))

(defun +trans--chinese-run-char-p (ch)
  "Return non-nil if CH is a Chinese character or mid-sentence punctuation."
  (and ch
       (not (+trans--chinese-end-punct-p ch))
       (or (aref (char-category-set ch) ?c)
           (memq ch '(?， ?、 ?： ?（ ?） ?「 ?」 ?“ ?” ?《 ?》))
           (and (+trans--org-p) (+trans--org-marker-char-p ch)))))

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
   ((and (+trans--org-p)
         (let ((bounds (+trans--org-sentence-bounds)))
           (and bounds (+trans--mostly-cjk-p (nth 2 bounds)) bounds))))
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
In Org, use the current heading, paragraph, or list item and keep
emphasis/verbatim markup.  In code comments, only the current
line's sentence is used so a whole comment block is not sent.
In other source code, use the current line.  In prose, use the
sentence at point.  An active region always wins."
  (cond
   ((use-region-p)
    (buffer-substring-no-properties (region-beginning) (region-end)))
   ((+trans--org-p)
    (or (nth 2 (+trans--org-sentence-bounds))
        (string-trim (or (thing-at-point 'line t) ""))))
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
  "Return non-nil if the current line is a comment line.
Org headings and keywords are not treated as comments."
  (and (not (+trans--org-p))
       (save-excursion
         (let ((eol (line-end-position))
               (bol (line-beginning-position)))
           (or (nth 4 (syntax-ppss (max bol (1- eol))))
               (progn
                 (goto-char bol)
                 (looking-at-p "\\s-*\\(//+\\|;+\\|#+\\|--\\|/\\*+\\|\\*\\)")))))))

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
An active region always wins.  In Org, use the current heading,
paragraph, or list item and keep markup.  In comments, adjacent
comment lines are collected.  Otherwise use the text paragraph."
  (cond
   ((use-region-p)
    (buffer-substring-no-properties (region-beginning) (region-end)))
   ((+trans--org-p)
    (or (+trans--org-paragraph-at-point) ""))
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

(defun +trans--http-json-async (url payload timeout callback)
  "Fetch URL without blocking Emacs, then call CALLBACK with (JSON ERR).
PAYLOAD if non-nil is a JSON POST body.  Uses curl as a subprocess."
  (require 'json)
  (let* ((curl (or (executable-find "curl")
                   (error "需要 curl 才能后台翻译")))
         (buf (generate-new-buffer " *+trans-http*"))
         (args (if payload
                   (list curl "-sS" "--max-time"
                         (number-to-string (or timeout +trans-timeout))
                         "-H" "Content-Type: application/json"
                         "-d" "@-"
                         url)
                 (list curl "-sS" "--max-time"
                       (number-to-string (or timeout 8))
                       url)))
         (proc (make-process
                :name "+trans-http"
                :buffer buf
                :command args
                :connection-type 'pipe
                :noquery t)))
    (set-process-sentinel
     proc
     (lambda (p _event)
       (when (memq (process-status p) '(exit signal))
         (let ((status (process-exit-status p))
               (pbuf (process-buffer p)))
           (cond
            ((and +trans--job
                  (not (eq p (gethash :proc +trans--job))))
             (when (buffer-live-p pbuf) (kill-buffer pbuf)))
            ((or (not (zerop status)) (not (buffer-live-p pbuf)))
             (when (buffer-live-p pbuf) (kill-buffer pbuf))
             (funcall callback nil (format "curl 退出码 %s" status)))
            (t
             (let ((json nil)
                   (err nil))
               (with-current-buffer pbuf
                 (goto-char (point-min))
                 (condition-case e
                     (setq json (json-parse-buffer
                                 :object-type 'alist :array-type 'list))
                   (error (setq err (error-message-string e)))))
               (when (buffer-live-p pbuf) (kill-buffer pbuf))
               (funcall callback json err))))))))
    (when payload
      (process-send-string proc payload)
      (process-send-eof proc))
    (when +trans--job
      (puthash :proc proc +trans--job)
      (puthash :buf buf +trans--job))
    proc))

(defun +trans--ollama-chat-async (prompt callback)
  "Send PROMPT to Ollama asynchronously. CALLBACK is (lambda (TEXT ERR))."
  (require 'json)
  (let ((payload (json-encode
                  `((model . ,+trans-ollama-model)
                    (stream . :json-false)
                    (messages . [((role . "user")
                                  (content . ,prompt))])))))
    (+trans--http-json-async
     +trans-ollama-url payload +trans-timeout
     (lambda (json err)
       (cond
        (err (funcall callback nil err))
        (t
         (let ((text (alist-get 'content (alist-get 'message json))))
           (if (and (stringp text) (not (string-empty-p (string-trim text))))
               (funcall callback (string-trim text) nil)
             (funcall callback nil "Ollama 返回空译文")))))))))

(defun +trans--google-async (text target-code callback)
  "Translate TEXT with Google asynchronously. CALLBACK is (lambda (TEXT ERR))."
  (let ((url (concat "https://translate.googleapis.com/translate_a/single"
                     "?client=gtx&sl=auto&dt=t"
                     "&tl=" target-code
                     "&q=" (url-hexify-string text))))
    (+trans--http-json-async
     url nil 8
     (lambda (json err)
       (cond
        (err (funcall callback nil err))
        (t
         (let ((text (ignore-errors (mapconcat #'car (car json) ""))))
           (if (and (stringp text) (not (string-empty-p (string-trim text))))
               (funcall callback (string-trim text) nil)
             (funcall callback nil "Google 返回空译文")))))))))

(defun +trans--translate-raw-async (text lang callback)
  "Translate TEXT asynchronously. CALLBACK is (lambda (TEXT ERR))."
  (let* ((to-en (pcase lang
                  ('en t)
                  ('zh nil)
                  (_ (+trans--mostly-cjk-p text))))
         (target-name (if to-en "英文" "简体中文"))
         (target-code (if to-en "en" "zh-CN")))
    (+trans--ollama-chat-async
     (format "将下面文本翻译成%s。只输出译文，不要解释，不要引号。%s\n%s"
             target-name (+trans--org-tag-note text) text)
     (lambda (s err)
       (if (and s (not err))
           (funcall callback s nil)
         (+trans--google-async
          text target-code
          (lambda (g gerr)
            (funcall callback g (or gerr err)))))))))

(defun +trans--translate-cores-async (cores lang callback)
  "Translate CORES asynchronously. CALLBACK is (lambda (LIST ERR))."
  (let ((n (length cores)))
    (cond
     ((= n 0) (funcall callback nil nil))
     ((= n 1)
      (+trans--translate-raw-async
       (car cores) lang
       (lambda (s err)
         (funcall callback (and s (list s)) err))))
     (t
      (let* ((target (if (eq lang 'en) "英文" "简体中文"))
             (i 0)
             (lines nil))
        (dolist (c cores)
          (setq i (1+ i))
          (push (format "%d||%s" i c) lines))
        (let ((body (string-join (nreverse lines) "\n")))
          (+trans--ollama-chat-async
           (format "将下面编号片段分别翻译成%s。每条格式必须是 数字||译文 ，不要解释，不要改编号。%s\n%s"
                   target
                   (+trans--org-tag-note body)
                   body)
           (lambda (raw err)
             (let ((parsed (and raw (not err) (+trans--parse-batch raw n))))
               (if parsed
                   (funcall callback parsed nil)
                 (+trans--translate-cores-seq-async cores lang nil callback)))))))))))

(defun +trans--translate-cores-seq-async (cores lang acc callback)
  "Fallback: translate CORES one by one, then CALLBACK with the list."
  (if (null cores)
      (funcall callback (nreverse acc) nil)
    (+trans--translate-raw-async
     (car cores) lang
     (lambda (s err)
       (if err
           (funcall callback nil err)
         (+trans--translate-cores-seq-async
          (cdr cores) lang (cons s acc) callback))))))

(defun +trans--ollama-chat (prompt)
  "Send PROMPT to local Ollama and return the assistant text."
  (require 'json)
  (let* ((payload (json-encode
                   `((model . ,+trans-ollama-model)
                     (stream . :json-false)
                     (messages . [((role . "user")
                                   (content . ,prompt))]))))
         (json (+trans--url-json +trans-ollama-url payload))
         (result (alist-get 'content (alist-get 'message json))))
    (unless (and (stringp result) (not (string-empty-p (string-trim result))))
      (error "Ollama returned an empty translation"))
    (string-trim result)))

(defun +trans--translate-ollama (text target)
  "Translate TEXT to TARGET language via local Ollama."
  (let ((note (+trans--org-tag-note text)))
    (+trans--ollama-chat
     (format "将下面文本翻译成%s。只输出译文，不要解释，不要引号。%s\n%s"
             target note text))))

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

(defun +trans--translate-raw (text &optional lang)
  "Translate TEXT between Chinese and English without markup handling.
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

(defun +trans--translate-text (text &optional lang)
  "Translate TEXT between Chinese and English.
LANG is `en or `zh; nil means auto-detect from TEXT.
Org markup is never sent to the translator: only readable text
is translated, then stitched back around the original markers."
  (if (+trans--looks-like-org-p text)
      (+trans--org-translate text lang)
    (+trans--translate-raw text lang)))

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

(defun +trans--show-result-buffer (text &optional title mode)
  "Show TEXT in a normal *Translate* buffer, not a float.
MODE is the major mode to enable; default is `text-mode'."
  (let ((buf (get-buffer-create "*Translate*")))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert text)
        (goto-char (point-min)))
      (funcall (or mode #'text-mode))
      (setq-local header-line-format (or title "译文")))
    (pop-to-buffer buf)))

(defun +trans--buffer-running-p ()
  "Return non-nil if a whole-buffer translation job is active."
  (and +trans--job (hash-table-p +trans--job)))

(defun +trans--job-cleanup-files ()
  "Delete temp files of the current job."
  (when +trans--job
    (dolist (key '(:in-file :out-file))
      (let ((f (gethash key +trans--job)))
        (when (and f (file-exists-p f))
          (ignore-errors (delete-file f)))))))

(defun +trans--job-stop-proc ()
  "Kill the worker subprocess of the current job, if any."
  (when +trans--job
    (let ((proc (gethash :proc +trans--job))
          (buf (gethash :buf +trans--job)))
      (when (and proc (process-live-p proc))
        (delete-process proc))
      (when (and buf (buffer-live-p buf))
        (kill-buffer buf)))
    (+trans--job-cleanup-files)))

(defun +trans/translate-buffer-cancel ()
  "Cancel the background whole-buffer translation, if any."
  (interactive)
  (unless (+trans--buffer-running-p)
    (user-error "没有正在进行的整文件翻译"))
  (+trans--job-stop-proc)
  (setq +trans--job nil)
  (message "已取消整文件翻译"))

(defun +trans--job-fail (err)
  "Abort the current job and show ERR."
  (+trans--job-stop-proc)
  (setq +trans--job nil)
  (message "翻译失败：%s" err))

(defun +trans--job-apply-all (batch-results)
  "Write all BATCH-RESULTS into the job results vector."
  (let ((results (gethash :results +trans--job))
        (batches (gethash :batches +trans--job)))
    (while (and batches batch-results)
      (let ((trs (car batch-results)))
        (dolist (job (car batches))
          (aset results (car job)
                (concat (nth 2 job)
                        (+trans--org-unpack-core
                         (car trs) (nth 5 job) (or (nth 6 job) (nth 1 job)))
                        (nth 3 job)))
          (setq trs (cdr trs))))
      (setq batches (cdr batches)
            batch-results (cdr batch-results)))))

(defun +trans--job-stitch ()
  "Build the finished translation string from the current job."
  (let ((parts (gethash :parts +trans--job))
        (results (gethash :results +trans--job)))
    (if parts
        (let ((i 0)
              out)
          (dolist (p parts)
            (push (or (aref results i) (car p)) out)
            (setq i (1+ i)))
          (apply #'concat (nreverse out)))
      (string-join (append results nil) "\n\n"))))

(defun +trans--job-filter (_proc output)
  "Show worker progress lines without doing other work."
  (when +trans--job
    (dolist (line (split-string output "\n" t))
      (when (string-match "^PROGRESS \\([0-9]+\\) \\([0-9]+\\)" line)
        (message "正在翻译整文件 %s/%s..."
                 (match-string 1 line)
                 (match-string 2 line))))))

(defun +trans--job-sentinel (proc _event)
  "Read the worker output when PROC exits."
  (when (and +trans--job
             (eq proc (gethash :proc +trans--job))
             (memq (process-status proc) '(exit signal)))
    (let* ((status (process-exit-status proc))
           (out-file (gethash :out-file +trans--job))
           (src (gethash :src +trans--job))
           (org-p (gethash :org-p +trans--job))
           (n (gethash :total +trans--job))
           (parsed nil)
           (err nil))
      (condition-case e
          (when (and out-file (file-readable-p out-file))
            (setq parsed
                  (json-parse-string
                   (with-temp-buffer
                     (insert-file-contents out-file)
                     (buffer-string))
                   :object-type 'alist
                   :array-type 'list)))
        (error (setq err (error-message-string e))))
      (cond
       (err (+trans--job-fail err))
       ((and parsed (not (eq (alist-get 'ok parsed) t)))
        (+trans--job-fail (or (alist-get 'error parsed) "worker failed")))
       ((not (zerop status))
        (+trans--job-fail (format "worker 退出码 %s" status)))
       ((null parsed)
        (+trans--job-fail "worker 没有写出结果"))
       (t
        (+trans--job-apply-all (alist-get 'batches parsed))
        (let ((text (+trans--job-stitch)))
          (+trans--job-cleanup-files)
          (setq +trans--job nil)
          (+trans--show-result-buffer
           text
           (format "译文 · %s" src)
           (when org-p #'org-mode))
          (message "整文件翻译完成（%d 段）" n)))))))

(defun +trans--job-write-input (job)
  "Write JOB batches to a temp JSON file and return the path."
  (require 'json)
  (let ((file (make-temp-file "trans-in-" nil ".json"))
        (payload
         `((url . ,+trans-ollama-url)
           (model . ,+trans-ollama-model)
           (timeout . ,+trans-timeout)
           (batches . ,(apply #'vector
                              (mapcar
                               (lambda (batch)
                                 `((to_en . ,(if (nth 4 (car batch)) t :json-false))
                                   (cores . ,(apply #'vector
                                                    (mapcar (lambda (j) (nth 1 j))
                                                            batch)))))
                               (gethash :batches job)))))))
    (let ((coding-system-for-write 'utf-8-unix))
      (with-temp-file file
        (insert (json-encode payload))))
    file))

(defun +trans--job-start-worker (job)
  "Start the external Ollama worker for JOB."
  (let* ((python (or (executable-find "python3")
                     (user-error "后台翻译需要 python3")))
         (worker +trans-worker-program)
         (in-file (+trans--job-write-input job))
         (out-file (make-temp-file "trans-out-" nil ".json"))
         (buf (generate-new-buffer " *+trans-worker*"))
         (proc nil))
    (unless (file-readable-p worker)
      (user-error "找不到翻译 worker: %s" worker))
    (puthash :in-file in-file job)
    (puthash :out-file out-file job)
    (puthash :buf buf job)
    (setq proc
          (make-process
           :name "+trans-worker"
           :buffer buf
           :command (list python "-u" worker in-file out-file)
           :connection-type 'pipe
           :noquery t
           :filter #'+trans--job-filter
           :sentinel #'+trans--job-sentinel))
    (puthash :proc proc job)
    proc))

(defun +trans--job-from-text (text src org-p)
  "Build a background job table for TEXT."
  (let ((job (make-hash-table :test 'eq))
        parts jobs idx results)
    (puthash :src src job)
    (puthash :org-p org-p job)
    (if org-p
        (progn
          (setq parts (+trans--org-partition text)
                jobs nil
                idx 0
                results (make-vector (length parts) nil))
          (dolist (p parts)
            (when (and (eq (cdr p) 'translate) (+trans--has-words-p (car p)))
              (pcase-let ((`(,lead ,core ,trail) (+trans--split-ws (car p))))
                (unless (string-empty-p core)
                  (pcase-let ((`(,prot ,alist ,orig) (+trans--org-pack-core core)))
                    (push (list idx prot lead trail
                                (+trans--mostly-cjk-p core)
                                alist orig)
                          jobs)))))
            (setq idx (1+ idx)))
          (puthash :parts parts job))
      (let ((paras (or (+trans--split-paragraphs text) (list text))))
        (setq jobs nil
              idx 0
              results (make-vector (length paras) nil))
        (dolist (part paras)
          (push (list idx part "" "" (+trans--mostly-cjk-p part)) jobs)
          (setq idx (1+ idx)))))
    (setq jobs (nreverse jobs))
    (puthash :results results job)
    (puthash :batches (+trans--group-batches jobs) job)
    (puthash :done 0 job)
    (puthash :total (max 1 (length (gethash :batches job))) job)
    job))

(defun +trans/translate-buffer ()
  "Translate the current buffer in the background.
The original file is left unchanged.  Network waits run in a
subprocess so Emacs stays usable.  Cancel with `SPC y q'."
  (interactive)
  (when (+trans--buffer-running-p)
    (unless (y-or-n-p "已有整文件翻译在后台进行。取消并重新开始？")
      (user-error "已取消"))
    (+trans/translate-buffer-cancel))
  (let ((text (string-trim (buffer-substring-no-properties (point-min) (point-max))))
        (src (or (and buffer-file-name (file-name-nondirectory buffer-file-name))
                 (buffer-name)))
        (org-p nil))
    (when (string-empty-p text)
      (user-error "当前文件没有可翻译的内容"))
    (when (and (derived-mode-p 'prog-mode)
               (not (y-or-n-p "当前是代码文件，整文件翻译可能把代码也译掉。继续？")))
      (user-error "已取消"))
    (when (and (> (length text) 8000)
               (not (y-or-n-p (format "文本约 %d 字，翻译会较久。继续？" (length text)))))
      (user-error "已取消"))
    (unless (executable-find "python3")
      (user-error "后台翻译需要 python3"))
    (setq org-p (or (+trans--org-p) (+trans--looks-like-org-p text)))
    (setq +trans--job (+trans--job-from-text text src org-p))
    (+trans--job-start-worker +trans--job)
    (message "整文件翻译已在后台开始（SPC y q 取消）")))


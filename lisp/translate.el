;;; translate.el --- offline dict + local Ollama translation -*- lexical-binding: t; -*-
;;
;; 换电脑后：git clone 本仓库到 ~/.doom.d，然后按下面做一次（或 M-x +trans/doctor）。
;;
;; 1) Doom
;;    doom sync && doom doctor
;;
;; 2) 离线词典 SPC s w
;;    brew install sdcv          # Linux: apt/dnf install sdcv
;;    # 朗道英汉/汉英 5.0 已放在仓库 share/stardict/，clone 后即可用
;;
;; 3) 句/段/整文件翻译 SPC y s / y p / y f
;;    brew install ollama python3
;;    ollama serve               # 本机 127.0.0.1:11434
;;    ollama pull qwen2.5-coder:7b
;;    # 整文件翻译还要用 bin/trans-worker.py（已随仓库），需要 python3
;;    # Ollama 不可用时，句/段会回退到 Google（需要外网）
;;
;; 快捷键：SPC s w 查词；SPC y s 句；SPC y p 段；SPC y f 整文件；
;;         SPC y q 取消整文件；SPC y e 中译英替换；SPC y d 检查环境
;;
;;; Code:

(defvar +trans-dict-dir
  (expand-file-name
   "share/stardict"
   (or (bound-and-true-p doom-user-dir)
       (expand-file-name "~/.doom.d")))
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

(defconst +trans--re-verbatim
  "=\\(?:[^= \t\n]\\|[^= \t\n][^=]*?[^= \t\n]\\)="
  "Org verbatim, allowing a fill-wrapped newline inside.")

(defconst +trans--re-code
  "~\\(?:[^~ \t\n]\\|[^~ \t\n][^~\n]*?[^~ \t\n]\\)~")

(defconst +trans--re-link
  "\\[\\[\\([^][\n]+\\)\\]\\[\\([^][\n]+\\)\\]\\]")

(defconst +trans--re-link-bare
  "\\[\\[\\([^][\n]+\\)\\]\\]")

(defconst +trans--re-link-any
  "\\[\\[[^][\n]+\\]\\(?:\\[[^][\n]+\\]\\)?\\]")

(defconst +trans--re-bold
  "\\*\\*\\([^*\n]+\\)\\*\\*")

(defconst +trans--re-strike
  "\\+\\(?:[^+ \t\n]\\|[^+ \t\n][^\n]*?[^+ \t\n]\\)\\+")

(defconst +trans--re-verbatim-line
  "=\\(?:[^= \t\n]\\|[^= \t\n][^=\n]*?[^= \t\n]\\)=")

(defconst +trans--comment-prefix-re
  "\\`\\s-*\\(//+\\|;+\\|#+\\|--\\|/\\*+\\|\\*+/?\\)\\s-*")

(defconst +trans--comment-line-re
  "\\s-*\\(//+\\|;+\\|#+\\|--\\|/\\*+\\|\\*\\)")

(defconst +trans--comment-suffix-re
  "\\s-*\\*/\\s-*\\'")

(defconst +trans--float-hint
  "v 选中  y 复制  Y 全部复制  q 关闭")


;;; Dictionary and float UI

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
       :desc "Chinese to English" "e" #'+trans/zh-to-en
       :desc "Check translation setup" "d" #'+trans/doctor))

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
    (message (or hint +trans--float-hint))))

(defun +trans--show-float (word)
  "Show WORD's offline dictionary result in a posframe tooltip."
  (require 'sdcv)
  (+trans--show-float-text
   (+trans--format-float-text
    (sdcv-search-with-dictionary word sdcv-dictionary-simple-list))))

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


;;; Small helpers

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

(defun +trans--to-en-p (text lang)
  "Whether TEXT should be translated to English.
LANG is `en, `zh, or nil for auto-detect."
  (pcase lang
    ('en t)
    ('zh nil)
    (_ (+trans--mostly-cjk-p text))))

(defun +trans--target-name (to-en)
  (if to-en "英文" "简体中文"))

(defun +trans--target-code (to-en)
  (if to-en "en" "zh-CN"))

(defun +trans--org-p ()
  "Return non-nil if the current buffer is Org."
  (derived-mode-p 'org-mode))

(defun +trans--looks-like-org-p (text)
  "Return non-nil if TEXT contains common Org markup."
  (or (+trans--org-p)
      (string-match-p
       "\\(?:\\*\\*[^*\n]+\\*\\*\\|=[^= \t\n][^=\n]*=\\|~[^~ \t\n][^~\n]*~\\|^\\*+ \\|#\\+\\)"
       text)))

(defun +trans--has-words-p (s)
  "Return non-nil if S contains letters to translate."
  (string-match-p "[[:alpha:][:nonascii:]]" s))

(defun +trans--split-ws (text)
  "Return (LEAD CORE TRAIL) whitespace split of TEXT."
  (let* ((lead (if (string-match "\\`[ \t\n]+" text) (match-string 0 text) ""))
         (trail (if (string-match "[ \t\n]+\\'" text) (match-string 0 text) ""))
         (core (string-trim text)))
    (list lead core trail)))

(defun +trans--split-paragraphs (text)
  "Split TEXT into paragraphs separated by blank lines."
  (split-string text "\n[ \t]*\n+" t "[ \t\n\r]+"))

(defun +trans--splice (text beg end insert)
  "Replace TEXT[BEG,END) with INSERT."
  (concat (substring text 0 beg) insert (substring text end)))

(defun +trans--pos-in-ranges-p (pos ranges)
  "Return non-nil if 0-based POS is inside any (START END ...) in RANGES."
  (let (hit)
    (dolist (r ranges hit)
      (when (and (>= pos (car r)) (< pos (cadr r)))
        (setq hit t)))))

(defun +trans--re-scan (text re fn)
  "Call FN for each match of RE in TEXT. FN receives no args; match data is set."
  (let ((start 0))
    (while (string-match re text start)
      (funcall fn)
      (setq start (match-end 0)))))

(defun +trans--re-ranges (text regexps)
  "Return ((START END) ...) for every match of REGEXPS in TEXT."
  (let (ranges)
    (dolist (re regexps)
      (+trans--re-scan text re
                       (lambda ()
                         (push (list (match-beginning 0) (match-end 0)) ranges))))
    ranges))

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


;;; Org structure (what must not be translated)

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

(defun +trans--org-keep-re (text re ranges len)
  "Add every match of RE in TEXT to RANGES."
  (+trans--re-scan text re
                   (lambda ()
                     (setq ranges (+trans--org-keep-add
                                   ranges (match-beginning 0) (match-end 0) len))))
  ranges)

(defun +trans--org-keep-ranges (text)
  "Return ((START END) ...) spans of Org markup that must not be translated."
  (let ((case-fold-search t)
        (len (length text))
        ranges)
    (setq ranges
          (+trans--org-keep-re
           text
           "#\\+BEGIN_\\(SRC\\|EXAMPLE\\|EXPORT\\|COMMENT\\)\\(?:.\\|\n\\)*?#\\+END_\\(?:SRC\\|EXAMPLE\\|EXPORT\\|COMMENT\\)"
           ranges len))
    (setq ranges
          (+trans--org-keep-re
           text ":\\(?:PROPERTIES\\|LOGBOOK\\):\\(?:.\\|\n\\)*?:END:"
           ranges len))
    (let ((start 0))
      (while (string-match "^[ \t]*#\\+\\(.*\\)$" text start)
        (let ((lb (match-beginning 0))
              (le (match-end 0))
              (body (match-string 1 text)))
          (setq ranges
                (if (string-match "\\`\\(TITLE\\|SUBTITLE\\|DESCRIPTION\\):[ \t]*" body)
                    (+trans--org-keep-add ranges lb (+ lb 2 (match-end 0)) len)
                  (+trans--org-keep-add ranges lb le len))
                start (min len (1+ le))))))
    (setq ranges
          (+trans--org-keep-re
           text
           (format "^\\*+\\s-+\\(?:\\(?:%s\\)\\s-+\\)?\\(?:\\[#[A-Za-z0-9]\\]\\s-+\\)?"
                   (+trans--org-todo-re))
           ranges len))
    (setq ranges
          (+trans--org-keep-re
           text "[ \t]+\\(:[[:alnum:]_@#%:]+:\\)[ \t]*$" ranges len))
    (setq ranges
          (+trans--org-keep-re
           text "^[ \t]*\\(?:[-+]\\|[0-9]+[.)]\\)\\s-+" ranges len))
    (+trans--merge-ranges ranges)))

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

(defun +trans--org-atomic-ranges (text)
  "Return ((START END) ...) spans in TEXT that must not be split."
  (+trans--re-ranges text (list +trans--re-verbatim +trans--re-code
                                +trans--re-link-any)))


;;; Org inline objects (links, verbatim) — protect around the model

(defun +trans--org-raw-oneline (s)
  "Collapse fill-wrapped verbatim S onto one line so Org can fontify it."
  (replace-regexp-in-string "[ \t]*\n[ \t]*" " " s))

(defun +trans--org-inline-collect (text)
  "Return ((BEG END KIND ...) ...) for inline Org objects in TEXT."
  (let (spans)
    (+trans--re-scan text +trans--re-link
                     (lambda ()
                       (push (list (match-beginning 0) (match-end 0) 'link
                                   (match-string 1 text) (match-string 2 text))
                             spans)))
    (dolist (spec
             (list
              (list +trans--re-link-bare
                    (lambda ()
                      (list (match-beginning 0) (match-end 0) 'link-bare
                            (match-string 1 text))))
              (list +trans--re-verbatim
                    (lambda ()
                      (list (match-beginning 0) (match-end 0) 'raw
                            (+trans--org-raw-oneline (match-string 0 text)))))
              (list +trans--re-code
                    (lambda ()
                      (list (match-beginning 0) (match-end 0) 'raw
                            (match-string 0 text))))
              (list +trans--re-bold
                    (lambda ()
                      (list (match-beginning 0) (match-end 0) 'wrap
                            "**" "**" (match-string 1 text))))
              (list +trans--re-strike
                    (lambda ()
                      (list (match-beginning 0) (match-end 0) 'wrap
                            "+" "+" (substring (match-string 0 text) 1 -1))))))
      (+trans--re-scan text (car spec)
                       (lambda ()
                         (unless (+trans--pos-in-ranges-p (match-beginning 0) spans)
                           (push (funcall (cadr spec)) spans)))))
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
           (setq out (+trans--splice out beg end
                                     (format "<t%d>%s</t%d>" n (nth 4 sp) n))))
          ((or 'link-bare 'raw)
           (push (cons n (list kind (nth 3 sp))) alist)
           (setq out (+trans--splice out beg end (format "<t%d/>" n))))
          ('wrap
           (push (cons n (list 'wrap (nth 3 sp) (nth 4 sp))) alist)
           (setq out (+trans--splice out beg end
                                     (format "<t%d>%s</t%d>" n (nth 5 sp) n)))))))
    (cons out alist)))

(defun +trans--org-desc-blown-p (inner orig)
  "Return non-nil if INNER is far longer than original link text ORIG."
  (let ((a (length (string-trim (or inner ""))))
        (b (length (or orig ""))))
    (and (> b 0) (> a (* 3 b)))))

(defun +trans--org-restore-markup (spec inner)
  "Build restored markup from SPEC and optional INNER text."
  (pcase spec
    (`(link ,url . ,maybe-orig)
     (let* ((orig (car maybe-orig))
            (desc (cond
                   ((null inner) url)
                   ((and orig (+trans--org-desc-blown-p inner orig)) orig)
                   (inner (string-trim inner))
                   (t url))))
       (format "[[%s][%s]]" url desc)))
    (`(link-bare ,url) (format "[[%s]]" url))
    (`(raw ,s) s)
    (`(wrap ,a ,b) (concat a (or inner "") b))))

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
          (let* ((inner (substring text after (match-beginning 0)))
                 (head (substring text 0 beg))
                 (rest (substring text (match-end 0)))
                 (markup (+trans--org-restore-markup spec inner)))
            (when markup
              (concat head markup rest))))))
     ((string-match (regexp-quote empty) text)
      (let ((repl (+trans--org-restore-markup spec nil)))
        (when repl
          (replace-regexp-in-string (regexp-quote empty) repl text t t)))))))

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

(defun +trans--org-insert-at (text pos insert)
  "Insert INSERT at POS in TEXT. Return (NEW-TEXT NEW-POS-AFTER-INSERT)."
  (list (concat (substring text 0 pos) insert (substring text pos))
        (+ pos (length insert))))

(defun +trans--org-replace-at (text pos insert)
  "Replace the char at POS with INSERT. Return (NEW-TEXT NEW-POS-AFTER)."
  (list (concat (substring text 0 pos) insert (substring text (1+ pos)))
        (+ pos (length insert))))

(defun +trans--org-fix-emphasis-borders (text)
  "Insert or normalize borders so `=verbatim=` next to CJK still fontifies."
  (let ((re (concat +trans--re-verbatim-line "\\|" +trans--re-code))
        (start 0))
    (while (string-match re text start)
      (let ((beg (match-beginning 0))
            (end (match-end 0)))
        (unless (+trans--org-emphasis-pre-ok-p text beg)
          (let ((ascii (and (> beg 0)
                            (+trans--org-cjk-punct-ascii (aref text (1- beg))))))
            (if ascii
                (setq text (car (+trans--org-replace-at text (1- beg) ascii)))
              (pcase-let ((`(,next ,new-end) (+trans--org-insert-at text beg " ")))
                (setq text next
                      beg new-end
                      end (1+ end))))))
        (unless (+trans--org-emphasis-post-ok-p text end)
          (let ((ascii (+trans--org-cjk-punct-ascii (aref text end))))
            (cond
             ((equal ascii "(")
              (pcase-let ((`(,next ,new-end)
                           (+trans--org-replace-at text end " (")))
                (setq text next end new-end)))
             (ascii
              (pcase-let ((`(,next ,new-end)
                           (+trans--org-replace-at text end ascii)))
                (setq text next end new-end)
                (when (and (< end (length text))
                           (aref (char-category-set (aref text end)) ?c)
                           (not (+trans--org-cjk-punct-ascii (aref text end))))
                  (pcase-let ((`(,next2 ,new-end2)
                               (+trans--org-insert-at text end " ")))
                    (setq text next2 end new-end2)))))
             (t
              (pcase-let ((`(,next ,new-end)
                           (+trans--org-insert-at text end " ")))
                (setq text next end new-end))))))
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


;;; Jobs: one shape for sentence, paragraph, and whole-buffer work

;; Job list: (idx core lead trail to-en alist original)
(defsubst +trans--j-idx (j) (nth 0 j))
(defsubst +trans--j-core (j) (nth 1 j))
(defsubst +trans--j-lead (j) (nth 2 j))
(defsubst +trans--j-trail (j) (nth 3 j))
(defsubst +trans--j-to-en (j) (nth 4 j))
(defsubst +trans--j-alist (j) (nth 5 j))
(defsubst +trans--j-orig (j) (nth 6 j))

(defun +trans--make-job (idx core lead trail to-en &optional alist original)
  (list idx core lead trail to-en alist original))

(defun +trans--jobs-from-parts (parts lang)
  "Build translation jobs from PARTS ((STR . keep|translate) ...).
LANG is `en, `zh, or nil.  Return (JOBS RESULTS-VECTOR)."
  (let (jobs
        (idx 0)
        (results (make-vector (length parts) nil)))
    (dolist (p parts)
      (when (and (eq (cdr p) 'translate) (+trans--has-words-p (car p)))
        (pcase-let ((`(,lead ,core ,trail) (+trans--split-ws (car p))))
          (unless (string-empty-p core)
            (pcase-let ((`(,prot ,alist ,orig) (+trans--org-pack-core core)))
              (push (+trans--make-job idx prot lead trail
                                      (+trans--to-en-p core lang)
                                      alist orig)
                    jobs)))))
      (setq idx (1+ idx)))
    (list (nreverse jobs) results)))

(defun +trans--jobs-from-paras (paras)
  "Build jobs from plain PARAS.  Return (JOBS RESULTS-VECTOR)."
  (let (jobs
        (idx 0)
        (results (make-vector (length paras) nil)))
    (dolist (part paras)
      (push (+trans--make-job idx part "" "" (+trans--mostly-cjk-p part)) jobs)
      (setq idx (1+ idx)))
    (list (nreverse jobs) results)))

(defun +trans--group-batches (jobs)
  "Group JOBS into request batches of the same target language."
  (let (batches cur cur-en cur-chars)
    (dolist (job jobs)
      (let ((en (+trans--j-to-en job))
            (n (length (+trans--j-core job))))
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

(defun +trans--apply-batch (results jobs translations)
  "Write TRANSLATIONS for JOBS into RESULTS."
  (let ((trs translations))
    (dolist (job jobs)
      (aset results (+trans--j-idx job)
            (concat (+trans--j-lead job)
                    (+trans--org-unpack-core
                     (car trs)
                     (+trans--j-alist job)
                     (or (+trans--j-orig job) (+trans--j-core job)))
                    (+trans--j-trail job)))
      (setq trs (cdr trs)))))

(defun +trans--stitch-parts (parts results)
  "Join PARTS, substituting RESULTS for translated slots."
  (let ((i 0)
        out)
    (dolist (p parts)
      (push (or (aref results i) (car p)) out)
      (setq i (1+ i)))
    (apply #'concat (nreverse out))))

(defun +trans--run-batches (jobs results progress)
  "Translate JOB batches synchronously into RESULTS."
  (let* ((batches (+trans--group-batches jobs))
         (total (max 1 (length batches)))
         (done 0))
    (dolist (batch batches)
      (setq done (1+ done))
      (when progress
        (funcall progress done total))
      (+trans--apply-batch
       results batch
       (+trans--translate-cores
        (mapcar #'+trans--j-core batch)
        (if (+trans--j-to-en (car batch)) 'en 'zh))))
    results))


;;; Org at point

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
        ((or 'paragraph 'verse-block 'quote-block 'table-cell 'item)
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


;;; Text at point (sentence / paragraph / comment / Chinese)

(defun +trans--in-comment-p ()
  "Return non-nil if point is inside a comment."
  (or (nth 4 (syntax-ppss))
      (save-excursion (comment-beginning))))

(defun +trans--sentence-sep-at-p (text pos atomic)
  "Return non-nil if TEXT at POS is a real sentence separator."
  (and (string-match-p "[。！？；;.!?]" (substring text pos (1+ pos)))
       (not (+trans--pos-in-ranges-p pos atomic))))

(defun +trans--sentence-span (text pos)
  "Return (START END) of the sentence in TEXT around 0-based index POS."
  (let* ((len (length text))
         (pos (max 0 (min pos (max 0 (1- len)))))
         (atomic (+trans--org-atomic-ranges text))
         start end i)
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

(defun +trans--comment-line-p ()
  "Return non-nil if the current line is a comment line."
  (and (not (+trans--org-p))
       (save-excursion
         (let ((eol (line-end-position))
               (bol (line-beginning-position)))
           (or (nth 4 (syntax-ppss (max bol (1- eol))))
               (progn
                 (goto-char bol)
                 (looking-at-p +trans--comment-line-re)))))))

(defun +trans--comment-line-body ()
  "Return current line's comment text without markers, or nil if empty."
  (let* ((line (buffer-substring-no-properties
                (line-beginning-position) (line-end-position)))
         (body (replace-regexp-in-string +trans--comment-prefix-re "" line)))
    (setq body (replace-regexp-in-string +trans--comment-suffix-re "" body))
    (setq body (string-trim body))
    (unless (string-empty-p body)
      body)))

(defun +trans--comment-line-sentence-bounds ()
  "Return (BEG END TEXT) for the sentence on the current comment line."
  (let* ((line-beg (line-beginning-position))
         (line (buffer-substring-no-properties line-beg (line-end-position)))
         (prefix (if (string-match +trans--comment-prefix-re line)
                     (match-end 0)
                   (if (string-match "\\`\\s-*" line)
                       (match-end 0)
                     0)))
         (suffix (if (string-match +trans--comment-suffix-re
                                   (substring line prefix))
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

(defun +trans--comment-paragraph-at-point ()
  "Return adjacent comment lines around point, without comment markers."
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

(defun +trans--org-marker-char-p (ch)
  "Return non-nil if CH is an Org emphasis or verbatim marker."
  (memq ch '(?* ?= ?~ ?/ ?_ ?+)))

(defun +trans--chinese-end-punct-p (ch)
  "Return non-nil if CH ends a Chinese sentence."
  (memq ch '(?。 ?！ ?？ ?；)))

(defun +trans--chinese-run-char-p (ch)
  "Return non-nil if CH is a Chinese character or mid-sentence punctuation."
  (and ch
       (not (+trans--chinese-end-punct-p ch))
       (or (aref (char-category-set ch) ?c)
           (memq ch '(?， ?、 ?： ?（ ?） ?「 ?」 ?“ ?” ?《 ?》))
           (and (+trans--org-p) (+trans--org-marker-char-p ch)))))

(defun +trans--chinese-run-at-point ()
  "Return (BEG END TEXT) for a contiguous Chinese sentence around point."
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
  "Return text to translate around point."
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

(defun +trans--paragraph-at-point ()
  "Return the paragraph to translate around point."
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


;;; Engines

(defun +trans--prompt-one (target text)
  (format "将下面文本翻译成%s。只输出译文，不要解释，不要引号。%s\n%s"
          target (+trans--org-tag-note text) text))

(defun +trans--prompt-batch (target cores)
  (let ((i 0)
        lines)
    (dolist (c cores)
      (setq i (1+ i))
      (push (format "%d||%s" i c) lines))
    (let ((body (string-join (nreverse lines) "\n")))
      (format "将下面编号片段分别翻译成%s。每条格式必须是 数字||译文 ，不要解释，不要改编号。%s\n%s"
              target (+trans--org-tag-note body) body))))

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

(defun +trans--google-url (text target-code)
  (concat "https://translate.googleapis.com/translate_a/single"
          "?client=gtx&sl=auto&dt=t"
          "&tl=" target-code
          "&q=" (url-hexify-string text)))

(defun +trans--translate-google (text target-code)
  "Translate TEXT to TARGET-CODE via the public Google Translate endpoint."
  (let* ((json (+trans--url-json (+trans--google-url text target-code) nil 8))
         (result (mapconcat #'car (car json) "")))
    (unless (and (stringp result) (not (string-empty-p (string-trim result))))
      (error "Google Translate returned an empty translation"))
    (string-trim result)))

(defun +trans--translate-raw (text &optional lang)
  "Translate TEXT between Chinese and English without markup handling."
  (let* ((to-en (+trans--to-en-p text lang))
         (target-name (+trans--target-name to-en))
         (target-code (+trans--target-code to-en))
         (ollama-err nil)
         (result nil))
    (setq result
          (condition-case err
              (+trans--ollama-chat (+trans--prompt-one target-name text))
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

(defun +trans--parse-batch (text n)
  "Parse numbered batch TEXT into N strings, or nil on mismatch."
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
              (+trans--parse-batch
               (+trans--ollama-chat
                (+trans--prompt-batch (+trans--target-name (eq lang 'en)) cores))
               n)
            (error nil))
          (mapcar (lambda (c) (+trans--translate-raw c lang)) cores))))))

(defun +trans--org-translate (text &optional lang progress)
  "Translate readable text in Org TEXT, leaving markup untouched."
  (let ((parts (+trans--org-partition text)))
    (pcase-let ((`(,jobs ,results) (+trans--jobs-from-parts parts lang)))
      (+trans--run-batches jobs results progress)
      (+trans--stitch-parts parts results))))

(defun +trans--translate-text (text &optional lang)
  "Translate TEXT between Chinese and English."
  (if (+trans--looks-like-org-p text)
      (+trans--org-translate text lang)
    (+trans--translate-raw text lang)))


;;; Interactive commands

(defun +trans--prompt-source (arg prompt getter)
  "Read text to translate: prefix ARG prompts, else GETTER, else PROMPT."
  (string-trim
   (or (and arg (read-string prompt))
       (funcall getter)
       (read-string prompt))))

(defun +trans--show-pair (src dst &optional dst-first)
  "Show SRC and DST in a float, or echo DST on TTY."
  (let ((text (if dst-first
                  (format "译文\n%s\n\n原文\n%s" dst src)
                (format "原文\n%s\n\n译文\n%s" src dst))))
    (if (and (display-graphic-p) (require 'posframe nil t))
        (+trans--show-float-text text)
      (message (if dst-first "译文: %s" "%s") dst))))

(defun +trans--doctor-dict-names ()
  "Return bookname values from StarDict .ifo files."
  (let (names)
    (when (file-directory-p +trans-dict-dir)
      (dolist (ifo (directory-files-recursively +trans-dict-dir "\\.ifo\\'"))
        (with-temp-buffer
          (insert-file-contents ifo)
          (when (re-search-forward "^bookname=\\(.*\\)$" nil t)
            (push (string-trim (match-string 1)) names)))))
    (nreverse names)))

(defun +trans--doctor-ollama-p ()
  "Return non-nil if the local Ollama chat endpoint answers."
  (require 'url)
  (let ((url (replace-regexp-in-string "/api/chat\\'" "/api/tags" +trans-ollama-url)))
    (ignore-errors
      (with-current-buffer (url-retrieve-synchronously url t t 3)
        (goto-char (point-min))
        (and (re-search-forward "\n\n" nil t) t)))))

(defun +trans/doctor ()
  "Check tools this module needs. Run after moving to a new machine."
  (interactive)
  (let* ((sdcv (executable-find "sdcv"))
         (py (executable-find "python3"))
         (worker (and (stringp +trans-worker-program)
                      (file-readable-p +trans-worker-program)))
         (dict-dir (file-directory-p +trans-dict-dir))
         (books (+trans--doctor-dict-names))
         (need '("朗道英汉字典5.0" "朗道汉英字典5.0"))
         (missing (delq nil
                        (mapcar (lambda (n) (unless (member n books) n)) need)))
         (ollama-bin (executable-find "ollama"))
         (ollama-up (+trans--doctor-ollama-p))
         (lines
          (list
           "翻译环境检查（换电脑后跑一次）"
           ""
           (if sdcv (format "OK  sdcv  %s" sdcv) "缺  sdcv  →  brew install sdcv")
           (if dict-dir
               (format "OK  词典目录  %s" +trans-dict-dir)
             (format "缺  词典目录  %s  →  确认仓库里有 share/stardict" +trans-dict-dir))
           (if (null missing)
               (format "OK  朗道词典  %s" (string-join books " / "))
             (format "缺  词典  %s  →  检查 share/stardict 是否完整"
                     (string-join missing "、")))
           (if py (format "OK  python3  %s" py) "缺  python3  →  brew install python3")
           (if worker
               (format "OK  worker  %s" +trans-worker-program)
             (format "缺  worker  %s" +trans-worker-program))
           (if ollama-bin
               (format "OK  ollama  %s" ollama-bin)
             "缺  ollama  →  brew install ollama && ollama pull qwen2.5-coder:7b")
           (if ollama-up
               (format "OK  Ollama 服务  %s  模型 %s"
                       +trans-ollama-url +trans-ollama-model)
             (format "缺  Ollama 服务  →  ollama serve && ollama pull %s"
                     +trans-ollama-model))
           ""
           "快捷键  SPC s w 查词  SPC y s 句  SPC y p 段  SPC y f 整文件  SPC y q 取消  SPC y e 中译英")))
    (+trans--show-result-buffer (string-join lines "\n") "翻译环境检查")))

(defun +trans/translate-sentence (&optional arg)
  "Translate the selected region or the sentence at point."
  (interactive "P")
  (let ((text (+trans--prompt-source arg "Translate: " #'+trans--sentence-at-point)))
    (when (string-empty-p text)
      (user-error "没有可翻译的文本"))
    (message "正在翻译...")
    (+trans--show-pair text (+trans--translate-text text))))

(defun +trans/translate-paragraph (&optional arg)
  "Translate the selected region or the paragraph at point."
  (interactive "P")
  (let ((text (+trans--prompt-source
               arg "Translate paragraph: " #'+trans--paragraph-at-point)))
    (when (string-empty-p text)
      (user-error "没有可翻译的段落"))
    (message "正在翻译整段...")
    (+trans--show-pair text (+trans--translate-text text) t)))

(defun +trans/zh-to-en (&optional arg)
  "Translate Chinese at point into English and replace it."
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


;;; Whole-buffer worker

(defun +trans--show-result-buffer (text &optional title mode)
  "Show TEXT in a normal *Translate* buffer, not a float."
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
      (+trans--apply-batch results (car batches) (car batch-results))
      (setq batches (cdr batches)
            batch-results (cdr batch-results)))))

(defun +trans--job-stitch ()
  "Build the finished translation string from the current job."
  (let ((parts (gethash :parts +trans--job))
        (results (gethash :results +trans--job)))
    (if parts
        (+trans--stitch-parts parts results)
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
                                 `((to_en . ,(if (+trans--j-to-en (car batch))
                                                 t :json-false))
                                   (cores . ,(apply #'vector
                                                    (mapcar #'+trans--j-core
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
         proc)
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
        parts jobs results)
    (puthash :src src job)
    (puthash :org-p org-p job)
    (if org-p
        (progn
          (setq parts (+trans--org-partition text))
          (pcase-let ((`(,js ,rs) (+trans--jobs-from-parts parts nil)))
            (setq jobs js results rs))
          (puthash :parts parts job))
      (pcase-let ((`(,js ,rs)
                   (+trans--jobs-from-paras
                    (or (+trans--split-paragraphs text) (list text)))))
        (setq jobs js results rs)))
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
        org-p)
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

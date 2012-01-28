;; 外部ファイル読み込み
;; (load-file "~/filepath")

;; (thing-at-point 'word)
;; (define-generic-mode)

(unless (file-directory-p (expand-file-name "~/.emacs.d"))
  (make-directory (expand-file-name "~/.emacs.d")))
(unless (file-directory-p (expand-file-name "~/.emacs.d/lisp"))
  (make-directory (expand-file-name "~/.emacs.d/lisp")))
(add-to-list 'load-path "~/.emacs.d/lisp")

(require 'cl)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; start and quit

(setq inhibit-startup-message t)
(setq frame-title-format (list '(:eval (my-format-time-string))
                               " | %b "
                               '(:eval (number-to-string (length (buffer-list-not-start-with-space))))
                               " buffers ["
                               invocation-name
                               " "
                               emacs-version
                               " "
                               (symbol-name system-type)
                               "] "
                               '(:eval (symbol-name last-command))))
(defun buffer-list-not-start-with-space ()
  (let ((bl (buffer-list))
        b nbl)
    (while bl
      (setq b (pop bl))
      (unless (string-equal " "
                            (substring (buffer-name b)
                                       0
                                       1))
        (add-to-list 'nbl b)))
    nbl))
(setq confirm-kill-emacs 'y-or-n-p)
(setq gc-cons-threshold (* 1024 1024 4))

(when window-system
  (add-to-list 'default-frame-alist '(cursor-type . box))
  (add-to-list 'default-frame-alist '(background-color . "white"))
  (add-to-list 'default-frame-alist '(foreground-color . "gray10"))
  ;; (add-to-list 'default-frame-alist '(alpha . (80 100 100 100))) ;聞いてないみたい
  )
(if window-system (menu-bar-mode 1) (menu-bar-mode 0))
(tool-bar-mode 0)
(set-scroll-bar-mode nil)
(add-hook 'kill-emacs-hook      ; 終了時に読み込んで壊れてないか調べる
          (lambda ()
            (when (file-readable-p "~/.emacs")
              (load-file "~/.emacs"))))

(add-hook 'after-init-hook
          (lambda ()
            ;; (message "init time: %d msec"
            ;;          (+ (* (- (nth 1 after-init-time) (nth 1 before-init-time)) 1000)
            ;;             (/ (- (nth 2 after-init-time) (nth 2 before-init-time)) 1000)))
            (message (emacs-init-time))
            (switch-to-buffer "*Messages*")
            ))

(cd ".")  ; when using windows use / instead of \ in `default-directory'

;; locale
(set-language-environment "Japanese")
(set-default-coding-systems 'utf-8-unix)
(prefer-coding-system 'utf-8-unix)
(setq system-time-locale "C")

(defvar my-prefix-map
  (make-sparse-keymap))
(add-hook 'after-init-hook
          (lambda ()
            (define-key ctl-x-map (kbd "C-x") my-prefix-map)))
(define-key my-prefix-map (kbd "C-q") 'quoted-insert)
(define-key my-prefix-map (kbd "C-z") 'suspend-frame)


;; display
(setq redisplay-dont-pause t)
(setq visible-bell t)
(setq ring-bell-function 'ignore)

;; (comint-show-maximum-output)

;; kill scratch
(add-hook 'after-init-hook
          (lambda ()
            (kill-buffer "*scratch*")))

(defun my-delete-frame-or-kill-emacs ()
  "delete frame when opening multiple frame, kill emacs when only one."
  (interactive)
  (if (eq 1
          (length (frame-list)))
      (save-buffers-kill-emacs)
    (delete-frame)))
(global-set-key (kbd "C-x C-c") 'my-delete-frame-or-kill-emacs)
(define-key my-prefix-map (kbd "C-x C-c") 'save-buffers-kill-emacs)

;; modifier keys
(setq mac-option-modifier 'control)
(setq w32-apps-modifier 'meta)

;; http://www.emacswiki.org/emacs/ChangingCursorDynamically
;; why saving buffer?
;; Change cursor color according to mode
(defvar hcz-set-cursor-color-color "")
(defvar hcz-set-cursor-color-buffer "")
(defun hcz-set-cursor-color-according-to-mode ()
  "change cursor color according to some minor modes."
  ;; set-cursor-color is somewhat costly, so we only call it when needed:
  (let ((color
         (if buffer-read-only "blue"
           (if overwrite-mode "yellow"
             "black"))))
    (unless (and
             (string= color hcz-set-cursor-color-color)
             (string= (buffer-name) hcz-set-cursor-color-buffer))
      (set-cursor-color (setq hcz-set-cursor-color-color color))
      (setq hcz-set-cursor-color-buffer (buffer-name)))))
(and window-system
     (add-hook 'post-command-hook 'hcz-set-cursor-color-according-to-mode))

(defun my-set-mode-line-color-read-only ()
  ""
  (let ((state (if buffer-read-only
                   'readonly
                 (if overwrite-mode
                     'overwrite
                   'insert))))
    (unless (eq state my-set-mode-line-color-state)
      (set-face-foreground 'modeline
                           (nth 1
                                (assq state
                                      my-set-mode-line-color-color)))
      (set-face-background 'modeline
                           (nth 2
                                (assq state
                                      my-set-mode-line-color-color)))
      (setq my-set-mode-line-color-state state))))
(defvar my-set-mode-line-color-color nil "")
(setq my-set-mode-line-color-color
      '((readonly "blue" "white")
        (overwrite "red" "white")
        (insert nil nil)))
(defvar my-set-mode-line-color-state nil "")
(add-hook 'post-command-hook 'my-set-mode-line-color-read-only)
(add-hook 'after-init-hook 'my-set-mode-line-color-read-only)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; mode-line

(setq eol-mnemonic-dos "crlf")
(setq eol-mnemonic-mac "cr")
(setq eol-mnemonic-unix "lf")

(which-function-mode 0)

(line-number-mode 0)
(column-number-mode 0)
(size-indication-mode 0)
(setq mode-line-position
      '(:eval (format "L%%l/%d,C%%c"
                      (count-lines (point-max)
                                   (point-min)))))

;; http://www.geocities.jp/simizu_daisuke/bunkei-meadow.html#frame-title
;; display date
(add-hook 'after-init-hook
          (lambda ()
            ;; (setq display-time-string-forms
            ;;       '(dayname ", " day " " monthname " " year " " 24-hours ":"minutes ":" seconds))
            (setq display-time-string-forms
                  '((my-format-time-string)))
            (when display-time-mode
              (display-time-update))
            ))
(setq display-time-interval 29)
(setq display-time-day-and-date t)
(if window-system
    (display-time-mode 0)
  (display-time-mode 1))

;; ;; current directory
;; (let ((ls (member 'mode-line-buffer-identification
;;                   mode-line-format)))
;;   (setcdr ls
;;           (cons '(:eval (concat " ("
;;                                 (abbreviate-file-name default-directory)
;;                                 ")"))
;;                 (cdr ls))))

;; ;; display last modified time
;; (let ((ls (member 'mode-line-buffer-identification
;;                   mode-line-format)))
;;   (setcdr ls
;;           (cons '(:eval (concat " "
;;                                 my-buffer-file-last-modified-time))
;;                 (cdr ls))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; minibuffer

(setq insert-default-directory t)
(setq completion-ignore-case t
      read-file-name-completion-ignore-case t
      read-buffer-completion-ignore-case t)
(setq resize-mini-window t)
(temp-buffer-resize-mode 1)

(fset 'yes-or-no-p 'y-or-n-p)

(define-key read-expression-map (kbd "TAB") 'lisp-complete-symbol) ; complete symbol when `eval'

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; letters, font-lock mode and fonts

;; change color for border
;; (set-face-foreground (make-face 'vertical-border-face) "white")
;; (set-face-background 'vertical-border-face "black")
;; (defface vertical-border-face
;;   `((((background dark))
;;      (:background "white"))
;;     (((background light))
;;      (:background "black")))
;;   "vertical border")
;; (set-display-table-slot standard-display-table 'vertical-border
;;                         (make-glyph-code #x3a 'vertical-border-face))
;; (set-face-foreground 'vertical-border "default")
;; (set-face-background 'vertical-border "white")

(when (eq system-type 'Darwin)
  (mac-set-input-method-parameter 'japanese 'cursor-color "red")
  (mac-set-input-method-parameter 'roman 'cursor-color "black"))

(when (and (boundp 'input-method-activate-hook) ;ちょっと正しいかわかんない
           (boundp 'input-method-inactivate-hook))
  (add-hook 'input-method-activate-hook
            (lambda () (set-cursor-color "red")))
  (add-hook 'input-method-inactivate-hook
            (lambda () (set-cursor-color "black"))))

(show-paren-mode 1)
(setq show-paren-style 'mixed)

(transient-mark-mode 1)

(global-font-lock-mode 1)
(setq font-lock-global-modes
      '(not
        help-mode
        eshell-mode
        term-mode
        Man-mode))

(standard-display-ascii ?\n "$\n")
(copy-face 'default 'my-eol-face)
(set-face-foreground 'my-eol-face "green")
;; (defface my-eol-face
;;   '((t (:foreground "green")))
;;   "eol.")

(standard-display-ascii ?\f "---------------------------------------------------------------------------------------^L")
(defface my-pagebreak-face
  '((t (:foreground "gray")))
  "pagebreak.")

(defvar my-eol-face
  '(("\n" . (0 my-eol-face t nil)))
  )
(defvar my-pagebreak-face
  '(("\f" . 'my-pagebreak-face)))
(defvar my-highlight-face
  '(("\t" . '(0 highlight t nil))
    ("　" . '(0 highlight t nil))))

;; (defvar my-face
;;   '(("\t" . 'highlight)
;;     ("　" . 'highlight)
;;     ("\n" . '(0 my-eol-face t nil))
;;     ("\f" . 'my-pagebreak-face)))

;; 現在行をハイライト
;; http://wiki.riywo.com/index.php?Meadow
(defface hlline-face
  '((((type x w32)
      (class color)
      (background dark))
     (:background "midnightblue")) ; :foreground "white")) ;; ハイライトの文字色は変えない方がいいかも
    (((type x w32)
      (class color)
      (background light))
     (:background "gainsboro"))
    (t
     (:underline "black")))
  "*Face used by hl-line.")
;; (defface hlline-ul-face
;;   '((t (:underline "yellow")))
;;   "underline yellow")
(setq hl-line-face 'hlline-face) ;; (setq hl-line-face nil)
(global-hl-line-mode 1) ;; (hl-line-mode 1)
(setq hl-line-global-modes
      '(not
        term-mode))

(add-hook 'font-lock-mode-hook
          (lambda ()
            ;; (font-lock-add-keywords nil my-eol-face)
            ;; (font-lock-add-keywords nil my-highlight-face)
            ))

(set-face-foreground 'font-lock-regexp-grouping-backslash "#666")
(set-face-foreground 'font-lock-regexp-grouping-construct "#f60")

;; fonts

(defun my-set-ascii-and-jp-font-with-size (list)
  ""
  (if (> emacs-major-version 22)
      (progn                            ; 23 or later
        (set-face-attribute 'default nil
                            :family (nth 0 list)
                            :height (nth 1 list))
        (set-fontset-font "fontset-default"
                          'japanese-jisx0208
                          (font-spec :family (nth 2 list) :size (nth 3 list)))
        (set-fontset-font "fontset-default"
                          'katakana-jisx0201
                          (font-spec :family (nth 2 list) :size (nth 3 list))))
                                        ; font spec is available in emacs23 and later, cannot used in emacs22
    (progn                              ; 22
      (set-face-attribute 'default nil
                          :family (nth 0 list)
                          :height (nth 1 list))
      (set-fontset-font "fontset-default"
                        'japanese-jisx0208
                        (cons (nth 2 list) "jisx0208.*"))
      (set-fontset-font "fontset-default"
                        'katakana-jisx0201
                        (cons (nth 2 list) "jisx0201.*"))
      )))
;; (my-set-ascii-and-jp-font-with-size '("dejavu sans mono" 90 "takaogothic" 13))
;; (my-set-ascii-and-jp-font-with-size '("dejavu sans mono" 100 "takaogothic" 14))
;; (my-set-ascii-and-jp-font-with-size '("dejavu sans mono" 100 "ms gothic" 14))
;; (my-set-ascii-and-jp-font-with-size '("monaco" 75 "takaogothic" 11))
;; (my-set-ascii-and-jp-font-with-size '("monaco" 90 "takaogothic" 13))
;; (my-set-ascii-and-jp-font-with-size '("ProggyCleanTTSZ" 120 "takaogothic" 11))
;; あ a

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; file handling

(setq revert-without-query ".+")

;; カーソルの場所を保存する
(when (require 'saveplace nil t)
  (setq-default save-place t))

;; http://www.bookshelf.jp/soft/meadow_24.html#SEC260
(setq make-backup-files t)
;; (make-directory (expand-file-name "~/.emacsbackup"))
(setq backup-directory-alist
      (cons (cons "\\.*$" (expand-file-name "~/.emacs.d/backup"))
            backup-directory-alist))
(setq version-control 'never)
(setq delete-old-versions t)

(setq auto-save-list-file-prefix (expand-file-name "~/.emacs.d/autosave/"))
(setq delete-auto-save-files t)

(add-to-list 'completion-ignored-extensions ".bak")
;; (setq delete-by-moving-to-trash t
;;       trash-directory "~/.emacs.d/trash")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; editting
(setq require-final-newline t)
(setq kill-whole-line t)
(setq scroll-conservatively 35
      scroll-margin 2
      scroll-step 0)                    ;4行ずつスクロール?
(setq default-major-mode 'text-mode)
(setq next-line-add-newlines nil)
(setq kill-read-only-ok t)
(setq truncate-partial-width-windows nil) ; when splitted horizontally
;; (setq-default line-spacing 0.2)
(setq-default indicate-empty-lines t)   ; なんだろうこれ
(setq-default tab-width 4)
(setq-default indent-tabs-mode nil)
(setq-default indent-line-function nil)
(pc-selection-mode 1)
(delete-selection-mode 1)
(cua-mode 0)

;; key bindings
;; moving around
;; (global-set-key (kbd "M-j") 'next-line)
;; (global-set-key (kbd "M-k") 'previous-line)
;; (global-set-key (kbd "M-h") 'backward-char)
;; (global-set-key (kbd "M-l") 'forward-char)
;;(keyboard-translate ?\M-j ?\C-j)
(global-set-key (kbd "M-p") 'backward-paragraph)
(global-set-key (kbd "M-n") 'forward-paragraph)
(global-set-key (kbd "C-<up>") (lambda () (interactive)(scroll-down 1)))
(global-set-key (kbd "C-<down>") (lambda () (interactive)(scroll-up 1)))
(global-set-key (kbd "C-<left>") 'scroll-down)
(global-set-key (kbd "C-<right>") 'scroll-up)
(global-set-key (kbd "<select>") 'previous-line-mark)
(define-key ctl-x-map (kbd "M-x") 'execute-extended-command)
(define-key ctl-x-map (kbd "M-:") 'eval-expression)

;; C-h and DEL
(global-set-key (kbd "C-h") (kbd "DEL"))
;; (global-set-key (kbd "C-h") 'backward-delete-char-untabify)
;; (global-set-key (kbd "DEL") help-map)
;; (global-set-key (kbd "C-h") (lambda ()
;;                               (interactive)
;;                               (call-interactively (key-binding (kbd "DEL")))))
;; (keyboard-translate ?\^h ?\^?) ; scimにはC-hを送りたい
;; (keyboard-translate ?\b ?\^h)          ; dont translate backspace

(global-set-key (kbd "C-m") 'reindent-then-newline-and-indent)
(global-set-key (kbd "C-o")
                ;; (lambda ()
                ;;   (interactive)
                ;;   (move-end-of-line nil)
                ;;   (newline-and-indent))
                (kbd "C-e C-m")
                )
(global-set-key (kbd "C-k") 'kill-whole-line)
(global-set-key (kbd "M-k") 'my-copy-whole-line)
;; (global-set-key "\C-z" 'undo) ; undo is C-/
;; (global-set-key (kbd "C-<return>") (lambda () (interactive) (insert "\f\n")))
(global-set-key (kbd "M-u") 'undo)
(global-set-key (kbd "C-r") 'query-replace-regexp)
(global-set-key (kbd "C-s") 'isearch-forward-regexp)
(global-set-key (kbd "M-i")
                (lambda ()
                  (interactive)
                  (call-interactively (key-binding (kbd "M-TAB"))))
                ;; (kbd "M-TAB")
                )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; gmail

(setq mail-interactive t
      send-mail-function 'smtpmail-send-it
      ;; message-send-mail-function 'smtpmail-send-it
      smtpmail-smtp-server "smtp.gmail.com"
      smtpmail-smtp-service 587
      smtpmail-starttls-credentials '(("smtp.gmail.com" 587 "8.slashes@gmail.com" nil))
      smtpmail-auth-credentials '(("smtp.gmail.com" 587 "8.slashes@gmail.com" nil))
      user-mail-address "8.slashes@gmail.com")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; buffer killing

(defun kill-buffer-by-major-mode (mode &optional exclude-current-buffer-p) ;mapcarとかつかって全部書き換える
  "kill buffers.
if EXCLUDE-CURRENT-BUFFER-P is non-nil, never kill current buffer"
  (interactive "xmajor mode of buffer to kill: ")
  (save-excursion
    (let ((bflist (buffer-list))
          (cbf (current-buffer))
          bf)
      (while bflist
        (setq bf (pop bflist))
        (set-buffer bf)
        (if (and (eq mode major-mode)   ;メジャーモードが一致し、かつ
                 (not (and exclude-current-buffer-p ;今のバッファを除外、今のバッファと一致 がともには満たされない
                           (eq bf cbf))))
            (kill-buffer bf))))))

(defun my-kill-this-buffer-when-hide (&optional buffer all-frames)
  ""
  (interactive)
  (let ((bf (or buffer
                (current-buffer))))
    (if (or (not buffer) (get-buffer-window bf all-frames))
        (run-with-timer 3 nil 'my-kill-this-buffer-when-hide bf all-frames)
      (kill-buffer bf))))
;; (add-hook 'dired-mode-hook
;;           'my-kill-this-buffer-when-hide)

(defvar my-kill-previous-buffer nil)
(defun my-kill-previous-buffer ()
  ""
  (when my-kill-previous-buffer
    (kill-buffer my-kill-previous-buffer))
  (setq my-kill-previous-buffer (current-buffer)))
;; (add-hook 'dired-mode-hook
;;           'my-kill-previous-buffer)

(defun my-query-kill-this-buffer ()
  ""
  (interactive)
  (if (y-or-n-p (concat "kill this buffer? :"))
      (kill-buffer (current-buffer))))
(substitute-key-definition 'kill-buffer 'my-query-kill-this-buffer global-map)
;;(global-set-key "\C-xk" 'my-query-kill-this-buffer)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; for emacsclient
;; (if window-system (server-start))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; global keys

(define-key my-prefix-map (kbd "C-f") 'make-frame-command)
(define-key my-prefix-map (kbd "C-o") 'occur)
(define-key my-prefix-map (kbd "C-s") 'my-execute-terminal)

;; (define-key my-prefix-map (kbd "C-h") help-map)
(global-set-key (kbd "C-\\") help-map)
(define-key ctl-x-map (kbd "DEL") help-map)
(define-key ctl-x-map (kbd "C-h") help-map)
(define-key help-map "a" 'apropos)

;; compose window
(global-set-key [?\C--] 'other-window)
(global-set-key [?\C-0] 'delete-window)
(global-set-key [?\C-1] 'delete-other-windows)
(global-set-key [?\C-2] 'split-window-vertically)
(global-set-key [?\C-3] 'split-window-horizontally)

;; disable annoying keys
(global-set-key [prior] 'ignore)
(global-set-key (kbd "<next>") 'ignore)
(global-set-key [menu] 'ignore)
(global-set-key [down-mouse-1] 'ignore)
(global-set-key [down-mouse-2] 'ignore)
(global-set-key [down-mouse-3] 'ignore)
(global-set-key [mouse-1] 'ignore)
(global-set-key [mouse-2] 'ignore)
(global-set-key [mouse-3] 'ignore)
(global-set-key (kbd "<eisu-toggle>") 'ignore)
(global-set-key (kbd "C-<eisu-toggle>") 'ignore)

(mouse-avoidance-mode 'banish)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; download library from web

;; (require 'url)

;; (defun dllib-if-needed (lib url &optional callback bite-compile-p force-download-p) ; dont use it
;;   "if LIB does not exist, download it from URL and rename to \"~/emacs.d/lisp/LIB.el\".
;; after download LIB successfully call CALLBACK. if LIB already exist, call CALLBACK immediately."
;;   (let* ((dir (expand-file-name "~/.emacs.d/lisp/"))
;;          (lpath (concat dir lib ".el")))
;;     (and (if (or force-download-p (not (locate-library lib)))
;;              (condition-case nil
;;                  (progn (url-copy-file url
;;                                        lpath
;;                                        t)
;;                         (and bite-compile-p
;;                              (byte-compile-file lpath)
;;                              t))
;;                (error (message "downloading %s...something wrong happened!" url)
;;                       nil))
;;            t)
;;          callback
;;          (funcall callback))))

(defun dllib-if-unfound (lib url &optional bite-compile-p force-download-p) ; new version
  "if LIB does not exist, download it from URL and locate it to \"~/emacs.d/lisp/LIB.el\".
return nil if LIB unfound and downloading failed, otherwise the path of LIB."
  (let* ((dir (expand-file-name "~/.emacs.d/lisp/"))
         (lpath (concat dir lib ".el"))
         (locate-p (locate-library lib)))
    (if (or force-download-p (not locate-p))
        (progn (condition-case nil
                   (progn (message "downloading %s..." url)
                          (url-copy-file url
                                         lpath
                                         t)
                          (when bite-compile-p
                            (byte-compile-file lpath)))
                 (error (message "downloading %s...something wrong happened!" url)
                        nil))
               (locate-library lib))
      locate-p)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;requireが必要なelispおよびhook

(require 'simple nil t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; share clipboard with x
(when (and window-system
          ;; (getenv "DESKTOP_SESSION")
          (not (eq window-system 'mac))
          )
  (setq x-select-enable-clipboard t     ; these settings seems to be useless when using emacs in terminal
        x-select-enable-primary nil)
  (global-set-key "\C-y" 'x-clipboard-yank))

;; urlに細かい説明あり。でも設定は上記だけでよさそう
;; http://garin.jp/doc/Linux/xwindow_clipboard

(and (not x-select-enable-clipboard)
     (executable-find "xclip")
     (dllib-if-unfound "xclip" "http://www.emacswiki.org/emacs/download/xclip.el" t)
     (require 'xclip nil t)
     (turn-on-xclip))

;; その他のhook
(add-hook 'after-save-hook
          'executable-make-buffer-file-executable-if-script-p)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; mode関連

(add-hook 'verilog-mode-hook
          (lambda ()
            (define-key verilog-mode-map ";" 'self-insert-command)))

(setq diff-switches "-u")
(add-hook 'diff-mode-hook
          (lambda ()
            (view-mode 1)
            (set-face-foreground 'diff-index-face "blue")
            (set-face-foreground 'diff-hunk-header-face "magenda")
            (set-face-foreground 'diff-removed-face "red")
            (set-face-foreground 'diff-added-face "blue")
            (set-face-foreground 'diff-changed-face "syan")
            ))

;; (ffap-bindings)

(add-hook 'sh-mode-hook
          (lambda ()
            (define-key sh-mode-map (kbd "C-x C-e") 'my-execute-shell-command-current-line)))
(defun my-execute-shell-command-current-line ()
  ""
  (interactive)
  (shell-command (buffer-substring-no-properties (point-at-bol)
                                                 (point))))

(add-hook 'inferior-python-mode-hook
          (lambda ()
            (define-key inferior-python-mode-map (kbd "<up>") 'comint-previous-input)
            (define-key inferior-python-mode-map (kbd "<down>") 'comint-next-input)))

(add-to-list 'Info-default-directory-list (expand-file-name "~/.info/emacs-ja"))

(setq bookmark-default-file "~/.emacs.d/bmk")

(add-hook 'apropos-mode-hook
          (lambda ()
            (define-key apropos-mode-map "j" 'next-line)
            (define-key apropos-mode-map "k" 'previous-line)))

(define-key minibuffer-local-map (kbd "C-u") (lambda () (interactive) (delete-region (point-at-bol) (point-at-eol))))

(add-hook 'isearch-mode-hook
          (lambda ()
            ;; (define-key isearch-mode-map (kbd "C-j") 'isearch-other-control-char)
            ;; (define-key isearch-mode-map (kbd "C-k") 'isearch-other-control-char)
            ;; (define-key isearch-mode-map (kbd "C-h") 'isearch-other-control-char)
            (define-key isearch-mode-map (kbd "C-h") 'isearch-delete-char)
            (define-key isearch-mode-map (kbd "M-r") 'isearch-query-replace-regexp)))

(add-hook 'outline-mode-hook
          (lambda ()
            (if (string-match "\\.md$" buffer-file-name)
                (set (make-local-variable 'outline-regexp) "#+ "))))
(add-to-list 'auto-mode-alist (cons "\\.ol$" 'outline-mode))

(add-to-list 'auto-mode-alist (cons "\\.md$" 'outline-mode))
(when (dllib-if-unfound "markdown-mode"
                        "http://jblevins.org/projects/markdown-mode/markdown-mode.el"
                        t)
  (add-to-list 'auto-mode-alist (cons "\\.md$" 'markdown-mode))
  (autoload 'markdown-mode "markdown-mode" "Major mode for editing Markdown files." nil)
  (add-hook 'markdown-mode-hook
            (lambda ()
              (outline-minor-mode 1)
              (set (make-local-variable 'comment-start) ";"))))

;; (add-hook 'c-mode-hook
;;           (lambda ()
;;             (set (make-local-variable 'comment-start) "//")
;;             (set (make-local-variable 'comment-end) "")))

;; http://d.hatena.ne.jp/emergent/20070203/1170512717
;; c-mode
;; (setq c-default-style "bsd")
;; BackSpace キーを「賢く」し，インデント幅は2桁，タブはスペースに展開
(add-hook 'c-mode-common-hook
          (lambda ()
            (setq c-basic-offset 2
                  indent-tabs-mode nil)
            ;; (set-face-foreground 'font-lock-keyword-face "blue")
            (c-toggle-hungry-state 1)
            ))

(defun my-compile-c-this-file ()
  ""
  (interactive)
  (compile (format "gcc -Wall -g -o %s %s"
                   (file-name-sans-extension buffer-file-name)
                   buffer-file-name)))
;; (when (require 'c nil t)(c-toggle-hungry-state t)

(when (dllib-if-unfound "js2-mode"
                        "https://github.com/mooz/js2-mode/raw/master/js2-mode.el"
                        t)
  (autoload 'js2-mode "js2-mode" nil t)
  (add-to-list 'auto-mode-alist '("\\.js$" . js2-mode)))
;; (add-hook 'js2-mode-hook
;;           (lambda ()
;;             (add-hook 'before-save-hook
;;                       'my-indent-buffer
;;                       nil
;;                       t)))
(add-hook 'js2-mode-hook
          (lambda ()
            (define-key js2-mode-map (kbd "C-m") (lambda ()
                                                   (interactive)
                                                   (js2-enter-key)
                                                   (indent-for-tab-command)))
            (add-hook (kill-local-variable 'before-save-hook)
                      'js2-before-save)))

(and nil
     (require 'zone nil t)
     (not (eq system-type 'windows-nt))
     ;; (zone-when-idle 180)
     (run-with-idle-timer 180 t (lambda ()
                                  (unless (memq major-mode
                                                '(term-mode))
                                    (zone)))))

(when (require 'uniquify nil t)
  (setq uniquify-buffer-name-style 'post-forward-angle-brackets)
  (setq uniquify-ignore-buffers-re "*[^*]+*")
  (setq uniquify-min-dir-content 1))

(add-hook 'view-mode-hook
          (lambda()
            (define-key view-mode-map "j" (lambda() (interactive) (scroll-up 1)))
            (define-key view-mode-map "k" (lambda() (interactive) (scroll-down 1)))
            (define-key view-mode-map "/" 'isearch-forward)
            (define-key view-mode-map "v" 'toggle-read-only)
            (define-key view-mode-map "q" 'bury-buffer)))
(global-set-key "\M-r" 'view-mode)
(setq view-read-only t)
;; (add-hook 'find-file-hook
;;           (lambda ()
;;             (when buffer-read-only
;;               (view-mode 1))))

(add-hook 'Man-mode-hook
          (lambda ()
            (view-mode 1)
            (setq truncate-lines nil)))
(setq Man-notify-method (if window-system
                            'newframe
                          'pushy))

;; (when (and (executable-find "git")
;;            (require 'sgit-mode nil t))
;;   (add-hook 'find-file-hook
;;             'sgit-load))

(require 'session nil t)

(when (require 'gtkbm nil t)
  (global-set-key (kbd "C-x C-d") 'gtkbm))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; frame buffer

;; (add-hook 'after-make-frame-functions
;;           (lambda (frame)
;;             (recentf-open-files)))

;; (defvar aaa nil)
;; (plist-get aaa 'abc)
;; (setq aaa (plist-put aaa 'abc 'efg))

(defvar my-frame-buffer-plist nil)
;; (setplist my-frame-buffer-plist nil)

(defun my-frame-buffer-add ()
  ""
  (setq my-frame-buffer-plist
        (plist-put my-frame-buffer-plist
                   (selected-frame)
                   (let ((lst (my-frame-buffer-get)))
                     (if lst
                         (add-to-list 'lst
                                      (current-buffer))
                       (list (current-buffer)))))))

(defun my-frame-buffer-remove ()
  ""
  (setq my-frame-buffer-plist
        (plist-put my-frame-buffer-plist
                   (selected-frame)
                   (delq (current-buffer)
                         (my-frame-buffer-get)))))

(defun my-frame-buffer-get (&optional frame)
  ""
  (plist-get my-frame-buffer-plist
             (or frame
                 (selected-frame))))

(defun my-frame-buffer-kill-all-buffer (frame)
  ""
  (mapcar 'kill-buffer
          (my-frame-buffer-get frame)))

(add-hook 'find-file-hook
          'my-frame-buffer-add)
;; (add-hook 'dired-mode-hook
;;           'my-frame-buffer-add)
(add-hook 'kill-buffer-hook
          'my-frame-buffer-remove)
(add-hook 'delete-frame-functions
          'my-frame-buffer-kill-all-buffer)

(frame-parameters (selected-frame))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; term mode

;; (setq multi-term-program shell-file-name)
(and (dllib-if-unfound "multi-term"
                       "http://www.emacswiki.org/emacs/download/multi-term.el"
                       t)
     (require 'multi-term nil t)
     (setq multi-term-switch-after-close nil))

;; http://d.hatena.ne.jp/goinger/20100416/1271399150
;; (setq term-ansi-default-program shell-file-name)
(add-hook 'term-setup-hook (lambda ()
                             (setq term-display-table (make-display-table))))
(add-hook 'term-mode-hook (lambda ()
                            (unless (memq (current-buffer) (and (featurep 'multi-term) ; current buffer is not multi-term buffer
                                                                (multi-term-list)))
                              ;; (define-key term-raw-map "\C-q" 'move-beginning-of-line)
                              ;; (define-key term-raw-map "\C-r" 'term-send-raw)
                              ;; (define-key term-raw-map "\C-s" 'term-send-raw)
                              ;; (define-key term-raw-map "\C-f" 'forward-char)
                              ;; (define-key term-raw-map "\C-b" 'backward-char)
                              ;; (define-key term-raw-map "\C-t" 'set-mark-command)
                              (define-key term-raw-map "\C-x" (lookup-key (current-global-map) "\C-x"))
                              (define-key term-raw-map "\C-z" (lookup-key (current-global-map) "\C-z")))
                            (define-key term-raw-map (kbd "ESC") 'term-send-raw)
                            (define-key term-raw-map [delete] 'term-send-raw)
                            (define-key term-raw-map "\C-h" 'term-send-backspace)
                            (define-key term-raw-map "\C-y" 'term-paste)
                            (define-key term-raw-map "\C-c" 'term-send-raw) ;; 'term-interrupt-subjob)
                            ;; (dolist (key '("<up>" "<down>" "<right>" "<left>"))
                            ;;   (define-key term-raw-map (kbd key) 'term-send-raw))
                            ;; (define-key term-raw-map "\C-d" 'delete-char)
                            (set (make-variable-buffer-local 'scroll-margin) 0)
                            ;; (set (make-variable-buffer-local 'cua-enable-cua-keys) nil)
                            ;; (cua-mode 0)
                            ;; (and cua-mode
                            ;;      (local-unset-key (kbd "C-c")))
                            ;; (define-key cua--prefix-override-keymap "\C-c" 'term-interrupt-subjob)
                            ))
;; (add-hook 'term-exec-hook 'forward-char)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; buffer switching

(when (require 'bs nil t)
  ;; (global-set-key "\C-x\C-b" 'bs-show)
  (defalias 'list-buffers 'bs-show))

;; (add-to-list 'bs-configurations '("processes" nil get-buffer-process ".*" nil nil))
(add-to-list 'bs-configurations '("same-dir" nil buffer-same-dir-p ".*" nil nil))
(add-to-list 'bs-configurations '("this-frame" nil (lambda (buf) (memq buf (my-frame-buffer-get))) ".*" nil nil))
;; (setq bs-configurations (list '("processes" nil get-buffer-process ".*" nil nil)
;;                               '("files-and-scratch" "^\\*scratch\\*$" nil nil bs-visits-non-file bs-sort-buffer-interns-are-last)))
(setq bs-default-configuration "this-frame")
(setq bs-default-sort-name "by name")
(add-hook 'bs-mode-hook
          (lambda ()
            (setq bs-default-configuration "this-frame")
            (set (make-variable-buffer-local 'scroll-margin) 0)
            ))

(defun buffer-same-dir-p (bf)
  "return t if BF's dir is same as current dir, otherwise nil."
  (let ((cdir (expand-file-name default-directory)))
    (with-current-buffer bf
      (equal (expand-file-name default-directory) cdir))))

(defun echo-buffer-list (&optional blist)
  "echo buffer list as string. BLIST is list with buffer objects as elements.
if arg is omitted use value of `buffer-list'."
  (interactive)
  (message (or (mapconcat (lambda (bf)
                            (concat (buffer-name bf)
                                    "\t"
                                    (with-current-buffer bf
                                      (symbol-name major-mode))
                                    "\t"
                                    (abbreviate-file-name (buffer-file-name bf))))
                          (or blist
                              (buffer-list))
                          "\n")
               "")))

(defun my-buffer-list ()
  "return buffer list."
  (delq nil
        (mapcar (lambda (bf)
                  (with-current-buffer bf
                    (and buffer-file-name
                         bf)))
                (buffer-list (selected-frame)))))

(defvar buffer-switch-list-function 'my-buffer-list)

(defun switch-to-previous-buffer-cycle (&optional silent-p)
  ""
  (interactive)
  (let ((bl (funcall buffer-switch-list-function)))
    (when bl
      (bury-buffer (car bl))
      (switch-to-buffer (or (nth 1 bl)
                            (car bl)))
      (or silent-p
          (echo-buffer-list (funcall buffer-switch-list-function))))))

(defun switch-to-next-buffer-cycle (&optional silent-p)
  ""
  (interactive)
  (let* ((bl (funcall buffer-switch-list-function))
         (bf (nth (- (length bl)
                     1)
                  bl)))
    (when bl
      (switch-to-buffer bf)
      (or silent-p
          (echo-buffer-list (funcall buffer-switch-list-function))))))

(iswitchb-mode 1)

(defun iswitchb-buffer-display-other-window ()
  ""
  (interactive)
  (let ((iswitchb-default-method 'display))
    (call-interactively 'iswitchb-buffer)))

(defun switch-to-other-buffer ()
  ""
  (interactive)
  (let ((buffer-switch-list-function 'buffer-list))
    (switch-to-previous-buffer-cycle t)))

(global-set-key (kbd "C-.") 'switch-to-previous-buffer-cycle)
(global-set-key (kbd "C-,") 'switch-to-next-buffer-cycle)
;; (global-set-key (kbd "C-\\") 'switch-to-other-buffer)
;; (global-set-key (kbd "C-\\") 'bury-buffer)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; sdic

(defun sdic-describe-word-at-point-echo ()
  ""
  (interactive)
  (save-window-excursion
    (sdic-describe-word-at-point))
  (save-excursion
    (set-buffer sdic-buffer-name)
    (message (buffer-substring (point-min)
                               (progn (goto-char (point-min))
                                      (or (and (re-search-forward "^\\w" nil t 4)
                                               (progn (previous-line) t)
                                               (point-at-eol))
                                          (point-max)))))))

(setq sdic-eiwa-dictionary-list '((sdicf-client "/usr/share/dict/gene.sdic")))
(setq sdic-waei-dictionary-list '((sdicf-client "/usr/share/dict/jedict.sdic" (add-keys-to-headword t))))
(setq sdic-disable-select-window t)
(setq sdic-window-height 7)
(when (require 'sdic nil t)
  ;; (define-key my-prefix-map "\C-w" 'sdic-describe-word)
  (define-key my-prefix-map "\C-t" 'sdic-describe-word-at-point-echo))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; vc
;; (require 'vc)

(setq vc-handled-backends nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; gauche-mode

(setq scheme-program-name "gosh")

(defun run-gauche-other-window ()
  "Run gauche on other window"
  (interactive)
  (switch-to-buffer-other-window
   (get-buffer-create "*scheme*"))
  (run-gauche))

(defun run-gauche ()
  "run gauche"
  (run-scheme "gosh"))

(defun scheme-send-buffer ()
  ""
  (interactive)
  (scheme-send-region (point-min) (point-max)))

(add-hook 'scheme-mode-hook
          (lambda ()
            (define-key scheme-mode-map "\C-c\C-b" 'scheme-send-buffer)))

;; http://d.hatena.ne.jp/kobapan/20090305/1236261804
;; http://www.katch.ne.jp/~leque/software/repos/gauche-mode/gauche-mode.el

(when (dllib-if-unfound "gauche-mode"
                        "http://www.katch.ne.jp/~leque/software/repos/gauche-mode/gauche-mode.el"
                        t)
  (setq auto-mode-alist
        (cons '("\.scm$" . gauche-mode) auto-mode-alist))
  (autoload 'gauche-mode "gauche-mode" "Major mode for Scheme." t)
  (autoload 'run-scheme "gauche-mode" "Run an inferior Scheme process." t)
  (add-hook 'gauche-mode-hook
            (lambda ()
              (define-key scheme-mode-map "\C-c\C-z" 'run-gauche-other-window))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; recentf-mode

(add-hook 'recentf-dialog-mode-hook
          'my-recentf-abbrev-list)

(defun my-recentf-delete-entry ()
  ""
  (interactive)
  (let ((p (point)))
    (setq recentf-list
          (delete (my-recentf-get-filename) recentf-list))
    (recentf-open-files)
    (goto-char p)))

(defun my-recentf-abbrev-list ()
  ""
  (setq recentf-list
        (mapcar 'abbreviate-file-name
                recentf-list)))

(defun my-recentf-view-file ()
  ""
  (interactive)
  (view-file (my-recentf-get-filename)))

(defun my-recentf-dired ()
  ""
  (interactive)
  (let ((file (my-recentf-get-filename)))
    (if (file-directory-p file)
        (dired file)
      (dired (or (file-name-directory file)
                 ".")))))

(defun my-recentf-x-open ()
  ""
  (interactive)
  (my-x-open (my-recentf-get-filename)))

(defun my-recentf-cd-and-find-file ()
  ""
  (interactive)
  (cd (file-name-directory (my-recentf-get-filename)))
  (call-interactively 'find-file))

(defun my-recentf-get-filename ()
  "get file name in recentf-mode"
  (replace-regexp-in-string "  \\(\\[.+?\\] \\)?" ; "  " or "  [\\d] "
                            ""
                            (buffer-substring-no-properties (point-at-bol)
                                                            (point-at-eol))))

(setq recentf-save-file (expand-file-name "~/.emacs.d/.recentf")
      recentf-max-menu-items 20
      recentf-max-saved-items 30
      recentf-show-file-shortcuts-flag nil)

(when (require 'recentf nil t)
  (global-set-key "\C-x\C-r" 'recentf-open-files)
  ;; (add-hook 'find-file-hook
  ;;           (lambda ()
  ;;             (recentf-add-file default-directory)))
  (recentf-mode 1)
  (add-to-list 'recentf-filename-handlers 'abbreviate-file-name)
  (add-to-list 'recentf-exclude "\\.emacs\\.d/\\.recentf"))

(add-hook 'recentf-dialog-mode-hook
          (lambda ()
            (recentf-save-list)
            ;; (define-key recentf-dialog-mode-map (kbd "C-x C-f") 'my-recentf-cd-and-find-file)
            (define-key recentf-dialog-mode-map (kbd "<up>") 'previous-line)
            (define-key recentf-dialog-mode-map (kbd "<down>") 'next-line)
            (define-key recentf-dialog-mode-map "o" 'my-recentf-x-open)
            (define-key recentf-dialog-mode-map "d" 'my-recentf-delete-entry)
            (define-key recentf-dialog-mode-map "@" 'my-recentf-dired)
            (define-key recentf-dialog-mode-map "v" 'my-recentf-view-file)
            (cd "~/")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; dired

(require 'dired)

(defun my-dired-diff ()
  ""
  (interactive)
  (let ((files (dired-get-marked-files nil nil nil t)))
    (if (eq (car files)
            t)
        (diff (cadr files) (dired-get-filename))
      (message "One files must be marked!"))))

(require 'dired-aux) ;; needed to use dired-dwim-target-directory
(defun my-dired-do-pack-or-unpack ()
  "pack or unpack files.
if targetting one file and that is archive file defined in `pack-program-alist', unpack that.
otherwise, pack marked files. prompt user to decide filename for archive."
  (interactive)
  (let* ((infiles (dired-get-marked-files t))
         (onefile (and (eq 1 ; filename if only one file targeted, otherwise nil.
                           (length infiles))
                       (car infiles))))
    (if (and onefile
             (my-pack-file-name-association onefile))
        (when (y-or-n-p (format "unpack %s? " onefile))
          (my-unpack onefile))
      (let* ((dir-default (dired-dwim-target-directory))
             (archive-default (my-pack-file-extension (file-name-nondirectory (car infiles))))
             (archive ;; (if (interactive-p)
              (read-file-name "Output file to pack : "
                              dir-default
                              nil
                              nil
                              archive-default)
              ;; (concat dir-default archive-default)
              ))
        (apply 'my-pack
               archive
               infiles))))
  (revert-buffer)
  ;; (dired-unmark-all-marks)
  )

(defun my-file-name-extension-with-tar (filename)
  "if FILENAME has extension with tar, like \"tar.gz\", return that.
otherwise, return extension normally."
  (if (string-equal "tar" (file-name-extension (file-name-sans-extension filename)))
      (concat "tar."
              (file-name-extension filename))
    (file-name-extension filename)))

(defun my-pack-file-extension (filename)
  "if FILENAME has extension and it can be used for pack, return FILENAME.
otherwise, return FILENAME with `my-pack-default-extension'"
  (if (my-pack-file-name-association filename)
      filename
    (concat filename "." my-pack-default-extension)))

(defvar my-7z-program-name
  (or (executable-find "7z")
      (executable-find "7za")
      (executable-find "7zr"))
  "7z program.")

(defvar my-pack-default-extension
  "7z"
  "default suffix for packing. filename with this suffix must matches one of `pack-program-alist'")

(defun my-pack-file-name-association (filename)
  "if the pattern matching FILENAME is found at car of the list in `pack-program-alist', return cdr of that list.
otherwise, return nil."
  (let ((case-fold-search nil))
    (assoc-default filename
                   my-pack-program-alist
                   'string-match-p
                   nil)))

(defvar my-pack-program-alist
  `(
    ("\\.7z\\'" ,(concat my-7z-program-name " a") ,(concat my-7z-program-name " x"))
    ("\\.zip\\'" "zip -r" "unzip")
    ("\\.tar\\'" "tar cf" "tar xf")
    ("\\.tgz\\'" "tar czf" "tar xzf")
    ("\\.tar\\.gz\\'" "tar czf" "tar xzf")
    )
  "Alist of filename patterns, command for pack and unpack.
Each element looks like (REGEXP PACKING-COMMAND UNPACKING-COMMAND).
PACKING-COMMAND and UNPACKING-COMMAND can be nil if the command is not available.
alist is searched from the beginning so pattern for \".tar.gz\" should be ahead of pattern for \".gz\"")
;; (string-match-p "\\.gz\\'" "aaa.gz")    ; \' matches string end, $ also matches the point before newline.

(defun my-unpack (archive)
  "unpack ARCHIVE. command for unpacking is defined in `pack-program-alist'"
  (interactive "fArchive to extract: ")
  (let* ((earchive (expand-file-name archive))
         (cmd (nth 1
                   (my-pack-file-name-association earchive)))
         )
    (if cmd
        (shell-command (concat cmd 
                               " "
                               (shell-quote-argument earchive)))
      (message "this is not archive file defined in `pack-program-alist'!"))))

(defun my-pack (archive &rest files)
  "pack FILES into ARCHIVE.
if ARCHIVE have extension defined in `pack-program-alist', use that command.
otherwise, use `pack-default-extension' for pack."
  (let* ((archive-ext (my-pack-file-extension (expand-file-name archive)))
         (cmd (car (my-pack-file-name-association archive-ext)))
         )
    (if cmd
        (shell-command (concat cmd
                               " "
                               (shell-quote-argument archive-ext)
                               " "
                               (mapconcat 'shell-quote-argument
                                          files
                                          " ")))
      (message "invalid extension for packing!"))))

(defun my-pop-to-buffer-erase-noselect (buffer-or-name)
  "pop up buffer using `display-buffer' and return that buffer."
  (let ((bf (get-buffer-create buffer-or-name)))
    (with-current-buffer bf
      (cd ".")
      (erase-buffer))
    (display-buffer bf)
    bf))

(defun my-replace-nasi-none ()
  ""
  (save-excursion
    (let ((buffer-read-only nil))
      (goto-char (point-min))
      (while (search-forward "なし" nil t)
        (replace-match "none")))))

(defun dired-get-du ()                  ;em-unix.el使えるかも
  "dired get disk usage"
  (interactive)
  (message "calculating du...")
  (dired-do-shell-command "du -hs * " nil (dired-get-marked-files)))

(defun my-dired-scroll-up ()
  ""
  (interactive)
  (my-dired-previous-line (- (window-height) 1)))

(defun my-dired-scroll-down ()
  ""
  (interactive)
  (my-dired-next-line (- (window-height) 1)))

(defun my-dired-previous-line (&optional arg)
  ""
  (interactive)
  (dired-previous-line (or arg 1))
  (my-dired-print-current-dir-and-file))

(defun my-dired-next-line (&optional arg)
  ""
  (interactive)
  (dired-next-line (or arg 1))
  (my-dired-print-current-dir-and-file))

(defun my-dired-print-current-dir-and-file ()
  (message "%s  %s"
           default-directory
           (buffer-substring-no-properties (point-at-bol)
                                           (point-at-eol))))

(defun dired-do-execute-as-command ()
  ""
  (interactive)
  (let ((file (dired-get-filename t)))
    (if (file-executable-p file)
        (start-process file nil file)
      (when (y-or-n-p "this file cant be executed. mark as executable and go? : ")
        (set-file-modes file (file-modes-symbolic-to-number "u+x" (file-modes file)))
        (start-process file nil file)))))

;;http://bach.istc.kobe-u.ac.jp/lect/tamlab/ubuntu/emacs.html

(defun my-dired-x-open ()
  ""
  (interactive)
  (my-x-open (dired-get-filename t t)))

(defun my-dired-up-directory ()
  ""
  (interactive)
  (my-dired-find-file ".."))

(defun my-dired-find-file (&optional filename)
  "if the file to open is a directory, kill current buffer after opening that file."
  (interactive)
  (let ((f (expand-file-name (or filename
                                 (dired-get-filename))))
        (bf (current-buffer)))
    (find-file f)
    (when (and (file-directory-p f)
               (not (get-buffer-window bf)))
      (kill-buffer bf))))

(if (eq window-system 'mac)
    (setq dired-listing-switches "-lhFG")
  (setq dired-listing-switches "-lhFG --time-style=long-iso")
  )
(define-minor-mode my-dired-display-all-mode
  ""
  :init-value nil
  :global t
  (my-dired-display-all-set)
  (when (eq major-mode 'dired-mode)
    (revert-buffer)))
(defun my-dired-display-all-set ()
  ""
  (if my-dired-display-all-mode
      (setq dired-actual-switches
            (concat "-A "
                    dired-actual-switches))
    (setq dired-actual-switches
          (replace-regexp-in-string "-A " "" dired-actual-switches))))
(add-hook 'dired-mode-hook
          'my-dired-display-all-set)

(put 'dired-find-alternate-file 'disabled nil)
(require 'ls-lisp)
;; (setq ls-lisp-use-insert-directory-program nil)
(setq ls-lisp-dirs-first t)
(setq dired-ls-F-marks-symlinks t)
(setq dired-dwim-target t)

;; (add-hook 'dired-after-readin-hook
;;           'my-replace-nasi-none)

;; (add-hook 'after-init-hook
;;           (lambda ()
;;             (dired ".")))

(add-hook 'dired-mode-hook
          (lambda ()
            (define-key dired-mode-map "o" 'my-dired-x-open)
            (define-key dired-mode-map "i" 'dired-get-du)
            (define-key dired-mode-map "!" 'shell-command)
            (define-key dired-mode-map "&" 'async-shell-command)
            (define-key dired-mode-map "X" 'dired-do-async-shell-command)
            (define-key dired-mode-map "=" 'my-dired-diff)
            (define-key dired-mode-map "B" 'gtkbm-add-current-dir)
            (define-key dired-mode-map "b" 'gtkbm)
            (define-key dired-mode-map "@" (lambda () (interactive) (my-x-open ".")))
            (define-key dired-mode-map (kbd "TAB") 'other-window)
            (define-key dired-mode-map "P" 'my-dired-do-pack-or-unpack)
            (define-key dired-mode-map "a" 'my-dired-display-all-mode)
            (define-key dired-mode-map "h" 'my-dired-display-all-mode)
            (define-key dired-mode-map "/" 'dired-isearch-filenames)
            ;; (substitute-key-definition 'dired-advertised-find-file 'my-dired-find-file dired-mode-map)
            ;; (substitute-key-definition 'dired-up-directory 'my-dired-up-directory dired-mode-map)
            ;; (define-key dired-mode-map (kbd "DEL") 'my-dired-up-directory)
            (define-key dired-mode-map (kbd "DEL") 'dired-up-directory)
            (substitute-key-definition 'dired-next-line 'my-dired-next-line dired-mode-map)
            (substitute-key-definition 'dired-previous-line 'my-dired-previous-line dired-mode-map)
            (define-key dired-mode-map (kbd "<left>") 'my-dired-scroll-up)
            (define-key dired-mode-map (kbd "<right>") 'my-dired-scroll-down)
            (let ((file "._Icon\015"))
              (when (file-readable-p file)
                (delete-file file)))))

;; http://homepage1.nifty.com/blankspace/emacs/dired.html
;; (add-hook 'dired-load-hook
;;           (lambda ()
;;             (load-library "ls-lisp")
;;             (setq ls-lisp-dirs-first t)
;;             (setq dired-listing-switches "-alhF"))) ;これ書く場所間違えてね？

;; (defadvice dired-next-line (after dired-next-line-print-directory activate)
;;   "print current directory when go down line"
;;   (dired-print-current-dir-and-file))

;; (defadvice dired-previous-line (after dired-previous-line-print-directory activate)
;;   "print current directory when go up line"
;;   (dired-print-current-dir-and-file))

;; http://blog.livedoor.jp/tek_nishi/archives/4693204.html

(defun my-dired-toggle-mark()
  (let ((cur (cond ((eq (following-char) dired-marker-char) ?\040)
                   (t dired-marker-char))))
    (delete-char 1)
    (insert cur)))

(defun my-dired-mark (arg)
  "toggle mark the current (or next ARG) files.
If on a subdir headerline, mark all its files except `.' and `..'.

Use \\[dired-unmark-all-files] to remove all marks
and \\[dired-unmark] on a subdir to remove the marks in
this subdir."

  (interactive "P")
  (if (dired-get-subdir)
      (save-excursion (dired-mark-subdir-files))
    (let ((inhibit-read-only t))
      (dired-repeat-over-lines
       (prefix-numeric-value arg)
       'my-dired-toggle-mark))))

(defun my-dired-mark-backward (arg)
  "In Dired, move up lines and toggle mark there.
Optional prefix ARG says how many lines to unflag; default is one line."
  (interactive "p")
  (my-dired-mark (- arg)))

(defun dired-mode-hooks()
  (local-set-key (kbd "SPC") 'my-dired-mark)
  (local-set-key (kbd "S-SPC") 'my-dired-mark-backward))
(add-hook 'dired-mode-hook 'dired-mode-hooks)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; eshell

(defun my-eshell-backward-delete-char ()
  (interactive)
  (when (< (save-excursion
             (eshell-bol)
             (point))
           (point))
    (backward-delete-char 1)))

(defvar my-eshell-frame-buffer-alist nil)

(defun my-eshell-frame-buffer (frame)
  "get buffer associated with FRAME. if buffer doesnt exist or killed, return nil."
  (let ((bf (cdr (assq frame my-eshell-frame-buffer-alist))))
    (and bf                            ;関連付けられたバッファが存在し
         (buffer-name bf)              ;かつkillされてない
         bf)))

(add-hook 'eshell-mode-hook
          (lambda ()
            (add-to-list 'my-eshell-frame-buffer-alist
                         (cons (selected-frame) (current-buffer)))))

(defun my-file-owner-p (file)
  "t if FILE is owned by me."
  (eq (user-uid) (nth 2 (file-attributes file))))

;; ;; http://www.bookshelf.jp/pukiwiki/pukiwiki.php?Eshell%A4%F2%BB%C8%A4%A4%A4%B3%A4%CA%A4%B9
;; ;; written by Stefan Reichoer <reichoer@web.de>
;; (defun eshell/less (&rest args)
;;   "Invoke `view-file' on the file.
;; \"less +42 foo\" also goes to line 42 in the buffer."
;;   (if args
;;       (while args
;;         (if (string-match "\\`\\+\\([0-9]+\\)\\'" (car args))
;;             (let* ((line (string-to-number (match-string 1 (pop args))))
;;                    (file (pop args)))
;;               (view-file file)
;;               (goto-line line))
;;           (view-file (pop args))))))

;; (defun eshell/git (&rest args)
;;   ""
;;   (eshell-parse-arguments (point-at-bol) (point-at-eol)))

(defun eshell/o (&optional file)
  (my-x-open (or file ".")))

;; (defun eshell/vi (&rest args)
;;   "Invoke `find-file' on the file.
;; \"vi +42 foo\" also goes to line 42 in the buffer."
;;   (while args
;;     (if (string-match "\\`\\+\\([0-9]+\\)\\'" (car args))
;;         (let* ((line (string-to-number (match-string 1 (pop args))))
;;                (file (pop args)))
;;           (find-file file)
;;           (goto-line line))
;;       (find-file (pop args)))))

(defun eshell/clear ()
  "Clear the current buffer, leaving one prompt at the top."
  (let ((inhibit-read-only t))
    (erase-buffer)))

(defun eshell/d (&optional dirname switches)
  "if first arg is omitted open current directory."
  (dired (or dirname ".") switches))

(defun eshell/v ()
  (view-mode 1))

(defalias 'eshell/: 'ignore)
(defalias 'eshell/type 'eshell/which)
;; (defalias 'eshell/vim 'eshell/vi)
(defalias 'eshell/ff 'find-file)
(defalias 'eshell/q 'eshell/exit)

(defun eshell-goto-prompt ()
  ""
  (interactive)
  (goto-char (point-max)))

(defun eshell-cd-default-directory (&optional eshell-buffer-or-name)
  "open eshell and change wd
if arg given, use that eshell buffer, otherwise make new eshell buffer."
  (interactive)
  (let ((dir (expand-file-name default-directory)))
    (switch-to-buffer (or eshell-buffer-or-name
                          (eshell t)))
    (unless (equal dir (expand-file-name default-directory))
      ;; (cd dir)
      ;; (eshell-interactive-print (concat "cd " dir "\n"))
      ;; (eshell-emit-prompt)
      (goto-char (point-max))
      (eshell-kill-input)
      (insert "cd " dir)
      (eshell-send-input))))

(setq eshell-directory-name "~/.emacs.d/eshell/")
(setq eshell-scroll-to-bottom-on-input t)
(setq eshell-cmpl-ignore-case t)
(setq eshell-cmpl-cycle-completions nil)
(setq eshell-highlight-prompt nil)
(setq eshell-ls-initial-args "-FG")     ; "-hF")
(setq eshell-prompt-function
      (lambda ()
        (with-temp-buffer
          (let (p1 p2 p3 p4)
            (insert " [")
            (setq p1 (point))
            (insert (abbreviate-file-name default-directory))
            (setq p2 (point))
            (insert "]"
                    "\n")
            (setq p3 (point))
            (insert user-login-name
                    "@"
                    (or (getenv "HOSTNAME")
                        (substring (shell-command-to-string (or (executable-find "hostname")
                                                                "echo ''"))
                                   0
                                   -1)))
            (setq p4 (point))
            (insert " "
                    (format-time-string "%a, %d %b %Y %T %z")
                    " eshell\n"
                    "last:"
                    (number-to-string eshell-last-command-status)
                    (if (= (user-uid)
                           0)
                        " # "
                      " $ "))
            (add-text-properties p1
                                 p2
                                 '(face ((foreground-color . "yellow"))))
            (add-text-properties p3
                                 p4
                                 '(face ((foreground-color . "cyan"))))
            (buffer-substring (point-min)
                              (point-max))))))

(add-hook 'eshell-mode-hook
          (lambda ()
            ;; (define-key eshell-mode-map (kbd "C-x C-x") (lambda ()
            ;;                                               (interactive)
            ;;                                               (switch-to-buffer (other-buffer))))
            (define-key eshell-mode-map (kbd "C-u") (lambda ()
                                                      (interactive)
                                                      (eshell-goto-prompt)
                                                      (eshell-kill-input)))
            (define-key eshell-mode-map (kbd "C-g") (lambda ()
                                                      (interactive)
                                                      (eshell-goto-prompt)
                                                      (my-keyboard-quit)))
            (define-key eshell-mode-map (kbd "DEL") 'my-eshell-backward-delete-char)
            (define-key eshell-mode-map (kbd "C-p") 'eshell-previous-matching-input-from-input)
            (define-key eshell-mode-map (kbd "C-n") 'eshell-next-matching-input-from-input)
            (mapcar (lambda (alias)
                      (add-to-list 'eshell-command-aliases-list
                                   alias))
                    '(("ll" "ls -l")
                      ("la" "ls -a")
                      ("lla" "ls -al")
                      ("ut" "slogin 03110414@un001.ecc.u-tokyo.ac.jp")
                      ("aptin" "sudo apt-get install")
                      ("u" "uname")
                      ("eless" "cat >>> (with-current-buffer (get-buffer-create \"*eshell output\") (erase-buffer) (setq buffer-read-only nil) (current-buffer)); (view-buffer (get-buffer \"*eshell output*\"))")
                      ("g" "git")))
            (add-to-list 'eshell-output-filter-functions 'eshell-truncate-buffer)
            (apply 'eshell/addpath exec-path)
            (set (make-variable-buffer-local 'scroll-margin) 0)
            (eshell/export "GIT_PAGER=")
            (eshell/export "GIT_EDITOR=")
            (eshell/export "LC_MESSAGES=C")
            (eshell/export "TERM=xterm")
            ))

;; (eval-after-load "em-alias"
;;   '(progn ;; (eshell/alias "ll" "ls -l")
;;      ;; (eshell/alias "la" "ls -a")
;;      ;; (eshell/alias "lla" "ls -al")
;;      (eshell/alias "sgcc" (if (eq system-type 'windows-nt)
;;                               "gcc -o win.$1.exe $1"
;;                             "gcc -o ${uname}.$1.out $1"))
;;      (eshell/alias "slmgcc" (if (eq system-type 'windows-nt)
;;                                 "gcc -lm -o win.$1.exe $1"
;;                               "gcc -lm -o ${uname}.$1.out $1"))
;;      ;; (eshell/alias "ut" "ssh g841105@un001.ecc.u-tokyo.ac.jp")
;;      (add-to-list 'recentf-exclude (concat eshell-directory-name "alias"))))

;; (define-key my-prefix-map (kbd "C-s") (lambda ()
;;                                         (interactive)
;;                                         (eshell-cd-default-directory (buffer-name (or (my-eshell-frame-buffer (selected-frame))
;;                                                                                       (eshell t))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 最終更新日時を得る

(defvar my-buffer-file-last-modified-time nil "")

(make-variable-buffer-local 'my-buffer-file-last-modified-time)

(defun my-get-file-last-modified-time (file)
  ""
  (nth 5
       (file-attributes file)))

(defun my-set-buffer-file-last-modified-time ()
  ""
  (make-local-variable 'my-buffer-file-last-modified-time)
  (setq my-buffer-file-last-modified-time
        (format-time-string "%Y/%m/%d %H:%M" (my-get-file-last-modified-time buffer-file-name))))

(add-hook 'find-file-hook
          'my-set-buffer-file-last-modified-time)
(add-hook 'after-save-hook
          'my-set-buffer-file-last-modified-time)
(add-hook 'after-revert-hook
          'my-set-buffer-file-last-modified-time)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; auto saving

(defun my-save-this-buffer (silent-p)
  "save current buffer if can without asking"
  (let ((cm (if (current-message)
                (format "%s\n" (current-message))
              ""))
        (fun (symbol-function (if silent-p
                                  'ignore
                                'message))))
    (cond ((active-minibuffer-window) nil)
          ((not buffer-file-name) (funcall fun "%ssaving... this buffer doesn't visit any file." cm) nil)
          ((not (file-exists-p buffer-file-name)) (funcall fun "%ssaving... file not exist. save manually first." com) nil)
          (buffer-read-only (funcall fun "%ssaving... this buffer is read-only." cm) nil)
          ((not (buffer-modified-p)) (funcal fun "%ssaving... not modified yet." cm) nil)
          ((not (file-writable-p buffer-file-name)) (funcall fun "%ssaving... you cannot change this file." cm) nil)
          (t (funcall fun "%ssaving..." cm)
             (save-buffer)
             (funcall fun "%ssaving... done." cm)))))
;; (if (and buffer-file-name
;;          (not buffer-read-only)
;;          (buffer-modified-p)
;;          (file-writable-p buffer-file-name))
;;     (save-buffer))) ; 静かな方

(defvar my-auto-save-this-buffer nil "auto save timer object")

(defun my-auto-save-this-buffer (sec &optional silent-p)
  "auto save current buffer if idle for SEC.
when SEC is nil, stop auto save if enabled."
  (if sec
      (progn (when my-auto-save-this-buffer
               (cancel-timer my-auto-save-this-buffer)
               (setq my-auto-save-this-buffer nil))
             (setq my-auto-save-this-buffer (run-with-idle-timer sec t 'my-save-this-buffer silent-p)))
    (when my-auto-save-this-buffer
      (cancel-timer my-auto-save-this-buffer)
      (setq my-auto-save-this-buffer nil))))

(my-auto-save-this-buffer 2 t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; misc funcs

(defvar my-desktop-terminal "roxterm")
(defun my-execute-terminal ()
  ""
  (interactive)
  (if (and (or (eq system-type 'windows-nt)
               window-system)
           my-desktop-terminal
           )
      (let ((process-environment (cons "TERM=xterm" process-environment)))
        (start-process "terminal"
                       nil
                       my-desktop-terminal))
    (my-execute-or-find-term)))

(defun my-term ()
  "open terminal buffer and return that buffer."
  (interactive)
  (if (eq system-type 'windows-nt)
      (eshell t)
    (if (featurep 'multi-term)
        (multi-term)
      (ansi-term "/bin/bash"))))

(defvar my-frame-term-plist nil)
;; (setplist my-frame-term-plist nil)
(defun my-execute-or-find-term ()
  ""
  (interactive)
  (let* ((buf (plist-get my-frame-term-plist (selected-frame))))
    (if (and buf
             (buffer-name buf))
        (switch-to-buffer buf)
      (setq my-frame-term-plist
            (plist-put my-frame-term-plist
                       (selected-frame)
                       (my-term))))))

(defun my-format-time-string (&optional time)
  ""
  (let ((system-time-locale "C"))
    (format-time-string "%a, %d %b %Y %T" time)))

(defvar my-filer nil)
(setq my-filer (or (executable-find "pcmanfm")
                   (executable-find "nautilus")))
(defun my-x-open (file)
  "open file."
  (interactive "FOpen File: ")
  (setq file (expand-file-name file))
  (message "Opening %s..." file)
  (cond ((eq system-type 'windows-nt)
         (call-process "cmd.exe" nil 0 nil "/c" "start" "" (convert-standard-filename file)))
        ((eq system-type 'darwin)
         (call-process "open" nil 0 nil file))
        ((not (getenv "DESKTOP_SESSION"))
         (find-file file))
        (t
         (if (file-directory-p file)
             (call-process my-filer nil 0 nil file)
           (call-process "xdg-open" nil 0 nil file))))
  (recentf-add-file file)
  (message "Opening %s...done" file))

(defvar my-auto-indent-buffer-mode-list
  '(emacs-lisp-mode
    sh-mode
    js-mode
    sgml-mode
    c-mode
    c++-mode))
(setq my-auto-indent-buffer-mode-list nil)
(defun my-indent-buffer ()
  "indent whole buffer."
  (interactive)
  (indent-region (point-min)
                 (point-max)))
(defun my-auto-indent-buffer ()
  ""
  (when (memq major-mode my-auto-indent-buffer-mode-list)
    (my-indent-buffer)))
(add-hook 'before-save-hook
          'my-auto-indent-buffer)

(defun my-keyboard-quit ()
  ""
  (interactive)
  (run-hooks 'before-keyboard-quit-hook)
  ;; (redisplay t)
  (redraw-display)
  ;; (run-hooks 'window-configuration-change-hook)
  (my-revert-buffer-if-needed)
  ;; (revert-buffer t t)
  (keyboard-quit)
  (insert "insert me")
  (run-hooks 'after-keyboard-quit-hook))
(substitute-key-definition 'keyboard-quit 'my-keyboard-quit global-map)
;; (global-set-key (kbd "C-g") 'my-keyboard-quit)

(defun my-convmv-sjis2utf8-test ()
  "run `convmv -r -f sjis -t utf8 *'
this is test, does not rename files"
  (interactive)
  (shell-command "convmv -r -f sjis -t utf8 *"))

(defun my-convmv-sjis2utf8-notest ()
  "run `convmv -r -f sjis -t utf8 * --notest'"
  (interactive)
  (shell-command "convmv -r -f sjis -t utf8 * --notest"))

(defun my-copy-whole-line ()
  ""
  (interactive)
  (kill-new (concat (buffer-substring (point-at-bol)
                                      (point-at-eol))
                    "\n")))

(defun kill-ring-save-buffer-file-name ()
  "get current filename"
  (interactive)
  (let ((file buffer-file-name))
    (if file
        (progn (kill-new file)
               (message file))
      (message "not visiting file."))))

;; ;; コマンド的な
;; (defvar my-execute-func-list nil "func list")
;; (defvar my-execute-func-hist-list nil "func hist list")
;; (setq my-execute-func-list '("(call-interactively 'my-francaiscd-b)"
;;                              "(call-interactively 'my-francaiscd-a)"
;;                              "parsec47"
;;                              "chromium-browser"
;;                              "inkscape"
;;                              "audacious"
;;                              "gnome-terminal"
;;                              "zkaicd.py"
;;                              "glchess"))

;; (defun my-execute-start-process-or-eval-sexp ()
;;   "execute something"
;;   (interactive)
;;   (let ((func (completing-read "command?: " my-execute-func-list nil nil "" my-execute-func-hist-list)))
;;     (if (string= "(" (substring func 0 1))
;;         (with-temp-buffer (insert func)
;;                           (eval-buffer))
;;       (start-process "ps"
;;                      nil
;;                      func))))

;; delete-trailing-whitespace
;; (defun my-delete-blanks-on-eol ()
;;   ""
;;   (interactive)
;;   (save-excursion
;;     (goto-char (point-min))
;;     (while (re-search-forward "[ \t]+$" nil t)
;;       (replace-match "" nil nil))))

(defvar my-revert-buffer-if-needed-last-buffer nil)

(defun my-revert-buffer-if-needed ()
  ""
  (interactive)
  (unless (eq my-revert-buffer-if-needed-last-buffer (current-buffer))
    (setq my-revert-buffer-if-needed-last-buffer (current-buffer))
    (when (or (eq major-mode 'dired-mode)
              (not (verify-visited-file-modtime (current-buffer))))
      (revert-buffer t t))))

(add-hook 'window-configuration-change-hook
          'my-revert-buffer-if-needed)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; forked from http://d.hatena.ne.jp/khiker/20100119/window_resize
(define-key my-prefix-map (kbd "C-w") 'my-window-organizer)

(defun my-window-organizer ()
  "Control window size and position."
  (interactive)
  (save-selected-window
    (select-window (window-at 0 0))
    (let ( ;; (window-obj (selected-window))
          ;; (current-width (window-width))
          ;; (current-height (window-height))
          action
          c)
      (catch 'end-flag
        (while t
          (setq action
                (read-key-sequence-vector (format "size[%dx%d] 1: maximize; 2, 3: split; 0: delete; o: select other; j, l: enlarge; h, k: shrink; q: quit."
                                                  (window-width)
                                                  (window-height))))
          (setq c (aref action 0))
          (cond ((= c ?l)
                 (unless (eq (window-width) (frame-width))
                   (enlarge-window-horizontally 1)))
                ((= c ?h)
                 (unless (eq (window-width) (frame-width))
                   (shrink-window-horizontally 1)))
                ((= c ?j)
                 (enlarge-window 1))
                ((= c ?k)
                 (shrink-window 1))
                ((= c ?o)
                 (other-window 1))
                ((memq c '(?d ?0))
                 (unless (eq (selected-window) (next-window (selected-window) 0 1))
                   (delete-window (selected-window))))
                ((= c ?1)
                 (delete-other-windows))
                ((= c ?2)
                 (split-window-vertically))
                ((= c ?3)
                 (split-window-horizontally))
                ((memq c '(?q ?\C-g))
                 (message "Quit")
                 (throw 'end-flag t))
                (t
                 (beep))))))))
;; (aref (read-key-sequence-vector "aa") 0)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; save and restore frame size
;;http://www.bookshelf.jp/soft/meadow_30.html#SEC416
(defun my-window-size-save ()
  (let* ((rlist (frame-parameters (selected-frame)))
         (ilist initial-frame-alist)
         (nCHeight (frame-height))
         (nCWidth (frame-width))
         (tMargin (if (integerp (cdr (assoc 'top rlist)))
                      (cdr (assoc 'top rlist)) 0))
         (lMargin (if (integerp (cdr (assoc 'left rlist)))
                      (cdr (assoc 'left rlist)) 0))
         buf
         (file "~/.emacs.d/.framesize.el")
         (recentf-exclude '("\\.emacs\\.d/\\.framesize\\.el$")))
    (if (get-file-buffer (expand-file-name file))
        (setq buf (get-file-buffer (expand-file-name file)))
      (setq buf (find-file-noselect file)))
    (set-buffer buf)
    (erase-buffer)
    (insert (concat
             ;; 初期値をいじるよりも modify-frame-parameters
             ;; で変えるだけの方がいい?
             "(delete 'width default-frame-alist)\n"
             "(delete 'height default-frame-alist)\n"
             "(delete 'top default-frame-alist)\n"
             "(delete 'left default-frame-alist)\n"
             "(setq default-frame-alist (append (list\n"
             "'(width . " (int-to-string nCWidth) ")\n"
             "'(height . " (int-to-string nCHeight) ")\n"
             "'(top . " (int-to-string tMargin) ")\n"
             "'(left . " (int-to-string lMargin) "))\n"
             "default-frame-alist))\n"
             ;;"(setq default-frame-alist default-frame-alist)"
             ))
    (save-buffer)
    ))
(defun my-window-size-load ()
  (let* ((file "~/.emacs.d/.framesize.el"))
    (if (file-exists-p file)
        (load file))))
(when window-system
  (my-window-size-load)
  (add-hook 'after-init-hook      ;何かがframeの大きさ勝手に変えやがる
            (lambda ()
              (run-with-timer 1
                              nil
                              (lambda ()
                                (modify-frame-parameters (selected-frame)
                                                         default-frame-alist))))
            t)
  ;; (add-hook 'make-frame-hook
  ;;           (lambda ()
  ;;             (run-with-timer 1
  ;;                             nil
  ;;                             (lambda ()
  ;;                               (modify-frame-parameters (selected-frame)
  ;;                                                        initial-frame-alist))))
  ;;           t)
  (add-hook 'kill-emacs-hook
            'my-window-size-save))

;; windowサイズを固定
;; setq default-frame-alist
;;       (append (list '(width . 80)
;; 		    '(height . 35)
;; 	      )
;; 	      default-frame-alist)
;; ) ;;デフォルトのフレーム設定

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ;; emacsを殺伐とさせる
;; ;; 補完させるとき失敗するからなし
;; ;; http://e-arrows.sakura.ne.jp/2010/05/emacs-should-be-more-savage.html
;; (defadvice message (before message-for-stupid (arg &rest arg2) activate)
;;   (setq arg
;;         (concat arg
;;                 (if (eq nil (string-match "\\. *$" arg)) ".")
;;                 " And You are a Coward!")))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ubuntu

(defun my-load-scim ()
  "use scim-bridge.el as japanese im."
  ;; Load scim-bridge.
  (require 'scim-bridge)
  ;; Turn on scim-mode automatically after loading .emacs
  (add-hook 'after-init-hook 'scim-mode-on)
  (setq scim-cursor-color "red")
  (scim-define-preedit-key ?\^h t)
  (scim-define-common-key ?\* nil)
  (scim-define-common-key ?\^/ nil))

(defun my-load-anthy ()
  "use anthy.el as japanese im."
  ;; anthy
  (require 'anthy)
  (global-set-key [muhenkan] (lambda () (interactive) (anthy-mode-off)))
  (global-set-key [henkan] (lambda () (interactive) (anthy-mode-on)))
  (when (>= emacs-major-version 23)
    (setq anthy-accept-timeout 1)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; windows用設定

;; (add-to-list 'exec-path "c:/Program Files/Gauche/bin/")

(defun start-ckw-bash ()
  ""
  (interactive)
  (start-process "ckw_bash"
                 nil
                 "C:/Documents and Settings/sr/Application Data/dbx/apps/ckw/ckw.exe")) ; cじゃないといけないらしい

(defun my-w32-add-export-path (&rest args)
  ""
  (mapcar (lambda (path)
            (add-to-list 'exec-path (expand-file-name path)))
          (reverse args))
  (setenv "PATH"
          (mapconcat 'convert-standard-filename
                     exec-path
                     ";")))

(when (eq system-type 'windows-nt)
  ;; (setq scheme-program-name "\"c:/Program Files/Gauche/bin/gosh.exe\" -i")
  ;; (setq python-python-command "c:/Python26/python.exe")

  (define-key my-prefix-map (kbd "C-c") 'start-ckw-bash)
  (my-w32-add-export-path "c:/WINDOWS"
                          (expand-file-name "~/bin")
                          (expand-file-name "~/dbx/apps/bin"))

  (when window-system
    (setq w32-enable-synthesized-fonts t))
  (setq file-name-coding-system 'sjis))



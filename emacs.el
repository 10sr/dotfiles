;; (and (file-readable-p "~/.dotfiles/emacs.el")
;;      (load-file "~/.dotfiles/emacs.el"))

;; make directories
(unless (file-directory-p (expand-file-name user-emacs-directory))
  (make-directory (expand-file-name user-emacs-directory)))
(let ((d (expand-file-name (concat user-emacs-directory
                                   "lisp"))))
  (unless (file-directory-p d)
    (make-directory d))
  (add-to-list 'load-path d))

(require 'cl nil t)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; download library from web

(defun fetch-library (url &optional byte-compile-p force-download-p)
  "If library does not exist, download it from URL and locate it in
\"~/emacs.d/lisp/\". Return nil if library unfound and failed to download,
otherwise the path where the library installed."
  (let* ((dir (expand-file-name (concat user-emacs-directory "lisp/")))
         (lib (file-name-sans-extension (file-name-nondirectory url)))
         (lpath (concat dir lib ".el"))
         (locate-p (locate-library lib)))
    (if (or force-download-p (not locate-p))
        (if (progn (message "Downloading %s..."
                            url)
                   (download-file url
                                  lpath
                                  t))
            (progn (message "Downloading %s...done"
                            url)
                   (when (and byte-compile-p
                              (require 'bytecomp nil t))
                     (and (file-exists-p (byte-compile-dest-file lpath))
                          (delete-file (byte-compile-dest-file lpath)))
                     (byte-compile-file lpath)))
          (progn (and (file-writable-p lpath)
                      (delete-file lpath))
                 (message "Downloading %s...failed"
                          url))))
    (locate-library lib)))

(defun download-file (url path &optional ok-if-already-exists)
  "Download file from URL and output to PATH."
  (or
   (let ((curl (executable-find "curl")))
     (when curl
       (if (and (not ok-if-already-exists)
                (file-exists-p path))
           nil
         (and (eq 0
                  (call-process curl
                                nil
                                nil
                                nil
                                "--output"
                                path
                                url
                                ))
              path))))
   (ignore-errors
        (require 'url)
        (url-copy-file url
                       path
                       ok-if-already-exists)
        path)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; autoload

(defmacro lazy-load-eval (feature &optional functions &rest body)
  "Define each FUNCTIONS to autoload from FEATURE.
FEATURE is a symbol. FUNCTIONS is a list of symbols. If FUNCTIONS is nil,
the function same as FEATURE is defined as autoloaded function. BODY is passed
 to `eval-after-load'.
When this macro is evaluated, this returns the path to library if FEATURE
found, otherwise returns nil."
  (let* ((libname (symbol-name (eval feature)))
         (libpath (locate-library libname)))
    (and libpath
         `(progn
            ,@(mapcar (lambda (f)
                        (unless (fboundp f)
                          `(progn
                            (message "Autoloaded function `%S' defined (%s)"
                                     (quote ,f)
                                     ,libpath)
                            (autoload (quote ,f)
                              ,libname
                              ,(concat "Autoloaded function defined in \""
                                       libpath
                                       "\".")
                              t))))
                      (or (eval functions)
                          `(,(eval feature))))
            (eval-after-load ,feature
              (quote (progn
                       ,@body)))
            ,libpath))))
(put 'lazy-load-eval 'lisp-indent-function 2)

(when (lazy-load-eval 'tetris nil
        (message "Tetris loaded!"))
  (message "Tetris found!"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; start and quit

(setq inhibit-startup-message t)
(setq confirm-kill-emacs 'y-or-n-p)
(setq gc-cons-threshold (* 1024 1024 4))

(when window-system
  (add-to-list 'default-frame-alist '(cursor-type . box))
  (add-to-list 'default-frame-alist '(background-color . "white"))
  (add-to-list 'default-frame-alist '(foreground-color . "gray10"))
  ;; (add-to-list 'default-frame-alist '(alpha . (80 100 100 100)))
  ;; does not work?
  )
;; (add-to-list 'default-frame-alist '(cursor-type . box))
(if window-system (menu-bar-mode 1) (menu-bar-mode 0))
(and (fboundp 'tool-bar-mode)
     (tool-bar-mode 0))
(and (fboundp 'set-scroll-bar-mode)
     (set-scroll-bar-mode nil))
(add-hook 'kill-emacs-hook
          ;; load init file when terminating emacs to ensure file is not broken
          'reload-init-file)

(add-hook 'after-init-hook
          (lambda ()
            (message "%s was taken to initialize emacs." (emacs-init-time))
            (switch-to-buffer "*Messages*")
            ))

(cd ".")  ; when using windows use / instead of \ in `default-directory'

;; locale
(set-language-environment "Japanese")
(set-default-coding-systems 'utf-8-unix)
(prefer-coding-system 'utf-8-unix)
(setq system-time-locale "C")

;; my prefix map
(define-prefix-command 'my-prefix-map)
(define-key ctl-x-map (kbd "C-x") 'my-prefix-map)
(define-key my-prefix-map (kbd "C-q") 'quoted-insert)
(define-key my-prefix-map (kbd "C-z") 'suspend-frame)

;; (comint-show-maximum-output)

;; kill scratch
(add-hook 'after-init-hook
          (lambda ()
            (kill-buffer "*scratch*")))

;; modifier keys
;; (setq mac-option-modifier 'control)
(setq w32-apps-modifier 'meta)

;; display
(setq redisplay-dont-pause t)
(setq visible-bell t)
(setq ring-bell-function 'ignore)
(mouse-avoidance-mode 'banish)

(and window-system
     (fetch-library
      "https://raw.github.com/10sr/emacs-lisp/master/save-window-size.el"
      t)
     (require 'save-window-size nil t))

(defun reload-init-file ()
  "Reload emacs init file."
  (interactive)
  (when (file-readable-p user-init-file)
    (load-file user-init-file)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; global keys

(global-set-key (kbd "<up>") (lambda() (interactive) (scroll-down 1)))
(global-set-key (kbd "<down>") (lambda() (interactive) (scroll-up 1)))
(global-set-key (kbd "<left>") 'scroll-down)
(global-set-key (kbd "<right>") 'scroll-up)

;; (define-key my-prefix-map (kbd "C-h") help-map)
(global-set-key (kbd "C-\\") help-map)
(define-key ctl-x-map (kbd "DEL") help-map)
(define-key ctl-x-map (kbd "C-h") help-map)
(define-key help-map "a" 'apropos)

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; title and mode-line

(when (fetch-library
  "https://raw.github.com/10sr/emacs-lisp/master/terminal-title.el"
  t)
 (require 'terminal-title nil t))

(setq eol-mnemonic-dos "\\r\\n")
(setq eol-mnemonic-mac "\\r")
(setq eol-mnemonic-unix "\\n")

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
            (when display-time-mode
              (display-time-update))
            ))
(setq display-time-interval 29)
(setq display-time-day-and-date t)
(setq display-time-format "%a, %d %b %Y %T")
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

;; http://www.masteringemacs.org/articles/2012/09/10/hiding-replacing-modeline-strings/
;; (add-to-list 'minor-mode-alist
;;              '(global-whitespace-mode ""))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; show current info
(defun my-message-current-info ()
  ""
  (interactive)
  (message "%s@%s:%s"
           user-login-name
           system-name
           (abbreviate-file-name default-directory)))

;; (run-with-idle-timer 3
;;                      t
;;                      'my-message-current-info)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; minibuffer

(setq insert-default-directory t)
(setq completion-ignore-case t
      read-file-name-completion-ignore-case t
      read-buffer-completion-ignore-case t)
(setq resize-mini-windows t)
(temp-buffer-resize-mode 1)
(savehist-mode 1)

(fset 'yes-or-no-p 'y-or-n-p)

;; complete symbol when `eval'
(define-key read-expression-map (kbd "TAB") 'lisp-complete-symbol)

(define-key minibuffer-local-map (kbd "C-u")
  (lambda () (interactive) (delete-region (point-at-bol) (point))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; letters, font-lock mode and fonts

;; (set-face-background 'vertical-border (face-foreground 'mode-line))

(and (or (eq system-type 'Darwin)
         (eq system-type 'darwin))
     (fboundp 'mac-set-input-method-parameter)
     (mac-set-input-method-parameter 'japanese 'cursor-color "red")
     (mac-set-input-method-parameter 'roman 'cursor-color "black"))

(when (and (boundp 'input-method-activate-hook) ; i dont know this is correct
           (boundp 'input-method-inactivate-hook))
  (add-hook 'input-method-activate-hook
            (lambda () (set-cursor-color "red")))
  (add-hook 'input-method-inactivate-hook
            (lambda () (set-cursor-color "black"))))

(show-paren-mode 1)
(setq show-paren-delay 0.5
      show-paren-style 'parenthesis)    ; mixed is hard to read
(set-face-background 'show-paren-match
                     (face-foreground 'default))
(set-face-inverse-video-p 'show-paren-match
                          t)

(transient-mark-mode 1)

(global-font-lock-mode 1)
(setq font-lock-global-modes
      '(not
        help-mode
        eshell-mode
        term-mode
        Man-mode))

;; (standard-display-ascii ?\n "$\n")

(defvar my-eol-face
  '(("\n" . (0 font-lock-comment-face t nil)))
  )
(defvar my-tab-face
  '(("\t" . '(0 highlight t nil))))
(defvar my-jspace-face
  '(("\u3000" . '(0 highlight t nil))))

(add-hook 'font-lock-mode-hook
          (lambda ()
            ;; (font-lock-add-keywords nil my-eol-face)
            (font-lock-add-keywords nil my-jspace-face)
            ))

(when (require 'whitespace nil t)
  (setq whitespace-style '(face
                           trailing     ; trailing blanks
                           newline      ; newlines
                           newline-mark ; use display table for newline
                           tab-mark
                           empty        ; empty lines at beg or end of buffer
                           lines-tail  ; lines over 80
                           ))
  ;; (setq whitespace-newline 'font-lock-comment-face)
  (add-to-list 'whitespace-display-mappings
               `(newline-mark ?\n ,(vconcat "$\n")))
  (add-to-list 'whitespace-display-mappings ; not work
               `(tab-mark ?\t ,(vconcat "^I\t")))
  (global-whitespace-mode t)
  (when (eq (display-color-cells)
            256)
    (set-face-foreground 'whitespace-newline "brightblack")))

(and nil
     (fetch-library
      "http://www.emacswiki.org/emacs/download/fill-column-indicator.el"
      t)
     (require 'fill-column-indicator nil t)
     (setq fill-column-indicator))

;; highlight current line
;; http://wiki.riywo.com/index.php?Meadow
(defface my-hl-line
  '((((min-colors 256)
      (background dark))
     (:background "color-234"))
    (((min-colors 256)
      (background light))
     (:background "color-234"))
    (t
     (:underline "black")))
  "*Face used by hl-line.")
(setq hl-line-face 'my-hl-line) ;; (setq hl-line-face nil)
(global-hl-line-mode 1) ;; (hl-line-mode 1)
(setq hl-line-global-modes
      '(not
        term-mode))

(set-face-foreground 'font-lock-regexp-grouping-backslash "#666")
(set-face-foreground 'font-lock-regexp-grouping-construct "#f60")

;; fonts

(defun my-set-ascii-and-jp-font (list)
  "font configuration"
  (let ((fspec1 (if (> emacs-major-version 22)
                   ;; font spec is available in emacs23 and later
                   (font-spec :family (nth 2 list) :size (nth 3 list))
                  (cons (nth 2 list) "jisx0208.*")))
        (fspec2 (if (> emacs-major-version 22)
                    (font-spec :family (nth 2 list) :size (nth 3 list))
                  (cons (nth 2 list) "jisx0201.*"))))
    (set-face-attribute 'default nil
                        :family (nth 0 list)
                        :height (nth 1 list))
    (set-fontset-font "fontset-default"
                      'japanese-jisx0208
                      fspec1)
    (set-fontset-font "fontset-default"
                      'katakana-jisx0201
                      fspec2)))
;; (my-set-ascii-and-jp-font '("dejavu sans mono" 90 "takaogothic" 13))
;; (my-set-ascii-and-jp-font '("dejavu sans mono" 100 "takaogothic" 14))
;; (my-set-ascii-and-jp-font '("dejavu sans mono" 100 "ms gothic" 14))
;; (my-set-ascii-and-jp-font '("monaco" 75 "takaogothic" 11))
;; (my-set-ascii-and-jp-font '("monaco" 90 "takaogothic" 13))
;; (my-set-ascii-and-jp-font '("ProggyCleanTTSZ" 120 "takaogothic" 11))
;; あ a

(and (fetch-library
      "https://raw.github.com/10sr/emacs-lisp/master/set-modeline-color.el"
      t)
     (progn
       (require 'set-modeline-color nil t)))

(let ((fg (face-foreground 'default))
      (bg (face-background 'default)))
  (set-face-background 'mode-line-inactive
                       (if (face-inverse-video-p 'mode-line) fg bg))
  (set-face-foreground 'mode-line-inactive
                       (if (face-inverse-video-p 'mode-line) bg fg)))
(set-face-underline-p 'mode-line-inactive
                      t)
(set-face-underline-p 'vertical-border
                      nil)

(and (fetch-library
      "https://raw.github.com/tarao/elisp/master/end-mark.el"
      t)
     (require 'end-mark nil t)
     (global-end-mark-mode))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; file handling

(setq revert-without-query '(".+"))

;; save cursor position
(setq save-place-file (concat user-emacs-directory
                              "places"))
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

(setq auto-save-list-file-prefix (expand-file-name "~/.emacs.d/auto-save/"))
(setq delete-auto-save-files t)

(add-to-list 'completion-ignored-extensions ".bak")
;; (setq delete-by-moving-to-trash t
;;       trash-directory "~/.emacs.d/trash")

(add-hook 'after-save-hook
          'executable-make-buffer-file-executable-if-script-p)

(setq bookmark-default-file "~/.emacs.d/bmk")
(add-hook 'recentf-load-hook
          (lambda ()
            (add-to-list 'recentf-exclude
                         (regexp-quote bookmark-default-file))))

(and (fetch-library
      "https://github.com/10sr/emacs-lisp/raw/master/read-only-only-mode.el"
      t)
     (lazy-load-eval 'read-only-only-mode))

(and (fetch-library
      "https://raw.github.com/10sr/emacs-lisp/master/smart-revert.el"
      t)
     (require 'smart-revert nil t)
     (smart-revert-on))

;; autosave

(and (fetch-library
      "https://raw.github.com/10sr/emacs-lisp/master/autosave.el"
      t)
     (require 'autosave nil t)
     (autosave-set 2))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; editting

(defun my-copy-whole-line ()
  ""
  (interactive)
  (kill-new (concat (buffer-substring (point-at-bol)
                                      (point-at-eol))
                    "\n")))

(setq require-final-newline t)
(setq kill-whole-line t)
(setq scroll-conservatively 35
      scroll-margin 2
      scroll-step 0)
(setq default-major-mode 'text-mode)
(setq next-line-add-newlines nil)
(setq kill-read-only-ok t)
(setq truncate-partial-width-windows nil) ; when splitted horizontally
;; (setq-default line-spacing 0.2)
(setq-default indicate-empty-lines t)   ; when using x indicate empty line
(setq-default tab-width 4)
(setq-default indent-tabs-mode nil)
(setq-default indent-line-function nil)
;; (pc-selection-mode 1) ; make some already defined keybind back to default
(delete-selection-mode 1)
(cua-mode 0)
(setq line-move-visual nil)

;; key bindings
;; moving around
;; (global-set-key (kbd "M-j") 'next-line)
;; (global-set-key (kbd "M-k") 'previous-line)
;; (global-set-key (kbd "M-h") 'backward-char)
;; (global-set-key (kbd "M-l") 'forward-char)
;;(keyboard-translate ?\M-j ?\C-j)
;; (global-set-key (kbd "M-p") 'backward-paragraph)
(define-key esc-map "p" 'backward-paragraph)
;; (global-set-key (kbd "M-n") 'forward-paragraph)
(define-key esc-map "n" 'forward-paragraph)
(global-set-key (kbd "C-<up>") (lambda () (interactive)(scroll-down 1)))
(global-set-key (kbd "C-<down>") (lambda () (interactive)(scroll-up 1)))
(global-set-key (kbd "C-<left>") 'scroll-down)
(global-set-key (kbd "C-<right>") 'scroll-up)
(global-set-key (kbd "<select>") 'ignore) ; 'previous-line-mark)
(define-key ctl-x-map (kbd "ESC x") 'execute-extended-command)
(define-key ctl-x-map (kbd "ESC :") 'eval-expression)

;; C-h and DEL
(global-set-key (kbd "C-h") (kbd "DEL"))

(global-set-key (kbd "C-m") 'reindent-then-newline-and-indent)
(global-set-key (kbd "C-o") (kbd "C-e C-m"))

(define-key esc-map "k" 'my-copy-whole-line)
;; (global-set-key "\C-z" 'undo) ; undo is M-u
(define-key esc-map "u" 'undo)
(define-key esc-map "i" (kbd "ESC TAB"))
;; (global-set-key (kbd "C-r") 'query-replace-regexp)
(global-set-key (kbd "C-s") 'isearch-forward-regexp)
(global-set-key (kbd "C-r") 'isearch-backward-regexp)

(define-key my-prefix-map (kbd "C-o") 'occur)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; gmail

(setq mail-interactive t
      send-mail-function 'smtpmail-send-it
      ;; message-send-mail-function 'smtpmail-send-it
      smtpmail-smtp-server "smtp.gmail.com"
      smtpmail-smtp-service 587
      smtpmail-starttls-credentials '(("smtp.gmail.com" 587
                                       "8.slashes@gmail.com" nil))
      smtpmail-auth-credentials '(("smtp.gmail.com" 587
                                   "8.slashes@gmail.com" nil))
      user-mail-address "8.slashes@gmail.com")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; buffer killing

(defun my-delete-window-killing-buffer () nil)

(defun my-query-kill-current-buffer ()
  ""
  (interactive)
  (if (y-or-n-p (concat "kill current buffer? :"))
      (kill-buffer (current-buffer))))
(substitute-key-definition 'kill-buffer
                           'my-query-kill-current-buffer
                           global-map)
;;(global-set-key "\C-xk" 'my-query-kill-current-buffer)

(defun my-kill-buffers ()
  ""
  (interactive)
  (mapcar (lambda (buf)
            (when (buffer-file-name buf)
              (kill-buffer buf)))
          (buffer-list)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; share clipboard with x

;; this page describes this in details, but only these sexps seem to be needed
;; http://garin.jp/doc/Linux/xwindow_clipboard

(and (not window-system)
     (not (eq window-system 'mac))
     (getenv "DISPLAY")
     (not (equal (getenv "DISPLAY") ""))
     (executable-find "xclip")
     ;; (< emacs-major-version 24)
     (fetch-library "http://www.emacswiki.org/emacs/download/xclip.el" t)
     (require 'xclip nil t)
     (turn-on-xclip))

(and (eq system-type 'darwin)
     (fetch-library
      "https://raw.github.com/10sr/emacs-lisp/master/pasteboard.el"
      t)
     (require 'pasteboard nil t)
     (turn-on-pasteboard)
     (getenv "TMUX")
     (pasteboard-enable-rtun))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; package

(when (require 'package nil t)
  (add-to-list 'package-archives
               '("melpa" . "http://melpa.milkbox.net/packages/")
               t)
  (add-to-list 'package-archives
               '("marmalade" . "http://marmalade-repo.org/packages/"))
  (add-to-list 'package-archives
               '("ELPA" . "http://tromey.com/elpa/"))
  (package-initialize))

(lazy-load-eval 'sudoku)

;; https://github.com/lunaryorn/flycheck
(when (require 'flycheck nil t)
  (add-hook 'after-init-hook #'global-flycheck-mode))

(defun my-install-packages ()
  nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; window

(and (fetch-library
      "https://raw.github.com/10sr/emacs-lisp/master/window-organizer.el"
      t)
     (lazy-load-eval 'window-organizer)
     (define-key ctl-x-map (kbd "w") 'window-organizer))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; some modes and hooks

;; todo: reg major mode when set explicitly

;; Detect file type from shebang and set major-mode.
(add-to-list 'interpreter-mode-alist
             '("python3" . python-mode))
(add-to-list 'interpreter-mode-alist
             '("python2" . python-mode))

;; http://fukuyama.co/foreign-regexp
'(and (fetch-library
       "https://raw.github.com/k-talo/foreign-regexp.el/master/foreign-regexp.el"
       t)
      (require 'foreign-regexp nil t)
      (progn
        (setq foreign-regexp/regexp-type 'perl)
        '(setq reb-re-syntax 'foreign-regexp)
        ))

(require 'session nil t)

(lazy-load-eval 'sql '(sql-mode)
    (require 'sql-indent nil t))

(and (fetch-library "https://raw.github.com/10sr/emacs-lisp/master/gtkbm.el"
                    t)
     (lazy-load-eval 'gtkbm)
     (global-set-key (kbd "C-x C-d") 'gtkbm))

(and (fetch-library
      "https://raw.github.com/10sr/emacs-lisp/master/git-command.el"
      t)
     (lazy-load-eval 'git-command)
     (define-key ctl-x-map "g" 'git-command))

(and (fetch-library
      "http://www.emacswiki.org/emacs/download/sl.el"
      t)
     (lazy-load-eval 'sl))

(defalias 'qcalc 'quick-calc)

(require 'simple nil t)

(add-hook 'makefile-mode-hook
          (lambda ()
            (define-key makefile-mode-map (kbd "C-m") 'newline-and-indent)
            ;; this functions is set in write-file-functions, i cannot find any
            ;; good way to remove this.
            (fset 'makefile-warn-suspicious-lines 'ignore)
            ))

(add-hook 'verilog-mode-hook
          (lambda ()
            (define-key verilog-mode-map ";" 'self-insert-command)))

(setq diff-switches "-u")
(add-hook 'diff-mode-hook
          (lambda ()
            (view-mode 1)
            (set-face-attribute 'diff-header nil
                                :foreground nil
                                :background nil
                                :weight 'bold)
            (set-face-attribute 'diff-file-header nil
                                :foreground nil
                                :background nil
                                :weight 'bold)
            (set-face-foreground 'diff-index-face "blue")
            (set-face-attribute 'diff-hunk-header nil
                                :foreground "cyan"
                                :weight 'normal)
            (set-face-attribute 'diff-context nil
                                ;; :foreground "white"
                                :foreground nil
                                :weight 'normal)
            (set-face-foreground 'diff-removed-face "red")
            (set-face-foreground 'diff-added-face "green")
            (set-face-attribute 'diff-changed nil
                                :foreground "magenta"
                                :weight 'normal)
            ))

;; (ffap-bindings)

(add-hook 'sh-mode-hook
          (lambda ()
            (define-key sh-mode-map
              (kbd "C-x C-e")
              'my-execute-shell-command-current-line)))
(setq sh-here-document-word "__EOC__")

(defun my-execute-shell-command-current-line ()
  ""
  (interactive)
  (shell-command (buffer-substring-no-properties (point-at-bol)
                                                 (point))))

(setq auto-mode-alist
      `(("autostart\\'" . sh-mode)
        ("xinitrc\\'" . sh-mode)
        ("xprograms\\'" . sh-mode)
        ("PKGBUILD\\'" . sh-mode)
        ,@auto-mode-alist))

(and (lazy-load-eval 'pkgbuild-mode)
     (setq auto-mode-alist (append '(("PKGBUILD\\'" . pkgbuild-mode))
                                   auto-mode-alist)))

(add-hook 'html-mode-hook
          (lambda ()
            (define-key html-mode-map (kbd "C-m")
              'reindent-then-newline-and-indent)))

(add-hook 'text-mode-hook
          (lambda ()
            (define-key text-mode-map (kbd "C-m") 'newline)))

(add-to-list 'Info-default-directory-list
             (expand-file-name "~/.info/emacs-ja"))

(add-hook 'apropos-mode-hook
          (lambda ()
            (define-key apropos-mode-map "n" 'next-line)
            (define-key apropos-mode-map "p" 'previous-line)
            ))

(add-hook 'isearch-mode-hook
          (lambda ()
            ;; (define-key isearch-mode-map
            ;;   (kbd "C-j") 'isearch-other-control-char)
            ;; (define-key isearch-mode-map
            ;;   (kbd "C-k") 'isearch-other-control-char)
            ;; (define-key isearch-mode-map
            ;;   (kbd "C-h") 'isearch-other-control-char)
            (define-key isearch-mode-map (kbd "C-h") 'isearch-delete-char)
            (define-key isearch-mode-map (kbd "M-r")
              'isearch-query-replace-regexp)))

(add-hook 'outline-mode-hook
          (lambda ()
            (if (string-match "\\.md\\'" buffer-file-name)
                (set (make-local-variable 'outline-regexp) "#+ "))))
(add-to-list 'auto-mode-alist (cons "\\.ol\\'" 'outline-mode))

(add-to-list 'auto-mode-alist (cons "\\.md\\'" 'outline-mode))
(when (fetch-library
       "http://jblevins.org/projects/markdown-mode/markdown-mode.el"
       t)
  (lazy-load-eval 'markdown-mode)
  (setq markdown-command (or (executable-find "markdown")
                             (executable-find "markdown.pl")))
  (add-to-list 'auto-mode-alist (cons "\\.md\\'" 'markdown-mode))
  (add-hook 'markdown-mode-hook
            (lambda ()
              (outline-minor-mode 1)
              (set (make-local-variable 'comment-start) ";"))))

;; http://d.hatena.ne.jp/emergent/20070203/1170512717
;; c-mode
;; (setq c-default-style "bsd")
(add-hook 'c-mode-common-hook
          (lambda ()
            (setq c-basic-offset 4
                  indent-tabs-mode nil)
            ;; (set-face-foreground 'font-lock-keyword-face "blue")
            (c-toggle-hungry-state -1)
            (and (require 'gtags nil t)
                 (gtags-mode 1))
            ))

(when (fetch-library
       "https://raw.github.com/mooz/js2-mode/master/js2-mode.el"
       t)
  (lazy-load-eval 'js2-mode)
  (add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
  (add-to-list 'auto-mode-alist '("\\.jsm\\'" . js2-mode))
  ;; (add-hook 'js2-mode-hook
  ;;           (lambda ()
  (add-hook 'js2-mode-hook
            (lambda ()
              (define-key js2-mode-map (kbd "C-m") (lambda ()
                                                     (interactive)
                                                     (js2-enter-key)
                                                     (indent-for-tab-command)))
              (add-hook (kill-local-variable 'before-save-hook)
                        'js2-before-save)
              ;; (add-hook 'before-save-hook
              ;;           'my-indent-buffer
              ;;           nil
              ;;           t)
              )))

(when (require 'uniquify nil t)
  (setq uniquify-buffer-name-style 'post-forward-angle-brackets)
  (setq uniquify-ignore-buffers-re "*[^*]+*")
  (setq uniquify-min-dir-content 1))

(add-hook 'view-mode-hook
          (lambda()
            (define-key view-mode-map "j"
              (lambda() (interactive) (scroll-up 1)))
            (define-key view-mode-map "k"
              (lambda() (interactive) (scroll-down 1)))
            (define-key view-mode-map "v" 'toggle-read-only)
            (define-key view-mode-map "q" 'bury-buffer)
            ;; (define-key view-mode-map "/" 'nonincremental-re-search-forward)
            ;; (define-key view-mode-map "?" 'nonincremental-re-search-backward)
            ;; (define-key view-mode-map
            ;;   "n" 'nonincremental-repeat-search-forward)
            ;; (define-key view-mode-map
            ;;   "N" 'nonincremental-repeat-search-backward)
            (define-key view-mode-map "/" 'isearch-forward-regexp)
            (define-key view-mode-map "?" 'isearch-backward-regexp)
            (define-key view-mode-map "n" 'isearch-repeat-forward)
            (define-key view-mode-map "N" 'isearch-repeat-backward)
            ))
(global-set-key "\M-r" 'view-mode)
(setq view-read-only t)

(add-hook 'Man-mode-hook
          (lambda ()
            (view-mode 1)
            (setq truncate-lines nil)))
(setq Man-notify-method (if window-system
                            'newframe
                          'pushy))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; python

(when (lazy-load-eval 'python '(python-mode))
  (setq python-python-command (or (executable-find "python3")
                                  (executable-find "python")))
  (defun my-python-run-as-command ()
    ""
    (interactive)
    (shell-command (concat python-python-command " " buffer-file-name)))
  (defun my-python-display-python-buffer ()
    ""
    (interactive)
    (set-window-text-height (display-buffer python-buffer
                                            t)
                            7))
  (add-hook 'python-mode-hook
            (lambda ()
              (define-key python-mode-map
                (kbd "C-c C-e") 'my-python-run-as-command)
              (define-key python-mode-map
                (kbd "C-c C-b") 'my-python-display-python-buffer)
              (define-key python-mode-map (kbd "C-m") 'newline-and-indent)))

  (add-hook 'inferior-python-mode-hook
            (lambda ()
              (my-python-display-python-buffer)
              (define-key inferior-python-mode-map
                (kbd "<up>") 'comint-previous-input)
              (define-key inferior-python-mode-map
                (kbd "<down>") 'comint-next-input))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; GNU GLOBAL(gtags)
;; http://uguisu.skr.jp/Windows/gtags.html
;; http://eigyr.dip.jp/gtags.html
;; http://cha.la.coocan.jp/doc/gnu_global.html

(when (lazy-load-eval 'gtags '(gtags-mode))
  (add-hook 'gtags-mode-hook
            (lambda ()
              (setq gtags-select-buffer-single t)
              ;; (local-set-key "\M-t" 'gtags-find-tag)
              ;; (local-set-key "\M-r" 'gtags-find-rtag)
              ;; (local-set-key "\M-s" 'gtags-find-symbol)
              ;; (local-set-key "\C-t" 'gtags-pop-stack)
              (define-key gtags-mode-map (kbd "C-x t h") 'gtags-find-tag-from-here)
              (define-key gtags-mode-map (kbd "C-x t t") 'gtags-find-tag)
              (define-key gtags-mode-map (kbd "C-x t r") 'gtags-find-rtag)
              (define-key gtags-mode-map (kbd "C-x t s") 'gtags-find-symbol)
              (define-key gtags-mode-map (kbd "C-x t p") 'gtags-find-pattern)
              (define-key gtags-mdoe-map (kbd "C-x t f") 'gtags-find-file)
              (define-key gtags-mode-map (kbd "C-x t b") 'gtags-pop-stack) ;back
              )))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; term mode

;; (setq multi-term-program shell-file-name)
(and (fetch-library "http://www.emacswiki.org/emacs/download/multi-term.el"
                    t)
     (lazy-load-eval 'multi-term)
     (progn
       (setq multi-term-switch-after-close nil)
       (setq multi-term-dedicated-select-after-open-p t)
       (setq multi-term-dedicated-window-height 20)))

(when (lazy-load-eval 'term '(term ansi-term))
  (defun my-term-quit-or-send-raw ()
    ""
    (interactive)
    (if (get-buffer-process (current-buffer))
        (call-interactively 'term-send-raw)
      (kill-buffer)))

  ;; http://d.hatena.ne.jp/goinger/20100416/1271399150
  ;; (setq term-ansi-default-program shell-file-name)
  (add-hook 'term-setup-hook
            (lambda ()
              (setq term-display-table (make-display-table))))
  (add-hook 'term-mode-hook
            (lambda ()
              (unless (memq (current-buffer)
                            (and (featurep 'multi-term)
                                 ;; current buffer is not multi-term buffer
                                 (multi-term-list)))
                ;; (define-key term-raw-map "\C-q" 'move-beginning-of-line)
                ;; (define-key term-raw-map "\C-r" 'term-send-raw)
                ;; (define-key term-raw-map "\C-s" 'term-send-raw)
                ;; (define-key term-raw-map "\C-f" 'forward-char)
                ;; (define-key term-raw-map "\C-b" 'backward-char)
                ;; (define-key term-raw-map "\C-t" 'set-mark-command)
                (define-key term-raw-map
                  "\C-x" (lookup-key (current-global-map) "\C-x"))
                (define-key term-raw-map
                  "\C-z" (lookup-key (current-global-map) "\C-z"))
                )
              ;; (define-key term-raw-map "\C-xl" 'term-line-mode)
              ;; (define-key term-mode-map "\C-xc" 'term-char-mode)
              (define-key term-raw-map (kbd "<up>")
                (lambda () (interactive) (scroll-down 1)))
              (define-key term-raw-map (kbd "<down>")
                (lambda () (interactive) (scroll-up 1)))
              (define-key term-raw-map (kbd "<right>") 'scroll-up)
              (define-key term-raw-map (kbd "<left>") 'scroll-down)
              (define-key term-raw-map (kbd "C-p") 'term-send-raw)
              (define-key term-raw-map (kbd "C-n") 'term-send-raw)
              (define-key term-raw-map "q" 'my-term-quit-or-send-raw)
              ;; (define-key term-raw-map (kbd "ESC") 'term-send-raw)
              (define-key term-raw-map [delete] 'term-send-raw)
              (define-key term-raw-map (kbd "DEL") 'term-send-backspace)
              (define-key term-raw-map "\C-y" 'term-paste)
              (define-key term-raw-map
                "\C-c" 'term-send-raw) ;; 'term-interrupt-subjob)
              '(define-key term-mode-map (kbd "C-x C-q") 'term-pager-toggle)
              ;; (dolist (key '("<up>" "<down>" "<right>" "<left>"))
              ;;   (define-key term-raw-map (read-kbd-macro key) 'term-send-raw))
              ;; (define-key term-raw-map "\C-d" 'delete-char)
              (set (make-local-variable 'scroll-margin) 0)
              ;; (set (make-local-variable 'cua-enable-cua-keys) nil)
              ;; (cua-mode 0)
              ;; (and cua-mode
              ;;      (local-unset-key (kbd "C-c")))
              ;; (define-key cua--prefix-override-keymap
              ;;"\C-c" 'term-interrupt-subjob)
              (set (make-local-variable 'hl-line-range-function)
                   (lambda ()
                     '(0 . 0)))
              ))
  ;; (add-hook 'term-exec-hook 'forward-char)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; buffer switching

(when (lazy-load-eval 'bs '(bs-show)
        ;; (add-to-list 'bs-configurations
        ;; '("processes" nil get-buffer-process ".*" nil nil))
        (add-to-list 'bs-configurations
                     '("this-frame" nil (lambda (buf)
                                          (memq buf (my-frame-buffer-get)))
                       ".*" nil nil))
        ;; (setq bs-configurations (list
        ;; '("processes" nil get-buffer-process ".*" nil nil)
        ;; '("files-and-scratch" "^\\*scratch\\*$" nil nil
        ;; bs-visits-non-file bs-sort-buffer-interns-are-last)))
        )
  ;; (global-set-key "\C-x\C-b" 'bs-show)
  (defalias 'list-buffers 'bs-show)
  (setq bs-default-configuration "files")
  (setq bs-default-sort-name "by nothing")
  (add-hook 'bs-mode-hook
            (lambda ()
              (setq bs-default-configuration "files")
              ;; (and bs--show-all
              ;;      (call-interactively 'bs-toggle-show-all))
              (set (make-local-variable 'scroll-margin) 0))))

(iswitchb-mode 1)

(defun iswitchb-buffer-display-other-window ()
  ""
  (interactive)
  (let ((iswitchb-default-method 'display))
    (call-interactively 'iswitchb-buffer)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; sdic

(when (lazy-load-eval 'sdic '(sdic-describe-word-at-point))
  ;; (define-key my-prefix-map "\C-w" 'sdic-describe-word)
  (define-key my-prefix-map "\C-t" 'sdic-describe-word-at-point-echo)
  (defun sdic-describe-word-at-point-echo ()
    ""
    (interactive)
    (save-window-excursion
      (sdic-describe-word-at-point))
    (save-excursion
      (set-buffer sdic-buffer-name)
      (message (buffer-substring (point-min)
                                 (progn (goto-char (point-min))
                                        (or (and (re-search-forward "^\\w"
                                                                    nil
                                                                    t
                                                                    4)
                                                 (progn (previous-line) t)
                                                 (point-at-eol))
                                            (point-max)))))))

  (setq sdic-eiwa-dictionary-list '((sdicf-client "/usr/share/dict/gene.sdic")))
  (setq sdic-waei-dictionary-list
        '((sdicf-client "/usr/share/dict/jedict.sdic" (add-keys-to-headword t))))
  (setq sdic-disable-select-window t)
  (setq sdic-window-height 7))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; vc
;; (require 'vc)

(setq vc-handled-backends '())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; gauche-mode
;; http://d.hatena.ne.jp/kobapan/20090305/1236261804
;; http://www.katch.ne.jp/~leque/software/repos/gauche-mode/gauche-mode.el

(when (and (fetch-library
            "http://www.katch.ne.jp/~leque/software/repos/gauche-mode/gauche-mode.el"
            t)
           (lazy-load-eval 'gauche-mode '(gauche-mode run-scheme)))
  (let ((s (executable-find "gosh")))
    (setq scheme-program-name s
          gauche-program-name s))

  (defun run-gauche-other-window ()
    "Run gauche on other window"
    (interactive)
    (switch-to-buffer-other-window
     (get-buffer-create "*scheme*"))
    (run-gauche))

  (defun run-gauche ()
    "run gauche"
    (run-scheme gauche-program-name)
    )

  (defun scheme-send-buffer ()
    ""
    (interactive)
    (scheme-send-region (point-min) (point-max))
    (my-scheme-display-scheme-buffer)
    )

  (defun my-scheme-display-scheme-buffer ()
    ""
    (interactive)
    (set-window-text-height (display-buffer scheme-buffer
                                            t)
                            7))

  (add-hook 'scheme-mode-hook
            (lambda ()
              nil))

  (add-hook 'inferior-scheme-mode-hook
            (lambda ()
              ;; (my-scheme-display-scheme-buffer)
              ))
  (setq auto-mode-alist
        (cons '("\.gosh\\'" . gauche-mode) auto-mode-alist))
  (setq auto-mode-alist
        (cons '("\.gaucherc\\'" . gauche-mode) auto-mode-alist))
  (add-hook 'gauche-mode-hook
            (lambda ()
              (define-key gauche-mode-map
                (kbd "C-c C-z") 'run-gauche-other-window)
              (define-key scheme-mode-map
                (kbd "C-c C-c") 'scheme-send-buffer)
              (define-key scheme-mode-map
                (kbd "C-c C-b") 'my-scheme-display-scheme-buffer))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; recentf-mode

(setq recentf-save-file (expand-file-name "~/.emacs.d/recentf")
      recentf-max-menu-items 20
      recentf-max-saved-items 30
      recentf-show-file-shortcuts-flag nil)

(when (require 'recentf nil t)
  (add-to-list 'recentf-exclude (regexp-quote recentf-save-file))
  (define-key ctl-x-map (kbd "C-r") 'recentf-open-files)
  (add-hook 'find-file-hook
            'recentf-save-list
            t)   ; save to file immediately after adding file to recentf list
  (add-hook 'kill-emacs-hook
            'recentf-load-list)
  (add-hook 'recentf-mode-hook
            'recentf-save-list)
  ;; (add-hook 'find-file-hook
  ;;           (lambda ()
  ;;             (recentf-add-file default-directory)))
  (and (fetch-library
        "https://raw.github.com/10sr/emacs-lisp/master/recentf-show.el"
        t)
       (lazy-load-eval 'recentf-show)
       (define-key ctl-x-map (kbd "C-r") 'recentf-show)
       (add-hook 'recentf-show-before-listing-hook
                 'recentf-load-list))
  (recentf-mode 1)
  (add-hook 'recentf-dialog-mode-hook
            (lambda ()
              ;; (recentf-save-list)
              ;; (define-key recentf-dialog-mode-map (kbd "C-x C-f")
              ;; 'my-recentf-cd-and-find-file)
              (define-key recentf-dialog-mode-map (kbd "<up>") 'previous-line)
              (define-key recentf-dialog-mode-map (kbd "<down>") 'next-line)
              (define-key recentf-dialog-mode-map "p" 'previous-line)
              (define-key recentf-dialog-mode-map "n" 'next-line)
              (cd "~/"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; dired

(when (lazy-load-eval 'dired nil)
  (defun my-dired-echo-file-head (arg)
    ""
    (interactive "P")
    (let ((f (dired-get-filename)))
      (message "%s"
               (with-temp-buffer
                 (insert-file-contents f)
                 (buffer-substring-no-properties
                  (point-min)
                  (progn (goto-line (if arg
                                        (prefix-numeric-value arg)
                                      10))
                         (point-at-eol)))))))

  (defun my-dired-diff ()
    ""
    (interactive)
    (let ((files (dired-get-marked-files nil nil nil t)))
      (if (eq (car files)
              t)
          (diff (cadr files) (dired-get-filename))
        (message "One files must be marked!"))))

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

  (defun dired-get-file-info ()
    "dired get file info"
    (interactive)
    (let ((f (shell-quote-argument (dired-get-filename t))))
      (if (file-directory-p f)
          (progn
            (message "Calculating disk usage...")
            (shell-command (concat "du -hsD "
                                   f)))
        (shell-command (concat "file "
                               f)))))

  (defun my-dired-scroll-up ()
    ""
    (interactive)
    (my-dired-previous-line (- (window-height) 1)))

  (defun my-dired-scroll-down ()
    ""
    (interactive)
    (my-dired-next-line (- (window-height) 1)))

  ;; (defun my-dired-forward-line (arg)
  ;;   ""
  ;;   (interactive "p"))

  (defun my-dired-previous-line (arg)
    ""
    (interactive "p")
    (if (> arg 0)
        (progn
          (if (eq (line-number-at-pos)
                  1)
              (goto-char (point-max))
            (forward-line -1))
          (my-dired-previous-line (if (or (dired-get-filename nil t)
                                          (dired-get-subdir))
                                      (- arg 1)
                                    arg)))
      (dired-move-to-filename)))

  (defun my-dired-next-line (arg)
    ""
    (interactive "p")
    (if (> arg 0)
        (progn
          (if (eq (point)
                  (point-max))
              (goto-char (point-min))
            (forward-line 1))
          (my-dired-next-line (if (or (dired-get-filename nil t)
                                      (dired-get-subdir))
                                  (- arg 1)
                                arg)))
      (dired-move-to-filename)))

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
        (when (y-or-n-p
               "this file cant be executed. mark as executable and go? : ")
          (set-file-modes file
                          (file-modes-symbolic-to-number "u+x" (file-modes file)))
          (start-process file nil file)))))

  ;;http://bach.istc.kobe-u.ac.jp/lect/tamlab/ubuntu/emacs.html

  (defun my-dired-x-open ()
    ""
    (interactive)
    (my-x-open (dired-get-filename t t)))

  (if (eq window-system 'mac)
      (setq dired-listing-switches "-lhFG")
    (setq dired-listing-switches "-lhFG --time-style=long-iso")
    )
  (setq dired-listing-switches "-lhFG")

  (put 'dired-find-alternate-file 'disabled nil)
  ;; when using dired-find-alternate-file
  ;; reuse current dired buffer for the file to open
  (setq dired-ls-F-marks-symlinks t)

  (require 'ls-lisp)
  (setq ls-lisp-use-insert-directory-program nil) ; always use ls-lisp
  (setq ls-lisp-dirs-first t)
  (setq ls-lisp-use-localized-time-format t)
  (setq ls-lisp-format-time-list
        '("%Y-%m-%d %H:%M"
          "%Y-%m-%d      "))

  (setq dired-dwim-target t)

  ;; (add-hook 'dired-after-readin-hook
  ;;           'my-replace-nasi-none)

  ;; (add-hook 'after-init-hook
  ;;           (lambda ()
  ;;             (dired ".")))

  (add-hook 'dired-mode-hook
            (lambda ()
              (define-key dired-mode-map "o" 'my-dired-x-open)
              (define-key dired-mode-map "i" 'dired-get-file-info)
              (define-key dired-mode-map "f" 'find-file)
              (define-key dired-mode-map "!" 'shell-command)
              (define-key dired-mode-map "&" 'async-shell-command)
              (define-key dired-mode-map "X" 'dired-do-async-shell-command)
              (define-key dired-mode-map "=" 'my-dired-diff)
              (define-key dired-mode-map "B" 'gtkbm-add-current-dir)
              (define-key dired-mode-map "b" 'gtkbm)
              (define-key dired-mode-map "h" 'my-dired-echo-file-head)
              (define-key dired-mode-map "@" (lambda ()
                                               (interactive) (my-x-open ".")))
              (define-key dired-mode-map (kbd "TAB") 'other-window)
              ;; (define-key dired-mode-map "P" 'my-dired-do-pack-or-unpack)
              (define-key dired-mode-map "/" 'dired-isearch-filenames)
              (define-key dired-mode-map (kbd "DEL") 'dired-up-directory)
              (define-key dired-mode-map (kbd "C-h") 'dired-up-directory)
              (substitute-key-definition 'dired-next-line
                                         'my-dired-next-line dired-mode-map)
              (substitute-key-definition 'dired-previous-line
                                         'my-dired-previous-line dired-mode-map)
              (define-key dired-mode-map (kbd "<left>") 'my-dired-scroll-up)
              (define-key dired-mode-map (kbd "<right>") 'my-dired-scroll-down)
              (define-key dired-mode-map (kbd "ESC p") 'my-dired-scroll-up)
              (define-key dired-mode-map (kbd "ESC n") 'my-dired-scroll-down)
              (let ((file "._Icon\015"))
                (when  nil (file-readable-p file)
                       (delete-file file)))))

  (and (fetch-library "https://raw.github.com/10sr/emacs-lisp/master/pack.el"
                      t)
       (lazy-load-eval 'pack '(dired-do-pack-or-unpack pack))
       (add-hook 'dired-mode-hook
                 (lambda ()
                   (define-key dired-mode-map "P" 'dired-do-pack-or-unpack))))

  (and (fetch-library
        "https://raw.github.com/10sr/emacs-lisp/master/dired-list-all-mode.el"
        t)
       (lazy-load-eval 'dired-list-all-mode)
       (setq dired-listing-switches "-lhFG")
       (add-hook 'dired-mode-hook
                 (lambda ()
                   (define-key dired-mode-map "a" 'dired-list-all-mode)
                   )))
  )                                       ; when dired locate

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

(add-hook 'dired-mode-hook
          (lambda ()
            (local-set-key (kbd "SPC") 'my-dired-mark)
            (local-set-key (kbd "S-SPC") 'my-dired-mark-backward))
          )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; eshell

(lazy-load-eval 'eshell nil

  (defun my-eshell-backward-delete-char ()
    (interactive)
    (when (< (save-excursion
               (eshell-bol)
               (point))
             (point))
      (backward-delete-char 1)))

  (defun my-file-owner-p (file)
    "t if FILE is owned by me."
    (eq (user-uid) (nth 2 (file-attributes file))))

  "http://www.bookshelf.jp/pukiwiki/pukiwiki.php\
?Eshell%A4%F2%BB%C8%A4%A4%A4%B3%A4%CA%A4%B9"
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

  (defun eshell/git (&rest args)
    ""
    (if (member (car args)
                '("di" "diff" "log" "show"))
        (apply 'eshell-exec-visual "git" args)
      (shell-command (mapconcat 'shell-quote-argument
                                `("git" ,@args)
                                " ")
                     t)
      ;; (eshell-external-command "git" args)
      ))

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
  (setq eshell-term-name "eterm-color")
  (setq eshell-scroll-to-bottom-on-input t)
  (setq eshell-cmpl-ignore-case t)
  (setq eshell-cmpl-cycle-completions nil)
  (setq eshell-highlight-prompt nil)
  (setq eshell-ls-initial-args '("-hCFG"
                                 "--color=auto"
                                 "--time-style=long-iso"))     ; "-hF")
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
                          (substring (shell-command-to-string
                                      (or (executable-find "hostname")
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
              ;;                             (switch-to-buffer (other-buffer))))
              (define-key eshell-mode-map (kbd "C-u") (lambda ()
                                                        (interactive)
                                                        (eshell-goto-prompt)
                                                        (eshell-kill-input)))
              (define-key eshell-mode-map (kbd "C-g") (lambda ()
                                                        (interactive)
                                                        (eshell-goto-prompt)
                                                        (my-keyboard-quit)))
              (define-key eshell-mode-map
                (kbd "DEL") 'my-eshell-backward-delete-char)
              (define-key eshell-mode-map
                (kbd "C-p") 'eshell-previous-matching-input-from-input)
              (define-key eshell-mode-map
                (kbd "C-n") 'eshell-next-matching-input-from-input)
              (apply 'eshell/addpath exec-path)
              (set (make-local-variable 'scroll-margin) 0)
              ;; (eshell/export "GIT_PAGER=")
              ;; (eshell/export "GIT_EDITOR=")
              (eshell/export "LC_MESSAGES=C")
              (switch-to-buffer (current-buffer)) ; move buffer top of list
              (set (make-local-variable 'hl-line-range-function)
                   (lambda ()
                     '(0 . 0)))
              (add-to-list 'eshell-virtual-targets
                           '("/dev/less"
                             (lambda (str)
                               (if str
                                   (with-current-buffer nil)))
                             nil))
              ))

  (add-hook 'eshell-mode-hook
            (lambda ()
              (add-to-list 'eshell-visual-commands "vim")
              ;; (add-to-list 'eshell-visual-commands "git")
              (add-to-list 'eshell-output-filter-functions
                           'eshell-truncate-buffer)
              (mapcar (lambda (alias)
                        (add-to-list 'eshell-command-aliases-list
                                     alias))
                      '(
                                        ; ("ll" "ls -l $*")
                                        ; ("la" "ls -a $*")
                                        ; ("lla" "ls -al $*")
                        ("aptin" "apt-get install $*")
                        ("eless"
                         (concat "cat >>> (with-current-buffer "
                                 "(get-buffer-create \"*eshell output\") "
                                 "(erase-buffer) "
                                 "(setq buffer-read-only nil) "
                                 "(current-buffer)) "
                                 "(view-buffer (get-buffer \"*eshell output*\"))")
                         ("g" "git $*")
                         ))
                      )))
  )                          ; eval after load eshell

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; frame buffer
;; todo: work well when opening files already opened on another window

(add-hook 'after-make-frame-functions
          (lambda (f)
            (set-window-buffer (frame-selected-window f)
                               "*Messages*")))

(defun make-frame-command-with-name (name)
  "Make frame with name specified."
  (interactive "sName for new frame: ")
  (set-frame-parameter (make-frame-command)
                       'name
                       name))

(defvar my-frame-buffer-plist nil)

(defun my-frame-buffer-add (&optional buf frame)
  ""
  (setq my-frame-buffer-plist
        (plist-put my-frame-buffer-plist
                   (or frame
                       (selected-frame))
                   (let ((lst (my-frame-buffer-get frame)))
                     (if lst
                         (add-to-list 'lst
                                      (or buf
                                          (current-buffer)))
                       (list (or buf
                                 (current-buffer))))))))

(defun my-frame-buffer-remove (&optional buf frame)
  ""
  (setq my-frame-buffer-plist
        (plist-put my-frame-buffer-plist
                   (or frame
                       (selected-frame))
                   (delq (or buf
                             (current-buffer))
                         (my-frame-buffer-get frame)))))

(defun my-frame-buffer-get (&optional frame)
  ""
  (plist-get my-frame-buffer-plist
             (or frame
                 (selected-frame))))

(defun my-frame-buffer-kill-all-buffer (&optional frame)
  ""
  (mapcar 'kill-buffer
          (my-frame-buffer-get frame)))

(add-hook 'find-file-hook
          'my-frame-buffer-add)
;; (add-hook 'term-mode-hook
;;           'my-frame-buffer-add)
(add-hook 'eshell-mode-hook
          'my-frame-buffer-add)
(add-hook 'Man-mode-hook
          'my-frame-buffer-add)

(add-hook 'kill-buffer-hook
          'my-frame-buffer-remove)
(add-hook 'delete-frame-functions
          'my-frame-buffer-kill-all-buffer)


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
    (my-term)))

(defvar my-term nil "my terminal buffer")
(defun my-term ()
  "open terminal buffer and return that buffer."
  (interactive)
  (if (and my-term
           (buffer-name my-term))
      (pop-to-buffer my-term)
    (setq my-term
          (save-window-excursion
            (funcall my-term-function)
            ))
    (and my-term
         (my-term))))

(defvar my-term-function nil
  "Function to create terminal buffer.")

;; (setq my-term-function
;;       (lambda ()
;;         (if (eq system-type 'windows-nt)
;;             (eshell)
;;           (if (require 'multi-term nil t)
;;               (multi-term)
;;             (ansi-term shell-file-name)))))

(setq my-term-function 'eshell)

(defun my-delete-frame-or-kill-emacs ()
  "delete frame when opening multiple frame, kill emacs when only one."
  (interactive)
  (if (eq 1
          (length (frame-list)))
      (save-buffers-kill-emacs)
    (delete-frame)))

(define-key my-prefix-map (kbd "C-s") 'my-execute-terminal)
(define-key my-prefix-map (kbd "C-f") 'make-frame-command-with-name)
(global-set-key (kbd "C-x C-c") 'my-delete-frame-or-kill-emacs)
(define-key my-prefix-map (kbd "C-x C-c") 'save-buffers-kill-emacs)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; x open

(defvar my-filer nil)
(setq my-filer (or (executable-find "pcmanfm")
                   (executable-find "nautilus")))
(defun my-x-open (file)
  "open file."
  (interactive "FOpen File: ")
  (setq file (expand-file-name file))
  (message "Opening %s..." file)
  (cond ((eq system-type 'windows-nt)
         (call-process "cmd.exe" nil 0 nil
                       "/c" "start" "" (convert-standard-filename file)))
        ((eq system-type 'darwin)
         (call-process "open" nil 0 nil file))
        ((getenv "DISPLAY")
         (call-process (or my-filer "xdg-open") nil 0 nil file))
        (t
         (find-file file))
        )
  ;; (recentf-add-file file)
  (message "Opening %s...done" file))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; misc funcs

(defun my-grep ()
  "Use git-grep if avaliable.."
  (interactive)
  (require 'grep)
  (let ((--grep-command-bak grep-command)
        (--grep-use-null-device-bak grep-use-null-device))
    (if (called-interactively-p 'any)
        (progn
          (when (eq 0
                    (shell-command "git rev-parse --git-dir"))
            (grep-apply-setting 'grep-command
                                "git --no-pager grep -nH -e ")
            (grep-apply-setting 'grep-use-null-device
                                nil))
          (call-interactively 'grep)
          (grep-apply-setting 'grep-command
                              --grep-command-bak)
          (grep-apply-setting 'grep-use-null-device
                              --grep-use-null-device-bak))
      (message "my-grep only allow interactive call."))))

(defun make ()
  "Run \"make -k\" in current directory."
  (interactive)
  (compile "make -k"))

(defvar sed-in-place-history nil
  "History of `sed-in-place'")

(defvar sed-in-place-command "sed --in-place=.bak -e")
(defun sed-in-place (command)
  "sed in place"
  (interactive (list (read-shell-command "sed in place: "
                                         (concat sed-in-place-command " ")
                                         'sed-in-place-history)))
  (shell-command command
                 "*sed in place*"))
(defun dired-do-sed-in-place (&optional arg)
  "sed in place dired"
  (interactive "P")
  (require 'dired-aux)
  (let* ((files (dired-get-marked-files t arg))
         (expr (dired-mark-read-string "Run sed-in-place for %s: "
                                       nil
                                       'sed-in-place
                                       arg
                                       files)))
    (if (equal expr
               "")
        (error "No expression specified")
      (shell-command (concat sed-in-place-command
                             " '"
                             expr
                             "' "
                             (mapconcat 'shell-quote-argument
                                        files
                                        " "))
                     "*sed in place*"))))

(defun dir-show (&optional dir)
  (interactive)
  (let ((bf (get-buffer-create "*dir show*"))
        (list-directory-brief-switches "-C"))
    (with-current-buffer bf
      (list-directory (or nil
                          default-directory)
                      nil))
    ))

(defun my-convmv-sjis2utf8-test ()
  "run `convmv -r -f sjis -t utf8 *'
this is test, does not rename files"
  (interactive)
  (shell-command "convmv -r -f sjis -t utf8 *"))

(defun my-convmv-sjis2utf8-notest ()
  "run `convmv -r -f sjis -t utf8 * --notest'"
  (interactive)
  (shell-command "convmv -r -f sjis -t utf8 * --notest"))

(defun kill-ring-save-buffer-file-name ()
  "get current filename"
  (interactive)
  (let ((file buffer-file-name))
    (if file
        (progn (kill-new file)
               (message file))
      (message "not visiting file."))))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ;; savage emacs
;; ;; when enabled emacs fails to complete
;; ;; http://e-arrows.sakura.ne.jp/2010/05/emacs-should-be-more-savage.html
;; (defadvice message (before message-for-stupid (arg &rest arg2) activate)
;;   (setq arg
;;         (concat arg
;;                 (if (eq nil (string-match "\\. *$" arg)) ".")
;;                 " Stupid!")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; japanese input method

(defun my-load-scim ()
  "use scim-bridge.el as japanese im."
  ;; Load scim-bridge.
  (when (require 'scim-bridge nil t)
    ;; Turn on scim-mode automatically after loading .emacs
    (add-hook 'after-init-hook 'scim-mode-on)
    (setq scim-cursor-color "red")
    (scim-define-preedit-key ?\^h t)
    (scim-define-common-key ?\* nil)
    (scim-define-common-key ?\^/ nil)))

(defun my-load-anthy ()
  "use anthy.el as japanese im."
  ;; anthy
  (when (require 'anthy nil t)
    (global-set-key
     (kbd "<muhenkan>") (lambda () (interactive) (anthy-mode-off)))
    (global-set-key (kbd "<henkan>") (lambda () (interactive) (anthy-mode-on)))
    (when (>= emacs-major-version 23)
      (setq anthy-accept-timeout 1))))

;; quail
;; aproposs input-method for some information
;; (setq default-input-method "japanese")
(defun my-load-mozc-el ()
  ""
  (setq mozc-leim-title "[MZ]")
  (when (require 'mozc nil t)
    (setq defauit-input-method "japanese-mozc")
    ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; for windows

(defun start-ckw-bash ()
  ""
  (interactive)
  (start-process
   "ckw_bash"
   nil
   "C:/Documents and Settings/sr/Application Data/dbx/apps/ckw/ckw.exe"))
;; command seems to have to be in c drive

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
  (my-w32-add-export-path "c:/Windows/system"
                          "c:/Windows/System32"
                          "c:/Program Files/Git/bin"
                          "c:/MinGW/bin"
                          "c:/MinGW/mingw32/bin"
                          (expand-file-name "~/.local/bin")
                          (expand-file-name "~/dbx/apps/bin"))

  (when window-system
    (setq w32-enable-synthesized-fonts t))
  (setq file-name-coding-system 'sjis))

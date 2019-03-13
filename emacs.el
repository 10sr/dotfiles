;;; emacs.el --- 10sr emacs initialization

;;; Code:

;; SETUP_LOAD: (let ((file "DOTFILES_DIR/emacs.el"))
;; SETUP_LOAD:   (and (file-readable-p file)
;; SETUP_LOAD:        (byte-recompile-file file nil 0 t)))

(setq debug-on-error t)

;; make directories
(unless (file-directory-p (expand-file-name user-emacs-directory))
  (make-directory (expand-file-name user-emacs-directory)))

(require 'cl-lib)
(require 'simple)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Some macros for internals

(defun call-after-init (func)
  "If `after-init-hook' has been run, call FUNC immediately.
Otherwize hook it."
  (if after-init-time
      (funcall func)
    (add-hook 'after-init-hook
              func)))

(defmacro safe-require-or-eval (feature)
  "Require FEATURE if available.

At compile time the feature will be loaded immediately."
  `(eval-and-compile
     (message "safe-require-or-eval: Trying to require %s" ,feature)
     (require ,feature nil t)))

(defmacro autoload-eval-lazily (feature &optional functions &rest body)
  "Define autoloading FEATURE that defines FUNCTIONS.
FEATURE is a symbol.  FUNCTIONS is a list of symbols.  If FUNCTIONS is nil,
the function same as FEATURE is defined as autoloaded function.  BODY is passed
 to `eval-after-load'.
After this macro is expanded, this returns the path to library if FEATURE
found, otherwise returns nil."
  (declare (indent 2) (debug t))
  (let* ((libname (symbol-name (eval feature)))
         (libpath (locate-library libname)))
    `(progn
       (when (locate-library ,libname)
         ,@(mapcar (lambda (f)
                     `(unless (fboundp ',f)
                        (progn
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
                       `(,(eval feature)))))
       (eval-after-load ,feature
         (quote (progn
                  ,@body)))
       (locate-library ,libname))))

(when (autoload-eval-lazily 'tetris nil
        (message "Tetris loaded!"))
  (message "Tetris found!"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; package

(set (defvar 10sr-package-list)
     '(
       vimrc-mode
       markdown-mode
       yaml-mode
       gnuplot-mode
       php-mode
       erlang
       js2-mode
       js-doc
       git-commit
       gitignore-mode
       adoc-mode
       go-mode
       ;; It seems malabar has been merged into jdee and this package
       ;; already removed
       ;; malabar-mode
       gosh-mode
       scala-mode
       ;;ensime


       ;; ack
       color-moccur
       ggtags
       flycheck
       auto-highlight-symbol
       hl-todo
       ;; Currently not available
       ;; pp-c-l
       xclip
       foreign-regexp
       multi-term
       term-run
       editorconfig
       git-ps1-mode
       restart-emacs
       fill-column-indicator
       pkgbuild-mode
       minibuffer-line
       which-key
       ;; I think this works in place of my autosave lib
       super-save
       pipenv
       imenu-list
       page-break-lines
       ;; sync-recentf
       aggressive-indent
       ;; fancy-narrow
       dired-filter
       wgrep
       magit
       git-gutter
       end-mark
       sl
       ;; TODO: Configure pony-tpl-mode
       pony-mode
       gited
       highlight-indentation
       diminish

       editorconfig
       editorconfig-custom-majormode

       git-command

       prompt-text

       ;; 10sr repository
       ;; 10sr-extras
       terminal-title
       recentf-show
       dired-list-all-mode
       pack
       set-modeline-color
       read-only-only-mode
       smart-revert
       autosave
       ;;window-organizer
       ilookup
       pasteboard
       awk-preview
       recently

       ))

(when (safe-require-or-eval 'package)
  (setq package-archives
        `(,@package-archives
          ("melpa" . "https://melpa.org/packages/")
          ;; Somehow fails to download via https
          ("10sr-el" . "http://10sr.github.io/emacs-lisp/elpa/")))
  (package-initialize)

  (defun my-auto-install-package ()
    "Install packages semi-automatically."
    (interactive)
    (package-refresh-contents)
    (mapc (lambda (pkg)
            (or (package-installed-p pkg)
                (package-install pkg)))
          10sr-package-list))
  )

;; (lazy-load-eval 'sudoku)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; my-idle-hook

(defvar my-idle-hook nil
  "Hook run when idle for several secs.")
(defvar my-idle-hook-sec 5
  "Second to run `my-idle-hook'.")
(run-with-idle-timer my-idle-hook-sec
                     t
                     (lambda ()
                       (run-hooks 'my-idle-hook)))

;; (add-hook 'my-idle-hook
;;           (lambda ()
;;             (message "idle hook message")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; start and quit

(setq inhibit-startup-message t)
(setq confirm-kill-emacs 'y-or-n-p)
(setq gc-cons-threshold (* 1024 1024 16))
(setq garbage-collection-messages nil)

(when window-system
  (add-to-list 'default-frame-alist '(cursor-type . box))
  (add-to-list 'default-frame-alist '(background-color . "white"))
  (add-to-list 'default-frame-alist '(foreground-color . "gray10"))
  ;; (add-to-list 'default-frame-alist '(alpha . (80 100 100 100)))
  ;; does not work?
  )
;; (add-to-list 'default-frame-alist '(cursor-type . box))
(menu-bar-mode 1)
(define-key ctl-x-map "m" 'menu-bar-open)
(and (fboundp 'tool-bar-mode)
     (tool-bar-mode 0))
(and (fboundp 'set-scroll-bar-mode)
     (set-scroll-bar-mode nil))

(call-after-init
 (lambda ()
   (message "%s %s" invocation-name emacs-version)
   (message "Invocation directory: %s" default-directory)
   (message "%s was taken to initialize emacs." (emacs-init-time))
   (view-echo-area-messages)
   ;; (view-emacs-news)
   ))

(cd ".")  ; when using windows use / instead of \ in `default-directory'

;; locale
(set-language-environment "Japanese")
(set-default-coding-systems 'utf-8-unix)
(prefer-coding-system 'utf-8-unix)
(setq system-time-locale "C")

;; my prefix map
(defvar my-prefix-map nil
  "My prefix map.")
(define-prefix-command 'my-prefix-map)
;; (define-key ctl-x-map (kbd "C-x") 'my-prefix-map)
;; (define-key my-prefix-map (kbd "C-q") 'quoted-insert)
;; (define-key my-prefix-map (kbd "C-z") 'suspend-frame)

;; (comint-show-maximum-output)

;; kill scratch
(call-after-init (lambda ()
                   (let ((buf (get-buffer "*scratch*")))
                     (when buf
                       (kill-buffer buf)))))

;; modifier keys
;; (setq mac-option-modifier 'control)

;; display
(setq visible-bell t)
(setq ring-bell-function 'ignore)
(mouse-avoidance-mode 'banish)
(setq echo-keystrokes 0.1)

(defun reload-init-file ()
  "Reload Emacs init file."
  (interactive)
  (when (and user-init-file
             (file-readable-p user-init-file))
    (load-file user-init-file)))

(safe-require-or-eval 'session)

;; server

(set-variable 'server-name (concat "server"
                                   (number-to-string (emacs-pid))))

;; In Cygwin Environment `server-runnning-p' stops when server-use-tcp is nil
;; In Darwin environment, init fails with message like 'Service name too long'
;; when server-use-tcp is nil
(when (or (eq system-type
              'cygwin)
          (eq system-type
              'darwin))
  (set-variable 'server-use-tcp t))

;; MSYS2 fix

(when (eq system-type
          'windows-nt)
  (setq shell-file-name
        (executable-find "bash"))
  '(setq function-key-map
         `(,@function-key-map ([pause] . [?\C-c])
                              ))
  (define-key key-translation-map
    (kbd "<pause>")
    (kbd "C-c"))
  '(keyboard-translate [pause]
                       (kbd "C-c")p)
  ;; TODO: move to other place later
  (when (not window-system)
    (setq interprogram-paste-function nil)
    (setq interprogram-cut-function nil)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; global keys

(global-set-key (kbd "<up>") 'scroll-down-line)
(global-set-key (kbd "<down>") 'scroll-up-line)
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

(when (safe-require-or-eval 'which-key)
  (which-key-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; editor

(setq kill-whole-line t)
(setq scroll-conservatively 35
      scroll-margin 2)
(setq-default major-mode 'text-mode)
(setq next-line-add-newlines nil)
(setq kill-read-only-ok t)
(setq truncate-partial-width-windows nil) ; when splitted horizontally
;; (setq-default line-spacing 0.2)
(setq-default indicate-empty-lines t)   ; when using x indicate empty line
;; (setq-default tab-width 4)
(setq-default indent-tabs-mode nil)
(setq-default indent-line-function 'indent-to-left-margin)
;; (setq-default indent-line-function nil)
(setq-default truncate-lines nil)
;; (pc-selection-mode 1) ; make some already defined keybind back to default
(delete-selection-mode 1)
(cua-mode 0)
(setq line-move-visual nil)
(setq create-lockfiles nil)

(add-hook 'before-save-hook
          'time-stamp)
;; Add Time-stamp: <> to insert timestamp there
(set-variable 'time-stamp-format
              "%:y-%02m-%02d %02H:%02M:%02S %Z 10sr")

;; key bindings
;; moving around
;;(keyboard-translate ?\M-j ?\C-j)
;; (global-set-key (kbd "M-p") 'backward-paragraph)
(define-key esc-map "p" 'backward-paragraph)
;; (global-set-key (kbd "M-n") 'forward-paragraph)
(define-key esc-map "n" 'forward-paragraph)
(global-set-key (kbd "C-<up>") 'scroll-down-line)
(global-set-key (kbd "C-<down>") 'scroll-up-line)
(global-set-key (kbd "C-<left>") 'scroll-down)
(global-set-key (kbd "C-<right>") 'scroll-up)
(global-set-key (kbd "<select>") 'ignore) ; 'previous-line-mark)
(define-key ctl-x-map (kbd "ESC x") 'execute-extended-command)
(define-key ctl-x-map (kbd "ESC :") 'eval-expression)

;; C-h and DEL
(global-set-key (kbd "C-h") (kbd "DEL"))

;;(global-set-key (kbd "C-m") 'reindent-then-newline-and-indent)
(global-set-key (kbd "C-m") 'newline-and-indent)
;; (global-set-key (kbd "C-o") (kbd "C-e C-m"))

;; (global-set-key "\C-z" 'undo) ; undo is M-u
(define-key esc-map "u" 'undo)
(define-key esc-map "i" (kbd "ESC TAB"))
;; (global-set-key (kbd "C-r") 'query-replace-regexp)
(global-set-key (kbd "C-s") 'isearch-forward-regexp)
(global-set-key (kbd "C-r") 'isearch-backward-regexp)

(require 'page-ext nil t)

(when (safe-require-or-eval 'page-break-lines)
  (global-page-break-lines-mode 1))

(when (safe-require-or-eval 'git-gutter)
  (custom-set-variables
   '(git-gutter:lighter " Gttr"))
  (custom-set-variables
   '(git-gutter:update-interval 2))
  (custom-set-variables
   '(git-gutter:unchanged-sign " "))
  (when (>= (display-color-cells)
            256)
    (let ((c "color-233"))
      (set-face-background 'git-gutter:modified c)
      (set-face-background 'git-gutter:added c)
      (set-face-background 'git-gutter:deleted c)
      (set-face-background 'git-gutter:unchanged c)))
  (global-git-gutter-mode 1)
  )

;; (when (safe-require-or-eval 'fancy-narrow)
;;   (fancy-narrow-mode 1))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; title and mode-line

(when (safe-require-or-eval 'terminal-title)
  ;; if TERM is not screen use default value
  (if (getenv "TMUX")
      ;; if use tmux locally just basename of current dir
      (set-variable 'terminal-title-format
                    '((file-name-nondirectory (directory-file-name
                                               default-directory))))
    (if (and (let ((tty-type (frame-parameter nil
                                              'tty-type)))
               (and tty-type
                    (equal (car (split-string tty-type
                                              "-"))
                           "screen")))
             (not (getenv "SSH_CONNECTION")))
        (set-variable 'terminal-title-format
                      '((file-name-nondirectory (directory-file-name
                                                 default-directory))))
      ;; seems that TMUX is used locally and ssh to remote host
      (set-variable 'terminal-title-format
                    `("em:"
                      ,user-login-name
                      "@"
                      ,(car (split-string (system-name)
                                          "\\."))
                      ":"
                      default-directory))
      )
    )
  (terminal-title-mode))

(setq eol-mnemonic-dos "\\r\\n")
(setq eol-mnemonic-mac "\\r")
(setq eol-mnemonic-unix "")

(which-function-mode 1)

(line-number-mode 0)
(column-number-mode 0)
(size-indication-mode 0)
(setq mode-line-position
      '(:eval (format "L%%l/%d%s:C%%c"
                      (count-lines (point-max)
                                   (point-min))
                      (if (buffer-narrowed-p)
                          "[N]"
                        "")
                      )))

(when (safe-require-or-eval 'diminish)
  ;; FIXME: Eval after enabling mode
  (diminish 'recently-mode)
  (diminish 'editorconfig-mode)
  (diminish 'auto-highlight-symbol-mode)
  (diminish 'global-whitespace-mode)
  (diminish 'which-key-mode)
  (diminish 'page-break-lines-mode)
  (diminish 'highlight-indentation-mode))

;; http://www.geocities.jp/simizu_daisuke/bunkei-meadow.html#frame-title

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; minibuffer

(setq insert-default-directory t)
(setq completion-ignore-case t
      read-file-name-completion-ignore-case t
      read-buffer-completion-ignore-case t)
(setq resize-mini-windows t)
(temp-buffer-resize-mode 1)
(savehist-mode 1)
(defvar display-time-format "%Y/%m/%d %a %H:%M")

(set-variable 'help-at-pt-display-when-idle t)

(fset 'yes-or-no-p 'y-or-n-p)

;; complete symbol when `eval'
(define-key read-expression-map (kbd "TAB") 'completion-at-point)

(define-key minibuffer-local-map (kbd "C-u")
  (lambda () (interactive) (delete-region (point-at-bol) (point))))
;; I dont know these bindings are good
(define-key minibuffer-local-map (kbd "C-p") (kbd "ESC p"))
(define-key minibuffer-local-map (kbd "C-n") (kbd "ESC n"))

(when (safe-require-or-eval 'minibuffer-line)
  (set-face-underline 'minibuffer-line nil)
  (set-variable 'minibuffer-line-refresh-interval
                25)

  ;; Set idle timer
  (defvar my-minibuffer-line--idle-timer nil)
  (defvar minibuffer-line-mode)
  (add-hook 'minibuffer-line-mode-hook
            (lambda ()
              (when my-minibuffer-line--idle-timer
                (cancel-timer my-minibuffer-line--idle-timer)
                (setq my-minibuffer-line--idle-timer nil))
              (when minibuffer-line-mode
                (setq my-minibuffer-line--idle-timer
                      (run-with-idle-timer 0.5
                                           t
                                           'minibuffer-line--update)))))


  (set-variable 'minibuffer-line-format
                `(,(concat user-login-name
                           "@"
                           (car (split-string (system-name)
                                              "\\."))
                           ":")
                  (:eval (abbreviate-file-name (or buffer-file-name
                                                   default-directory)))
                  (:eval (and (fboundp 'git-ps1-mode-get-current)
                              (git-ps1-mode-get-current " [GIT:%s]")))
                  " "
                  (:eval (format-time-string display-time-format))))
  (minibuffer-line-mode 1)
  )

(when (safe-require-or-eval 'prompt-text)

  (set-variable 'prompt-text-format
                `(,(concat ""
                           user-login-name
                           "@"
                           (car (split-string (system-name)
                                              "\\."))
                           ":")
                  (:eval (abbreviate-file-name (or buffer-file-name
                                                   default-directory)))
                  (:eval (and (fboundp 'git-ps1-mode-get-current)
                              (git-ps1-mode-get-current " [GIT:%s]")))
                  " "
                  (:eval (format-time-string display-time-format))
                  "\n"
                  (:eval (symbol-name this-command))
                  ": "))
  (prompt-text-mode 1))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; letters, font-lock mode and fonts

(setq text-quoting-style 'grave)

;; (set-face-background 'vertical-border (face-foreground 'mode-line))

;; (set-window-margins (selected-window) 1 1)

(unless window-system
  (setq frame-background-mode 'dark))

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

(when (safe-require-or-eval 'paren)
  (show-paren-mode 1)
  (setq show-paren-delay 0.5
        show-paren-style 'parenthesis)    ; mixed is hard to read
  ;; (set-face-background 'show-paren-match
  ;;                      "black")
  ;;                      ;; (face-foreground 'default))
  ;; (set-face-foreground 'show-paren-match
  ;;                      "white")
  ;; (set-face-inverse-video-p 'show-paren-match
  ;;                           t)
  )

(transient-mark-mode 1)

(global-font-lock-mode 1)
(setq font-lock-global-modes
      '(not
        help-mode
        eshell-mode
        ;;term-mode
        Man-mode
        magit-diff-mode
        magit-revision-mode))

;; (standard-display-ascii ?\n "$\n")

;; (defvar my-eol-face
;;   '(("\n" . (0 font-lock-comment-face t nil)))
;;   )
;; (defvar my-tab-face
;;   '(("\t" . '(0 highlight t nil))))
(defvar my-jspace-face
  '(("\u3000" . '(0 highlight t nil))))

(add-hook 'font-lock-mode-hook
          (lambda ()
            ;; (font-lock-add-keywords nil my-eol-face)
            (font-lock-add-keywords nil my-jspace-face)
            ))

(when (safe-require-or-eval 'whitespace)
  (add-to-list 'whitespace-display-mappings
               ;; We need t since last one takes precedence
               `(tab-mark ?\t ,(vconcat "^I\t")) t)
  ;; (add-to-list 'whitespace-display-mappings
  ;;              `(newline-mark ?\n ,(vconcat "$\n")))
  (setq whitespace-style '(face
                           trailing     ; trailing blanks
                           ;; tabs
                           ;; spaces
                           ;; lines
                           lines-tail  ; lines over 80
                           newline      ; newlines
                           empty        ; empty lines at beg or end of buffer
                           ;; big-indent
                           ;; space-mark
                           tab-mark
                           newline-mark ; use display table for newline
                           ))
  ;; (setq whitespace-newline 'font-lock-comment-face)
  ;; (setq whitespace-style (delq 'newline-mark whitespace-style))
  (defun my-whitesspace-mode-reload ()
    "Reload whitespace-mode config."
    (interactive)
    (when whitespace-mode
      (whitespace-mode 0)
      (whitespace-mode 1)))

  (set-variable 'whitespace-line-column nil)
  (global-whitespace-mode t)
  (add-hook 'dired-mode-hook
            (lambda ()
              (set (make-local-variable 'whitespace-style) nil)))
  (if (>= (display-color-cells)
          256)
      (set-face-foreground 'whitespace-newline "color-109")
    ;; (progn
    ;;   (set-face-bold-p 'whitespace-newline
    ;;                      t))
    ))
(and nil
     '(safe-require-or-eval 'fill-column-indicator)
     (setq fill-column-indicator))

;; highlight current line
;; http://wiki.riywo.com/index.php?Meadow
(face-spec-set 'hl-line
               '((((min-colors 256)
                   (background dark))
                  (:background "color-234"))
                 (((min-colors 256)
                   (background light))
                  (:background "color-234"))
                 (t
                  (:underline "black"))))
(set-variable 'hl-line-global-modes
              '(not
                term-mode))
(global-hl-line-mode 1) ;; (hl-line-mode 1)

(set-face-foreground 'font-lock-regexp-grouping-backslash "#666")
(set-face-foreground 'font-lock-regexp-grouping-construct "#f60")

;;(safe-require-or-eval 'set-modeline-color)

;; (let ((fg (face-foreground 'default))
;;       (bg (face-background 'default)))
;;   (set-face-background 'mode-line-inactive
;;                        (if (face-inverse-video-p 'mode-line) fg bg))
;;   (set-face-foreground 'mode-line-inactive
;;                        (if (face-inverse-video-p 'mode-line) bg fg)))
;; (set-face-underline 'mode-line-inactive
;;                     t)
;; (set-face-underline 'vertical-border
;;                     nil)

;; (when (safe-require-or-eval 'end-mark)
;;   (global-end-mark-mode))

;; M-x highlight-* to highlight things
(global-hi-lock-mode 1)

(unless (fboundp 'highlight-region-text)
  (defun highlight-region-text (beg end)
    "Highlight text between BEG and END."
    (interactive "r")
    (highlight-regexp (regexp-quote (buffer-substring-no-properties beg
                                                                    end)))
    (setq deactivate-mark t)))

(when (safe-require-or-eval 'auto-highlight-symbol)
  (set-variable 'ahs-idle-interval 0.6)
  (global-auto-highlight-symbol-mode 1))


(when (safe-require-or-eval 'highlight-indentation)
  (set-face-background 'highlight-indentation-face "color-236")
  (dolist (hook
           '(
             prog-mode-hook
             ))
    (add-hook hook
              'highlight-indentation-mode)))
;; (set-face-background 'highlight-indentation-current-column-face "#c3b3b3")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; file handling

(when (and (autoload-eval-lazily 'fzf)
           (executable-find "fzf"))
  ;; Too slow!
  ;; (set-variable 'fzf/executable "sk")
  ;; (set-variable 'fzf/args "--color bw --print-query")
  ;; Modified from hardcoded default to include:
  ;;  - directories
  ;;  - hidden files
  ;;  - root directory (.)
  ;;  - parent directory (..)
  (let* ((find (if (executable-find "bfs")
                   ;; Breadth-first find https://github.com/tavianator/bfs
                   "bfs"
                 "find"))
         (defcmd (concat "set -eu; set -o pipefail; "
                         "echo .; "
                         "echo ..; "
                         "command " find " -L . "
                         "-mindepth 1 "
                         "\\( -fstype 'sysfs' -o -fstype 'devfs' -o -fstype 'devtmpfs' -o -fstype 'proc' \\) -prune "
                         "-o -print "
                         "2> /dev/null "
                         "| "
                         "cut -b3-")))
    (setenv "FZF_DEFAULT_COMMAND" defcmd))
  (set-variable 'fzf/window-height 45)
  (define-key ctl-x-map (kbd "C-f") 'fzf)
  )

(when (safe-require-or-eval 'recently)
  (recently-mode 1))

(when (safe-require-or-eval 'editorconfig)
  (set-variable 'editorconfig-get-properties-function
                'editorconfig-core-get-properties-hash)
  (editorconfig-mode 1)
  (set-variable 'editorconfig-mode-lighter " EC")
  (when (fboundp 'ws-butler-mode)
    (set-variable 'editorconfig-trim-whitespaces-mode
                  'ws-butler-mode))
  (with-eval-after-load 'org-src
    ;; [*.org\[\*Org Src*\[ c \]*\]]
    (add-hook 'org-src-mode-hook
              'editorconfig-mode-apply t)))

(when (fboundp 'editorconfig-custom-majormode)
  (add-hook 'editorconfig-after-apply-functions
            'editorconfig-custom-majormode))

;; Add readonly=true to set read-only-mode
(add-hook 'editorconfig-after-apply-functions
          (lambda (props)
            (let ((r (gethash 'readonly props)))
              (when (and (string= r "true")
                         (not buffer-read-only))
                (read-only-mode 1)))))

(add-hook 'editorconfig-hack-properties-functions
          '(lambda (props)
             (when (derived-mode-p 'makefile-mode)
               (puthash 'indent_style "tab" props))))

;; (when (fboundp 'editorconfig-charset-extras)
;;   (add-hook 'editorconfig-custom-hooks
;;             'editorconfig-charset-extras))

(setq revert-without-query '(".+"))

;; save cursor position
(when (safe-require-or-eval 'saveplace)
  (setq-default save-place t)
  (setq save-place-file (concat user-emacs-directory
                                "places")))

;; http://www.bookshelf.jp/soft/meadow_24.html#SEC260
(setq make-backup-files t)
(setq vc-make-backup-files t)
;; (make-directory (expand-file-name "~/.emacsbackup"))
(setq backup-directory-alist
      (cons (cons "." (expand-file-name (concat user-emacs-directory
                                                "backup")))
            backup-directory-alist))
(setq version-control 't)
(setq delete-old-versions t)
(setq kept-new-versions 20)

(setq auto-save-list-file-prefix (expand-file-name (concat user-emacs-directory
                                                           "auto-save/")))
;; (setq delete-auto-save-files t)
(setq auto-save-visited-interval 8)
(auto-save-visited-mode 1)

(add-to-list 'completion-ignored-extensions ".bak")
(set-variable 'completion-cycle-threshold nil)  ;; NEVER use
(setq delete-by-moving-to-trash t)
;;       trash-directory "~/.emacs.d/trash")

(add-hook 'after-save-hook
          'executable-make-buffer-file-executable-if-script-p)

(set-variable 'bookmark-default-file
              (expand-file-name (concat user-emacs-directory
                                        "bmk")))
(set-variable 'bookmark-save-flag
              1)
(with-eval-after-load 'recentf
  (defvar recentf-exclude)
  (defvar bookmark-default-file)
  (add-to-list 'recentf-exclude
               (regexp-quote bookmark-default-file)))

(when (safe-require-or-eval 'smart-revert)
  (smart-revert-on))

;; autosave
;; auto-save-visited-mode can be used instead?
;; (when (safe-require-or-eval 'autosave)
;;   (autosave-set 8))


;; bookmarks

;; (define-key ctl-x-map "m" 'list-bookmarks)

;; vc

(set-variable 'vc-handled-backends '(RCS))
(set-variable 'vc-rcs-register-switches "-l")
(set-variable 'vc-rcs-checkin-switches "-l")
(set-variable 'vc-command-messages t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; share clipboard with x

;; this page describes this in details, but only these sexps seem to be needed
;; http://garin.jp/doc/Linux/xwindow_clipboard

(and nil
     (not window-system)
     (not (eq window-system 'mac))
     (getenv "DISPLAY")
     (not (equal (getenv "DISPLAY") ""))
     (executable-find "xclip")
     ;; (< emacs-major-version 24)
     '(safe-require-or-eval 'xclip)
     nil
     (turn-on-xclip))

(and (eq system-type 'darwin)
     (safe-require-or-eval 'pasteboard)
     (turn-on-pasteboard))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; some modes and hooks

;; Include some extra modes
(require 'generic-x)

(when (autoload-eval-lazily 'wgrep)
  (set-variable 'wgrep-auto-save-buffer t)
  (with-eval-after-load 'grep
    (defvar grep-mode-map)
    (define-key grep-mode-map
      "e"
      'wgrep-change-to-wgrep-mode)))

(with-eval-after-load 'remember
  (defvar remember-mode-map (make-sparse-keymap))
  (define-key remember-mode-map (kbd "C-x C-s") 'ignore))

(with-eval-after-load 'magit-files
  ;; `global-magit-file-mode' is enabled by default and this mode overwrites
  ;; existing keybindings.
  ;; Apparently it is a HARMFUL behavior and it is really awful that I have
  ;; to disable thie mode here, but do anyway.
  ;; See also https://github.com/magit/magit/issues/3517
  (global-magit-file-mode -1))

(with-eval-after-load 'magit-section
  (set-face-background 'magit-section-highlight
                       nil))

(with-eval-after-load 'magit-diff
  (set-face-background 'magit-diff-added-highlight
                       nil)
  (set-face-background 'magit-diff-removed-highlight
                       nil)
  (set-face-background 'magit-diff-context-highlight
                       nil)
  )

(when (boundp 'git-rebase-filename-regexp)
  (add-to-list 'auto-mode-alist
               `(,git-rebase-filename-regexp . text-mode)))

(when (safe-require-or-eval 'aggressive-indent)
  (defvar aggressive-indent-excluded-modes)
  (setq aggressive-indent-excluded-modes
        `(diff-mode
          toml-mode
          conf-mode
          dockerfile-mode
          groovy-mode
          scala-mode
          ,@aggressive-indent-excluded-modes))
  (global-aggressive-indent-mode 1))

(when (autoload-eval-lazily 'ggtags)
  (add-hook 'c-mode-common-hook
            (lambda ()
              (when (derived-mode-p 'c-mode 'c++-mode 'java-mode)
                (ggtags-mode 1))))
  (add-hook 'python-mode-hook
            (lambda ()
              (ggtags-mode 1))))

(when (autoload-eval-lazily 'imenu-list)
  ;; (set-variable 'imenu-list-auto-resize t)
  (set-variable 'imenu-list-focus-after-activation t)
  (define-key ctl-x-map "l" 'imenu-list-smart-toggle))

(add-hook 'emacs-lisp-mode-hook
          (lambda ()
            (setq imenu-generic-expression
                  `(("Sections" ";;;\+\n;; \\(.*\\)\n" 1)
                    ,@imenu-generic-expression))))

(with-eval-after-load 'compile
  (defvar compilation-filter-start)
  (defvar compilation-error-regexp-alist)
  (require 'ansi-color)
  (add-hook 'compilation-filter-hook
            (lambda ()
              (let ((inhibit-read-only t))
                (ansi-color-apply-on-region compilation-filter-start
                                            (point)))))
  (add-to-list 'compilation-error-regexp-alist
               ;; ansible-lint
               '("^\\([^ \n]+\\):\\([0-9]+\\)$" 1 2)))

;; Workaround to avoid ensime error
(defvar ensime-mode-key-prefix nil)

;; http://qiita.com/sune2/items/b73037f9e85962f5afb7
(when (safe-require-or-eval 'company)
  (global-company-mode)
  (set-variable 'company-idle-delay 0.5)
  (set-variable 'company-minimum-prefix-length 2)
  (set-variable 'company-selection-wrap-around t))


;; https://github.com/lunaryorn/flycheck
(when (safe-require-or-eval 'flycheck)
  (call-after-init 'global-flycheck-mode))

(when (autoload-eval-lazily 'ilookup)
  (define-key ctl-x-map "d" 'ilookup-open-word))

(set-variable 'ac-ignore-case nil)

(when (autoload-eval-lazily 'term-run '(term-run-shell-command term-run))
  (define-key ctl-x-map "t" 'term-run-shell-command))

(add-to-list 'safe-local-variable-values
             '(encoding utf-8))
(setq enable-local-variables :safe)

;; Detect file type from shebang and set major-mode.
(add-to-list 'interpreter-mode-alist
             '("python3" . python-mode))
(add-to-list 'interpreter-mode-alist
             '("python2" . python-mode))
(with-eval-after-load 'python
  (defvar python-mode-map (make-sparse-keymap))
  (define-key python-mode-map (kbd "C-m") 'newline-and-indent))

(when (autoload-eval-lazily 'pipenv)
  (add-hook 'python-mode-hook
            (lambda ()
              (pipenv-mode 1)
              (pipenv-projectile-after-switch-default)))
  )
(set-variable 'flycheck-python-pycompile-executable "python3")
(set-variable 'python-indent-guess-indent-offset nil)
(with-eval-after-load 'blacken
  (when (require 'with-venv nil t)
    (with-venv-advice-add 'blacken-buffer)))


;; http://fukuyama.co/foreign-regexp
'(and (safe-require-or-eval 'foreign-regexp)
      (progn
        (setq foreign-regexp/regexp-type 'perl)
        '(setq reb-re-syntax 'foreign-regexp)
        ))

(autoload-eval-lazily 'sql '(sql-mode)
  (require 'sql-indent nil t))
(add-to-list 'auto-mode-alist
             '("\\.hql\\'" . sql-mode))

(when (autoload-eval-lazily 'git-command)
  (define-key ctl-x-map "g" 'git-command))

(when (autoload-eval-lazily 'gited)
  (define-key ctl-x-map (kbd "C-g") 'gited-list))

(when (safe-require-or-eval 'git-commit)
  (global-git-commit-mode 1))
(with-eval-after-load 'git-commit
  (add-hook 'git-commit-setup-hook
            'turn-off-auto-fill t))

(autoload-eval-lazily 'sl)

(with-eval-after-load 'rst
  (defvar rst-mode-map)
  (define-key rst-mode-map (kbd "C-m") 'newline-and-indent))

(with-eval-after-load 'jdee
  (add-hook 'jdee-mode-hook
            (lambda ()
              (make-local-variable 'global-mode-string)
              (add-to-list 'global-mode-string
                           mode-line-position))))

;; Cannot enable error thrown. Why???
;; https://github.com/m0smith/malabar-mode#Installation
;; (when (autoload-eval-lazily 'malabar-mode)
;;   (add-to-list 'load-path
;;                (expand-file-name (concat user-emacs-directory "/cedet")))
;;   (safe-require-or-eval 'cedet-devel-load)
;;   (call-after-init 'activate-malabar-mode))

(with-eval-after-load 'make-mode
  (defvar makefile-mode-map (make-sparse-keymap))
  (define-key makefile-mode-map (kbd "C-m") 'newline-and-indent)
  ;; this functions is set in write-file-functions, i cannot find any
  ;; good way to remove this.
  (fset 'makefile-warn-suspicious-lines 'ignore))

(with-eval-after-load 'verilog-mode
  (defvar verilog-mode-map (make-sparse-keymap))
  (define-key verilog-mode-map ";" 'self-insert-command))


(setq diff-switches "-u")
(with-eval-after-load 'diff-mode
  ;; (when (and (eq major-mode
  ;;                'diff-mode)
  ;;            (not buffer-file-name))
  ;;   ;; do not pass when major-mode is derived mode of diff-mode
  ;;   (view-mode 1))
  (set-face-attribute 'diff-header nil
                      :foreground nil
                      :background nil
                      :weight 'bold)
  (set-face-attribute 'diff-file-header nil
                      :foreground nil
                      :background nil
                      :weight 'bold)
  (set-face-foreground 'diff-index "blue")
  (set-face-attribute 'diff-hunk-header nil
                      :foreground "cyan"
                      :weight 'normal)
  (set-face-attribute 'diff-context nil
                      ;; :foreground "white"
                      :foreground nil
                      :weight 'normal)
  (set-face-foreground 'diff-removed "red")
  (set-face-foreground 'diff-added "green")
  (set-face-background 'diff-removed nil)
  (set-face-background 'diff-added nil)
  (set-face-attribute 'diff-changed nil
                      :foreground "magenta"
                      :weight 'normal)
  (set-face-attribute 'diff-refine-changed nil
                      :foreground nil
                      :background nil
                      :weight 'bold
                      :inverse-video t)
  ;; Annoying !
  ;;(diff-auto-refine-mode)
  )

;; (ffap-bindings)

(set-variable 'browse-url-browser-function
              'eww-browse-url)

(set-variable 'sh-here-document-word "__EOC__")

(when (autoload-eval-lazily 'adoc-mode
          nil
        (defvar adoc-mode-map (make-sparse-keymap))
        (define-key adoc-mode-map (kbd "C-m") 'newline))
  (setq auto-mode-alist
        `(("\\.adoc\\'" . adoc-mode)
          ("\\.asciidoc\\'" . adoc-mode)
          ,@auto-mode-alist)))

(with-eval-after-load 'markup-faces
  ;; Is this too match ?
  (set-face-foreground 'markup-meta-face
                       "color-245")
  (set-face-foreground 'markup-meta-hide-face
                       "color-245")
  )

;; TODO: check if this is required
(when (autoload-eval-lazily 'groovy-mode nil
        (defvar groovy-mode-map (make-sparse-keymap))
        (define-key groovy-mode-map "(" 'self-insert-command)
        (define-key groovy-mode-map ")" 'self-insert-command)
        (define-key groovy-mode-map (kbd "C-m") 'newline-and-indent)
        )
  (add-to-list 'auto-mode-alist
               '("build\\.gradle\\'" . groovy-mode)))

(add-to-list 'auto-mode-alist
             '("\\.gawk\\'" . awk-mode))

(with-eval-after-load 'yaml-mode
  (defvar yaml-mode-map (make-sparse-keymap))
  (define-key yaml-mode-map (kbd "C-m") 'newline))

(with-eval-after-load 'html-mode
  (defvar html-mode-map (make-sparse-keymap))
  (define-key html-mode-map (kbd "C-m") 'reindent-then-newline-and-indent))

(with-eval-after-load 'text-mode
  (define-key text-mode-map (kbd "C-m") 'newline))

(autoload-eval-lazily 'info nil
  (defvar Info-additional-directory-list)
  (dolist (dir (directory-files (concat user-emacs-directory
                                        "info")
                                t
                                "^[^.].*"))
    (when (file-directory-p dir)
      (add-to-list 'Info-additional-directory-list
                   dir)))
  (let ((dir (expand-file-name "~/.brew/share/info")))
    (when (file-directory-p dir)
      (add-to-list 'Info-additional-directory-list
                   dir))))

(with-eval-after-load 'apropos
  (defvar apropos-mode-map (make-sparse-keymap))
  (define-key apropos-mode-map "n" 'next-line)
  (define-key apropos-mode-map "p" 'previous-line))

;; `isearch' library does not call `provide' so cannot use with-eval-after-load
;; (define-key isearch-mode-map
;;   (kbd "C-j") 'isearch-other-control-char)
;; (define-key isearch-mode-map
;;   (kbd "C-k") 'isearch-other-control-char)
;; (define-key isearch-mode-map
;;   (kbd "C-h") 'isearch-other-control-char)
(define-key isearch-mode-map (kbd "C-h") 'isearch-del-char)
(define-key isearch-mode-map (kbd "M-r")
  'isearch-query-replace-regexp)
;; do not cleanup isearch highlight: use `lazy-highlight-cleanup' to remove
(setq lazy-highlight-cleanup nil)
;; face for isearch highlighing
(set-face-attribute 'lazy-highlight
                    nil
                    :foreground `unspecified
                    :background `unspecified
                    :underline t
                    ;; :weight `bold
                    )

(add-hook 'outline-mode-hook
          (lambda ()
            (when (string-match "\\.md\\'" buffer-file-name)
              (set (make-local-variable 'outline-regexp) "#+ "))))
(add-hook 'outline-mode-hook
          'outline-show-all)
(add-to-list 'auto-mode-alist (cons "\\.ol\\'" 'outline-mode))

(add-to-list 'auto-mode-alist (cons "\\.md\\'" 'outline-mode))
(when (autoload-eval-lazily 'markdown-mode
          '(markdown-mode gfm-mode)
        (defvar gfm-mode-map (make-sparse-keymap))
        (define-key gfm-mode-map (kbd "C-m") 'electric-indent-just-newline))
  (add-to-list 'auto-mode-alist (cons "\\.md\\'" 'gfm-mode))
  (set-variable 'markdown-command (or (executable-find "markdown")
                                      (executable-find "markdown.pl")
                                      ""))
  (add-hook 'markdown-mode-hook
            (lambda ()
              (outline-minor-mode 1)
              (flyspell-mode)
              (set (make-local-variable 'comment-start) ";")))
  )

;; c-mode
;; http://www.emacswiki.org/emacs/IndentingC
;; http://en.wikipedia.org/wiki/Indent_style
;; http://d.hatena.ne.jp/emergent/20070203/1170512717
;; http://seesaawiki.jp/whiteflare503/d/Emacs%20%a5%a4%a5%f3%a5%c7%a5%f3%a5%c8
(with-eval-after-load 'cc-vars
  (defvar c-default-style nil)
  (add-to-list 'c-default-style
               '(c-mode . "k&r"))
  (add-to-list 'c-default-style
               '(c++-mode . "k&r")))

(autoload-eval-lazily 'js2-mode nil
  ;; currently do not use js2-mode
  ;; (add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
  ;; (add-to-list 'auto-mode-alist '("\\.jsm\\'" . js2-mode))
  ;; (defvar js2-mode-map (make-sparse-keymap))
  ;; (define-key js2-mode-map (kbd "C-m") (lambda ()
  ;;                                        (interactive)
  ;;                                        (js2-enter-key)
  ;;                                        (indent-for-tab-command)))
  ;; (add-hook (kill-local-variable 'before-save-hook)
  ;;           'js2-before-save)
  ;; (add-hook 'before-save-hook
  ;;           'my-indent-buffer
  ;;           nil
  ;;           t)
  )

(add-to-list 'interpreter-mode-alist
             '("node" . js-mode))

(add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)

(with-eval-after-load 'uniquify
  (setq uniquify-buffer-name-style 'post-forward-angle-brackets)
  (setq uniquify-ignore-buffers-re "*[^*]+*")
  (setq uniquify-min-dir-content 1))

(with-eval-after-load 'view
  (defvar view-mode-map (make-sparse-keymap))
  (define-key view-mode-map "j" 'scroll-up-line)
  (define-key view-mode-map "k" 'scroll-down-line)
  (define-key view-mode-map "v" 'toggle-read-only)
  (define-key view-mode-map "q" 'bury-buffer)
  ;; (define-key view-mode-map "/" 'nonincremental-re-search-forward)
  ;; (define-key view-mode-map "?" 'nonincremental-re-search-backward)
  ;; (define-key view-mode-map
  ;;   "n" 'nonincremental-repeat-search-forward)
  ;; (define-key view-mode-map
  ;;   "N" 'nonincremental-repeat-search-backward)

  ;; N conflicts with git-walktree
  ;; (define-key view-mode-map "/" 'isearch-forward-regexp)
  ;; (define-key view-mode-map "?" 'isearch-backward-regexp)
  ;; (define-key view-mode-map "n" 'isearch-repeat-forward)
  ;; (define-key view-mode-map "N" 'isearch-repeat-backward)

  (define-key view-mode-map (kbd "C-m") 'my-rgrep-symbol-at-point))
(global-set-key "\M-r" 'view-mode)
;; (setq view-read-only t)

(with-eval-after-load 'term
  (defvar term-raw-map (make-sparse-keymap))
  (define-key term-raw-map (kbd "C-x")
    (lookup-key (current-global-map)
                (kbd "C-x"))))
(add-hook 'term-mode-hook
          (lambda ()
            ;; Stop current line highlighting
            (set (make-local-variable (defvar hl-line-range-function))
                 (lambda () '(0 . 0)))
            (set (make-local-variable 'scroll-margin)
                 0)
            (set-variable 'term-buffer-maximum-size 20480)
            ))

(add-hook 'Man-mode-hook
          (lambda ()
            (view-mode 1)
            (setq truncate-lines nil)))
(set-variable 'Man-notify-method (if window-system
                                     'newframe
                                   'aggressive))

(set-variable 'woman-cache-filename (expand-file-name (concat user-emacs-directory
                                                              "woman_cache.el")))
;; not work because man.el will be loaded when man called
(defalias 'man 'woman)

(add-to-list 'auto-mode-alist
             '("tox\\.ini\\'" . conf-unix-mode))

(when (autoload-eval-lazily 'toml-mode)
  (add-to-list 'auto-mode-alist
               '("/tox\\.ini\\'" . toml-mode))
  (add-to-list 'auto-mode-alist
               '("/Pipfile\\'" . toml-mode))
  (add-to-list 'auto-mode-alist
               '("/poetry\\.lock\\'" . toml-mode))
  )

(when (autoload-eval-lazily 'json-mode)
  (add-to-list 'auto-mode-alist
               '("/Pipfile\\.lock\\'" . json-mode)))

(add-hook 'go-mode-hook
          (lambda()
            (defvar go-mode-map)
            (add-hook 'before-save-hook' 'gofmt-before-save nil t)
            (define-key go-mode-map (kbd "M-.") 'godef-jump)))

(when (autoload-eval-lazily 'k8s-mode)
  (add-to-list 'auto-mode-alist
               '("\\.k8s\\'" . k8s-mode)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; buffers

(defvar bs-configurations)
(when (autoload-eval-lazily 'bs '(bs-show)
        (add-to-list 'bs-configurations
                     '("specials" "^\\*" nil ".*" nil nil))
        (add-to-list 'bs-configurations
                     '("files-and-specials" "^\\*" buffer-file-name ".*" nil nil))
        (defvar bs-mode-map)
        (defvar bs-current-configuration)
        (define-key bs-mode-map (kbd "t")
          ;; TODO: fix toggle feature
          (lambda ()
            (interactive)
            (if (string= "specials"
                         bs-current-configuration)
                (bs-set-configuration "files")
              (bs-set-configuration "specials"))
            (bs-refresh)
            (bs-message-without-log "%s"
                                    (bs--current-config-message))))
        ;; (setq bs-configurations (list
        ;; '("processes" nil get-buffer-process ".*" nil nil)
        ;; '("files-and-scratch" "^\\*scratch\\*$" nil nil
        ;; bs-visits-non-file bs-sort-buffer-interns-are-last)))
        )
  (defalias 'list-buffers 'bs-show)
  (set-variable 'bs-default-configuration "files-and-specials")
  (set-variable 'bs-default-sort-name "by nothing")
  (add-hook 'bs-mode-hook
            (lambda ()
              (set (make-local-variable 'scroll-margin) 0))))

;;(iswitchb-mode 1)
(icomplete-mode)

(defun iswitchb-buffer-display-other-window ()
  "Do iswitchb in other window."
  (interactive)
  (let ((iswitchb-default-method 'display))
    (call-interactively 'iswitchb-buffer)))


;; buffer killing

;; (defun my-delete-window-killing-buffer () nil)

(defun my-query-kill-current-buffer ()
  "Interactively kill current buffer."
  (interactive)
  (if (y-or-n-p (concat "kill current buffer? :"))
      (kill-buffer (current-buffer))))
;;(global-set-key "\C-xk" 'my-query-kill-current-buffer)
(substitute-key-definition 'kill-buffer
                           'my-query-kill-current-buffer
                           global-map)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; recentf-mode

(set-variable 'recentf-save-file (expand-file-name (concat user-emacs-directory
                                                           "recentf")))
(set-variable 'recentf-max-menu-items 20)
(set-variable 'recentf-max-saved-items 30)
(set-variable 'recentf-show-file-shortcuts-flag nil)
(set-variable 'recentf-auto-cleanup 3)

;; (safe-require-or-eval 'sync-recentf)

;; (when (safe-require-or-eval 'recentf)
;;   (add-to-list 'recentf-exclude
;;                (regexp-quote recentf-save-file))
;;   (add-to-list 'recentf-exclude
;;                (regexp-quote (expand-file-name user-emacs-directory)))
;;   (add-to-list 'recentf-exclude
;;                "/sync-recentf-marker\\'")
;;   (define-key ctl-x-map (kbd "C-r") 'recentf-open-files)
;;   (remove-hook 'find-file-hook
;;                'recentf-track-opened-file)
;;   (defun my-recentf-load-track-save-list ()
;;     "Load current recentf list from file, track current visiting file, then save
;; the list."
;;     (recentf-load-list)
;;     (recentf-track-opened-file)
;;     (recentf-save-list))
;;   (add-hook 'find-file-hook
;;             'my-recentf-load-track-save-list)
;;   (add-hook 'kill-emacs-hook
;;             'recentf-load-list)
;;   ;;(run-with-idle-timer 5 t 'recentf-save-list)
;;   ;; (add-hook 'find-file-hook
;;   ;;           (lambda ()
;;   ;;             (recentf-add-file default-directory)))
;;   (when (autoload-eval-lazily 'recentf-show)
;;     (define-key ctl-x-map (kbd "C-r") 'recentf-show)
;;     ;; (add-hook 'recentf-show-before-listing-hook
;;     ;;           'recentf-load-list)
;;     )
;;   (recentf-mode 1)
;;   (define-key recentf-dialog-mode-map (kbd "<up>") 'previous-line)
;;   (define-key recentf-dialog-mode-map (kbd "<down>") 'next-line)
;;   (define-key recentf-dialog-mode-map "p" 'previous-line)
;;   (define-key recentf-dialog-mode-map "n" 'next-line)
;;   (add-hook 'recentf-dialog-mode-hook
;;             (lambda ()
;;               ;; (recentf-save-list)
;;               ;; (define-key recentf-dialog-mode-map (kbd "C-x C-f")
;;               ;; 'my-recentf-cd-and-find-file)
;;               (cd "~/"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; dired

(defun my-file-head (filename &optional n)
  "Return list of first N lines of file FILENAME."
  ;; TODO: Fix for janapese text
  ;; TODO: Fix for short text
  (let ((num (or n 10))
        (size 100)
        (beg 0)
        (end 0)
        (result '())
        (read -1))
    (with-temp-buffer
      (erase-buffer)
      (while (or (<= (count-lines (point-min)
                                  (point-max))
                     num)
                 (eq read 0))
        (setq end (+ beg size))
        (setq read (nth 1 (insert-file-contents-literally filename
                                                          nil
                                                          beg
                                                          end)))
        (goto-char (point-max))
        (setq beg (+ beg size)))
      (goto-char (point-min))
      (while (< (length result) num)
        (let ((start (point)))
          (forward-line 1)
          (setq result
                `(,@result ,(buffer-substring-no-properties start
                                                            (point))))))
      result
      ;; (buffer-substring-no-properties (point-min)
      ;;                                 (progn
      ;;                                   (forward-line num)
      ;;                                   (point)))
      )))
;; (apply 'concat (my-file-head "./shrc" 10)


(defun my-dired-echo-file-head (arg)
  "Echo head of current file.

ARG is num to show, or defaults to 7."
  (interactive "P")
  (let ((f (dired-get-filename)))
    (message "%s"
             (apply 'concat
                    (my-file-head f
                                  7)))))

(defun my-dired-diff ()
  "Show diff of marked file and file of current line."
  (interactive)
  (let ((files (dired-get-marked-files nil nil nil t)))
    (if (eq (car files)
            t)
        (diff (cadr files) (dired-get-filename))
      (message "One file must be marked!"))))

(defun dired-get-file-info ()
  "Print information of current line file."
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
  "Scroll up."
  (interactive)
  (my-dired-previous-line (- (window-height) 1)))

(defun my-dired-scroll-down ()
  "Scroll down."
  (interactive)
  (my-dired-next-line (- (window-height) 1)))

;; (defun my-dired-forward-line (arg)
;;   ""
;;   (interactive "p"))

(defun my-dired-previous-line (arg)
  "Move ARG lines up."
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
  "Move ARG lines down."
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

(defun my-tramp-remote-find-file (f)
  "Open F."
  (interactive (list (read-file-name "My Find File Tramp: "
                                     "/scp:"
                                     nil ;; "/scp:"
                                     (confirm-nonexistent-file-or-buffer))))
  (find-file f))

;;http://bach.istc.kobe-u.ac.jp/lect/tamlab/ubuntu/emacs.html

(if (eq window-system 'mac)
    (setq dired-listing-switches "-lhF")
  (setq dired-listing-switches "-lhF --time-style=long-iso")
  )
(setq dired-listing-switches "-lhF")

;; when using dired-find-alternate-file
;; reuse current dired buffer for the file to open
;; (put 'dired-find-alternate-file 'disabled nil)
(set-variable 'dired-ls-F-marks-symlinks t)

(set-variable 'ls-lisp-use-insert-directory-program nil) ; always use ls-lisp
(set-variable 'ls-lisp-dirs-first t)
(set-variable 'ls-lisp-use-localized-time-format t)
(set-variable 'ls-lisp-format-time-list
              '("%Y-%m-%d %H:%M"
                "%Y-%m-%d      "))

(set-variable 'dired-dwim-target t)
(set-variable 'dired-isearch-filenames t)
(set-variable 'dired-hide-details-hide-symlink-targets nil)
(set-variable 'dired-hide-details-hide-information-lines nil)

(set-variable 'dired-deletion-confirmer 'y-or-n-p)
(set-variable 'dired-recursive-deletes 'always)

;; (add-hook 'dired-after-readin-hook
;;           'my-replace-nasi-none)

(with-eval-after-load 'dired
  (safe-require-or-eval 'ls-lisp)
  (defvar dired-mode-map (make-sparse-keymap))
  ;; dired-do-chgrp sometimes cause system hung
  (define-key dired-mode-map "G" 'ignore)
  (define-key dired-mode-map "e" 'wdired-change-to-wdired-mode)
  (define-key dired-mode-map "i" 'dired-get-file-info)
  (define-key dired-mode-map "f" 'find-file)
  (define-key dired-mode-map "!" 'shell-command)
  (define-key dired-mode-map "&" 'async-shell-command)
  (define-key dired-mode-map "X" 'dired-do-async-shell-command)
  (define-key dired-mode-map "=" 'my-dired-diff)
  (define-key dired-mode-map "B" 'gtkbm-add-current-dir)
  (define-key dired-mode-map "b" 'gtkbm)
  (define-key dired-mode-map "h" 'my-dired-echo-file-head)
  (define-key dired-mode-map (kbd "TAB") 'other-window)
  ;; (define-key dired-mode-map "P" 'my-dired-do-pack-or-unpack)
  (define-key dired-mode-map "/" 'dired-isearch-filenames)
  (define-key dired-mode-map (kbd "DEL") 'dired-up-directory)
  (define-key dired-mode-map (kbd "C-h") 'dired-up-directory)
  (substitute-key-definition 'dired-next-line
                             'my-dired-next-line
                             dired-mode-map)
  (substitute-key-definition 'dired-previous-line
                             'my-dired-previous-line
                             dired-mode-map)
  ;; (define-key dired-mode-map (kbd "C-p") 'my-dired-previous-line)
  ;; (define-key dired-mode-map (kbd "p") 'my-dired-previous-line)
  ;; (define-key dired-mode-map (kbd "C-n") 'my-dired-next-line)
  ;; (define-key dired-mode-map (kbd "n") 'my-dired-next-line)
  (define-key dired-mode-map (kbd "<left>") 'my-dired-scroll-up)
  (define-key dired-mode-map (kbd "<right>") 'my-dired-scroll-down)
  (define-key dired-mode-map (kbd "ESC p") 'my-dired-scroll-up)
  (define-key dired-mode-map (kbd "ESC n") 'my-dired-scroll-down)

  (add-hook 'dired-mode-hook
            (lambda ()
              (when (fboundp 'dired-hide-details-mode)
                (dired-hide-details-mode t)
                (local-set-key "l" 'dired-hide-details-mode))
              (let ((file "._Icon\015"))
                (when nil
                  '(file-readable-p file)
                  (delete-file file)))))

  (when (autoload-eval-lazily 'pack '(dired-do-pack-or-unpack pack-pack)
          (add-to-list 'pack-program-alist
                       '("\\.txz\\'" :pack "tar -cJf" :unpack "tar -xf")))
    (set-variable 'pack-silence
                  t)
    (with-eval-after-load 'dired
      (define-key dired-mode-map "P" 'pack-dired-dwim)))

  (when (autoload-eval-lazily 'dired-list-all-mode)
    (setq dired-listing-switches "-lhF")
    (with-eval-after-load 'dired
      (define-key dired-mode-map "a" 'dired-list-all-mode))))

(when (autoload-eval-lazily 'dired-filter)
  (add-hook 'dired-mode-hook
            'dired-filter-mode))
(set-variable 'dired-filter-stack nil)

;; Currently disabled in favor of dired-from-git-ls-files
;; (define-key ctl-x-map "f" 'find-dired)


;; It works!
;; (pop-to-buffer (dired-noselect '("." "shrc" "emacs.el")))

(defun my-dired-git-ls-files (args)
  "Dired from git ls-files."
  (interactive "sgit ls-files args: ")
  (pop-to-buffer-same-window
   (dired-noselect `(,default-directory
                      ,@(split-string (shell-command-to-string (concat "git ls-files -z " args))
                                      "\0" t))
                   ""))
  )

(define-key ctl-x-map (kbd "f") 'my-dired-git-ls-files)
(with-eval-after-load 'dired
  (defvar dired-mode-map (make-sparse-keymap))
  (define-key dired-mode-map "G" 'my-dired-git-ls-files))

;; (define-minor-mode my-dired-glob-filter)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; misc funcs

(when (fboundp 'browse-url-default-macosx-browser)
  (defalias 'browse-osx 'browse-url-default-macosx-browser))

(defalias 'qcalc 'quick-calc)

(defun memo (&optional dir)
  "Open memo.txt in DIR."
  (interactive)
  (pop-to-buffer (find-file-noselect (concat (if dir
                                                 (file-name-as-directory dir)
                                               "")
                                             "memo.txt"))))

(set (defvar my-rgrep-alist nil
       "Alist of rgrep command.
Each element is in the form like (NAME SEXP COMMAND), where SEXP returns the
condition to choose COMMAND when evaluated.")
     `(
       ;; ripgrep
       ("rg"
        (executable-find "rg")
        "rg --hidden --no-heading --smart-case ")

       ;; git grep
       ("gitgrep"
        (eq 0
            (shell-command "git rev-parse --git-dir"))
        "git --no-pager -c color.grep=always grep -nH -e ")

       ;; sift
       ("sift"
        (executable-find "sift")
        ("sift --binary-skip --filename --line-number --git --smart-case "))

       ;; the silver searcher
       ("ag"
        (executable-find "ag")
        "ag --nogroup --nopager --filename ")

       ;; ack
       ("ack"
        (executable-find "ack")
        "ack --nogroup --nopager --with-filename ")

       ;; gnu global
       ("global"
        (and (require 'ggtags nil t)
             (executable-find "global")
             (ggtags-current-project-root))
        "global --result grep ")

       ;; grep
       ("grep"
        t
        ,(concat "find . "
                 "-path '*/.git' -prune -o "
                 "-path '*/.svn' -prune -o "
                 "-type f -print0 | "
                 "xargs -0 grep -nH -e "))
       )
     )

(defvar my-rgrep-default nil
  "Default command name for my-rgrep.")

(defun my-rgrep-grep-command (&optional name alist)
  "Return recursive grep command for current directory or nil.
If NAME is given, use that without testing.
Commands are searched from ALIST."
  (if alist
      (if name
          ;; if name is given search that from alist and return the command
          (nth 2 (assoc name
                        alist))

        ;; if name is not given try test in 1th elem
        (let ((car (car alist))
              (cdr (cdr alist)))

          (if (eval (nth 1 car))
              ;; if the condition is true return the command
              (nth 2 car)

            ;; try next one
            (and cdr
                 (my-rgrep-grep-command name cdr)))))

    ;; if alist is not given set default value
    (my-rgrep-grep-command name my-rgrep-alist)))

(defun my-rgrep (command-args)
  "My recursive grep.  Run COMMAND-ARGS.
If prefix argument is given, use current symbol as default search target
and search from projectile root (if projectile is available)."
  (interactive (let ((cmd (my-rgrep-grep-command my-rgrep-default
                                                 nil)))
                 (if cmd
                     (list (read-shell-command "grep command: "
                                               (concat cmd
                                                       (if current-prefix-arg
                                                           (thing-at-point 'symbol t)
                                                         ""))
                                               'grep-find-history))
                   (error "My-Rgrep: Command for rgrep not found")
                   )))
  (if (and current-prefix-arg
           (safe-require-or-eval 'projectile)
           (projectile-project-p))
      (projectile-with-default-dir (projectile-project-root)
                                   (compilation-start command-args
                                                      'grep-mode))
    (compilation-start command-args
                       'grep-mode)))

(defun my-rgrep-thing-at-point-projectile-root ()
  "My recursive grep to find thing at point from project root."
  (interactive)
  (let* ((cmd (my-rgrep-grep-command my-rgrep-default
                                     nil))
         (command-args
          (if cmd
              (concat cmd
                      (or (thing-at-point 'symbol t)
                          (error "No symbol at point")))
            (error "My-Rgrep: Command for rgrep not found"))))
    (if (safe-require-or-eval 'projectile)
        (projectile-with-default-dir (or (projectile-project-root)
                                         default-directory)
                                     (compilation-start command-args
                                                        'grep-mode))
      (compilation-start command-args
                         'grep-mode))))


(defmacro define-my-rgrep (name)
  "Define rgrep for NAME."
  `(defun ,(intern (concat "my-rgrep-"
                           name)) ()
     ,(format "My recursive grep by %s."
              name)
     (interactive)
     (let ((my-rgrep-default ,name))
       (if (called-interactively-p 'any)
           (call-interactively 'my-rgrep)
         (error "Not intended to be called noninteractively.  Use `my-rgrep'"))))
  )

(define-my-rgrep "ack")
(define-my-rgrep "ag")
(define-my-rgrep "rg")
(define-my-rgrep "sift")
(define-my-rgrep "gitgrep")
(define-my-rgrep "grep")
(define-my-rgrep "global")

(define-key ctl-x-map "s" 'my-rgrep)
(define-key ctl-x-map "." 'my-rgrep-thing-at-point-projectile-root)

(defun my-occur (regexp &optional region)
  "My occur command to search REGEXP."
  (interactive (list (read-string "List lines matching regexp: "
                                  (thing-at-point 'symbol t))))
  (occur regexp nil region))
(define-key ctl-x-map (kbd "C-o") 'my-occur)

(set-variable 'dumb-jump-prefer-searcher 'rg)

(defalias 'make 'compile)
(define-key ctl-x-map "c" 'compile)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; editorconfig-auto-apply

(define-minor-mode editorconfig-auto-apply-mode
  "When saving .editorconfig file update buffer configs."
  :global t  ;; TODO: global nil and instruct to hook to editorconfig-conf-mode-hook
  :lighter ""
  (if editorconfig-auto-apply-mode
      (add-hook 'after-save-hook
                'editorconfig-auto-apply-mode--run)
    (remove-hook 'after-save-hook
                 'editorconfig-auto-apply-mode--run)))

(defun editorconfig-auto-apply-mode--run ()
  "When saving .editorconfig file walk all buffers and update configs."
  (when (eq major-mode
            'editorconfig-conf-mode)
    (let ((dir (file-name-directory buffer-file-name)))
      (cl-dolist (buf (buffer-list))
        (when (and (buffer-file-name buf)
                   (file-in-directory-p (buffer-file-name buf)
                                        dir))
          (with-current-buffer buf
            (editorconfig-mode-apply)))))))

(editorconfig-auto-apply-mode 1)



(define-key ctl-x-map (kbd "C-r") 'recently-show)
(define-key ctl-x-map "T" 'git-worktree)
(define-key ctl-x-map "W" 'git-walktree)

;;;;;;;;;;;;;;;;
;; flychcek-black

(require 'flycheck)

(flycheck-define-checker python-black-check
  "A Python style checker."
  :command ("python3" "-m" "black"
            "--check"
            (config-file "--config" flycheck-black)
            source)
  :error-parser flycheck-parse-black-check
  ;; :error-patterns
  ;; (
  ;;  (error line-start "error: cannot format " (file-name) ": " (message) ": " line ":" column ": " (one-or-more any) line-end)
  ;;  (error line-start (message) " " (file-name) line-end)
  ;;  )
  :enabled (lambda ()
             (or (not (flycheck-python-needs-module-p 'python-black-check))
                 (flycheck-python-find-module 'python-black-check "black")))
  :verify (lambda (_) (flycheck-python-verify-module 'python-black-check "black"))
  :modes python-mode)

;; (flycheck-define-checker python-black-diff
;;   "A Python style checker."
;;   :command ("python3"
;;             "-m" "black"
;;             (config-file "--config" flycheck-black)
;;             "--diff" source)
;;   :error-parser my-flycheck-parse-unified-diff
;;   :enabled (lambda ()
;;              (or (not (flycheck-python-needs-module-p 'python-black))
;;                  (flycheck-python-find-module 'python-black "black")))
;;   :verify (lambda (_) (flycheck-python-verify-module 'python-black "black"))
;;   :modes python-mode)

(flycheck-def-config-file-var flycheck-black python-black-check "pyproject.toml"
  :safe #'stringp)

(add-to-list 'flycheck-checkers
             'python-black-check)

(defun flycheck-parse-black-check (output checker buffer)
  "Flycheck parser to check if reformat is required."
  (let ((result nil))
    (with-temp-buffer
      (insert output)
      (save-match-data
        (goto-char (point-min))
        (when (re-search-forward "^would reformat .*$" nil t)
          (add-to-list 'result (flycheck-error-new-at
                                (point-min)
                                nil
                                'error
                                ;;(format "Black: %s" (match-string 0))
                                "Black: would reformat"
                                :buffer buffer
                                :checker checker)))
        (goto-char (point-min))
        (when (re-search-forward "^error: .*$" nil t)
          (add-to-list 'result (flycheck-error-new-at
                                (point-min)
                                nil
                                'error
                                ;; Fix not to include absolute file path
                                (format "Black: %s" (match-string 0))
                                :buffer buffer
                                :checker checker)))))
    result))

(defun my-flycheck-parse-unified-diff (output checker buffer)
  "Flycheck parser to parse diff output."
  (let ((source-line 0)
        (result ())
        (hunk "HUNK"))
    (with-temp-buffer
      (insert output)
      (goto-char (point-min))
      (while (not (eq (point) (point-max)))
        ;; FIXME: Do not stop when no result
        (while (not (re-search-forward "^@@ -\\([0-9]+\\),\\([0-9]+\\) \\+\\([0-9]+\\),\\([0-9]+\\) @@.*$" (point-at-eol) t))
          (forward-line 1)
          (goto-char (point-at-bol)))
        (setq source-line
              (string-to-number (match-string 1)))
        ;; TODO: Add filename support
        (setq hunk
              (match-string 0))
        ;;(while (not (rearch-forward "^\\(-\\|\\+\\)")))
        )
      (add-to-list 'result
                   (flycheck-error-new-at
                    0
                    nil
                    'error
                    "MESSAGE"
                    :buffer buffer
                    :checker checker
                    :group hunk)))
    result))

(set-variable 'flycheck-python-mypy-ini
              ".mypy.ini")

;; Local Variables:
;; flycheck-disabled-checkers: (emacs-lisp-checkdoc)
;; flycheck-checker: emacs-lisp
;; End:

;;; emancs.el ends here

;;; emacs.el --- 10sr emacs initialization

;;; Code:

;; SETUP_LOAD: (load "bytecomp")  ;; Required for WSL environment
;; SETUP_LOAD: (let ((file "DOTFILES_DIR/emacs.el"))
;; SETUP_LOAD:   (and (file-readable-p file)
;; SETUP_LOAD:        (byte-recompile-file file nil 0 t)))

;; TODO: Use custom-set-variables in place of set-variable

(setq debug-on-error t)

;; make directories
(unless (file-directory-p (expand-file-name user-emacs-directory))
  (make-directory (expand-file-name user-emacs-directory)))

(require 'cl-lib)
(require 'simple)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Some macros for internals

(defmacro eval-after-init (&rest body)
  "If `after-init-hook' has been run, run BODY immediately.
Otherwize hook it."
  (declare (indent 0) (debug t))
  `(if after-init-time
       ;; Currently after-init-hook is run just after setting after-init-hook
       (progn
         ,@body)
     (add-hook 'after-init-hook
               (lambda ()
                 ,@body))))

;; TODO: Remove: Just checking fboundp should be enough for most cases
(defmacro safe-require-or-eval (feature)
  "Require FEATURE if available.

At compile time the feature will be loaded immediately."
  `(eval-and-compile
     (message "safe-require-or-eval: Trying to require %s" ,feature)
     (require ,feature nil t)))

;; TODO: Remove: Defining autoload is no more needed for most cases
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
       web-mode


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
       aggressive-indent
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
       fzf
       fic-mode
       term-cursor
       pydoc
       swoop
       color-identifiers-mode
       dired-k

       editorconfig
       editorconfig-custom-majormode

       git-command

       prompt-text

       ;; 10sr repository
       ;; 10sr-extras
       terminal-title
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
(setq initial-buffer-choice 'messages-buffer)
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

(eval-after-init
  (message "%s %s" invocation-name emacs-version)
  (message "Invocation directory: %s" default-directory)
  (message "%s was taken to initialize emacs." (emacs-init-time))
  ;; (view-echo-area-messages)
  ;; (view-emacs-news)
  )

(with-current-buffer "*Messages*"
  (emacs-lock-mode 'kill))

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
(global-set-key (kbd "C-^") 'my-prefix-map)
;; (define-key my-prefix-map (kbd "C-q") 'quoted-insert)
;; (define-key my-prefix-map (kbd "C-z") 'suspend-frame)

;; (comint-show-maximum-output)

;; kill scratch
(eval-after-init
  (let ((buf (get-buffer "*scratch*")))
    (when buf
      (kill-buffer buf))))

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

;; Interactively evaluate Emacs Lisp expressions
(define-key ctl-x-map "i" 'ielm)

(when (safe-require-or-eval 'which-key)
  (which-key-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; editor

;; Basically it should not set globally (instead use something like file local
;; variables or editorconfig), but for most cases I just need this...
(defun my-set-require-final-newline ()
  "Set `require-final-newline'."
  (set (make-local-variable 'require-final-newline)
       mode-require-final-newline))

(add-hook 'prog-mode-hook
          'my-set-require-final-newline)
(add-hook 'text-mode-hook
          'my-set-require-final-newline)
(add-hook 'conf-mode-hook
          'my-set-require-final-newline)

;; Used from term-cursor
;; hbar is too hard to find...
(defun my-cursor-type-change (&rest args)
  "ARGS are discarded."
  ;; TODO: Support wdired and wgrep
  (if buffer-read-only
      (setq cursor-type 'hbar)
    (setq cursor-type 'box)))
;; (add-hook 'switch-buffer-functions
;;           'my-cursor-type-change)
;; (add-hook 'read-only-mode-hook
;;           'my-cursor-type-change)
;; (when (fboundp 'global-term-cursor-mode)
;;   (global-term-cursor-mode 1))
;; ;; (term-cursor--eval)


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
;; (normal-erase-is-backspace-mode 1)

;;(global-set-key (kbd "C-m") 'reindent-then-newline-and-indent)
(global-set-key (kbd "C-m") 'newline-and-indent)
;; (global-set-key (kbd "C-o") (kbd "C-e C-m"))

;; (global-set-key "\C-z" 'undo) ; undo is M-u
(define-key esc-map "u" 'undo)
(define-key esc-map "i" (kbd "ESC TAB"))
;; (global-set-key (kbd "C-r") 'query-replace-regexp)
(global-set-key (kbd "C-s") 'isearch-forward-regexp)
(global-set-key (kbd "C-r") 'isearch-backward-regexp)

(when (fboundp 'undo-fu-only-undo)
  (global-set-key (kbd "C-_") 'undo-fu-only-undo))
(when (fboundp 'undo-fu-only-redo)
  (global-set-key (kbd "C-M-_") 'undo-fu-only-redo))

(require 'page-ext nil t)

(when (safe-require-or-eval 'page-break-lines)
  (global-page-break-lines-mode 1))

(when (safe-require-or-eval 'git-gutter)
  (declare-function global-git-gutter-mode "git-gutter")
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
      '(:eval (format ":%%l:%%c /%d%s"
                      (count-lines (point-max)
                                   (point-min))
                      (if (buffer-narrowed-p)
                          "[N]"
                        "")
                      )))

(when (safe-require-or-eval 'diminish)
  ;; FIXME: Eval after enabling mode
  (eval-after-init
    (diminish 'recently-mode)
    (diminish 'editorconfig-mode)
    (diminish 'auto-highlight-symbol-mode)
    (diminish 'global-whitespace-mode)
    (diminish 'which-key-mode)
    (diminish 'page-break-lines-mode)
    (diminish 'highlight-indentation-mode)
    (diminish 'color-identifiers-mode)
    ))

(setq mode-line-front-space "")
;; (setq mode-line-end-spaces "")
;; Set current frame name to empty string
(setq-default mode-line-format
              (let* ((l mode-line-format)
                     (l (cl-substitute " " "   "
                                       l
                                       :test 'equal))
                     (l (cl-substitute " " "  "
                                       l
                                       :test 'equal))
                     )
                l))

(set-frame-parameter nil 'name "")

;; See color-name-rgb-alist for available color names
;; http://www.raebear.net/computers/emacs-colors/
;; https://www.emacswiki.org/emacs/ListColors
;; (list-colors-display is not a complete list)
(defconst my-mode-line-background-default
  (face-background 'mode-line)
  "Default color of mode-line at init.")
(defun my-mode-line-color-update (&rest args)
  "ARGS are discarded"
  (let ((ro "skyblue")
        (rw my-mode-line-background-default))
    (if (or (not buffer-read-only)
            (and (eq major-mode 'wdired-mode)))
        (set-face-background 'mode-line
                             rw)
      (set-face-background 'mode-line
                           ro))))
(add-hook 'switch-buffer-functions
          'my-mode-line-color-update)
(add-hook 'read-only-mode-hook
          'my-mode-line-color-update)
(add-hook 'wdired-mode-hook
          'my-mode-line-color-update)
(advice-add 'wdired-change-to-dired-mode
            :after
            'my-mode-line-color-update)
(advice-add 'wgrep-change-to-wgrep-mode
            :after
            'my-mode-line-color-update)
(advice-add 'wgrep-to-original-mode
            :after
            'my-mode-line-color-update)

(set-face-background 'header-line
                     my-mode-line-background-default)

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

(autoload-eval-lazily 'helm nil
  (defvar helm-map)
  (define-key helm-map (kbd "C-h") (kbd "DEL")))

(setq-default header-line-format
              '(:eval (let ((f (or (buffer-file-name)
                                   default-directory)))
                        (when f
                          (abbreviate-file-name f)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; letters, font-lock mode and fonts

(when (fboundp 'color-identifiers-mode)
  (add-hook 'prog-mode-hook
            'color-identifiers-mode))

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
        ))

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
               `(tab-mark ?\t ,(vconcat "|>\t")) t)
  ;; (add-to-list 'whitespace-display-mappings
  ;;              `(newline-mark ?\n ,(vconcat "$\n")))
  (setq whitespace-style '(face
                           trailing     ; trailing blanks
                           ;; tabs
                           ;; spaces
                           ;; lines
                           lines-tail  ; lines over 80
                           newline      ; newlines
                           ;; empty        ; empty lines at beg or end of buffer
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

(defun my-gen-hl-line-color-dark ()
  "Generate color for current line in black background."
  (let* ((candidates (mapcar 'number-to-string (number-sequence 1 6)))
         (limit (length candidates))
         (r 0) (g 0) (b 0))
    (while (and (<= (abs (- r g)) 1)
                (<= (abs (- g b)) 1)
                (<= (abs (- b r)) 1))
      (setq r (random limit))
      (setq g (random limit))
      (setq b (random limit)))
    (format "#%s%s%s"
            (nth r candidates)
            (nth g candidates)
            (nth b candidates)
            )))
;; (my-gen-hl-line-color-dark)

;; highlight current line
;; http://wiki.riywo.com/index.php?Meadow
(face-spec-set 'hl-line
               `((((min-colors 256)
                   (background dark))
                  ;; Rotate midnightblue
                  (:background ,(my-gen-hl-line-color-dark)))
                 (((min-colors 256)
                   (background light))
                  ;; TODO: What is should be?
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
             text-mode-hook
             ))
    (add-hook hook
              'highlight-indentation-mode)))
;; (set-face-background 'highlight-indentation-current-column-face "#c3b3b3")

(when (fboundp 'fic-mode)
  (add-hook 'prog-mode-hook
            'fic-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; file handling

(auto-insert-mode 1)

;; fzf

;; Too slow in term buffer!
;; (set-variable 'fzf/executable "sk")
;; (set-variable 'fzf/args "--color bw --print-query")
;; Modified from hardcoded default to include:
;;  - directories
;;  - hidden files
;;  - root directory (.)
;;  - parent directory (..)
;; ripgrep cannot list directories...
;; (setenv "FZF_DEFAULT_COMMAND" "rg --files --hidden --follow --glob '!.git/*' --no-ignore")
(let* ((find (if (executable-find "bfs")
                 ;; Breadth-first find https://github.com/tavianator/bfs
                 "bfs"
               ;; Use gfind if available?
               "find"))
       (findcmd (concat "set -eu; set -o pipefail; "
                        "echo .; "
                        "echo ..; "
                        "command " find " -L . "
                        "-mindepth 1 "
                        "\\( -fstype 'sysfs' -o -fstype 'devfs' -o -fstype 'devtmpfs' -o -fstype 'proc' \\) -prune "
                        "-o -print "
                        "2> /dev/null "
                        "| "
                        "cut -b3-"))
       (fdcmd (concat "set -eu; set -o pipefail; "
                      "echo .; "
                      "echo ..; "
                      "command fd "
                      "--follow --hidden --no-ignore "
                      "--color always "
                      "2>/dev/null")))
  (if (executable-find "fd")
      (setenv "FZF_DEFAULT_COMMAND" fdcmd)
    (setenv "FZF_DEFAULT_COMMAND" findcmd)))
(set-variable 'fzf/window-height 45)
(set-variable 'fzf/args "--print-query --ansi --color='bg+:-1' --inline-info --cycle")
;; (set-variable 'fzf/args "--print-query --ansi --inline-info --cycle")
;; (set-variable 'fzf/args "--print-query --ansi --color=bw --inline-info --cycle")

(defun my-fzf-or-find-file ()
  "Call fzf if usable or call find-file."
  (declare (interactive-only t))
  (interactive)
  (if (and (executable-find "fzf")
           (fboundp 'fzf)
           (not (file-remote-p default-directory)))
      (fzf)
    (call-interactively 'find-file)))
(define-key ctl-x-map "f" 'my-fzf-or-find-file)

(defun my-fzf-all-lines ()
  "Fzf all lines."
  (interactive)
  (let ((process-environment (cl-copy-list process-environment)))
    (setenv "FZF_DEFAULT_COMMAND" "rg -nH --no-heading --hidden --follow --glob '!.git/*' --color=always ^")
    (fzf)))
(define-key ctl-x-map "S" 'my-fzf-all-lines)

;; recently

(when (safe-require-or-eval 'recently)
  (define-key ctl-x-map (kbd "C-r") 'recently-show)
  (set-variable 'recently-max 1000)
  (defvar recently-excludes)
  (add-to-list 'recently-excludes
               (rx-to-string (list 'and
                                   'string-start
                                   (expand-file-name package-user-dir))
                             t))
  (recently-mode 1))

(when (safe-require-or-eval 'editorconfig)
  (set-variable 'editorconfig-get-properties-function
                'editorconfig-core-get-properties-hash)
  (editorconfig-mode 1)
  (set-variable 'editorconfig-mode-lighter "")
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
               (puthash 'indent_style "tab" props))
             (when (derived-mode-p 'diff-mode)
               (puthash 'trim_trailing_whitespace "false" props)
               (puthash 'insert_final_newline "false" props)
               )
             ))

(when (fboundp 'editorconfig-auto-apply-enable)
  (add-hook 'editorconfig-conf-mode-hook
            'editorconfig-auto-apply-enable))



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

;; (add-to-list 'auto-save-file-name-transforms
;;              `(".*" ,(concat user-emacs-directory "auto-save-dir") t))
;; (setq auto-save-interval 3)
;; (auto-save-mode 1)

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

;; Derived from https://github.com/ensime/ensime-emacs/issues/591#issuecomment-291916753
(defun my-scalafmt ()
  (interactive)
  (cl-assert buffer-file-name)
  (cl-assert (not (buffer-modified-p)))
  (let* ((configdir (locate-dominating-file default-directory ".scalafmt.conf"))
         (configoption (if configdir
                           (concat " --config "
                                   (shell-quote-argument (expand-file-name configdir))
                                   ".scalafmt.conf"
                                   )
                         ""))
         (str (concat "scalafmt -f "
                      (shell-quote-argument buffer-file-name)
                      configoption
                      " -i --exclude ensime")))
    (message str)
    (shell-command-to-string str))
  (message "scalafmt done")
  (revert-buffer nil t))

(when (fboundp 'web-mode)
  (add-to-list 'auto-mode-alist
               '("\\.html\\.j2\\'" . web-mode))
  (add-to-list 'auto-mode-alist
               ;; Django Template Language
               '("\\.dtl\\'" . web-mode))
  )

(when (autoload-eval-lazily 'wgrep)
  (set-variable 'wgrep-auto-save-buffer t)
  (with-eval-after-load 'grep
    (defvar grep-mode-map)
    (define-key grep-mode-map
      "e"
      'wgrep-change-to-wgrep-mode)))

(when (fboundp 'grep-context-mode)
  (add-hook 'compilation-mode-hook #'grep-context-mode))

(with-eval-after-load 'remember
  (defvar remember-mode-map)
  (define-key remember-mode-map (kbd "C-x C-s") 'ignore))
(set-variable 'remember-notes-initial-major-mode
              'change-log-mode)

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

;; Sane colors
(with-eval-after-load 'magit-diff
  (set-face-background 'magit-diff-context nil)
  (set-face-background 'magit-diff-context-highlight nil)

  (set-face-foreground 'magit-diff-hunk-heading nil)
  (set-face-background 'magit-diff-hunk-heading nil)
  (set-face-foreground 'magit-diff-hunk-heading-highlight nil)
  (set-face-background 'magit-diff-hunk-heading-highlight nil)

  ;; https://blog.shibayu36.org/entry/2016/03/27/220552
  (set-face-foreground 'magit-diff-added "green")
  (set-face-background 'magit-diff-added nil)
  (set-face-foreground 'magit-diff-added-highlight "green")
  (set-face-background 'magit-diff-added-highlight nil)
  (set-face-foreground 'magit-diff-removed "red")
  (set-face-background 'magit-diff-removed nil)
  (set-face-foreground 'magit-diff-removed-highlight "red")
  (set-face-background 'magit-diff-removed-highlight nil)
  (set-face-background 'magit-diff-lines-boundary "blue")
  )

(defun my-magit-messenger (file line)
  "Magit messenger."
  (interactive (list buffer-file-name
                     (line-number-at-pos)))
  (cl-assert file)
  (cl-assert line)
  (let* ((blame-args '("-w"))
         (id (with-temp-buffer
               (let ((exit (apply 'call-process
                                  "git"  ;; PROGRAM
                                  nil  ;; INFILE
                                  t  ;; DESTINATION
                                  nil  ;; DISPLAY
                                  "--no-pager"  ;; ARGS
                                  "blame"
                                  "-L"
                                  (format "%d,+1" line)
                                  "--porcelain"
                                  file
                                  blame-args
                                  )))
                 (goto-char (point-min))
                 (cl-assert (eq exit 0)
                            "Failed: %s" (buffer-substring (point)
                                                           (point-at-eol)))
                 (save-match-data
                   (re-search-forward (rx buffer-start
                                          (one-or-more hex-digit)))
                   (match-string 0))))))
    (magit-show-commit id)))


(when (boundp 'git-rebase-filename-regexp)
  (add-to-list 'auto-mode-alist
               `(,git-rebase-filename-regexp . text-mode)))

(when (safe-require-or-eval 'aggressive-indent)
  (defvar aggressive-indent-excluded-modes)
  (setq aggressive-indent-excluded-modes
        `(web-mode
          diff-mode
          toml-mode
          conf-mode
          dockerfile-mode
          groovy-mode
          scala-mode
          ,@aggressive-indent-excluded-modes))
  (global-aggressive-indent-mode 1))

(when (fboundp 'ggtags-mode)
  (add-hook 'c-mode-common-hook
            'ggtags-mode)
  (add-hook 'python-mode-hook
            'ggtags-mode)
  (add-hook 'js-mode-hook
            'ggtags-mode)
  (add-hook 'scheme-mode-hook
            'ggtags-mode)
  )

(when (fboundp 'imenu-list-minor-mode)
  (defvar imenu-list-buffer-name)
  (defun my-imenu-list-toggle ()
    "My 'imenu-list` toggle."
    (interactive)
    (require 'imenu-list)
    (if (eq (window-buffer)
            (get-buffer imenu-list-buffer-name))
        (imenu-list-minor-mode -1)
      (imenu-list-minor-mode 1)))
  ;; (set-variable 'imenu-list-auto-resize t)
  (set-variable 'imenu-list-focus-after-activation t)
  (define-key ctl-x-map (kbd "C-l") 'my-imenu-list-toggle))

(add-hook 'emacs-lisp-mode-hook
          (lambda ()
            (setq imenu-generic-expression
                  `(("Sections" ";;;\+\n;; \\(.*\\)\n" 1)
                    ,@imenu-generic-expression))))
;; TODO: Try paraedit http://daregada.blogspot.com/2012/03/paredit.html

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
               '("^\\([^ \n]+\\):\\([0-9]+\\)$" 1 2))
  (add-to-list 'compilation-error-regexp-alist
               ;; pydocstyle
               '("^\\([^ \n]+\\):\\([0-9]+\\) " 1 2))
  )

(when (safe-require-or-eval 'company)
  ;; http://qiita.com/sune2/items/b73037f9e85962f5afb7
  ;; https://qiita.com/yuze/items/a145b1e3edb6d0c24cbf
  (global-company-mode)
  (set-variable 'company-idle-delay nil)
  (set-variable 'company-minimum-prefix-length 2)
  (set-variable 'company-selection-wrap-around t)

  (defvar company-mode-map)
  (define-key company-mode-map (kbd "C-i") 'company-indent-or-complete-common)
  ;; (with-eval-after-load 'python
  ;;   (defvar python-indent-trigger-commands)
  ;;   ;; TODO: This disables completion in puthon?
  ;;   (add-to-list 'python-indent-trigger-commands
  ;;                'company-indent-or-complete-common))
  (define-key ctl-x-map (kbd "C-i") 'company-complete)  ; Originally `indent-rigidly'

  (defvar company-active-map)
  (define-key company-active-map (kbd "C-n") 'company-select-next)
  (define-key company-active-map (kbd "C-p") 'company-select-previous)
  (define-key company-active-map (kbd "C-s") 'company-filter-candidates)
  (define-key company-active-map (kbd "C-i") 'company-complete-selection)
  (define-key company-active-map (kbd "C-f") 'company-complete-selection)

  (defvar company-mode)
  (defvar company-candidates)
  (defvar company-candidates-length)
  ;; (popup-tip "Hello, World!")
  (defun my-company-lighter-current-length ()
    "Get current candidate length."
    (interactive)
    (let ((l nil)
          (inhibit-message t))
      (when (and company-mode
                 (not (minibufferp))
                 ;; Do nothing when already in company completion
                 (not company-candidates))
        ;; FIXME: Somehow it cannto catch errors from ggtags
        (ignore-errors
          ;; (company-auto-begin)
          (company-manual-begin)
          (setq l company-candidates-length)
          (company-cancel)))
      (if l
          (format "[%d]" l)
        "")))

  (defvar company-lighter)
  (set-variable 'company-lighter-base "Cmp")
  ;; (add-to-list 'company-lighter
  ;;              '(:eval (my-company-lighter-current-length))
  ;;              t)

  ;; This breaks japanese text input
  ;; (set-variable 'my-company-length-popup-tip-timer
  ;;               (run-with-idle-timer 0.2 t
  ;;                                    'my-company-length-popup-tip))

  ;; (current-active-maps)
  ;; (lookup-key)
  '(mapcar (lambda (map)
             (lookup-key map (kbd "C-i")))
           (current-active-maps))

  ;; https://qiita.com/syohex/items/8d21d7422f14e9b53b17
  (set-face-attribute 'company-tooltip nil
                      :foreground "black" :background "lightgrey")
  (set-face-attribute 'company-tooltip-common nil
                      :foreground "black" :background "lightgrey")
  (set-face-attribute 'company-tooltip-common-selection nil
                      :foreground "white" :background "steelblue")
  (set-face-attribute 'company-tooltip-selection nil
                      :foreground "black" :background "steelblue")
  (set-face-attribute 'company-preview-common nil
                      :background nil :foreground "lightgrey" :underline t)
  (set-face-attribute 'company-scrollbar-fg nil
                      :background "orange")
  (set-face-attribute 'company-scrollbar-bg nil
                      :background "gray40")

  )

;; https://github.com/lunaryorn/flycheck
;; TODO: Any way to disable auto check?
;; Update flycheck-hooks-alist?
(when (safe-require-or-eval 'flycheck)
  (eval-after-init (global-flycheck-mode)))
;; (set-variable 'flycheck-display-errors-delay 2.0)
;; (fset 'flycheck-display-error-at-point-soon 'ignore)

;; (with-eval-after-load 'flycheck
;;   (when (fboundp 'flycheck-black-check-setup)
;;     (flycheck-black-check-setup)))

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
;; I want to use this, but this breaks normal self-insert-command
;; (set-variable 'py-indent-list-style
;;               'one-level-to-beginning-of-statement)
(set-variable 'pydoc-command
              "python3 -m pydoc")
(with-eval-after-load 'pydoc
  (when (require 'with-venv nil t)
    (with-venv-advice-add 'pydoc)))


(when (autoload-eval-lazily 'pipenv)
  ;; (declare-function pipenv-projectile-after-switch-default "pipenv")
  ;; (add-hook 'python-mode-hook
  ;;           (lambda ()
  ;;             (pipenv-mode 1)
  ;;             (pipenv-projectile-after-switch-default)))
  )

(set-variable 'flycheck-python-mypy-ini ".mypy.ini")
(set-variable 'flycheck-flake8rc "setup.cfg")

(set-variable 'flycheck-python-pylint-executable "python3")
(set-variable 'flycheck-python-pycompile-executable "python3")

(set-variable 'python-indent-guess-indent-offset nil)

(with-eval-after-load 'blacken
  (when (require 'with-venv nil t)
    (with-venv-advice-add 'blacken-buffer)))

;; isortify-buffer breaks buffer when it contains japanese text
(defun my-isortify ()
  (interactive)
  (cl-assert buffer-file-name)
  (cl-assert (not (buffer-modified-p)))
  (with-venv
    (call-process "python" ;; PROGRAM
                  nil  ;; INFILE
                  nil  ;; DESTINATION
                  nil  ;; DISPLAY
                  "-m" "isort" buffer-file-name))
  (message "isortify done")
  (revert-buffer nil t))

;; https://github.com/lunaryorn/old-emacs-configuration/blob/master/lisp/flycheck-virtualenv.el
(defun my-set-venv-flycheck-executable-find ()
  "Set flycheck executabie find."
  (interactive)
  (set-variable 'flycheck-executable-find
                '(lambda (e)
                   (with-venv
                     (executable-find e)))
                t))

(defun my-update-flycheck-flake8-error-level-alist ()
  "Update `flycheck-flake8-error-level-alist'."
  (defvar flycheck-flake8-error-level-alist)
  ;; (add-to-list 'flycheck-flake8-error-level-alist
  ;;              '("^D.*$" . warning))
  (set-variable 'flycheck-flake8-error-level-alist
                nil)
  )

(add-hook 'python-mode-hook
          'my-set-venv-flycheck-executable-find)
(add-hook 'python-mode-hook
          'my-update-flycheck-flake8-error-level-alist)
;; Run multiple chekcers
;; https://github.com/flycheck/flycheck/issues/186


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

;; This keybind cause unexpected call really many many times
;; (when (autoload-eval-lazily 'gited)
;;   (define-key ctl-x-map (kbd "C-g") 'gited-list))
(defalias 'gited 'gited-list)

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
;;   (eval-after-init (activate-malabar-mode)))

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
(autoload 'diff-goto-source "diff-mode")
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
        (define-key gfm-mode-map (kbd "C-m") 'electric-indent-just-newline)
        (define-key gfm-mode-map "`" nil)  ;; markdown-electric-backquote
        )
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
            (set-variable 'hl-line-range-function (lambda () '(0 . 0)) t)
            (set-variable 'scroll-margin 0 t)
            ))
(set-variable 'term-buffer-maximum-size 20480)
(set-variable 'term-suppress-hard-newline t)

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
(declare-function bs-set-configuration "bs")
(declare-function bs-refresh "bs")
(declare-function bs-message-without-log "bs")
(declare-function bs--current-config-message "bs")
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
(defun  my-force-query-kill-current-buffer ()
  "Interactively kill current buffer."
  (interactive)
  (when (y-or-n-p (concat "kill current buffer? :"))
    (let ((kill-buffer-hook nil)
          (kill-buffer-query-functions nil))
      (kill-buffer (current-buffer)))))
;;(global-set-key "\C-xk" 'my-query-kill-current-buffer)

;; Originally C-x C-k -> kmacro-keymap
;; (global-set-key "\C-x\C-k" 'kmacro-keymap)
(global-set-key (kbd "C-x C-k") 'my-query-kill-current-buffer)
(substitute-key-definition 'kill-buffer
                           'my-query-kill-current-buffer
                           global-map)

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
  (let* ((file (dired-get-filename t))
         (quoted (shell-quote-argument file)))
    (if (file-directory-p file)
        (progn
          (message "Calculating disk usage...")
          (let ((du (or (executable-find "gdu")
                        (executable-find "du")
                        (error "du not found"))))
            (shell-command (concat du
                                   " -hsD "
                                   quoted))))
      (shell-command (concat "file "
                             quoted)))))

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
  ;; (define-key dired-mode-map "f" 'find-file)
  (define-key dired-mode-map "f" 'my-fzf-or-find-file)
  (define-key dired-mode-map "z" 'fzf)
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

  (when (fboundp 'pack-dired-dwim)
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


(defvar my-dired-git-ls-files-history
  "History for `my-dired-git-ls-files'." nil)
(defun my-dired-git-ls-files (arg)
  "Dired from git ls-files."
  (interactive (list
                (read-shell-command "git ls-files: "
                                    "git ls-files -z ")))
  (pop-to-buffer-same-window
   (dired-noselect `(,default-directory
                      ,@(split-string (shell-command-to-string arg)
                                      "\0" t))
                   ""))
  )

(define-key ctl-x-map (kbd "G") 'my-dired-git-ls-files)
(with-eval-after-load 'dired
  (defvar dired-mode-map (make-sparse-keymap))
  (define-key dired-mode-map "G" 'my-dired-git-ls-files))

(with-eval-after-load 'pack
  (set-variable 'pack-silence
                t)
  (defvar pack-program-alist)
  (add-to-list 'pack-program-alist
               '("\\.txz\\'" :pack "tar -cJf" :unpack "tar -xf"))

  (when (executable-find "aunpack")
    (add-to-list 'pack-program-alist
                 ' ("\\.zip\\'"
                    :pack ("zip" "-r" archive sources)
                    :pack-append ("zip" "-r" archive sources)
                    :unpack ("aunpack" archive))))
  )


;; (define-minor-mode my-dired-glob-filter)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; misc funcs

(define-key ctl-x-map "T" 'git-worktree)
(define-key ctl-x-map "W" 'git-walktree)

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

;; TODO: remember-projectile

(set (defvar my-privnotes-path nil
       "My privnotes repository path.")
     (expand-file-name "~/my/privnotes"))

(defun my-privnotes-readme (dir)
  "Open my privnotes DIR."
  (interactive (list
                (read-file-name "Privnotes: "
                                (expand-file-name (format-time-string "%Y%m%d_")
                                                  my-privnotes-path))))
  (let ((path (expand-file-name "README.md" dir)))
    (with-current-buffer (find-file path)
      (unless (file-exists-p path)
        (insert (file-name-base dir)
                "\n"
                "=======\n"
                "\n\n")))))
(define-key ctl-x-map "p" 'my-privnotes-readme)

(set (defvar my-rgrep-alist nil
       "Alist of rgrep command.
Each element is in the form like (NAME SEXP COMMAND), where SEXP returns the
condition to choose COMMAND when evaluated.")
     `(
       ;; ripgrep
       ("rg"
        (executable-find "rg")
        "rg -nH --no-heading --hidden --glob '!.git/' --smart-case -M 1280 ")

       ;; git grep
       ("gitgrep"
        (eq 0
            (shell-command "git rev-parse --git-dir"))
        "git --no-pager grep -nH -e ")

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

(defun my-pushbullet-note (text &optional title)
  "Push TEXT."
  (interactive "sText to Push: ")
  (pb/push-item '("") text "note" (or title "")))


;;;;;;;;;;;;;;;;;;;;;
;; git-bug

(defconst git-bug-ls-regexp
  (eval-when-compile
    (rx bol
        (submatch (one-or-more alphanumeric))  ; id
        ;; (one-or-more any)
        (one-or-more space)
        (submatch (or "open" "close"))  ; status
        (one-or-more space)
        (submatch (maximal-match (zero-or-more print)))  ; title
        "\t"
        (submatch (one-or-more alphanumeric))  ; user
        (one-or-more space)
        "C:"
        (submatch (one-or-more digit))  ; Comment num
        (one-or-more space)
        "L:"
        (submatch (one-or-more digit))  ; Label num
        eol
        ))
  "Regexp to parse line of output of git-bug ls.
Used by `git-bug-ls'.")

(defun git-bug-bugs ()
  "Get list of git-bug bugs."
  (with-temp-buffer
    (git-bug--call-process "bug" "ls")
    (goto-char (point-min))
    (let ((bugs nil))
      (while (not (eq (point) (point-max)))
        (save-match-data
          (when (re-search-forward git-bug-ls-regexp (point-at-eol) t)
            (setq bugs `(,@bugs
                         ,(list
                           :id (match-string 1)
                           :status (match-string 2)
                           :title (string-trim (match-string 3))
                           :user (match-string 4)
                           :comment-num (match-string 5)
                           :label-num (match-string 6)
                           )))))
        (forward-line 1)
        (goto-char (point-at-bol)))
      bugs)))

(defun git-bug-ls ()
  "Open and select git bug list buffer."
  (interactive)
  (pop-to-buffer (git-bug-ls-noselect)))

(defun git-bug-ls-noselect (&optional directory)
  "Open git bug list buffer.

If optional arg DIRECTORY is given change current directory to there before
initializing."
  (setq directory (expand-file-name (or directory
                                        default-directory)))
  (cl-assert (file-directory-p directory))
  (let* ((root (git-bug--get-repository-root directory))
         (name (file-name-nondirectory root))
         (bname (format "*GitBug<%s>*" name)))
    (with-current-buffer (get-buffer-create bname)
      (cd root)
      (git-bug-ls--set-tabulated-list-mode-variables)
      (git-bug-ls-mode)
      (current-buffer))))

(defun git-bug--get-repository-root (dir)
  "Resolve repository root of DIR.

If DIR is not inside of any git repository, signal an error."
  (cl-assert (file-directory-p dir))
  (with-temp-buffer
    (cd dir)
    (git-bug--call-process "rev-parse" "--show-toplevel")
    (goto-char (point-min))
    (buffer-substring-no-properties (point-at-bol) (point-at-eol))))

(defun git-bug--call-process (&rest args)
  "Start git process synchronously with ARGS.

Raise error when git process ends with non-zero status.
Any output will be written to current buffer."
  (let ((status (apply 'call-process
                       "git"
                       nil
                       t
                       nil
                       args)))
    (cl-assert (eq status 0)
               nil
               (buffer-substring-no-properties (point-min) (point-max)))))


;;;;;;;;;;;;;;;;;;;
;; peek-file-mode

(defvar peek-file-buffers
  ()
  "Peek buffers.")

(defun peek-file (file)
  "Peek FILE."
  (interactive "fFile to peek: ")
  (with-current-buffer (find-file file)
    (peek-file-mode)))

(define-minor-mode peek-file-mode
  "Peek file mode."
  :lighter "PK"
  (view-mode peek-file-mode)
  (add-to-list 'peek-file-buffers
               (current-buffer))
  (add-hook 'switch-buffer-functions
            'peek-file-remove-buffers))

(defun peek-file-remove-buffers (&args _)
  "Remove peek file buffers."
  (cl-dolist (buf (cl-copy-list peek-file-buffers))
    (unless (get-buffer-window buf t)
      (setq peek-file-buffers
            (delq buf
                  peek-file-buffers))
      (with-current-buffer buf
        (when peek-file-mode
          (kill-buffer))))))


;;;;;;;;;;;;;;;;;;;;
;; remember-projectile

;; TODO: Add global-minor-mode
(defvar remember-data-file)
(defun my-set-remember-data-file-buffer-local ()
  "Set `remember-data-file'."
  (when (require 'projectile nil t)
    (setq-local remember-data-file
                (expand-file-name ".remember.notes"
                                  (projectile-project-root)))))

(add-hook 'after-change-major-mode-hook
          'my-set-remember-data-file-buffer-local)

(define-key ctl-x-map "R" 'remember)


;; ivy

(defvar ivy-re-builders-alist)
(set-variable 'ivy-re-builders-alist
              '((t . (lambda (s)
                       ;; Ignore whitespace
                       (ivy--regex-fuzzy (replace-regexp-in-string (rx space)
                                                                   ""
                                                                   s))))))

(with-eval-after-load 'ivy
  (defvar ivy-minibuffer-map)
  (define-key ivy-minibuffer-map (kbd "C-u")
    (lambda () (interactive) (delete-region (point-at-bol) (point)))))

(when (fboundp 'counsel-M-x)
  (define-key esc-map "x" 'counsel-M-x)
  ;; (counsel-mode 1)
  ;; counsel-fzf executes fzf -f QUERY for each input
  ;; (define-key ctl-x-map "f"
  ;;   (lambda ()
  ;;     (interactive
  ;;      (let ((process-environment (cl-copy-list process-environment)))
  ;;        (setenv "FZF_DEFAULT_COMMAND" nil)
  ;;        (counsel-fzf)))))
  )

(when (and (fboundp 'ivy-read)
           (locate-library "counsel"))

  (defun my-counsel-describe-symbol ()
    "Forwaord to `describe-symbol'."
    (interactive)
    (eval-and-compile (require 'help-mode))  ;; describe-symbol-backends
    (eval-and-compile (require 'counsel))
    (ivy-read "Describe symbol: " obarray
              ;; From describe-symbol definition
              :predicate (lambda (vv)
                           (cl-some (lambda (x) (funcall (nth 1 x) vv))
                                    describe-symbol-backends))
              :require-match t
              :history 'counsel-describe-symbol-history
              :keymap counsel-describe-map
              :preselect (ivy-thing-at-point)
              :action (lambda (x)
                        (describe-symbol (intern x)))
              :caller 'my-counsel-describe-symbol))

  (define-key help-map "o" 'my-counsel-describe-symbol)
  )

(with-eval-after-load 'ivy
  (ivy-configure 'my-counsel-describe-symbol
    :sort-fn #'ivy-string<)
  )


(when (fboundp 'counsel-imenu)
  (define-key ctl-x-map "l" 'counsel-imenu))

(when (fboundp 'swoop)
  ;; (global-set-key (kbd "C-s") 'swoop)
  ;; (global-set-key (kbd "C-r") 'swoop)
  ;; (define-key esc-map (kbd "C-s") 'swoop-multi)
  (define-key esc-map (kbd "C-s") 'swoop)
  (with-eval-after-load 'swoop-lib
    (defvar swoop-map)
    (define-key swoop-map (kbd "C-s") 'swoop-action-goto-line-next)
    (define-key swoop-map (kbd "C-r") 'swoop-action-goto-line-prev)
    )
  ;; TODO: case sensitive swoop
  ;; Wrap swoop-async-get-match-lines-list?
  ;; (with-eval-after-load 'swoop-lib
  ;;   (defun my-swoop-advice-smart-case (orig-func &rest args)
  ;;     "Function to wrap swoop function."
  ;;     (message "args: %S" args)
  ;;     (let* (
  ;;            (query (car args))
  ;;            ;; (query (plist-get args
  ;;            ;;                   :$query))
  ;;            (case-fold-search
  ;;             (let ((case-fold-search nil))
  ;;               (not (string-match-p (rx upper) query))))
  ;;            )
  ;;       ;; (message "case-fold-search: %S" case-fold-search)
  ;;       ;; (message "query: %S" query)
  ;;       (apply orig-func args)))
  ;;   (advice-add 'swoop-async-get-match-lines-list
  ;;               :around
  ;;               'my-swoop-advice-smart-case)
  ;;   (set-variable 'swoop-async-get-match-lines-list
  ;;                 (byte-compile 'swoop-async-get-match-lines-list))
  ;;   )

  )

;; なんかよくわからないけど頻繁に index.lock を残してしまう
(when (fboundp 'dired-k)
  (set-variable 'dired-k-style 'git)

  ;; What is the best way of doing this?
  (with-eval-after-load 'dired-k
    (fset 'dired-k--highlight-by-file-attribyte 'ignore))
  ;; (set-variable 'dired-k-size-colors
  ;;               `((,most-positive-fixnum)))
  ;; (set-variable 'dired-k-date-colors
  ;;               `((,most-positive-fixnum)))

  ;; always execute dired-k when dired buffer is opened and reverted
  ;; (add-hook 'dired-after-readin-hook #'dired-k-no-revert)

  ;; This causes:
  ;; fatal: Unable to create '.git/index.lock': File exist.s
  ;; (add-hook 'switch-buffer-functions
  ;;           (lambda (prev cur)
  ;;             (when (derived-mode-p 'dired-mode)
  ;;               (dired-k-no-revert))))
  )

(add-hook 'python-mode-hook
          (lambda ()
            ;; Currently on python-mode eldoc-mode sometimes print
            ;; wired message on "from" keyword:
            ;;var from = require("./from")
            (eldoc-mode -1)))

;; ?
(define-key input-decode-map "\e[1;5C" [C-right])
(define-key input-decode-map "\e[1;5D" [C-left])

;; mozc
(when (locate-library "mozc")
  ;; https://tottoto.net/mac-emacs-karabiner-elements-japanese-input-method-config/
  (with-eval-after-load 'mozc
    ;; (define-key mozc-mode-map (kbd "C-h") 'backward-delete-char)
    ;; (define-key mozc-mode-map (kbd "C-p") (kbd "<up>"))
    ;; (define-key mozc-mode-map (kbd "C-n") (kbd "SPC"))
    )
  ;; (setq default-input-method "japanese-mozc")
  (custom-set-variables '(mozc-leim-title "あ"))
  (defun turn-on-input-method ()
    (interactive)
    (activate-input-method default-input-method))
  (defun turn-off-input-method ()
    (interactive)
    (deactivate-input-method))
  ;; (setq mozc-candidate-style 'echo-area)

  (require 'mozc-popup)
  (set-variable 'mozc-candidate-style 'popup)

  (require 'mozc-im)
  (setq default-input-method "japanese-mozc-im")
  (global-set-key (kbd "C-j") 'toggle-input-method)
  (global-set-key (kbd "C-c m e") 'turn-on-input-method)
  (global-set-key (kbd "C-c m d") 'turn-off-input-method)
  ) ;; 日本語入力ぐぐる
;; (global-set-key
;;  (kbd "C-<f11>" . turn-on-input-method)
;;  ("C-<f12>" . turn-off-input-method))

;; (setq mozc-candidate-style 'overlay)
;; (setq mozc-candidate-style 'echo-area)

(defvar my-cousel-recently-history nil "History of `my-counsel-recently'.")

(when (and (require 'recently nil t)
           (fboundp 'ivy-read))
  (defun my-counsel-recently ()
    "Consel `recently'."
    (interactive)
    (ivy-read "Recently: " (recently-list)
              :require-match t
              :history 'my-cousel-recently-history
              :preselect default-directory
              :action (lambda (x) (find-file x))
              :caller 'my-counsel-recently))

  (define-key ctl-x-map (kbd "C-r") 'my-counsel-recently)
  )


;; Local Variables:
;; flycheck-disabled-checkers: (emacs-lisp-checkdoc)
;; flycheck-checker: emacs-lisp
;; End:

;;; emancs.el ends here

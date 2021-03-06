;;; init --- My init file

;;; Commentary:

;;; Code:

(setenv "PATH"
  (concat
    (concat (getenv "HOME") "/go/bin") ":"
    (getenv "PATH")
  )
)

(setq exec-path (append exec-path (list (concat (getenv "HOME") "/go/bin"))))

(setq inhibit-startup-screen t)

(set-frame-font "Liberation Mono-10")

(require 'package)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/"))
;; Disable implicit package initialization after init.el is executed
(setq package-enable-at-startup nil)
;; Initialize package explicitly
(package-initialize)

;; Set up color themes
;; make the fringe stand out from the background
(if (display-graphic-p)
  (setq solarized-distinct-fringe-background t)
  (setq linum-format "%d "))
;; TODO: disable the next line for console mode
(scroll-bar-mode -1)

;; make the modeline high contrast
(setq solarized-high-contrast-mode-line t)
(setq my-color-themes (list 'solarized-dark
                            'solarized-light))
(defun my-theme-set-default ()
  "Set the first row."
  (interactive)
  (setq theme-current my-color-themes)
  (load-theme (car theme-current) t)

  ;; reset trailing-whitespace face
  (set-face-background 'trailing-whitespace "red")
)
(defun my-theme-cycle ()
  "Set the next theme."
  (interactive)
  (setq theme-current (cdr theme-current))
  (if (null theme-current)
      (setq theme-current my-color-themes))
  (load-theme (car theme-current) t)
  ;; reset trailing-whitespace face
  (set-face-background 'trailing-whitespace "red")
  (message "%S" (car theme-current)))

(my-theme-set-default)
(global-set-key [f12] 'my-theme-cycle)

;; disable toolbar and menubar
(tool-bar-mode -1)
(menu-bar-mode -1)

(require 'ido)
(setq ido-save-directory-list-file "~/.emacs.d/ido.last")
(ido-mode t)

(add-to-list 'load-path "~/.emacs.d/lisp/")

;; linum-mode
(global-linum-mode 1)
(linum-on)
(column-number-mode 1)

;; switch off linum for selected modes
(setq linum-disabled-modes-list '(eshell-mode compilation-mode))
(require 'linum-off)

;; show file names in title (some distros like Fedora override the format - defer to post-init)
(add-hook 'after-init-hook (lambda ()
                             (setq frame-title-format "%b: %f")))

;; smooth scrolling
(setq scroll-step           1
      scroll-conservatively 10000)

(show-paren-mode 1)
(setq show-paren-delay 0)

(setq-default c-default-style "linux")

;; Go to matched paranthesis
(defun goto-match-paren (arg)
  "Go to the matching parenthesis for ARG if on parenthesis, otherwise insert %.
vi style of % jumping to matching brace."
  (interactive "p")
  (cond ((looking-at "\\s\(") (forward-list 1) (backward-char 1))
        ((looking-at "\\s\)") (forward-char 1) (backward-list 1))
        (t (self-insert-command (or arg 1)))))
(global-set-key (kbd "C-5") 'goto-match-paren)

;; Switch on easy window navigation
;in text terminal Shift-<Up> doesn't work -> don't use default bindings
;;(windmove-default-keybindings)
(define-prefix-command 'windows-map) ;; create key map for windows related commands
(global-set-key (kbd "C-a") 'windows-map) ;; create new prefix key C-a
(global-set-key (kbd "C-a <left>")  'windmove-left)
(global-set-key (kbd "C-a <right>") 'windmove-right)
(global-set-key (kbd "C-a <up>")    'windmove-up)
(global-set-key (kbd "C-a <down>")  'windmove-down)
(global-set-key (kbd "C-w") 'windows-map) ;; create new prefix key C-w
(global-set-key (kbd "C-w h")  'windmove-left)
(global-set-key (kbd "C-w l") 'windmove-right)
(global-set-key (kbd "C-w k")    'windmove-up)
(global-set-key (kbd "C-w j")  'windmove-down)

;; Evil mode
(require 'evil)
(evil-mode 1)
;; Unbind SPC and RET from Evil mode.
(defun my-move-key (keymap-from keymap-to key)
     "Move key binding from one keymap to another, deleting from the old location."
     (define-key keymap-to key (lookup-key keymap-from key))
     (define-key keymap-from key nil))
(my-move-key evil-motion-state-map evil-normal-state-map (kbd "RET"))
(my-move-key evil-motion-state-map evil-normal-state-map " ")
;; change mode-line color by evil state
(require 'cl)
(lexical-let ((default-color (cons (face-background 'mode-line)
                                   (face-foreground 'mode-line))))
  (add-hook 'post-command-hook
    (lambda ()
      (let ((color (cond ((minibufferp) default-color)
                         ((evil-insert-state-p) '("#e80000" . "#ffffff"))
                         ((evil-emacs-state-p)  '("#444488" . "#ffffff"))
                         ((buffer-modified-p)   '("#006fa0" . "#ffffff"))
                         (t default-color))))
        (set-face-background 'mode-line (car color))
        (set-face-foreground 'mode-line (cdr color))))))

;; Winner mode
(winner-mode 1)

;; undo-tree -> C-x u
(require 'undo-tree)
(global-undo-tree-mode)

;; Show extra whitespace
(defface extra-whitespace-face
  '((t (:background "pale green")))
  "Used for tabs and such.")
(defvar my-extra-keywords
  '(("\t" . 'extra-whitespace-face)))
(add-hook 'emacs-lisp-mode-hook
          (lambda ()
            (font-lock-add-keywords nil my-extra-keywords)
            (setq indent-tabs-mode nil)
            (setq show-trailing-whitespace t)))
(add-hook 'text-mode-hook
          (lambda ()
            (font-lock-add-keywords nil my-extra-keywords)
            (setq indent-tabs-mode nil)
            (setq show-trailing-whitespace t)))
(add-hook 'python-mode-hook
          (lambda ()
            (font-lock-add-keywords nil my-extra-keywords)
            (setq indent-tabs-mode nil)
            (setq show-trailing-whitespace t)))
(add-hook 'c-mode-hook
          (lambda ()
            (setq show-trailing-whitespace t)))

(require 'highlight-symbol)
;;(setq highlight-symbol-on-navigation-p t)
(global-set-key (kbd "C-8") 'highlight-symbol-next)
(global-set-key (kbd "C-3") 'highlight-symbol-prev)
(global-set-key [(control f3)] 'highlight-symbol-at-point)

;; autocomplete
(require 'go-autocomplete)
(require 'auto-complete-config)
(ac-config-default)

(global-set-key (kbd "C-x g") 'magit-status)
(global-set-key (kbd "C-x M-g") 'magit-dispatch-popup)

;; Bitbake
(require 'bb-mode)
(setq auto-mode-alist (cons '("\\.bb$" . bb-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.inc$" . bb-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.bbappend$" . bb-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.bbclass$" . bb-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.conf$" . bb-mode) auto-mode-alist))

;; multi-term
(setq multi-term-program "/bin/bash")
(require 'multi-term)
(add-to-list 'term-unbind-key-list "C-w")
;; unbind Ctl-r to let shell do reverse search in its history
(setq term-bind-key-alist (delq (assoc "C-r" term-bind-key-alist) term-bind-key-alist))

(global-set-key (kbd "C-t") 'multi-term-next)

;; Show date time
(display-time-mode 1)
(setq display-time-format "[%a %h %e %H:%M]")

;; CScope integration
(require 'xcscope)
(define-key global-map [(ctrl f6)] 'cscope-set-initial-directory)

;; Enable Flycheck
(add-hook 'after-init-hook #'global-flycheck-mode)

;; Go Guru
(require 'go-guru)

(defun my-go-mode-hook ()
  "This is called when we are in go mode."
  (add-hook 'before-save-hook #'gofmt-before-save)
  (setq gofmt-command "goimports")                ; gofmt uses invokes goimports
  (if (not (string-match "go" compile-command))   ; set compile command default
      (set (make-local-variable 'compile-command)
           "go build -v && go test -v && go vet"))
  (local-set-key (kbd "M-p") 'compile)            ; Invoke compiler
  (local-set-key (kbd "M-P") 'recompile)          ; Redo most recent compile cmd
  (local-set-key (kbd "M-]") 'next-error)         ; Go to next error (or msg)
  (local-set-key (kbd "M-[") 'previous-error)     ; Go to previous error or msg
  (go-guru-hl-identifier-mode)
)

(add-hook 'go-mode-hook 'my-go-mode-hook)

(add-hook 'yaml-mode-hook 'highlight-indentation-mode)
(add-hook 'python-mode-hook 'highlight-indentation-mode)
(add-hook 'bazel-mode-hook (lambda () (add-hook 'before-save-hook #'bazel-format nil t)))

;; Bison mode
(require 'bison-mode)

;; Emacs which-key mode on
(which-key-mode)

(setq ispell-list-command "--list")
(dolist (hook '(text-mode-hook))
      (add-hook hook (lambda () (flyspell-mode 1))))
(dolist (hook '(markdown-mode-hook))
      (add-hook hook (lambda () (flyspell-mode 1))))
(dolist (hook '(change-log-mode-hook log-edit-mode-hook))
      (add-hook hook (lambda () (flyspell-mode -1))))

;; LSP mode
(with-eval-after-load 'lsp-mode
  (add-hook 'lsp-mode-hook #'lsp-enable-which-key-integration))
(use-package lsp-mode
  ;; set prefix for lsp-command-keymap (default is "s-l", few alternatives - "C-l", "C-c l")
  :init (setq lsp-keymap-prefix "C-c l")
  :hook (
         (c++-mode . lsp)
         (lsp-mod . lsp-enable-which-key-integration))
  :commands lsp)
(setq gc-cons-threshold 100000000)
(setq read-process-output-max (* 1024 1024)) ;; 1mb

(use-package use-package-ensure-system-package
  :ensure t)

(use-package ccls
  :after projectile
  :ensure-system-package ccls
  :custom
  (ccls-args nil)
  (ccls-executable (executable-find "ccls"))
  (projectile-project-root-files-top-down-recurring
   (append '("compile_commands.json" ".ccls")
           projectile-project-root-files-top-down-recurring))
  :config (push ".ccls-cache" projectile-globally-ignored-directories))

(use-package google-c-style
  :hook ((c-mode c++-mode) . google-set-c-style)
         (c-mode-common . google-make-newline-indent))


;; TODO
;; 1. diff-hl

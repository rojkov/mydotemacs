(setq inhibit-startup-screen t)

(set-default-font "Monospace-10")

(require 'package)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/"))
;; Disable implicit package initialization after init.el is executed
(setq package-enable-at-startup nil)
;; Initialize package explicitly
(package-initialize)

;; Set up color themes
(require 'color-theme)
(color-theme-initialize)
(setq my-color-themes (list 'color-theme-hober
                            'color-theme-classic
                            'color-theme-deep-blue))
(defun my-theme-set-default ()
  "Set the first row."
  (interactive)
  (setq theme-current my-color-themes)
  (funcall (car theme-current))

  ;; reset trailing-whitespace face
  (set-face-background 'trailing-whitespace "red")
)
(defun my-theme-cycle ()
  "Set the next theme."
  (interactive)
  (setq theme-current (cdr theme-current))
  (if (null theme-current)
      (setq theme-current my-color-themes))
  (funcall (car theme-current))
  ;; reset trailing-whitespace face
  (set-face-background 'trailing-whitespace "red")
  (message "%S" (car theme-current)))
(eval-after-load "color-theme"
  '(progn
     (setq color-theme-is-global nil) ; Initialization
     (my-theme-set-default)
     (global-set-key [f12] 'my-theme-cycle)))

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
  "Go to the matching parenthesis if on parenthesis, otherwise insert %.
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

;; CUA mode
(cua-mode t)
(setq cua-auto-tabify-rectangles nil) ;; Don't tabify after rectangle commands
(transient-mark-mode 1) ;; No region when it is not highlighted
(setq cua-keep-region-after-copy nil)

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

(require 'highlight-symbol)
;;(setq highlight-symbol-on-navigation-p t)
(global-set-key (kbd "C-8") 'highlight-symbol-next)
(global-set-key (kbd "C-3") 'highlight-symbol-prev)
(global-set-key [(control f3)] 'highlight-symbol-at-point)

;; autocomplete
(require 'auto-complete-config)
(ac-config-default)

(global-set-key (kbd "C-x g") 'magit-status)
(global-set-key (kbd "C-x M-g") 'magit-dispatch-popup)

(scroll-bar-mode -1)

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
(global-set-key (kbd "C-t") 'multi-term-next)

;; Show date time
(display-time-mode 1)
(setq display-time-format "[ww%U %a %h %e %H:%M]")

;; TODO
;; 1. diff-hl

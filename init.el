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

;; linum-mode
(global-linum-mode 1)
(linum-on)
(column-number-mode 1)

;; show file names in title
(setq frame-title-format "%b: %f")

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

;; TODO
;; 1. diff-hl

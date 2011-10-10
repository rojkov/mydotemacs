;;; .emacs --- Emacs initialization
;;; Code:
(setq inhibit-startup-screen t)

(set-default-font "Monospace-10")

(require 'color-theme)
(color-theme-initialize)
(setq my-color-themes (list 'color-theme-deep-blue
                            'color-theme-arjen))
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

;; Switch on easy window navigation
(windmove-default-keybindings)

;; CUA mode
(cua-mode t)
(setq cua-auto-tabify-rectangles nil) ;; Don't tabify after rectangle commands
(transient-mark-mode 1) ;; No region when it is not highlighted
(setq cua-keep-region-after-copy nil) ;; Standard Windows behaviour

;; Redo
(require 'redo "~/.emacs.d/vendors/redo.el")
(global-set-key (kbd "C-/") 'undo)
(global-set-key (kbd "C-x C-/") 'redo)

;; Go to matched paranthesis
(defun goto-match-paren (arg)
  "Go to the matching parenthesis if on parenthesis, otherwise insert %.
vi style of % jumping to matching brace."
  (interactive "p")
  (cond ((looking-at "\\s\(") (forward-list 1) (backward-char 1))
        ((looking-at "\\s\)") (forward-char 1) (backward-list 1))
        (t (self-insert-command (or arg 1)))))
(global-set-key (kbd "C-5") 'goto-match-paren)

(require 'whitespace)

(add-hook 'python-mode-hook (lambda () (setq show-trailing-whitespace t)))
(global-set-key (kbd "C-c w") 'delete-trailing-whitespace)

;; Show extra whitespace
(defface extra-whitespace-face
  '((t (:background "pale green")))
  "Used for tabs and such.")
(defvar my-extra-keywords
  '(("\t" . 'extra-whitespace-face)))
(add-hook 'emacs-lisp-mode-hook
          (lambda () (font-lock-add-keywords nil my-extra-keywords)))
(add-hook 'text-mode-hook
          (lambda () (font-lock-add-keywords nil my-extra-keywords)))
(add-hook 'python-mode-hook
          (lambda () (font-lock-add-keywords nil my-extra-keywords)))

(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)

(show-paren-mode 1)
(setq show-paren-delay 0)

(require 'highlight-symbol "~/.emacs.d/highlight-symbol.el")
;;(setq highlight-symbol-on-navigation-p t)
(global-set-key (kbd "C-8") 'highlight-symbol-next)
(global-set-key (kbd "C-3") 'highlight-symbol-prev)
(global-set-key [(control f3)] 'highlight-symbol-at-point)

;; rope
(require 'pymacs)
(pymacs-load "ropemacs" "rope-")

;; autocomplete
(add-to-list 'load-path "~/.emacs.d/autocomplete")
(require 'auto-complete-config)
(add-to-list 'ac-dictionary-directories "~/.emacs.d/autocomplete/ac-dict")
(ac-config-default)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ropemacs

(defun ac-ropemacs-candidates ()
  (mapcar (lambda (completion)
      (concat ac-prefix completion))
    (rope-completions)))

(ac-define-source nropemacs
  '((candidates . ac-ropemacs-candidates)
    (symbol . "p")))

(ac-define-source nropemacs-dot
  '((candidates . ac-ropemacs-candidates)
    (symbol . "p")
    (prefix . c-dot)
    (requires . 0)))

(defun ac-nropemacs-setup ()
  (setq ac-sources '(ac-source-nropemacs ac-source-nropemacs-dot)))

;; extended ropemacs

(defun ac-eropemacs-candidates ()
  (mapcar (lambda (proposal)
          (destructuring-bind (name doc type) proposal
            (list (concat ac-prefix name) doc
                  (if type (substring type 0 1) nil))))
        (rope-extended-completions)))

(defun ac-eropemacs-document (item) (car item))

;; This function is not used as ac-define-source's symbol cons accepts
;; sequence (like "p"), not function.
(defun ac-eropemacs-symbol (item) (cadr item))

(ac-define-source extended-ropemacs
  '((candidates . ac-eropemacs-candidates)
    (document . ac-eropemacs-document)
    (symbol . "p")))

(ac-define-source extended-ropemacs-dot
  '((candidates . ac-eropemacs-candidates)
    (document . ac-eropemacs-document)
    (symbol . "p")
    (prefix . c-dot)
    (requires . 0)))

(defun ac-eropemacs-setup ()
  (setq ac-sources '(ac-source-extended-ropemacs ac-source-extended-ropemacs-dot)))

(defun ac-ropemacs-setup ()
  (if (functionp 'rope-extended-completions)
      (add-hook 'python-mode-hook 'ac-eropemacs-setup)
    (add-hook 'python-mode-hook 'ac-nropemacs-setup)))

(ac-ropemacs-setup)
;; end of ropemacs setup
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(add-hook 'python-mode-hook '(lambda () (define-key python-mode-map "\C-m" 'newline-and-indent)))
(add-hook 'python-mode-hook 'turn-on-eldoc-mode)

;; linum-mode
(require 'linum-off "~/.emacs.d/vendors/linum-off.el")
(global-linum-mode 1)
(linum-on)
(column-number-mode 1)

;; visible-mark-mode
(require 'visible-mark "~/.emacs.d/vendors/visible-mark.el")
(global-visible-mark-mode 1)

;; smooth scrolling
(setq scroll-step           1
      scroll-conservatively 10000)

;; show file names in title
(setq frame-title-format "%b: %f")

(require 'ido)
(setq ido-save-directory-list-file "~/.emacs.d/ido.last")
(ido-mode t)

(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(ecb-options-version "2.32")
 '(highlight-symbol-on-navigation-p t))
(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 )

(provide '.emacs)

;; Erlang mode
(setq load-path (cons "/usr/lib/erlang/lib/tools-2.6.5/emacs" load-path))
(setq erlang-root-dir "/usr/lib/erlang")
(setq exec-path (cons "/usr/lib/erlang/lib/tools-2.6.5/emacs" exec-path))
;;(require 'erlang-start "/usr/lib/erlang/lib/tools-2.6.5/emacs/erlang-start.el")
(require 'erlang-start)

;;; .emacs ends here

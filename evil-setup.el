;; for evil-collection, set this before loading evil
(setq evil-want-integration nil)

;; set up evil mode to work nicely
(require 'evil)
(evil-mode 1)
(setq evil-want-fine-undo t)

;; evil-collection
(require 'evil-collection)
(evil-collection-init)

;; magit
(require 'evil-magit)

;; surround
(global-evil-surround-mode 1)

;; code folding
(evil-vimish-fold-mode 1)

;; find tags
(defun my-jump-to-tag ()
  (interactive)
  (call-interactively (key-binding (kbd "g C-]"))))
(define-key evil-normal-state-map (kbd "C-]") 'my-jump-to-tag)

;; space to enter one character
(defun enter-one-character ()
  (interactive)
  (insert (read-char)))
(define-key evil-normal-state-map (kbd "SPC") 'enter-one-character)

;; enter, S-enter to create a new line
(defun enter-one-line-below ()
  (interactive)
  (evil-insert-newline-below))
(define-key evil-normal-state-map (kbd "C-j") 'enter-one-line-below)
(defun enter-one-line-above ()
  (interactive)
  (evil-insert-newline-above))
(define-key evil-normal-state-map (kbd "C-k") 'enter-one-line-above)

;; visual line mode keys
(add-hook 'visual-line-mode-hook
          (lambda ()
            (define-key evil-normal-state-map (kbd "j") 'next-line)
            (define-key evil-normal-state-map (kbd "k") 'previous-line)))

;; in insert mode, highlight line
(add-hook 'evil-insert-state-entry-hook
          (lambda () (hl-line-mode t)))
(add-hook 'evil-insert-state-exit-hook
          (lambda ()
            (let ((current-prefix-arg '(0)))
              (call-interactively 'hl-line-mode))))
(custom-set-faces
 '(hl-line ((t (:background "gray18")))))

;; key chord
(defun normal-state-forward ()
  (interactive)
  (evil-normal-state)
  (forward-char))
(key-chord-define evil-insert-state-map "jk" 'normal-state-forward)
(key-chord-define evil-visual-state-map "jk" 'evil-normal-state)
(key-chord-define evil-emacs-state-map "jk" 'evil-normal-state)
(key-chord-define evil-replace-state-map "jk" 'evil-normal-state)
(setq key-chord-two-keys-delay 0.2)

;; save with :W
(evil-ex-define-cmd "W" 'save-buffer)

;; always go to emacs mode in ansi-term char-mode
(defun evil-term-char-mode ()
  "enter term-char-mode and evil-emacs-state"
  (interactive)
  (term-char-mode)
  (evil-emacs-state))
(defun evil-term-line-mode ()
  "enter term-line-mode and evil-emacs-state"
  (interactive)
  (term-line-mode)
  (evil-normal-state))
(add-hook 'term-mode-hook
          (lambda ()
            (define-key term-mode-map (kbd "C-c C-k")
              'evil-term-char-mode)))
(add-hook 'term-mode-hook
          (lambda ()
            (define-key term-raw-map (kbd "C-c C-j")
              'evil-term-line-mode)))

;; expand region in normal mode
(require 'expand-region)
(define-key evil-normal-state-map (kbd "+") 'er/expand-region)
;; - to contract region, 0 to reset

;; evil-numbers, C-a and C-x in Vim
;; conflicts with org-mode
;; (define-key evil-normal-state-map (kbd "C-c =") 'evil-numbers/inc-at-pt)
;; (define-key evil-normal-state-map (kbd "C-c -") 'evil-numbers/dec-at-pt)

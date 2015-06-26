;; set up evil mode to work nicely
(evil-mode 1)
(setq evil-want-fine-undo t)

;; TODO try space as leader key
;; http://sheerun.net/2014/03/21/how-to-boost-your-vim-productivity/
;; https://github.com/cofi/evil-leader

;; find tags
(defun my-jump-to-tag ()
  (interactive)
  (evil-execute-in-emacs-state)
  (call-interactively (key-binding (kbd "M-."))))
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
 '(hl-line ((t (:background "color-236")))))

;; key chord
(key-chord-mode 1)
(defun normal-state-forward ()
  (interactive)
  (evil-normal-state)
  (forward-char))
(key-chord-define evil-insert-state-map "jk" 'normal-state-forward)
(key-chord-define evil-visual-state-map "jk" 'evil-normal-state)
(key-chord-define evil-emacs-state-map "jk" 'evil-normal-state)
(key-chord-define evil-replace-state-map "jk" 'evil-normal-state)
(setq key-chord-two-keys-delay 0.2)

;; surround
(global-evil-surround-mode 1)

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

;; add evil magic to other modes

(require 'magit)
;; Make HJKL keys work in special buffers
(evil-add-hjkl-bindings magit-branch-manager-mode-map 'emacs
  "K" 'magit-discard-item
  "L" 'magit-key-mode-popup-logging)
(evil-add-hjkl-bindings magit-status-mode-map 'emacs
  "K" 'magit-discard-item
  "l" 'magit-key-mode-popup-logging
  "h" 'magit-toggle-diff-refine-hunk)
  ;; "C-w" nil
  ;; "C-w h" 'evil-window-left
  ;; "C-w l" 'evil-window-right)
(evil-add-hjkl-bindings magit-log-mode-map 'emacs)
(evil-add-hjkl-bindings magit-commit-mode-map 'emacs)

(evil-add-hjkl-bindings occur-mode 'emacs)
(evil-add-hjkl-bindings ibuffer-mode 'emacs)
(require 'browse-kill-ring)
(define-key browse-kill-ring-mode-map (kbd "k") 'browse-kill-ring-previous) ; why is this slow!?
(define-key browse-kill-ring-mode-map (kbd "j") 'browse-kill-ring-forward) ; why is this slow!?

;; doesn't work:
;; (defun add-other-hjkl-bindings (keymap state)
;;   (evil-define-key state keymap 
;;      "C-w h" (lookup-key evil-motion-state-map "C-w h")
;;      "C-w j" (lookup-key evil-motion-state-map "C-w j")
;;      "C-w k" (lookup-key evil-motion-state-map "C-w k")
;;      "C-w l" (lookup-key evil-motion-state-map "C-w l")))

;; expand region in normal mode
(require 'expand-region)
(define-key evil-normal-state-map (kbd "+") 'er/expand-region)
;; - to contract region, 0 to reset

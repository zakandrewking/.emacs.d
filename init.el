;;-----------------------------------------------------------------------
;; setup
;;-----------------------------------------------------------------------
;; set default directory
(let ((default-directory "~/.emacs.d/lisp/"))
  (normal-top-level-add-to-load-path '("."))
  (normal-top-level-add-subdirs-to-load-path))

(defun reload-dotemacs ()
  "reload your .emacs file without restarting Emacs"
  (interactive)
  (load-file "~/.emacs") )

(put 'upcase-region 'disabled nil)
(desktop-save-mode 1)
(setq clean-buffer-list-delay-general 0)
(setq magic-mode-alist ())

;; http://whattheemacsd.com/file-defuns.el-01.html
(defun rename-file-and-buffer ()
  "Renames current buffer and file it is visiting."
  (interactive)
  (let ((name (buffer-name))
        (filename (buffer-file-name)))
    (if (not (and filename (file-exists-p filename)))
        (error "Buffer '%s' is not visiting a file!" name)
      (let ((new-name (read-file-name "New name: " filename)))
        (if (get-buffer new-name)
            (error "A buffer named '%s' already exists!" new-name)
          (rename-file filename new-name 1)
          (rename-buffer new-name)
          (set-visited-file-name new-name)
          (set-buffer-modified-p nil)
          (message "File '%s' successfully renamed to '%s'"
                   name (file-name-nondirectory new-name)))))))

;; start emacsserver
(server-start)

;; no menu bar
(menu-bar-mode -1)

;; terminal emacs
(unless (featurep 'aquamacs)
  ;; Set up el-get
  (add-to-list 'load-path "~/.emacs.d/el-get/el-get")

  (unless (require 'el-get nil 'noerror)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.github.com/dimitri/el-get/master/el-get-install.el")
      (let (el-get-master-branch)
        (goto-char (point-max))
        (eval-print-last-sexp))))

  ;; local sources
  (setq el-get-sources
        '((:name nxhtml
		 :type github
		 :pkgname "emacsmirror/nxhtml"
		 :prepare (progn
			    (load "~/.emacs.d/el-get/nxhtml/autostart.el"))
		 )
	  ) 
  )
  (setq my-packages
        (append
         '(el-get cedet matlab-mode nxhtml git-gutter)
	 (mapcar 'el-get-source-name el-get-sources)))

  (el-get-cleanup my-packages)
  (el-get 'sync my-packages)

  ;; color theme
  ;; Should match terminal color theme,
  ;; or at least add `export TERM=xterm-256color` to your bash_profile.
  (load-theme 'wombat t)

  ;; linum-mode
  (global-linum-mode 1)
  (setq linum-format "%3d ")
  )

(unless window-system
  ;; Enable mouse support
  (require 'mouse)
  (xterm-mouse-mode t)
  (global-set-key [mouse-4] '(lambda ()
                               (interactive)
                               (scroll-down 1)))
  (global-set-key [mouse-5] '(lambda ()
                               (interactive)
                               (scroll-up 1)))
  (defun track-mouse (e))
  (setq mouse-sel-mode t)
  )

;;-----------------------------------------------------------------------
;; copy-paste
;;-----------------------------------------------------------------------

;; system copy paste
(defun pt-pbpaste ()
  "Paste data from pasteboard."
  (interactive)
  (shell-command-on-region
   (point)
   (if mark-active (mark) (point))
   "pbpaste" nil t))

(defun pt-pbcopy ()
  "Copy region to pasteboard."
  (interactive)
  (print (mark))
  (when mark-active
    (shell-command-on-region
     (point) (mark) "pbcopy")
    (kill-buffer "*Shell Command Output*")))

;; hide autosaves in temp directory
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))
;; hide backup files
(setq backup-directory-alist '(("." . "~/.emacs.d/backup"))
  backup-by-copying t    ; Don't delink hardlinks
  version-control t      ; Use version numbers on backups
  delete-old-versions t  ; Automatically delete excess backups
  kept-new-versions 20   ; how many of the newest versions to keep
  kept-old-versions 5    ; and how many of the old
  )

;;-----------------------------------------------------------------------
;; movement
;;-----------------------------------------------------------------------

;; kill the characters from the cursor to the beginning of the current line
(defun backward-kill-line (arg)
  "Kill chars backward until encountering the end of a line."
  (interactive "p")
  (kill-line 0))

;; M-<del> ... delete one word
(defun my-backward-kill-word (arg)
  "Backward kill word"
  (interactive "p")
  (backward-kill-word 1))

;; ido for easy buffer switching
;; http://www.emacswiki.org/emacs/InteractivelyDoThings#Ido
(require 'ido)
(ido-mode t)

;; stop annoying "Command attempted to use minibuffer while in minibuffer."
;; http://trey-jackson.blogspot.com/2010/04/emacs-tip-36-abort-minibuffer-when.html
(defun stop-using-minibuffer ()
  "kill the minibuffer"
  (when (and (>= (recursion-depth) 1) (active-minibuffer-window))
    (abort-recursive-edit)))

(add-hook 'mouse-leave-buffer-hook 'stop-using-minibuffer)

;;-----------------------------------------------------------------------
;; aquamacs
;;-----------------------------------------------------------------------

(when (featurep 'aquamacs)
  (tool-bar-mode -1)
  (tooltip-mode -1)
  (global-linum-mode t)

  ;; set global font
  (set-default-font "-apple-Inconsolata-medium-normal-normal-*-17-*-*-*-m-0-iso10646-1")

  ;; set color theme
  (require 'color-theme)
  (color-theme-initialize)
  (color-theme-charcoal-black)
  (set-face-background 'modeline "#FFEF94"))

;;-----------------------------------------------------------------------
;; appearance
;;-----------------------------------------------------------------------

(column-number-mode t)
(line-number-mode t)

;; speedbar
(require 'sr-speedbar)
(setq resize-mini-windows nil)
(setq speedbar-use-images nil)
(setq sr-speedbar-auto-refresh nil)
(setq sr-speedbar-max-width 100)
(setq sr-speedbar-width-console 50)
(setq sr-speedbar-width-x 50)

;; line highlight color
(global-hl-line-mode 0)

;; highlight lines that are too long
;; (require 'whitespace)
;; (whitespace-mode nil)
;; (defun whitespace-hook ()
;;   (interactive)
;;   (setq whitespace-style '(face lines-tail))
;;   (whitespace-mode t))

;;-----------------------------------------------------------------------
;; tramp
;;----------------------------------------------------------------------- 

;; only look for hosts in the .ssh/config file
(require 'tramp)
(tramp-set-completion-function "ssh"
           '((tramp-parse-sconfig "~/.ssh/config")))
(tramp-set-completion-function "scpc"
           '((tramp-parse-sconfig "~/.ssh/config")))

;;-----------------------------------------------------------------------
;; window switching
;;-----------------------------------------------------------------------

;; windmove for fast window switching
;; (when (fboundp 'windmove-default-keybindings)
;;   (windmove-default-keybindings))
;; (global-set-key (kbd "C-<left>")  'windmove-left)
;; (global-set-key (kbd "C-<right>") 'windmove-right)
;; (global-set-key (kbd "C-<up>")    'windmove-up)
;; (global-set-key (kbd "C-<down>")  'windmove-down)

(defun switch-to-minibuffer ()
  "Switch to minibuffer window."
  (interactive)
  (if (active-minibuffer-window)
      (select-window (active-minibuffer-window))
    (error "Minibuffer is not active")))

(defun my-other-window ()
  (interactive)
  (select-window (next-window nil 'never-minibuf nil)))

;;-----------------------------------------------------------------------
;; flymake
;;-----------------------------------------------------------------------

;;(require 'flymake)

;;(defun flymake-get-tex-args (file-name) (list "pdflatex"
;;    (list "-file-line-error" "-draftmode" "-interaction=nonstopmode" file-name)))

;;(add-hook 'LaTeX-mode-hook 'flymake-mode)
;;(add-hook 'MATLAB-mode-hook 'flymake-mode)

;;-----------------------------------------------------------------------
;; TAB behavior
;;-----------------------------------------------------------------------

;; pabbrev
(require 'pabbrev)
(defun pabbrev-hook ()
  (pabbrev-mode 1))
(add-hook 'c-mode-hook             'pabbrev-hook)
(add-hook 'sh-mode-hook            'pabbrev-hook)
(add-hook 'emacs-lisp-mode-hook    'pabbrev-hook)
(add-hook 'LaTeX-mode-hook         'pabbrev-hook)
(add-hook 'matlab-mode-hook        'pabbrev-hook)
(add-hook 'python-mode-hook        'pabbrev-hook)
(add-hook 'nxhtml-mode-hook             'pabbrev-hook)
(add-hook 'nxhtml-mumamo-mode-hook      'pabbrev-hook)
(add-hook 'org-mode-hook           'pabbrev-hook)
(add-hook 'javascript-mode-hook    'pabbrev-hook)

(defun pabbrev-suggestions-ido (suggestion-list)
  "Use ido to display menu of all pabbrev suggestions."
  (when suggestion-list
    (pabbrev-suggestions-insert-word pabbrev-expand-previous-word)
    (pabbrev-suggestions-insert-word
     (ido-completing-read "Completions: " (mapcar 'car suggestion-list)))))

(defun pabbrev-suggestions-insert-word (word)
  "Insert word in place of current suggestion, with no attempt to kill pabbrev-buffer."
  (let ((point))
    (save-excursion
      (let ((bounds (pabbrev-bounds-of-thing-at-point)))
        (progn
          (delete-region (car bounds) (cdr bounds))
          (insert word)
          (setq point (point)))))
    (if point
        (goto-char point))))

(fset 'pabbrev-suggestions-goto-buffer 'pabbrev-suggestions-ido)

;; ;; in some modes, just expand
;; (defun just-expand (arg)
;;   (interactive "*P")
;;   (dabbrev-expand arg))
;; (defun my-tab-expand ()
;;   (local-set-key [tab] 'just-expand))
;; (add-hook 'comint-mode-hook 'my-tab-expand)
;;
;; OR
;;
;; (defun indent-or-expand (arg)
;;   "Either indent according to mode, or expand the word preceding
;; point."
;;   (interactive "*P")
;;   (if (and
;;        (or (bobp) (= ?w (char-syntax (char-before))))
;;        (or (eobp) (not (= ?w (char-syntax (char-after))))))
;;       (dabbrev-expand arg)
;;     (indent-according-to-mode)))

;;-----------------------------------------------------------------------
;; TEXT management
;;-----------------------------------------------------------------------

(defun iwb ()
  "indent whole buffer"
  (interactive)
  (delete-trailing-whitespace)
  (indent-region (point-min) (point-max) nil)
  (untabify (point-min) (point-max)))

;; use auto-fill-mode to wrap lines ('fill paragraph') after fill-column lines
(setq-default fill-column 80)

;; (defun itc ()
;;    "indent to column of mark"
;;    (interactive)
;;    (setq pos (point))
;;    (goto-char (mark t))
;;    (setq col (current-column))
;;    (goto-char pos)
;;    (just-one-space)
;;    (indent-to-column col))
;; (global-set-key (kbd "M-<tab>") 'itc)

;;-----------------------------------------------------------------------
;; coffee programming
;;-----------------------------------------------------------------------

(require 'coffee-mode)
;; (autoload 'coffee-mode "coffeescript" "Enter coffee-mode" t)
(add-to-list 'auto-mode-alist '("\\.coffee\\'" . python-mode))

;;-----------------------------------------------------------------------
;; shell programming
;;-----------------------------------------------------------------------

(add-to-list 'auto-mode-alist '("\\.sh\\'" . shell-script-mode))
(add-to-list 'auto-mode-alist '("\\.pbs\\'" . shell-script-mode))
(add-to-list 'auto-mode-alist '("\\.ext\\'" . shell-script-mode))

;;-----------------------------------------------------------------------
;; matlab programming
;;-----------------------------------------------------------------------
;; (setq load-path (cons (concat *personal-elisp* "matlab/") load-path))
(autoload 'matlab-mode "matlab" "Enter Matlab Mode." t)
;; (autoload 'matlab-shell "matlab" "Interactive Matlab Mode." t)
;; (setq matlab-shell-command "matlab")
(add-to-list 'auto-mode-alist '("\\.m\\'" . matlab-mode))

;;  (autoload 'mlint-minor-mode "mlint" "\
;; Toggle mlint minor mode, a mode for showing mlint errors.
;; With prefix ARG, turn mlint minor mode on iff ARG is positive.
;; \\{mlint-minor-mode-map\\}
;; \(fn &optional ARG)" t nil)
;; (defun mlint-hook ()
;;    (mlint-minor-mode 1))
;; (add-hook 'matlab-mode-hook 'mlint-hook)

;; (setq matlab-shell-command-switches '("-nodesktop -nosplash")) ; -nojvm")
(add-hook 'matlab-mode-hook
          (lambda ()
            (setq matlab-indent-level 4)
            (setq fill-column 80)
            (define-key matlab-mode-map "\M-;" 'comment-dwim)))
;; (setq matlab-show-mlint-warnings nil)
;; (setq mlint-programs '("/Applications/MATLAB_R2010b.app/bin/maci64/mlint"))

;;-----------------------------------------------------------------------
;; Latex programming
;;-----------------------------------------------------------------------

(autoload 'flyspell-mode "flyspell" "On-the-fly spelling checker." t)
(add-hook 'LaTeX-mode-hook 'flyspell-mode)

;;-----------------------------------------------------------------------
;; web programming
;;-----------------------------------------------------------------------

(add-to-list 'auto-mode-alist '("\\.html\\'" . nxhtml-mode))
(setq mumamo-chunk-coloring 5)
(custom-set-faces
 '(mumamo-border-face-in ((t (:inherit font-lock-preprocessor-face :underline t :weight bold))))
 '(mumamo-border-face-out ((t (:inherit font-lock-preprocessor-face :underline t :weight bold)))))

;;-----------------------------------------------------------------------
;; python programming
;;-----------------------------------------------------------------------

(require 'python-mode)
(add-to-list 'auto-mode-alist '("\\.py\\'" . python-mode))

;;-----------------------------------------------------------------------
;; Markdown mode
;;-----------------------------------------------------------------------
(require 'markdown-mode)
(add-to-list 'auto-mode-alist '("\\.md\\'" . gfm-mode)) ;github-flavored markdown

;;-----------------------------------------------------------------------
;; Deft mode
;;-----------------------------------------------------------------------

(require 'deft)
(setq deft-directory "~/Dropbox/PlainText/")
(setq deft-extension "txt")
(setq deft-text-mode 'org-mode)
(setq deft-use-filename-as-title t)
(put 'erase-buffer 'disabled nil)
(put 'toggle-mac-option-modifier 'disabled t)

;; always open text in org-mode
(add-to-list 'auto-mode-alist '("\\.txt\\'" . org-mode))

(setq org-startup-folded nil)

;;-----------------------------------------------------------------------
;; git gutter
;;-----------------------------------------------------------------------

(when (featurep 'aquamacs)
  (require 'git-gutter-fringe)
  (add-hook 'python-mode-hook 'git-gutter-mode)
  (add-hook 'matlab-mode-hook 'git-gutter-mode))

(unless (featurep 'aquamacs)
  (require 'git-gutter)
  (setq git-gutter:update-threshold nil)
  (setq git-gutter:update-hooks '(after-save-hook after-revert-hook)))

;;-----------------------------------------------------------------------
;; key bindings
;;-----------------------------------------------------------------------

(defvar my-keys-minor-mode-map (make-keymap) "my-keys-minor-mode keymap.")

(define-key my-keys-minor-mode-map (kbd "C-c d") 'deft)
(define-key my-keys-minor-mode-map (kbd "C-c q") 'auto-fill-mode)
;; (define-key my-keys-minor-mode-map "\C-u" 'backward-kill-line) ;; C-u: universal-argument
(define-key my-keys-minor-mode-map "\C-x\ \C-r" 'find-file-read-only)
(define-key my-keys-minor-mode-map (kbd "M-<backspace>") 'my-backward-kill-word)
(define-key my-keys-minor-mode-map (kbd "M-<up>") 'backward-paragraph)
(define-key my-keys-minor-mode-map (kbd "M-<down>") 'forward-paragraph)
(define-key my-keys-minor-mode-map "\C-xo" 'my-other-window)
(define-key my-keys-minor-mode-map "\C-co" 'switch-to-minibuffer)
(define-key my-keys-minor-mode-map "\C-x\ \C-k" 'kill-buffer)
(define-key my-keys-minor-mode-map (kbd "C-c s") 'sr-speedbar-toggle)
(define-key my-keys-minor-mode-map (kbd "C-c g") 'git-gutter:toggle)
(define-key my-keys-minor-mode-map (kbd "C-c l") 'linum-mode)
(define-key my-keys-minor-mode-map (kbd "C-c p") 'pabbrev-mode)
(define-key my-keys-minor-mode-map (kbd "C-c v") 'pt-pbpaste)
(define-key my-keys-minor-mode-map (kbd "C-c c") 'pt-pbcopy)
(define-key my-keys-minor-mode-map (kbd "C-x C-b") 'ibuffer)

(define-minor-mode my-keys-minor-mode
  "A minor mode so that my key settings override annoying major modes."
  t " my-keys" 'my-keys-minor-mode-map)
(my-keys-minor-mode 1)

(global-unset-key (kbd "C-z"))

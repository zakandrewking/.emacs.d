;;-----------------------------------------------------------------------
;; packages
;;-----------------------------------------------------------------------

(when (>= emacs-major-version 24)
  (require 'cl)
  (require 'package)
  (add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t)
  (package-initialize)

  (defvar required-packages '(evil magit deft key-chord js2-mode
    browse-kill-ring yaml-mode ag smart-mode-line web-mode)
    "a list of packages to ensure are installed at launch.")

  ;; method to check if all packages are installed
  (defun packages-installed-p ()
    (loop for p in required-packages
		  when (not (package-installed-p p)) do (return nil)
		  finally (return t)))

  ;; if not all packages are installed, check one by one and install the missing ones.
  (unless (packages-installed-p)
	;; check for new packages (package versions)
    (message "%s" "Emacs is now refreshing its package database...")
    (package-refresh-contents)
    (message "%s" " done.")
	;; install the missing packages
    (dolist (p required-packages)
      (when (not (package-installed-p p))
		(package-install p))))
  
  ;; evil mode setup
  (evil-mode 1)
  (setq evil-want-fine-undo t)
  (define-key evil-normal-state-map (kbd "C-]") (kbd "\\ M-."))
  ; key chord
  (key-chord-mode 1)
  (key-chord-define evil-insert-state-map "jk" 'evil-normal-state)
  (setq key-chord-two-keys-delay 0.2)
  
  ;; smart-mode-line
  (sml/setup)
  (sml/apply-theme 'respectful)

  ;; js2-mode setup
  (add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
  (add-to-list 'auto-mode-alist '("\\.json\\'" . javascript-mode))
  ;; include underscores in the word definition (especially useful for evil-mode
  ;; superstar)
  (add-hook 'js2-mode-hook (lambda () (modify-syntax-entry ?_ "w")))
  
  ;; python
  (setq tab-width 4)
  (setq py-indent-offset 4)

  ;; web-mode
  (add-to-list 'auto-mode-alist '("\\.html\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.jinja2\\'" . web-mode))
  (setq web-mode-engines-alist '(("django" . "\\.html\\'")))

  ;; browse-kill-ring
  (browse-kill-ring-default-keybindings)
  
  ;; yaml-mode
  (add-to-list 'auto-mode-alist '("\\.ya?ml\\'" . yaml-mode))

  ;; deft-mode
  (setq deft-directory "~/Dropbox (Personal)/PlainText/")
  (setq deft-extension "txt")
  (setq deft-text-mode 'org-mode)
  (setq deft-use-filename-as-title t)
  (put 'erase-buffer 'disabled nil)
  (put 'toggle-mac-option-modifier 'disabled t)
  (add-hook 'org-mode-hook 'auto-fill-mode)

  ;; sr-speedbar
  ;; (setq resize-mini-windows nil)
  ;; (setq speedbar-use-images nil)
  ;; (setq sr-speedbar-auto-refresh nil)
  ;; (setq sr-speedbar-max-width 100)
  ;; (setq sr-speedbar-width-console 50)
  ;; (setq sr-speedbar-width-x 50)
  )

;;-----------------------------------------------------------------------
;; setup
;;-----------------------------------------------------------------------

;; set default directory
(let ((default-directory "~/.emacs.d/lisp/"))
  (normal-top-level-add-to-load-path '("."))
  (normal-top-level-add-subdirs-to-load-path))

;; variables
(put 'upcase-region 'disabled nil)
(desktop-save-mode 1)
(setq clean-buffer-list-delay-general 0)
(setq magic-mode-alist ())
(setq tab-width 4)
(menu-bar-mode -1) ; no menu bar
(setq linum-format "%3d ")
(setq large-file-warning-threshold 5000000)
(global-linum-mode 0) ; linum-mode off
(column-number-mode t)
(line-number-mode t)
(global-hl-line-mode 0) ; line highlight color

;; start emacsserver
(server-start)

;; color theme
;; Should match terminal color theme,
;; or at least add `export TERM=xterm-256color` to your bash_profile.
(load-theme 'wombat t)
(display-time)

;; Enable mouse support
(unless window-system
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
;; functions
;;-----------------------------------------------------------------------

(defun reload-dotemacs ()
  "reload your .emacs file without restarting Emacs"
  (interactive)
  (load-file "~/.emacs") )

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

;; Use ido-find-file from ibuffer window
(require 'ibuffer)
(defun ibuffer-ido-find-file ()
  "Like `ido-find-file', but default to the directory of the buffer at point."
  (interactive
   (let ((default-directory (let ((buf (ibuffer-current-buffer)))
                              (if (buffer-live-p buf)
                                  (with-current-buffer buf
                                    default-directory)
                                default-directory))))
     (ido-find-file-in-dir default-directory))))
(define-key ibuffer-mode-map "\C-x\C-f" 'ibuffer-ido-find-file)

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
;; tab behavior
;;-----------------------------------------------------------------------

;; pabbrev
(require 'pabbrev)
(setq pabbrev-minimal-expansion-p 1)
(defun pabbrev-get-previous-binding ()
  "override default"
  (nil))

(global-pabbrev-mode 1)

(defun no-pabbrev-hook ()
  "Turn off pabbrev mode"
  (pabbrev-mode 0))
(add-hook 'js-mode-hook 'no-pabbrev-hook)

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

;;-----------------------------------------------------------------------
;; text management
;;-----------------------------------------------------------------------

(defun indent-return-indent ()
  "indent current line, then return, and intent next line"
  (interactive)
  (indent-for-tab-command)
  (newline-and-indent))

(defun indent-return-comment-indent ()
  "indent current line, then return new comment line, and intent"
  (interactive)
  (indent-for-tab-command)
  (indent-new-comment-line))

(defun iwb ()
  "indent whole buffer"
  (interactive)
  (delete-trailing-whitespace)
  (indent-region (point-min) (point-max) nil)
  (untabify (point-min) (point-max)))

;; use auto-fill-mode to wrap lines ('fill paragraph') after fill-column lines
(setq-default fill-column 80)

(defun unfill-paragraph ()
  "Replace newline chars in current paragraph by single spaces.
This command does the inverse of `fill-paragraph'."
  (interactive)
  (let ((fill-column 90002000)) ; 90002000 is just random. you can use `most-positive-fixnum'
    (fill-paragraph nil)))

(defun unfill-region (start end)
  "Replace newline chars in region by single spaces.
This command does the inverse of `fill-region'."
  (interactive "r")
  (let ((fill-column 90002000))
    (fill-region start end)))

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

;; Use C-s C-M-w C-s to search word symbol at point
;; http://www.emacswiki.org/emacs/SearchAtPoint
(require 'etags)
(defun isearch-yank-regexp (regexp)
  "Pull REGEXP into search regexp." 
  (let ((isearch-regexp nil)) ;; Dynamic binding of global.
    (isearch-yank-string regexp))
  (if (not isearch-regexp)
      (isearch-toggle-regexp))
  (isearch-search-and-update))

(defun isearch-yank-symbol ()
  "Put symbol at current point into search string."
  (interactive)
  (let ((sym (find-tag-default)))
    (if (null sym)
	(message "No symbol at point")
      (isearch-yank-regexp
       (concat "\\_<" (regexp-quote sym) "\\_>")))))
(define-key isearch-mode-map "\C-\M-w" 'isearch-yank-symbol)

;;-----------------------------------------------------------------------
;; shell programming
;;-----------------------------------------------------------------------

(add-to-list 'auto-mode-alist '("\\.sh\\'" . shell-script-mode))
(add-to-list 'auto-mode-alist '("\\.pbs\\'" . shell-script-mode))
(add-to-list 'auto-mode-alist '("\\.ext\\'" . shell-script-mode))

;;-----------------------------------------------------------------------
;; matlab programming
;;-----------------------------------------------------------------------

(autoload 'matlab-mode "matlab" "Enter Matlab Mode." t)

(add-to-list 'auto-mode-alist '("\\.m\\'" . matlab-mode))

(add-hook 'matlab-mode-hook
          (lambda ()
            (setq matlab-indent-level 4)
            (setq fill-column 80)
            (define-key matlab-mode-map "\M-;" 'comment-dwim)))

;;-----------------------------------------------------------------------
;; Latex programming
;;-----------------------------------------------------------------------

(defun vlm-hook () (visual-line-mode 1))
(add-hook 'latex-mode-hook 'vlm-hook)

;; (autoload 'flyspell-mode "flyspell" "On-the-fly spelling checker." t)
;; (add-hook 'latex-mode-hook 'flyspell-mode)

;;-----------------------------------------------------------------------
;; Markdown mode
;;-----------------------------------------------------------------------

(require 'markdown-mode)
(add-to-list 'auto-mode-alist '("\\.md\\'" . gfm-mode)) ; github-flavored markdown

;;-----------------------------------------------------------------------
;; Org mode
;;-----------------------------------------------------------------------

;; always open text in org-mode
(add-to-list 'auto-mode-alist '("\\.txt\\'" . org-mode))

(setq org-startup-folded nil)

(defun my-org-right-and-heading ()
  (interactive)
  (org-insert-heading)
  (org-metaright)
  (org-ctrl-c-minus))

(custom-set-faces
 '(org-level-2 ((t (:inherit outline-2 :foreground "color-81"))))
 '(org-level-3 ((t (:inherit outline-3 :foreground "color-137")))))

;;-----------------------------------------------------------------------
;; Mac OS X
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

;; use M-x locate
(setq locate-command "mdfind")

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
;; (define-key my-keys-minor-mode-map (kbd "C-c g") 'git-gutter:toggle)
(define-key my-keys-minor-mode-map (kbd "C-c l") 'linum-mode)
(define-key my-keys-minor-mode-map (kbd "C-c p") 'pabbrev-mode)
(define-key my-keys-minor-mode-map (kbd "C-c v") 'pt-pbpaste)
(define-key my-keys-minor-mode-map (kbd "C-c c") 'pt-pbcopy)
(define-key my-keys-minor-mode-map (kbd "C-c w") 'visual-line-mode)
(define-key my-keys-minor-mode-map (kbd "C-x C-b") 'ibuffer)
(define-key my-keys-minor-mode-map (kbd "C-M-i") 'indent-for-tab-command)
(define-key my-keys-minor-mode-map (kbd "C-j") 'indent-return-indent)
(define-key my-keys-minor-mode-map (kbd "M-j") 'indent-return-comment-indent)
(define-key my-keys-minor-mode-map (kbd "C-c 9") 'org-cycle)
(define-key my-keys-minor-mode-map (kbd "C-c 0") 'org-global-cycle)
(define-key my-keys-minor-mode-map (kbd "M-j") 'org-insert-heading)
(define-key my-keys-minor-mode-map (kbd "C-M-j") 'my-org-right-and-heading)
(define-key my-keys-minor-mode-map (kbd "C-M-f") 'org-metaright)
(define-key my-keys-minor-mode-map (kbd "C-M-b") 'org-metaleft)
(define-key my-keys-minor-mode-map (kbd "C-c m") 'magit-status)
;; (define-key my-keys-minor-mode-map (kdb "C-<space>") ' ;
(define-key my-keys-minor-mode-map (kbd "C-c g") 'vc-git-grep)

(define-minor-mode my-keys-minor-mode
  "A minor mode so that my key settings override annoying major
  modes." t " my-keys" 'my-keys-minor-mode-map)
(my-keys-minor-mode 1)

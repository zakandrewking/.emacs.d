;;-----------------------------------------------------------------------
;; packages
;;-----------------------------------------------------------------------

(when (>= emacs-major-version 24)
  (require 'cl)
  (require 'package)
  (add-to-list 'package-archives
	       '("melpa" . "http://melpa.milkbox.net/packages/") t)
  (package-initialize)

  (defvar required-packages ()
    "a list of packages to ensure are installed at launch.")
  (setq required-packages '(evil magit deft key-chord js2-mode
  browse-kill-ring yaml-mode ag smart-mode-line web-mode auctex
  ess evil-surround deft markdown-mode auto-complete yasnippet
  sql-indent multi-term json-mode))

  ;; method to check if all packages are installed
  (defun packages-installed-p ()
    (loop for p in required-packages
		  when (not (package-installed-p p)) do (return nil)
		  finally (return t)))

  ;; if not all packages are installed, check one by one and install the missing
  ;; ones.
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
  ;; key chord
  (key-chord-mode 1)
  (key-chord-define evil-insert-state-map "jk" 'evil-normal-state)
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
  
  ;; smart-mode-line
  (setq sml/no-confirm-load-theme t)
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
  (add-hook 'python-mode-hook (lambda () (modify-syntax-entry ?_ "w")))

  ;; web-mode
  (add-to-list 'auto-mode-alist '("\\.html\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.jinja2\\'" . web-mode))
  (setq web-mode-engines-alist '(("django" . "\\.html\\'")))

  ;; browse-kill-ring
  (browse-kill-ring-default-keybindings)
  
  ;; yaml-mode
  (add-to-list 'auto-mode-alist '("\\.ya?ml\\'" . yaml-mode))

  ;; sr-speedbar
  ;; (setq resize-mini-windows nil)
  ;; (setq speedbar-use-images nil)
  ;; (setq sr-speedbar-auto-refresh nil)
  ;; (setq sr-speedbar-max-width 100)
  ;; (setq sr-speedbar-width-console 50)
  ;; (setq sr-speedbar-width-x 50)

  ;; deft-mode
  (setq deft-directory "~/Dropbox (Personal)/PlainText/")
  (setq deft-extension "txt")
  (setq deft-text-mode 'org-mode)
  (setq deft-use-filename-as-title t)
  (put 'erase-buffer 'disabled nil)
  (put 'toggle-mac-option-modifier 'disabled t)
  (add-hook 'org-mode-hook 'auto-fill-mode)

  ;; markdown
  (add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))
  
  ;; auto-complete
  (global-auto-complete-mode 1)
  ;; sources and faces
  (defun ac-yasnippet-candidates ()
    (if (null ac-prefix)
	(yas-active-keys)
	(all-completions ac-prefix (yas-active-keys))))
  (ac-define-source yasnippet
    '((depends yasnippet)
      (candidates . ac-yasnippet-candidates)
      (action . yas/expand)
      (requires . 1)
      (limit . 5)
      (symbol . "a")))
  (ac-define-source my-words-in-same-mode-buffers
    '((init . ac-update-word-index)
      (candidates . (ac-word-candidates
		     (lambda (buffer)
		       (derived-mode-p (buffer-local-value 'major-mode buffer)))))
      (requires . 3)
      (limit . 5)))
  (setq-default ac-sources '(ac-source-yasnippet
			     ac-source-my-words-in-same-mode-buffers))
  ;; modes to activate ac-mode
  (add-to-list 'ac-modes 'latex-mode)
  (add-to-list 'ac-modes 'lisp-mode)
  (add-to-list 'ac-modes 'python-mode)
  (add-to-list 'ac-modes 'matlab-mode)
  (add-to-list 'ac-modes 'shell-script-mode)
  (add-to-list 'ac-modes 'js2-mode)
  (add-to-list 'ac-modes 'markdown-mode)
  (add-to-list 'ac-modes 'haskell-mode)
  ;; C-n C-p for next/previous expansion
  (define-key ac-completing-map (kbd "C-n") 'ac-next)
  (define-key ac-completing-map (kbd "C-p") 'ac-previous)
  ;; don't choose next candidate with tab
  (define-key ac-complete-mode-map (kbd "TAB") 'ac-expand-common)
  ;; show immediately, with fewer options and only after 4 chars
  (setq ac-auto-show-menu 0.0)
  (setq ac-delay 0.0)
  (setq ac-candidate-menu-min 0)
  ;; (setq ac-auto-start 4) ;; redundant
  ;; (setq ac-candidate-limit 5)
  ;; enable ac even in strings, comments, and docs
  (setq ac-disable-faces nil)
  
  ;; yasnippet
  (yas-global-mode 1)
  (define-key yas-minor-mode-map (kbd "<tab>") nil)
  (define-key yas-minor-mode-map (kbd "TAB") nil)
  (define-key yas-minor-mode-map (kbd "<backtab>") 'yas-expand) ; shift-tab

  ;; sql-indent
  (eval-after-load "sql"
    (load-library "sql-indent")) 

  ;; json-mode
  (add-to-list 'auto-mode-alist '("\\.json\\'" . json-mode))
  )

;;-----------------------------------------------------------------------
;; setup
;;-----------------------------------------------------------------------

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
(setq recentf-save-file "~/.emacs.d/recentf")
(setq indent-tabs-mode nil)

;; start emacsserver
(server-start)

;; color theme
;; Should match terminal color theme,
;; or at least add `export TERM=xterm-256color` to your bash_profile.
(load-theme 'wombat t)

;; date and time
(custom-set-variables
 '(display-time-day-and-date t)
 '(display-time-default-load-average nil))
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

;; color shell command outputs
;; 
;; e.g. in dired mode, call ! on a file, give `jq -C '.'`, and see a colorful
;; output
;; (require 'ansi-color)

;; (defadvice display-message-or-buffer (before ansi-color activate)
;;   "Process ANSI color codes in shell output."
;;   (let ((buf (ad-get-arg 0)))
;;     (and (bufferp buf)
;;          (string= (buffer-name buf) "*Shell Command Output*")
;;          (with-current-buffer buf
;;            (ansi-color-apply-on-region (point-min) (point-max))))))

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

;; recent files
(require 'recentf)
(recentf-mode t)
(setq recentf-max-saved-items 150)
(defun ido-recentf-open ()
  "Use `ido-completing-read' to \\[find-file] a recent file"
  (interactive)
  (if (find-file (ido-completing-read "Find recent file: " recentf-list))
      ()
    (message "Aborting")))
;; TODO make magic commands to enter recentf from ido buffer and find-file

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
;; ERC
;;-----------------------------------------------------------------------

;; check channels
(setq erc-track-exclude-types '("JOIN" "NICK" "PART" "QUIT" "MODE"
                                "324" "329" "332" "333" "353" "477"))
;; don't show any of this
(setq erc-hide-list '("JOIN" "PART" "QUIT" "NICK"))

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

(require 'tex)
(add-hook 'LaTeX-mode-hook (lambda () (visual-line-mode 1)))

(defun use-default-paragraph-delimiters ()
  (setq paragraph-start (default-value 'paragraph-start)
        paragraph-separate (default-value 'paragraph-separate)))
(add-hook 'LaTeX-mode-hook 'use-default-paragraph-delimiters)

;; save automatically
(setq TeX-save-query nil)

;; (autoload 'flyspell-mode "flyspell" "On-the-fly spelling checker." t)
;; (add-hook 'latex-mode-hook 'flyspell-mode)

;;-----------------------------------------------------------------------
;; Org mode
;;-----------------------------------------------------------------------

;; always open text in org-mode
(add-to-list 'auto-mode-alist '("\\.txt\\'" . org-mode))

(setq org-startup-folded nil)

;; refile to beginning of groups
(setq org-reverse-note-order t)

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
;; (define-key my-keys-minor-mode-map "\C-x\ \C-r" 'find-file-read-only)
(define-key my-keys-minor-mode-map "\C-x\ \C-r" 'ido-recentf-open) 
;; (define-key my-keys-minor-mode-map (kbd "M-<backspace>") 'my-backward-kill-word)
;; (define-key my-keys-minor-mode-map (kbd "M-<up>") 'backward-paragraph)
;; (define-key my-keys-minor-mode-map (kbd "M-<down>") 'forward-paragraph)
(define-key my-keys-minor-mode-map "\C-xo" 'my-other-window)
(define-key my-keys-minor-mode-map "\C-co" 'switch-to-minibuffer)
(define-key my-keys-minor-mode-map "\C-x\ \C-k" 'kill-buffer)
;; (define-key my-keys-minor-mode-map (kbd "C-c s") 'sr-speedbar-toggle)
;; (define-key my-keys-minor-mode-map (kbd "C-c g") 'git-gutter:toggle)
(define-key my-keys-minor-mode-map (kbd "C-c l") 'linum-mode)
(define-key my-keys-minor-mode-map (kbd "C-c p") 'pabbrev-mode)
(define-key my-keys-minor-mode-map (kbd "C-c a") 'auto-complete-mode)
(define-key my-keys-minor-mode-map (kbd "C-c v") 'pt-pbpaste)
(define-key my-keys-minor-mode-map (kbd "C-c c") 'pt-pbcopy)
(define-key my-keys-minor-mode-map (kbd "C-c w") 'visual-line-mode)
(define-key my-keys-minor-mode-map (kbd "C-x C-b") 'ibuffer)
(define-key my-keys-minor-mode-map (kbd "C-M-i") 'indent-for-tab-command)
(define-key my-keys-minor-mode-map (kbd "C-j") 'indent-return-indent)
;; (define-key my-keys-minor-mode-map (kbd "M-j") 'indent-return-comment-indent)
(define-key my-keys-minor-mode-map (kbd "C-c 9") 'org-cycle)
(define-key my-keys-minor-mode-map (kbd "C-c 0") 'org-global-cycle)
(define-key my-keys-minor-mode-map (kbd "M-j") 'org-insert-heading)
(define-key my-keys-minor-mode-map (kbd "C-M-j") 'my-org-right-and-heading)
(define-key my-keys-minor-mode-map (kbd "C-M-f") 'org-metaright)
(define-key my-keys-minor-mode-map (kbd "C-M-b") 'org-metaleft)
(define-key my-keys-minor-mode-map (kbd "C-c m") 'magit-status)
;; (define-key my-keys-minor-mode-map (kbd "C-<space>") ' ;
(define-key my-keys-minor-mode-map (kbd "C-c g") 'vc-git-grep)

(define-minor-mode my-keys-minor-mode
  "A minor mode so that my key settings override annoying major
  modes." t " my-keys" 'my-keys-minor-mode-map)
(my-keys-minor-mode 1)

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
(setq clean-buffer-list-deelay-general 0)
(setq magic-mode-alist ())

;; http://whattheemacsd.com/file-defuns.el-01.html
(defun rename-current-buffer-file ()
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
			    (load "~/.emacs.d/el-get/nxhtml/autostart.el")
			    ))
	  )
  )
  (setq my-packages
        (append
         '(el-get matlab-mode nxhtml)
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

;;-----------------------------------------------------------------------
;; movement
;;-----------------------------------------------------------------------

;; kill the characters from the cursor to the beginning of the current line
;; C-u
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

;;-----------------------------------------------------------------------
;; aquamacs
;;-----------------------------------------------------------------------

(when (featurep 'aquamacs)
  (tool-bar-mode -1)
  (tooltip-mode -1)
  (global-linum-mode t))
;; '(aquamacs-additional-fontsets nil t)
;; '(aquamacs-autoface-mode nil)
;; '(aquamacs-customization-version-id 215 t)
;; '(aquamacs-tool-bar-user-customization nil t)
;; '(default-frame-alist (quote ((tool-bar-lines . 0) (fringe) (internal-border-width . 0) (vertical-scroll-bars . right) (cursor-type . box) (menu-bar-lines . 1) (left-fringe . 4) (right-fringe . 0) (background-color . "Grey15") (background-mode . dark) (border-color . "Grey") (cursor-color . "Grey") (foreground-color . "Grey") (mouse-color . "Grey"))))
;; '(ns-tool-bar-display-mode (quote both) t)
;; '(ns-tool-bar-size-mode (quote regular) t)
;; '(tabbar-mode nil nil (tabbar))
;; '(visual-line-mode nil t))

;;-----------------------------------------------------------------------
;; appearance
;;-----------------------------------------------------------------------

;; set-frame-font

(setq resize-mini-windows nil)
(setq speedbar-use-images nil)
(setq sr-speedbar-auto-refresh nil)
(setq sr-speedbar-max-width 100)
(setq sr-speedbar-width-console 50)
(setq sr-speedbar-width-x 50)

; Set cursor color
;; (set-cursor-color "#bb584b") 

(when (featurep 'aquamacs)
  ;; set global font
  (set-default-font "-apple-Inconsolata-medium-normal-normal-*-17-*-*-*-m-0-iso10646-1")

  ;; set color theme
  (require 'color-theme)
  (color-theme-initialize)
  (color-theme-charcoal-black)
  (set-face-background 'modeline "#FFEF94"))

;; line highlight color
(global-hl-line-mode 0)
;; (set-face-background 'hl-line "#222222")

;; highlight lines that are too long
;; (require 'whitespace)
;; (whitespace-mode nil)
;; (defun whitespace-hook ()
;;   (interactive)
;;   (setq whitespace-style '(face lines-tail))
;;   (whitespace-mode t))
;; (add-hook 'c-mode-hook                       'whitespace-hook)

;; (require 'minimap)
;;  * Use 'M-x minimap-create' in a buffer you're currently editing.
;;  * Use 'M-x minimap-kill' to kill the minimap.

(require 'sr-speedbar)

;; (require 'highlight-indentation)
;; (add-hook 'python-mode-hook 'highlight-indentation-mode)
;; (set-face-background 'highlight-indentation-face "#222222")
;; (set-face-background 'highlight-indentation-current-column-face "#222222")

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
;; anything.el
;;-----------------------------------------------------------------------

;; (require 'anything-match-plugin)
;; (require 'anything-config)
;; (defun my-anything ()
;;   (interactive)
;;   (anything-other-buffer
;;    '(
;;      ;; anything-c-source-locate
;;      anything-c-source-buffers
;;      anything-c-source-file-name-history
;;      anything-c-source-files-in-current-dir
;;      ;; anything-c-source-info-pages
;;      ;; anything-c-source-info-elisp
;;      ;; anything-c-source-man-pages
;;      anything-c-source-emacs-commands
;;      )
;;    " *my-anything*"))
;; (global-set-key (kbd "C-'") 'my-anything)

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

;; (defun my-tab-fix ()
;;   (local-set-key [tab] 'indent-or-expand))


;; (add-hook 'c-mode-hook          'my-tab-fix)
;; (add-hook 'sh-mode-hook         'my-tab-fix)
;; (add-hook 'emacs-lisp-mode-hook 'my-tab-fix)
;; (add-hook 'LaTeX-mode-hook      'my-tab-fix)
;; (add-hook 'matlab-mode-hook     'my-tab-fix)
;; (add-hook 'python-mode-hook     'my-tab-fix)
;; more mode hooks, yada yada, etc ...

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

;; for GDB/debugging in general
;; (global-set-key (kbd "<f10>") 'gud-cont) ;; C-c C-r
;; (global-set-key (kbd "<f9>") 'gud-step);; equiv matlab step in
;; (global-set-key (kbd "<f8>") 'gud-next) ;; equiv matlab step 1
;; (globa l-set-key (kbd "<f7>") 'gud-finish) ;; equiv matlab step out

;; tab to column of mark with M-<tab>

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

;; (setq whitespace-action '(auto-cleanup)) ;; automatically clean up bad whitespace
;; (setq whitespace-style '(trailing space-before-tab indentation empty space-after-tab)) ;; only show bad whitespace

;;-----------------------------------------------------------------------
;; shell programming
;;-----------------------------------------------------------------------

(add-to-list 'auto-mode-alist '("\\.sh\\'" . shell-script-mode))
(add-to-list 'auto-mode-alist '("\\.pbs\\'" . shell-script-mode))

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

;; is this working?? installed in .emacs.d/lisp and info file /usr/share/info/latex2e.info
;; (define-key help-map "\C-l" 'latex-help)
;; (add-hook 'Latex-mode-hook
;;           (function (lambda ()
;;                       (define-key LaTeX-mode-map "\C-ci" 'latex-help))))
;; http://emacswiki.org/emacs/AUCTeX

(put 'dired-find-alternate-file 'disabled nil)

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

;; http://pedrokroger.net/2010/07/configuring-emacs-as-a-python-ide-2/
;; http://curiousprogrammer.wordpress.com/2009/03/27/emacs-comint/

;; (require 'comint)
;; (define-key comint-mode-map [(meta p)]
;;    'comint-previous-matching-input-from-input)
;; (define-key comint-mode-map [(meta n)]
;;    'comint-next-matching-input-from-input)
;; (define-key comint-mode-map [(control meta n)]
;;     'comint-next-input)
;; (define-key comint-mode-map [(control meta p)]
;;     'comint-previous-input)


;; (setq comint-completion-autolist t
;;                                      ;list possibilities on partial
;;                                      ;completion
;;        comint-completion-recexact nil
;;                                      ;use shortest compl. if
;;                                      ;characters cannot be added
;;        ;; how many history items are stored in comint-buffers (e.g. py-shell)
;;        ;; use the HISTSIZE environment variable that shells use (if avail.)
;;        ;; (default is 32)
;;        comint-input-ring-size (string-to-number (or (getenv "HISTSIZE") "100")))

;; (setq ipython-command "/usr/local/bin/ipython")
;; (setq py-shell-name "ipython")

;; (setq py-python-command-args '("-pylab" "-colors" "LightBG"))
;; (setq ipython-completion-command-string "print(';'.join(get_ipython().Completer.complete('%s')[1])) #PYTHON-MODE SILENT\n")
;; (require 'anything-ipython)
;; (add-hook 'python-mode-hook #'(lambda ()
;;                                 (define-key py-mode-map (kbd "<tab>") 'anything-ipython-complete)))
;; (add-hook 'ipython-shell-hook #'(lambda ()
;;                                   (define-key py-mode-map (kbd "<tab>") 'anything-ipython-complete)))

;; (defun py-clear ()
;;   (interactive)
;;   (let ((comint-buffer-maximum-size 0))
;;     (comint-truncate-buffer)))

;;-----------------------------------------------------------------------
;; Markdown mode
;;-----------------------------------------------------------------------
(require 'markdown-mode)
;; (autoload 'markdown-mode "markdown-mode.el"
;;   "Major mode for editing Markdown files" t)
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

;;-----------------------------------------------------------------------
;; key bindings
;;-----------------------------------------------------------------------

(defvar my-keys-minor-mode-map (make-keymap) "my-keys-minor-mode keymap.")

(define-key my-keys-minor-mode-map (kbd "C-c d") 'deft)
(define-key my-keys-minor-mode-map (kbd "C-c q") 'auto-fill-mode)
(define-key my-keys-minor-mode-map "\C-u" 'backward-kill-line)
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

(define-minor-mode my-keys-minor-mode
  "A minor mode so that my key settings override annoying major modes."
  t " my-keys" 'my-keys-minor-mode-map)
(my-keys-minor-mode 1)

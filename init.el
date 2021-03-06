;-----------------------------------------------------------------------
;; variables
;;-----------------------------------------------------------------------

(defvar common-editing-modes
    (list 'latex-mode 'lisp-mode 'emacs-lisp-mode 'python-mode 'matlab-mode
    'sh-mode 'js2-mode 'markdown-mode 'haskell-mode 'org-mode 'c-mode 'css-mode
    'web-mode 'typescript-mode 'plantuml-mode))

;; for evil-collection, set this before loading evil
(setq evil-want-integration nil)

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
  (setq required-packages '(evil magit deft key-chord js2-mode browse-kill-ring
                                 yaml-mode ag smart-mode-line web-mode auctex
                                 ess evil-surround deft markdown-mode
                                 auto-complete yasnippet sql-indent multi-term
                                 ido-completing-read+ expand-region elm-mode smex
                                 org-download matlab-mode edit-server json-mode
                                 fill-column-indicator gams-mode tide csv-mode
                                 ac-etags dockerfile-mode company
                                 evil-vimish-fold plantuml-mode flycheck-mypy
                                 evil-magit evil-collection))

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
  (load "~/.emacs.d/evil-setup.el")

  ;; JavaScript setup
  (load "~/.emacs.d/javascript-setup.el")

  ;; TypeScript & tide setup
  (load "~/.emacs.d/typescript-setup.el")

  ;; key chord
  (key-chord-mode 1)
  ;; quick buffer switch
  (defun switch-to-last-buffer ()
    (interactive)
    (switch-to-buffer (other-buffer (current-buffer) 1)))
  (key-chord-define-global "hl" 'switch-to-last-buffer)

  ;; magit
  ;; Use H in diff to refine hunk (e.g. show word diff)
  (define-key magit-status-mode-map (kbd "H") 'magit-diff-toggle-refine-hunk)
  ;; don't check when pushing
  (setq magit-push-always-verify nil)

  ;; smart-mode-line
  (setq sml/no-confirm-load-theme t)
  (sml/setup)
  (sml/apply-theme 'respectful)

  ;; python
  (add-hook 'python-mode-hook (lambda () (modify-syntax-entry ?_ "w")))

  ;; python flycheck
  (setq-default flycheck-disabled-checkers '(python-flake8
                                             python-pylint
                                             python-pycompile))

  ;; pycodestyle from https://github.com/piger/flycheck-pycodestyle
  (require 'flycheck)
  (flycheck-define-checker python-pycodestyle
    "A Python syntax and style checker using pycodestyle (former pep8)."
    :command ("pycodestyle" source-inplace)
    :error-patterns
    ((error line-start (file-name) ":" line ":" column ":" (message) line-end))
    :modes python-mode)
  (add-to-list 'flycheck-checkers 'python-pycodestyle)

  ;; mypy default followed by pycodestyle
  (require 'flycheck-mypy)
  (setq flycheck-python-mypy-args '("--follow-imports=silent"
                                    "--ignore-missing-imports"))
  (flycheck-add-next-checker 'python-mypy 'python-pycodestyle)
  (add-hook 'python-mode-hook (lambda  ()
                                (flycheck-mode t)
                                (flycheck-select-checker 'python-mypy)))

  ;; web-mode
  (require 'web-mode)
  (add-to-list 'auto-mode-alist '("\\.html\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.jinja2\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.css\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.tsx\\'" . web-mode))
  (setq web-mode-engines-alist '(("django" . "\\.html\\'")))
  (setq web-mode-enable-css-colorization t)
  (add-hook 'web-mode-hook
            (lambda ()
              (setq tab-width 2)
              (setq indent-tabs-mode nil)
              (setq web-mode-markup-indent-offset 2)
              (setq web-mode-css-indent-offset 2)
              (setq web-mode-code-indent-offset 2)
              (setq web-mode-indent-style 2)))
  ;; comment style
  (add-to-list 'web-mode-comment-formats '("javascript" . "//" )) ; TODO does this work?
  ;; auctex style block commands
  (define-key web-mode-map (kbd "C-c C-e") 'web-mode-element-insert)
  (define-key web-mode-map (kbd "C-c ]") 'web-mode-element-close)
  (define-key web-mode-map (kbd "C-M-a") 'web-mode-element-beginning)
  (define-key web-mode-map (kbd "C-M-e") 'web-mode-element-end)
  (define-key web-mode-map (kbd "C-u C-c C-e") 'web-mode-element-rename)
  ;; default paragraph definition for auctex
  (defun use-default-paragraph-delimiters ()
    (setq paragraph-start (default-value 'paragraph-start)
          paragraph-separate (default-value 'paragraph-separate)))
  (add-hook 'tex-mode-hook 'use-default-paragraph-delimiters)

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
  (require 'deft)
  (setq deft-directory "~/Dropbox (Personal)/notes/")
  (setq deft-extension "txt")
  (setq deft-text-mode 'org-mode)
  (setq deft-use-filename-as-title t)
  (setq deft-use-filter-string-for-filename t)
  (put 'erase-buffer 'disabled nil)
  (put 'toggle-mac-option-modifier 'disabled t)
  (add-hook 'org-mode-hook 'auto-fill-mode)
  (defun deft-enter-insert-mode ()
    ;; delay seems necessary
    (run-at-time "0.1 sec" nil 'evil-insert-state))
  (add-hook 'deft-mode-hook 'deft-enter-insert-mode)
  ;; fix TAB
  (define-key deft-mode-map (kbd "TAB") 'widget-forward)
  (define-key deft-mode-map (kbd "C-<return>") 'deft-new-file)
  (define-key deft-mode-map (kbd "C-M-<return>") 'deft-new-file-named)
  ;; capture
  (setq org-default-notes-file (concat deft-directory "CAPTURE.txt"))
  ;; This fix may be necessary so deft width doesn't stretch too far:
  ;; https://github.com/timvisher/deft/issues/1
  ;;
  ;;    (defun deft-buffer-setup ()
  ;;    "Render the file browser in the *Deft* buffer."
  ;; -  (setq deft-window-width (window-width))
  ;; +  (setq deft-window-width (- (window-width) 2))
  ;;    (let ((inhibit-read-only t))
  ;;      (erase-buffer))
  ;;    (remove-overlays)
  ;;
  ;; After making the change, run C-u 0 M-x byte-compile-file deft.el

  ;; markdown
  (require 'markdown-mode)
  (add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))
  (defun run-build ()
    (interactive)
    (save-buffer)
    (message "Building")
    (shell-command "./build"))
  (define-key markdown-mode-map (kbd "C-c C-c") 'run-build)

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
  ;; (ac-define-source my-words-in-same-mode-buffers
  ;;   '((init . ac-update-word-index)
  ;;     (candidates . (ac-word-candidates
  ;;                    (lambda (buffer)
  ;;                      (derived-mode-p (buffer-local-value 'major-mode buffer)))))
  ;;     (requires . 3)
  ;;     (limit . 5)))
  (ac-etags-setup)
  (custom-set-faces
   '(ac-etags-candidate-face ((t (:inherit ac-candidate-face))))
   '(ac-etags-selection-face ((t (:inherit ac-selection-face)))))
  (setq-default ac-sources '(ac-source-yasnippet
                             ac-source-words-in-buffer
                             ;; ac-source-etags
                             ))
                             ;; ac-source-my-words-in-same-mode-buffers))
  ;; modes to activate ac-mode
  (defun setup-ac (mode)
    (add-to-list 'ac-modes mode))
  (mapcar 'setup-ac common-editing-modes)

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
  (setq yas-snippet-dirs (list "~/.emacs.d/snippets"))
  (define-key yas-minor-mode-map (kbd "<tab>") nil)
  (define-key yas-minor-mode-map (kbd "TAB") nil)
  (define-key yas-minor-mode-map (kbd "<backtab>") 'yas-expand) ; shift-tab

  ;; sql-indent
  (eval-after-load "sql"
    (load-library "sql-indent"))

  ;; ido for easy buffer switching
  (require 'ido)
  (ido-mode t)
  (ido-everywhere 1)
  (setq ido-case-fold t)
  ;; ido-ubiquitous
  (require 'ido-completing-read+)
  (ido-ubiquitous-mode 1)
  ;; increase this for unicode completion with C-x 8 RET
  (setq ido-cr+-max-items 100000)

  ;; smex
  (global-set-key (kbd "M-x") 'smex)
  (global-set-key (kbd "M-X") 'smex-major-mode-commands)

  ;; org-download - drag-and-drop download settings
  (require 'org-download)
  (setq-default org-download-heading-lvl nil)
  (setq-default org-download-image-dir "IMG")

  ;; no menu bar (wasn't working higher up)
  (menu-bar-mode -1)
  (fringe-mode -1)

  ;; edit server for Chrome
  (when (require 'edit-server nil t)
    (setq edit-server-new-frame nil)
    (edit-server-start))

  ;; fill column. Turn off fci with auto-complete to avoid odd display bugs
  ;; https://github.com/alpaker/Fill-Column-Indicator/issues/21
  ;; (defvar sanityinc/fci-mode-suppressed nil)
  ;; (defadvice popup-create (before suppress-fci-mode activate)
  ;;   "Suspend fci-mode while popups are visible"
  ;;   (set (make-local-variable 'sanityinc/fci-mode-suppressed) fci-mode)
  ;;   (when fci-mode
  ;;     (turn-off-fci-mode)))
  ;; (defadvice popup-delete (after restore-fci-mode activate)
  ;;   "Restore fci-mode when all popups have closed"
  ;;   (when (and (not popup-instances) sanityinc/fci-mode-suppressed)
  ;;     (setq sanityinc/fci-mode-suppressed nil)
  ;;     (turn-on-fci-mode)))
  ;; (fci-mode 1) ;; don't activate because of annoying fringe indicators on
  ;; narrow windows

  ;; gams-mode
  (add-to-list 'auto-mode-alist '("\\.ga?ms\\'" . gams-mode))

  ;; csv mode
  (add-to-list 'auto-mode-alist '("\\.tsv\\'" . csv-mode))
  (add-to-list 'auto-mode-alist '("\\.csv\\'" . csv-mode))

  ;; set separators based on file extension
  (make-variable-buffer-local 'csv-separators)
  (make-variable-buffer-local 'csv-separator-chars)
  (make-variable-buffer-local 'csv--skip-regexp)
  (make-variable-buffer-local 'csv-separator-regexp)
  (make-variable-buffer-local 'csv-font-lock-keywords)
  (defun set-csv-separators ()
    (let ((ext (file-name-extension (buffer-file-name))))
      (if (or (equal ext "tsv") (equal ext "txt"))
          (setq csv-separators '("	"))
        (setq csv-separators '(","))
        )
      ;; this is straight from csv-mode.el
      (setq csv-separator-chars (mapcar 'string-to-char csv-separators)
            csv--skip-regexp (apply 'concat "^\n" csv-separators)
            csv-separator-regexp (apply 'concat `("[" ,@csv-separators "]"))
            csv-font-lock-keywords
            `((,csv-separator-regexp (0 'csv-separator-face))))
      ;; disable minor modes for speed
      (font-lock-mode -1)
      ))
  (add-hook 'csv-mode-hook 'set-csv-separators)

  ;; plantuml-mode
  (require 'plantuml-mode)
  (defun my-plantuml-preview ()
    "Preview plantuml in a new pane"
    (interactive)
    (save-buffer)
    (plantuml-preview 4)
    )
  (define-key plantuml-mode-map (kbd "C-c p") 'my-plantuml-preview)
  )

;;-----------------------------------------------------------------------
;; setup
;;-----------------------------------------------------------------------

;; emacs settings
(put 'upcase-region 'disabled nil)
(desktop-save-mode 1)

(setq magic-mode-alist ())
(setq-default tab-width 4)
(setq-default indent-tabs-mode nil)
(setq large-file-warning-threshold 5000000)
(global-linum-mode 0) ; linum-mode off
(setq linum-format "%3d ")
(column-number-mode t)
(line-number-mode t)
(global-hl-line-mode 0) ; line highlight color
(setq recentf-save-file "~/.emacs.d/recentf")
(set-default 'truncate-lines t)

;; never pop up new frames
(setq ns-pop-up-frames nil)

;; scroll one line at a time (less "jumpy" than defaults)
(setq mouse-wheel-scroll-amount '(2 ((shift) . 1))) ;; two lines at a time
(setq mouse-wheel-progressive-speed nil) ;; don't accelerate scrolling
(setq mouse-wheel-follow-mouse 't) ;; scroll window under mouse

;; start emacsserver
(server-start)

;; color theme
;; Should match terminal color theme,
;; or at least add `export TERM=xterm-256color` to your bash_profile.
(load-theme 'wombat t)
(custom-set-faces
 '(default ((t (:background "gray14")))))

;; date and time
(custom-set-variables
 '(display-time-day-and-date t)
 '(display-time-default-load-average nil))
(display-time)

;; parens. automatically pair parens, but make it buffer local
;; http://emacs.stackexchange.com/questions/5981/how-to-make-electric-pair-mode-buffer-local
(defun my-inhibit-electric-pair-mode (char)
  (not (member major-mode common-editing-modes)))
(setq electric-pair-inhibit-predicate #'my-inhibit-electric-pair-mode)
(electric-pair-mode 1)

;; remove trailing whitespace
(defun remove-trailing-whitepace-on-save ()
  (add-hook 'before-save-hook 'delete-trailing-whitespace nil t))
(mapcar
 (lambda (hook) (add-hook hook 'remove-trailing-whitepace-on-save))
 (list 'LaTeX-mode-hook 'lisp-mode-hook 'python-mode-hook
       'matlab-mode-hook 'sh-mode-hook 'js2-mode-hook
       'markdown-mode-hook 'haskell-mode-hook 'c-mode-hook
       'css-mode-hook 'web-mode-hook 'rst-mode-hook
       'typescript-mode-hook 'emacs-lisp-mode-hook))

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

;; remake the scratch buffer
;; http://stackoverflow.com/questions/234963/re-open-scratch-buffer-in-emacs
(setq initial-scratch-message "")
(run-with-idle-timer 60 t (lambda () (get-buffer-create "*scratch*")))

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
;; System-specific functions
;;-----------------------------------------------------------------------

;; Terminal emacs enable mouse support
;; (unless window-system ;; emacs daemon runs both terminal and gui emacs
(require 'mouse)
(xterm-mouse-mode t)
(global-set-key [mouse-4] (lambda ()
                            (interactive)
                            (scroll-down 1)))
(global-set-key [mouse-5] (lambda ()
                            (interactive)
                            (scroll-up 1)))
(defun track-mouse (e))
(setq mouse-sel-mode t)
;; )

;; Mac OS X use M-x locate
(setq locate-command "mdfind")

;; Mac OS X system copy paste
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

(defun pt-cut()
  "Cut region and put on OS X system pasteboard."
  (interactive)
  (pt-pbcopy)
  (delete-region (region-beginning) (region-end)))
;; GUI Emacs
;; (when window-system ;; run this code when the emacs daemon starts too
;; don't use Mac OSX full screen
(setq ns-use-native-fullscreen nil)
;; font
(set-face-attribute 'default nil :family "Inconsolata"
                    :height 170 :weight 'normal)

;; turn off toolbar and scrollbar
(tool-bar-mode -1)
(scroll-bar-mode -1)
;; command-enter for fullscreen
(global-set-key (kbd "<s-return>") 'toggle-frame-fullscreen)
;; no blinking cursor
(blink-cursor-mode 0)
;; PATH
(setenv "PATH"
        (concat
         "/usr/local/bin" ":"
         "/Library/TeX/texbin" ":"
         (getenv "PATH")))
(setq exec-path (append exec-path '("/usr/local/bin")))

;; Separate the clipboard. Adapted from:
;; http://stackoverflow.com/questions/22849281/on-emacs-for-osx-how-to-keep-kill-ring-and-clipboard-separate
(defun isolate-kill-ring()
  "Isolate Emacs kill ring from OS X system pasteboard."
  (interactive)
  (setq interprogram-cut-function nil)
  (setq interprogram-paste-function nil)
  ;; http://stackoverflow.com/questions/17127009/how-to-disable-emacs-evil-selection-auto-copies-to-clipboard
  (defun x-select-text (text))
  (setq x-select-enable-clipboard nil)
  (setq x-select-enable-primary nil))

;; separate kill ring from clipboard
(isolate-kill-ring)
;; bind CMD+C to pasteboard-copy
(global-set-key (kbd "s-c") 'pt-pbcopy)
;; bind CMD+V to pasteboard-paste
(global-set-key (kbd "s-v") 'pt-pbpaste)
;; bind CMD+X to pasteboard-cut
(global-set-key (kbd "s-x") 'pt-cut)
;; )

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

;; stop annoying "Command attempted to use minibuffer while in minibuffer."
;; http://trey-jackson.blogspot.com/2010/04/emacs-tip-36-abort-minibuffer-when.html
(defun stop-using-minibuffer ()
  "kill the minibuffer"
  (when (and (>= (recursion-depth) 1) (active-minibuffer-window))
    (abort-recursive-edit)))

(add-hook 'mouse-leave-buffer-hook 'stop-using-minibuffer)

;; comment line
;; (defun comment-line ()
;;   "Comment the current line"
;;   (interactive)
;;   (let (beg end)
;;     (if (use-region-p)
;;         (progn
;;           ;; TODO change region to whole line
;;           (setq beg (region-beginning) end (region-end)))
;;       (setq beg (line-beginning-position) end (line-end-position)))
;;     (comment-region beg end)))
;; (comment-line)
;; (define-key org-mode-map (kbd "C-M-;") 'comment-line)

;;-----------------------------------------------------------------------
;; Regular expressions
;;-----------------------------------------------------------------------

;; use string regex style
(require 're-builder)
(setq reb-re-syntax 'string)

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

;; split and switch buffers
(defun vsplit-last-buffer ()
  (interactive)
  (if (eq (length (window-list)) 1)
      (progn
        (split-window-vertically)
        (other-window 1 nil)
        (switch-to-next-buffer))
    (delete-window)))

(defun hsplit-last-buffer ()
  (interactive)
  (if (eq (length (window-list)) 1)
      (progn
        (split-window-horizontally)
        (other-window 1 nil)
        (switch-to-next-buffer))
    (delete-window)))

;;-----------------------------------------------------------------------
;; text management
;;-----------------------------------------------------------------------

;; (defun indent-return-indent ()
;;   "indent current line, then return, and intent next line"
;;   (interactive)
;;   (indent-for-tab-command)
;;   (newline-and-indent))

;; (defun indent-return-comment-indent ()
;;   "indent current line, then return new comment line, and intent"
;;   (interactive)
;;   (indent-for-tab-command)
;;   (indent-new-comment-line))

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

;; insert the name of a local file
(defun insert-file-name (filename &optional args)
  "Insert name of file FILENAME into buffer after point.

  Prefixed with \\[universal-argument], expand the file name to
  its fully canocalized path.  See `expand-file-name'.

  Prefixed with \\[negative-argument], use relative path to file
  name from current directory, `default-directory'.  See
  `file-relative-name'.

  The default with no prefix is to insert the file name exactly as
  it appears in the minibuffer prompt."
  ;; Based on insert-file in Emacs -- ashawley 20080926
  (interactive "*fInsert file name: \nP")
  (cond ((eq '- args)
         (insert (file-relative-name filename)))
        ((not (null args))
         (insert (expand-file-name filename)))
        (t
         (insert filename))))

;;-----------------------------------------------------------------------
;; shell programming
;;-----------------------------------------------------------------------

(add-to-list 'auto-mode-alist '("\\.sh\\'" . sh-mode))
(add-to-list 'auto-mode-alist '("\\.pbs\\'" . sh-mode))
(add-to-list 'auto-mode-alist '("\\.ext\\'" . sh-mode))

;;-----------------------------------------------------------------------
;; emacs lisp programming
;;-----------------------------------------------------------------------

;; include dashes in the word definition (especially useful for evil-mode
;; superstar)
(add-hook 'emacs-lisp-mode-hook (lambda () (modify-syntax-entry ?- "w")))

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
;; XML
;;-----------------------------------------------------------------------

;; don't auto-validate
(setq rng-nxml-auto-validate-flag nil)

;;-----------------------------------------------------------------------
;; Latex programming
;;-----------------------------------------------------------------------

(require 'tex)
;; from the manual
(setq TeX-parse-self t)

;; modes to activate
(add-hook 'LaTeX-mode-hook (lambda () (visual-line-mode 1)))
(add-hook 'LaTeX-mode-hook (lambda ()
                             (flyspell-mode 1)
                             (ac-flyspell-workaround)))

;; syntax highlighting
(setq font-latex-match-reference-keywords
      '(("citep" "[{")
        ("citet" "[{")))

(defun use-default-paragraph-delimiters ()
  (setq paragraph-start (default-value 'paragraph-start)
        paragraph-separate (default-value 'paragraph-separate)))
(add-hook 'LaTeX-mode-hook 'use-default-paragraph-delimiters)

;; save automatically
(setq TeX-save-query nil)

;; color sections
(setq font-latex-fontify-sectioning 'color)

;; previews with preview-latex and ghostscript
(setq preview-gs-options
      (quote
       ("-q" "-dNOPAUSE" "-DNOPLATFONTS" "-dPrinted" "-dTextAlphaBits=4"
        "-dGraphicsAlphaBits=4")))

;; help reftex find the bib file
(setq reftex-use-external-file-finders t)
(setq reftex-default-bibliography '("./bib.bib"))

;;-----------------------------------------------------------------------
;; markdown mode
;;-----------------------------------------------------------------------

;; modes to activate
(add-hook 'markdown-mode-hook 'flyspell-mode)
(add-hook 'markdown-mode-hook 'auto-fill-mode)

;;-----------------------------------------------------------------------
;; Org mode
;;-----------------------------------------------------------------------

(require 'org)

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

(defun org-metaright-or-cycle ()
  (interactive)
  (condition-case nil
      (org-metaright)
    (error (org-ctrl-c-minus))))

(custom-set-faces
 '(org-level-2 ((t (:inherit outline-2 :foreground "turquoise1"))))
 '(org-level-3 ((t (:inherit outline-3 :foreground "tan3")))))

(defun org-toggle-checkbox-with-prefix ()
  (interactive)
  (let ((current-prefix-arg '(4)))
    (call-interactively 'org-toggle-checkbox)))

;; beamer export
(require 'ox-latex)
(add-to-list 'org-latex-classes
             '("beamer" "\\documentclass[compress]{beamer}

\\newcounter{slidenumber}
\\setbeamertemplate{footline}[text line]{
  \\parbox{\\linewidth}{
    \\hspace*{-25pt}
    \\includegraphics[width=3cm]{sbrg_logo.pdf}
    \\hfill
    \\vspace*{8pt}
    \\setcounter{slidenumber}{\\insertpagenumber}
    \\addtocounter{slidenumber}{-\\insertframestartpage}
    \\addtocounter{slidenumber}{1}
    \\insertframenumber{} / \\inserttotalframenumber}}
\\setbeamertemplate{navigation symbols}{}

\\usepackage{palatino}
\\usepackage{raleway}

\\usefonttheme[stillsansseriftext]{serif}
\\usecolortheme[RGB={13,88,153}]{structure}
\\setbeamerfont{title}{size=\\huge}

\\defbeamertemplate*{title page}{customized}[1][]
{
  \\centering
  \\vspace{2cm}
  \\usebeamerfont{title}\\inserttitle\\par
  \\usebeamerfont{subtitle}\\usebeamercolor[fg]{subtitle}\\insertsubtitle\\par
  \\vspace{1.5cm}
  \\usebeamerfont{author}\\insertauthor\\par
  \\vspace{0.5cm}
  \\usebeamerfont{date}\\insertdate\\par
}

\\usepackage{natbib}
\\bibliographystyle{apalike}
\\setbeamertemplate{bibliography item}{}
\\renewcommand\\bibfont{\\scriptsize}

% for tabular
\\usepackage{booktabs}

% fixltx2e package for \\textsubscript
\\usepackage{fixltx2e}

% for code
\\usepackage{listings}
\\usepackage{color}
\\definecolor{mygreen}{rgb}{0,0.6,0}
\\definecolor{mygray}{rgb}{0.5,0.5,0.5}
\\definecolor{mymauve}{rgb}{0.58,0,0.82}
\\lstset{
  basicstyle=\\scriptsize,
  commentstyle=\\color{mygreen},    % comment style
  keywordstyle=\\color{blue},       % keyword style
  stringstyle=\\color{mymauve}     % string literal style
}

% align images
\\usepackage[export]{adjustbox}

% To change margin on one slide only
\\newenvironment{changemargin}[2]{%
  \\begin{list}{}{%
      \\setlength{\\topsep}{0pt}%
      \\setlength{\\leftmargin}{#1}%
      \\setlength{\\rightmargin}{#2}%
      \\setlength{\\listparindent}{\\parindent}%
      \\setlength{\\itemindent}{\\parindent}%
      \\setlength{\\parsep}{\\parskip}%
    }%
  \\item[]}{\\end{list}}"
  ("\\section{%s}" . "\\section*{%s}")
  ("\\subsection{%s}" . "\\subsection*{%s}")
  ("\\subsubsection{%s}" . "\\subsubsection*{%s}")))

;; keyboard shortcuts
(define-key org-mode-map (kbd "C-c 9") 'org-cycle)
(define-key org-mode-map (kbd "C-c 0") 'org-global-cycle)
(define-key org-mode-map (kbd "M-j") 'org-insert-heading)
(define-key org-mode-map (kbd "C-M-j") 'my-org-right-and-heading)
(define-key org-mode-map (kbd "C-M-f") 'org-metaright-or-cycle)
(define-key org-mode-map (kbd "C-M-b") 'org-metaleft)
(define-key org-mode-map (kbd "C-c x") 'org-toggle-checkbox-with-prefix)

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
(define-key my-keys-minor-mode-map (kbd "C-c a") 'auto-complete-mode)
(define-key my-keys-minor-mode-map (kbd "C-c v") 'pt-pbpaste)
(define-key my-keys-minor-mode-map (kbd "C-c c") 'pt-pbcopy)
(define-key my-keys-minor-mode-map (kbd "C-c w") 'visual-line-mode)
(define-key my-keys-minor-mode-map (kbd "C-x C-b") 'ibuffer)
;; (define-key my-keys-minor-mode-map (kbd "C-M-i") 'indent-for-tab-command)
;; (define-key my-keys-minor-mode-map (kbd "C-j") 'indent-return-indent)
;; (define-key my-keys-minor-mode-map (kbd "M-j") 'indent-return-comment-indent)
(define-key my-keys-minor-mode-map (kbd "C-c m") 'magit-status)
;; (define-key my-keys-minor-mode-map (kbd "C-<space>") ' ;
;; (define-key my-keys-minor-mode-map (kbd "C-c g") 'vc-git-grep)
(define-key my-keys-minor-mode-map (kbd "C-w '") 'hsplit-last-buffer)
(define-key my-keys-minor-mode-map (kbd "C-w C-'") 'hsplit-last-buffer)
(define-key my-keys-minor-mode-map (kbd "C-w \"") 'vsplit-last-buffer)
(define-key my-keys-minor-mode-map (kbd "C-w C-\"") 'vsplit-last-buffer)
(define-key my-keys-minor-mode-map (kbd "C-w C-k") 'ido-kill-buffer)
(define-key my-keys-minor-mode-map (kbd "s-=") 'text-scale-increase)
(define-key my-keys-minor-mode-map (kbd "s--") 'text-scale-decrease)
(define-key my-keys-minor-mode-map (kbd "s-0") 'text-scale-adjust)
(define-key my-keys-minor-mode-map (kbd "s-/") 'comment-dwim)
(define-key my-keys-minor-mode-map "\C-cg" 'org-capture)

(define-minor-mode my-keys-minor-mode
  "A minor mode so that my key settings override annoying major
  modes." t " my-keys" 'my-keys-minor-mode-map)
(my-keys-minor-mode 1)

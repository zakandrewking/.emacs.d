;; typescript-mode
(add-to-list 'auto-mode-alist '("\\.tsx?\\'" . typescript-mode))

;; company options. pull these out into a generic script
;; (setq company-idle-delay 0.0)
;; (setq company-minimum-prefix-length 1)
;; TODO begin completion on tab with empty space
;; TODO company fall back on ddabrev
;; TODO python eldoc
;; TODO function docs in tide/company. docs for console.log?
;; TODO C-] jump to definition with tide

(setq-default typescript-indent-level 2)

;; (defun setup-tide-mode ()
;;   (interactive)
;;   (tide-setup)
;;   (flycheck-mode +1)
;;   (eldoc-mode +1)
;;   (tide-hl-identifier-mode +1)
;;   ;; company is an optional dependency. You have to
;;   ;; install it separately via package-install
;;   ;; `M-x package-install [ret] company`
;;   (company-mode +1)
;;   (auto-complete-mode -1)
;;   (fringe-mode)
;;   (setq tide-tsserver-executable "node_modules/typescript/bin/tsserver")
;;   (define-key tide-mode-map (kbd "M-d") 'tide-documentation-at-point)
;;   (define-key tide-mode-map (kbd "M-]") 'tide-jump-to-definition)
;;   (define-key tide-mode-map (kbd "M-[") 'tide-jump-back)
;;   (define-key tide-mode-map (kbd "<tab>") 'company-complete))

;; ;; aligns annotation to the right hand side
;; (setq company-tooltip-align-annotations t)

;; ;; formats the buffer before saving
;; ;; (add-hook 'before-save-hook 'tide-format-before-save)

;; (add-hook 'typescript-mode-hook #'setup-tide-mode)

;; (add-hook 'web-mode-hook
;;           (lambda ()
;;             (when (string-equal "tsx" (file-name-extension buffer-file-name))
;;               (setup-tide-mode))))

;; jedi
;; ----
;; jedi:install-server
;; (defun my/python-mode-hook ()
;;   (add-to-list 'company-backends 'company-jedi))
;;
;; (add-hook 'python-mode-hook 'my/python-mode-hook)
;;
;; jedi is still so slow!!!!

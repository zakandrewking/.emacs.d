;; js2-mode setup
(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
(add-to-list 'auto-mode-alist '("\\.json\\'" . javascript-mode))

;; vars
(setq-default js2-strict-inconsistent-return-warning nil)
(setq-default js2-strict-trailing-comma-warning nil)
(setq-default js2-strict-missing-semi-warning nil)
(setq-default js2-global-externs
              '("module" "require" "buster" "sinon" "assert" "refute"
                "setTimeout" "clearTimeout" "setInterval" "clearInterval"
                "location" "__dirname" "console" "JSON"))
;; when breaking strings, put `+' on the first line
(setq-default js2-concat-multiline-strings 'eol)

;; js2-newline with return
(eval-after-load 'js2-mode
  '(define-key js2-mode-map (kbd "RET") 'js2-line-break))

;; include underscores in the word definition (especially useful for evil-mode
;; superstar)
(add-hook 'js2-mode-hook (lambda () (modify-syntax-entry ?_ "w")))

;; After js2 has parsed a js file, we look for jslint globals decl comment ("/* global Fred, _, Harry */") and
;; add any symbols to a buffer-local var of acceptable global vars
;; Note that we also support the "symbol: true" way of specifying names via a hack (remove any ":true"
;; to make it look like a plain decl, and any ':false' are left behind so they'll effectively be ignored as
;; you can;t have a symbol called "someName:false"
;; Adapted from: https://github.com/magnars/.emacs.d/blob/master/settings/setup-js2-mode.el
(add-hook 'js2-post-parse-callbacks
          (lambda ()
            (when (> (buffer-size) 0)
              (let ((btext (replace-regexp-in-string
                            ": *true" " "
                            (replace-regexp-in-string "[\n\t ]+" " " (buffer-substring-no-properties 1 (buffer-size)) t t))))
                (mapc (apply-partially 'add-to-list 'js2-additional-externs)
                      (split-string
                       (if (string-match "/\\* *global *\\(.*?\\) *\\*/" btext) (match-string-no-properties 1 btext) "")
                       " *, *" t))
                ))))

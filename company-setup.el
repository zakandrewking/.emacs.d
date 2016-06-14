;; ymcd installation:
;; brew install cmake node
;; npm install -g typescript
;; git clone git@github.com:Valloric/ycmd.git
;; cd ycmd
;; git submodule update --init --recursive
;; ./build --all

(require 'ycmd)
(add-hook 'after-init-hook #'global-ycmd-mode)

(set-variable 'ycmd-server-command '("/usr/local/bin/python"))
(add-to-list  'ycmd-server-command (expand-file-name "~/repos/ycmd/ycmd") t)
;; (set-variable 'ycmd-global-config (expand-file-name "~/oss/emacs/ycmd/cpp/ycm/ycm_extra_conf.py"))

(require 'company)
(require 'company-ycmd)
(company-ycmd-setup)

(setq company-idle-delay 0.0)
(setq company-minimum-prefix-length 0)
(setq display-time-day-and-date t)
(setq display-time-default-load-average nil)

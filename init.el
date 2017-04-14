(require 'cl)
;; 環境
(set-language-environment "Japanese")
(prefer-coding-system 'utf-8)
(setq-default cursor-type 'bar)
(set-face-attribute 'default nil
                    :family "Menlo"
                    :height 120)

;; 自動生成ファイル
(setq backup-directory-alist '(("." . "~/.emacs.d/tmp")))
(setq auto-save-file-name-transforms '((".*" "~/.emacs.d/tmp/" t)))
(setq auto-save-list-file-prefix nil)
(setq create-lockfiles nil)

;; インデント
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)
(add-hook 'c-mode-common-hook
          '(lambda ()
             (setq indent-tabs-mode t
                   c-basic-offset 4
                   tab-width 4
                   c-auto-new-line t
                   c-hungry-delete-key t)))

;; 表示
(load-theme 'deeper-blue t)
(tool-bar-mode 0)
(line-number-mode t)
(column-number-mode t)
(global-linum-mode t)

;; スクロール
(setq scroll-conservatively 1
      scroll-margin 1
      next-screen-context-lines 1)

(electric-pair-mode 1)
(setq kill-whole-line t)
(setq require-final-newline t)
(setq inhibit-startup-screen t)
(setq ring-bell-function 'ignore)

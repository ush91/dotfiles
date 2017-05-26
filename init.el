(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)
(package-initialize)

(require 'cl)
;; 環境
(set-language-environment "Japanese")
(prefer-coding-system 'utf-8)
(setq-default cursor-type 'bar)
(set-face-attribute 'default nil
                    :family "Menlo"
                    :height 120)
(setq initial-frame-alist
      '((top . 0)
        (height . 50)
        (left . 845)))

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
             (setq ;indent-tabs-mode t
                   c-basic-offset 4
                   tab-width 4
                   c-auto-new-line t
                   c-hungry-delete-key t)))
(global-set-key "\C-a" '(lambda ()
                          (interactive)
                          (if (bolp)
                              (back-to-indentation) (beginning-of-line))))

;; テーマ
(load-theme 'deeper-blue t)
(set-face-attribute 'show-paren-match nil
                    :background "#400000")

;; 表示
(tool-bar-mode 0)
(line-number-mode t)
(column-number-mode t)
(global-linum-mode t)
(electric-pair-mode 1)
(show-paren-mode t)
(setq show-paren-style 'expression)

;; スクロール
(setq scroll-conservatively 1
      scroll-margin 1
      next-screen-context-lines 1)

;; その他
(setq kill-whole-line t)
(setq require-final-newline t)
(add-hook 'before-save-hook 'delete-trailing-whitespace)
(setq inhibit-startup-screen t)
(setq ring-bell-function 'ignore)

;; clipboard
;(setq x-select-enable-clipboard t)
;(setq mouse-drag-copy-region t)
;(delete-selection-mode t)

;; packages
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(irony-additional-clang-options (quote ("-std=c++11")))
 '(package-selected-packages
   (quote
    (dashboard popwin flycheck-irony company-irony-c-headers simpleclip company-irony company irony yasnippet))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;(require 'yasnippet)
(with-eval-after-load 'yasnippet
  (define-key yas-keymap (kbd "<tab>") nil)
;  (add-hook 'c++-mode-hook '(yas-global-mode 1))
  )
(yas-global-mode 1)

(require 'company)
(with-eval-after-load 'company
  (global-company-mode 1)
  (global-set-key (kbd "C-M-i") 'company-complete)
  (add-to-list 'company-backends 'company-irony)
  (add-to-list 'company-backends '(company-irony-c-headers company-irony))
  (define-key company-active-map (kbd "<tab>") 'company-complete-selection))

(with-eval-after-load 'irony
  (custom-set-variables '(irony-additional-clang-options '("-std=c++11")))
  (add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options)
  (add-hook 'irony-mode-hook
            '(lambda ()
               (interactive)
               (define-key irony-mode-map [remap completion-at-point]
                 'irony-completion-at-point-async)
               (define-key irony-mode-map [remap complete-symbol]
                 'irony-completion-at-point-async)))
  (add-hook 'c-mode-common-hook 'irony-mode)
  (setq company-idle-delay 0.0))

(simpleclip-mode 1)

;; (require 'dashboard)
;; (dashboard-setup-startup-hook)
;; (setq dashboard-items '((recents . 10)
;;                         (bookmarks . 5)))

;(require 'popwin)
;(setq display-buffer-function 'popwin:display-buffer)

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
      '((top . 23)
        (left . 775)
        (height . 50)
        (width . 90)))
; (frame-parameters (selected-frame))

;; 自動生成ファイル
(setq backup-directory-alist '(("." . "~/.emacs.d/tmp")))
(setq auto-save-file-name-transforms '((".*" "~/.emacs.d/tmp/" t)))
(setq auto-save-list-file-prefix nil)
(setq create-lockfiles nil)

;; インデント
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)
(add-hook 'python-mode-hook
          '(lambda ()
             (setq indent-tabs-mode nil
                   python-indent-offset 4)))

;; C-aでインデントの頭へ
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

;; recentf
(setq recentf-max-saved-items 1000)
(setq recentf-auto-cleanup 'never)
(setq recentf-auto-cleanup-timer
      (run-with-idle-timer 30 t 'recentf-save-list))
(recentf-mode 1)

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
    (shackle company-math exec-path-from-shell deferred company-jedi tabbar dashboard flycheck-irony company-irony-c-headers simpleclip company-irony company irony yasnippet))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(exec-path-from-shell-initialize)

;(require 'yasnippet)
(with-eval-after-load 'yasnippet
  (define-key yas-keymap (kbd "<tab>") nil)
;  (add-hook 'c++-mode-hook '(yas-global-mode 1))
  )
(yas-global-mode 1)

(require 'company)
(global-company-mode 1)
(global-set-key (kbd "C-M-i") 'company-complete)
(define-key company-active-map (kbd "<tab>") 'company-complete-selection)
(setq company-backends (delete 'company-clang company-backends))

(with-eval-after-load 'irony
  (custom-set-variables '(irony-additional-clang-options '("-std=c++11")))
  (add-to-list 'company-backends 'company-irony)
  (add-to-list 'company-backends '(company-irony-c-headers company-irony))
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

(with-eval-after-load 'jedi-core
  (add-hook 'python-mode-hook 'jedi:setup)
  (add-to-list 'company-backends 'company-jedi)
  (setq jedi:complete-on-dot t))

(simpleclip-mode 1)

(setq shackle-rules
      '(("*compilation*" :frame t)))
(shackle-mode 1)

;; (require 'dashboard)
;; (dashboard-setup-startup-hook)
;; (setq dashboard-items '((recents . 10)
;;                         (bookmarks . 5)))


;; C/C++
(add-hook 'c-mode-common-hook
          '(lambda ()
             (setq c-basic-offset 4
                   tab-width 4
                   c-auto-new-line t
                   c-hungry-delete-key t)
             (local-set-key "\C-cc" 'desperately-compile)))


;; scheme
(add-hook 'scheme-mode-hook
          '(lambda ()
             (setq scheme-program-name "gosh -i")))


;; TeX
(with-eval-after-load 'latex-mode
  (add-to-list 'company-backends 'company-math-symbols-latex)
  (add-to-list 'company-backends 'company-latex-commands))
(add-hook 'latex-mode-hook
          '(lambda ()
             (local-set-key "\C-cc" 'desperately-compile)))


;; make
;; https://emacs.stackexchange.com/questions/7475/
(defun desperately-compile ()
  "Traveling up the path, find Makefile and compile"
  (interactive)
  (let ((pos (locate-dominating-file default-directory "Makefile")))
    (when pos
      (with-temp-buffer
        (cd pos)
        (compile "make -k")))))

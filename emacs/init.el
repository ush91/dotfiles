(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)
(package-initialize)

(require 'cl-lib)

;; Environment
(set-language-environment "Japanese")
(prefer-coding-system 'utf-8-unix)
(setq-default cursor-type 'bar)
(cond ((equal system-type 'darwin)
       (set-face-attribute 'default nil
                           :family "Menlo"
                           :height 120)
       (setq initial-frame-alist
             '((top . 23)
               (left . 775)
               (height . 50)
               (width . 90))))
      ((equal system-type 'gnu/linux)
       (set-face-attribute 'default nil
                           :family "Myrica M"
                           :height 120)
       (setq initial-frame-alist
             '((top . 35)
               (left . 120)
               (height . 58)
               (width . 90)))
       (when (require 'mozc nil t)
         (setq default-input-method "japanese-mozc")
         (global-set-key
          [henkan] (lambda () (interactive)
                     (unless current-input-method
                       (toggle-input-method))))
         (defun advice:mozc-handle-event-muhenkan (event)
           (when (equal event 'muhenkan)
             (toggle-input-method)))
         (advice-add 'mozc-handle-event :before 'advice:mozc-handle-event-muhenkan)))
      ((equal system-type 'windows-nt)
       (set-face-attribute 'default nil
                           :family "Myrica M"
                           :height 120)
       (setq initial-frame-alist
             '((top . 0)
               (left . 120)
               (height . 62)
               (width . 90)))
       (setq w32-pipe-read-delay 0)))
; (frame-parameters (selected-frame))

;; Backups
(setq backup-directory-alist '(("." . "~/.emacs.d/tmp")))
(setq auto-save-file-name-transforms '((".*" "~/.emacs.d/tmp/" t)))
(setq auto-save-list-file-prefix nil)
(setq create-lockfiles nil)

;; Indent
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)

;; Goto top of indentaion with C-a
(global-set-key "\C-a" '(lambda ()
                          (interactive)
                          (if (bolp)
                              (back-to-indentation) (beginning-of-line))))

;; Theme
(load-theme 'deeper-blue t)
(set-face-attribute 'show-paren-match nil
                    :background "#400000")

;; View
(tool-bar-mode 0)
(line-number-mode t)
(column-number-mode t)
(global-linum-mode t)
(electric-pair-mode 1)
(show-paren-mode t)
(setq show-paren-style 'expression)

;; Scroll
(setq scroll-conservatively 1000
      scroll-margin 1
      next-screen-context-lines 1)
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1) ((control))))


;; recentf
(setq recentf-max-saved-items 1000)
(setq recentf-auto-cleanup 'never)
(setq recentf-auto-cleanup-timer
      (run-with-idle-timer 300 t 'recentf-save-list))
(setq recentf-exclude '("/.emacs.d/bookmarks" "/.emacs.d/elpa/"))
(recentf-mode 1)

;; Others
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
    (company-statistics electric-operator shackle company-math exec-path-from-shell deferred company-jedi tabbar dashboard flycheck-irony company-irony-c-headers simpleclip company-irony company irony yasnippet))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(when (package-installed-p 'exec-path-from-shell)
  (exec-path-from-shell-initialize))

(when (package-installed-p 'yasnippet)
  (yas-global-mode 1)
  (define-key yas-keymap (kbd "<tab>") nil))

(when (package-installed-p 'company)
  (global-company-mode 1)
  (global-set-key (kbd "C-M-i") 'company-complete)
  (define-key company-active-map (kbd "<tab>") 'company-complete-selection)
  (setq company-idle-delay 0)
  (setq company-minimum-prefix-length 2)
  (setq company-dabbrev-downcase nil)

  (setq company-backends
        '((company-files company-keywords company-capf company-yasnippet)
          (company-dabbrev company-abbrev)))
  (make-variable-buffer-local 'company-backends)

  (when (package-installed-p 'company-statistics)
    (company-statistics-mode))

  (when (and (package-installed-p 'company-irony)
             (package-installed-p 'company-irony-c-headers))
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
      (add-hook 'c-mode-common-hook 'irony-mode))
    (add-hook 'c-mode-common-hook
              '(lambda ()
                 (add-to-list 'company-backends '(company-irony
                                                  company-irony-c-headers)))))

  (when (package-installed-p 'company-jedi)
    (with-eval-after-load 'jedi-core
      (add-hook 'python-mode-hook 'jedi:setup)
      (setq jedi:complete-on-dot t))
    (add-hook 'python-mode-hook
              '(lambda ()
                 (add-to-list 'company-backends 'company-jedi))))

  (when (package-installed-p 'company-math)
    (add-hook 'latex-mode-hook
              '(lambda ()
                 (add-to-list 'company-backends '(company-math-symbols-latex
                                                  company-latex-commands))))))


(when (package-installed-p 'simpleclip)
  (simpleclip-mode 1))

(when (package-installed-p 'shackle)
  (shackle-mode 1)
  (setq shackle-rules
        '(("*compilation*" :frame t :other t))))

(when (package-installed-p 'dashboard)
  (dashboard-setup-startup-hook)
  (setq dashboard-items '((recents . 20)
                          (bookmarks . 10))))

(when (package-installed-p 'electric-operator)
  (add-hook 'c-mode-common-hook 'electric-operator-mode)
  (add-hook 'python-mode-hook 'electric-operator-mode))


;; C/C++
(add-hook 'c-mode-common-hook 'irony-mode)
(add-hook 'c-mode-common-hook
          '(lambda ()
             (setq c-basic-offset 4
                   tab-width 4
                   c-auto-new-line t
                   c-hungry-delete-key t)
             (local-set-key "\C-cc" 'desperately-compile)
             (c-set-offset 'innamespace 0)))

;; Python
(add-hook 'python-mode-hook
          '(lambda ()
             (setq indent-tabs-mode nil
                   python-indent-offset 4)))

;; scheme
(add-hook 'scheme-mode-hook
          '(lambda ()
             (setq scheme-program-name "gosh -i")))

;; TeX
(with-eval-after-load 'tex-mode
  (require 'server)
  (unless (server-running-p)
    (server-start)))
(add-hook 'latex-mode-hook
          '(lambda ()
             (local-set-key "\C-cc" 'desperately-compile)))


;; make
;; https://emacs.stackexchange.com/questions/7475/
(defun desperately-compile ()
  "Traveling up the path, find Makefile and compile"
  (interactive)
  (with-temp-buffer
    (while (and (not (file-exists-p "Makefile"))
                (not (file-exists-p "CMakeLists.txt"))
                (not (equal default-directory "/")))
      (cd ".."))
    (cond ((file-exists-p "Makefile")
           (compile "make -k"))
          ((file-exists-p "CMakeLists.txt")
           (when (not (file-exists-p "build"))
             (make-directory "build"))
           (cd "build")
           (if (file-newer-than-file-p "Makefile" "../CMakeLists.txt")
               (compile "make -k")
             (compile "cmake .. && make -k")))
          ((equal default-directory "/")
           (message "No Makefile or CMakeLists.txt"))
          (t
           (cd "..")))))

;; cmake
(autoload 'cmake-mode "cmake-mode" nil t)
(add-to-list 'auto-mode-alist '("CMakeLists\\.txt\\'" . cmake-mode))
(add-to-list 'auto-mode-alist '("\\.cmake\\'" . cmake-mode))
(when (package-installed-p 'company)
  (add-hook 'cmake-mode-hook
            '(lambda ()
               (add-to-list 'company-backends 'company-cmake))))


(setq compilation-scroll-output t
      compilation-always-kill t)

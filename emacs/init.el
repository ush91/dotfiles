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
(let ((tmp (file-name-as-directory (expand-file-name "tmp" user-emacs-directory))))
  (setq backup-directory-alist (list (cons "." tmp)))
  (setq auto-save-file-name-transforms (list (list ".*" tmp t))))
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

(setq var:active-mode-line-color "deep sky blue")
(set-face-background 'mode-line var:active-mode-line-color)
(defun hook:set-active-mode-line-focus-in ()
  "Set mode line color to active color (deep sky blue) when focus in"
  (setq var:active-mode-line-color "deep sky blue")
  (set-face-background 'mode-line var:active-mode-line-color))
(defun hook:set-active-mode-line-focus-out ()
  "Set mode line color to inactive color (dark slate gray) when focus out"
  (setq var:active-mode-line-color "dark slate gray")
  (set-face-background 'mode-line var:active-mode-line-color))
(add-hook 'focus-in-hook 'hook:set-active-mode-line-focus-in)
(add-hook 'focus-out-hook 'hook:set-active-mode-line-focus-out)

(defun hook:change-mode-line-color-save-complete ()
  "Set mode line color to lawn green for saving and reserve to restore the color"
  (set-face-background 'mode-line "lawn green")
  (run-with-idle-timer
   0.15 nil (lambda () (set-face-background 'mode-line var:active-mode-line-color))))
(add-hook 'after-save-hook 'hook:change-mode-line-color-save-complete)

(defun hook:change-mode-line-color-ring-bell ()
  "Set mode line color to orange red for warning and reserve to restore the color"
  (set-face-background 'mode-line "orange red")
  (run-with-idle-timer
   0.04 nil (lambda () (set-face-background 'mode-line var:active-mode-line-color))))
(setq ring-bell-function 'hook:change-mode-line-color-ring-bell)

;; View
(tool-bar-mode 0)
(unless (eq window-system 'ns) (menu-bar-mode 0))
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
(setq recentf-auto-save-timer
      (run-with-idle-timer 300 t 'recentf-save-list))
(let ((emacs-root (regexp-quote (expand-file-name user-emacs-directory))))
  (setq recentf-exclude
        (list (concat emacs-root "\\(?:bookmarks\\|recentf\\)\\'")
              (concat emacs-root (file-name-as-directory "elpa")))))
(recentf-mode 1)

;; Others
(setq kill-whole-line t)
(setq require-final-newline t)
(add-hook 'before-save-hook 'delete-trailing-whitespace)
(setq inhibit-startup-screen t)

;; clipboard
;(setq x-select-enable-clipboard t)
;(setq mouse-drag-copy-region t)
;(delete-selection-mode t)

;; packages
(setq custom-file (locate-user-emacs-file "customfile.el"))

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
             (package-installed-p 'company-c-headers))
    (with-eval-after-load 'irony
      (custom-set-variables '(irony-additional-clang-options '("-std=c++14")))
      (add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options)
      (add-hook 'irony-mode-hook
                '(lambda ()
                   (interactive)
                   (define-key irony-mode-map [remap completion-at-point]
                     'irony-completion-at-point-async)
                   (define-key irony-mode-map [remap complete-symbol]
                     'irony-completion-at-point-async)))
      (add-hook 'c-mode-common-hook 'irony-mode)

      ;; https://github.com/randomphrase/company-c-headers/issues/14#issuecomment-300564039
      (custom-set-variables
       '(company-c-headers-path-user
         (lambda () (when irony-mode
                      (irony--extract-user-search-paths irony--compile-options
                                                        irony--working-directory)))))

      ;; macOS workaround
      ;; https://github.com/Sarcasm/irony-mode/wiki/Mac-OS-X-issues-and-workaround
      (when (equal system-type 'darwin)
        (require 'company-c-headers)
        (let ((paths (split-string (substring (shell-command-to-string "echo | clang -xc++ -v -E - 2>&1 | sed -n '/^#include </,/^End/s|^ \\([^ ]*\\)$|\\1|p'") 0 -1) "\n")))
          (custom-set-variables
           '(irony-additional-clang-options
             (append irony-additional-clang-options
                     (mapcar (lambda (p) (concat "-I" p)) paths)))
           '(company-c-headers-path-system paths)))))

    (add-hook 'c-mode-common-hook
              '(lambda ()
                 (add-to-list 'company-backends '(company-c-headers
                                                  company-irony
                                                  company-yasnippet)))))

  (when (package-installed-p 'company-jedi)
    (with-eval-after-load 'jedi-core
      (add-hook 'python-mode-hook 'jedi:setup)
      (setq jedi:complete-on-dot t))
    (add-hook 'python-mode-hook
              '(lambda ()
                 (add-to-list 'company-backends 'company-jedi)))

    (defun setup-jedi-for-pipenv-venv ()
      (when (and (executable-find "pipenv")
                 (locate-dominating-file (buffer-file-name) "Pipfile"))
        (let ((server-args (list "--virtual-env" (substring (shell-command-to-string "pipenv --venv") 0 -1))))
          (setq-local jedi:server-args
                      (if (boundp 'jedi:server-args)
                        (append jedi:server-args server-args)
                        server-args)))))

    (add-hook 'python-mode-hook 'setup-jedi-for-pipenv-venv)

    ;; workaround for jedi replacing int literal with keywords (and, if, etc.)
    ;; https://www.reddit.com/r/emacs/comments/7dnbxl/
    (defun advice:prevent-jedi-replace-int (fn command &optional arg &rest _)
      (unless (when (and (equal command 'prefix)
                         (> (point) 0))
                (let ((prefix (company-grab-symbol)))
                  (when prefix
                    (string-match "^[0-9]+$" prefix))))
        (funcall fn command arg)))
    (advice-add 'company-jedi :around 'advice:prevent-jedi-replace-int))

  (when (package-installed-p 'company-math)
    (add-hook 'tex-mode-hook
              '(lambda ()
                 (add-to-list 'company-backends '(company-math-symbols-latex
                                                  company-latex-commands))))))

(defun hook:set-flycheck-eslint-executable ()
  "Find and set local installed eslint."
  (when (buffer-file-name)
    (let ((rootdir (locate-dominating-file (buffer-file-name) "package.json")))
      (when rootdir
        (let ((eslint-path (expand-file-name "node_modules/.bin/eslint" rootdir)))
          (when (file-exists-p eslint-path)
            (setq-local flycheck-javascript-eslint-executable eslint-path)))))))

(when (and (package-installed-p 'vue-mode)
           (package-installed-p 'flycheck))
  (with-eval-after-load 'vue-mode
    (require 'flycheck)
    (flycheck-add-mode 'javascript-eslint 'vue-mode)
    (flycheck-add-mode 'javascript-eslint 'vue-html-mode)
    (flycheck-add-mode 'javascript-eslint 'css-mode)
    (add-hook 'vue-mode-hook 'flycheck-mode)
    (add-hook 'vue-mode-hook 'hook:set-flycheck-eslint-executable)))

(when (package-installed-p 'typescript-mode)
  (with-eval-after-load 'typescript-mode
    (setq typescript-indent-level 2)
    (add-hook 'typescript-mode-hook 'eglot-ensure)))

(when (package-installed-p 'vue-html-mode)
  (setq-default vue-html-extra-indent 2))

(when (and (package-installed-p 'rustic)
           (package-installed-p 'exec-path-from-shell))
  (with-eval-after-load 'rustic
    (exec-path-from-shell-copy-envs '("RUSTUP_HOME" "CARGO_HOME"))
    (setq-default rustic-format-on-save t)))

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
  (add-hook 'python-mode-hook 'electric-operator-mode)
  (add-hook 'js-mode-hook 'electric-operator-mode))

(when (and (package-installed-p 'rustic)
           (package-installed-p 'electric-operator))
  (with-eval-after-load 'rustic
    (require 'electric-operator)
    (apply #'electric-operator-add-rules-for-mode 'rustic-mode
           (electric-operator-get-rules-for-mode 'rust-mode))
    (electric-operator-add-rules-for-mode 'rustic-mode
                                          (cons "->" " ->")
                                          (cons "=>" " =>")
                                          (cons "<<" " <<")
                                          (cons ">>" " >>"))
    (add-hook 'rustic-mode-hook 'electric-operator-mode)))

(when (and (package-installed-p 'rustic)
           (package-installed-p 'eglot))
  (with-eval-after-load 'rustic
    (setq-default rustic-rls-pkg 'eglot)))

;; Comment key bind
(defun comment-or-uncomment-region-or-line-or-insert ()
  "Comment out or uncomment or create empty comment line"
  (interactive)
  (when comment-start
    (if (use-region-p)
        (comment-or-uncomment-region (region-beginning) (region-end))
      (if (and (not (equal last-command
                           'comment-or-uncomment-region-or-line-or-insert))
               (save-excursion (beginning-of-line) (looking-at-p "\\s-*$")))
          (progn (indent-according-to-mode)
                 (insert (comment-padright comment-start))
                 (save-excursion (unless (string= "" comment-end)
                                   (insert (comment-padleft comment-end)))))
        (comment-line 1)))))
(global-set-key "\M-;" 'comment-or-uncomment-region-or-line-or-insert)

(global-set-key (kbd "<M-right>") 'bs-cycle-next)
(global-set-key (kbd "<M-left>") 'bs-cycle-previous)


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
(add-to-list 'auto-mode-alist '("\\.tex\\'" . latex-mode))
(with-eval-after-load 'tex-mode
  (require 'server)
  (unless (server-running-p)
    (server-start)))
(add-hook 'tex-mode-hook
          '(lambda ()
             (local-set-key "\C-cc" 'desperately-compile)))

;; JavaScript
(with-eval-after-load 'js
  (setq-default js-indent-level 2))

(with-eval-after-load 'css-mode
  (setq-default css-indent-offset 2))

;; TOML
(with-eval-after-load 'conf-mode
  (when (package-installed-p 'electric-operator)
    (require 'electric-operator)
    (electric-operator-add-rules-for-mode 'conf-toml-mode
                                          (cons "=" " = "))
    (add-hook 'conf-toml-mode-hook 'electric-operator-mode)))


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

(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
(add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/"))
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))
(require 'diminish)
(require 'bind-key)

(add-to-list 'load-path "~/.emacs.d/lisp")

(require 'editorconfig)
(editorconfig-mode 1)

(require 'nodejs-repl)

(use-package scss-mode
  :mode (("\\.scss\\'" . scss-mode)))

(setq dired-use-ls-dired nil)

(setq visible-bell nil) ;; The default
(setq ring-bell-function 'ignore)

(setq css-indent-offset 2)

;; Always ALWAYS use UTF-8
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(prefer-coding-system 'utf-8)

;; copy and paste
(setq interprogram-cut-function 'paste-to-osx
      interprogram-paste-function 'copy-from-osx)

(defun copy-from-osx ()
  (let ((coding-system-for-read 'utf-8))
    (shell-command-to-string "pbpaste")))

(defun paste-to-osx (text &optional push)
  (let ((process-connection-type nil))
    (let ((proc (start-process "pbcopy" "*Messages*" "pbcopy")))
      (set-process-sentinel proc 'ignore) ;; stifle noise in *Messages*
      (process-send-string proc text)
      (process-send-eof proc)))
  text)

;;; kill other buffers
(defun kill-other-buffers ()
    "Kill all other buffers."
    (interactive)
    (mapc 'kill-buffer
          (delq (current-buffer)
                (remove-if-not 'buffer-file-name (buffer-list)))))

;; Always ask for y/n keypress instead of typing out 'yes' or 'no'
(defalias 'yes-or-no-p 'y-or-n-p)

(setq dotfiles-dir (file-name-directory
                    (or (buffer-file-name) load-file-name)))

;; auto saving
(setq auto-save-default t)
(setq auto-save-visited-file-name t)
(setq auto-save-interval 20) ; twenty keystrokes
(setq auto-save-timeout 1) ; 1 second of idle time

(defvar user-temporary-file-directory
  (concat temporary-file-directory user-login-name "/"))

(make-directory user-temporary-file-directory t)

(setq backup-by-copying t)

(setq backup-directory-alist
      `(("." . ,user-temporary-file-directory)
        (,tramp-file-name-regexp nil)))

(setq auto-save-list-file-prefix
      (concat user-temporary-file-directory ".auto-saves-"))

(setq auto-save-file-name-transforms
      `((".*" ,user-temporary-file-directory t)))

(setq create-lockfiles nil)

(global-linum-mode t)
(global-hl-line-mode t)
(setq inhibit-startup-message t)
(setq x-underline-at-descent-line t)
(when (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(when (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(unless (display-graphic-p) (menu-bar-mode -1))
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1)))
(setq mouse-wheel-progressive-speed nil)
(setq mouse-wheel-follow-mouse 't)
(setq scroll-step 1)
(setq column-number-mode t)
(setq ns-pop-up-frames nil)
(setq-default show-trailing-whitespace t)
(add-hook 'term-mode-hook (lambda () (setq show-trailing-whitespace nil)))
(setq-default visible-bell 'top-bottom)
(setq tab-width 2)

;; Allow hash to be entered
(global-set-key (kbd "M-3") '(lambda () (interactive) (insert "#")))

(autoload
  'ace-jump-mode-pop-mark
  "ace-jump-mode"
  "Ace jump back:-)"
  t)

(show-paren-mode 1)

(define-key global-map (kbd "C-x SPC") 'ace-jump-mode-pop-mark)
(autoload
  'ace-jump-mode
  "ace-jump-mode"
  "Emacs quick move minor mode"
  t)

(define-key global-map (kbd "C-c SPC") 'ace-jump-mode)

(add-to-list 'auto-mode-alist '("\\.yml\\'" . yaml-mode))
(add-hook 'yaml-mode-hook
          (lambda ()
            (define-key yaml-mode-map "\C-m" 'newline-and-indent)))

(use-package ag
  :commands (ag ag-files ag-regexp ag-project ag-dired helm-ag)
  :config (setq ag-highlight-search t
                ag-reuse-buffers t))

;; js2-mode
(use-package js2-mode
  :defer 1
  :mode "\\.js$"
  :init
  (defadvice js-jsx-indent-line (after js-jsx-indent-line-after-hack activate)
    "Workaround sgml-mode and follow airbnb component style."
    (let* ((cur-line (buffer-substring-no-properties
		      (line-beginning-position)
		      (line-end-position))))
      (if (string-match "^\\( +\\)\/?> *$" cur-line)
	  (let* ((empty-spaces (match-string 1 cur-line)))
	    (replace-regexp empty-spaces
			    (make-string (- (length empty-spaces) sgml-basic-offset) 32)
			    nil
			    (line-beginning-position) (line-end-position))))))
  :config
  (progn
    (add-to-list 'interpreter-mode-alist '("node" . js2-mode))

    (add-hook 'js2-mode-hook #'tern-mode)

    (setq js2-basic-offset 2
          js2-bounce-indent-p t
          js2-strict-missing-semi-warning nil
          js2-concat-multiline-strings nil
          js2-include-node-externs t
          js2-skip-preprocessor-directives t
          js2-strict-inconsistent-return-warning nil)))

(use-package js2-refactor               ; Refactor JavaScript
  :ensure t
  :after js2-mode
  :init (add-hook 'js2-mode-hook #'js2-refactor-mode)
  :config (js2r-add-keybindings-with-prefix "C-c m r"))


(use-package rjsx-mode                  ; JSX mode
  :ensure t
  :mode ("\\.js\\'" . rjsx-mode))

(use-package prettier-js
  :init
  (add-hook 'js2-mode-hook 'prettier-js-mode)
  :config
  (setq prettier-js-args '(
			   "--bracket-spacing" "true"
			   "--single-quote" "true"
			   "--trailing-comman" "none"
			   "--jsx-parser-same-line" "false"
			   "--parser" "flow"
			   ))
  )

;; Tern
(use-package tern
  :defer 1
  :init (autoload 'tern-mode "tern" nil t))

(use-package company-tern
  :defer 1
  :config
  (progn
    (setq tern-command (append tern-command '("--no-port-file")))
    (add-to-list 'company-backends 'company-tern)))

(use-package smartparens
  :ensure t
  :init
  (smartparens-global-mode 1)
  (show-smartparens-global-mode)
  (dolist (hook '(inferior-emacs-lisp-mode-hook
                  emacs-lisp-mode-hook))
    (add-hook hook #'smartparens-strict-mode))
  :config
  (require 'smartparens-config)
  (setq sp-autoskip-closing-pair 'always)
  :diminish (smartparens-mode))

(setq js-indent-level 2)
(setq jsx-indent-level 2)

(use-package magit
  :init (progn
          (setq magit-push-always-verify nil)))

(use-package server)
(unless (server-running-p)
  (server-start))

(use-package color-theme
  :init
  (load-theme 'wombat))

(set-frame-font "Monaco-14")

(use-package projectile
  :ensure    projectile
  :config    (projectile-global-mode t)
  :init      (progn
               ;; set projectile custom variables
               (setq projectile-enable-caching t))
  :diminish   projectile-mode)
(projectile-global-mode)

(use-package ido
  :ensure t
  :config
  (setq ido-enable-prefix nil
        ido-enable-flex-matching t
        ido-create-new-buffer 'always
        ido-use-filename-at-point 'guess
        ido-max-prospects 10
        ido-default-file-method 'selected-window
        ido-auto-merge-work-directories-length -1)
  (ido-mode +1))

(use-package flx-ido
  :ensure t
  :config
  (flx-ido-mode +1)
  ;; disable ido faces to see flx highlights
  (setq ido-use-faces nil))

(use-package smex
  :ensure t
  :bind ("M-x" . smex))

(use-package markdown-mode
  :ensure t)

(use-package yaml-mode
  :ensure t)


(use-package company
  :ensure t
  :config
  (global-company-mode))

(use-package flycheck
  :ensure t
  :config
  (add-hook 'after-init-hook #'global-flycheck-mode))

(setq-default flycheck-disabled-checkers '(emacs-lisp-checkdoc))

;; disable jshint since we prefer eslintchecking
(setq-default flycheck-disabled-checkers
              (append flycheck-disabled-checkers
                      '(javascript-jshint)))

;; customize flycheck temp file prefix
(setq-default flycheck-temp-prefix ".flycheck")

(setq-default flycheck-disabled-checkers
  (append flycheck-disabled-checkers
          '(json-jsonlist)))

(when (memq window-system '(mac ns))
  (exec-path-from-shell-initialize))

(defun my/use-eslint-from-node-modules ()
  (let* ((root (locate-dominating-file
                (or (buffer-file-name) default-directory)
                "node_modules"))
         (eslint (and root
                      (expand-file-name "node_modules/eslint/bin/eslint.js"
                                        root))))
    (when (and eslint (file-executable-p eslint))
      (setq-local flycheck-javascript-eslint-executable eslint))))

(add-hook 'flycheck-mode-hook #'my/use-eslint-from-node-modules)

(use-package ag)

(use-package restclient
  :mode ("\\.http\\'" . restclient-mode))

(setq exec-path-from-shell-check-startup-files nil)

(exec-path-from-shell-initialize)

(setq ag-reuse-buffers 't)

(global-set-key "\M-/" 'hippie-expand)

(add-hook 'prog-mode-hook  'rainbow-delimiters-mode)

(use-package rainbow-mode
  :ensure t
  :config
  (add-hook 'prog-mode-hook #'rainbow-mode))

;; set indent to 2
(setq css-indent-offset 2)

(add-to-list 'auto-mode-alist '("\\.css\\'" . css-mode))
(add-to-list 'auto-mode-alist '("\\.scss\\'" . css-mode))
(add-to-list 'auto-mode-alist '("\\.sass\\'" . css-mode))


(delete-selection-mode t)

(defun beautify-json ()
  (interactive)
  (let ((b (if mark-active (min (point) (mark)) (point-min)))
        (e (if mark-active (max (point) (mark)) (point-max))))
    (shell-command-on-region b e
     "python -mjson.tool" (current-buffer) t)))

(setq company-dabbrev-downcase nil)

(provide 'init)

;; when you have a selection, typing text replaces it all.
(delete-selection-mode t)

;; nice scrolling
(setq scroll-margin 0
      scroll-conservatively 100000
      scroll-preserve-screen-position 1)

;; show matching paren
(require 'paren)
(setq show-paren-style 'parenthesis)
(show-paren-mode +1)

(use-package drag-stuff
  :demand t
  :diminish drag-stuff-mode
  :config
  (progn
    (drag-stuff-global-mode t)
    (drag-stuff-define-keys)
    (add-to-list 'drag-stuff-except-modes 'org-mode)
    (add-to-list 'drag-stuff-except-modes 'rebase-mode)))

(put 'downcase-region 'disabled nil)
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (rjsx-mode js2-refactor web-beautify js2-mode ag zenburn-theme yaml-mode use-package smex scss-mode sass-mode rainbow-mode rainbow-delimiters projectile markdown-mode magit key-chord json-mode git-gutter gist flycheck-hdevtools flx-ido exec-path-from-shell elein company))))

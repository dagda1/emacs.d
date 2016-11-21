(require 'package)

(setq package-list '(company
                     auto-complete
                     cider
                     clojure-mode
                     clojurescript-mode
                     elein
                     paredit
                     paredit-everywhere
                     popup
                     rainbow-delimiters
                     rainbow-mode
                     color-theme
                     zenburn-theme
                     flycheck
                     flycheck-hdevtools
                     ruby-mode
                     ruby-end
                     ruby-tools
                     coffee-mode
                     ido
                     markdown-mode
                     scss-mode
                     projectile
                     magit
                     git-gutter
                     gist
                     minitest
                     rbenv
                     smex
                     flx-ido
                     key-chord
                     web-mode
                     json-mode
                     ace-jump-mode
                     ag
                     org
                     exec-path-from-shell
                     haskell-mode
                     sass-mode
                     ))

(add-to-list 'load-path "~/.emacs.d/vendor/")

(setq dired-use-ls-dired nil)

(setq visible-bell nil) ;; The default
(setq ring-bell-function 'ignore)

(eval-after-load 'haskell-mode
  '(define-key haskell-mode-map (kbd "C-c C-o") 'haskell-compile))
(eval-after-load 'haskell-cabal
  '(define-key haskell-cabal-mode-map (kbd "C-c C-o") 'haskell-compile))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(coffee-tab-width 2 t)
 '(flycheck-coffeelintrc "~/.emacs.d/coffeelint.json")
 '(haskell-process-type (quote stack-ghci))
 '(package-selected-packages
   (quote
    (haskell-mode exec-path-from-shell ag ace-jump-mode json-mode web-mode key-chord flx-ido smex rbenv minitest gist git-gutter magit projectile scss-mode markdown-mode coffee-mode ruby-tools ruby-end flycheck-hdevtools flycheck zenburn-theme color-theme rainbow-mode rainbow-delimiters paredit-everywhere paredit elein clojurescript-mode cider auto-complete company))))

;;set tab width globally
(setq-default indent-tabs-mode nil)
(setq-default tab-width 2)
(setq css-indent-offset 2)

;; Allow hash to be entered
(global-set-key (kbd "M-3") '(lambda () (interactive) (insert "#")))
(tool-bar-mode -1)

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

(require 'web-mode)

(defun my-web-mode-hook ()
  "web-mode settings"
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-css-indent-offset 2)
  (setq web-mode-code-indent-offset 2)
  (setq web-mode-enable-auto-quoting nil))

(add-hook 'web-mode-hook  'my-web-mode-hook)

(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.js?$" . web-mode))

(setq-default web-mode-comment-formats (remove '("javascript" . "/*") web-mode-comment-formats))
(add-to-list 'web-mode-comment-formats '("javascript" . "//"))

(setq web-mode-content-types-alist
  '(("jsx" . "\\.js[x]?\\'")))

(define-minor-mode pair-helpers-minor-mode
  "This mode contains little helpers for js developement"
  nil
  ""
  '(((kbd "{") . insert-js-block-parentheses)
    ((kbd "'") . insert-js-closing-quote)))

(defun insert-js-block-parentheses ()
  (interactive)
  (insert "{")
  (insert "}")
  (forward-char -1))

(defun insert-js-closing-quote ()
  (interactive)
  (insert "'")
  (insert "'")
  (forward-char -1))

(add-hook 'coffee-mode-hook 'pair-helpers-minor-mode)
(add-hook 'ruby-mode-hook 'pair-helpers-minor-mode)
(add-hook 'json-mode-hook 'pair-helpers-minor-mode)
(add-hook 'jsx-mode-hook 'pair-helpers-minor-mode)
(add-hook 'haskell-mode-hook 'pair-helpers-minor-mode)
(add-hook 'web-mode-hook 'pair-helpers-minor-mode)

(setq js-indent-level 2)
(setq jsx-indent-level 2)

; list the repositories containing them
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/") t)

; activate all the packages (in particular autoloads)
(package-initialize)

; fetch the list of packages available
(unless package-archive-contents
  (package-refresh-contents))

; install the missing packages
(dolist (package package-list)
  (unless (package-installed-p package)
    (package-install package)))

(require 'magit)
; magit configuration
(setq magit-push-always-verify nil)

;; Always ALWAYS use UTF-8
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(prefer-coding-system 'utf-8)
(load-library "iso-transl")

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

(require 'server)
(unless (server-running-p)
  (server-start))

(setq-default show-trailing-whitespace t)
(add-hook 'term-mode-hook (lambda () (setq show-trailing-whitespace nil)))
(setq-default visible-bell 'top-bottom)
(setq-default default-tab-width 2)
(setq-default indent-tabs-mode nil)

(setq coffee-tab-width 2)

(add-hook 'coffee-mode-hook 'flymake-mode)

(require 'color-theme)
(load-theme 'wombat t)

(set-frame-font "Monaco-14")

(require 'projectile)
(projectile-global-mode)
(setq projectile-enable-caching nil)

(require 'ido)

(ido-mode +1)

;;; smarter fuzzy matching for ido
(flx-ido-mode +1)
;; disable ido faces to see flx highlights
(setq ido-use-faces nil)

;;; smex, remember recently and most frequently used commands
(require 'smex)
(smex-initialize)
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)

(provide 'prelude-ido)

(require 'rainbow-delimiters)
(add-hook 'prog-mode-hook #'rainbow-delimiters-mode)

(require 'company)
(global-company-mode 1)
(add-hook 'after-init-hook #'global-flycheck-mode)
(add-hook 'after-init-hook 'global-company-mode)

(require 'flycheck)

(setq-default flycheck-disabled-checkers '(emacs-lisp-checkdoc))

;; turn on flychecking globally
(add-hook 'after-init-hook #'global-flycheck-mode)

;; disable jshint since we prefer eslint checking
(setq-default flycheck-disabled-checkers
              (append flycheck-disabled-checkers
                      '(javascript-jshint)))

;; customize flycheck temp file prefix
(setq-default flycheck-temp-prefix ".flycheck")

(setq-default flycheck-disabled-checkers
  (append flycheck-disabled-checkers
          '(json-jsonlist)))

(flycheck-add-mode 'javascript-eslint 'web-mode)

(dolist (mode '(ruby web))
  (add-hook (intern (format "%s-mode-hook" mode))
            '(lambda ()
               (add-to-list (make-local-variable 'paredit-space-for-delimiter-predicates)
                            (lambda (_ _) nil))
               (enable-paredit-mode))))

;; ruby config
(require 'ag)
(require 'ruby-tools)
(require 'ruby-end)

(add-to-list 'auto-mode-alist '("\\.rb\\'" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.ru\\'" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.rake\\'" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.gemspec\\'" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.rabl\\'" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.jbuilder\\'" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.prawn\\'" . ruby-mode))
(add-to-list 'auto-mode-alist '("Capfile" . ruby-mode))
(add-to-list 'auto-mode-alist '("Gemfile" . ruby-mode))
(add-to-list 'auto-mode-alist '("Rakefile" . ruby-mode))
(add-to-list 'auto-mode-alist '("Vagrantfile" . ruby-mode))
(add-to-list 'auto-mode-alist '("Cheffile" . ruby-mode))
(add-to-list 'auto-mode-alist '("Berksfile" . ruby-mode))

(setq ruby-indent-tabs-mode nil)
(setq ruby-indent-level 2)

(require 'minitest)
(add-hook 'ruby-mode-hook 'minitest-mode)

(defun cowboyd-minitest-setup-env (do-run &rest arguments)
  "Use Bash for minitest to avoid Zsh issues. Also, activate rbenv ruby if
necessary"
  (let ((shell-file-name "/bin/bash"))
    (rbenv-use-corresponding)
    (apply do-run arguments)))

(advice-add 'minitest--run-command :around #'cowboyd-minitest-setup-env)

(require 'rbenv)
(global-rbenv-mode)
(rbenv-use-global)

(autoload 'enh-ruby-mode "enh-ruby-mode" "" t)
(add-to-list 'interpreter-mode-alist '("ruby" . enh-ruby-mode))

(setq enh-ruby-program rbenv-ruby-shim)

(add-hook 'enh-ruby-mode-hook 'minitest-mode)

(require 'handlebars-mode)

(require 'paredit)

(setq exec-path-from-shell-check-startup-files nil)

(exec-path-from-shell-initialize)

(setq ag-reuse-buffers 't)

(global-set-key "\M-/" 'hippie-expand)

; cider config
(require 'cider)
(setq nrepl-hide-special-buffers t)
(setq cider-show-error-buffer nil)

(define-key clojure-mode-map (kbd "C-o j") 'cider-jack-in)
(define-key clojure-mode-map (kbd "C-o J") 'cider-restart)
(define-key clojure-mode-map (kbd "C-o y") 'cider-eval-last-sexp-and-append)

(add-hook 'prog-mode-hook  'rainbow-delimiters-mode)

(define-clojure-indent
  (defroutes 'defun)
  (GET 2)
  (POST 2)
  (PUT 2)
  (DELETE 2)
  (HEAD 2)
  (ANY 2)
  (context 2))

(add-hook 'clojure-mode-hook
          (lambda ()
            (local-set-key (kbd "M-e") 'forward-sexp)
            (local-set-key (kbd "M-a") 'backward-sexp)))

(defun turn-on-paredit () (paredit-mode t))

(define-key paredit-mode-map (kbd "C-^") 'paredit-remove-newlines)
(define-key paredit-mode-map (kbd "M-^") 'paredit-delete-indentation)

(add-hook 'emacs-lisp-mode-hook       'turn-on-paredit)
(add-hook 'lisp-mode-hook             'turn-on-paredit)
(add-hook 'lisp-interaction-mode-hook 'turn-on-paredit)
(add-hook 'scheme-mode-hook           'turn-on-paredit)
(add-hook 'clojure-mode-hook          'turn-on-paredit)
(add-hook 'cider-repl-mode-hook       'turn-on-paredit)
(add-hook 'sibiliant-mode-hook        'turn-on-paredit)

(defun beautify-json ()
  (interactive)
  (let ((b (if mark-active (min (point) (mark)) (point-min)))
        (e (if mark-active (max (point) (mark)) (point-max))))
    (shell-command-on-region b e
     "python -mjson.tool" (current-buffer) t)))

(setq company-dabbrev-downcase nil)

(provide 'init)

(put 'downcase-region 'disabled nil)
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

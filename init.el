(require 'package)
;; ; list the packages you want
(setq package-list '(company
                     auto-complete
                     autopair
                     ac-cider
                     cider
                     4clojure
                     clojure-mode
                     clojure-cheatsheet
                     clojure-snippets
                     clojurescript-mode
                     clj-refactor
                     elein
                     paredit
                     popup
                     rainbow-delimiters  ;; Mode for alternating paren colors
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
                     magit
                     minitest
                     rbenv
                     smex
                     flx-ido
                     js2-mode
                     key-chord
                     smartparens
                     jsx-mode
                     ace-jump-mode
                     ag
                     ))

;; Allow hash to be entered
(global-set-key (kbd "M-3") '(lambda () (interactive) (insert "#")))
(tool-bar-mode -1)

(autoload
  'ace-jump-mode-pop-mark
  "ace-jump-mode"
  "Ace jump back:-)"
  t)

(define-key global-map (kbd "C-x SPC") 'ace-jump-mode-pop-mark)
(autoload
  'ace-jump-mode
  "ace-jump-mode"
  "Emacs quick move minor mode"
  t)

(define-key global-map (kbd "C-c SPC") 'ace-jump-mode)

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

(require 'jsx-mode)
(add-to-list 'auto-mode-alist '("\\.jsx\\'" . jsx-mode))

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
(custom-set-variables
 '(coffee-tab-width 2)
 '(flycheck-coffeelintrc "~/.emacs.d/coffeelint.json"))

(add-hook 'coffee-mode-hook 'flymake-mode)
(add-hook 'coffee-mode-hook
          'disable-electric-indent-mode)

(require 'color-theme)
(load-theme 'wombat t)

(set-default-font "M+ 1mn-15")

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

; (require 'icomplete)
(require 'company)
(global-company-mode 1)
(add-hook 'after-init-hook #'global-flycheck-mode)
(add-hook 'after-init-hook 'global-company-mode)

;; ruby config
(require 'ag)
; (autoload 'ruby-mode "ruby-mode" nil t)
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

(require 'rbenv)
(global-rbenv-mode)
(rbenv-use-global)

(autoload 'enh-ruby-mode "enh-ruby-mode" "" t)
(add-to-list 'interpreter-mode-alist '("ruby" . enh-ruby-mode))

(setq enh-ruby-program rbenv-ruby-shim)

(add-hook 'enh-ruby-mode-hook 'minitest-mode)

;; js config
(setq js-indent-level 2)
(add-to-list 'load-path "~/.emacs.d/vendor/")
(require 'handlebars-mode)

;; autopair
(require 'autopair)

(defvar autopair-modes '(r-mode ruby-mode coffee-mode js-mode))
(defun turn-on-autopair-mode () (autopair-mode 1))
(dolist (mode autopair-modes) (add-hook (intern (concat (symbol-name mode) "-hook")) 'turn-on-autopair-mode))

(require 'paredit)
(defadvice paredit-mode (around disable-autopairs-around (arg))
  "Disable autopairs mode if paredit-mode is turned on"
  ad-do-it
  (if (null ad-return-value)
      (autopair-mode 1)
    (autopair-mode 0)
    ))

(ad-activate 'paredit-mode)

;; fix the PATH variable
(defun set-exec-path-from-shell-PATH ()
  (let ((path-from-shell (shell-command-to-string "TERM=vt100 $SHELL -i -c 'echo $PATH'")))
    (setenv "PATH" path-from-shell)
    (setq exec-path (split-string path-from-shell path-separator))))

(when window-system (set-exec-path-from-shell-PATH))

(setq ag-reuse-buffers 't)

(global-set-key "\M-/" 'hippie-expand)
;; Append result of evaluating previous expression (Clojure):
(defun cider-eval-last-sexp-and-append ()
  "Evaluate the expression preceding point and append result."
  (interactive)
  (let ((last-sexp (cider-last-sexp)))

;; we have to be sure the evaluation won't result in an error
(cider-eval-and-get-value last-sexp)
(with-current-buffer (current-buffer)
  (insert ";;=>"))
(cider-interactive-eval-print last-sexp)))

(require 'paredit)

; cider config
(require 'cider)
(setq nrepl-hide-special-buffers t)
(setq cider-show-error-buffer nil)

(define-key clojure-mode-map (kbd "C-o j") 'cider-jack-in)
(define-key clojure-mode-map (kbd "C-o J") 'cider-restart)
(define-key clojure-mode-map (kbd "C-o y") 'cider-eval-last-sexp-and-append)

(add-hook 'prog-mode-hook  'rainbow-delimiters-mode)
(add-hook 'cider-repl-mode-hook 'rainbow-delimiters-mode)

(define-clojure-indent
  (defroutes 'defun)
  (GET 2)
  (POST 2)
  (PUT 2)
  (DELETE 2)
  (HEAD 2)
  (ANY 2)
  (context 2))

(defun cider-send-and-evaluate-sexp ()
   "Sends the s-expression located before the point or the active
region to the REPL and evaluates it. Then the Clojure buffer is
activated as if nothing happened."
   (interactive)
   (if (not (region-active-p))
       (cider-insert-last-sexp-in-repl)
     (cider-insert-in-repl
      (buffer-substring (region-beginning) (region-end)) nil))
   (cider-switch-to-repl-buffer)
   (cider-repl-closing-return)
   (cider-switch-to-last-clojure-buffer)
   (message ""))

(add-hook 'clojure-mode-hook
          (lambda ()
            (local-set-key (kbd "M-e") 'forward-sexp)
            (local-set-key (kbd "M-a") 'backward-sexp)
            (local-set-key (kbd "C-c C-S-v") 'cider-send-and-evaluate-sexp)))

(defun my-paredit-nonlisp ()
  "Turn on paredit mode for non-lisps."
  (interactive)
  (set (make-local-variable 'paredit-space-for-delimiter-predicates)
       '((lambda (endp delimiter) nil)))
  (paredit-mode 1))

(define-key paredit-mode-map (kbd "C-^") 'paredit-remove-newlines)
(define-key paredit-mode-map (kbd "M-^") 'paredit-delete-indentation)

(defun turn-on-paredit () (paredit-mode t))

(add-hook 'emacs-lisp-mode-hook       'turn-on-paredit)
(add-hook 'lisp-mode-hook             'turn-on-paredit)
(add-hook 'lisp-interaction-mode-hook 'turn-on-paredit)
(add-hook 'scheme-mode-hook           'turn-on-paredit)
(add-hook 'clojure-mode-hook          'turn-on-paredit)
(add-hook 'cider-repl-mode-hook       'turn-on-paredit)
(add-hook 'sibiliant-mode-hook        'turn-on-paredit)
(add-hook 'js-mode-hook 'my-paredit-nonlisp)

(dolist (mode '(ruby coffee))
  (add-hook (intern (format "%s-mode-hook" mode))
            '(lambda ()
               (add-to-list (make-local-variable 'paredit-space-for-delimiter-predicates)
                            (lambda (_ _) nil))
               (enable-paredit-mode))))

(add-hook 'js2-mode-hook 'turn-on-paredit)

(defvar electrify-return-match
    "[\]}\)\"]"
    "If this regexp matches the text after the cursor, do an \"electric\" return.")

(defun electrify-return-if-match (arg)
    "If the text after the cursor matches `electrify-return-match' then
  open and indent an empty line between the cursor and the text.  Move the
  cursor to the new line."
    (interactive "P")
    (let ((case-fold-search nil))
      (if (looking-at electrify-return-match)
          (save-excursion (newline-and-indent)))
      (newline arg)
      (indent-according-to-mode)))
(add-hook 'paredit-mode-hook
        (lambda ()
          (local-set-key (kbd "RET") 'electrify-return-if-match)))

(require 'ac-cider)
(add-hook 'cider-mode-hook 'ac-flyspell-workaround)
(add-hook 'cider-mode-hook 'ac-cider-setup)
(add-hook 'cider-repl-mode-hook 'ac-cider-setup)

(when (package-installed-p '4clojure)
  (defadvice 4clojure-open-question (around 4clojure-open-question-around)
    "Start a cider/nREPL connection if one hasn't already been started when
    opening 4clojure questions."
    ad-do-it
    (unless cider-current-clojure-buffer
      (cider-jack-in)))

   (define-key clojure-mode-map (kbd "<f2> a") '4clojure-check-answers)
   (define-key clojure-mode-map (kbd "<f2> n") '4clojure-next-question)
   (define-key clojure-mode-map (kbd "<f2> p") '4clojure-previous-question))

(defvar ha-4clojure-place-file (concat user-emacs-directory "4clojure-place.txt"))

(defun ha-file-to-string (file)
  "Read the contents of FILE and return as a string."
  (with-temp-buffer
    (insert-file-contents file)
    (buffer-substring-no-properties (point-min) (point-max))))

(defun ha-file-to-list (file)
  "Return a list of lines in FILE."
  (split-string (ha-file-to-string file) "\n" t))

(defun ha-4clojure-last-project (file)
  (interactive "f")
  (if (file-exists-p file)
      (car (ha-file-to-list file))
    "1"))

(defun 4clojure-start-session ()
  (interactive)
  (4clojure-login "dagda1")
  (4clojure-open-question
   (ha-4clojure-last-project ha-4clojure-place-file)))

(global-set-key (kbd "<f2> s") '4clojure-start-session)

(defun ha-string-to-file (string file)
  (interactive "sEnter the string: \nFFile to save to: ")
  (with-temp-file file
    (insert string)))

(when (package-installed-p '4clojure)
  (defun ha-4clojure-store-place (num)
      (ha-string-to-file (int-to-string num) ha-4clojure-place-file))

  (defadvice 4clojure-next-question (after ha-4clojure-next-question)
    "Save the place for each question you progress to."
    (ha-4clojure-store-place (4clojure/problem-number-of-current-buffer)))

  (defadvice 4clojure-open-question (after ha-4clojure-next-question)
    "Save the place for each question you progress to."
    (ha-4clojure-store-place (4clojure/problem-number-of-current-buffer)))

  (ad-activate '4clojure-next-question)
  (ad-activate '4clojure-open-question))

(show-smartparens-global-mode +1)

(defun beautify-json ()
  (interactive)
  (let ((b (if mark-active (min (point) (mark)) (point-min)))
        (e (if mark-active (max (point) (mark)) (point-max))))
    (shell-command-on-region b e
     "python -mjson.tool" (current-buffer) t)))

;;set tab width globally
(setq-default indent-tabs-mode nil)
(setq-default tab-width 2)
(setq js-indent-level 2)
(setq css-indent-offset 2)
(provide 'init)
;;; init.el ends here


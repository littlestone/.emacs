
(setq user-full-name "Ulf Ejlertsson")
(setq user-mail-address "ulf.ejlertsson@gmail.com")

(global-auto-revert-mode t)

(setq mac-option-modifier nil
      mac-command-modifier 'meta
      x-select-enable-clipboard t)

;;;; macros
(defmacro after (mode &rest body)
  "`eval-after-load' MODE evaluate BODY."
  (declare (indent defun))
  `(eval-after-load ,mode
     '(progn ,@body)))

(require 'use-package)
(eval-when-compile
  (setq use-package-verbose (null byte-compile-current-file)))

(require 'git)
(require 'cl)
(require 'eshell)
(require 'em-smart)
(require 'yasnippet)
(require 'flx-ido)
(require 'ansi-color) 
(require 'helm-config)

(setq inhibit-splash-screen t
      initial-scratch-message nil)

(setq visible-bell t)
(fset 'yes-or-no-p 'y-or-n-p)

(when window-system
  (tool-bar-mode -1))

(server-start)

(add-to-list 'exec-path "/Users/ulf/bin")

(defun set-exec-path-from-shell-PATH ()
  (let ((path-from-shell (shell-command-to-string "$SHELL -i -c 'echo $PATH'")))
    (setenv "PATH" path-from-shell)
    (setq exec-path (split-string path-from-shell path-separator))))

(if window-system (set-exec-path-from-shell-PATH))

(use-package zenburn-theme)

(use-package yasnippet
  :init
  (progn
    (let ((snippets-dir (f-expand "snippets" user-emacs-directory)))
      (yas/load-directory snippets-dir)
      (setq yas/snippet-dirs snippets-dir))
    (yas-global-mode 1)
    (setq-default yas/prompt-functions '(yas/ido-prompt))))

(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)

(defun reload-dotemacs ()
  "Reload .emacs"
  (interactive)
  (load-file "~/.emacs.d/init.el"))
(global-set-key "\M-e" 'reload-dotemacs)

(set-default-font "Menlo-10")
(setq mac-allow-anti-aliasing t)
(set-frame-parameter (selected-frame) 'alpha '(96 84))
(add-to-list 'default-frame-alist '(alpha 96 84))

(defun toggle-transparency ()
  (interactive)
  (let ((param (cadr (frame-parameter nil 'alpha))))
    (if (and param (/= param 100))
        (set-frame-parameter nil 'alpha '(100 100))
      (set-frame-parameter nil 'alpha '(85 50)))))
(global-set-key (kbd "C-c t") 'toggle-transparency)

(setq redisplay-dont-pause t)

(if window-system (set-frame-font "Menlo-10"))
(if window-system (setq frame-title-format
  '("" invocation-name ": "(:eval (if (buffer-file-name)
                (abbreviate-file-name (buffer-file-name))
                  "%b")))))
(if (window-system)
  (set-frame-width (selected-frame) 200))

(defun lh-get-height-max ()
  (- (/ (cadddr (display-usable-bounds))
        (frame-char-height))
     2))

(defun lh-max-frames ()
  (interactive)
  (modify-all-frames-parameters (list (cons 'height (lh-get-height-max)))))
(lh-max-frames)

(require 'uniquify)
(setq
  uniquify-buffer-name-style 'post-forward
  uniquify-separator ":")

(use-package smex
  :init (smex-initialize)
  :bind ("M-x" . smex))

(use-package multiple-cursors
  :bind (("C->" . mc/mark-next-like-this)
         ("C-<" . mc/mark-previous-like-this)))

(use-package popwin
  :config (setq display-buffer-function 'popwin:display-buffer))

(use-package projectile
  :init (projectile-global-mode 1)
  :config
  (progn
    (setq projectile-enable-caching t)
    (setq projectile-require-project-root nil)
    (setq projectile-completion-system 'ido)
    (add-to-list 'projectile-globally-ignored-files ".DS_Store")))

(use-package magit
  :init
  (progn
    (use-package magit-blame)
    (bind-key "C-c C-a" 'magit-just-amend magit-mode-map))
  :config
  (progn    
    (setq magit-default-tracking-name-function 'magit-default-tracking-name-branch-only)
    (setq magit-set-upstream-on-push t)
    (setq magit-completing-read-function 'magit-ido-completing-read)
    (setq magit-stage-all-confirm nil)
    (setq magit-unstage-all-confirm nil)
    (add-hook 'magit-mode-hook 'rinari-launch))
  :bind ("C-x g" . magit-status))

(use-package git-gutter+
  :diminish git-gutter+-mode
  :config
  (progn
    (use-package git-gutter-fringe+
      :config
      (git-gutter-fr+-minimal))
    (global-git-gutter+-mode 1)))

(use-package ace-jump-mode
  :bind ("C-c SPC" . ace-jump-mode))

(use-package uniquify
  :config (setq uniquify-buffer-name-style 'post-forward
                uniquify-separator ":"))

(use-package eshell
  :bind ("M-e" . eshell)
  :init
  (add-hook 'eshell-first-time-mode-hook
            (lambda ()
              (add-to-list 'eshell-visual-commands "htop")))
  :config
  (progn
    (setq eshell-history-size 5000)
    (setq eshell-save-history-on-exit t)
    (setq eshell-where-to-jump 'begin)
    (setq eshell-review-quick-commands nil)
    (setq eshell-smart-space-goes-to-end t)
))

(use-package dash
  :config (dash-enable-font-lock))

(use-package dired-x)

(use-package ido
  :init (ido-mode 1)
  :config
  (progn
    (setq ido-case-fold t)
    (setq ido-everywhere t)
    (setq ido-enable-prefix nil)
    (setq ido-enable-flex-matching t)
    (setq ido-ubiquitous t)
    (setq ido-use-virtual-buffers t)
    (setq ido-create-new-buffer 'always)
    (setq ido-max-prospects 10)
    (setq ido-file-extensions-order '(".erl" ".el" ".hs" ".ml"))
    (add-to-list 'ido-ignore-files "\\.DS_Store")))

(use-package ido-vertical-mode
  :init (ido-vertical-mode 1))

(use-package helm-config
  :init
  (progn
    (bind-key "C-c M-x" 'helm-M-x)
    (bind-key "C-h a" 'helm-c-apropos)
    (bind-key "M-s a" 'helm-do-grep)
    (bind-key "M-s b" 'helm-occur)
    (bind-key "M-s F" 'helm-for-files)))

(use-package auto-complete-config
  :init
    (ac-config-default))

(use-package smartparens
  :init
  (progn
    (use-package smartparens-config)
    (smartparens-global-mode 1)
    (show-smartparens-global-mode 1))
  :config     
  (progn
    (require 'smartparens-config)
    (setq smartparens-strict-mode t)))

(use-package rainbow-delimiters)
(after 'rainbow-delimiters-autoloads
  (add-hook 'prog-mode-hook 'rainbow-delimiters-mode-enable))

(require 'ob-tangle)
(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))
(define-key global-map "\C-cl" 'org-store-link)
(define-key global-map "\C-ca" 'org-agenda)
(setq org-export-with-LaTeX-fragments t)
(setq org-log-done t)

(org-babel-do-load-languages
 'org-babel-load-languages
  '( (haskell . t)
     ;;(clojure. t)
     (sh . t)
     (python . t)
     (emacs-lisp . t)
     (C . t)
     (ocaml . t)
   ))

(setq org-src-fontify-natively t)
(setq org-src-tab-acts-natively t)

(require 'org-latex)
(unless (boundp 'org-export-latex-classes)
  (setq org-export-latex-classes nil))
(add-to-list 'org-export-latex-classes
             '("article"
               "\\documentclass{article}"
               ("\\section{%s}" . "\\section*{%s}")))

(setq org-export-latex-listings t)
(add-to-list 'org-export-latex-packages-alist '("" "listings"))
(add-to-list 'org-export-latex-packages-alist '("" "color"))

(setq org-directory "~/org")
(setq org-mobile-directory "~/Dropbox/Apps/MobileOrg")

(setq org-mobile-inbox-for-pull "~/org/inbox.org")

(setq deft-directory "~/Dropbox/org/deft")
(setq deft-use-filename-as-title t)
(setq deft-extension "org")
(setq deft-text-mode 'org-mode)

;;(require 'tex-site)

(require 'recentf)

;; get rid of `find-file-read-only' and replace it with something
;; more useful.
(global-set-key (kbd "C-x C-r") 'ido-recentf-open)

;; enable recent files mode.
(recentf-mode t)

; 50 files ought to be enough.
(setq recentf-max-saved-items 50)

(defun ido-recentf-open ()
  "Use `ido-completing-read' to \\[find-file] a recent file"
  (interactive)
  (if (find-file (ido-completing-read "Find recent file: " recentf-list))
      (message "Opening file...")
    (message "Aborting")))

(require 'undo-tree)
(after 'undo-tree-autoloads
  (global-undo-tree-mode t)
  (setq undo-tree-visualizer-relative-timestamps t)
  (setq undo-tree-visualizer-timestamps t))

(setq ack-prompt-for-directory t)
(setq ack-executable (executable-find "ack-grep"))

(add-hook 'c-mode-hook        'flyspell-prog-mode 1)
(add-hook 'c++-mode-hook      'flyspell-prog-mode 1)
(add-hook 'makefile-mode-hook 'flyspell-prog-mode 1)
(add-hook 'python-mode-hook   'flyspell-prog-mode 1)
(add-hook 'sh-mode-hook       'flyspell-prog-mode 1)
(add-hook 'c-mode-common-hook 'turn-on-auto-fill)
(add-hook 'c++-mode-common-hook ' turn-on-auto-fill)
(add-hook 'prog-mode-hook 'flyspell-prog-mode 1)

(add-to-list 'auto-mode-alist '("\\.scons$" . python-mode))

(require 'cc-mode)

(add-hook 'c-mode-common-hook 'turn-on-auto-fill)

(add-hook 'c-mode-hook (function (lambda()
                  (c-set-style "bsd")
                  (setq c-basic-offset 8)
                  (setq indent-tabs-mode nil)
                  ) ) )

(defun ue-c-namespace-open-indent (langelem)
  "Used with c-set-offset, indents namespace opening braces to the same indentation as the line on which the namespace declaration starts."
  (save-excursion
    (goto-char (cdr langelem))
    (let ((column (current-column)))
      (beginning-of-line)
      (skip-chars-forward " \t")
      (- (current-column) column)))
  )

(defun ue-c-namespace-indent (langelem)
  "Used with c-set-offset, indents namespace scope elements 2 spaces
from the namespace declaration iff the open brace sits on a line by itself."
  (save-excursion
    (if (progn (goto-char (cdr langelem))
               (setq column (current-column))
               (end-of-line)
               (while (and (search-backward "{" nil t)
                           (assoc 'incomment (c-guess-basic-syntax))))
               (skip-chars-backward " \t")
               (bolp))
        2)))

(add-hook 'c++-mode-common-hook ' turn-on-auto-fill)

(add-hook 'c++-mode-hook (function (lambda()
                    (c-set-style "bsd")
                        (c-set-offset 'innamespace 'ue-c-namespace-indent)
                            (c-set-offset 'namespace-open 'ue-c-namespace-open-indent)
                                (c-set-offset 'access-label -3)
                    (setq c-basic-offset 4)
                    (setq indent-tabs-mode nil)
                    ) ))

(add-to-list 'load-path "~/.emacs.d" "~/Dev/svn/llvm/tools/clang/utils")
;;(setq load-path (cons "~/.emacs.d" "~/Dev/svn/llvm/trunk/tools/clang/utils"))
;;(setq ac-sources '(ac-source-clang-complete))
;;(setq ac-auto-start nil)
;;(define-key c-mode-base-map (kbd "M-/") 'auto-complete)

(setq load-path
  (cons (expand-file-name "~/Dev/svn/llvm/utils/emacs") load-path))
(require 'llvm-mode)

(setq load-path
  (cons (expand-file-name "~/Dev/svn/llvm/utils/emacs/tablegen-mode.el") load-path))
(require 'tablegen-mode)

;(require 'rainbow-delimiters)
;(after 'rainbow-delimiters-autoloads
;  (add-hook 'prog-mode-hook 'rainbow-delimiters-mode-enable))
;(global-rainbow-delimiters-mode)

(require 'highlight-parentheses)

(require 'paredit)

;(require 'shen-mode)
;(require 'inf-shen) ; <- for interaction with an external shen process

;(setq inferior-lisp-program "/Applications/Clozure\ CL.app")
;     (require 'slime-autoloads)

(setq geiser-active-implementations '(racket))

;; quack mode settings for scheme
(autoload 'quack "quack" nil t)

;; This hook lets you use your theme colours instead of quack's ones.
(defun scheme-mode-quack-hook ()
  (setq quack-global-menu-p nil)
  ;;(require 'quack)
  (quack-pretty-lambda-p t)
  (setq quack-fontify-style 'emacs))

(add-hook 'scheme-mode-hook 'scheme-mode-quack-hook)

(add-to-list
   'load-path
       (car (file-expand-wildcards "/usr/local/lib/erlang/lib/tools-*/emacs")))
(setq erlang-root-dir "/usr/local/lib/erlang")
(setq exec-path (cons "/usr/local/lib/erlang/bin" exec-path))
(require 'erlang-start)
(require 'erlang-flymake)
(defvar inferior-erlang-prompt-timeout t)

(add-to-list 'auto-mode-alist '("\\.erl?$" . erlang-mode))
(add-to-list 'auto-mode-alist '("\\.hrl?$" . erlang-mode))

(add-to-list 'load-path "/usr/local/share/distel/elisp")
(require 'distel)
(distel-setup)

;; FlyMake for Erlang.
(require 'flymake)
(setq flymake-gui-warnings-enabled nil)
(setq flymake-log-level 3)
(erlang-flymake-only-on-save)

;(require 'flymake)
(defun flymake-erlang-init ()
  (let* ((temp-file (flymake-init-create-temp-buffer-copy
                     'flymake-create-temp-inplace))
         (local-file (file-relative-name temp-file
                (file-name-directory buffer-file-name))))
    (list "~/bin/eflymake" (list local-file))))

(add-to-list 'flymake-allowed-file-name-masks '("\\.erl\\'" flymake-erlang-init))

(defun ue-erlang-mode-hook ()
        ;; when starting an Erlang shell in Emacs, default in the node name
        (setq inferior-erlang-machine-options '("-sname" "emacs"))
        ;; add Erlang functions to an imenu menu
        (imenu-add-to-menubar "imenu")
        ;; customize keys
        (local-set-key [return] 'newline-and-indent)
        (flymake-mode 1)
        )
;; Some Erlang customizations
(add-hook 'erlang-mode-hook 'ue-erlang-mode-hook)



(defun ue-erlang-mode-hook ()
(flymake-mode 1))

(add-hook 'erlang-mode-hook 'ue-erlang-mode-hook)

(require 'haskell-mode)
(add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
(add-hook 'haskell-mode-hook 'turn-on-haskell-indent)
(add-hook 'haskell-mode-hook 'font-lock-mode)

(add-hook 'haskell-mode-hook 'turn-on-haskell-font-lock)
(setq haskell-font-lock-symbols t)

 (autoload 'ghc-init "ghc" nil t)
 (add-hook 'haskell-mode-hook
           (lambda ()
               (ghc-init)
               (flymake-mode)
               (require 'auto-complete-config)
               (auto-complete-mode t)
               (add-to-list 'ac-sources 'ac-source-ghc-mod)))
;; haskell-mode hooks
(add-hook 'haskell-mode-hook 'capitalized-words-mode)
(add-hook 'haskell-mode-hook 'turn-on-haskell-decl-scan)
(defun pretty-lambdas-haskell ()
  (font-lock-add-keywords
   nil `((,(concat "\\(" (regexp-quote "\\") "\\)")
          (0 (progn (compose-region (match-beginning 1) (match-end 1)
                                    ,(make-char 'greek-iso8859-7 107))
                    nil))))))

(add-hook 'haskell-mode-hook (lambda () (ghc-init) (flymake-mode)))


(add-hook 'haskell-mode-hook 'pretty-lambdas-haskell)

(eval-after-load 'haskell-font-lock
 '(setq haskell-font-lock-symbols-alist
        (delq nil
              (mapcar (lambda (rewrite)
                        (if (member (car rewrite) '("->" "<-"))
                            nil rewrite))
                      haskell-font-lock-symbols-alist))))

'(agda2-include-dirs (quote ("/Users/ulf/Dev/haskell/lib-0.6/src")))
(load-file (let ((coding-system-for-read 'utf-8))
                (shell-command-to-string "agda-mode locate")))

(add-to-list 'auto-mode-alist '("\\.ml[iylp]?" . tuareg-mode))
(autoload 'tuareg-mode "tuareg" "Major mode for editing OCaml code" t)
(autoload 'tuareg-run-ocaml "tuareg" "Run an inferior OCaml process." t)
(autoload 'ocamldebug "ocamldebug" "Run the OCaml debugger" t)
(autoload 'tuareg-imenu-set-imenu "tuareg-imenu"
  "Configuration of imenu for tuareg" t)
(add-hook 'tuareg-mode-hook 'tuareg-imenu-set-imenu)
(setq auto-mode-alist
    (append '(("\\.ml[ily]?$" . tuareg-mode)
       ("\\.topml$" . tuareg-mode))
       auto-mode-alist))

(setq tuareg-font-lock-symbols t)

;; Indent `=' like a standard keyword.
(setq tuareg-lazy-= t)
;; Indent [({ like standard keywords.
(setq tuareg-lazy-paren t)
;; No indentation after `in' keywords.
(setq tuareg-in-indent 0)

(add-hook 'tuareg-mode-hook
          ;; Turn on auto-fill minor mode.
          (lambda () (auto-fill-mode 1)))

(setq auto-mode-alist (remove (rassoc 'verilog-mode auto-mode-alist) auto-mode-alist))
;;(load-file (concat site-packages-dir "/ProofGeneral/generic/proof-site.el"))
(setq coq-prog-name "/usr/local/bin/coqtop")
(setq auto-mode-alist (cons '("\.v$" . coq-mode) auto-mode-alist))
(add-to-list 'load-path "/usr/local/lib/emacs/site-lisp")
(autoload 'coq-mode "coq" "Major mode for editing Coq vernacular." t)

;(require 'python-mode)
;(require 'python-pep8)
;(require 'python-pylint)

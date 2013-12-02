(require 'cask "~/.cask/cask.el")
(cask-initialize)

(require 'pallet)

;; This file loads org-mode and then loads the rest of our Emacs initialization
;; from Emacs lisp embedded in literate org-mode files.
;; Load up org-mode and (now included) org babel for elisp embedded in org mode files
(setq dotfiles-dir (file-name-directory (or (buffer-file-name) load-file-name)))

(let* ((org-dir (expand-file-name
                 "lisp" (expand-file-name
                         "org" (expand-file-name
                                "src" dotfiles-dir))))
       (org-contrib-dir (expand-file-name
                         "lisp" (expand-file-name
                                 "contrib" (expand-file-name
                                            ".." org-dir))))
       (load-path (append (list org-dir org-contrib-dir)
                          (or load-path nil))))

;; load up org-mode and org-babel
(require 'org-install)
(require 'ob-tangle))

;; load up all literate org-mode files in this directory
(mapc #'org-babel-load-file (directory-files dotfiles-dir t "\\.org$"))

(org-babel-load-file (expand-file-name "emacs-config.org" user-emacs-directory))


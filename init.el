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

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes (quote ("146d24de1bb61ddfa64062c29b5ff57065552a7c4019bee5d869e938782dfc2a" "a53714de04cd4fdb92ed711ae479f6a1d7d5f093880bfd161467c3f589725453" "11d069fbfb0510e2b32a5787e26b762898c7e480364cbc0779fe841662e4cf5d" default)))
 '(powerline-default-separator (quote arrow)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

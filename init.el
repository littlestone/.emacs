;;
;; -*- Emacs-Lisp -*-
;; ---------------------------------------------------------------------------
;; init.el
;; ---------------------------------------------------------------------------
;;;;
;; This file loads Org-mode and then loads the rest of our Emacs initialization
;; from Emacs lisp embedded in literate Org-mode files.

(org-babel-load-file (expand-file-name "emacs-config.org" user-emacs-directory))

;; We do not want to mess with these...
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-enabled-themes (quote (zenburn)))
 '(custom-safe-themes (quote ("31e407508d9a61e1f7cd7933d52612f3e440b89f8da48fe56e4e483c227ae7b9" "f77fa50af9de18450bcfd2f9c27bc69fa1693417" default)))
 '(quack-global-menu-p nil)
 '(send-mail-function (quote smtpmail-send-it))
 '(smtpmail-smtp-server "smtp.gmail.com")
 '(smtpmail-smtp-service 587))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;; End of File

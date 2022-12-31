;; -*- mode: elisp -*-

;;start the server
(server-start)

;; MELPA
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
;; Comment/uncomment this line to enable MELPA Stable if desired.  See `package-archive-priorities`
;; and `package-pinned-packages`. Most users will not need or want to do this.
;;(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/") t)
(package-initialize)


;; Disable the splash screen (to enable it again, replace the t with 0)
(setq inhibit-splash-screen 0)

;; Enable transient mark mode
(transient-mark-mode 1)

;; Enable visual-line-mode

(visual-line-mode 1)

;;;;Org mode configuration
;; Enable Org mode
(require 'org)
;; Make Org mode work with files ending in .org
;; (add-to-list 'auto-mode-alist '("\\.org$" . org-mode))
;; The above is the default in recent emacsen

(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-enabled-themes '(dracula))
 '(custom-safe-themes
   '("fe1c13d75398b1c8fd7fdd1241a55c286b86c3e4ce513c4292d01383de152cb7" default))
 '(org-agenda-files '("/home/iain/notes/quick_notes.org"))
 '(package-selected-packages '(counsel dracula-theme org-modern)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "Liberation Sans" :foundry "1ASC" :slant normal :weight normal :height 158 :width normal)))))


;; Set keyboard shortcuts

(global-set-key "\C-cc" 'org-capture)

;; org-capture notes

(setq org-default-notes-file "~/notes/quick_notes.org")

(setq org-capture-templates
      '(("n" "Quick Note" entry (file org-default-notes-file)
	 "* %?" :empty-lines 1)))

;; Ivy
(ivy-mode 1)

(use-package ivy :demand
      :config
      (setq ivy-use-virtual-buffers t
            ivy-count-format "%d/%d "))
(put 'erase-buffer 'disabled nil)

;; Dired - Hide hidden files
  (use-package dired-hide-dotfiles
    :hook
    (dired-mode . dired-hide-dotfiles-mode)
    :bind
    (:map dired-mode-map ("." . dired-hide-dotfiles-mode)))

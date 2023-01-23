;; -*- mode: elisp -*-

;;start the server
(server-start)

;; Initialize package sources
(require 'package)

(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("org" . "https://orgmode.org/elpa/")
                         ("elpa" . "https://elpa.gnu.org/packages/")))

(unless package--initialized)
  (package-initialize)

(unless package-archive-contents
  (package-refresh-contents))


(require 'use-package)
(setq use-package-always-ensure t)

;; what is this?? - required to auto install packages
(unless (package-installed-p 'use-package)
  (package-install 'use-package))


;; Visuals 
(setq inhibit-startup-message t) ;Disable the splash screen (to enable it again, replace the t with 0)
(scroll-bar-mode -1) ; Disable visible scroll bar
(tool-bar-mode -1) ; Disable toolbar
(tooltip-mode -1)  ; Disable tooltips
(set-fringe-mode 10)
(menu-bar-mode -1)

(column-number-mode)
(global-display-line-numbers-mode t)

;; Disable line numbers for some modes
(dolist (mode '(org-mode-hook
                term-mode-hook
                shell-mode-hook
                eshell-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

(set-face-attribute 'default nil :font "Hack" :height 120)

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

(use-package doom-themes
  :init (load-theme 'doom-gruvbox t))

;; Set keyboard shortcuts

(global-set-key "\C-cc" 'org-capture)

(global-set-key (kbd "<escape>") 'keyboard-escape-quit)



(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package ivy-rich
  :init
  (ivy-rich-mode 1))


;; org-capture notes

(setq org-default-notes-file "~/notes/quick_notes.org")

(setq org-capture-templates
      '(("n" "Quick Note" entry
	 (file org-default-notes-file)
	 "* %?\n %i" :empty-lines 1)
	("r" "Recipe" entry
	 (file+headline "~/notes/ideas.org" "Recipes to try")
	 "** %?\n")
	("j" "Journal entry" entry
	 (file+datetree "~/notes/journal.org")
	 "**** %U %^{Title}\n %?" :empty-lines 1)))


;; Which key
(use-package which-key
  :init (which-key-mode)
  :diminish which-key-mode
  :config
  (setq which-key-idle-delay 1))

;; Ivy

(use-package ivy
  :diminish
  :bind ("C-s" . swiper)
  :config
  (setq ivy-use-virtual-buffers t ;what does this do?
        ivy-count-format "%d/%d ") ; what does this do?
  (ivy-mode 1))

(put 'erase-buffer 'disabled nil) ; what does this do?


(use-package helpful
  :custom
  (counsel-describe-function-function #'helpful-callable)
  (counsel-describe-variable-function #'helpful-variable)
  :bind
  ([remap describe-function] . counsel-describe-function)
  ([remap describe-command] . helpful-command)
  ([remap describe-variable] . counsel-describe-variable)
  ([remap describe-key] . helpful-key))



;; Dired - Hide hidden files
(use-package dired-hide-dotfiles
  :hook
  (dired-mode . dired-hide-dotfiles-mode)
  :bind
  (:map dired-mode-map ("." . dired-hide-dotfiles-mode)))

;; Dired - Store backups
(setq
   backup-by-copying t      ; don't clobber symlinks
   backup-directory-alist
    '(("." . "~/.backups/"))    ; don't litter my fs tree
   delete-old-versions t
   kept-new-versions 6
   kept-old-versions 2
   version-control t)       ; use versioned backups

;; Avoid lock files
(setq create-lockfiles nil)

(use-package projectile
  :diminish projectile-mode
  :config (projectile-mode)
  :custom ((projectile-completion-system 'ivy))
  :bind-keymap
  ("C-c p" . projectile-command-map)
  :init
  ;; NOTE: Set this to the folder where you keep your Git repos!
  (when (file-directory-p "~/computing")
    (setq projectile-project-search-path '("~/computing")))
  (setq projectile-switch-project-action #'projectile-dired))

(use-package counsel-projectile
  :config (counsel-projectile-mode))

(use-package doom-modeline
  :ensure t
  :init (doom-modeline-mode 1)
  :custom ((doom-modeline-height 15)))


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(rainbow-delimiters ivy-rich helpful which-key doom-modeline use-package shrink-path doom-themes dired-hide-dotfiles counsel-projectile compat)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

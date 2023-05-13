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
(defun me/org-mode-setup ()
  (org-indent-mode)
  (variable-pitch-mode 1)
  (visual-line-mode 1))

(use-package org
  :hook (org-mode . me/org-mode-setup))


(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)

(use-package doom-themes
  :init (load-theme 'doom-gruvbox t))

;; Set keyboard shortcuts

(global-set-key "\C-cc" 'org-capture)

					;
;(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package all-the-icons)

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

(use-package ivy-rich
  :init
  (ivy-rich-mode 1))

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

;; Copy between open dired-buffers
(setq dired-dwim-target t)

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


;; Custom functions

(defun me/counsel-notes ()
      "list everything recursively"
      (interactive)
      (let* ((cands (split-string
                     (shell-command-to-string "find ~/notes -type f") "\n" t)))
        (ivy-read "File: " cands
                  :action #'find-file
                  :caller 'me/counsel-notes)))

(defun me/batch-open-rad-notes ()
(mapc #'find-file-noselect
      (directory-files-recursively "~/notes/Radiology notes/" "")))


;(defun me/gdl ()
;  (interactive)
;  (dired "~/downloads")) 



;; Key bindings

;(global-set-key (kbd "C-M-j") 'counsel-switch-buffer)
;(define-key dired-mode-map (kbd "C-c gdl") 'me/gdl) 
;(global-set-key (kbd "C-c gdl") 'me/gdl) 

(global-set-key (kbd "C-c gh") (lambda () (interactive) (dired "~/"))) 
(global-set-key (kbd "C-c gtr") (lambda () (interactive) (dired "~/.local/share/Trash/files"))) 
(global-set-key (kbd "C-c gdl") (lambda () (interactive) (dired "~/downloads")))
(global-set-key (kbd "C-c gco") (lambda () (interactive) (dired "~/computing")))
(global-set-key (kbd "C-c ggh") (lambda () (interactive) (dired "~/computing/git/")))
(global-set-key (kbd "C-c ggd") (lambda () (interactive) (dired "~/computing/git/dotfiles/")))
(global-set-key (kbd "C-c gmd") (lambda () (interactive) (dired "~/my_docs")))
(global-set-key (kbd "C-c gfn") (lambda () (interactive) (dired "~/my_docs/financial/")))
(global-set-key (kbd "C-c gta") (lambda () (interactive) (dired "~/my_docs/financial/Tax")))
(global-set-key (kbd "C-c gps") (lambda () (interactive) (dired "~/my_docs/financial/Payslips")))
(global-set-key (kbd "C-c gwd") (lambda () (interactive) (dired "~/work_docs")))                                
(global-set-key (kbd "C-c gvi") (lambda () (interactive) (dired "~/media/videos")))                             
(global-set-key (kbd "C-c gtt") (lambda () (interactive) (dired "~/media/videos/tutorials")))                   
(global-set-key (kbd "C-c gph") (lambda () (interactive) (dired "~/media/photos")))                             
(global-set-key (kbd "C-c gwp") (lambda () (interactive) (dired "~/media/pictures/wallpapers")))                
(global-set-key (kbd "C-c g.c") (lambda () (interactive) (dired "~/.config")))                                  
(global-set-key (kbd "C-c gmu") (lambda () (interactive) (dired "~/media/music")))                              
(global-set-key (kbd "C-c gpi") (lambda () (interactive) (dired "~/media/pictures")))                           
(global-set-key (kbd "C-c gtv") (lambda () (interactive) (dired "~/media/TV")))                                 
(global-set-key (kbd "C-c gfi") (lambda () (interactive) (dired "~/media/Films")))                              
(global-set-key (kbd "C-c gws") (lambda () (interactive) (dired "~/media/websites")))                           
(global-set-key (kbd "C-c gsc") (lambda () (interactive) (dired "~/scripts")))                                  
(global-set-key (kbd "C-c ggs") (lambda () (interactive) (dired "~/computing/git/scripts/")))                   
(global-set-key (kbd "C-c g.t") (lambda () (interactive) (dired "~/.test")))                                    
(global-set-key (kbd "C-c gme") (lambda () (interactive) (dired "~/media")))                                    
(global-set-key (kbd "C-c grm") (lambda () (interactive) (dired "/run/media/")))                                
(global-set-key (kbd "C-c gpm") (lambda () (interactive) (dired "~/phone_media")))                              
(global-set-key (kbd "C-c goc") (lambda () (interactive) (dired "~/work_docs/Oncall")))                         
(global-set-key (kbd "C-c gst") (lambda () (interactive) (dired "~/work_docs/ST5")))                            
(global-set-key (kbd "C-c gtb") (lambda () (interactive) (dired "~/work_docs/Reading/Textbooks")))              
(global-set-key (kbd "C-c g2b") (lambda () (interactive) (dired "~/work_docs/Reading/Textbooks/Exam/2b")))      
(global-set-key (kbd "C-c gwg") (lambda () (interactive) (dired "~/work_docs/Reading/Guidelines/Ghali_UHW")))   
(global-set-key (kbd "C-c gss") (lambda () (interactive) (dired "~/media/pictures/screenshots")))               
(global-set-key (kbd "C-c gsi") (lambda () (interactive) (dired "~/media/pictures/saved_images")))              
(global-set-key (kbd "C-c gbf") (lambda () (interactive) (dired "~/media/Books/fiction")))                      
(global-set-key (kbd "C-c gbn") (lambda () (interactive) (dired "~/media/Books/non_fiction")))                  
(global-set-key (kbd "C-c gbo") (lambda () (interactive) (dired "~/media/Books/")))                             
(global-set-key (kbd "C-c gvn") (lambda () (interactive) (dired "~/media/videos/new")))                         
(global-set-key (kbd "C-c gvl") (lambda () (interactive) (dired "~/media/videos/library")))                     
(global-set-key (kbd "C-c gvi") (lambda () (interactive) (dired "~/media/videos")))                             
(global-set-key (kbd "C-c gws") (lambda () (interactive) (dired "~/media/websites/")))                          
(global-set-key (kbd "C-c gtp") (lambda () (interactive) (dired "~/computing/templates")))                      
(global-set-key (kbd "C-c g.l") (lambda () (interactive) (dired "~/.local")))                                   
(global-set-key (kbd "C-c gen") (lambda () (interactive) (dired "~/.test/envs")))                               
(global-set-key (kbd "C-c gts") (lambda () (interactive) (dired "~/.test/scripts/")))                           
(global-set-key (kbd "C-c glt") (lambda () (interactive) (dired "~/computing/laptop")))                         
(global-set-key (kbd "C-c gdt") (lambda () (interactive) (dired "~/computing/desktop")))                        
(global-set-key (kbd "C-c gy1") (lambda () (interactive) (dired "~/work_docs/ST1")))                            
(global-set-key (kbd "C-c gy2") (lambda () (interactive) (dired "~/work_docs/ST2")))                            
(global-set-key (kbd "C-c gy3") (lambda () (interactive) (dired "~/work_docs/ST3")))                            
(global-set-key (kbd "C-c gy4") (lambda () (interactive) (dired "~/work_docs/ST4")))                            
(global-set-key (kbd "C-c gcp") (lambda () (interactive) (dired "~/.test/cprog")))                              
(global-set-key (kbd "C-c gsf") (lambda () (interactive) (dired "~/.shellfunctions")))                          
(global-set-key (kbd "C-c gnn") (lambda () (interactive) (dired "~/notes")))
(global-set-key (kbd "C-c gwr") (lambda () (interactive) (dired "~/work_docs/ST4/Rota")))
(global-set-key (kbd "C-c gja") (lambda () (interactive) (dired "~/work_docs/Reading/Journal articles/")))
(global-set-key (kbd "C-c gba") (lambda () (interactive) (dired "~/.local/share/lf")))
(global-set-key (kbd "C-c gnv") (lambda () (interactive) (dired "~/.config/nvim")))
(global-set-key (kbd "C-c gpp") (lambda () (interactive) (dired "~/.local/share/nvim/site/pack/packer/start/")))
(global-set-key (kbd "C-c gse") (lambda () (interactive) (dired "/etc/))")))


(global-set-key (kbd "C-x C-b") 'ibuffer)
(global-set-key (kbd "<C-M-left>") 'previous-buffer)
(global-set-key (kbd "<C-M-right>") 'next-buffer)
(global-set-key (kbd "C-c n") 'me/counsel-notes)
(global-set-key (kbd "C-.") 'other-window)



(use-package magit
  :custom
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1))


(org-babel-do-load-languages
  'org-babel-load-languages
  '((emacs-lisp . t)
    (python . t)))

(require 'org-tempo)

(add-to-list 'org-structure-template-alist '("sh" . "src shell"))
(add-to-list 'org-structure-template-alist '("el" . "src emacs-lisp"))
(add-to-list 'org-structure-template-alist '("py" . "src python"))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(magit all-the-icons-ivy-rich all-the-icons rainbow-delimiters helpful which-key doom-modeline use-package shrink-path doom-themes dired-hide-dotfiles compat)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
(put 'dired-find-alternate-file 'disabled nil)

;;start the server
(server-start)

;; Initialize package sources
 (require 'package)

 (setq package-archives '(("melpa" . "https://melpa.org/packages/")
                          ("org" . "https://orgmode.org/elpa/")
                          ("elpa" . "https://elpa.gnu.org/packages/")))

 (unless package--initialized
   (package-initialize))

 (unless package-archive-contents
   (package-refresh-contents))


;; (require 'use-package)
 (setq use-package-always-ensure t)

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
(global-display-line-numbers-mode -1)

;; Disable line numbers for some modes
(dolist (mode '(org-mode-hook
                term-mode-hook
                shell-mode-hook
                eshell-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

(set-face-attribute 'default nil :font "FreeSans" :height 140)

;; Set the fixed pitch face
(set-face-attribute 'fixed-pitch nil :font "FreeSans" :height 140)

;; Set the variable pitch face
(set-face-attribute 'variable-pitch nil :font "FreeSans" :height 140 :weight 'regular)

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

    ;; org-capture notes

  (setq org-default-notes-file "~/notes/quick_notes.org")
  (setq org-support-shift-select t)

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

(org-babel-do-load-languages
  'org-babel-load-languages
  '((emacs-lisp . t)
    (python . t)))

(require 'org-tempo)

(add-to-list 'org-structure-template-alist '("sh" . "src shell"))
(add-to-list 'org-structure-template-alist '("el" . "src emacs-lisp"))
(add-to-list 'org-structure-template-alist '("py" . "src python"))

(use-package doom-themes)

(use-package modus-themes
  :init
  (setq modus-themes-org-blocks 'gray-background)
  (load-theme 'modus-vivendi-tinted t))

(use-package rainbow-delimiters
:hook (prog-mode . rainbow-delimiters-mode))

;;  (use-package all-the-icons)

(use-package nerd-icons-dired
:hook
(dired-mode . nerd-icons-dired-mode))
;; If not working run M-x nerd-icons-install-fonts

(use-package which-key
  :init (which-key-mode)
  :diminish which-key-mode
  :config
  (setq which-key-idle-delay 1))

(use-package doom-modeline
  :ensure t
  :init (doom-modeline-mode 1)
  :custom ((doom-modeline-height 15)))

(winner-mode 1)

(electric-pair-mode 1)

(recentf-mode 1)
(setq recentf-max-menu-items 25)
(setq recentf-max-saved-items 25)

(use-package vertico
  :ensure t
  :custom
  (vertico-cycle t)
  :init
  (vertico-mode))

(use-package marginalia
  :after vertico
  :ensure t
  :custom
  (marginalia-annotators '(marginalia-annotators-heavy marginalia-annotators-light nil))
  :init
  (marginalia-mode))

(use-package consult
:demand t
:bind ("C-s" . consult-line))

(use-package orderless
:ensure t
:custom
(completion-styles '(orderless basic))
(completion-category-overrides '((file (styles basic partial-completion)))))

(use-package embark
  :ensure t

  :bind
  (("C-." . embark-act)         ;; pick some comfortable binding
   ("C-;" . embark-dwim)        ;; good alternative: M-.
   ("C-h B" . embark-bindings)) ;; alternative for `describe-bindings'

  :init

  ;; Optionally replace the key help with a completing-read interface
  (setq prefix-help-command #'embark-prefix-help-command)

  :config

  ;; Hide the mode line of the Embark live/completions buffers
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none)))))

;; Consult users will also want the embark-consult package.
(use-package embark-consult
  :ensure t ; only need to install it, embark loads it after consult if found
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

(use-package dired-hide-dotfiles
                :hook
                (dired-mode . dired-hide-dotfiles-mode)
                :bind
                (:map dired-mode-map
;;                      ("h" . dired-up-directory )
                      ("." . dired-hide-dotfiles-mode )))

              (add-hook 'dired-mode-hook 'dired-hide-details-mode)
              (setq dired-listing-switches "-al --group-directories-first")

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

(use-package dired-subtree :ensure t
  :after dired
  :config
  (bind-key "<tab>" #'dired-subtree-toggle dired-mode-map)
  (bind-key "<backtab>" #'dired-subtree-cycle dired-mode-map))

(use-package lsp-mode
   :custom
 (lsp-completion-provider :none)
 :init
 (defun my/lsp-mode-setup-completion ()
   (setf (alist-get 'styles (alist-get 'lsp-capf completion-category-defaults))
         '(orderless))) ;; Configure orderless
   :hook (;; replace XXX-mode with concrete major-mode(e. g. python-mode)
          (python-ts-mode . lsp)
          (bash-ts-mode . lsp)
          (lua-mode . lsp)
          ;; if you want which-key integration
(lsp-completion-mode . my/lsp-mode-setup-completion))
   :commands lsp)

 (use-package lsp-ui
   :hook (lsp-mode . lsp-ui-mode)
   :custom
   (lsp-ui-doc-position 'bottom))

(use-package python-mode
  :ensure nil
  :mode "\\.py\\'"
  :hook (python-ts-mode . lsp))

(use-package lsp-pyright
:ensure t
:hook (python-ts-mode . (lambda ()
                        (require 'lsp-pyright)
                        (lsp))))

(use-package python-black
    :ensure t
:demand t
:after python
:hook ((python-ts-mode . python-black-on-save-mode)))

(use-package lua-mode
  :ensure nil
  :mode "\\.lua\\'"
  :hook (lua-mode . lsp))

(use-package corfu
   :after orderless
   ;; Optional customizations
   :custom
   (corfu-cycle t)                ;; Enable cycling for `corfu-next/previous'
   (corfu-auto t)                 ;; Enable auto completion
   (corfu-separator ?\s)          ;; Orderless field separator
   (corfu-quit-at-boundary t)   ;; Never quit at completion boundary
   (corfu-quit-no-match t)      ;; Never quit, even if there is no match
   (corfu-preview-current nil)    ;; Disable current candidate preview
   ;; (corfu-preselect-first nil)    ;; Disable candidate preselection
   ;; (corfu-on-exact-match nil)     ;; Configure handling of exact matches
   ;; (corfu-echo-documentation nil) ;; Disable documentation in the echo area
   (corfu-scroll-margin 5)        ;; Use scroll margin
   ;; Enable Corfu only for certain modes.
   :hook ((prog-mode . corfu-mode)
          (shell-mode . corfu-mode)
          (eshell-mode . corfu-mode))
   ;; Recommended: Enable Corfu globally.
   ;; This is recommended since Dabbrev can be used globally (M-/).
   ;; See also `corfu-excluded-modes'.
   :init
   (global-corfu-mode) ; This does not play well in eshell if you run a repl
   (setq corfu-auto t))

;; Add extensions
(use-package cape
  ;; Bind dedicated completion commands
  ;; Alternative prefix keys: C-c p, M-p, M-+, ...
  :bind (("C-c p p" . completion-at-point) ;; capf
         ("C-c p t" . complete-tag)        ;; etags
         ("C-c p d" . cape-dabbrev)        ;; or dabbrev-completion
         ("C-c p h" . cape-history)
         ("C-c p f" . cape-file)
         ("C-c p k" . cape-keyword)
         ("C-c p s" . cape-elisp-symbol)
         ("C-c p e" . cape-elisp-block)
         ("C-c p a" . cape-abbrev)
         ("C-c p l" . cape-line)
         ("C-c p w" . cape-dict)
         ("C-c p :" . cape-emoji)
         ("C-c p \\" . cape-tex)
         ("C-c p _" . cape-tex)
         ("C-c p ^" . cape-tex)
         ("C-c p &" . cape-sgml)
         ("C-c p r" . cape-rfc1345))
  :init
  ;; Add to the global default value of `completion-at-point-functions' which is
  ;; used by `completion-at-point'.  The order of the functions matters, the
  ;; first function returning a result wins.  Note that the list of buffer-local
  ;; completion functions takes precedence over the global list.
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  (add-to-list 'completion-at-point-functions #'cape-file)
  (add-to-list 'completion-at-point-functions #'cape-elisp-block)
  ;;(add-to-list 'completion-at-point-functions #'cape-history)
  ;;(add-to-list 'completion-at-point-functions #'cape-keyword)
  ;;(add-to-list 'completion-at-point-functions #'cape-tex)
  ;;(add-to-list 'completion-at-point-functions #'cape-sgml)
  ;;(add-to-list 'completion-at-point-functions #'cape-rfc1345)
  ;;(add-to-list 'completion-at-point-functions #'cape-abbrev)
  ;;(add-to-list 'completion-at-point-functions #'cape-dict)
  ;;(add-to-list 'completion-at-point-functions #'cape-elisp-symbol)
  ;;(add-to-list 'completion-at-point-functions #'cape-line)
)

;; Use Dabbrev with Corfu!
(use-package dabbrev
  ;; Swap M-/ and C-M-/
  :bind (("M-/" . dabbrev-completion)
         ("C-M-/" . dabbrev-expand))
  :config
  (add-to-list 'dabbrev-ignored-buffer-regexps "\\` ")
  ;; Since 29.1, use `dabbrev-ignored-buffer-regexps' on older.
  (add-to-list 'dabbrev-ignored-buffer-modes 'doc-view-mode)
  (add-to-list 'dabbrev-ignored-buffer-modes 'pdf-view-mode))

(use-package eglot
  :ensure t
  :defer t
  :hook ((python-mode . eglot-ensure)
         (lua-mode . eglot-ensure))
  :config
  (add-to-list 'eglot-server-programs
               `(python-mode
                 . ,(eglot-alternatives '("pyright-langserver" "--stdio")))))

(setq vc-follow-symlinks t)

(use-package pdf-tools
:defer t
:commands (pdf-loader-install)
:mode "\\.pdf\\'"
:bind (:map pdf-view-mode-map
("j" . pdf-view-next-line-or-next-page)
("k" . pdf-view-previous-line-or-previous-page))
:init (pdf-loader-install)
:config (add-to-list 'revert-without-query ".pdf"))

(setq treesit-language-source-alist
    '((bash "https://github.com/tree-sitter/tree-sitter-bash")
      (cmake "https://github.com/uyha/tree-sitter-cmake")
      (css "https://github.com/tree-sitter/tree-sitter-css")
      (elisp "https://github.com/Wilfred/tree-sitter-elisp")
      (go "https://github.com/tree-sitter/tree-sitter-go")
      (html "https://github.com/tree-sitter/tree-sitter-html")
      (javascript "https://github.com/tree-sitter/tree-sitter-javascript" "master" "src")
      (json "https://github.com/tree-sitter/tree-sitter-json")
      (make "https://github.com/alemuller/tree-sitter-make")
      (markdown "https://github.com/ikatyang/tree-sitter-markdown")
      (python "https://github.com/tree-sitter/tree-sitter-python")
      (toml "https://github.com/tree-sitter/tree-sitter-toml")
      (tsx "https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src")
      (typescript "https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src")
      (yaml "https://github.com/ikatyang/tree-sitter-yaml")))

 (setq major-mode-remap-alist
'((yaml-mode . yaml-ts-mode)
  (bash-mode . bash-ts-mode)
  (js2-mode . js-ts-mode)
  (typescript-mode . typescript-ts-mode)
  (json-mode . json-ts-mode)
  (css-mode . css-ts-mode)
  (python-mode . python-ts-mode)))

;; forces emacs to make vertical splits 
  (setq split-height-threshold nil)
    (setq split-width-threshold 0)

(setq custom-file "~/.config/emacs/customize-options.el")
(load custom-file)

(defun me/vertico-notes ()
              "list all note files"
              (interactive)
              (let* ((cands (split-string
                             (shell-command-to-string "find ~/notes -type f") "\n" t)))
                (find-file (completing-read "File: " cands))))

        (defun me/batch-open-rad-notes ()
        (mapc #'find-file-noselect
              (directory-files-recursively "~/notes/Radiology notes/" "")))


        (defun me/show-in-lf ()
        "Shows the current file in the lf file browser"
        (interactive)
        (shell-command (concat "lf -remote \"send select '" (buffer-file-name) "'\""))
        (start-process "showinlf" nil "~/.config/sway/scripts/togglefiles.sh" ""))

        (defun me/dired-open-file ()
        "In dired, open the file named on this line."
        (interactive)
        (let* ((file (dired-get-filename nil t)))
          (message "Opening %s..." file)
           (let ((filetype (mailcap-file-name-to-mime-type file)))
                    (if (or (string-equal filetype "application/vnd.lotus-organizer") (string-equal filetype "nil"))
                        (find-file file)
                        (browse-url-xdg-open file)))
          (message "Opening %s done" file)))

      (add-hook 'dired-mode-hook
                (lambda () (local-set-key (kbd "C-<return>") #'me/dired-open-file)))


    (defun me/open-anything ()
              "list everything recursively"
              (interactive)
              (let* ((cands (split-string
                             (shell-command-to-string "~/scripts/system/findallfiles.sh") "\n" t)))
                (let* ((file (completing-read "File: " cands)))
                  (let ((filetype (mailcap-file-name-to-mime-type file)))
                    (if (or (string-equal filetype "application/vnd.lotus-organizer") (string-equal filetype "nil"))
                        (find-file file)
                        (browse-url-xdg-open file))))))

  (defun me/kill-dired-buffers ()
       (interactive)
       (mapc (lambda (buffer) 
             (when (eq 'dired-mode (buffer-local-value 'major-mode buffer)) 
               (kill-buffer buffer))) 
           (buffer-list)))

  (defun me/switch-to-scratch-and-back ()
        "Toggle between *scratch* buffer and the current buffer.
         If the *scratch* buffer does not exist, create it."
        (interactive)
        (let ((scratch-buffer-name (get-buffer-create "*scratch*")))
            (if (equal (current-buffer) scratch-buffer-name)
                nil
                (switch-to-buffer scratch-buffer-name))))


(defun me/switch-to-quicknotes-and-back ()
        "Toggle between *scratch* buffer and the current buffer.
         If the *scratch* buffer does not exist, create it."
        (interactive)
        (if (equal (buffer-name) "quick_notes.org")
                (switch-to-buffer (other-buffer))
                (find-file "~/notes/quick_notes.org")))

(defun me/ff-link-org ()
    (interactive)
    (if (string-match system-name "laptop")
        (insert (shell-command-to-string "lz4jsoncat $HOME/.mozilla/firefox/jx17iz6w.default-release/sessionstore-backups/recovery.jsonlz4 | jq -r '.windows[0].tabs | sort_by(.lastAccessed)[-1] | .entries[.index-1] | \"[[\" + (.url) + \"][\" + (.title) + \"]]\"' | tr -d '\n'"))
        (insert (shell-command-to-string "lz4jsoncat $HOME/.mozilla/firefox/7ryvpua6.default-release/sessionstore-backups/recovery.jsonlz4 | jq -r '.windows[0].tabs | sort_by(.lastAccessed)[-1] | .entries[.index-1] | \"[[\" + (.url) + \"][\" + (.title) + \"]]\"' | tr -d '\n'"))
    ))

(defun me/copy-line ()
(interactive)
(save-excursion
(beginning-of-line)
(let ((beg (point)))
  (end-of-line)
  (copy-region-as-kill beg (point)))))

(load-file "~/.config/emacs/shortcuts.el")

(global-set-key (kbd "C-c m") 'imenu)
   (global-set-key (kbd "C-x C-b") 'ibuffer)
   (global-set-key (kbd "<C-M-left>") 'previous-buffer)
   (global-set-key (kbd "<C-M-right>") 'next-buffer)
   (global-set-key (kbd "M-n") 'forward-paragraph)
   (global-set-key (kbd "M-p") 'backward-paragraph)
   (global-set-key (kbd "<C-tab>") 'other-window)
   (global-set-key (kbd "<f5>") 'recentf) 
   (global-set-key (kbd "<f6>") 'bookmark-jump)
   (global-set-key (kbd "C-=") 'text-scale-increase)
   (global-set-key (kbd "C--") 'text-scale-decrease)
   (keymap-set           ctl-x-map "k" 'kill-current-buffer) ; Replace C-x k (kill buffer) with kill-current-buffer
   (keymap-set           ctl-x-map "f" 'find-file) ; Replace C-x f (set-fill-column) with find-file (C-x C-f usually)
   (keymap-set         ctl-x-r-map "d" 'bookmark-delete) ; Repace C-x r d (delete-rectangle) with delete bookmark

   (defun me/save-and-quit ()
     (interactive)
     (save-buffer)
     (kill-this-buffer))

   (global-set-key (kbd "C-q") 'me/save-and-quit)

   ;; Escape always quits
   (global-set-key [escape] 'keyboard-escape-quit)

   (defun me/toggle-windows ()
     (interactive)
     (if (> (count-windows) 1)
         (delete-other-windows)
       (progn (split-window-right)
              (other-window 1))))

   (add-hook 'ibuffer-mode-hook
             '(lambda ()
                (keymap-set ibuffer-mode-map "M-o" 'me/toggle-windows)))
   (global-set-key (kbd "M-o") 'me/toggle-windows) 

(load-file "~/.config/emacs/my-custom-keys.el")

(defhydra window-hydra
(:color blue)
"Adjust windows"
("h" windmove-left "Move left")
("j" windmove-down "Move down")
("k" windmove-up "Move up")
("l" windmove-right "Move right")
("d" delete-window "Close window")
("o" delete-other-windows "Delete other windows")
("s" split-window-right "Make vertical split"))

(defhydra org-mode-hydra
(:color amaranth)
"Select action"
("TAB" org-cycle "Org Cycle")
("j" org-next-visible-heading "Move down")
("k" org-previous-visible-heading "Move up")
("l" windmove-right "Move right")
("d" delete-window "Close window")
("s" (lambda () (interactive) (hydra-keyboard-quit) (org-insert-structure-template "src emacs-lisp")) "Structure template" :exit t)
("q" hydra-keyboard-quit "quit" :exit t))

(global-set-key (kbd "C-c n") #'me/vertico-notes)
(global-set-key (kbd "C-c olf") #'me/show-in-lf)
(global-set-key (kbd "C-c oa") #'me/open-anything)
(global-set-key (kbd "C-c b") #'me/switch-to-scratch-and-back)
(global-set-key (kbd "<f7>") #'me/switch-to-scratch-and-back)
(global-set-key (kbd "C-c qn") #'me/switch-to-quicknotes-and-back)
(global-set-key (kbd "<f8>") #'me/switch-to-quicknotes-and-back)
(global-set-key (kbd "C-c dd") #'me/kill-dired-buffers)
(global-set-key (kbd "C-c il") #'me/ff-link-org)
(global-set-key (kbd "C-c cl") #'me/copy-line)
(global-set-key (kbd "C-c cr") #'copy-region-as-kill)

(put 'erase-buffer 'disabled nil) ; what does this do?
(put 'dired-find-alternate-file 'disabled nil)

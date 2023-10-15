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
  :init (load-theme 'modus-vivendi-tinted t))

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
  (:map dired-mode-map ("." . dired-hide-dotfiles-mode)))

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
  :init
  ;; set prefix for lsp-command-keymap (few alternatives - "C-l", "C-c l")
  (setq lsp-keymap-prefix "C-c l")
  :hook (;; replace XXX-mode with concrete major-mode(e. g. python-mode)
         (python-ts-mode . lsp)
         (bash-ts-mode . lsp)
         (lua-mode . lsp)
         ;; if you want which-key integration
         (lsp-mode . lsp-enable-which-key-integration))
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

(use-package lua-mode
  :ensure nil
  :mode "\\.lua\\'"
  :hook (lua-mode . lsp))

(use-package corfu
  ;; Optional customizations
  :custom
  (corfu-cycle t)                 ; Allows cycling through candidates
  (corfu-auto t)                  ; Enable auto completion
  (corfu-auto-prefix 2)
  (corfu-auto-delay 0.0)
  (corfu-popupinfo-delay '(0.5 . 0.2))
  (corfu-preview-current 'insert) ; Do not preview current candidate
  (corfu-preselect-first nil)
  (corfu-on-exact-match nil)      ; Don't auto expand tempel snippets

  ;; Optionally use TAB for cycling, default is `corfu-complete'.
  :bind (:map corfu-map
              ("M-SPC"      . corfu-insert-separator)
              ("TAB"        . corfu-next)
              ([tab]        . corfu-next)
              ("S-TAB"      . corfu-previous)
              ([backtab]    . corfu-previous)
              ("<escape>" . corfu-quit)
              ("RET"        . corfu-insert))

  :init
  (global-corfu-mode)
  (corfu-history-mode)
  (corfu-popupinfo-mode) ; Popup completion info
  :config
  (add-hook 'eshell-mode-hook
            (lambda () (setq-local corfu-quit-at-boundary t
                              corfu-quit-no-match t
                              corfu-auto nil)
              (corfu-mode))))

(use-package cape
  :defer 10
  :bind ("C-c f" . cape-file)
  :init
  ;; Add `completion-at-point-functions', used by `completion-at-point'.
  (defalias 'dabbrev-after-2 (cape-capf-prefix-length #'cape-dabbrev 2))
  (add-to-list 'completion-at-point-functions 'dabbrev-after-2 t)
  (cl-pushnew #'cape-file completion-at-point-functions)
  :config
  ;; Silence then pcomplete capf, no errors or messages!
  (advice-add 'pcomplete-completions-at-point :around #'cape-wrap-silent)

  ;; Ensure that pcomplete does not write to the buffer
  ;; and behaves as a pure `completion-at-point-function'.
  (advice-add 'pcomplete-completions-at-point :around #'cape-wrap-purify))

(setq vc-follow-symlinks t)

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
                (switch-to-buffer (other-buffer))
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

(global-set-key (kbd "C-c x") 'me/toggle-windows)
(add-hook 'ibuffer-mode-hook
       '(lambda ()
          (keymap-set ibuffer-mode-map "M-o" 'me/toggle-windows)))
(global-set-key (kbd "M-o") 'me/toggle-windows)

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

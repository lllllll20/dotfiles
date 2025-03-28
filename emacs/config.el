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
(set-fringe-mode 0)

(menu-bar-mode -1)
(setq-default cursor-type 'bar)

(add-to-list 'default-frame-alist '(fullscreen . maximized))


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
(set-face-attribute 'variable-pitch nil :font "FreeSerif" :height 160 :weight 'regular)

(set-face-attribute 'org-block nil :font "FreeSans" :height 160 :weight 'regular)

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

(setq org-M-RET-may-split-line '((default . nil)))
(setq org-insert-heading-respect-content t)

(setq org-capture-templates
      '(("n" "Quick Note" entry
         (file org-default-notes-file)
         "* %?\n %i" :empty-lines 1)
        ("r" "Recipe" entry
         (file+headline "~/notes/ideas.org" "Recipe ideas")
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

(setq org-return-follows-link t)

(use-package doom-themes)
(setq custom-safe-themes t)
(use-package modus-themes
  :init
  (setq modus-themes-org-blocks 'gray-background))
(load-file "~/.config/emacs/themefile.el")

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

(defun my-toggle-writing-mode ()
  "Toggle distraction-free writing mode.
  Enables or disables `olivetti-mode`, sets `olivetti-body-width` to 0.8,
  and hides/shows the mode line."
  (interactive)
  ;; Set the body width to 80% for olivetti mode
  (setq olivetti-body-width 0.8)


  
  ;; Toggle the mode line visibility
  (if (eq mode-line-format nil)
      (progn
        (setq mode-line-format (default-value 'mode-line-format))
        (force-mode-line-update))  ;; Restore the mode line
    (progn
      (setq mode-line-format nil)
      (force-mode-line-update)))  ;; Hide the mode line

  ;; Toggle olivetti-mode
  (olivetti-mode 'toggle))

   
    ;; Assign it to a keybinding for quick access
    (global-set-key (kbd "C-c w") #'my-toggle-writing-mode)

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

;;     (use-package dired-hide-dotfiles
;;                 :hook
;;                 (dired-mode . dired-hide-dotfiles-mode)
;;                 :bind
;;                 (:map dired-mode-map
;; ;;                      ("h" . dired-up-directory )
;;                       ("." . dired-hide-dotfiles-mode )))

;;               (add-hook 'dired-mode-hook 'dired-hide-details-mode)
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
              (add-hook 'dired-mode-hook 'my-test-keys-insert-mode-activate)

(use-package dired-subtree :ensure t
  :after dired
  :config
  (bind-key "<tab>" #'dired-subtree-toggle dired-mode-map)
  (bind-key "<backtab>" #'dired-subtree-cycle dired-mode-map))

(defun xah-open-in-external-app (&optional Fname)
  "Open the current file or dired marked files in external app.
When called in emacs lisp, if Fname is given, open that.

URL `http://xahlee.info/emacs/emacs/emacs_dired_open_file_in_ext_apps.html'
Version: 2019-11-04 2023-04-05 2023-06-26"
  (interactive)
  (let (xfileList xdoIt)
    (setq xfileList
          (if Fname
              (list Fname)
            (if (eq major-mode 'dired-mode)
                (dired-get-marked-files)
              (list buffer-file-name))))
    (setq xdoIt (if (<= (length xfileList) 10) t (y-or-n-p "Open more than 10 files? ")))
    (when xdoIt
      (cond
       ((eq system-type 'windows-nt)
        (let ((xoutBuf (get-buffer-create "*xah open in external app*"))
              (xcmdlist (list "PowerShell" "-Command" "Invoke-Item" "-LiteralPath")))
          (mapc
           (lambda (x)
             (message "%s" x)
             (apply 'start-process (append (list "xah open in external app" xoutBuf) xcmdlist (list (format "'%s'" (if (string-match "'" x) (replace-match "`'" t t x) x))) nil)))
           xfileList)
          ;; (switch-to-buffer-other-window xoutBuf)
          )
        ;; old code. calling shell. also have a bug if filename contain apostrophe
        ;; (mapc (lambda (xfpath) (shell-command (concat "PowerShell -Command \"Invoke-Item -LiteralPath\" " "'" (shell-quote-argument (expand-file-name xfpath)) "'"))) xfileList)
        )
       ((eq system-type 'darwin)
        (mapc (lambda (xfpath) (shell-command (concat "open " (shell-quote-argument xfpath)))) xfileList))
       ((eq system-type 'gnu/linux)
        (mapc (lambda (xfpath)
                (call-process shell-file-name nil 0 nil
                              shell-command-switch
                              (format "%s %s"
                                      "xdg-open"
                                      (shell-quote-argument xfpath))))
              xfileList))
       ((eq system-type 'berkeley-unix)
        (mapc (lambda (xfpath) (let ((process-connection-type nil)) (start-process "" nil "xdg-open" xfpath))) xfileList))))))


(defun my-l ()
  "..."
  (interactive)
  (let ((fname  (dired-get-filename)))
    (if (file-directory-p fname)
        (dired-find-alternate-file)
      (xah-open-in-external-app fname))))



(eval-after-load "dired" '(progn
                            (define-key dired-mode-map (kbd "<return>") 'my-l)
                            (define-key dired-mode-map (kbd "l") 'my-l)
                            (define-key dired-mode-map (kbd "j") 'dired-next-line)
                            (define-key dired-mode-map (kbd "k") 'dired-previous-line)
                            (define-key dired-mode-map (kbd "h") (lambda () (interactive) (find-alternate-file "..")))))

(defun get-full-path-of-file-at-point ()
  "Get the full path of the file at point in a dired buffer and yank it to the kill ring."
  (interactive)
  (if (eq major-mode 'dired-mode)
      (let* ((file (dired-get-file-for-visit))
             (dir (file-name-directory (dired-current-directory)))
             (full-path (expand-file-name file dir)))
        (kill-new full-path)
        (message "Full path yanked to kill ring: %s" full-path))
    (message "Not in a dired buffer")))

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
   (define-key corfu-map [escape] #'corfu-quit)

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

(use-package ranger
    :ensure t
    :config
    (ranger-override-dired-mode t)) ;; Optional, replaces dired with ranger


  (defun my-ranger-setup ()
    "Set cursor to block and switch to insert mode in ranger-mode."
    (when (eq major-mode 'ranger-mode)
      (setq cursor-type 'box)   ;; Set cursor to block
      (my-test-keys-insert-mode-init)))     ;; Switch to insert mode


  (defun my-ranger-key-setup ()
    "Custom ranger keybindings."
    (define-key ranger-mode-map (kbd "RET") 'ranger-open-in-external-app)  ;; Remap RET to external open function
    (define-key ranger-mode-map (kbd "g") 'my-bookmark-jump))

  (add-hook 'ranger-mode-hook #'my-ranger-key-setup)


  (defun my-bookmark-open-with-ranger (bookmark)
    "Open a bookmarked directory with ranger instead of dired."
    (interactive)
    (let ((file (bookmark-get-filename bookmark)))
      (if (and file (file-directory-p file))
          (ranger file)  ;; open with ranger if it's a directory
        (bookmark-jump bookmark))))  ;; fallback to the normal bookmark jump for files


  (defun my-bookmark-jump (bookmark)
    "Jump to a bookmark, using ranger for directories."
    (interactive
     (list (bookmark-completing-read "Jump to bookmark: ")))
    (my-bookmark-open-with-ranger bookmark))

(defun my-ranger-exit-command ()
  "The command to run when exiting ranger mode."
  (message "Exiting ranger mode!"))

(add-hook 'ranger-mode-hook
          (lambda ()
            (add-hook 'kill-buffer-hook 'my-test-keys-command-mode-init nil t)))

;; forces emacs to make vertical splits
  (setq split-height-threshold nil)
    (setq split-width-threshold 0)

(setq custom-file "~/.config/emacs/customize-options.el")
(load custom-file)

;Define a general key-map which can override major mode bindings

(defun my-test-keys-insert-mode-escape ()
  (interactive)
  (if (region-active-p)
      (deactivate-mark)
    (if (active-minibuffer-window)
        (abort-recursive-edit)
        (if (derived-mode-p 'ranger-mode)
          (ranger-close)
        (my-test-keys-command-mode-activate)))))


(defvar my-insertmode-keys-minor-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-h") 'backward-word)
    (define-key map (kbd "C-j") 'forward-paragraph)
    (define-key map (kbd "C-k") 'backward-paragraph)
    (define-key map (kbd "C-l") 'forward-word)
    (define-key map (kbd "M-h") 'backward-char)
    (define-key map (kbd "M-j") 'next-line)
    (define-key map (kbd "M-k") 'previous-line)
    (define-key map (kbd "M-l") 'forward-char)
    (define-key map (kbd "C-M-h") 'previous-buffer)
    (define-key map (kbd "C-M-j") 'end-of-buffer)
    (define-key map (kbd "C-M-k") 'beginning-of-buffer)
    (define-key map (kbd "C-M-l") 'next-buffer)
    (define-key map (kbd "<f1>-k") 'describe-key)
    (define-key map (kbd "<f1>-f") 'describe-function)
    (define-key map (kbd "<f1>-v") 'describe-variable)
    (define-key map (kbd "<f1>-m") 'describe-mode)
    (define-key map (kbd "M-SPC") 'rectangle-mark-mode)
    (define-key map (kbd "<escape>") 'my-test-keys-insert-mode-escape)
    map) 
  "my-insertmode-keys-minor-mode keymap.") 



;; create and enable the minor mode
(define-minor-mode my-insertmode-keys-minor-mode
  "A minor mode for more comfortable navigation."
  :init-value t
  :lighter " my-keys")

(my-insertmode-keys-minor-mode 1)

;; The following is necessary to insertmode major mode keybindings, which otherwise take precedence
(add-to-list 'emulation-mode-map-alists `(my-insertmode-keys-minor-mode . ,my-insertmode-keys-minor-mode-map)) 



                                        ; Define the modal key mode and keymap

(define-minor-mode my-test-keys-minor-mode
  "Minor mode to be able to move using hjkl"
  :lighter " my-test-modal-keys"
  :keymap '(([remap self-insert-command]  ignore)) ; The actual keymaps are defined later below
  )

(progn
  (defun my-test-keys-command-mode-escape ()
    (interactive)
    (when (region-active-p)
      (deactivate-mark))
    (when (active-minibuffer-window)
      (abort-recursive-edit)))

  (define-key my-test-keys-minor-mode-map (kbd "<escape>")     'my-test-keys-command-mode-escape))

;;(add-to-list 'emulation-mode-map-alists '(my-modal-keys-minor-mode . ,my-modal-keys-minor-mode-map))

(keymap-set my-test-keys-minor-mode-map "a" 'move-beginning-of-line)
(keymap-set my-test-keys-minor-mode-map "e" 'move-end-of-line)
(keymap-set my-test-keys-minor-mode-map "h" 'backward-char)
(keymap-set my-test-keys-minor-mode-map "i" 'my-test-keys-insert-mode-activate)
(keymap-set my-test-keys-minor-mode-map "j" 'next-line)
(keymap-set my-test-keys-minor-mode-map "k" 'previous-line)
(keymap-set my-test-keys-minor-mode-map "l" 'forward-char)
(keymap-set my-test-keys-minor-mode-map "o" 'org-mode-hydra/body)
(keymap-set my-test-keys-minor-mode-map "f" 'file-hydra/body)
(keymap-set my-test-keys-minor-mode-map "g" 'my-bookmark-jump)
(keymap-set my-test-keys-minor-mode-map "r" 'undo-redo)
(keymap-set my-test-keys-minor-mode-map "s" 'consult-line)
(keymap-set my-test-keys-minor-mode-map "/" 'consult-line)
(keymap-set my-test-keys-minor-mode-map "u" 'undo)
(keymap-set my-test-keys-minor-mode-map "w" 'window-hydra/body)
(keymap-set my-test-keys-minor-mode-map "x" 'execute-extended-command)
(keymap-set my-test-keys-minor-mode-map "y" 'yank)
(keymap-set my-test-keys-minor-mode-map "SPC" 'me/insert-space)
(keymap-set my-test-keys-minor-mode-map "," 'eval-last-sexp)
(define-key my-test-keys-minor-mode-map (kbd "<C-return>") 'er/expand-region)

(defun me/insert-space ()
  "Just pass through a space"
  (interactive)
  (self-insert-command 1 ?\s))


(defun me/cut-thing ()
  "Cut active region or offer choice"
  (interactive)
  (if (region-active-p)
      (kill-region (point) (mark))
    (cut-text-hydra/body)))

(defun me/delete-current-text-block ()
  "Cut the current text block plus blank lines, or selection, and copy to `kill-ring'.

If cursor is between blank lines, delete following blank lines.

URL `http://xahlee.info/emacs/emacs/emacs_delete_block.html'
Created: 2017-07-09
Version: 2023-10-09"
  (interactive)
  (let (xp1 xp2)
    (if (region-active-p)
        (setq xp1 (region-beginning) xp2 (region-end))
      (progn
        (if (re-search-backward "\n[ \t]*\n+" nil :move)
            (setq xp1 (goto-char (match-end 0)))
          (setq xp1 (point)))
        (if (re-search-forward "\n[ \t]*\n+" nil :move)
            (setq xp2 (match-end 0))
          (setq xp2 (point-max)))))
    (kill-region xp1 xp2)))

(defhydra cut-text-hydra
  (:color blue)
  "select region of text to copy"
  ("w" kill-word "Cut to end of word")      
  ("e" kill-line "Cut to end of line")      
  ("p" me/delete-current-text-block "Cut block")      
  ("d" kill-whole-line "Cut whole line"))

(keymap-set my-test-keys-minor-mode-map "d" 'me/cut-thing)

(defun me/copy-current-text-block ()
  "Copy the current text block without surrounding blank lines to `kill-ring`.
If cursor is between blank lines, copy the following text block."
  (interactive)
  (let (xp1 xp2)
    (save-excursion
      (if (region-active-p)
          (setq xp1 (region-beginning) xp2 (region-end))
        (progn
          (if (re-search-backward "\n[ \t]*\n" nil :move)
              (setq xp1 (goto-char (match-end 0)))
            (setq xp1 (point-min)))
          (if (re-search-forward "\n[ \t]*\n" nil :move)
              (setq xp2 (match-beginning 0))
            (setq xp2 (point-max)))))
      ;; Move the start and end points to skip over any leading/trailing whitespace
      (goto-char xp1)
      (skip-chars-forward " \t\n")
      (setq xp1 (point))
      (goto-char xp2)
      (skip-chars-backward " \t\n")
      (setq xp2 (point)))
    (kill-ring-save xp1 xp2)
    (message "Text block copied to kill-ring.")))

(defun me/copy-thing ()
  "Copy active region or offer choice"
  (interactive)
  (if (region-active-p)
      (kill-ring-save (point) (mark))
    (copy-text-hydra/body)))

(defun me/copy-line ()
  "Copy the current line, or lines if a region is active, to the `kill-ring'."
  (interactive)
  (let (start end)
    (if (use-region-p)
        (setq start (region-beginning) end (region-end))
      (setq start (line-beginning-position)
            end (line-beginning-position 2)))
    (kill-ring-save start end)
    (message "Line copied to kill-ring.")))


(defun me/copy-word ()
  "Copy the word at point, including hyphenated words, to the `kill-ring'."
  (interactive)
  (let (start end)
    (save-excursion
      ;; Move to the beginning of the word or hyphenated word
      (skip-syntax-backward "w_")
      (while (looking-back "-")
        (skip-syntax-backward "w_"))
      (setq start (point))
      ;; Move to the end of the word or hyphenated word
      (skip-syntax-forward "w_")
      (while (looking-at "-")
        (skip-forward "w_"))
      (setq end (point)))
    (kill-ring-save start end)
    (message "Word copied to kill-ring.")))


(defun me/copy-sentence ()
  "Copy the sentence at point to the `kill-ring'."
  (interactive)
  (let (start end)
    (save-excursion
      ;; Move to the beginning of the sentence
      (backward-sentence)
      (setq start (point))
      ;; Move to the end of the sentence
      (forward-sentence)
      (setq end (point)))
    (kill-ring-save start end)
    (message "Sentence copied to kill-ring.")))

(defun me/copy-whole-buffer ()
  "Copy the entire buffer to the clipboard."
  (interactive)
  (save-excursion
    (mark-whole-buffer)
    (kill-ring-save (point-min) (point-max))))


(defhydra copy-text-hydra
  (:color blue)
  "select region of text to copy"
  ("w" me/copy-word "Cut to end of word")      
  ("c" me/copy-line "Copy line")
  ("l" me/copy-line "Copy line")
  ("a" me/copy-whole-buffer "Copy whole buffer")      
  ("p" me/copy-current-text-block "Copy paragraph")      
  ("s" me/copy-sentence "Copy paragraph")      
  ("d" kill-whole-line "Cut whole line"))

(keymap-set my-test-keys-minor-mode-map "c" 'me/copy-thing)


(defhydra search-hydra
  (:color blue)
  "Select type of search"
  ("s" consult-line "Consult-Line")      
  ("r" query-replace "Query-Replace"))

(defun me/kill-all-dired-buffers ()
  "Kill all Dired buffers."
  (interactive)
  (let ((count 0))
    (dolist (buffer (buffer-list))
      (when (eq (buffer-local-value 'major-mode buffer) 'dired-mode)
        (kill-buffer buffer)
        (setq count (1+ count))))
    (message "Killed %d Dired buffer(s)" count)))

(defhydra mode-hydra
  (:color blue)
  "Miscellaneous functions"
  ("s" search-hydra/body "Search and replace operations")      
  ("e" kill-line "Cut to end of line")
  ("r" ranger "Ranger mode")
  ("b" me/delete-current-text-block "Cut block")      
  ("d" me/kill-all-dired-buffers "Kill all dired buffers"))

(keymap-set my-test-keys-minor-mode-map "m" 'mode-hydra/body)


(defhydra set-mark-hydra
  (:color blue)
  "select region of text to copy"
  ("e" er/expand-region "Expand region")      
  ("h" set-mark-command "Mark by line")
  ("j" set-mark-command "Mark by line")
  ("k" set-mark-command "Mark by line")
  ("l" set-mark-command "Mark by line")      
  ("r" rectangle-mark-mode "Mark rectangle")
  ("a" mark-whole-buffer "Mark whole buffer")
  ("p" mark-paragraph "Mark paragraph"))

(defun my-set-mark-wrapper ()
  "Set the mark or toggle position if region active"
  (interactive)
  (if (region-active-p) (exchange-point-and-mark)
    (set-mark-hydra/body)))

(keymap-set my-test-keys-minor-mode-map "v" 'my-set-mark-wrapper)


(defun my-next-buffer ()
  "Move to next buffer.
      Press l will do it again, press h will move to previous buffer. Press other key to exit."
  (interactive)
  (let ((skip-buffers '("*Messages*" "*Scratch*" "*Async-native-compile-log*")))
    (next-buffer)
    (while (member (buffer-name) skip-buffers) (next-buffer)))
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "l") 'my-next-buffer)
    (define-key map (kbd "h") 'my-previous-buffer)
    (set-transient-map map t)))

(defun my-previous-buffer ()
  "move cursor to previous buffer.
   Press h will do it again, press l will move to next buffer. Press other key to exit."
  (interactive)
  (let ((skip-buffers '("*Messages*" "*Scratch*" "*Async-native-compile-log*")))
    (next-buffer)
    (while (member (buffer-name) skip-buffers) (next-buffer)))
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "l") 'my-next-buffer)
    (define-key map (kbd "h") 'my-previous-buffer)
    (set-transient-map map t)))

(defun me/find-org-files-in-my-documents ()
  "Use `find-dired` to identify .org files in ~/my_docs/ and display the results in a dired buffer."
  (interactive)
  (find-lisp-find-dired "~/my_docs/" "\\.org$"))


(defun me/find-org-files-in-work-documents ()
  "Use `find-dired` to identify .org files in ~/work_docs/ and display the results in a dired buffer."
  (interactive)
  (find-lisp-find-dired "~/work_docs/" "\\.org$"))



(defhydra select-buffer-or-file-hydra
  (:color blue)
  "Open Buffer"
  ("d" (progn (dired "~/") (my-test-keys-insert-mode-activate)) "Open dired")      
  ("r" recentf "Recent file")      
  ("j" switch-to-buffer "List buffers")      
  ("s" scratch-buffer "Show scratch buffer")      
  ("k" kill-current-buffer "Kill current buffer")      
  ("h" my-previous-buffer "Previous buffer")      
  ("l" my-next-buffer "Next buffer")      
  ("m" me/find-org-files-in-my-documents "My Org docs")      
  ("w" me/find-org-files-in-work-documents "My Org docs")      
  ("e" (find-file "~/notes/Computing/Programs/emacs.org") "Emacs")
  ("t" (find-file "~/notes/todo.org") "Todo")      
  ("i" (find-file "~/notes/ideas.org") "Ideas")      
  ("q" (find-file "~/notes/quick_notes.org") "Quick notes")      
  ("n" me/vertico-notes "Select notes")      
  ("b" bookmark-jump "Select bookmarked file")) 

(keymap-set my-test-keys-minor-mode-map "b" 'select-buffer-or-file-hydra/body)

(defvar my-test-keys-command-mode--deactivate-func nil)
(defvar my-insert-state-p t)

(defvar my-mode-line-indicator " COMMAND"
  "Indicator for the current mode (insert or command) in the mode line.")

(defun update-mode-line-indicator ()
  "Update the mode line indicator based on the current state."
  (setq my-mode-line-indicator
        (if my-insert-state-p " INSERT" " COMMAND"))
  (force-mode-line-update))


(defun my-test-keys-command-mode-init ()
  (interactive)
  (setq my-insert-state-p nil)
  (when my-test-keys-command-mode--deactivate-func
    (funcall my-test-keys-command-mode--deactivate-func))
  (setq my-test-keys-command-mode--deactivate-func
        (set-transient-map my-test-keys-minor-mode-map (lambda () t)))
  (update-mode-line-faces)
  (update-mode-line-indicator)
  (setq cursor-type 'box))

(defun my-test-keys-insert-mode-init ()
  (interactive)
  (setq my-insert-state-p t)
  (when my-test-keys-command-mode--deactivate-func
    (funcall my-test-keys-command-mode--deactivate-func))
  (update-mode-line-faces)
  (update-mode-line-indicator)
  (setq cursor-type 'bar))

;; Define custom faces for insert and command mode
(defface my-insert-mode-face
  '((t (:foreground "#ffffff" :background "#484d67" :box "#979797"))) ; Modus Vivendi Tinted insert mode color
  "Face for insert mode in the mode line.")

(defface my-command-mode-face
  '((t (:foreground "#ffffff" :background "#a78cfa" :box "#979797"))) ; Modus Vivendi Tinted command mode color
  "Face for command mode in the mode line.")

;; Function to update modeline face based on current mode
(defun update-mode-line-faces ()
  "Update modeline face based on current mode."
  (if my-insert-state-p
      (set-face-attribute 'mode-line nil :background "#484d67" :foreground "#ffffff" :box "#979797")
    (set-face-attribute 'mode-line nil :background "#a78cfa" :foreground "#ffffff" :box "#979797")))

;; Hook to update modeline faces whenever mode changes
;;(add-hook 'post-command-hook 'update-mode-line-faces)

;; Append the indicator to the global mode string
(add-to-list 'global-mode-string '(:eval my-mode-line-indicator) t)

;; Initial update
(update-mode-line-indicator)



;;; (funcall my-test-keys-command-mode--deactivate-func) This is all thats needed to deactivate command mode

(defun my-test-keys-insert-mode-activate ()
  "Activate insertion mode."
  (interactive)
  (my-test-keys-insert-mode-init)
                                        ;(run-hooks 'xah-fly-insert-mode-activate-hook)
  )

(defun my-test-keys-command-mode-activate ()
  "Activate commandion mode."
  (interactive)
  (my-test-keys-command-mode-init)
                                        ;(run-hooks 'xah-fly-command-mode-activate-hook)
  )

(defun my-test-keys-mode-toggle ()
  (interactive)
  (if my-insert-state-p
      (my-test-keys-command-mode-activate)
    (my-test-keys-insert-mode-activate)))

(add-hook 'minibuffer-setup-hook 'my-minibuffer-entry-insert-setup)

(defvar my-command-history-p nil)

(defun my-minibuffer-entry-insert-setup ()
  (if my-insert-state-p nil
    (progn
      (setq my-command-history-p t)
      (my-test-keys-insert-mode-activate)
      )))

(defun my-minibuffer-exit-setup ()

  (if my-command-history-p
      (progn
        (setq my-command-history-p nil)
        (my-test-keys-command-mode-activate)
        )))

(add-hook 'minibuffer-exit-hook 'my-minibuffer-exit-setup)

(add-hook 'buffer-list-update-hook 'my-cursor-hack-function)


(defun my-cursor-hack-function ()		 
  "Function to run after buffer list update." 
  (if (eq major-mode 'dired-mode)
      (progn
        (my-test-keys-insert-mode-init)
        (setq cursor-type 'box))
    ;;	(setq my-insert-state-p nil))
    (if my-insert-state-p			 
        (my-test-keys-insert-mode-init)	 
      (my-test-keys-command-mode-init))))

;;(add-hook 'dired-mode-hook 'my-test-keys-insert-mode-activate)
(advice-add 'quit-window :after 'my-test-keys-command-mode-activate)

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
  (let ((file (if (eq major-mode 'dired-mode)
                  (expand-file-name (dired-get-file-for-visit) (file-name-directory (dired-current-directory)))
                (buffer-file-name))))
    (shell-command (concat "lf -remote \"send select '" file "'\"")))
  (start-process "showinlf" nil "~/.config/sway/scripts/togglefiles.sh" ""))

(defun me/dired-open-file ()
  "In dired, open the file named on this line."
  (interactive)
  (let* ((file (dired-get-filename nil t)))
    (message "Opening %s..." file)
    (let ((filetype (mailcap-file-name-to-mime-type file)))
      (if (or (string-equal filetype "application/vnd.lotus-organizer") (string-equal filetype "nil") (string-equal filetype "text/plain"))
          (find-file file)
        (browse-url-xdg-open file)))
    (message "Opening %s done" file)))

(add-hook 'dired-mode-hook
          (lambda () (local-set-key (kbd "C-<return>") #'me/dired-open-file)))


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

(defun me/select-theme ()
  "Change theme interactively."
  (interactive)
  (let* ((cands (custom-available-themes))
         (theme (completing-read "Theme: " cands)))

    (with-temp-buffer
      (insert (format "(load-theme '%s t)\n" theme))
      (write-region (point-min) (point-max) "~/.config/emacs/themefile.el"))

    ;; Load and enable the selected theme
    (load-theme (intern theme) t)))

(load-file "~/.config/emacs/shortcuts.el")

(eval-after-load "dired" (progn
  '(define-key dired-mode-map "p" 'get-full-path-of-file-at-point)
  '(define-key dired-mode-map "z" 'me/show-in-lf)))

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
   (global-set-key (kbd "M-<drag-mouse-9>") 'next-buffer)
   (global-set-key (kbd "M-<drag-mouse-8>") 'previous-buffer)
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

;;   (load-file "~/.config/emacs/my-custom-keys.el")


   (defun my/insert-newline-below ()
     "Insert a newline below the current line and move the cursor to it."
     (interactive)
     (end-of-line)   ; Move to the end of the current line
     (newline-and-indent)) ; Insert a newline and move the cursor to it

   (global-set-key (kbd "C-<return>") 'my/insert-newline-below)


   (defun my/insert-newline-above-no-move ()
     "Insert a newline above the current line without moving the cursor."
     (interactive)
     (save-excursion              ; Save the current cursor position
       (beginning-of-line)        ; Move to the beginning of the current line
       (newline)                  ; Insert a newline
       (previous-line)            ; Move the cursor to the new line
       (indent-according-to-mode))) ; Indent the new line according to the mode

   (global-set-key (kbd "C-p") 'my/insert-newline-above-no-move)

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

(defun my-forward-heading ()
      "move cursor to next heading.
   Press j will do it again, press k will move to previous heading. Press other key to exit."
     (interactive)
     (outline-next-heading)
     (let ((map (make-sparse-keymap)))
        (define-key map (kbd "TAB") 'org-cycle)
        (define-key map (kbd "j") 'outline-next-heading)
        (define-key map (kbd "k") 'outline-previous-heading)
        (set-transient-map map t)))

   (defun my-previous-heading ()
     "move cursor to previous heading.
Press k will do it again, press j will move to next heading. Press other key to exit."
     (interactive)
     (outline-previous-heading)
     (let ((map (make-sparse-keymap)))
        (define-key map (kbd "TAB") 'org-cycle)
        (define-key map (kbd "j") 'outline-next-heading)
        (define-key map (kbd "k") 'outline-previous-heading)
        (set-transient-map map t)))

(defhydra org-mode-hydra
      (:color blue)
      "Select action"
      ("TAB" org-cycle "Org Cycle")
      ("c" org-capture "Capture")
      ("f" me/ff-link-org "Insert firefox link")
      ("j" my-forward-heading "Move down")
      ("k" my-previous-heading "Move up")
      ("l" org-insert-link "Insert link")
      ("m" consult-imenu "Search by heading")
      ("o" my-toggle-writing-mode "Toggle Olivetti mode")
      ("s" (lambda () (interactive) (hydra-keyboard-quit) (org-insert-structure-template "src emacs-lisp")) "Structure template" :exit t)
      ("t" me/insert-date-stamp "Timestamp")
      ("q" hydra-keyboard-quit "quit" :exit t))

(defhydra file-hydra
  (:color blue)
  "Select action"
  ("d" dired "Open dired")
  ("f" find-file "Find file")
  ("n" write-file "Save as")
  ("p" ffap "Find file at point")
  ("s" save-buffer "Save buffer")
  ("q" hydra-keyboard-quit "quit" :exit t))

(global-set-key (kbd "C-c n") #'me/vertico-notes)
(global-set-key (kbd "C-c olf") #'me/show-in-lf)
(global-set-key (kbd "C-c il") #'me/ff-link-org)

(put 'erase-buffer 'disabled nil) ; what does this do?
(put 'dired-find-alternate-file 'disabled nil)

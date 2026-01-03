;; (require 'server)
;; (unless (server-running-p)
;;   (server-start))

(require 'package)
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("org" . "https://orgmode.org/elpa/")
                         ("elpa" . "https://elpa.gnu.org/packages/")))
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(require 'use-package)
(setq use-package-always-ensure t)

;; --- BASIC UI SETTINGS ---
(use-package emacs
  :init
  (setq inhibit-startup-message t)
  (scroll-bar-mode -1)
  (tool-bar-mode -1)
  (tooltip-mode -1)
  (menu-bar-mode -1)
  (column-number-mode t)
  :config
  ;; Standard UI settings
  (add-to-list 'default-frame-alist '(fullscreen . maximized))
  
  ;; Platform-specific Font Settings
  (if (eq system-type 'android)
      (progn
        (set-face-attribute 'default nil :font "Monospace" :height 220)
        (set-face-attribute 'variable-pitch nil :font "Sans Serif" :height 240))
    (progn
      (set-face-attribute 'default nil :font "FreeSans" :height 140)
      (set-face-attribute 'variable-pitch nil :font "FreeSerif" :height 160))))

;; Consult

(use-package consult
  :ensure t)


;; Vertico

(use-package vertico
  :ensure t
  :init
  (vertico-mode 1)
  :config
  ;; Optional: Hide commands that do not apply to the current mode
  (setq read-extended-command-predicate
        #'command-completion-default-include-p))


;; Marginalia

(use-package marginalia
  :ensure t
  :init
  (marginalia-mode 1))

;; Orderless

(use-package orderless
  :ensure t
  :custom
  ;; Configure orderless as the primary completion style
  (completion-styles '(orderless basic))
  ;; Optional: allow literal matching for files (better for path completion)
  (completion-category-overrides '((file (styles basic partial-completion)))))

;; Modus themes

;; --- THEME SETTINGS ---
(use-package modus-themes
  :ensure t
  :config
  ;; 1. Set specific aesthetic options for the tinted variant
  (setq modus-themes-italic-constructs t
        modus-themes-bold-constructs t
        modus-themes-mixed-fonts t)

  ;; 2. Load the theme immediately
  (load-theme 'modus-vivendi-tinted t))

;; 3. Global Toggle on F5 (in case you want to swap to light mode later)
(setq modus-themes-to-toggle '(modus-operandi modus-vivendi-tinted))
(global-set-key (kbd "<f5>") #'modus-themes-toggle)

;; hydra code

(use-package hydra :ensure t)


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



(defun my-copy-wrapper ()
  "Copy active region or offer choice"
  (interactive)
  (if (region-active-p)
      (kill-ring-save (point) (mark))
    (hydra-copy/body)))

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

(defhydra hydra-copy
  (:color blue)
  "select region of text to copy"
  ("w" me/copy-word "Cut to end of word")      
  ("c" me/copy-line "Copy line")
  ("l" me/copy-line "Copy line")
  ("a" me/copy-whole-buffer "Copy whole buffer")      
  ("p" me/copy-current-text-block "Copy paragraph")      
  ("s" me/copy-sentence "Copy sentence")      
  ("d" kill-whole-line "Cut whole line"))



(defun me/delete-sentence ()
  "Delete text from the previous sentence-ending punctuation to the next one, including both."
  (interactive)
  (let (start end)
    (save-excursion
      ;; Search backward for sentence-ending punctuation
      (if (re-search-backward "[.!?]" nil t)
          (forward-char) ;; move past the punctuation
        (beginning-of-buffer))
      (skip-chars-forward " \n\t") ;; skip whitespace
      (setq start (point))
      ;; Now search forward for next sentence-ending punctuation
      (when (re-search-forward "[.!?]" nil t)
        (forward-char) ;; include the punctuation
        (setq end (point))))
    ;; Kill the region if both points are valid
    (when (and start end)
      (kill-region start end))))

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
  ("s" me/delete-sentence "Delete sentence")
  ("f" me/zap-to-stop "Delete end of sentence")
  ("d" kill-whole-line "Cut whole line"))


(defhydra hydra-set-mark
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
  "Set the mark or toggle position if region active. 
If no region, launch the selection hydra."
  (interactive)
  (if (region-active-p) 
      (exchange-point-and-mark)
    (hydra-set-mark/body)))


(defhydra hydra-buffer (:color blue)
  "Act on the buffer"
  ("k" kill-current-buffer "Kill current buffer")
  ("s" (switch-to-buffer "*scratch*") "Scratch buffer")
  ("q" nil))

(defhydra hydra-save-write (:color blue)
  "
Save/Write Tasks:
_w_: Save Buffer        _r_: Rename File/Buffer
_a_: Save As (Write)    _q_: Cancel
"
  ("w" save-buffer)
  ("a" write-file)
  ("r" rename-visited-file)
  ("q" nil "quit"))


(defun my-find-org-files-in-my-documents ()
  "Use `find-dired` to identify .org files in ~/my_docs/ and display the results in a dired buffer."
  (interactive)
  (find-lisp-find-dired "~/my_docs/" "\\.org$"))


(defun my-find-org-files-in-work-documents ()
  "Use `find-dired` to identify .org files in ~/work_docs/ and display the results in a dired buffer."
  (interactive)
  (find-lisp-find-dired "~/work_docs/" "\\.org$"))



(defhydra hydra-opener (:color blue)
  "
Open:
_n_: Notes Selector    _m_: Personal Notes
_w_: Work Notes        _q_: Cancel
"
  ("n" (my-open-notes) "Opens notes with vertico") ; Replace with your function later
  ("w" my-find-org-files-in-work-documents "Work documents")     ; Replace with your function later
  ("m" my-find-org-files-in-my-documents "My documents") ; Replace with your function later
  ("q" nil "quit"))


;; --- SETUP FUNCTIONS AND VARIABLES ---


(defvar my-fm-show-hidden nil
  "Non-nil if my-fm should show hidden files.")

(defun my-fm-toggle-hidden ()
  "Toggle the display of hidden files in the file manager."
  (interactive)
  (setq my-fm-show-hidden (not my-fm-show-hidden))
  (my-fm-draw my-fm-current-dir)
  (message "Show hidden files: %s" (if my-fm-show-hidden "On" "Off")))


(defun my-fm-toggle-storage ()
  "Switch between Android shared storage and Emacs internal home."
  (interactive)
  (if (eq system-type 'android)
      (let ((shared "/storage/emulated/0/")
            (internal (expand-file-name "~/")))
        (if (string-prefix-p shared (expand-file-name my-fm-current-dir))
            (my-fm-draw internal)
          (my-fm-draw shared)))
    (message "Not on Android: Standard home used.")))

(defun my-fm-open-in-nautilus ()
  "Open the current file's directory or the directory at point in Nautilus."
  (interactive)
  (let ((path (get-text-property (point) 'file-path)))
    (if (and path (eq system-type 'gnu/linux))
        (progn
          (message "Opening in Nautilus...")
          (call-process "nautilus" nil 0 nil path))
      (message "Nautilus is only available on GNU/Linux."))))

;; More functions

(defun my-toggle-previous-buffer ()
  "Switch to the previously used buffer, skipping system buffers but allowing *scratch*."
  (interactive)
  (let ((current (current-buffer))
        (target nil))
    ;; Iterate through the buffer list (ordered by most recent use)
    (dolist (buf (buffer-list))
      (let ((name (buffer-name buf)))
        ;; Only set target if we haven't found one yet
        (when (and (not target)
                   (not (eq buf current))
                   (buffer-live-p buf) ; Ensure buffer is still active
                   (or (not (string-prefix-p "*" name))
                       (string-equal name "*scratch*"))
                   (not (string-prefix-p " " name)))
          (setq target buf))))
    ;; Perform the switch or show the message only if no target was found
    (if target
        (switch-to-buffer target)
      (message "No previous valid buffer found."))))

(defun my-open-notes ()
  "list all note files"
  (interactive)
  (let* ((cands (split-string
                 (shell-command-to-string "find ~/notes -type f") "\n" t)))
    (find-file (completing-read "File: " cands))))

(defun my-toggle-night-mode ()
  "Toggle between Modus Operandi (light) and Modus Vivendi (dark)."
  (interactive)
  (if (member 'modus-operandi custom-enabled-themes)
      (progn
        (disable-theme 'modus-operandi)
        (load-theme 'modus-vivendi t)
        (message "Night Mode Active (Modus Vivendi)"))
    (progn
      (disable-theme 'modus-vivendi)
      (load-theme 'modus-operandi t)
      (message "Day Mode Active (Modus Operandi)"))))



;; --- CUSTOM FILE MANAGER (THE BASE) ---

(defvar my-fm-current-dir nil)

(defun my-fm-draw (path)
  "Render the file list for PATH, respecting `my-fm-show-hidden`."
  (let ((buf (get-buffer-create "*FileManager*")))
    (setq my-fm-current-dir (expand-file-name path))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (my-fm-mode)
        (my-command-mode -1)
        ;; Get all files, then filter them manually based on the toggle
        (dolist (file (directory-files my-fm-current-dir))
          (unless (and (not my-fm-show-hidden)
                       (string-prefix-p "." file))
            (let ((full-path (expand-file-name file my-fm-current-dir)))
              (insert (propertize (concat file "\n") 'file-path full-path)))))
        (goto-char (point-min)))
      (switch-to-buffer buf))))


(defun my-fm-select ()
  "Select the file under point. If directory, navigate; otherwise handle externally or in Emacs."
  (interactive)
  (let ((path (get-text-property (point) 'file-path)))
    (when path
      (cond
       ;; 1. Navigate if it's a directory
       ((file-directory-p path)
        (my-fm-draw path))
       
       ;; 2. Handle PDFs specifically on Linux (Force Evince)
       ((and (eq system-type 'gnu/linux) 
             (string-match-p "\\.pdf\\'" path))
        (call-process "evince" nil 0 nil path))

       ;; 3. Handle other media types externally
       ((string-match-p "\\.\\(png\\|jpg\\|jpeg\\|mp4\\)\\'" path)
        (cond
         ((eq system-type 'android)
          (android-file-external-opener path))
         ((eq system-type 'gnu/linux)
          (call-process "xdg-open" nil 0 nil path))
         (t (find-file path))))
       
       ;; 4. Fallback to standard Emacs opening
       (t (find-file path))))))




(defun my-fm-up ()
  "Move up one directory."
  (interactive)
  (my-fm-draw (file-name-directory (directory-file-name my-fm-current-dir))))



(defvar my-fm-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "j") 'next-line)
    (define-key map (kbd "k") 'previous-line)
    (define-key map (kbd "l") 'my-fm-select)
    (define-key map (kbd "h") 'my-fm-up)
    (define-key map (kbd "s") 'my-fm-toggle-storage)
    (define-key map (kbd ".") 'my-fm-toggle-hidden)
    (define-key map (kbd "o") 'my-fm-open-in-nautilus)
    (define-key map (kbd "g") (lambda () (interactive) (my-fm-draw my-fm-current-dir)))
    (define-key map (kbd "q") 'quit-window)
    map)
  "Keymap for my-fm-mode.")

(define-derived-mode my-fm-mode special-mode "FileManager"
  "A custom minimalist file manager mode."
  (setq buffer-read-only t)
  (hl-line-mode 1))


;; --- TWO-KEY JUMP SYSTEM ---
(defvar my-fm-jump-map (make-sparse-keymap))
(define-key my-fm-jump-map (kbd "h") (lambda () (interactive) (my-fm-draw "~/")))
(define-key my-fm-jump-map (kbd "w") (lambda () (interactive) (my-fm-draw "~/work_docs/")))


(defvar my-bm-mode-map
  (let ((map (make-sparse-keymap)))
    ;; Navigation
    (define-key map (kbd "j") 'next-line)
    (define-key map (kbd "d") 'my-bm-kill)   ; K kills the buffer
    (define-key map (kbd "k") 'previous-line)

    ;; Actions
    (define-key map (kbd "RET") 'my-bm-select)
    (define-key map (kbd "l") 'my-bm-select)
    (define-key map (kbd "g") 'my-bm-draw)
    (define-key map (kbd "SPC") 'my-bm-toggle)
    (define-key map (kbd "q") 'quit-window)
    map))

(define-derived-mode my-bm-mode special-mode "BufferManager"
  "A custom minimalist buffer manager mode."
  (setq buffer-read-only t)
  (hl-line-mode 1))

;; --- CUSTOM BUFFER MANAGER ---
;; --- CUSTOM BUFFER MANAGER WITH HEADINGS ---


(define-derived-mode my-bm-mode special-mode "BufferManager"
  "A custom minimalist buffer manager mode."
  :keymap my-bm-mode-map
  (setq buffer-read-only t)
  (hl-line-mode 1))


(defun my-bm-draw ()
  "Render buffers and ensure Command Mode is OFF in this buffer."
  (interactive)
  (let ((buf (get-buffer-create "*BufferManager*"))
        (user-bufs '())
        (sys-bufs '()))
    (dolist (b (buffer-list))
      (let ((name (buffer-name b)))
        (cond
         ((string-match-p "\\(Echo Area\\|Minibuffer\\)" name) nil)
         ((string-equal name "*scratch*") (push b user-bufs))
         ((or (string-prefix-p " " name) (string-prefix-p "*" name)) (push b sys-bufs))
         (t (push b user-bufs)))))

    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (my-bm-mode) ; Set the major mode
        (my-command-mode -1) ; FORCE command mode off here
        
        (insert (propertize "--- USER BUFFERS ---\n" 'face 'bold))
        (dolist (b (reverse user-bufs))
          (let ((start (point)))
            (insert "  " (buffer-name b) "\n")
            (add-text-properties start (point) (list 'buffer-target b))))
        
        (insert (propertize "\n--- SYSTEM BUFFERS ---\n" 'face 'shadow))
        (dolist (b (reverse sys-bufs))
          (let ((start (point)))
            (insert "  " (buffer-name b) "\n")
            (add-text-properties start (point) (list 'buffer-target b))))
        
        (goto-char (point-min))
        (forward-line 1)))
    (switch-to-buffer buf)))

(defun my-bm-select ()
  "Switch to the buffer and re-enable command mode."
  (interactive)
  (let ((target (get-char-property (point) 'buffer-target)))
    (unless target
      (setq target (get-char-property (line-beginning-position) 'buffer-target)))
    (if (buffer-live-p target)
        (progn
          (switch-to-buffer target)
          (my-command-mode 1)) ; Re-enable your modal system
      (message "No buffer on this line."))))


(defun my-bm-kill ()
  "Kill the buffer under point and refresh the list."
  (interactive)
  (let ((target (get-char-property (point) 'buffer-target)))
    (unless target
      (setq target (get-char-property (line-beginning-position) 'buffer-target)))
    (when (buffer-live-p target)
      (kill-buffer target)
      (my-bm-draw))))

(defun my-bm-toggle ()
  "Toggle the Buffer Manager, ensuring we don't land in the File Manager."
  (interactive)
  (if (eq (current-buffer) (get-buffer "*BufferManager*"))
      ;; LOGIC FOR CLOSING THE BM
      (let ((target-buffer nil)
            (blist (buffer-list)))
        ;; Iterate through buffers to find the first one that isn't a "manager"
        (while (and blist (not target-buffer))
          (let ((buf (car blist)))
            (with-current-buffer buf
              (unless (or (derived-mode-p 'my-bm-mode)
                          (derived-mode-p 'my-fm-mode)
                          (string-prefix-p " " (buffer-name buf))) ; Skip hidden system buffers
                (setq target-buffer buf))))
          (setq blist (cdr blist)))
        
        (if target-buffer
            (switch-to-buffer target-buffer)
          (bury-buffer))) ; Fallback if no other "real" buffer exists
    ;; LOGIC FOR OPENING THE BM
    (my-bm-draw)))


;; --- MODAL SYSTEM ---

(define-minor-mode my-command-mode
  "A custom modal minor mode."
  :lighter " COMMAND"
  :keymap (let ((map (make-sparse-keymap)))
            (define-key map [remap self-insert-command] 'ignore)
            (define-key map (kbd "h") 'backward-char)
            (define-key map (kbd "j") 'next-line)
            (define-key map (kbd "k") 'previous-line)
            (define-key map (kbd "l") 'forward-char)
            (define-key map (kbd "i") 'my-toggle-modal)
            (define-key map (kbd "u") 'undo)
            (define-key map (kbd "r") 'redo)
	    (define-key map (kbd "s") 'hydra-save-write/body)
	    (define-key map (kbd "a") 'move-beginning-of-line)
            (define-key map (kbd "e") 'move-end-of-line)
            (define-key map (kbd "y") 'yank)
	    (define-key map (kbd "/") 'consult-line)
	    (define-key map (kbd "o") 'hydra-opener/body)
	    (define-key map (kbd "z") 'my-toggle-previous-buffer)
	    (define-key map (kbd "t") 'my-toggle-night-mode)
            (define-key map (kbd "f") (lambda () (interactive) (let ((start-dir (if (and (eq system-type 'android)
											 (string-equal (expand-file-name default-directory) 
												       (expand-file-name "~/")))
										    "/storage/emulated/0/"
										  default-directory)))
								 (my-fm-draw start-dir))))
            ;;(my-fm-draw default-directory)))
	    (define-key map (kbd "SPC") 'my-bm-toggle)
	    (define-key map (kbd "c") 'my-copy-wrapper)
	    (define-key map (kbd "d") 'me/cut-thing)
	    (define-key map (kbd "b") 'hydra-buffer/body)
	    (define-key map (kbd "v") 'my-set-mark-wrapper)
	    (define-key map (kbd "x") 'execute-extended-command)
            map)
  :global nil
  (if my-command-mode
      (setq cursor-type 'box)    ; Box cursor for Command mode
    (setq cursor-type 'bar)))    ; Bar cursor for Insert mode

(defun my-toggle-modal ()
  "Toggle between command and insert mode, with safety for manager buffers."
  (interactive)
  (if (derived-mode-p 'my-bm-mode 'my-fm-mode)
      (progn
        (my-command-mode -1)
        (setq cursor-type 'bar)   ; Ensure bar in manager buffers
        (message "Command mode disabled in Manager."))
    (if my-command-mode
        (progn
          (my-command-mode -1)
          (setq cursor-type 'bar) ; Switch to bar for INSERT
          (message "INSERT"))
      (progn
        (my-command-mode 1)
        (setq cursor-type 'box) ; Switch to box for COMMAND
        (message "COMMAND")))))


;; Global Escape logic
(global-set-key (kbd "<escape>") 
		(lambda () (interactive)
		  (if (active-minibuffer-window) (abort-recursive-edit)
		    (unless my-command-mode (my-toggle-modal)))))

(add-hook 'after-change-major-mode-hook 
	  (lambda () 
	    (unless (or (minibufferp) 
			(derived-mode-p 'my-fm-mode)
			(derived-mode-p 'my-bm-mode)) ;; ADD THIS LINE
	      (my-command-mode 1))))


;; --- MINIBUFFER NAVIGATION ---
(with-eval-after-load 'elisp-mode
  (define-key minibuffer-local-map (kbd "M-h") 'backward-char)
  (define-key minibuffer-local-map (kbd "M-l") 'forward-char)
  (define-key minibuffer-local-map (kbd "M-j") 'next-line)
  (define-key minibuffer-local-map (kbd "M-k") 'previous-line))

;; Specifically for Vertico (if you have it installed)
(with-eval-after-load 'vertico
  (define-key vertico-map (kbd "M-j") 'vertico-next)
  (define-key vertico-map (kbd "M-k") 'vertico-previous))


;; Force command mode on startup
(add-hook 'minibuffer-setup-hook (lambda () (setq cursor-type 'bar)))
(add-hook 'emacs-startup-hook (lambda () (my-command-mode 1)))

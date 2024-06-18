;Define a general key-map which can override major mode bindings

(defun my-test-keys-insert-mode-escape ()
  (interactive)
  (if (region-active-p)
      (deactivate-mark)
    (if (active-minibuffer-window)
        (abort-recursive-edit)
      (if (derived-mode-p 'dired-mode)
          (abort-recursive-edit)
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
(keymap-set my-test-keys-minor-mode-map "g" 'bookmark-jump)
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

(defhydra copy-text-hydra
  (:color blue)
  "select region of text to copy"
  ("w" me/copy-word "Cut to end of word")      
  ("l" me/copy-line "Copy line")      
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

(defhydra miscellaneous-hydra
  (:color blue)
  "select region of text to copy"
  ("s" search-hydra/body "Search and replace operations")      
  ("e" kill-line "Cut to end of line")      
  ("b" me/delete-current-text-block "Cut block")      
  ("d" me/kill-all-dired-buffers "Kill all dired buffers"))

(keymap-set my-test-keys-minor-mode-map "m" 'miscellaneous-hydra/body)


(defhydra set-mark-hydra
  (:color blue)
  "select region of text to copy"
  ("e" er/expand-region "Expand region")      
  ("r" rectangle-mark-mode "Mark rectangle")      
  ("v" set-mark-command "Mark by line")
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
        (next-buffer)
        (let ((map (make-sparse-keymap)))
           (define-key map (kbd "l") 'next-buffer)
           (define-key map (kbd "h") 'previous-buffer)
           (set-transient-map map t)))

      (defun my-previous-buffer ()
        "move cursor to previous buffer.
   Press h will do it again, press l will move to next buffer. Press other key to exit."
        (interactive)
        (previous-buffer)
        (let ((map (make-sparse-keymap)))
           (define-key map (kbd "l") 'next-buffer)
           (define-key map (kbd "h") 'previous-buffer)
           (set-transient-map map t)))

(defun me/find-org-files-in-my-documents ()
  "Use `find-dired` to identify .org files in ~/my_docs/ and display the results in a dired buffer."
  (interactive)
  (let ((directory "~/my_docs/")
        (args "-type f -name \"*.org\""))
    (find-dired directory args)))


(defun me/find-org-files-in-work-documents ()
  "Use `find-dired` to identify .org files in ~/work_docs/ and display the results in a dired buffer."
  (interactive)
  (let ((directory "~/work_docs/")
        (args "-type f -name \"*.org\""))
    (find-dired directory args)))




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


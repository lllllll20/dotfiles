;Define a general key-map which can override major mode bindings


(defun my-test-keys-insert-mode-escape ()
  (interactive)
  (if (region-active-p)
      (deactivate-mark)
    (if (active-minibuffer-window)
	(abort-recursive-edit)
      (my-test-keys-command-mode-activate))))
      

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
    (define-key map (kbd "C-M-h") 'beginning-of-line)
    (define-key map (kbd "C-M-j") 'end-of-buffer)
    (define-key map (kbd "C-M-k") 'beginning-of-buffer)
    (define-key map (kbd "C-M-l") 'end-of-line)
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

(keymap-set my-test-keys-minor-mode-map "h" 'backward-char)
(keymap-set my-test-keys-minor-mode-map "l" 'forward-char)
(keymap-set my-test-keys-minor-mode-map "j" 'next-line)
(keymap-set my-test-keys-minor-mode-map "k" 'previous-line)
(keymap-set my-test-keys-minor-mode-map "i" 'my-test-keys-insert-mode-activate)
(define-key my-test-keys-minor-mode-map (kbd "<C-return>") 'er/expand-region)

(defvar my-test-keys-command-mode--deactivate-func nil)
(defvar my-insert-state-p t)


(defun my-test-keys-command-mode-init ()
  (interactive)
  (setq my-insert-state-p nil)
  (when my-test-keys-command-mode--deactivate-func
    (funcall my-test-keys-command-mode--deactivate-func))
  (setq my-test-keys-command-mode--deactivate-func
	(set-transient-map my-test-keys-minor-mode-map (lambda () t))))

(defun my-test-keys-insert-mode-init ()
  (interactive)
  (setq my-insert-state-p t)
  (funcall my-test-keys-command-mode--deactivate-func))

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

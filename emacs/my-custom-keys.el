
; Define a general key-map

(defvar my-overwrite-keys-minor-mode-map
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
    map)
  "my-overwrite-keys-minor-mode keymap.")

(define-minor-mode my-overwrite-keys-minor-mode
  "A minor mode for more comfortable navigation."
  :init-value t
  :lighter " my-keys")


(my-overwrite-keys-minor-mode 1)

; following is necessary to overwrite major mode keybindings, which otherwise take precedence
(add-to-list 'emulation-mode-map-alists `((my-overwrite-keys-minor-mode . ,my-overwrite-keys-minor-mode-map))) 

; Define the modal key map

(define-minor-mode my-modal-keys-minor-mode
  "Minor mode to be able to move using hjkl"
  :lighter " my-modal-keys"
  :keymap '(([remap self-insert-command]  ignore)) ; The actual keymaps are defined later below
  (add-to-list 'emulation-mode-map-alists '(my-modal-keys-minor-mode . my-modal-keys-minor-mode-map))
  )
(keymap-set my-modal-keys-minor-mode-map "<escape>" 'my-modal-keys-minor-mode)

(keymap-set my-overwrite-keys-minor-mode-map "M-SPC" 'my-modal-keys-minor-mode)

(keymap-set my-modal-keys-minor-mode-map "h" 'backward-char)
(keymap-set my-modal-keys-minor-mode-map "j" 'next-line)
(keymap-set my-modal-keys-minor-mode-map "k" 'previous-line)
(keymap-set my-modal-keys-minor-mode-map "l" 'forward-char)





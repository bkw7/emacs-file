;; MY .EMACS

; add emacs extensions to path
(add-to-list 'load-path "~/emacs-extensions/")

(setq-default c-basic-offset 4) ; tab size of 4 for c/c++
(global-set-key "\C-x\C-b" 'buffer-menu) ; makes buffer menu appear in current window

(setq x-select-enable-clipboard t) ; allow emacs to access clipboard
(setq x-select-enable-clipboard t)
  (global-set-key [(shift delete)] 'clipboard-kill-region)
  (global-set-key [(control insert)] 'clipboard-kill-ring-save)
  (global-set-key [(shift insert)] 'clipboard-yank)

; Configure M - up and M - down to move lines up and down
(defun move-text-internal (arg) 
   (cond 
    ((and mark-active transient-mark-mode) 
     (if (> (point) (mark)) 
        (exchange-point-and-mark)) 
     (let ((column (current-column)) 
          (text (delete-and-extract-region (point) (mark)))) 
       (forward-line arg) 
       (move-to-column column t) 
       (set-mark (point)) 
       (insert text) 
       (exchange-point-and-mark) 
       (setq deactivate-mark nil))) 
    (t 
     (beginning-of-line) 
     (when (or (> arg 0) (not (bobp))) 
       (forward-line) 
       (when (or (< arg 0) (not (eobp))) 
        (transpose-lines arg)) 
       (forward-line -1))))) 
(defun move-text-down (arg) 
   "Move region (transient-mark-mode active) or current line 
  arg lines down." 
   (interactive "*p") 
   (move-text-internal arg)) 
(defun move-text-up (arg) 
   "Move region (transient-mark-mode active) or current line 
  arg lines up." 
   (interactive "*p") 
   (move-text-internal (- arg))) 
(global-set-key [\M-up] 'move-text-up) 
(global-set-key [\M-down] 'move-text-down) 

;;; Fix junk characters in shell-mode
(add-hook 'shell-mode-hook 
          'ansi-color-for-comint-mode-on)
;autopair
(require 'autopair)
(autopair-global-mode) ;; enable autopair in all buffers 

; extra keybindings
(global-set-key [?\C-x ?\C-n] 'next-buffer) 
(global-set-key [?\C-x ?\C-p] 'previous-buffer)
(global-set-key [?\C-x ?\C-a] 'compile)

(setq backup-directory-alist '(("." . "~/.emacs.d/backup"))
  backup-by-copying t    ; Don't delink hardlinks                                                                                                                               
  version-control t      ; Use version numbers on backups                                                                                                                       
  delete-old-versions t  ; Automatically delete excess backups                                                                                                                  
  kept-new-versions 20   ; how many of the newest versions to keep                                                                                                              
  kept-old-versions 5    ; and how many of the old                                                                                                                              
  )
(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(inhibit-startup-screen t))
(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 )

(when (fboundp 'winner-mode)
  (winner-mode 1))

(global-set-key (kbd "C-x p") 'previous-multiframe-window)
(global-set-key (kbd "S-C-<left>") 'shrink-window-horizontally)
(global-set-key (kbd "S-C-<right>") 'enlarge-window-horizontally)
(global-set-key (kbd "S-C-<down>") 'shrink-window)
(global-set-key (kbd "S-C-<up>") 'enlarge-window)
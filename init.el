;; -*- lexical-binding: t -*-

(menu-bar-mode -1)

(setq tab-always-indent 'complete)
(setq enable-recursive-minibuffers t)

(progn
  (setq backup-by-copying t
        backup-directory-alist '(("." . "~/.saves/"))
        delete-old-versions t
        kept-new-versions 6
        kept-old-versions 2
        version-control t)
  (setq auto-save-file-name-transforms `((".*" ,temporary-file-directory t)))
  (setq backup-directory-alist `((".*" . ,temporary-file-directory))))

(progn
  (put 'downcase-region 'disabled nil)
  (put 'upcase-region 'disabled nil))

;; Configure indentation.
(progn
  (setq-default indent-tabs-mode nil)
  (electric-indent-mode -1)

  (setq c-basic-offset 2)
  (setq css-indent-offset 2)
  (setq js-indent-level 2)
  (setq sh-basic-offset 2))

;; Builtin global modes.
(progn
  (global-auto-revert-mode))

(defmacro save-column (&rest body)
  `(let ((column (current-column)))
     (unwind-protect
         (progn ,@body)
       (move-to-column column))))
(put 'save-column 'lisp-indent-function 0)

(defun move-line-up ()
  (interactive)
  (save-column
    (transpose-lines 1)
    (forward-line -2)))

(defun move-line-down ()
  (interactive)
  (save-column
    (forward-line 1)
    (transpose-lines 1)
    (forward-line -1)))

(defun sort-lines-dwim ()
  "Sort the lines in the buffer (or the region, if active)."
  (interactive)
  (if (region-active-p)
      (call-interactively 'sort-lines)
    (sort-lines nil (point-min) (point-max))))

;; Unset prefixes
(progn
  (global-unset-key (kbd "C-c c"))
  (global-unset-key (kbd "C-M-o"))
  (global-unset-key (kbd "C-M-h"))
  )

(global-set-key (kbd "C-c c") 'recompile)

(progn
  (global-set-key (kbd "C-c f") 'projectile-find-file)
  (global-set-key (kbd "C-c g") 'projectile-ripgrep)
  )

(progn
  (global-set-key (kbd "M-n") 'move-line-down)
  (global-set-key (kbd "M-p") 'move-line-up)
  )

(global-set-key (kbd "C-c s") 'magit-status)

(progn
  (global-set-key (kbd "C-M-h f") 'describe-function)
  (global-set-key (kbd "C-M-h v") 'describe-variable)
  (global-set-key (kbd "C-M-h k") 'describe-key)
  (global-set-key (kbd "C-M-x") 'eval-defun)
  (global-set-key (kbd "C-c a") 'align-regexp)
  (global-set-key (kbd "C-c b") 'shell)
  (global-set-key (kbd "C-c d c") 'describe-char)
  (global-set-key (kbd "C-c d f") 'describe-function)
  (global-set-key (kbd "C-c d m") 'describe-mode)
  (global-set-key (kbd "C-c f") 'projectile-find-file)
  (global-set-key (kbd "C-c j") 'join-line)
  (global-set-key (kbd "C-c k") 'fundamental-mode)
  (global-set-key (kbd "C-c m") 'make-directory)
  (global-set-key (kbd "C-c n") 'normal-mode)
  (global-set-key (kbd "C-c o") 'occur)
  (global-set-key (kbd "C-c w") 'browse-url)
  (global-set-key (kbd "C-c y") 'browse-kill-ring)
  (global-set-key (kbd "C-c z") 'sort-lines-dwim)
  (global-set-key (kbd "C-h") 'backward-delete-char)
  (global-set-key (kbd "C-x C-b") 'ibuffer)
  (global-set-key (kbd "C-x C-o") 'other-window)
  (global-set-key (kbd "C-x t") 'string-rectangle)
  (global-set-key (kbd "M-/") 'hippie-expand)
  (global-set-key (kbd "M-h") 'backward-kill-word)
  (global-set-key (kbd "RET") 'newline)
  )

(vertico-mode)
;(savehist-mode)

(setq completion-styles '(orderless basic)
        completion-category-defaults nil
        completion-category-overrides '((file (styles partial-completion))))

(marginalia-mode)

(require 'eglot)
(add-to-list 'eglot-server-programs '(elixir-mode "elixir-ls"))

(add-hook 'elixir-mode-hook 'eglot-ensure)

;; -*- lexical-binding: t -*-

(set-frame-font "DM Mono-16" nil t)
(setq inhibit-splash-screen t)
(setq frame-resize-pixelwise t)

(when (fboundp 'tool-bar-mode)
  (blink-cursor-mode -1)
  (tool-bar-mode -1)
  (menu-bar-mode -1)
  (scroll-bar-mode -1))

(set-fringe-mode 24)

(add-hook 'after-save-hook
          'executable-make-buffer-file-executable-if-script-p)

(setq fill-nobreak-predicate '(fill-single-word-nobreak-p))
(setq kill-whole-line t)
(setq whitespace-style '(face trailing lines-tail empty))
(setq uniquify-buffer-name-style 'post-forward-angle-brackets)

(load-theme 'zenburn t)
(set-face-background 'default "#000000")
(set-face-background 'fringe "#000000")

(menu-bar-mode -1)
(column-number-mode 1)

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
(global-company-mode)

;(savehist-mode)
(require 'orderless)
(setq completion-styles '(orderless basic)
        completion-category-defaults nil
        completion-category-overrides '((file (styles partial-completion))))

(marginalia-mode)

(require 'eglot)
(add-to-list 'eglot-server-programs '(elixir-mode "elixir-ls"))

(add-hook 'elixir-mode-hook
          (lambda ()
            (eglot-ensure)
            (add-hook 'before-save-hook 'eglot-format nil t)))

(add-hook 'emacs-lisp-mode-hook #'enable-paredit-mode)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("23621fdae219f1d49f831789073af07fbcd7ea67223688aacfdc91acaf394fa3" "1ecd6ec06003d01d836627631850716b12949f85d86428f916c58a2ba8937d13" "e7bae1e5baa33d1d36a90a2ce11e446e4fe57d0949d3c3597d7b46dd80e03830" default)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(defun my-zoom-in ()
  (interactive)
  (default-text-scale-increase))

(defun my-zoom-out ()
  (interactive)
  (default-text-scale-decrease))

(progn
  (global-set-key (kbd "C-x C-+") 'my-zoom-in)
  (global-set-key (kbd "C-x C--") 'my-zoom-out))

(progn
  (require 'dired)
  (require 'dired-x)
  (define-key dired-mode-map (kbd "r") 'wdired-change-to-wdired-mode)
  )

(defun magit-save ()
  (interactive)
  (save-some-buffers)
  (magit-git-command "git save"))

(global-set-key (kbd "C-c S") 'magit-save)

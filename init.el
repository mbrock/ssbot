;; -*- lexical-binding: t -*-

(set-frame-font "Berkeley Mono-16" nil t)
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
  (global-auto-revert-mode)
  (global-whitespace-cleanup-mode)
  (global-hl-line-mode))

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
  (global-set-key (kbd "C-c t") 'projectile-run-vterm)
  )

(global-company-mode)

(add-hook 'emacs-lisp-mode-hook #'enable-paredit-mode)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("23621fdae219f1d49f831789073af07fbcd7ea67223688aacfdc91acaf394fa3" "1ecd6ec06003d01d836627631850716b12949f85d86428f916c58a2ba8937d13" "e7bae1e5baa33d1d36a90a2ce11e446e4fe57d0949d3c3597d7b46dd80e03830" default))
 '(package-selected-packages
   '(yaml-mode zig-mode zenburn-theme whitespace-cleanup-mode vterm vertico typescript-mode sweeprolog s rainbow-delimiters projectile-ripgrep paredit orderless nix-mode marginalia magit flycheck elixir-mode eglot editorconfig direnv default-text-scale company))
 '(prolog-char-quote-workaround t)
 '(prolog-electric-colon-flag nil)
 '(prolog-electric-dash-flag nil)
 '(prolog-electric-dot-flag nil)
 '(prolog-electric-if-then-else-flag nil)
 '(prolog-electric-tab-flag nil)
 '(prolog-electric-underscore-flag t)
 '(prolog-indent-width 4))

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

(global-set-key (kbd "C-c .") 'xref-find-definitions)

(global-set-key (kbd "C-c q") 'fill-paragraph)

(progn
  (load-file "./vendor/copilot-emacs/copilot.el")

  (add-hook 'prog-mode-hook 'copilot-mode)

  (global-set-key (kbd "C-c C-SPC") 'copilot-complete)
  (setq copilot-idle-delay 0)
  
  (with-eval-after-load 'company
    ;; disable inline previews
    (delq 'company-preview-if-just-one-frontend company-frontends))
  
  (define-key copilot-completion-map (kbd "C-c TAB") 'copilot-accept-completion)
  ;; C-c C-n to get the next completion
  ;; C-c C-p to get the previous completion
  (define-key copilot-completion-map (kbd "C-c C-n") 'copilot-next-completion)
  (define-key copilot-completion-map (kbd "C-c C-p") 'copilot-previous-completion))
  

(load-file (concat (getenv "SWI_SOURCE") "/packages/sweep/sweeprolog.el"))
(add-to-list 'auto-mode-alist '("\\.pl$" . sweeprolog-mode))

;(setq sweeprolog-swipl-sources (getenv "SWI_SOURCE"))

(load-file "vendor/prolog.el")
(setq prolog-system 'swi)
(setq ediprolog-system 'swi)
(define-key prolog-mode-map (kbd "C-c C-c") 'ediprolog-dwim)
(define-key sweeprolog-mode-map (kbd "C-c C-r") 'ediprolog-dwim)

(load-file "./vendor/ttl-mode/ttl-mode.el")
(add-to-list 'auto-mode-alist '("\\.ttl\\'" . ttl-mode))

(setq ttl-electric-punctuation nil
      ttl-indent-level 2)

;;(use-package outli
;;  :load-path "vendor/outli"
;;    :bind (:map outli-mode-map
;;	      ("C-c C-p" . (lambda () (interactive) (outline-back-to-heading))))
;;  :hook ((prog-mode text-mode) . outli-mode)) ; or whichever modes you prefer

(load-file "vendor/outli/outli.el")
(add-hook 'prog-mode-hook 'outli-mode)
(add-hook 'text-mode-hook 'outli-mode)
(define-key outli-mode-map (kbd "C-c C-p") 'outli-back-to-heading)

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;; set up vertico, consult, marginalia, orderless
(progn
  (require 'vertico)
  (vertico-mode)

  (setq vertico-cycle t
        vertico-count 20)
  
  (setq minibuffer-prompt-properties
        '(read-only t cursor-intangible t face minibuffer-prompt))

  (add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)

  ;; vertico-directory
  (require 'vertico-directory)
  (define-key vertico-map (kbd "RET") #'vertico-directory-enter)
  (define-key vertico-map (kbd "DEL") #'vertico-directory-delete-char)
  (define-key vertico-map (kbd "M-DEL") #'vertico-directory-delete-word)
  (define-key vertico-map (kbd "M-d") #'vertico-directory-delete-word)
  
  ;; consult
  (require 'consult)
  (setq consult-project-root-function #'project-root)
  
  (global-set-key (kbd "C-x b") #'consult-buffer)
  (global-set-key (kbd "C-x 4 b") #'consult-buffer-other-window)
  (global-set-key (kbd "C-x 5 b") #'consult-buffer-other-frame)
  (global-set-key (kbd "C-x r b") #'consult-bookmark)
  (global-set-key (kbd "C-c h") #'consult-history)
  (global-set-key (kbd "M-y") #'consult-yank-pop)
  
  (require 'savehist)
  (savehist-mode)
  (setq savehist-file (concat user-emacs-directory ".emacs-history"))

  (require 'marginalia)
  (marginalia-mode)

  (require 'orderless)
  (setq completion-styles '(orderless basic)
        completion-category-defaults nil
        completion-category-overrides
        '((file (styles partial-completion)))))


;; make dired show file sizes in human readable format
(setq dired-listing-switches "-alh")

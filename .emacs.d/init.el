(menu-bar-mode -1)
(tool-bar-mode -1)
(setq visible-bell 1)

(setq bookmark-save-flag 1)

(setq display-time-24hr-format t)
(setq display-time-default-load-average nil)
(display-time-mode 1)

(require 'package)
(add-to-list 'package-archives  '("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'package-archives '("org" . "https://orgmode.org/elpa/") t)

(package-initialize)

(global-set-key (kbd "M-o")
		'other-window)

(global-set-key (kbd "C-.")
		'company-complete)

(add-to-list 'auto-mode-alist '("\\.ex\\'" . elixir-ts-mode))
(add-to-list 'auto-mode-alist '("\\.exs\\'" . elixir-ts-mode))
(add-to-list 'auto-mode-alist '("\\.heex\\'" . heex-ts-mode))

(winner-mode)

(use-package nov
  :ensure
  :init
  (add-to-list 'auto-mode-alist '("\\.epub\\'" . nov-mode)))

;; markdown mode is required to render elixir eldoc correctly
(use-package markdown-mode
  :ensure t)

;; highligt changes to files compared to the git state.
(use-package diff-hl
  :ensure
  :after magit
  :demand
  :hook (magit-post-refresh . diff-hl-magit-post-refresh)
  :config
  
  (global-diff-hl-mode)
  (diff-hl-flydiff-mode))

;; insert closing params etc
(use-package smartparens
  :ensure  smartparens ;; install the package
  :hook (prog-mode text-mode markdown-mode elixir-ts-mode) ;; add `smartparens-mode` to these hooks
  :config
  ;; load default config
  (require 'smartparens-config))


(use-package nix-mode
  :ensure t
  :init
  (add-to-list 'default-frame-alist '(inhibit-double-buffering . t)))


(use-package hurl-mode :mode "\\.hurl\\'"
  :vc (:url "https://github.com/JasZhe/hurl-mode"
            :rev :newest
            :branch "main"))

;; from https://www.emacswiki.org/emacs/BackupDirectory#h5o-4
(setq
   backup-by-copying t      ; don't clobber symlinks
   backup-directory-alist
    '(("." . "~/.emacs_savefiles/"))    ; don't litter my fs tree
   delete-old-versions t
   kept-new-versions 6
   kept-old-versions 2
   version-control t)       ; use versioned backups

;; (use-package vscode-dark-plus-theme
;;   :ensure t
;;   :config
;;   (load-theme 'vscode-dark-plus t)
;;    (set-face-font 'default "-1ASC-Liberation Mono-normal-normal-normal-*-24-*-*-*-m-0-iso10646-1")
;;   )


(use-package zenburn-theme
  :ensure t
  :config
  (custom-set-faces
   ;; make file references in exunit output visible
   '(ansi-color-black ((t (:background "MediumPurple2" :foreground "MediumPurple2")))))

  (load-theme 'zenburn t))

(use-package emacs
  :config
  (column-number-mode)
  )

(use-package emacs
  :ensure nil
  :custom
 
;; Should use:
;; (mapc #'treesit-install-language-grammar (mapcar #'car treesit-language-source-alist))
;; at least once per installation or while changing this list
  (treesit-language-source-alist
   '((heex "https://github.com/phoenixframework/tree-sitter-heex")
    (elixir "https://github.com/elixir-lang/tree-sitter-elixir"))))
 

(use-package dockerfile-mode
  :ensure t)

(use-package terraform-mode
  :ensure t
  :config
  (add-hook 'terraform-mode-hook
            (lambda () (add-hook 'before-save-hook 'terraform-format-buffer nil t)))
  )

(add-to-list 'default-frame-alist '(inhibit-double-buffering . t))

(use-package erlang
  :ensure t
  :config)


;; Compilation output auto follow
(setq compilation-scroll-output t)

;; line numbers
(use-package emacs
  :init
  (defun ii/enabled-line-numbers ()
    "Enable line numbers."
    (interactive)
    (display-line-numbers-mode)
    (setq display-line-numbers 'absolute))
  (add-hook 'prog-mode-hook 'ii/enabled-line-numbers))

;; testing next-ls elixir language server
;; (add-to-list 'exec-path "path/to/next-ls/bin/")

;; (use-package eglot
;;   :ensure nil
;;   :config
;;   (with-eval-after-load 'eglot
;;     (add-to-list 'eglot-server-programs
;; 		 `((elixir-ts-mode heex-ts-mode elixir-mode) .
;; 		   ("nextls" "--stdio=true" :initializationOptions (:experimental (:completions (:enable t)))))))

;;   (add-hook 'elixir-mode-hook 'eglot-ensure)
;;   (add-hook 'elixir-ts-mode-hook 'eglot-ensure)
;;   (add-hook 'heex-ts-mode-hook 'eglot-ensure))


;;     (add-to-list 'eglot-server-programs
;; 		 `((elixir-ts-mode heex-ts-mode elixir-mode) .
;; 		   ("nextls" "--stdio=true" :initializationOptions (:experimental (:completions (:enable t)))))))

;;   (add-hook 'elixir-mode-hook 'eglot-ensure)
;;   (add-hook 'elixir-ts-mode-hook 'eglot-ensure)
;;   (add-hook 'heex-ts-mode-hook 'eglot-ensure))

(use-package
  eglot
  :ensure nil
  :config (add-to-list 'eglot-server-programs `((elixir-ts-mode heex-ts-mode elixir-mode) . ("elixir-ls"))))


(use-package
 elixir-ts-mode
 :hook (elixir-ts-mode . eglot-ensure)
 (before-save . eglot-format))

;; make eldoc not pop up the minibuffer
(setq eldoc-echo-area-use-multiline-p nil)

(use-package exunit
 :ensure t
 :config (add-hook 'elixir-ts-mode-hook 'exunit-mode))

(use-package flycheck
  :ensure t
  :config (global-flycheck-mode))

;; My Emacs JSON starter pack:
(use-package json-mode
  :ensure t
  :config (add-hook 'json-mode-hook #'flycheck-mode))

;; Uses jq to format JSON in buffer
(use-package jq-format
  :ensure t
  :after json-mode)

;; Uses jsonlint to detect malformed JSON
(use-package flymake-json
  :ensure t)

(add-hook 'json-mode-hook 'flymake-json-load)

(use-package yaml-mode
  :ensure t)

(use-package groovy-mode
	     :ensure t)

(use-package org
  :config
  (setq org-return-follows-link  t)
  (setq org-duration-format (quote h:mm)))

(use-package magit
  :defer t
  :ensure t
  :init
  (global-set-key (kbd "C-x g") 'magit-status)
  (global-set-key (kbd "C-x M-g") 'magit-dispatch-popup)
  :config
					;(setq magit-diff-refine-hunk 'all)
  (setq magit-diff-refine-hunk nil)
  (setq magit-refresh-status-buffer nil)
  (setq magit-list-refs-sortby "-committerdate"))


(use-package web-mode
  :defer t
  :ensure t)

(use-package company
  :defer t
  :ensure t
  :config
  (setq company-idle-delay 0.00001)
  :init
  (add-hook 'after-init-hook 'global-company-mode))

(use-package helm
  :ensure t
  :defer t
  :config
  (setq helm-move-to-line-cycle-in-source nil)
  :init
  (helm-mode 1)
  (setq helm-mode-fuzzy-match t)
  (setq completion-styles '(flex))
  (global-set-key (kbd "C-x C-f") 'helm-find-files)
  (global-set-key (kbd "M-x") 'helm-M-x))

(use-package helm-git-grep
  :ensure t
  :defer t
  :config
  (setq  helm-allow-mouse nil)
  :init
  (global-set-key (kbd "C-c g") 'helm-git-grep))

(add-hook 'elixir-mode-hook
          (lambda () (add-hook 'before-save-hook 'elixir-format nil t)))

(use-package which-key
  :ensure t
  :config
  (which-key-mode))

(use-package minions
  :ensure t
  :config
  (minions-mode))

(use-package envrc
  :ensure t)

(use-package dumb-jump
  :ensure t)

(defun ii/decode-jwt (start end &optional jwt)
  "Decode JWT in region and print to help buffer."
  (interactive "r")
  (let* ((tok (if jwt jwt
                (buffer-substring start end)))
         (data (s-split "\\." tok))
         (header (car data))
         (claims (cadr data)))
    (with-temp-buffer
      (insert (format "%s\n\n%s"
                      (base64-decode-string header t)
                      (base64-decode-string claims t)))
      (json-pretty-print-buffer)
      (with-output-to-temp-buffer "*JWT*"
        (princ (buffer-string)))))
  t)

(envrc-global-mode)


;;; init.el ends here
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(company diff-hl dockerfile-mode dumb-jump envrc erlang exunit
	     flycheck flymake-json groovy-mode helm helm-git-grep
	     hurl-mode jq-format json-mode magit markdown-mode minions
	     nix-mode nov smartparens terraform-mode tree-sitter-langs
	     treemacs web-mode yaml-mode zenburn-theme)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-black ((t (:background "MediumPurple2" :foreground "MediumPurple2")))))

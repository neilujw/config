(menu-bar-mode -1)
(tool-bar-mode -1)
(setq visible-bell 1)

(setq bookmark-save-flag 1)

(setq display-time-24hr-format t)
(setq display-time-default-load-average nil)
(display-time-mode 1)

(tab-bar-mode 1)
(setq tab-bar-show 1) ; only show tab bars if there is more than one tab

(setq imenu-flatten 'prefix)
(setq imenu-level-separator ".")
(setq imenu-auto-rescan 1)


;(hs-minor-mode 1)
(global-hl-line-mode 1) ; highlight line
(global-auto-revert-mode 1) ; keep buffer up to date

(global-completion-preview-mode 1)

; (setq project-mode-line nil)

(load-file "~/.emacs.d/ibuffer-groups.el")

(require 'package)

(add-to-list 'package-archives  '("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'package-archives '("org" . "https://orgmode.org/elpa/") t)

(package-initialize)

(global-set-key (kbd "C-x C-b")
		'ibuffer)

;(global-set-key (kbd "M-o")
;		'other-window)

(global-set-key (kbd "C-.")
		'company-complete)


(global-set-key (kbd "M-n")
		'flymake-show-buffer-diagnostics)

(global-set-key (kbd "C-7")
		'undo)

(add-to-list 'auto-mode-alist '("\\.ex\\'" . elixir-ts-mode))
(add-to-list 'auto-mode-alist '("\\.exs\\'" . elixir-ts-mode))
(add-to-list 'auto-mode-alist '("\\.heex\\'" . heex-ts-mode))

(add-to-list 'display-buffer-alist
    '("\\*e?shell\\*" display-buffer-in-direction
       (direction . bottom)
       (window . root)
       (window-height . 0.2)))

(add-to-list 'display-buffer-alist
    '("\\*Flymake.*\\*" (display-buffer-reuse-window display-buffer-in-side-window)
       (side . bottom)
       (slot . 1)
       (window-height . 0.15)))

(add-to-list 'display-buffer-alist
    '("\\*exunit.*\\*" (display-buffer-reuse-window display-buffer-in-direction)
       (direction . bottom)
       (window . root)
       (window-height . 0.3)))

(add-to-list 'display-buffer-alist
    '((major-mode . dired-mode) display-buffer-in-side-window
       (side . left)
       (slot . 1)
       (window-height . fit-window-to-buffer)))

(defun x-buffer-has-project (buffer action)
  (with-current-buffer buffer (project-current nil)))

;; (defun x-tab-group-name (buffer alist)
;;   (with-current-buffer buffer (concat ">" (or (cdr (project-name (project-current "Ungrouped")))))))

(defun x-tab-tab-name (buffer alist)
  (with-current-buffer buffer
    (project-name (project-current))))

(defun x-reload-tab-bars (&optional dummy)
  (interactive)
  (x-tab-bar-tabs-set (frame-parameter nil 'tabs)))

(defun x-tab-bar-tabs-set (tabs &optional frame)
  (set-frame-parameter frame 'tabs (seq-sort-by (lambda (el) (alist-get 'group el nil))
                                                #'string-lessp
                                                tabs)))

(add-to-list 'display-buffer-alist
             '(x-buffer-has-project
               (display-buffer-in-tab display-buffer-reuse-window)
               (tab-name . x-tab-tab-name)))

(add-hook 'window-selection-change-functions #'x-reload-tab-bars)

(add-hook 'dired-mode-hook
	  (lambda() "Do not show dired detail by default" (dired-hide-details-mode)))

; use one buffer for dired
(setq dired-kill-when-opening-new-dired-buffer 1)

; M-x window-toggle-side-windows (show/hide side windows!)

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
;; (use-package smartparens
;;   :ensure  smartparens ;; install the package
;;   :hook (prog-mode text-mode markdown-mode elixir-ts-mode) ;; add `smartparens-mode` to these hooks
;;   :config
;;   ;; load default config
;;   (require 'smartparens-config))


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


(setq zenburn-override-colors-alist
      '(("zenburn-bg" . "#111111") ;; background
	("zenburn-bg-1" . "#5F5F5F") ;; modeline background + company complete selection 
	("zenburn-bg+1" . "#3F3F3F") ;; helm selection highlight + window separator + inactive tabs background; company complete background
	("zenburn-green+1" . "#B3B1B1") ;; modeline foreground
	))


(load-theme 'zenburn t)

;; zenburn colors for mode-line
;; `(mode-line
;;   ((,class (:foreground ,zenburn-green+1
;;                         :background ,zenburn-bg-1
;;                         :box (:line-width -1 :style released-button)))
;;    (t :inverse-video t)))
;; `(mode-line-buffer-id ((t (:foreground ,zenburn-yellow :weight bold))))
;; `(mode-line-inactive
;;   ((t (:foreground ,zenburn-green-2
;;                    :background ,zenburn-bg-05
;;                    :box (:line-width -1 :style released-button)))))



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
  :config (add-to-list 'eglot-server-programs `((elixir-ts-mode heex-ts-mode) . ("elixir-ls"))))

(fset #'jsonrpc--log-event #'ignore)

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

(setq magit-display-buffer-function ' magit-display-buffer-fullframe-status-v1)
(setq magit-bury-buffer-function 'magit-restore-window-configuration)


(use-package web-mode
  :defer t
  :ensure t)

;; (use-package company
;;   :defer t
;;   :ensure t
;;   :config
;;   (setq company-idle-delay 0.00001)
;;   :init
;;   (add-hook 'after-init-hook 'global-company-mode))

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
  (global-set-key (kbd "M-x") 'helm-M-x)
  (global-set-key (kbd "M-y") 'helm-show-kill-ring))

(use-package helm-git-grep
  :ensure t
  :defer t
  :config
  (setq  helm-allow-mouse nil)
  :init
  (global-set-key (kbd "C-c g") 'helm-git-grep))


(use-package which-key
  :ensure t
  :config
  (which-key-mode))

(use-package minions
  :ensure t
  :config
  (minions-mode))

(use-package ace-window
  :ensure t
  :bind
  (:map global-map
	("M-o" . ace-window)
	("M-p" . ace-swap-window))
  :config
  (setq aw-scope 'frame
	aw-ignore-current t))

(use-package projectile
  :ensure t
  :init
  (setq projectile-project-search-path '("~/ws/" "~/config/"))
  (projectile-mode +1)
  :bind (:map projectile-mode-map
              ("C-c p" . projectile-command-map)))

(use-package envrc
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
; level of treesitter feature, 1-4
(custom-set-variables
 '(treesit-font-lock-level 4))


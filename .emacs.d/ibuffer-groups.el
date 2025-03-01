(let ((not-dired-filter '(not used-mode . dired-mode))
      (not-magit-filter '(not name . "^magit")))
  (setq ibuffer-saved-filter-groups
     `(("default"
      ("f360" (and (filename . "ws/f360") ,not-magit-filter ,not-dired-filter))
      ("other elixir" (file-extension . "exs?") ,not-magit-filter ,not-dired-filter)
      ("emacs"
       (and (file-extension . "el") ,not-magit-filter ,not-dired-filter))
      ("magit" (name . "^magit"))
      ("dired" (mode . dired-mode))))))

(add-hook 'ibuffer-mode-hook
	  (lambda ()
	    (ibuffer-switch-to-saved-filter-groups "default")))

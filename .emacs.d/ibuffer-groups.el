(setq  ibuffer-saved-filter-groups
   '(("default"
      ("f360" (filename . "ws/f360"))
      ("other elixir" (file-extension . "exs?"))
      ("emacs"
       (or (file-extension . "el") (name . "^\\*Messages\\*$")))
      ("magit" (name . "^magit")))))

(add-hook 'ibuffer-mode-hook
	  (lambda ()
	    (ibuffer-switch-to-saved-filter-groups "default")))

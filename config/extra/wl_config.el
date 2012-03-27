
(autoload 'wl "wl" "Wanderlust" t)
(autoload 'wl-other-frame "wl" "Wanderlust on new frame" t)
(autoload 'wl-draft "wl-draft" "Write draft with Wanderlust." t)
(if (boundp 'mail-user-agent)
    (setq mail-user-agent 'wl-user-agent))
(if (boundp 'read-mail-command)
  (setq read-mail-command 'wl))
(if (fboundp 'define-mail-user-agent)
  (define-mail-user-agent
    'wl-user-agent
    'wl-user-agent-compose
    'wl-draft-send
    'wl-draft-kill
    'mail-send-hook))

(global-set-key [f3] 'wl)

(setq wl-init-file "~/.wl/init.gpg")

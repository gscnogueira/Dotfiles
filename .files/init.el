(require 'package)
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                             ("org" . "https://orgmode.org/elpa/")
                             ("elpa" . "https://elpa.gnu.org/packages/")))
(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

(unless (package-installed-p 'use-package)
  (package-install 'use-package))
(require 'use-package)
(setq use-package-always-ensure t)

(setq inhibit-splash-screen t)   ;; Don't show startup message
(scroll-bar-mode -1)	       ;; Disable visible scrollbar
(tool-bar-mode -1)	       ;; Disable the toolbar
(tooltip-mode -1)	               ;; Disable tooltips
(set-fringe-mode 10)	       ;; Give some breathing room
(menu-bar-mode -1)	       ;; Disable the menu bar
(blink-cursor-mode 1)	       ;; Blink cursor
(setq mouse-autoselect-window t)

;;(setq visible-bell t)          ;; Set up the visual bell

(column-number-mode)                 ;; show column number in the mode-line
(global-display-line-numbers-mode t) ;; enable line numbers

(dolist (mode '(org-mode-hook
                dired-mode-hook
                shell-mode-hook
                term-mode-hook
                tetris-mode-hook
                Man-mode-hook
                Man-mode-hook
                inferior-haskell-mode
                helpful-mode-hook
                mu4e-main-mode-hook
                doc-view-mode-hook
                calendar-mode-hook
                inferior-octave-mode-hook
                vterm-mode-hook
                elfeed-search-mode-hook
                elfeed-show-mode-hook
                eshell-mode-hook))
    (add-hook mode (lambda() (display-line-numbers-mode 0))))

(use-package winner-mode
  :ensure nil
  :init
  (winner-mode)
  :bind(:map evil-window-map
             ("u" . winner-undo)
             ("U" . winner-redo)))

(global-set-key (kbd "C-x p") 'previous-window-any-frame)

(global-unset-key (kbd "C-x ["))
(global-unset-key (kbd "C-x ]"))
(global-unset-key (kbd "C-x C-b"))

(global-set-key (kbd "C-x [") 'previous-buffer)
(global-set-key (kbd "C-x ]") 'next-buffer)
(global-set-key (kbd "C-x C-b") 'ibuffer)

(global-auto-revert-mode 1)

(set-face-attribute 'default nil
                    :font "JetBrains Mono"
                    :height 105
                    :weight 'normal)

;; Set fixed pitch face
(set-face-attribute 'fixed-pitch nil
                    :font "JetBrains Mono"
                    :height 105)

;; Set variable pitch face
;;(set-face-attribute 'variable-pitch nil
                    ;;:font "Cantarell"
                    ;;:height 135
                    ;;:weight 'regular)

(use-package emojify)

(use-package all-the-icons)

(use-package doom-modeline
  :after all-the-icons
  :init
  (doom-modeline-mode 1))
(setq doom-modeline-height 40)

(use-package doom-themes
  :init (load-theme 'doom-solarized-dark t))

(setq code-directory "/home/gabriel/Code/")
(setq config-directory "/home/gabriel/.dotfiles/")
(setq semestre-file "/home/gabriel/Notes/UnB/2021-01/2021-01.org")

(set-register ?p (cons 'file (concat code-directory "PC/test.cpp")))
(set-register ?c (cons 'file code-directory))
(set-register ?d (cons 'file config-directory))
(set-register ?u (cons 'file semestre-file))

(use-package undo-tree
  :config
  (global-undo-tree-mode 1))

(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

(use-package which-key
  :init (which-key-mode)
  :diminish which-key-mode
  :config
  (setq which-key-idle-delay 0.3))

(defun gscn/evil-hook ()
  (dolist (mode '(shell-mode eshell-mode vterm-mode))
          (add-to-list 'evil-emacs-state-modes mode)))

(use-package evil
  :init
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil) ;; necessary to use evil collection
  (evil-mode 1)
  :hook (evil-mode . gscn/evil-hook)
  :config
  (define-key evil-insert-state-map (kbd "C-g") 'evil-normal-state)
  (evil-set-initial-state 'messages-buffer-mode 'normal)
  (evil-set-undo-system 'undo-tree)) 

(use-package evil-collection
  :after evil
  :config
  (evil-collection-init))

(use-package ivy
  :diminish ;; dont show minor mode in the bar

  :init (ivy-mode 1)
  :bind (;;("C-s" . swiper) ;; have to install swiper
	 :map ivy-minibuffer-map
	 ("TAB" . ivy-alt-done)
	 ("C-l" . ivy-alt-done)
	 ("C-j" . ivy-next-line)
	 ("C-k" . ivy-previous-line)
	 :map ivy-switch-buffer-map
	 ("C-k" . ivy-previous-line)
	 ("C-l" . ivy-done)
	 ("C-d" . ivy-switch-buffer-kill)
	 :map ivy-reverse-i-search-map
	 ("C-k" . ivy-previous-line)
	 ("C-d" . ivy-reverse-i-search-kill)))

(use-package counsel
  :bind (("M-x" . counsel-M-x)
	 ("C-x b" . counsel-ibuffer)
	 ("C-x C-f" . counsel-find-file)
	 ("C-x C-r" . counsel-buffer-or-recentf)
	 ("C-M-j" . counsel-switch-buffer)
	 :map minibuffer-local-map
	 ("C-r" . counsel-minibuffer-history))
  :config
  (setq ivy-initial-inputs-alist nil) ;; Don't start searches with ^
  (recentf-mode 1)) ;; Don't start searches with ^

(use-package ivy-rich
  :init
  (ivy-rich-mode 1))

(use-package lsp-mode
  :commands (lsp lsp-deferred)
  :init
  (setq lsp-keymap-prefix "C-c l")
  :config
  (lsp-enable-which-key-integration t))

(use-package lsp-ui
  :hook (lsp-mode . lsp-ui-mode)
  :custom
  (lsp-ui-doc-position 'at-point))

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package smartparens
  :hook ((prog-mode . smartparens-mode)
         (prog-mode . show-smartparens-mode)))

(use-package evil-nerd-commenter
  :config
  (evilnc-default-hotkeys))

(use-package evil-surround
  :config
  (global-evil-surround-mode 1))

(setq-default tab-width 4)
(setq-default evil-shift-width 4)

(setq lsp-clients-clangd-args '("--header-insertion-decorators=0" "--header-insertion=never"))
(add-hook 'c++-mode-hook 'lsp-deferred)

(use-package csv-mode)

(use-package helpful
  :custom
  (counsel-describe-function-function #'helpful-callable)
  (counsel-describe-variable-function #'helpful-variable)
  :bind
  ([remap describe-function] . counsel-describe-function)
  ([remap describe-command] . helpful-command)
  ([remap describe-variable] . counsel-describe-variable)
  ([remap describe-key] . helpful-key))

(use-package go-mode)

(use-package haskell-mode)

(use-package ess)

(use-package typescript-mode
  :mode "\\.ts\\'"
  :hook (typescript-mode . lsp-deferred)
  :config
  (setq typescript-indent-level 2))

(use-package vimrc-mode)

(setq auto-mode-alist
		(cons '("\\.m$" . octave-mode) auto-mode-alist))

(add-hook 'octave-mode-hook
		  (lambda ()
			(abbrev-mode 1)
			(auto-fill-mode 1)
			(if (eq window-system 'x)
				(font-lock-mode 1))))

;; (use-package octave-mode
;;   :ensure nil
;;   :bind(
;; 		:map octave-mode-map
;; 			 ("<C-return>" . octave-send-line)
;; 			 ))

(require 'octave)
(define-key octave-mode-map (kbd "<C-return>") 'octave-send-line)

(use-package company
  :after lsp-mode
  :hook (prog-mode . company-mode)
  :custom
  (company-minimum-prefix-length 1)
  (company-idle-delay 0.0)
  (company-format-margin-function 'company-vscode-dark-icons-margin))

(use-package projectile
  :config
  (projectile-mode)
  (setq projectile-switch-project-action 'projectile-dired)
  :bind-keymap
  ("C-c p" . projectile-command-map)
  :init
  (setq projectile-project-search-path '("~/Code/UnB/" "~/.dotfiles/")))

(use-package hydra)

(use-package magit
:custom
(magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1))

(setq calendar-date-style 'european)

(use-package perspective
  :bind (("C-x k" . persp-kill-buffer*))
  :init
  (persp-mode))

(use-package crux
  :bind (
         ("C-x 4 -" . crux-transpose-windows)))

(setq Man-notify-method 'aggressive)

(use-package yasnippet
  :config
  (yas-global-mode 1))

(defun org-icons ()
   "Beautify org mode keywords."
   (interactive)
   (setq prettify-symbols-alist '(("[ ]" . "")
                                  ("[X]" . "")
                                  ))
   (prettify-symbols-mode))

(defun gscn/org-mode-setup()
  (org-indent-mode)
  (org-icons)
  (visual-line-mode 1)) 

(use-package org
  :hook ((org-mode . gscn/org-mode-setup)
         (org-mode . org-toggle-pretty-entities))
  :config
  (setq org-ellipsis " ▾"
        org-hide-emphasis-markers t
        org-startup-folded t
        org-directory "~/Notes")

  (setq org-list-demote-modify-bullet
        '(("+" . "-") ("-" . "+") ("*" . "-"))))

(require 'org-faces)

(dolist (face '((org-level-1 . 1.2)
                (org-level-2 . 1.1)
                (org-level-3 . 1.0)
                (org-level-4 . 1.0)
                (org-level-5 . 1.0)
                (org-level-6 . 1.0)
                (org-level-7 . 1.0)))
  (set-face-attribute (car face) nil :height (cdr face)))


(set-face-attribute 'org-document-title nil :height 1.5 :foreground "#b58900")

(use-package org-bullets
  :after org
  :hook (org-mode . org-bullets-mode)
  :custom
  (org-bullets-bullet-list '("◉" "○" "●" "○" "●" "○" "●")))

(setq org-clock-sound  "~/.config/sounds/pop.wav")
(setq org-show-notification-timeout 1)

;;(defun gscn/org-mode-visual-fill ()
    ;;(setq visual-fill-column-width 100
          ;;visual-fill-column-center-text t)
    ;;(visual-fill-column-mode 1))
;;
  ;;(use-package visual-fill-column
    ;;:hook (org-mode . gscn/org-mode-visual-fill))

(org-babel-do-load-languages
 'org-babel-load-languages '(
                             (emacs-lisp . t)
                             (C . t)
                             (python . t)
                             (shell . t)
                             (js     . t)
                             (haskell . t)))

(setq org-confirm-babel-evaluate nil) ;; não pergunta se vc quer validar
(setq org-src-window-setup 'current-window)

(require 'org-tempo)

(add-to-list 'org-structure-template-alist '("sh" . "src shell"))
(add-to-list 'org-structure-template-alist '("el" . "src elisp"))
(add-to-list 'org-structure-template-alist '("py" . "src python"))
(add-to-list 'org-structure-template-alist '("cpp" . "src cpp"))
(add-to-list 'org-structure-template-alist '("hs" . "src haskell"))
(add-to-list 'org-structure-template-alist '("js" . "src js :results output"))

(defun gscn/org-babel-tangle-config ()
  (when (string-match

         (expand-file-name "~/.dotfiles/.*\.org$")
         (buffer-file-name))
    (let ((org-confirm-babel-evaluate nil))
      (org-babel-tangle))))


(add-hook 'org-mode-hook (lambda () (add-hook 'after-save-hook #'gscn/org-babel-tangle-config)))

(use-package org-roam
  :init
  (setq org-roam-v2-ack t)
  :custom
  (org-roam-directory "~/Notes")
  :bind (("C-c n l" . org-roam-buffer-toggle)
         ("C-c f" . org-roam-node-find)
         ("C-c i" . org-roam-node-insert)
         )
  :config
  (org-roam-setup)
  )

(use-package term
  :config
  (setq explicit-shell-file-name "zsh")
  (setq term-prompt-regexp "^[^#$%>\\n]*[#$%>] *"))

(use-package eterm-256color
  :hook (term-mode . eterm-256color-mode))

(use-package vterm
    :commands vterm
    :bind ("C-c t" . vterm-other-window)
    :config
    (setq vterm-max-scrollback 10000)
    (evil-set-initial-state 'vterm-mode 'emacs)
)

(use-package vterm-toggle
      :bind (
             ("C-;" . vterm-toggle))
      :config
      (setq vterm-toggle-hide-method 'reset-window-configration)
      (setq vterm-toggle-reset-window-configration-after-exit t)
      (setq vterm-toggle-fullscreen-p nil)
(add-to-list 'display-buffer-alist
      '((lambda(bufname _) (with-current-buffer bufname (equal major-mode 'vterm-mode)))
         (display-buffer-reuse-window display-buffer-same-window)))
)

(defun gscn/configure-eshell ()
  ;; Save command history when commands are entered
  (add-hook 'eshell-pre-command-hook 'eshell-save-some-history)

  ;; Truncate buffer for performance
  (add-to-list 'eshell-output-filter-functions 'eshell-truncate-buffer)

  ;;Bind some useful keys for evil-mode
  (evil-define-key '(normal insert visual) ehsell-mode-map (kbd "C-r") 'counsel-esh-history)
  (evil-define-key '(normal insert visual) ehsell-mode-map (kbd "<home>") 'eshell-bol)
  (evil-normalize-keymaps)

  (setq eshell-history-size         10000
        ehsell-buffer-maximum-lines 10000
        eshell-hist-ignoredups      t))

(use-package eshell-git-prompt)

(use-package eshell
  :hook (eshell-first-time-mode . gscn/configure-eshell)
  :config
  (setq eshell-mode-map (make-sparse-keymap))
  (eshell-git-prompt-use-theme 'git-radar))

(defun eshell/ff (&rest args)
  (apply #'find-file args))

(defun eshell/cl ()
  (eshell/clear 1))

(defun eshell/gg (&rest args)
  (shell-command-to-string "ls"))

(use-package eshell-syntax-highlighting
  :after esh-mode
  :config
  ;; Enable in all Eshell buffers.
  (eshell-syntax-highlighting-global-mode +1))

(use-package eshell-toggle
  :bind ("C-:" . eshell-toggle))

(defun dired-videos ()
  (interactive)
  (dired-single-buffer "~/Videos/"))

(use-package dired-single)
(use-package dired
  :ensure nil
  :commands (dired dired-jump)
  :bind (("C-x C-j" . dired-jump))
  :custom
  ((dired-listing-switches "-agho --group-directories-first"))
  :config
  (evil-collection-define-key 'normal 'dired-mode-map
    "h" 'dired-single-up-directory
    "l" 'dired-single-buffer
    "b" 'dired-videos
    ))

(use-package all-the-icons-dired
  :hook (dired-mode .  all-the-icons-dired-mode))

(use-package dired-open
  :config
  (setq dired-open-extensions '(("png" . "sxiv")
                                ("mp4" . "mpv")
                                ("mkv" . "mpv"))))

(use-package dired-hide-dotfiles
  :hook (dired-mode . dired-hide-dotfiles-mode)
  :config
  (evil-collection-define-key 'normal 'dired-mode-map
    "H" 'dired-hide-dotfiles-mode))

(defun gscn/lookup-password(&rest keys)
  (let ((result (apply #'auth-source-search keys)))
    (if result
      (funcall (plist-get (car result) :secret))
      nil)))

(use-package mu4e
  :ensure nil
  :defer 20 ;; Wait until 20 seconds after startup
  :load-path "/usr/share/emacs/site-lisp/mu4e/"

  :bind (:map global-map
              ("C-c m " . mu4e))
  :config

  ;; This is set to 't' to avoid mail syncing isses when using mbsync
  (setq mu4e-change-filenames-when-moving t)

  ;; Refresh mail using isync every 10 minutes
  (setq mu4e-update-interval (* 10 60))
  (setq mu4e-get-mail-command "mbsync -a")
  (setq mu4e-maildir "~/Documents/Mail")
  (setq mu4e-compose-format-flowed t) ;; Text will be adapted to screen size 
  (setq mu4e-compose-signature "Att.\nGabriel S. C. Nogueira") ;; Text will be adapted to screen size 

  (setq user-mail-address "gab.nog94@gmail.com")
  (setq user-full-name "Gabriel da Silva Corvino Nogueira")
  (setq mu4e-drafts-folder "/[Gmail]/Rascunhos")
  (setq mu4e-sent-folder "/[Gmail]/E-mails enviados")
  (setq mu4e-refile-folder "/[Gmail]/Todos os e-mails")
  (setq mu4e-trash-folder "/[Gmail]/Lixeira")
  (setq smtpmail-smtp-server "smtp.gmail.com")
  (setq smtpmail-smtp-service 465)
  (setq smtpmail-stream-type 'ssl)
  (setq message-send-mail-function 'smtpmail-send-it)

  (setq mu4e-maildir-shortcuts
        '(("/Inbox"                    . ?i)
          ("/[Gmail]/E-mails enviados" . ?e)
          ("/[Gmail]/Lixeira"          . ?l)
          ("/[Gmail]/Rascunhos"        . ?r)
          ("/[Gmail]/Todos os e-mails" . ?t)))

  ( setq mu4e-bookmarks 
   '((:name "Unread messages" :query "flag:unread AND NOT flag:trashed" :key 117)
     (:name "Inbox" :query "maildir:/Inbox" :key ?i)
     (:name "Today's messages" :query "date:today..now" :key 116)
     (:name "Last 7 days" :query "date:7d..now" :hide-unread t :key 119)
     (:name "Messages with images" :query "mime:image/*" :key 112))
   )
  (mu4e t))

(defun gscn/elfeed-setup ()
    (( elfed-search-set-filter "@6-months-ago")
     ))
  (use-package elfeed
    :bind (:map global-map
            ("C-c e " . elfeed))
    :config
    (setq elfeed-feeds '(
                          ("https://feeds.feedburner.com/diolinux ")
                          ("https://itsfoss.com/feed/")
                          ("https://lukesmith.xyz/rss.xml")
                          ("https://noticias.unb.br/?format=feed&type=rss")
                          ("https://cic.unb.br/feed/")
                          ("https://decrepitos.com/podcast/feed.xml")
                          ("https://notrelated.libsyn.com/rss")
                          ("https://anchor.fm/s/14298150/podcast/rss")
                          ("https://www.youtube.com/feeds/videos.xml?channel_id=UCld68syR8Wi-GY_n4CaoJGA")
                          ("https://www.youtube.com/feeds/videos.xml?channel_id=UCEf5U1dB5a2e2S-XUlnhxSA")
                          ("https://www.youtube.com/feeds/videos.xml?channel_id=UCVls1GmFKf6WlTraIb_IaJg")
                          ("https://www.youtube.com/feeds/videos.xml?channel_id=UC2eYFnH61tmytImy1mTYvhA")
                          ("https://www.youtube.com/feeds/videos.xml?channel_id=UCsnGwSIHyoYN0kiINAGUKxg")
                          ("https://github.com/dracula/dracula-theme/commits/master.atom")
                          ("https://github.com/UnBalloon/aulas-avancadas/commits/main.atom")
                          ("https://www.archlinux.org/feeds/news/")
                          ("https://suckless.org/atom.xml")
                          ("https://emacsredux.com/atom.xml")
                          ("https://www.reddit.com/r/emacs.rss")


                          ))
    (advice-add 'elfeed :after 'elfeed-update)
)

(defun gscn/set-font-faces()
  (message "Setting faces!")
  (set-face-attribute 'default nil :font "JetBrains Mono" :height 105 :weight 'regular))

(if (daemonp)
    (add-hook 'after-make-frame-functions
	      (lambda(frame)
		(setq doom-modeline-icon t)
		(with-selected-frame frame
		  (gscn/set-font-faces))))
 (gscn/set-font-faces))

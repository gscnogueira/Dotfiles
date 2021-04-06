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

;;(setq visible-bell t)          ;; Set up the visual bell

(column-number-mode)                 ;; show column number in the mode-line
(global-display-line-numbers-mode t) ;; enable line numbers

(dolist (mode '(org-mode-hook
                shell-mode-hook
                term-mode-hook
                vterm-mode-hook
                elfeed-mode
                eshell-mode-hook))
    (add-hook mode (lambda() (display-line-numbers-mode 0))))

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

(use-package all-the-icons)

(use-package doom-modeline
  :init (doom-modeline-mode 1))

(use-package doom-themes
  :init (load-theme 'doom-nord t))

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

(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

(use-package which-key
  :init (which-key-mode)
  :diminish which-key-mode
  :config
  (setq which-key-idle-delay 0.3))

(defun gscn/evil-hook ()
  (dolist (mode '(shell-mode eshell-mode))
          (add-to-list 'evil-emacs-state-modes mode)))

(use-package evil
  :init
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil) ;; necessary to use evil collection
  (setq evil-want-C-u-scroll t)
  (evil-mode 1)
  :hook (evil-mode . gscn/evil-hook)
  :config
  (define-key evil-insert-state-map (kbd "C-g") 'evil-normal-state)
  (evil-set-initial-state 'messages-buffer-mode 'normal)) 

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
	 ("C-M-j" . counsel-switch-buffer)
	 :map minibuffer-local-map
	 ("C-r" . counsel-minibuffer-history))
  :config
  (setq ivy-initial-inputs-alist nil)) ;; Don't start searches with ^

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

(add-hook 'c++-mode-hook 'lsp-deferred)

(use-package helpful
  :custom
  (counsel-describe-function-function #'helpful-callable)
  (counsel-describe-variable-function #'helpful-variable)
  :bind
  ([remap describe-function] . counsel-describe-function)
  ([remap describe-command] . helpful-command)
  ([remap describe-variable] . counsel-describe-variable)
  ([remap describe-key] . helpful-key))

(use-package vimrc-mode)

(use-package typescript-mode
  :mode "\\.ts\\'"
  :hook (typescript-mode . lsp-deferred)
  :config
  (setq typescript-indent-level 2))

(use-package csv-mode)

(use-package company
  :after lsp-mode
  :hook (prog-mode . company-mode)
  :custom
  (company-minimun-prefix-lenght 1)
  (company-idle-delay 0.0))

(use-package company-box
:hook (company-mode . company-box-mode))

(use-package projectile
  :config (projectile-mode)
  :bind-keymap
  ("C-c p" . projectile-command-map)
  :init
  (when (and (file-directory-p "~/Code/Projects") (file-directory-p "~/.dotfiles"))
    (setq projectile-project-search-path '("~/Code/Projects" "~/.dotfiles"))))
  (setq projectile-switch-project-action #'projectile-dired)

(use-package counsel-projectile
  :config (counsel-projectile-mode))

(use-package magit
:custom
(magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1))

(defun gscn/org-mode-setup()
  (org-indent-mode)
  (visual-line-mode 1)) 

(use-package org
  :hook ((org-mode . gscn/org-mode-setup)
         (org-mode . org-toggle-pretty-entities))
  :config
  (setq org-ellipsis " ▾"
        org-hide-emphasis-markers t
        org-startup-folded t))
(require 'org-faces)

(dolist (face '((org-level-1 . 1.2)
                (org-level-2 . 1.1)
                (org-level-3 . 1.0)
                (org-level-4 . 1.0)
                (org-level-5 . 1.0)
                (org-level-6 . 1.0)
                (org-level-7 . 1.0)))
  (set-face-attribute (car face) nil :height (cdr face)))

(use-package org-bullets
  :after org
  :hook (org-mode . org-bullets-mode)
  :custom
  (org-bullets-bullet-list '("◉" "○" "●" "○" "●" "○" "●")))

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
                             (python . t)))

(setq org-confirm-babel-evaluate nil) ;; não pergunta se vc quer validar

(require 'org-tempo)

(add-to-list 'org-structure-template-alist '("sh" . "src shell"))
(add-to-list 'org-structure-template-alist '("el" . "src elisp"))
(add-to-list 'org-structure-template-alist '("py" . "src python"))
(add-to-list 'org-structure-template-alist '("cpp" . "src cpp"))

(defun gscn/org-babel-tangle-config ()
  (when (string-match

         (expand-file-name "~/.dotfiles/.*\.org$")
         (buffer-file-name))
    (let ((org-confirm-babel-evaluate nil))
      (org-babel-tangle))))


(add-hook 'org-mode-hook (lambda () (add-hook 'after-save-hook #'gscn/org-babel-tangle-config)))

(use-package term
  :config
  (setq explicit-shell-file-name "zsh")
  (setq term-prompt-regexp "^[^#$%>\\n]*[#$%>] *"))

(use-package eterm-256color
  :hook (term-mode . eterm-256color-mode))

(use-package vterm
  :commands vterm
  :config
  (setq vterm-max-scrollback 10000))

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
        eshell-hist-ignoredups      t
        ehsell-scroll-to-bottom-on-input t))

(use-package eshell-git-prompt)

(use-package eshell
  :hook (eshell-first-time-mode . gscn/configure-eshell)
  :config
  (eshell-git-prompt-use-theme 'powerline))

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
    "l" 'dired-single-buffer))

(use-package all-the-icons-dired
  :hook (dired-mode .  all-the-icons-dired-mode))

(use-package dired-open
  :config
  (setq dired-open-extensions '(("png" . "sxiv")
                                ("mkv" . "mpv"))))

(use-package dired-hide-dotfiles
  :hook (dired-mode . dired-hide-dotfiles-mode)
  :config
  (evil-collection-define-key 'normal 'dired-mode-map
    "H" 'dired-hide-dotfiles-mode))

(use-package elfeed
  :bind (:map global-map
          ("C-c e " . elfeed))
  :config
  (setq elfeed-feeds '(
                        ("https://feeds.feedburner.com/TheHackersNews?format=xml")
                        ("https://feeds.feedburner.com/diolinux ")
                        ("https://itsfoss.com/feed/")
                        ("https://lukesmith.xyz/rss.xml")
                        ("https://noticias.unb.br/?format=feed&type=rss")
                        ("https://cic.unb.br/feed/")
                        ("https://www.adm.unb.br/index.php?format=feed&type=rss")
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
                        ))
  (advice-add 'elfeed :after 'elfeed-update))

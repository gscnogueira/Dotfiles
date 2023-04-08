(setq inhibit-splash-screen t)
(scroll-bar-mode -1)
(tool-bar-mode -1)
(tooltip-mode -1)
(menu-bar-mode -1)

(load-theme 'modus-vivendi t)
(setq modus-themes-italic-constructs t)
(setq modus-themes-bold-constructs t)
(setq modus-themes-paren-match '(underline intense))
(setq modus-themes-syntax '(alt-syntax))

(defun gscn/set-font-faces()
  (set-face-attribute 'default nil
                      :font "JetBrains Mono"
                      :height 118
                      :weight 'normal ))
(gscn/set-font-faces)

(use-package all-the-icons)

(use-package doom-modeline
  :after all-the-icons
  :init
  (doom-modeline-mode 1))

(column-number-mode)                 ;; show column number in the mode-line

(global-display-line-numbers-mode t) ;; enable line numbers

(dolist (mode '(
                org-agenda-mode-hook
                org-mode-hook
                Man-mode-hook
                Man-mode-hook
                calendar-mode-hook
                dired-mode-hook
                doc-view-mode-hook
                elfeed-search-mode-hook
                elfeed-show-mode-hook
                eshell-mode-hook
                helpful-mode-hook
                inferior-haskell-mode
                inferior-octave-mode-hook
                mu4e-main-mode-hook
                shell-mode-hook
                term-mode-hook
                tetris-mode-hook
                vterm-mode-hook
                ))
  (add-hook mode (lambda() (display-line-numbers-mode 0))))

(require 'package)
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
			 ("org" . "https://orgmode.org/elpa/")
			 ("elpa" . "https://elpa.gnu.org/packages/")))
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)

(use-package undo-tree
  :config
  (global-undo-tree-mode 1))

(use-package evil
  :init
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil) ;; necessary to use evil collection
  (evil-mode 1)
  :config
  (evil-set-undo-system 'undo-tree)
  )

(use-package evil-collection
  :after evil
  :config
  (evil-collection-init))

(use-package ivy
  :init (ivy-mode 1)
  :bind (
         :map ivy-minibuffer-map
         ("C-k" . ivy-previous-line)
         ("C-j" . ivy-next-line)
         ("C-k" . ivy-previous-line)
         ("TAB" . ivy-alt-done)
         :map ivy-switch-buffer-map
         ("C-d" . ivy-switch-buffer-kill)
         ("C-k" . ivy-previous-line)
  ))

(use-package ivy-rich
  :init
  (ivy-rich-mode 1))

(use-package counsel
  :bind (
         ("M-x" . counsel-M-x)
         ("C-x b" . counsel-switch-buffer)
         ("C-x C-f" . counsel-find-file)
         ("C-x C-r" . counsel-buffer-or-recentf)
         ("C-M-j" . counsel-switch-buffer)
         :map minibuffer-local-map
         ("C-r" . counsel-minibuffer-history))
  :config
  (recentf-mode 1))

(use-package org
  :hook ((org-mode . org-indent-mode)
         (org-mode . visual-line-mode))
  :config
  (setq org-ellipsis " ▾"
        org-startup-folded t
        org-directory "~/Notes"
        org-hide-emphasis-markers t
        org-startup-with-inline-images t
        org-src-window-setup 'current-window
        org-log-done 'time)
  :bind
  (("C-c a" . org-agenda-list)
   ("C-c t" . org-todo-list)
   :map org-mode-map
   ("C-c SPC" . counsel-outline)
   ))

(setq prettify-symbols-alist '(("[ ]" . "")
                               ("[X]" . "")
                               ))

(setq-default org-agenda-files
              '("~/Notes/20210807112735-tasks.org"
                "~/Notes/20221025142716-engenharia_de_software_es.org"
                "~/Notes/20221025142726-programacao_concorrente_pc.org"
                "~/Notes/20221025142734-programacao_funcional_pf.org"
                "~/Notes/20221025142743-automatos_e_computabilidade_ac.org"
                "~/Notes/20220127092030-knedle.org"
                "~/Notes/20221025142750-computacao_experimental_ce.org"
                "~/Notes/20221025142758-informatica_e_sociedade_is.org"
                "~/Notes/20210904224143-aniversarios.org"))

(use-package org-bullets
  :after org
  :hook (org-mode . org-bullets-mode)
  :custom
  (org-bullets-bullet-list '("◉" "○" "✸")))

(org-babel-do-load-languages
 'org-babel-load-languages '((emacs-lisp . t)
                             (R . t)
                             (C . t)
                             (python . t)
                             (shell . t)
                             (ruby . t)
                             (sql . t)
                             (js     . t)
                             (haskell . t)))

(setq org-confirm-babel-evaluate nil)

(require 'org-tempo)

(setq org-structure-template-alist
      (append org-structure-template-alist '(("sh" . "src shell")
                                             ("el" . "src elisp")
                                             ("hs" . "src haskell")
                                             ("py" . "src python"))))

(setq org-tangle-files '("~/.dotfiles/.*\.org$"
                         "~/.xmonad/.*\.org$"))

(defun gscn/org-babel-tangle-config ()
  (when
      (let ((files (mapcar #'expand-file-name org-tangle-files)))
        (cl-some (lambda (x) (string-match-p x (buffer-file-name)))
                 files))
    (let ((org-confirm-babel-evaluate nil))
      (org-babel-tangle))
    )
  )

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

(add-to-list 'org-link-frame-setup '(file . find-file))

(use-package haskell-mode)

(use-package scala-mode
  :interpreter
    ("scala" . scala-mode))

(use-package vterm
  :commands vterm
  :config
  (setq vterm-max-scrollback 10000)
  (evil-set-initial-state 'vterm-mode 'emacs))

(use-package vterm-toggle
  :bind (("C-;" . vterm-toggle))
  :config
  (setq vterm-toggle-hide-method 'reset-window-configration)
  (setq vterm-toggle-reset-window-configration-after-exit t)
  (setq vterm-toggle-fullscreen-p nil)
  (add-to-list 'display-buffer-alist
               '((lambda(bufname _) (with-current-buffer bufname (equal major-mode 'vterm-mode)))
                 (display-buffer-reuse-window display-buffer-same-window)))
  )

(use-package auctex)
(setq TeX-auto-save t)
(setq TeX-parse-self t)

(use-package ox-reveal)

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package hydra)

(defhydra hydra-config-files (:color blue)
  "Config Files"
  ("e" (find-file (expand-file-name "~/.dotfiles/Emacs.org")) "Emacs")
  ("r" (find-file (expand-file-name "~/.config/rofi/config.rasi")) "Rofi")
  ("x" (find-file (expand-file-name "~/.xmonad/README.org")) "Xmonad")
  ("z" (find-file (expand-file-name "~/.dotfiles/Zsh.org")) "Zsh")
  )

(global-set-key (kbd "C-c c") 'hydra-config-files/body)

(setq make-backup-files nil)

(use-package dired
  :ensure nil
  :commands (dired dired-jump)
  :bind (("C-x C-j" . dired-jump))
  :custom
  ((dired-listing-switches "-gho --group-directories-first"))
  :config
  (evil-collection-define-key 'normal 'dired-mode-map
    "h" 'dired-up-directory
    "l" 'dired-find-file))

(winner-mode)

(global-unset-key (kbd "C-x ["))
(global-unset-key (kbd "C-x ]"))
(global-unset-key (kbd "C-x C-b"))

(global-set-key (kbd "C-x [") 'previous-buffer)
(global-set-key (kbd "C-x ]") 'next-buffer)
(global-set-key (kbd "C-x C-b") 'ibuffer)

(if (daemonp)
    (add-hook 'after-make-frame-functions
              (lambda(frame)
                (setq doom-modeline-icon t)
                (with-selected-frame frame
                  (gscn/set-font-faces))))
  (gscn/set-font-faces))

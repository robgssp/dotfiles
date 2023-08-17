; -*- emacs-lisp -*-
(require 'cl-lib)

;;; Error handling for the terminally lazy.
;;; This ends up wrapping quite a lot since I want to copy this file
;;; to workstations with old-ass underfeatured emacs.
(defmacro try (&rest body)
  `(condition-case ex
       (progn ,@body)
     ('error (message "%s failed." '(progn ,@body)))))

;;; Packages
(setq straight-use-package-by-default t)
(setq straight-check-for-modifications '(check-on-save find-when-checking))

(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(straight-use-package 'use-package)

;;; Fix whole file's indent
(defun indent-buffer ()
  "indent whole buffer"
  (interactive)
  (delete-trailing-whitespace)
  (indent-region (point-min) (point-max) nil))

(defun other-window-back (count &optional all-frames)
  (interactive "p")
  (other-window (- count) all-frames))

(defun filename-to-kill-ring ()
  "Copy the current buffer file name to the kill ring"
  (interactive)
  (let ((filename (if (equal major-mode 'dired-mode)
                      default-directory
                    (buffer-file-name))))
    (when filename
      (kill-new filename)
      (message "Copied file name '%s' to the kill ring." filename))))

;;; Sort sequence alphabetically
(defun sort-words (reverse beg end)
  "Sort words in region alphabetically, in REVERSE if negative.
    Prefixed with negative \\[universal-argument], sorts in reverse.

    The variable `sort-fold-case' determines whether alphabetic case
    affects the sort order.

    See `sort-regexp-fields'."
  (interactive "*P\nr")
  (sort-regexp-fields reverse "\\S-+" "\\&" beg end))

;;; Disable UI shit
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))

(setq read-file-name-completion-ignore-case t)
(setq completion-ignore-case t)
(setq next-screen-context-lines 7)
(setq auto-save-timeout 5)
(setq native-comp-async-report-warnings-errors nil)

(setq visible-bell 1)
(prefer-coding-system 'utf-8)

;;; Colors and styling
(when (cl-equalp (system-name) "saturn")
  (when (eq system-type 'gnu/linux)
    (use-package melancholy-theme
      :config
      (load-theme 'melancholy t)))

  (when (eq system-type 'windows-nt)
    (load-theme 'tango-dark t)
    (set-frame-font "Terminus-12" nil t)))

;;; Fixed tabs globally
(setq-default indent-tabs-mode nil)
(setq-default tab-width 8)

;;; Text mode tweaks
;;(setq-default fill-column 80)
(add-hook 'text-mode-hook 'turn-on-auto-fill)

;;; Global keybindings
(global-set-key (kbd "RET") 'newline-and-indent)
(global-set-key (kbd "C-x t") 'eshell)
(global-set-key (kbd "C-<tab>") 'completion-at-point)
(global-set-key (kbd "C-w") 'backward-kill-word)
(global-set-key (kbd "C-x C-k") 'kill-region)
(global-set-key (kbd "C-x w") 'other-window-back)
(global-set-key (kbd "<f13>") 'ignore)

;;; Helper modes
(global-auto-revert-mode)
(add-hook 'dired-mode-hook 'auto-revert-mode)
(if (version<= emacs-version "26.1.0")
    (global-linum-mode 1)
  (global-display-line-numbers-mode))

(use-package company
  :hook ((after-init . global-company-mode)
         (org-mode . (lambda () (company-mode -1)))))

;;; Spell-check
(when (eq system-type 'windows-nt)
  (setq ispell-program-name "C:/msys64/mingw64/bin/hunspell.exe")
  (setenv "LANG" "en_US"))

(use-package ripgrep
  :defer t
  :hook (ripgrep-search-mode . visual-line-mode)
  :bind (:map global-map
         ("C-c g" . ripgrep-regexp)))

(use-package magit :defer t)

;;; C Formatting
(setq c-basic-offset 8)

(c-add-style "myc++"
             '("bsd"
               (indent-tabs-mode . nil)
               (c-basic-offset . 4)))

(add-hook 'c-mode-hook
          (lambda ()
            (c-set-style "linux")))

(add-hook 'c++-mode-hook
          (lambda ()
            (c-set-style "myc++")
            (setq c-indent-tabs-mode nil
                  c-basic-offset 4)))


;;; Mail
;; LOLNO: requires "meson" to build
;; (use-package mu4e)

;;; General lispy stuffs
(use-package paredit
  :hook ((lisp-mode . paredit-mode)
         (emacs-lisp-mode . paredit-mode)
         (lisp-interaction-mode . paredit-mode)
         (scheme-mode . paredit-mode))
  :bind (:map paredit-mode-map
              ("C-w" . paredit-backward-kill-word)))

(define-key emacs-lisp-mode-map (kbd "C-c C-b") 'eval-buffer)
(define-key emacs-lisp-mode-map (kbd "C-c C-l") 'load-file)
(define-key global-map (kbd "C-c e d") 'toggle-debug-on-error)

;;; SLIME setup
(use-package slime
  :bind (:map slime-mode-map
              ("C-<tab>" . slime-complete-symbol)
              :map slime-editing-map
              ("C-<tab>" . slime-complete-symbol))
  :init
  (setq-default inferior-lisp-program "sbcl")
  (setq-default slime-contribs '(slime-fancy slime-asdf slime-scratch slime-mrepl)))

;;; Hexl Mode
(add-hook 'hexl-mode-hook
          (lambda ()
            (define-key hexl-mode-map (kbd "M-i") 'hexl-insert-hex-string)))

;;; Scheme setup
(use-package geiser :defer t)

;;; Haskell setup
(use-package hindent
  :defer t
  :hook (haskell-mode . hindent-mode))

(use-package haskell-mode
  :defer t
  :bind (:map haskell-mode-map
              ("C-c C-l" . 'haskell-process-load-or-reload)
              ("C-`" . 'haskell-interactive-bring)
              ("C-c C-t" . 'haskell-process-do-type)
              ("C-c C-i" . 'haskell-process-do-info)
              ("C-c C-c" . 'haskell-process-cabal-build)
              ("C-c C-k" . 'haskell-interactive-mode-clear)
              ("C-c c" . 'haskell-process-cabal)
              ("M-." . 'haskell-mode-jump-to-def)))

;; SHM requires an executable which isn't built with
;; straight.el. Figure that out next time I'm writing haskell.

;; (use-package shm
;;   :hook (haskell-mode . structured-haskell-mode)
;;   :bind (:map shm-map ("C-w" . shm/backward-kill-word)))

;;; Go Mode
(use-package go-mode
  :defer t
  :hook ((before-save . gofmt-before-save)
         (go-mode . (lambda () (setq tab-width 4)))))


;;; ERC
(use-package erc
  :defer t
  :init
  (setq erc-nick "robgssp"))

;;; Python Mode
(setq python-python-command "python3")
(eval-after-load "python"
  '(define-key python-mode-map (kbd "C-c C-e") 'python-send-defun))

;;; Lua Mode
(use-package lua-mode :defer t)

;;; HTML
(defun unhtml (start end)
  (interactive "r")
  (save-excursion
    (save-restriction
      (narrow-to-region start end)
      (goto-char (point-min))
      (replace-string "&" "&amp;")
      (goto-char (point-min))
      (replace-string "<" "&lt;")
      (goto-char (point-min))
      (replace-string ">" "&gt;"))))

;;; Jabber.el
(setq jabber-account-list
      '(("robgssp@gmail.com"
         (:network-server . "talk.google.com")
         (:connection-type . ssl))))

;;; Julia
(use-package julia-mode :defer t)
(use-package julia-repl
  :hook (julia-mode . julia-repl-mode))

;;; Supercollider
;; ERROR package found nowhere
;; (autoload 'sclang-start "sclang" nil t)
;; (autoload 'sclang-mode "sclang" nil t)
;; (add-to-list 'auto-mode-alist '("\\.\\(sc\\|scd\\)$" . sclang-mode))

;;; Erlang
;; Disabled unless/until I actually want it: it downloads all of OTP, ~800M.
;; (use-package erlang :defer t)

;;; J
(use-package j-mode :defer t)

;;; forth
(use-package forth-mode :defer t)

;;; Maxima Mode
(use-package maxima
  :commands (maxima imaxima maxima-mode imath-mode)
  :mode ("\\.ma[cx]\\'" . maxima-mode)
  :init
  (setq imaxima-use-maxima-mode-flag t))

;;; Prolog stuff
;; mode comes with Emacs
(setq prolog-system 'swi)
(add-to-list 'auto-mode-alist '("\\.pr$" . prolog-mode))

;;; Clojure
(use-package cider :defer t
  :hook (cider-mode . cider-turn-on-eldoc-mode))
(use-package clojure-mode :defer t)

(use-package rainbow-delimiters
  :hook (clojure-mode . rainbow-delimiters-mode))

;;; Sisal
;; I have no memory of this place
;; (add-to-list 'load-path "~/build/sisal/sisal-14.1.0/sisalmode")
;; (add-to-list 'auto-mode-alist '("\\.sis\\'" . sisal-mode))
;; (autoload 'sisal-mode "sisal-mode" nil t)

;;; SML
(use-package sml-mode
  :defer t
  :init
  (setq-default sml-indent-level 3))

;;: Javascript
(setq-default js-indent-level 2)
(add-to-list 'auto-mode-alist '("\\.ts\\'" . js-mode))

;;; Org mode
(use-package org
  :defer t
  :hook (org-mode
         . (lambda ()
             (auto-fill-mode -1)
             (visual-line-mode 1)
             (org-indent-mode 1)
             (set (make-local-variable 'auto-save-visited-mode) t)
             (setq buffer-file-coding-system 'utf-8-unix)))
  :config
  (dolist (backend '(beamer md man))
    (push backend org-export-backends))

  (add-to-list 'org-tags-exclude-from-inheritance "project")

  (require 'org-id))

(auto-save-visited-mode 1)
(setq-default auto-save-visited-mode nil)

(use-package org-superstar
  :hook  (org-mode . org-superstar-mode))

(use-package org-roam
  :defer t
  :bind (:map global-map
         ("C-c l" . org-store-link)
         ("C-c a" . org-agenda)
         ("C-c c" . org-capture)
         ("C-c n n" . org-roam-node-find)
         ("C-c n l" . org-roam-node-insert)
         ("C-c n i" . org-id-store-link)
         ("C-c n b" . org-roam-buffer-toggle)
         ("C-c d y" . org-roam-dailies-goto-yesterday)
         ("C-c d d" . org-roam-dailies-goto-today)
         ("C-c d t" . org-roam-dailies-goto-tomorrow)
         ("C-c d c" . org-roam-dailies-goto-date)
         ("C-c d n" . org-roam-dailies-goto-next-note)
         ("C-c d p" . org-roam-dailies-goto-previous-note)
         :map minibuffer-local-completion-map
         ("SPC" . self-insert-command)
         :map org-mode-map
         ("C-c n f a" . org-anki-sync-entry)
         ("C-c n f d" . org-anki-delete-entry)
         ("C-c n f u" . org-anki-update-all))
  :custom
  (org-roam-directory (if (equal system-type 'windows-nt)
                          "c:/Users/robgs/Documents/org"
                        "/home/robert/Documents/org"))
  (org-roam-dailies-directory "journals/")
  (org-roam-capture-templates
   '(("d" "default" plain
      "%?" :target
      (file+head "pages/${slug}.org" "#+title: ${title}\n")
      :unnarrowed t)))
  ;; (org-capture-templates
  ;;  '(("n" "note" entry (file "pages/${slug}.org")
  ;;     :unnarrowed t)))
  (org-startup-folded 'nofold)

  :init

  ;; Hack: logseq sticks backup files in the roam directory. Filter
  ;; them out of org's file list.
  ;; (defun rob/filter-org-files (fn &rest args)
  ;;   (cl-remove-if (lambda (file)
  ;;                   (string-match "logseq/bak" file))
  ;;                 (apply fn args)))

  ;; (advice-add 'org-roam--list-files :around #'rob/filter-org-files)

  (setq org-roam-db-node-include-function
        (lambda ()
          (not (string-match "logseq/bak" (buffer-file-name)))))

  :config
  (org-roam-db-autosync-mode)
  (defalias 'org-font-lock-ensure 'font-lock-ensure)
  (setq org-id-link-to-org-use-id t)
  (add-to-list 'display-buffer-alist
               '("\\*org-roam\\*"
                  (display-buffer-in-direction)
                  (direction . right)
                  (window-width . 0.33)
                  (window-height . fit-window-to-buffer))))

;; todo tracking machinery. Copied from https://d12frosted.io/posts/2021-01-16-task-management-with-roam-vol5.html

(use-package vulpea
  :defer t
  :commands (vulpea-buffer-tags-get vulpea-buffer-tags-set))

(defun rob/project-p ()
  (org-element-map
      (org-element-parse-buffer 'headline)
      'headline
    (lambda (h) (eq (org-element-property :todo-type h)
                    'todo))
    nil 'first-match))

(add-hook 'find-file-hook #'rob/project-update-tag)
(add-hook 'before-save-hook #'rob/project-update-tag)

(defun rob/project-update-tag ()
  (when (and (not (active-minibuffer-window))
             (rob/org-note-p))
    (save-excursion
      (goto-char (point-min))
      (let* ((tags (vulpea-buffer-tags-get))
             (original-tags tags))
        (if (rob/project-p)
            (setq tags (cons "project" tags))
          (setq tags (remove "project" tags)))

        (setq tags (seq-uniq tags))

        (when (or (seq-difference tags original-tags)
                  (seq-difference original-tags tags))
          (apply #'vulpea-buffer-tags-set tags))))))

(defun rob/org-note-p ()
  (and buffer-file-name
       (boundp 'org-roam-directory)
       (string-prefix-p (expand-file-name (file-name-as-directory org-roam-directory))
                        (file-name-directory buffer-file-name))
       (string-suffix-p ".org" buffer-file-name)))

(defun rob/project-files ()
  "Return a list of note files containing the 'project' tag"
  (seq-uniq
   (seq-map
    #'car
    (org-roam-db-query
     [:select [nodes:file]
              :from tags
              :left-join nodes
              :on (= tags:node-id nodes:id)
              :where (like tag (quote "%\"project\"%"))]))))

(defun rob/agenda-files-update (&rest _)
  (setq org-agenda-files (sort (rob/project-files) 'string<)))

(advice-add 'org-agenda :before #'rob/agenda-files-update)
(advice-add 'org-agenda :before #'rob/agenda-files-update)

(use-package org-anki
  :defer t)

;;; Nix
(use-package nix-mode :defer t)

;;; Rust
(use-package rust-mode :defer t)

;;; Powershell
(use-package powershell :defer t)

(use-package lsp-mode
  :hook (rust-mode . lsp))

;;; Gnus
(use-package gnus :defer t)
(use-package hydra)

(setq send-mail-function 'smtpmail-send-it
      smtpmail-smtp-server "smtp.gmail.com"
      smtpmail-smtp-service 587
      smtpmail-stream-type 'starttls
      user-full-name "Rob Glossop"
      user-mail-address "robgssp@gmail.com")

;;; Terraform
(use-package terraform-mode :defer t)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(canlock-password "a3979c726470bbc6fec6c7f21c32906a234548b7")
 '(custom-safe-themes
   '("6198e96f1fd7de3889a1b6ab8be1fc9b7c734cc9db0b0f16b635a2974601f977" "8db4b03b9ae654d4a57804286eb3e332725c84d7cdab38463cb6b97d5762ad26" default))
 '(erc-modules
   '(autojoin button completion fill irccontrols list match menu move-to-prompt netsplit networks noncommands readonly ring stamp track))
 '(geiser-repl-read-only-prompt-p nil)
 '(hindent-style "chris-done")
 '(idris-interpreter-path "~/.local/bin/idris")
 '(inhibit-startup-screen t)
 '(safe-local-variable-values
   '((elisp-lint-indent-specs
      (describe . 1)
      (it . 1)
      (thread-first . 0)
      (cl-flet . 1)
      (cl-flet* . 1)
      (org-element-map . defun)
      (org-roam-dolist-with-progress . 2)
      (org-roam-with-temp-buffer . 1)
      (org-with-point-at . 1)
      (magit-insert-section . defun)
      (magit-section-case . 0)
      (org-roam-with-file . 2))
     (elisp-lint-ignored-validators "byte-compile" "package-lint")
     (eval c-set-offset 'inlambda 0)
     (eval c-set-offset 'access-label '-)
     (eval c-set-offset 'substatement-open 0)
     (eval c-set-offset 'arglist-cont-nonempty '+)
     (eval c-set-offset 'arglist-cont 0)
     (eval c-set-offset 'arglist-intro '+)
     (eval c-set-offset 'inline-open 0)
     (eval c-set-offset 'defun-open 0)
     (eval c-set-offset 'innamespace 0)
     (indicate-empty-lines . t)
     (c-block-comment-prefix . "  ")))
 '(show-trailing-whitespace t)
 '(vc-follow-symlinks t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
(put 'set-goal-column 'disabled nil)

(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)

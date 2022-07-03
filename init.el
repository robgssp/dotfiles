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
(setq next-screen-context-lines 7)

;;; Color scheme
;; (add-to-list 'custom-theme-load-path "~/build/emacs-color-theme-solarized")
;; (setq frame-background-mode 'dark)
;; (load-theme 'solarized t)
;; (enable-theme 'solarized)

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
(global-set-key (kbd "C-c l") #'org-store-link)
(global-set-key (kbd "C-c a") #'org-agenda)
(global-set-key (kbd "C-c c") #'org-capture)


;;; Helper modes
(global-auto-revert-mode)
(global-linum-mode 1)

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
(use-package erlang :defer t)

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

;;; Org mode
;; (use-package org-roam)
(eval-after-load "org-mode"
  '(progn
     (dolist (backend '(beamer md man))
       (push backend org-export-backends))

     (require 'org-id)
     (org-roam-db-autosync-mode)
     (setq org-roam-directory (file-truename "~/org-roam-test"))))

;;; Nix
(use-package nix-mode :defer t)

;;; Rust
(use-package rust-mode :defer t)

(use-package lsp-mode
  :hook (rust-mode . lsp))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default default default italic underline success warning error])
 '(ansi-color-names-vector
   ["#2d3743" "#ff4242" "#74af68" "#dbdb95" "#34cae2" "#008b8b" "#00ede1" "#e1e1e0"])
 '(canlock-password "a3979c726470bbc6fec6c7f21c32906a234548b7")
 '(custom-safe-themes
   '("8db4b03b9ae654d4a57804286eb3e332725c84d7cdab38463cb6b97d5762ad26" default))
 '(erc-modules
   '(autojoin button completion fill irccontrols list match menu move-to-prompt netsplit networks noncommands readonly ring stamp track))
 '(geiser-repl-read-only-prompt-p nil)
 '(hindent-style "chris-done")
 '(idris-interpreter-path "~/.local/bin/idris")
 '(inhibit-startup-screen t)
 '(show-trailing-whitespace t)
 '(smtpmail-smtp-server "smtp.gmail.com")
 '(smtpmail-smtp-service 25)
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
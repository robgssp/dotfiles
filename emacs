; -*- emacs-lisp -*-
(require 'cl-lib)

;;; Error handling for the terminally lazy.
;;; This ends up wrapping quite a lot since I want to copy this file
;;; to workstations with old-ass underfeatured emacs.
(defmacro try (&rest body)
  `(condition-case ex
       (progn ,@body)
     ('error (message "%s failed." '(progn ,@body)))))

(defmacro prr (form)
  `(progn (message "%s" ',form) ,form))

(defmacro prall (&rest body)
  `(progn ,@(mapcar (lambda (f) `(prr ,f)) body)))

;;; Packages
(try (require 'package)
     (setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                              ("melpa" . "http://melpa.org/packages/")))
     (package-initialize))

;;; Fonts
(setq inhibit-startup-screen t)

(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))

(setq read-file-name-completion-ignore-case t)

;;; Color scheme
;; (add-to-list 'custom-theme-load-path "~/build/emacs-color-theme-solarized")
;; (setq frame-background-mode 'dark)
;; (load-theme 'solarized t)
;; (enable-theme 'solarized)

;;; Fixed tabs
(setq-default indent-tabs-mode nil)
(setq-default tab-width 8)
(setq c-default-style "linux"
      c-basic-offset 8)

;; (smart-tabs-insinuate 'c 'javascript)

(c-add-style "myc++"
             '("bsd"
               (indent-tabs-mode . nil)
               (c-basic-offset . 4)))

(add-hook 'c++-mode-hook
          (lambda ()
            (c-set-style "myc++")
            (setq c-indent-tabs-mode nil
                  c-basic-offset 4)))

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


;;; Global keybindings

(define-key global-map (kbd "RET") 'newline-and-indent)
(define-key global-map (kbd "C-x t") 'eshell)
(define-key global-map [C-tab] 'completion-at-point)
(define-key global-map (kbd "C-w") 'backward-kill-word)
(define-key global-map (kbd "C-x C-k") 'kill-region)
(define-key global-map (kbd "C-x w") 'other-window-back)
(global-set-key (kbd "C-c l") #'org-store-link)
(global-set-key (kbd "C-c a") #'org-agenda)
(global-set-key (kbd "C-c c") #'org-capture)

;;; Completion
(ivy-mode)

;;; Helper modes
(global-auto-revert-mode)

;;; Mail
(autoload 'mu4e "mu4e" nil t)

(add-to-list 'load-path "~/emacs")

;;; General lispy stuffs

(try
 (add-hook 'paredit-mode-hook
           (lambda ()
             (define-key paredit-mode-map (kbd "C-w") 'paredit-backward-kill-word))))

(add-hook 'lisp-mode-hook
	  (lambda ()
	    (setq indent-tabs-mode nil)
	    (try (paredit-mode +1))))

(add-hook 'emacs-lisp-mode-hook
	  (lambda ()
	    (setq indent-tabs-mode nil)
	    (try (paredit-mode +1))))

(add-hook 'lisp-interaction-mode-hook (lambda () (try (paredit-mode +1))))
(add-hook 'scheme-mode-hook           (lambda () (try (paredit-mode +1))))

;;; SLIME setup
(setq-default inferior-lisp-program "sbcl")
(setq-default slime-contribs '(slime-fancy slime-asdf slime-scratch slime-mrepl))
(autoload 'slime "slime" nil t)
(autoload 'slime-connect "slime" nil t)
(autoload 'slime-mode "slime" nil t)
(eval-after-load "slime"
  '(progn (define-key slime-mode-map [C-tab] 'slime-complete-symbol)
          (define-key slime-editing-map [C-tab] 'slime-complete-symbol)))

;;; Hexl Mode
(add-hook 'hexl-mode-hook
          (lambda ()
            (define-key hexl-mode-map (kbd "M-i") 'hexl-insert-hex-string)))

;;; Scheme setup
(setq-default scheme-program-name "scheme48")
(add-to-list 'auto-mode-alist '("\\.t$" . scheme-mode))
(eval-after-load "geiser-impl"
  '(add-to-list 'geiser-implementations-alist '((regexp "\\.t$") guile)))
;; Now handled with packager

(add-to-list 'load-path "~/build/scheme48/scheme48-1.9/emacs")
(autoload 'scheme-mode "cmuscheme48" nil t)

;;; Haskell setup
(add-hook 'haskell-mode-hook 'structured-haskell-mode)
(add-hook 'haskell-mode-hook 'hindent-mode)
(eval-after-load "haskell-mode"
  '(progn (define-key haskell-mode-map (kbd "C-c C-l") 'haskell-process-load-or-reload)
          (define-key haskell-mode-map (kbd "C-`") 'haskell-interactive-bring)
          (define-key haskell-mode-map (kbd "C-c C-t") 'haskell-process-do-type)
          (define-key haskell-mode-map (kbd "C-c C-i") 'haskell-process-do-info)
          (define-key haskell-mode-map (kbd "C-c C-c") 'haskell-process-cabal-build)
          (define-key haskell-mode-map (kbd "C-c C-k") 'haskell-interactive-mode-clear)
          (define-key haskell-mode-map (kbd "C-c c") 'haskell-process-cabal)
          (define-key haskell-mode-map (kbd "SPC") 'haskell-mode-contextual-space)
          (define-key haskell-mode-map (kbd "M-.") 'haskell-mode-jump-to-def)))

(eval-after-load "shm"
  '(progn (define-key shm-map (kbd "C-w") 'shm/backward-kill-word)
          ;; (set-face-background 'shm-current-face "#eee8d5")
          ;; (set-face-background 'shm-quarantine-face "lemonchiffon")
          ))

;;; Go Mode
(add-hook 'go-mode-hook
          '(lambda ()
             (setq c-basic-offset 4
                   indent-tabs-mode nil)
             (add-hook 'before-save-hook 'gofmt-before-save)))

;;; Window size
;; (dolist (i '((width . 180)
;;              (height . 50)))
;;   (add-to-list 'default-frame-alist i))

;;; misc
(setq next-screen-context-lines 7)

;;; Pianobar mode
(add-to-list 'load-path "/home/robert/build/pianobar.el")
(autoload 'pianobar "pianobar.el" nil t)
(put 'narrow-to-region 'disabled nil)

;; (setq gnus-ignored-newsgroups "^to\\.\\|^[0-9. ]+\\( \\|$\\)\\|^[\"]\"[#'()]")

;;; Text mode tweaks
;;(setq-default fill-column 80)
(add-hook 'text-mode-hook 'turn-on-auto-fill)

;;; ERC
(autoload 'erc "erc" nil t)
(setq erc-nick "robgssp")

;;; Python Mode
(setq python-python-command "python3")
(autoload 'python-mode "python" nil t)
(autoload 'run-python "python" nil t)
(eval-after-load "python"
  '(define-key python-mode-map (kbd "C-c C-e") 'python-send-defun))

;;; Lua Mode
(add-to-list 'load-path "~/build/emacs-lua/lua-mode")
(autoload 'lua-mode "lua-mode" nil t)

;;; D mode
(autoload 'd-mode "d-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.d\\'" . d-mode))

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
(add-hook 'julia-mode-hook 'julia-repl-mode)

;;; Supercollider
(autoload 'sclang-start "sclang" nil t)
(autoload 'sclang-mode "sclang" nil t)
(add-to-list 'auto-mode-alist '("\\.\\(sc\\|scd\\)$" . sclang-mode))

;;; Erlang
(add-to-list 'load-path "/usr/lib/erlang/lib/tools-2.6.11/emacs/")
(autoload 'erlang-mode "erlang-start" nil t)
(autoload 'run-erlang "erlang-start" nil t)

;;; J
(setq j-path "/home/robert/build/j/j64-701/bin")
(setq j-command "jconsole")
(autoload 'j-mode "j-mode" nil t)
(autoload 'j-shell "j-mode" nil t)

;;; forth
(add-to-list 'load-path "/home/robert/build/gforth/gforth-0.7.2")
(add-to-list 'auto-mode-alist '("\\.fr$" . forth-mode))
(autoload 'forth-mode "gforth" nil t)
(autoload 'run-forth "gforth" nil t)

;;; Maxima Mode
(add-to-list 'load-path "/usr/share/maxima/5.34.0/emacs/")
(autoload 'maxima-mode "maxima" "Maxima mode" t)
(autoload 'imaxima "imaxima" "Frontend for maxima with Image support" t)
(autoload 'maxima "maxima" "Maxima interaction" t)
(autoload 'imath-mode "imath" "Imath mode for math formula input" t)
(setq imaxima-use-maxima-mode-flag t)

;;; Prolog stuff
(setq prolog-system 'swi)
(add-to-list 'auto-mode-alist '("\\.pr$" . prolog-mode))

;;; Clojure
(add-hook 'cider-mode-hook 'cider-turn-on-eldoc-mode)
(add-hook 'clojure-mode-hook 'rainbow-delimiters-mode)

;;; Sisal
(add-to-list 'load-path "~/build/sisal/sisal-14.1.0/sisalmode")
(add-to-list 'auto-mode-alist '("\\.sis$" . sisal-mode))
(autoload 'sisal-mode "sisal-mode" nil t)

;;; SML
(add-to-list 'auto-mode-alist '("\\.fun$" . sml-mode))
(setq-default sml-indent-level 3)
(add-hook 'sml-mode-hook
          (lambda ()
            (setq indent-tabs-mode nil)))

;;; Org mode
(eval-after-load "org-mode"
  '(progn (require 'ox)
          (require 'ox-beamer)
          (require 'org-id)))

(setq org-roam-directory (file-truename "~/org-roam-test"))
(org-roam-db-autosync-mode)

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
 '(package-selected-packages
   '(magit counsel ivy slime org-roam julia-mode julia-repl rainbow-delimiters cider clojure-mode erlang yaml-mode solarized-theme sml-mode scad-mode paredit nix-mode lua-mode haskell-mode))
 '(show-trailing-whitespace t)
 '(smtpmail-smtp-server "smtp.gmail.com")
 '(smtpmail-smtp-service 25))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
(put 'set-goal-column 'disabled nil)

(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)

; -*-  mode: emacs-lisp; lexical-binding: t;  -*-

(profiler-start 'cpu+mem)
(message "Loading early-init.el!")
(setq full-init-start-time (float-time))
(setq package-quickstart t)

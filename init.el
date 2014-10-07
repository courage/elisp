;;----------------------------------------------------------------------
;; detect system type
(defvar terminal-p (not (display-graphic-p)))

;;----------------------------------------------------------------------
;; Utility functions

(require 'cl)

(defun mrc/directory-join (&rest path-elements)
  "Join together a set of path element strings with the platform path separator."
  (reduce '(lambda (directory file)
             (concat (file-name-as-directory directory) file))
          path-elements))

(defun mrc/relative-directory-join (&rest path-elements)
  "Joins together path elements, relative to the current script's path."
  (apply #'mrc/directory-join
         (cons (file-name-directory (or load-file-name buffer-file-name))
               path-elements)))

;; Functions for making nice balanced columns in a frame.
(load-file (mrc/relative-directory-join "balanced-frame-splits.el"))

;;----------------------------------------------------------------------
;; Configuration and Minor Modes

;; Remove annoying space consumers
(tool-bar-mode -1)
(scroll-bar-mode -1)

;; Stop beeping at me
(setq visible-bell t)

;; Help me count parens
(show-paren-mode 1)

;; Who should display in a special frame? No one.
(setq special-display-regexps nil)

;; create the autosave dir if necessary, since emacs won't.
(make-directory "~/.emacs.d/autosaves/" t)

;; always revert files that change outside the editor
(global-auto-revert-mode t)

;; display column numbers
(column-number-mode)

;; Tabs
(setq-default indent-tabs-mode nil)

;; make the mouse work in the terminal
(when terminal-p
  (require 'mouse)
  (xterm-mouse-mode t)
  (defun track-mouse (e)))

;; Easier buffer disambiguation
(require 'uniquify)
(setq uniquify-buffer-name-style 'reverse)

;; interactively do things (and icomplete)
(require 'ido)
(ido-mode t)
(setq ido-enable-flex-matching t)
(setq ido-use-filename-at-point 'guess)
(icomplete-mode 1)

;; org-mode
(setq org-todo-keywords '("TODO" "STARTED" "WAITING" "DONE"))

;;----------------------------------------------------------------------
;; Third party modes

(add-to-list 'load-path
             (mrc/relative-directory-join "third_party" "find-things-fast"))
(require 'find-things-fast)

(add-to-list 'load-path
             (mrc/relative-directory-join "third_party" "go-mode"))
(require 'go-mode)

(add-to-list 'load-path
             (mrc/relative-directory-join "third_party" "undo-tree"))
(require 'undo-tree)
(global-undo-tree-mode)

;;----------------------------------------------------------------------
;; Mode hooks

(defun mrc/c-mode-common-hook ()
  (with-ftf-project-root
   (setq ff-always-try-to-create nil
         ff-search-directories (list "." default-directory))))
(add-hook 'c-mode-common-hook 'mrc/c-mode-common-hook)

(defun mrc/emacs-lisp-mode-hook ()
  (ftf-add-filetypes '("*.el" "*.elisp")))
(add-hook 'emacs-lisp-mode-hook 'mrc/emacs-lisp-mode-hook)

(defun mrc/go-mode-hook ()
  (setq tab-width 4
        indent-tabs-mode t)
  (if (executable-find "go")
      (add-hook 'before-save-hook 'gofmt-before-save)))
(add-hook 'go-mode-hook 'mrc/go-mode-hook)

;;----------------------------------------------------------------------
;; shortcuts to common stuff
(defun mrc/find-init ()
  (interactive)
  (find-file "~/elisp/init.el"))
(defun mrc/find-notes ()
  (interactive)
  (find-file "~/notes/notes.org"))
(defun mrc/find-organizer ()
  (interactive)
  (find-file "~/notes/todo.org"))

;;----------------------------------------------------------------------
;; key bindings

(global-set-key (kbd "C-c 3") 'split-into-two-columns-and-cycle)
(global-set-key (kbd "C-c C-3") 'split-into-two-columns-and-follow)
(global-set-key (kbd "C-c 4") 'split-into-three-columns-and-cycle)
(global-set-key (kbd "C-c C-4") 'split-into-three-columns-and-follow)

(global-set-key (kbd "C-c o") 'ff-find-related-file)

(global-set-key (kbd "C-c p g") 'ftf-grepsource)
(global-set-key (kbd "C-c p f") 'ftf-find-file)

(global-set-key (kbd "<M-s-left>") 'windmove-left)
(global-set-key (kbd "<M-s-right>") 'windmove-right)
(global-set-key (kbd "<M-s-up>") 'windmove-up)
(global-set-key (kbd "<M-s-down>") 'windmove-down)

(global-set-key (kbd "S-<f12>") 'mrc/find-notes)

(global-set-key (kbd "<f11>") 'mrc/find-init)
(global-set-key (kbd "<f12>") 'mrc/find-organizer)

;;----------------------------------------------------------------------
;; Custom variables (don't make more than one of these).

(custom-set-variables
  '(auto-save-file-name-transforms '((".*" "~/.emacs.d/autosaves/\\1" t)))
  '(backup-directory-alist '((".*" . "~/.emacs.d/backups/")))
  '(cr-flymake-ninja-build-file "out/Debug/build.ninja"))

;;----------------------------------------------------------------------
;; Local machine specific set up

(let ((local-el (mrc/relative-directory-join "local" "local.el")))
  (if (file-readable-p local-el)
      (load-file local-el)))

(message "Init to win it.")


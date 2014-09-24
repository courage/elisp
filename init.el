;;----------------------------------------------------------------------
;; detect system type
(defvar terminal-p (not (display-graphic-p)))

;;----------------------------------------------------------------------
;; Frame and window management

(defun select-next-window ()
  "Switch to the next window"
  (interactive)
  (select-window (next-window)))

(defun select-previous-window ()
  "Switch to the previous window"
  (interactive)
  (select-window (previous-window)))

(defun split-into-two-columns ()
  "Split the frame into two columns"
  (interactive)
  (delete-other-windows)
  (split-window-horizontally))

(defun split-into-two-columns-and-cycle ()
  "Split the frame into two columns and select the next buffer in the second window."
  (interactive)
  (split-into-two-columns)
  (select-next-window)
  (next-buffer)
  (select-next-window))

(defun split-into-two-columns-and-follow ()
  "Split the frame into two columns and enable follow mode."
  (interactive)
  (split-into-two-columns)
  (follow-mode 1))

(defun split-into-three-columns ()
  "Split the frame into three columns"
  (interactive)
  (delete-other-windows)
  (split-window-horizontally)
  (split-window-horizontally)
  (balance-windows))

(defun split-into-three-columns-and-cycle ()
  "Split the frame into three columns and select new buffers in the other windows. The current window
will be in the middle of the new layout."
  (interactive)
  (split-into-three-columns)
  (next-buffer)
  (select-next-window)
  (select-next-window)
  (next-buffer)
  (next-buffer)
  (select-next-window)
  (select-next-window))

(defun split-into-three-columns-and-follow ()
  "Split the frame into three columns and turn on follow mode."
  (interactive)
  (split-into-three-columns)
  (follow-mode 1))

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

;;----------------------------------------------------------------------
;; key bindings

(global-set-key (kbd "C-c 3") 'split-into-two-columns-and-cycle)
(global-set-key (kbd "C-c C-3") 'split-into-two-columns-and-follow)
(global-set-key (kbd "C-c 4") 'split-into-three-columns-and-cycle)
(global-set-key (kbd "C-c C-4") 'split-into-three-columns-and-follow)

;;----------------------------------------------------------------------
;; Custom variables (don't make more than one of these).

(custom-set-variables
  '(auto-save-file-name-transforms '((".*" "~/.emacs.d/autosaves/\\1" t)))
  '(backup-directory-alist '((".*" . "~/.emacs.d/backups/")))
  '(cr-flymake-ninja-build-file "out/Debug/build.ninja"))

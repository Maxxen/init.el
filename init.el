
;; Add custom theme path
;;(let ((basedir "~/.emacs.d/themes/"))
;;  (dolist (f (directory-files basedir))
;;    (if (and (not (or (equal f ".") (equal f "..")))
;;	     (file-directory-p (concat basedir f)))
;;	(add-to-list 'custom-theme-load-path (concat basedir f)))))

;;; Code:

;; Disable GUI elements
(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)

;; Lock-files are not really neccessary
(setq create-lockfiles nil)

;; Remove startup screen
(setq inhibit-startup-message t)

;; Change Yes/No to y/n
(defalias 'yes-or-no-p 'y-or-n-p)

;; Fix scrolling
(setq scroll-conservatively 100)

;; Stop at subwords when jumping with C/M-<arrow>
(global-subword-mode t)

;; Prevent mini-buffer from getting stuck if focus is lost
(defun stop-using-minibuffer ()
  "Kill the minibuffer."
  (when (and (>= (recursion-depth) 1) (active-minibuffer-window))
    (abort-recursive-edit)))

(add-hook 'mouse-leave-buffer-hook 'stop-using-minibuffer)


;; Tab width
(setq-default tab-width 4)

;; Electric pair mode
;; electric pairs
(setq electirc-pair-pairs '(
			    (?\( . ?\))
			    (?\{ . ?\})
			    (?\[ . ?\])
			    (?\" . ?\")
			    ))
(electric-pair-mode t)

;; Remove bell sound
(setq ring-bell-function 'ignore)

;; Global highlight
;;(when window-system (global-hl-line-mode t))

;; Prettify symbols (optional)
(when window-system (global-prettify-symbols-mode t))

;; Set up bash/terminal/shell shortcut
(defvar my-term-shell "/bin/bash")
;; advice is function to execute before/after/while another
;; in this case we hook into ansi-term and interactively push our
;; shell (my-term-shell) path as arguments, thus always launching bash
(advice-add 'ansi-term :before (lambda (&rest r) (interactive (list my-term-shell))))

;; IDO mode
(setq ido-enable-flex-matching nil)
(setq ido-create-new-buffer 'always)
(setq ido-everywhere t)
(ido-mode 1)

;; Fix buffers
(global-set-key (kbd "C-x C-b") 'ido-switch-buffer)
(global-set-key (kbd "C-x b") 'ibuffer) ;; Replace crappy buffer-list with ibuffer
(setq ibuffer-expret t) ;; I dont make mistakes
(defun kill-current-buffer ()
  (interactive)
  (kill-buffer (current-buffer)))
(global-set-key (kbd "C-x k") 'kill-current-buffer) ;; Always kill current buffer

;; Fix window switching
(defun split-and-follow-h ()
  (interactive)
  (split-window-below)
  (balance-windows)
  (other-window 1))
(global-set-key (kbd "C-x 2") 'split-and-follow-h)

(defun split-and-follow-v ()
  (interactive)
  (split-window-right)
  (balance-windows)
  (other-window 1))
(global-set-key (kbd "C-x 3") 'split-and-follow-v)

;; Backups
;; (setq make-backup-file nil)
;; (setq auto-save-default nil)

(setq backup-directory-alist
      `(("." . ,(expand-file-name
                 (concat user-emacs-directory "file-backups")))))

;; Allow 10 backups, warn if backing up file of size > 1000000
(setq delete-old-versions t
      backup-by-copying   t
      kept-new-versions   20
      kept-old-versions   10
      version-control     t
      large-file-warning-threshold   1000000)

(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

;; Save cutomize settings in different file
(setq custom-file "~/.emacs.d/customizations.el")

;; Set up MELPA
(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives
	     '("melpa" . "https://melpa.org/packages/"))
(package-initialize)


;; Theme
(use-package vscode-dark-plus-theme
  :config
  (load-theme 'vscode-dark-plus t))


;; C-style

(setq c-default-style "stroustrup"
	  c-basic-offset 4)

;; Set up use-package
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))


;; Org-mode tweaks
(setq org-confirm-babel-evaluate nil
      org-src-fontify-natively t
      org-src-tab-acts-natively t
      org-support-shift-select t)

(org-babel-do-load-languages 'org-babel-load-languages '((C . t)
							 (python . t)))

(global-set-key (kbd "\C-c a") 'org-agenda)
(setq org-agenda-dim-blocked-tasks t)
(setq org-agenda-files '("~/org/organizer.org" "~/org/notes/"))
(setq org-todo-keywords
  '((sequence "TODO(t)" "IN-PROGRESS(i)" "WAITING(w)" "|" "DONE(d)" "CANCELED(c)")))

(setq org-todo-keyword-faces
      '(("IN-PROGRESS" . org-warning) ("WAITING" . org-warning)))

(setq org-enforce-todo-dependencies t)
(setq org-enforce-todo-checkbox-dependencies t)
(setq org-completion-use-ido t)

(global-set-key (kbd "C-c o") (lambda () (interactive) (find-file "~/org/organizer.org")))

(global-set-key (kbd "\C-c c") 'org-capture)

;; Create a new file for notes
(defun note/generate-org-note-name ()
  (setq note--name (read-string "Name: "))
  (setq note--time (format-time-string "%Y-%m-%d"))
  (setq note--filename
	(expand-file-name (format "%s-%s.org"
				  note--time
				  (replace-regexp-in-string " " "-" (downcase note--name)))
			  "~/org/notes/")))


;; Delete note file if aborted!
(defun note/delete-note-on-abort ()
  (let ((key (plist-get org-capture-plist :key)))
    (if (and org-note-abort (equal key "n"))
	(delete-file note--filename))))

(add-hook 'org-capture-after-finalize-hook 'note/delete-note-on-abort)

(setq org-default-notes-file "~/org/organizer.org")
(setq org-capture-templates
      '(("t" "TODO" entry
	 (file+headline "~/org/organizer.org" "Tasks")
	 "* TODO %?\n  %i\n  %a")
	("n" "Note" plain 
	 (file note/generate-org-note-name)
	 "#+TITLE: %((lambda () note--name))\n#+CATEGORY: Notes\n \n%U\n\n* %((lambda () note--name)) %^G\n\n%?")))

(use-package org-cliplink
  :ensure t
  :config (global-set-key (kbd "C-c p") 'org-cliplink))

(use-package org-download
  :ensure t
  :config (setq-default org-download-image-dir "~/org/images/")
  :hook (dired-mode . org-download-enable))

(use-package org-ref
  :ensure t
  :config
  (setq reftex-default-bibliography '("~/org/bibliography/references.bib"))
  (setq org-ref-bibliography-notes "~/org/bibliography/notes.org"
	org-ref-default-bibliography '("~/org/bibliography/references.bib")
	org-ref-pdf-directory "/org/bibliography/bibtex-pdfs/"))

;; Which key
;; - Shows C-x - completions in minibuffer
(use-package which-key
  :ensure t
  :init
  (which-key-mode))

;; Hungry deletion
(use-package hungry-delete
  :ensure t
  :config (global-hungry-delete-mode))

;; Kill ring popup
(use-package popup-kill-ring
  :ensure t
  :bind ("M-y" . popup-kill-ring))

;; Company mode
(use-package company
  :ensure t
  :init
  (add-hook 'after-init-hook 'global-company-mode)
  :config ;; Optionally set delay
  (setq company-idle-delay 0)
  (setq company-minimum-prefix-length 3))

(use-package company-quickhelp
  :ensure t
  :init
  (company-quickhelp-mode 1)
  (use-package pos-tip
    :ensure t))


;; Diminish minor modes
;; - Hides selected minor modes from the modeline
(use-package diminish
  :ensure t
  :config
  (diminish 'company-mode)
  (diminish 'hungry-delete-mode)
  (diminish 'which-key-mode))

;; ESS mode
;; - Emacs Speak Statistics, R integration
(use-package ess
  :ensure t)

;; Flycheck
(use-package flycheck
  :ensure t
  :init (global-flycheck-mode)
  :config (setq flycheck-check-syntax-automatically '(mode-enabled save)))


(add-hook 'c++-mode-hook (lambda () (setq flycheck-gcc-language-standard "c++11")))
(add-hook 'c++-mode-hook (lambda () (setq flycheck-clang-language-standard "c++11")))

;; Language Server Protocol
(use-package lsp-mode
  :ensure t
  :commands lsp
  :config (setq lsp-enable-snippet nil))

(use-package lsp-ui
  :ensure t)

(use-package company-lsp
  :ensure t
  :init (push 'company-lsp company-backends)
  :config (setq company-lsp-async t)
  :commands company-lsp)

;; Rust support
(use-package toml-mode
  :ensure t)

(use-package rust-mode
  :ensure t
  :hook (rust-mode . lsp)
  :config (define-key rust-mode-map (kbd "TAB") #'company-indent-or-complete-common))

;; Add keybindings for interacting with Cargo
(use-package cargo
  :ensure t
  :hook (rust-mode . cargo-minor-mode))

(use-package flycheck-rust
  :ensure t
  :config (add-hook 'flycheck-mode-hook #'flycheck-rust-setup))


;; Tramp settings
(setq tramp-default-method "ssh")
(customize-set-variable 'tramp-syntax 'simplified)


(use-package irony
  :ensure t
  :hook ((c++-mode . irony-mode)
	 (c-mode . irony-mode)
	 (irony-mode . irony-cdb-autosetup-compile-options)))

(add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options)

(use-package company-irony
  :ensure t
  :config (add-to-list 'company-backends 'company-irony))



;; Web-mode and typescript
(use-package web-mode
  :ensure t
  :mode (("\\.html?\\'" . web-mode)
		 ("\\.php\\'" . web-mode)
		 ("\\.scss\\'" . web-mode)
		 ("\\.tsx\\'" . web-mode)
		 ("\\.js\\'" . web-mode))
		 ;;("\\.jsx\\'" . web-mode))
  :config
  (setq web-mode-markup-indent-offset 2
        web-mode-css-indent-offset 2
        web-mode-code-indent-offset 2
        web-mode-block-padding 2
        web-mode-comment-style 2

        web-mode-enable-css-colorization t
        web-mode-enable-auto-pairing t
        web-mode-enable-comment-keywords t
        web-mode-enable-current-element-highlight t

		;;web-mode-content-types-alist '(("jsx" . "\\.js[x]?\\'"))
		))

(use-package typescript-mode
  :ensure t
  :config
  (setq typescript-indent-level 2)
  (setq indent-tabs-mode nil))

(use-package tide
  :ensure t
  :after (typescript-mode company flycheck)
  :hook ((typescript-mode . tide-hl-identifier-mode)
         (before-save . tide-format-before-save)))

(use-package rjsx-mode
  :ensure t
  :config
  (setq js-indent-level 2)
  (setq indent-tabs-mode nil))

;; Add newline and indent when expanding html tags
(defun tag-expand ()
  (interactive)
  (if (and
       (looking-at "[ \t]*<")
       (looking-back ">[ \t]*"))
      (progn (newline-and-indent)
             (save-excursion (newline-and-indent))
             (indent-according-to-mode))
    (newline-and-indent)))

(add-hook 'rjsx-mode-hook (lambda () (local-set-key (kbd "RET") 'tag-expand)))


;; Zig mode
(use-package zig-mode
  :ensure t)


;; Tex
(setq TeX-parse-self t)
(setq TeX-auto-save t)
(setq-default TeX-master nil)


;; Load saved customizations from custom file
(load-file custom-file)

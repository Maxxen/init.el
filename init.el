

;; Add custom theme path
;;(let ((basedir "~/.emacs.d/themes/"))
;;  (dolist (f (directory-files basedir))
;;    (if (and (not (or (equal f ".") (equal f "..")))
;;	     (file-directory-p (concat basedir f)))
;;	(add-to-list 'custom-theme-load-path (concat basedir f)))))

;; Disable GUI elements
(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)

;; Remove startup screen
(setq inhibit-startup-message t)

;; Change Yes/No to y/n
(defalias 'yes-or-no-p 'y-or-n-p)

;; Fix scrolling
(setq scroll-conservatively 100)

;; Stop at subwords when jumping with C/M-<arrow>
(global-subword-mode t)

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




;; Set up MELPA
(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives
	     '("melpa" . "https://melpa.org/packages/"))
(package-initialize)

;; Set up use-package
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))


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
  :init (global-flycheck-mode))


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


;; Typescript support
(use-package tide
  :ensure t
  :after (typescript-mode company flycheck)
  :hook ((typescript-mode . tide-setup)
         (typescript-mode . tide-hl-identifier-mode)
         (before-save . tide-format-before-save)))




;; CUSTOM SETTINGS
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default default default italic underline success warning error])
 '(ansi-color-names-vector
   ["#666666" "#FFFFFF" "#EEEEEE" "#ffffff" "#ffffff" "#FFFFFF" "#ffffff" "#ffffff"])
 '(custom-enabled-themes (quote (cloud)))
 '(custom-safe-themes
   (quote
    ("2bd0a595cbed0e18370692aeb9f8953b522bd2eeb9bdcb14800d0eeb15d93378" "96998f6f11ef9f551b427b8853d947a7857ea5a578c75aa9c4e7c73fe04d10b4" "c48551a5fb7b9fc019bf3f61ebf14cf7c9cdca79bcb2a4219195371c02268f11" "987b709680284a5858d5fe7e4e428463a20dfabe0a6f2a6146b3b8c7c529f08b" "e0d42a58c84161a0744ceab595370cbe290949968ab62273aed6212df0ea94b4" "3cd28471e80be3bd2657ca3f03fbb2884ab669662271794360866ab60b6cb6e6" "3cc2385c39257fed66238921602d8104d8fd6266ad88a006d0a4325336f5ee02" "e9776d12e4ccb722a2a732c6e80423331bcb93f02e089ba2a4b02e85de1cf00e" "72a81c54c97b9e5efcc3ea214382615649ebb539cb4f2fe3a46cd12af72c7607" "58c6711a3b568437bab07a30385d34aacf64156cc5137ea20e799984f4227265" "3d5ef3d7ed58c9ad321f05360ad8a6b24585b9c49abcee67bdcbb0fe583a6950" "b3775ba758e7d31f3bb849e7c9e48ff60929a792961a2d536edec8f68c671ca5" "9b59e147dbbde5e638ea1cde5ec0a358d5f269d27bd2b893a0947c4a867e14c1" default)))
 '(linum-format " %7i ")
 '(package-selected-packages
   (quote
    (python-docstring sublime-themes lsp-ui flycheck company-lsp lsp-mode ess diminish irony which-key use-package popup-kill-ring hungry-delete eziam-theme company cloud-theme)))
 '(python-shell-interpreter "python3"))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:foundry "ADBO" :family "Source Code Pro")))))

;;'(ido-subdir ((t (:foreground "#2f7e9d")))))


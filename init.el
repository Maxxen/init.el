
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

(use-package company-irony
  :ensure t
  :config (add-to-list 'company-backends 'company-irony))


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
 '(custom-enabled-themes (quote (MaxTheme)))
 '(custom-safe-themes
   (quote
    ("e37ef8296550b527c7cd6f13ce88a25b58d206924fc680b73bec7cc996420585" "d5634d5f2e8b06801d9a3be2ed08bc1bee95546d8bc50dbf3dfad30d7802d96a" "872a25a9326f5066c67f8ee1f7e0aa5b932c3ff128b8c290bf04a78e89ffb963" "a47f5eb62c3a7193559798cc981e7c6fcd9cf6c96af205671ffe9a9e774d285f" "6c2c98c4797f3a87d3a2783c46c1877faae9b116721718681501cdcde52f760c" "f5b9de1144360bafc88595178f6480a22d9ee3d0d9eb41d383bcf6affd1530f4" "ff0dc94cab3ac00f4b6cd22ae62a3d216e86c61ac3ff9e34b13a7c8bee72098a" "0c79be728e39722a0b444222c19995a87b43b8b11163e21ecc795e750c4f316d" "3b58b1384dd2f2aa0fdc34c619b194fe0323c1c3e63a5154239b4b2e6801331f" "c0b33c319c84f6c63de4fbce20ce9e3c6f16a3ad94f4845100965ae179eccc57" "01aefa0d4deaaa831bf02dba362777e18b380c27cc8ac4cc92fa261fe2faaf9f" "8d16081b05758c4e1989bc51f6206be6a81e2f96b22997e050695f2fa86141bd" "81dec9ef071efd73bb9228380d23fe40412709c4bd5efda85fcdd26c4c12d3d0" "4cd322f9d82836d6bf8d311347a9c3b436b26efa721d03c4511379e7f7489fc3" "ac6802f2349921e97dcbab144d5ebfa7293612b04ff0c00c670cfb0e0b9d1bb8" "5f4794a8d69691ce8128341fc49595d6eafeb11dbf2131f178b5ffe791f73094" "f5759b2a9a28c724edbc8c8f5a3a8bd86b6603e2e1ee5e90470338bb2121bcfc" "67c86b00e53278feaedd24cc12896272ff1ad9b5a43213e031abdb5b417c05e1" "f37c3f4400ac802548e12f818a4c8a13e407fe29d06e011eca0e4cb586475ce6" "dfd53a86c92dc4380115f19e6ace1ab687a09f829d829819b536de2f6c60c73d" "0c8f1637942294ad767e46d695fe9b43323ce60dd0d4a0d74ec5b182c0a93564" "1787003541ba3554d0150fcb23ebe4b2f5f34ff26f0dc1a5c207fc38baa44a4b" "aa0de247bfbf68bdad7bcf3fa0de965b7a0c99b07174bf1c9b692d52c3bbc405" "d8f73e0be57dd930fcdefae67f3615c52393904638eac1c083528992acaf2f45" "a62b75bffb2a24ace6c71ca6f46196d261844c674f41eb1fae19eaf422817d80" "2bd0a595cbed0e18370692aeb9f8953b522bd2eeb9bdcb14800d0eeb15d93378" "96998f6f11ef9f551b427b8853d947a7857ea5a578c75aa9c4e7c73fe04d10b4" "c48551a5fb7b9fc019bf3f61ebf14cf7c9cdca79bcb2a4219195371c02268f11" "987b709680284a5858d5fe7e4e428463a20dfabe0a6f2a6146b3b8c7c529f08b" "e0d42a58c84161a0744ceab595370cbe290949968ab62273aed6212df0ea94b4" "3cd28471e80be3bd2657ca3f03fbb2884ab669662271794360866ab60b6cb6e6" "3cc2385c39257fed66238921602d8104d8fd6266ad88a006d0a4325336f5ee02" "e9776d12e4ccb722a2a732c6e80423331bcb93f02e089ba2a4b02e85de1cf00e" "72a81c54c97b9e5efcc3ea214382615649ebb539cb4f2fe3a46cd12af72c7607" "58c6711a3b568437bab07a30385d34aacf64156cc5137ea20e799984f4227265" "3d5ef3d7ed58c9ad321f05360ad8a6b24585b9c49abcee67bdcbb0fe583a6950" "b3775ba758e7d31f3bb849e7c9e48ff60929a792961a2d536edec8f68c671ca5" "9b59e147dbbde5e638ea1cde5ec0a358d5f269d27bd2b893a0947c4a867e14c1" default)))
 '(linum-format " %7i ")
 '(package-selected-packages
   (quote
    (company-irony jinja2-mode web-mode json-mode rust-mode toml-mode cargo flycheck-rust python-docstring sublime-themes lsp-ui flycheck company-lsp lsp-mode ess diminish irony which-key use-package popup-kill-ring hungry-delete eziam-theme company cloud-theme)))
 '(python-shell-interpreter "python3"))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "Inconsolata")))))

;;'(ido-subdir ((t (:foreground "#2f7e9d")))))


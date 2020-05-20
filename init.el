;; package --- Summary
;;; Code:
;;; Commentary:

(setq inhibit-startup-screen t)

(set-face-foreground 'minibuffer-prompt "green")

(autoload 'ibuffer "ibuffer" "List buffers." t)

(set-terminal-coding-system 'utf-8)

(show-paren-mode)

;; MELPA
(require 'package)
(let* ((no-ssl (and (memq system-type '(windows-nt ms-dos))
                    (not (gnutls-available-p))))
       (proto (if no-ssl "http" "https")))
  ;; Comment/uncomment these two lines to enable/disable MELPA and MELPA Stable as desired
  (add-to-list 'package-archives (cons "melpa" (concat proto "://melpa.org/packages/")) t)
  (add-to-list 'package-archives '("org" . "https://orgmode.org/elpa/") t)
  ;; (add-to-list 'package-archives (cons "melpa-stable" (concat proto "://stable.melpa.org/packages/")) t)
  (when (< emacs-major-version 24)
    ;; For important compatibility libraries like cl-lib
    (add-to-list 'package-archives (cons "gnu" (concat proto "://elpa.gnu.org/packages/")))))

;; Some combination of GNU TLS and Emacs fail to retrieve archive
;; contents over https.
;; https://www.reddit.com/r/emacs/comments/cdei4p/failed_to_download_gnu_archive_bad_request/etw48ux
;; https://debbugs.gnu.org/cgi/bugreport.cgi?bug=34341

(if (and (version< emacs-version "26.3") (>= libgnutls-version 30604))
    (setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3"))


(package-initialize)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("76c5b2592c62f6b48923c00f97f74bcb7ddb741618283bdb2be35f3c0e1030e3" "84890723510d225c45aaff941a7e201606a48b973f0121cb9bcb0b9399be8cba" default)))
 '(org-modules
   (quote
    (org-bbdb org-bibtex org-docview org-gnus org-info org-irc org-mhe org-rmail org-w3m)))
 '(package-selected-packages
   (quote
    (rustic js2-mode use-package tree-mode ace-window dap-mode helm-lsp lsp-treemacs company-lsp lsp-ui lsp-mode jedi pyenv-mode-auto pyenv-mode highlight-indent-guides slime restclient rainbow-delimiters persp-mode elscreen-fr elscreen plantuml-mode htmlize org exec-path-from-shell json-navigator flycheck whole-line-or-region magit imenu-list ibuffer-projectile zenburn-theme))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(lsp-ui-doc-background ((t (:background nil))))
 '(lsp-ui-doc-header ((t (:inherit (font-lock-string-face italic))))))

;; package install zenburn-theme
(load-theme 'zenburn)
(enable-theme 'zenburn)
(set-face-attribute 'default nil :height 140)

(menu-bar-mode -1)

(if window-system
    (tool-bar-mode -1)
  )

(add-hook 'prog-mode-hook 'linum-mode)

(add-hook 'ibuffer-mode-hook (lambda () (ibuffer-auto-mode 1)))

;; install ibuffer-projectile
(add-hook 'ibuffer-hook
    (lambda ()
      (ibuffer-projectile-set-filter-groups)
      (unless (eq ibuffer-sorting-mode 'alphabetic)
        (ibuffer-do-sort-by-alphabetic))))

;; ibuffer
(global-set-key (kbd "C-x C-b") 'ibuffer)

;; kill whole line
;; package install whole-line-or-region
(add-hook 'prog-mode-hook 'whole-line-or-region-global-mode)

;; package install imenu-list
(global-set-key (kbd "C-c C-o") 'imenu-list-smart-toggle)

;; comment and uncomment region
(global-set-key (kbd "C-c c") 'comment-region)
(global-set-key (kbd "C-c u") 'uncomment-region)

;; diff between buffer and file
(global-set-key (kbd "C-c C-d") 'diff-buffer-with-file)

;; Magit
(setq vc-handled-backends nil)
(global-auto-revert-mode t)
(magit-auto-revert-mode)
(global-set-key (kbd "C-x g") 'magit-status)
(global-set-key (kbd "C-x M-g") 'magit-dispatch-popup)
(global-magit-file-mode)

;; scrolling
(defun window-half-height ()
  (max 1 (/ (1- (window-height (selected-window))) 2)))

(defun scroll-up-half ()
  (interactive)
  (scroll-up (window-half-height)))

(defun scroll-down-half ()
  (interactive)
  (scroll-down (window-half-height)))

(global-set-key (kbd "C-v") 'scroll-up-half)
(global-set-key (kbd "C-b") 'scroll-down-half)

;; Search file with set root location
;; M-x find-name-dired: you will be prompted for a root directory and a filename pattern.
;; Press t to "toggle mark" for all files found.
;; Press Q for "Query-Replace in Files...": you will be prompted for query/substitution regexps.
;; Proceed as with query-replace-regexp: SPACE to replace and move to next match, n to skip a match, etc.
;; Press C-x s to save buffers. (You can then press y, n or ! to save all at once)
(global-set-key (kbd "C-f") 'find-name-dired)


;; moving point
;; M-r runs the command move-to-window-line-top-bottom
;; With no argument, positions point at center of window.
;; Successive calls position point at positions defined
;; by "recenter-positions"

;; C-l current line to center
;; C-u 5 C-l current line to almost top
;; C-u 0 C-l current line to top
;; M-> to end of buffer
;; M-< to top of buffer
;; C-x C-SPC to last place
;; C-u C-SPC
;; Move cursor to previous marked position of current buffer.
;; C-c >, C-c < selection indent
;; to turn off GNU screen flow control, C-z :flow off

;; C-M-f     forward-list  Move forward over a parenthetical group
;; C-M-b     backward-list  Move backward over a parenthetical group

;; to paste The easiest way with emacs24 is:
;;
;; M-x electric-indent-mode RET
;; That disables auto indentation.
;;
;; Paste your thing.
;;
;; renable
;;
;; M-x electric-indent-mode RET
;; Or just M-x UP-Arrow ;-)

;; Search Word Under Cursor
;; While in isearch, press C-w
;; to select more strings to the right of cursor.

;; Column enter
;; goto first line, first column
;; C-SPC
;; goto last line (first column)
;; C-x r t
;; and type

;; Column delete
;; C-x r k
;; https://www.gnu.org/software/emacs/manual/html_node/emacs/Rectangles.html

;; check current buffer file commit log
;; M-x magic-log-buffer-file

;; find and replace after cursor
;; M-x query-replace


(add-hook 'after-init-hook #'global-flycheck-mode)
(setq flycheck-check-syntax-automatically '(save))

(setq js-indent-level 2)

;; https://github.com/DamienCassou/json-navigator

;; https://github.com/purcell/exec-path-from-shell
(when (memq window-system '(mac ns x))
  (exec-path-from-shell-initialize))

;; default shell in M-x term
(setq-default explicit-shell-file-name "/usr/local/bin/zsh")

;; org mode
(org-babel-do-load-languages
 'org-babel-load-languages
 '((plantuml . t)
   (python . t)))

;; for syntax highlight
;; install htmlize
(setq org-src-fontify-natively t
    org-src-tab-acts-natively t
    org-confirm-babel-evaluate nil
    org-edit-src-content-indentation 0)

(setq org-plantuml-jar-path
      (expand-file-name "~/plantuml.jar"))

;; install plantuml-mode
;; Enable plantuml-mode for PlantUML files
(add-to-list 'auto-mode-alist '("\\.plantuml\\'" . plantuml-mode))

;; Elscreen
;; and
;; elscreen-fr
(setq elscreen-prefix-key "\C-t")
(elscreen-start)
(set-face-attribute 'elscreen-tab-background-face nil
		    :background "grey10"
                    :foreground "grey90")
(set-face-attribute 'elscreen-tab-control-face nil
		     :background "grey20"
                     :foreground "grey90")
(set-face-attribute 'elscreen-tab-current-screen-face nil
		    :background "grey20"
		    :foreground "grey90")
(set-face-attribute 'elscreen-tab-other-screen-face nil
		    :background "grey30"
                    :foreground "grey60")

;; https://github.com/bbatsov/projectile
(projectile-mode +1)
(define-key projectile-mode-map (kbd "C-p") 'projectile-command-map)

;; install perspective
(persp-mode)

;; https://irreal.org/blog/?p=1559
(cua-selection-mode 1)

(setq backup-directory-alist `(("." . "~/.saves")))
(global-linum-mode 1) ; always show line numbers

;; install restclient.el
;; Enable restclient-mode for .http files
(add-to-list 'auto-mode-alist '("\\.http\\'" . restclient-mode))


;; slime
;; Set your lisp system and, optionally, some contribs
(setq inferior-lisp-program "/usr/local/bin/sbcl")
(setq slime-contribs '(slime-fancy))

;; ediff
(defun ediff-copy-both-to-C ()
  (interactive)
  (ediff-copy-diff ediff-current-difference nil 'C nil
                   (concat
                    (ediff-get-region-contents ediff-current-difference 'A ediff-control-buffer)
                    (ediff-get-region-contents ediff-current-difference 'B ediff-control-buffer))))
(defun add-d-to-ediff-mode-map () (define-key ediff-mode-map "d" 'ediff-copy-both-to-C))
(add-hook 'ediff-keymap-setup-hook 'add-d-to-ediff-mode-map)

(setq ediff-window-setup-function 'ediff-setup-windows-plain)

;; Tell emacs where is your personal elisp lib dir
(add-to-list 'load-path "~/.emacs.d/lisp/")

(global-set-key (kbd "C-c e") 'end-of-buffer)
(global-set-key (kbd "C-c a") 'beginning-of-buffer)
(global-set-key (kbd "C-a") 'back-to-indentation)
(global-set-key (kbd "M-m") 'move-beginning-of-line)

;; install highlight-indent-guides
(add-hook 'prog-mode-hook 'highlight-indent-guides-mode)
(add-hook 'yaml-mode-hook 'highlight-indent-guides-mode)
(add-hook 'json-mode-hook 'highlight-indent-guides-mode)

;; shell
(setq explicit-shell-file-name "/bin/zsh")

;;
(setq show-trailing-whitespace t)

(defun delete-trailing-whitespace-save-buffer ()
  "Delete trailing whitespaces in buffer before saving."
  (interactive)
  (progn
    (delete-trailing-whitespace)
    (save-buffer)))

(global-set-key (kbd "C-x C-s") 'delete-trailing-whitespace-save-buffer)

;; install pyenv-mode-auto
(require 'pyenv-mode-auto)

(require 'pyenv-mode)

(defun projectile-pyenv-mode-set ()
  "Set pyenv version matching project name."
  (let ((project (projectile-project-name)))
    (if (member project (pyenv-mode-versions))
        (pyenv-mode-set project)
      (pyenv-mode-unset))))

(add-hook 'projectile-after-switch-project-hook 'projectile-pyenv-mode-set)


;; package install jedi
;; M-X jedi:install-server
(add-hook 'python-mode-hook 'jedi:setup)
(setq jedi:complete-on-dot t)
(setq jedi:tooltip-method nil)

(add-hook 'python-mode-hook
	  (lambda ()
	    (progn
	      (local-set-key
	       (kbd "M-.") 'jedi:goto-definition)
	      (local-set-key
	       (kbd "M-,") 'jedi:goto-definition-pop-marker)
	      (local-set-key
	       (kbd "C-c w") 'delete-trailing-whitespace))))

;;; lsp
(use-package lsp-mode
  :init (setq lsp-keymap-prefix "C-c l")
  :ensure t
  :commands lsp
  :custom
  (lsp-auto-guess-root nil)
  (lsp-prefer-flymake nil) ; Use flycheck instead of flymake
  :bind (:map lsp-mode-map ("C-c C-f" . lsp-format-buffer))
  :hook ((java-mode) . lsp)
         (lsp-mode . lsp-enable-which-key-integration))


(use-package lsp-ui
  :after lsp-mode
  :diminish
  :commands lsp-ui-mode
  :custom-face
  (lsp-ui-doc-background ((t (:background nil))))
  (lsp-ui-doc-header ((t (:inherit (font-lock-string-face italic)))))
  :bind (:map lsp-ui-mode-map
              ([remap xref-find-definitions] . lsp-ui-peek-find-definitions)
              ([remap xref-find-references] . lsp-ui-peek-find-references)
              ("C-c u" . lsp-ui-imenu))
  :custom
  (lsp-ui-doc-enable t)
  (lsp-ui-doc-header t)
  (lsp-ui-doc-include-signature t)
  (lsp-ui-doc-position 'top)
  (lsp-ui-doc-border (face-foreground 'default))
  (lsp-ui-sideline-enable nil)
  (lsp-ui-sideline-ignore-duplicate t)
  (lsp-ui-sideline-show-code-actions nil)
  :config
  ;; Use lsp-ui-doc-webkit only in GUI
  (setq lsp-ui-doc-use-webkit t)
  ;; WORKAROUND Hide mode-line of the lsp-ui-imenu buffer
  ;; https://github.com/emacs-lsp/lsp-ui/issues/243
  (defadvice lsp-ui-imenu (after hide-lsp-ui-imenu-mode-line activate)
    (setq mode-line-format nil)))

;; org-drill
(eval-after-load "org"
  '(progn
    (add-to-list 'load-path "~/.emacs.d/lisp/")
    (add-to-list 'load-path "~/.emacs.d/org-drill/")
    (require 'org-drill)))

;; rustic
(use-package rustic)
(when (string= system-type "darwin")
  (setq dired-use-ls-dired nil))
;;; .emacs ends here

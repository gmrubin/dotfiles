(setq ns-pop-up-frames nil)

;;; Emacs Load Path
(setq load-path (cons "~/.emacs.d" load-path))

(server-start)
(require 'edit-server)
(edit-server-start)

(menu-bar-mode 0)

;;; Package.el
(load "package")
(package-initialize)

;;; Pymacs
(autoload 'pymacs-apply "pymacs")
(autoload 'pymacs-call "pymacs")
(autoload 'pymacs-eval "pymacs" nil t)
(autoload 'pymacs-exec "pymacs" nil t)
(autoload 'pymacs-load "pymacs" nil t)

;;; User info
(setq user-mail-address "gregg.m.rubin@gmail.com")

;;; Tramp
(require 'tramp)
(setq tramp-default-method "scp")

;;; Default Mode
(setq default-major-mode 'text-mode)

;;; Completion
; (iswitchb-mode 1)
;(icomplete-mode 1)
;(eval-after-load "icomplete" '(progn (require 'icomplete+)))

;;; Key-bindings for next-buffer and prev-buffer
;(global-set-key (kbd "M-p") 'previous-buffer)
;(global-set-key (kbd "M-n") 'next-buffer)

;;; Aquamacs stuff
(setq mac-command-modifier 'meta)

;;; Theme
(load "color-theme")
(load "color-theme-twilight")
(if window-system
    (color-theme-twilight)
  (color-theme-charcoal-black))

(add-hook 'find-file-hook 'auto-revert-mode)

(defun copy-buffer ()
  (interactive)

  (set-window-buffer
   (get-lru-window)
   (window-buffer (selected-window)))

  (select-window (get-lru-window)))

(defun swap-buffers ()
  (interactive)

  (setq window-one (selected-window))
  (setq window-two (get-lru-window))

  (setq buffer-one (window-buffer window-one))
  (setq buffer-two (window-buffer window-two))

  (set-window-buffer window-one buffer-two)
  (set-window-buffer window-two buffer-one)

  (select-window window-two) )

(global-set-key "\C-x\C-b" 'copy-buffer)
(global-set-key "\C-x\C-v" 'swap-buffers)

;;; Block cursor
(setq default-cursor-type 'block)
(setq x-stretch-cursor t)

;;; Highlight current line
(global-hl-line-mode 1)

;;; Remove Menu Bar
(menu-bar-mode -1)

;;; Debug on error when loading emacs
(setq debug-on-error t)

;;; Line numbering
(setq column-number-mode t)
(setq line-number-mode t)

;;; Paren matching
(show-paren-mode t)

;;; CamelCase
;(add-hook 'find-file-hook
;          (lambda () (c-subword-mode 1)))

;;; Scrolling
(setq scroll-margin 2)
(setq scroll-step 1)

;;; Show trailing whitespace
(setq space-color "#562626")
(setq-default show-trailing-whitespace t)
(set-face-background 'trailing-whitespace space-color)

;;; Indent with 4 spaces
(setq standard-indent 4)
(setq c-basic-offset 4)
(setq-default indent-tabs-mode nil)

;;; Display tabs as 8 spaces wide
(setq tab-width 8)

;;; Blast bad whitespace on C-x t
(defun fix-whitespace nil
  (interactive)
  (delete-trailing-whitespace)
  (untabify (point-min) (point-max)))
(global-set-key (kbd "C-x t") 'fix-whitespace)

;;; Abbreviation Expansion
(global-set-key (kbd "M-/") 'hippie-expand)

;;; Snippets
(add-to-list 'load-path "~/.emacs.d/yasnippet-0.5.6")
(require 'yasnippet)
(yas/initialize)
(yas/load-directory "~/.emacs.d/yasnippet-0.5.6/snippets")

;;; Puppet Mode
(autoload 'puppet-mode "puppet-mode" "Major mode for editing puppet manifests")
(add-to-list 'auto-mode-alist '("\\.pp$" . puppet-mode))

;;; Groovy Mode
(autoload 'groovy-mode "groovy-mode" "Groovy editing mode." t)
(add-to-list 'auto-mode-alist '("\.groovy$" . groovy-mode))
(add-to-list 'interpreter-mode-alist '("groovy" . groovy-mode))

;;; Haskell Mode
(load "haskell-mode/haskell-site-file")
(add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
(add-hook 'haskell-mode-hook 'turn-on-haskell-indent)
(add-hook 'haskell-mode-hook 'font-lock-mode)
(add-hook 'haskell-mode-hook 'imenu-add-menubar-index)

;;; SML Mode
(load "sml-mode/sml-mode-startup")

;;; haXe Mode
(require 'haxe-mode)

;;; ActionScript Mode
(autoload 'actionscript-mode "actionscript-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.as$" . actionscript-mode))

;;; nXML Mode
(load "nxml-mode/rng-auto.el")
(setq auto-mode-alist
      (cons '("\\.\\(xml\\|xsl\\|rng\\|xhtml\\)\\'" . nxml-mode)
            auto-mode-alist))
(fset 'html-mode 'nxml-mode)

;;; HTML Mode
(add-to-list 'auto-mode-alist '("\\.html$" . html-mode))
(load "django-html-mode.el")
(add-to-list 'auto-mode-alist '("\\.djang10$" . django-html-mode))

(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(confluence-default-space-alist (list (cons confluence-url "DOCS")))
 '(confluence-url "http://mongodb.onconfluence.com/rpc/xmlrpc")
 '(frame-background-mode (quote dark))
 '(inhibit-startup-screen t)
 '(mode-require-final-newline (quote visit-save))
 '(require-final-newline (quote visit-save))
 '(tab-width 4)
 '(uniquify-buffer-name-style (quote reverse) nil (uniquify))
 '(uniquify-ignore-buffers-re nil)
 '(uniquify-separator "/"))
(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 )

;;; CONFLUENCE
(add-to-list 'load-path "~/.emacs.d/confluence-el")
(require 'confluence)

;; note, all customization must be in *one* custom-set-variables block



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; confluence editing support (with longlines mode)

(autoload 'confluence-get-page "confluence" nil t)

(eval-after-load "confluence"
  '(progn
     (require 'longlines)
     (progn
       (add-hook 'confluence-mode-hook 'longlines-mode)
       (add-hook 'confluence-before-save-hook 'longlines-before-revert-hook)
       (add-hook 'confluence-before-revert-hook 'longlines-before-revert-hook)
       (add-hook 'confluence-mode-hook '(lambda () (local-set-key "\C-j" 'confluence-newline-and-indent))))))

;; LongLines mode: http://www.emacswiki.org/emacs-en/LongLines
(autoload 'longlines-mode "longlines" "LongLines Mode." t)

(eval-after-load "longlines"
  '(progn
     (defvar longlines-mode-was-active nil)
     (make-variable-buffer-local 'longlines-mode-was-active)

     (defun longlines-suspend ()
       (if longlines-mode
           (progn
             (setq longlines-mode-was-active t)
             (longlines-mode 0))))

     (defun longlines-restore ()
       (if longlines-mode-was-active
           (progn
             (setq longlines-mode-was-active nil)
             (longlines-mode 1))))

     ;; longlines doesn't play well with ediff, so suspend it during diffs
     (defadvice ediff-make-temp-file (before make-temp-file-suspend-ll
                                             activate compile preactivate)
       "Suspend longlines when running ediff."
       (with-current-buffer (ad-get-arg 0)
         (longlines-suspend)))


     (add-hook 'ediff-cleanup-hook
               '(lambda ()
                  (dolist (tmp-buf (list ediff-buffer-A
                                         ediff-buffer-B
                                         ediff-buffer-C))
                    (if (buffer-live-p tmp-buf)
                        (with-current-buffer tmp-buf
                          (longlines-restore))))))))

;; keybindings (change to suit)

;; open confluence page
(global-set-key "\C-xwf" 'confluence-get-page)

;; setup confluence mode
(add-hook 'confluence-mode-hook
          '(lambda ()
             (local-set-key "\C-xw" confluence-prefix-map)))

;; added by gmrubin on 10-20-11 - setup compile from file as per http://www.cs.unc.edu/~gb/blog/2008/03/15/running-python-from-within-emacs/ 
(defun my-compile ()
  "Use compile to run python programs"
  (interactive)
  (compile (concat "python " (buffer-name))))
(setq compilation-scroll-output t)

(add-hook 'python-mode-hook
          '(lambda ()
             (local-set-key "\C-c\C-c" 'my-compile)))

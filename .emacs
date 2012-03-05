(setq ns-pop-up-frames nil)

;;; Emacs Load Path
(setq load-path (cons "~/.emacs.d" load-path))

;;; Try and prevent annoying pop-up when runnign multiple servers (GR)
(if (file-exists-p
 (concat (getenv "TMPDIR") "emacs"
         (number-to-string
          (user-real-uid)) "/server"))
nil (server-start))


;;;GR commented this section out because .emacs file was not loading when laptop was not connected to a monitor
;;;(server-start)
;;(require 'edit-server)
;;(edit-server-start)

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
  (color-theme-twilight))



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

;; COMPILE SHORTCUT - added by gmrubin on 10-20-11 - setup compile from file as per http://www.cs.unc.edu/~gb/blog/2008/03/15/running-python-from-within-emacs/ 
(defun my-compile ()
  "Use compile to run python programs"
  (interactive)
  (compile (concat "python " (buffer-name))))
(setq compilation-scroll-output t)

(add-hook 'python-mode-hook
          '(lambda ()
             (local-set-key "\C-c\C-c" 'my-compile)))

;; PHP MODE - added by gmrubin on 10-31-11
(load "php-mode")
(add-to-list 'auto-mode-alist
     	     '("\\.php[34]?\\'\\|\\.phtml\\'" . php-mode))

;;;-----------------------------------------------------------------------------------------------
;;; ansi-term stuff
;;; http://pages.physics.cornell.edu/~myers/teaching/ComputationalMethods/python/ipython.html

(require 'term)
(defun visit-ansi-term ()
  "If the current buffer is:
     1) a running ansi-term named *ansi-term*, rename it.
     2) a stopped ansi-term, kill it and create a new one.
     3) a non ansi-term, go to an already running ansi-term
        or start a new one while killing a defunt one"
  (interactive)
  (let ((is-term (string= "term-mode" major-mode))
        (is-running (term-check-proc (buffer-name)))
        (term-cmd "/bin/bash")
        (anon-term (get-buffer "*ansi-term*")))
    (if is-term
        (if is-running
            (if (string= "*ansi-term*" (buffer-name))
                (call-interactively 'rename-buffer)
              (if anon-term
                  (switch-to-buffer "*ansi-term*")
                (ansi-term term-cmd)))
          (kill-buffer (buffer-name))
          (ansi-term term-cmd))
      (if anon-term
          (if (term-check-proc "*ansi-term*")
              (switch-to-buffer "*ansi-term*")
            (kill-buffer "*ansi-term*")
            (ansi-term term-cmd))
        (ansi-term term-cmd)))))
(global-set-key (kbd "<f2>") 'visit-ansi-term)

;;;-----------------------------------------------------------------------------------------------
;;; ansi-color stuff
;;; http://tapoueh.org/blog/2011/07/29-emacs-ansi-colors.html

(require 'ansi-color)
  (setq ansi-color-names-vector
        (vector (frame-parameter nil 'background-color)
              "#8F9D6A" "#8ae234" "#edd400" "#729fcf"
              "#729FCF" "cyan3" "#8F9D6A")
        ansi-term-color-vector ansi-color-names-vector
        ansi-color-map (ansi-color-make-color-map))

;;;-----------------------------------------------------------------------------------------------
;;; mouse-wheel zoom
;;; http://stackoverflow.com/questions/5533110/emacs-zoom-in-zoom-out

(global-set-key [C-mouse-4] 'text-scale-increase)
(global-set-key [C-mouse-5] 'text-scale-decrease)

;;;-----------------------------------------------------------------------------------------------
;;; yaml-mode
;;; https://github.com/yoshiki/yaml-mode

(require 'yaml-mode)
(add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode))

;;;-----------------------------------------------------------------------------------------------
;;; haml-mode
;;; https://github.com/nex3/haml-mode

(add-to-list 'load-path "~/.emacs.d/haml-mode/")
(require 'haml-mode)


;;
;; packages
;;
(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t)
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
(package-initialize)

;; auto-settings
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default default default italic underline success warning error])
 '(ansi-color-names-vector
   ["black" "red3" "ForestGreen" "yellow3" "blue" "magenta3" "DeepSkyBlue" "gray50"])
 '(custom-enabled-themes (quote (madhat2r)))
 '(custom-safe-themes
   (quote
    ("0b7ee9bac81558c11000b65100f29b09488ff9182c083fb303c7f13fd0ec8d2b" default)))
 '(fci-rule-color "#151515")
 '(package-selected-packages
   (quote
    (elpy treemacs-projectile treemacs-evil treemacs ido-vertical-mode ido-completing-read+ smex ido-ubiquitous jazz-theme madhat2r-theme green-screen-theme helm-themes which-key yasnippet multiple-cursors expand-region dired-toggle dired-toggle-sudo paredit use-package undo-tree auto-complete markdown-preview-mode latex-math-preview ein rainbow-delimiters smartparens magit exec-path-from-shell markdown-mode matlab-mode pdf-tools auto-virtualenvwrapper virtualenvwrapper w3m mew yatex jedi multi-term)))
 '(safe-local-variable-values (quote ((tex-main-file . "report_19"))))
 '(send-mail-function (quote smtpmail-send-it))
 '(term-default-bg-color "#000000")
 '(term-default-fg-color "#dddd00"))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )


;; use-package
(require 'use-package)

;;
;; ssh
;;
(require 'tramp)
(setq tramp-default-method "ssh")


;;
;;mozc
;;
(require 'mozc)
(set-language-environment "Japanese")
(setq default-input-method "japanese-mozc")
(prefer-coding-system 'utf-8)

;;
;; windmove
(when (fboundp 'windmove-default-keybindings)
  (windmove-default-keybindings))
(global-set-key (kbd "C-c <left>") 'windmove-left)
(global-set-key (kbd "C-c <down>") 'windmove-down)
(global-set-key (kbd "C-c <up>") 'windmove-up)
(global-set-key (kbd "C-c <right>") 'windmove-right)
;;
;;jedi
;;
(require 'epc)
(require 'auto-complete-config)
(require 'python)

;;;;PYTHONPATH上のソースコードがauto-completeの補完対象になる;;;;;
(setenv "PYTHONPATH" "/usr/local/lib/python2.7/site-packages")
(require 'jedi)
(add-hook 'python-mode-hook 'jedi:setup)
(setq jedi:complete-on-dot t)

;;python modeのときはsmartparens起動
(add-hook 'python-mode-hook 'smartparens-mode)

;;key変更(mozc)
(global-set-key (kbd "C-j") 'toggle-input-method)

;;
;;Yatex
;;

;;yatex-modeの起動
(setq auto-mode-alist 
      (cons (cons "\\.tex$" 'yatex-mode) auto-mode-alist))
(autoload 'yatex-mode "yatex" "Yet Another LaTeX mode" t)
;; 野鳥が置いてある directry の load-path 設定
;; default で load-path が通っている場合は必要ありません

;(setq load-path
;      (cons (expand-file-name
;             "~/.emacs.d/elpa/yatex-20171030.502") load-path))

(add-to-list 'load-path "~/.emacs.d/elpa/yatex-20171030.502")
;; 文章作成時の日本語文字コード
;; 0: no-converion
;; 1: Shift JIS (windows & dos default)
;; 2: ISO-2022-JP (other default)
;; 3: EUC
;; 4: UTF-8
(setq YaTeX-kanji-code 4)

(setq tex-command "platex")

(setq bibtex-command "pbibtex")
(defvar YaTeX-dvi2-command-ext-alist
  '(("[agx]dvi\\|dviout\\|emacsclient" . ".dvi")
   ("ghostview\\|gv" . ".ps")
   ("acroread\\|pdf\\|Preview\\|TeXShop\\|Skim\\|evince\\|apvlv" . ".pdf")))
(setq dviprint-command-format "dvipdfmx -f ptex-ipaex.map %s")
;;reftex-mode
(add-hook 'yatex-mode-hook
          #'(lambda ()
              (reftex-mode 1)
              (define-key reftex-mode-map
                (concat YaTeX-prefix ">") 'YaTeX-comment-region)
              (define-key reftex-mode-map
                (concat YaTeX-prefix "<") 'YaTeX-uncomment-region)))
(setq reftex-default-bibliography '("/home/azumi/lab/progress_report/reference.bib"))

;;mail
;;(setq user-mail-address "ee47067@meiji.ac.jp")
;;(setq user-full-name "Azumi Ohno")
;;(setq smtpmail-smtp-server "smtp.office365.com")
;;(setq mail-user-agent 'message-user-agent)
;;(setq message-send-mail-function 'message-smtpmail-send-it)

;; Mew
(autoload 'mew "mew" nil t)
(autoload 'mew-send "mew" nil t)

;; Optional setup (Read Mail menu):
(setq read-mail-command 'mew)

;; Optional setup (e.g. C-xm for sending a message):
(autoload 'mew-user-agent-compose "mew" nil t)
(if (boundp 'mail-user-agent)
    (setq mail-user-agent 'mew-user-agent))
(if (fboundp 'define-mail-user-agent)
    (define-mail-user-agent
      'mew-user-agent
      'mew-user-agent-compose
      'mew-draft-send-message
      'mew-draft-kill
      'mew-send-hook))

(autoload 'mew "mew" nil t)
(autoload 'mew-send "mew" nil t)
(setq mew-fcc "+outbox") ; 送信メールを保存
(setq exec-path (cons "/usr/bin" exec-path))

;; eww
(setq eww-search-prefix "http://www.google.co.jp/search?q=")
(defun eww-mode-hook--rename-buffer ()
  "Rename eww browser's buffer so sites open in new page."
  (rename-buffer "eww" t))
(add-hook 'eww-mode-hook 'eww-mode-hook--rename-buffer)


(defvar eww-disable-colorize t)
(defun shr-colorize-region--disable (orig start end fg &optional bg &rest _)
  (unless eww-disable-colorize
    (funcall orig start end fg)))
(advice-add 'shr-colorize-region :around 'shr-colorize-region--disable)
(advice-add 'eww-colorize-region :around 'shr-colorize-region--disable)
(defun eww-disable-color ()
  "eww で文字色を反映させない"
  (interactive)
  (setq-local eww-disable-colorize t)
  (eww-reload))
(defun eww-enable-color ()
  "eww で文字色を反映させる"
  (interactive)
  (setq-local eww-disable-colorize nil)
  (eww-reload))

;;prolog mode
(autoload 'prolog-mode "prolog" "Major mode for editing Prolog programs." t)
(add-to-list 'auto-mode-alist '("\\.pl\\'" . prolog-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 行と桁の表示
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(line-number-mode t)
(column-number-mode t)
;; 選択範囲の情報表示
(defun count-lines-and-chars ()
  (if mark-active
      (format "[%3d:%4d]"
              (count-lines (region-beginning) (region-end))
              (- (region-end) (region-beginning)))
    ""))
(add-to-list 'default-mode-line-format
             '(:eval (count-lines-and-chars)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;日本語の印刷
(setq ps-multibyte-buffer 'non-latin-printer)
(require 'ps-mule)
(defalias 'ps-mule-header-string-charsets 'ignore)


;; (require 'pdf-preview)
;; (setq pdf-preview-preview-command "evince")
;;multi-term
(add-to-list 'load-path "~/.emacs.d/elpa")
(require 'multi-term)

;pdf-toolsに関する設定
(require 'pdf-tools)
(require 'pdf-annot) 
(require 'pdf-history) 
(require 'pdf-info) 
(require 'pdf-isearch) 
(require 'pdf-links) 
(require 'pdf-misc) 
(require 'pdf-occur) 
(require 'pdf-outline) 
(require 'pdf-sync) 
(require 'tablist-filter)
(require 'tablist)
(add-to-list 'auto-mode-alist (cons "\\.pdf$" 'pdf-view-mode))

(require 'linum)
(global-linum-mode)
(defcustom linum-disabled-modes-list '(doc-view-mode pdf-view-mode)
  "* List of modes disabled when global linum mode is on"
  :type '(repeat (sexp :tag "Major mode"))
  :tag " Major modes where linum is disabled: "
  :group 'linum
  )
(defcustom linum-disable-starred-buffers 't
  "* Disable buffers that have stars in them like *Gnu Emacs*"
  :type 'boolean
  :group 'linum)
(defun linum-on ()
  "* When linum is running globally, disable line number in modes defined in `linum-disabled-modes-list'. Changed by linum-off. Also turns off numbering in starred modes like *scratch*"
  (unless (or (minibufferp) (member major-mode linum-disabled-modes-list)
          (and linum-disable-starred-buffers (string-match "*" (buffer-name)))
          )
    (linum-mode 1)))
(provide 'setup-linum)

(add-hook 'pdf-view-mode-hook
  (lambda ()
    (pdf-misc-size-indication-minor-mode)
    (pdf-links-minor-mode)
    (pdf-isearch-minor-mode)
  )
)
;; magit
(global-set-key (kbd "C-x g") 'magit-status)

;;for csv2latextable
;; for csv2latextable
(defun csv2latextable ()
  (interactive)
  (insert
   (shell-command-to-string
    (concat "python /home/azumi/tools/csv2latextable/csv2latextable.py "
	    "\""
	    (read-file-name "input csv file: ")
	    "\" "
	    (if (y-or-n-p "No header mode? : ")
		"--noheader" nil)))))


;; ein: emacs IPython notebook
(require 'ein)

;; ein: latex preview
(add-to-list 'load-path "~/.emacs.d/elpa/ein-preview-latex")
;; (require 'ein-preview-latex)


;; markdown preview
(setq markdown-command "pandoc")

;; capslock → ctrl
(global-set-key (kbd "<eisu-toggle>") 'control)


;; paredit settings
(use-package paredit
  :hook ((emacs-lisp-mode . enable-paredit-mode)
	 (lisp-interaction-mode . enable-paredit-mode)
	 (lisp-mode . enable-paredit-mode)
	 (ielm-mode . enable-paredit-mode)))
(show-paren-mode 1)
;;; undo-tree
(use-package undo-tree
  :init
  (global-undo-tree-mode t)
  :bind
  ("M-/" . undo-tree-redo))

;;; multiple-cursors
(use-package multiple-curosors
  :bind
  (("C-S-c C-S-c" . mc/edit-lines)
   ("C->" . mc/mark-next-like-this)
   ("C-<" . mc/mark-next-previous-like-this)
   ("C-c C->" . mc/mark-all-like-this)))

;;; around dired
(use-package wdired
  :hook (dired-load . wdired)
  :bind (:map dired-mode-map
	      ("r" . wdired-change-to-wdired-mode))
  :config (setq dired-dwin-target t))

;;; expand-region
(use-package expand-region
  :bind
  (("C-@" . er/expand-region) 
   ("C-M-@" . er/contract-region))
  :init
  (transient-mark-mode t))


;;; lisp eval-and-replace
(defun eval-and-replace ()
  "Replace the preceding sexp with its value."
  (interactive)
  (backward-kill-sexp)
  (condition-case nil
      (prin1 (eval (read (current-kill 0)))
             (current-buffer))
    (error (message "Invalid expression")
	   (insert (current-kill 0)))))

(global-set-key (kbd "C-c C-r") 'eval-and-replace)

;;; which-key config
(which-key-setup-side-window-bottom)
(which-key-mode 1)


;;; ido settings
(ido-mode 1)
(ido-everywhere 1)
(setq ido-enable-flex-matching t)	;あいまい一致

(require 'ido-completing-read+)
(ido-ubiquitous-mode 1)

(use-package smex
  :bind
  (("M-x" . smex)
   ("M-X" . smex-major-mode-commands)))

(setq ido-max-window-height 0.75)	; idoが使うwindowの高さを設定
(ido-vertical-mode 1)
(setq ido-vertical-define-keys 'C-n-and-C-p-only)


;;; treemacs settings
(use-package treemacs
  :ensure t
  :defer t
  :init
  (with-eval-after-load 'winum
    (define-key winum-keymap (kbd "M-0") #'treemacs-select-window))
  :config
  (progn
    (setq treemacs-collapse-dirs              (if (executable-find "python") 3 0)
          treemacs-file-event-delay           5000
          treemacs-follow-after-init          t
          treemacs-follow-recenter-distance   0.1
          treemacs-goto-tag-strategy          'refetch-index
          treemacs-indentation                2
          treemacs-indentation-string         " "
          treemacs-is-never-other-window      nil
          treemacs-no-png-images              nil
          treemacs-project-follow-cleanup     nil
          treemacs-persist-file               (expand-file-name ".cache/treemacs-persist" user-emacs-directory)
          treemacs-recenter-after-file-follow nil
          treemacs-recenter-after-tag-follow  nil
          treemacs-show-hidden-files          t
          treemacs-silent-filewatch           nil
          treemacs-silent-refresh             nil
          treemacs-sorting                    'alphabetic-desc
          treemacs-space-between-root-nodes   t
          treemacs-tag-follow-cleanup         t
          treemacs-tag-follow-delay           1.5
          treemacs-width                      35)

    ;; The default width and height of the icons is 22 pixels. If you are
    ;; using a Hi-DPI display, uncomment this to double the icon size.
    ;;(treemacs-resize-icons 44)

    (treemacs-follow-mode t)
    (treemacs-filewatch-mode t)
    (treemacs-fringe-indicator-mode t)
    (pcase (cons (not (null (executable-find "git")))
                 (not (null (executable-find "python3"))))
      (`(t . t)
       (treemacs-git-mode 'extended))
      (`(t . _)
       (treemacs-git-mode 'simple))))
  :bind
  (:map global-map
        ("M-0"       . treemacs-select-window)
        ("C-x t 1"   . treemacs-delete-other-windows)
        ("C-x t t"   . treemacs)
	([f8] . treemacs)
        ("C-x t B"   . treemacs-bookmark)
        ("C-x t C-t" . treemacs-find-file)
        ("C-x t M-t" . treemacs-find-tag)))

(use-package treemacs-evil
  :after treemacs evil
  :ensure t)

(use-package treemacs-projectile
  :after treemacs projectile
  :ensure t)


;;; elpy (python-IDE)
(exec-path-from-shell-initialize)

(elpy-enable)
(setenv "WORKON_HOME" "/home/azumi/.pyenv/versions/anaconda3-5.0.1")
(add-to-list 'exec-path "~/.pyenv/shims")
(setq elpy-rpc-backend "jedi")
(add-to-list 'load-path "~/.pyenv/versions/bin/")

;;; yasnipets
;; 自分用・追加用テンプレート -> mysnippetに作成したテンプレートが格納される
(require 'yasnippet)
(setq yas-snippet-dirs
      '("~/.emacs.d/mysnippets"
        "~/.emacs.d/yasnippets"
        ))

;; 既存スニペットを挿入する
(define-key yas-minor-mode-map (kbd "C-x i i") 'yas-insert-snippet)
;; 新規スニペットを作成するバッファを用意する
(define-key yas-minor-mode-map (kbd "C-x i n") 'yas-new-snippet)
;; 既存スニペットを閲覧・編集する
(define-key yas-minor-mode-map (kbd "C-x i v") 'yas-visit-snippet-file)

(yas-global-mode 1)

;;; インデントをtabから半角スペースに変更
(setq-default indent-tabs-mode nil)

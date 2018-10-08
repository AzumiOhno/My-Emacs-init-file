
;; straight.el 
(let ((bootstrap-file (concat user-emacs-directory "straight/repos/straight.el/bootstrap.el"))
      (bootstrap-version 3))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; use-packageをインストールする
(straight-use-package 'use-package)

;; オプションなしで自動的にuse-packageをstraight.elにフォールバックする
;; 本来は (use-package hoge :straight t) のように書く必要がある
(setq straight-use-package-by-default t)

;;; custom-theme
;; (use-package madhat2r-theme
;;   :init (load-theme 'madhat2r t))
(use-package doom-themes
  :config (progn
            (setq doom-themes-enable-bold t
                  doom-themes-enable-italic t)
            (load-theme 'doom-one t)
            (doom-themes-visual-bell-config)
            (doom-themes-treemacs-config)
            (doom-themes-org-config)))

;;
;; ssh
;;
(use-package tramp)
(setq tramp-default-method "ssh")

;;
;;mozc(linuxのみ)
;;
(when (eq system-type 'gnu/linux)
  (use-package mozc)
  (set-language-environment "Japanese")
  (setq default-input-method "japanese-mozc")
  (prefer-coding-system 'utf-8)
  (set-fontset-font t 'japanese-jisx0208 "IPAPGothic")
  ;;key変更(mozc)
  (global-set-key (kbd "C-j") 'toggle-input-method))

(defun advice:mozc-key-event-with-ctrl-key--with-ctrl (r)
  (cond ((and (not (null (cdr r))) (eq (cadr r) 'control) (null (cddr r)))
         (case (car r)
           (102 r) ; C-f
           (98 r) ; C-b
           (110 '(down)) ; C-n
           (112 '(up))  ; C-p
           (t r)
           ))
        (t r)))

(advice-add 'mozc-key-event-to-key-and-modifiers :filter-return 'advice:mozc-key-event-with-ctrl-key--with-ctrl)
;; (advice-remove 'mozc-key-event-to-key-and-modifiers 'mozc-key-event-with-ctrl-key)

;;
;; windmove
;; 
(when (fboundp 'windmove-default-keybindings)
  (windmove-default-keybindings))
(global-set-key (kbd "C-c <left>") 'windmove-left)
(global-set-key (kbd "C-c <down>") 'windmove-down)
(global-set-key (kbd "C-c <up>") 'windmove-up)
(global-set-key (kbd "C-c <right>") 'windmove-right)


;;python modeのときはsmartparens起動
(use-package smartparens
  :hook (python-mode . smartparens-mode))
;; (add-hook 'python-mode-hook 'smartparens-mode)

;;
;;Yatex
;;
(use-package yatex
  :mode
  ("\\.tex$" . yatex-mode)
  :init
  (add-hook 'yatex-mode-hook
          #'(lambda ()
              (reftex-mode 1)
              (define-key reftex-mode-map
                (concat YaTeX-prefix ">") 'YaTeX-comment-region)
              (define-key reftex-mode-map
                (concat YaTeX-prefix "<") 'YaTeX-uncomment-region)))
  :config
  (progn (setq YaTex-kanji-code nil)
         (setq tex-command "platex")
         (setq bibtex-command "pbibtex")
         (setq reftex-defaultbibliography '("/home/azumi/lab/progress_report/reference.bib"))))
;; 文章作成時の日本語文字コード
;; 0: no-converion
;; 1: Shift JIS (windows & dos default)
;; 2: ISO-2022-JP (other default)
;; 3: EUC
;; 4: UTF-8
;; (setq YaTeX-kanji-code 0)

;; (setq tex-command "platex")

;; (setq bibtex-command "pbibtex")
;; (defvar YaTeX-dvi2-command-ext-alist
;;   '(("[agx]dvi\\|dviout\\|emacsclient" . ".dvi")
;;    ("ghostview\\|gv" . ".ps")
;;    ("acroread\\|pdf\\|Preview\\|TeXShop\\|Skim\\|evince\\|apvlv" . ".pdf")))
;; (setq dviprint-command-format "dvipdfmx -f ptex-ipaex.map %s")
;; ;;reftex-mode
;; (add-hook 'yatex-mode-hook
;;           #'(lambda ()
;;               (reftex-mode 1)
;;               (define-key reftex-mode-map
;;                 (concat YaTeX-prefix ">") 'YaTeX-comment-region)
;;               (define-key reftex-mode-map
;;                 (concat YaTeX-prefix "<") 'YaTeX-uncomment-region)))
;; (setq reftex-default-bibliography '("/home/azumi/lab/progress_report/reference.bib"))

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
(add-to-list 'mode-line-format
             '(:eval (count-lines-and-chars)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;日本語の印刷
(setq ps-multibyte-buffer 'non-latin-printer)
(require 'ps-mule)
(defalias 'ps-mule-header-string-charsets 'ignore)


;; (use-package pdf-preview)
;; (setq pdf-preview-preview-command "evince")
;;multi-term
(use-package multi-term)

;; ;pdf-toolsに関する設定
;; (use-package pdf-tools)
;; (use-package pdf-annot) 
;; (use-package pdf-history) 
;; (use-package pdf-info) 
;; (use-package pdf-isearch) 
;; (use-package pdf-links) 
;; (use-package pdf-misc) 
;; (use-package pdf-occur) 
;; (use-package pdf-outline) 
;; (use-package pdf-sync) 
;; (use-package tablist-filter)
;; (use-package tablist)
;; (add-to-list auto-mode-alist (cons "\\.pdf$" 'pdf-view-mode))

(use-package linum)
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
(use-package magit
  :bind ("C-x g" . magit-status))
;; (global-set-key (kbd "C-x g") 'magit-status)

;; markdown
(use-package markdown-mode
  :config (setq markdown-command "pandoc"))

;;; undo-tree
(use-package undo-tree
  :init
  (global-undo-tree-mode t)
  :bind
  ("M-/" . undo-tree-redo))

;; multiple-cursors
(use-package multiple-cursors
  :bind
  (("C-S-c C-S-c" . mc/edit-lines)
   ("C->" . mc/mark-next-like-this)
   ("C-<" . mc/mark-next-previous-like-this)
   ("C-c C->" . mc/mark-all-like-this)))

;;; wdired
(use-package wdired)
(add-hook 'dired-load-hook 'wdired)
(define-key dired-mode-map (kbd "r") 'wdired-change-to-wdired-mode)
(setq dired-dwim-target t)

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
(use-package which-key)
(which-key-setup-side-window-bottom)
(which-key-mode 1)


;;; ido settings
(ido-mode 1)
(ido-everywhere 1)
(setq ido-enable-flex-matching t)	;あいまい一致

(use-package ido-completing-read+)
(ido-ubiquitous-mode 1)

(use-package smex
  :bind
  (("M-x" . smex)
   ("M-X" . smex-major-mode-commands)))

(setq ido-max-window-height 0.75)	; idoが使うwindowの高さを設定
(use-package ido-vertical-mode)
(ido-vertical-mode 1)
(setq ido-vertical-define-keys 'C-n-and-C-p-only)

;;; treemacs configuration
(use-package treemacs
  :defer t
  :init
  (with-eval-after-load 'winum
    (define-key winum-keymap (kbd "M-0") #'treemacs-select-window))
  :config
  (progn
    (setq treemacs-collapse-dirs              (if (executable-find "python") 3 0)
          treemacs-deferred-git-apply-delay   0.5
          treemacs-display-in-side-window     t
          treemacs-file-event-delay           5000
          treemacs-file-follow-delay          0.2
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
        ("C-x t B"   . treemacs-bookmark)
        ("C-x t C-t" . treemacs-find-file)
        ("C-x t M-t" . treemacs-find-tag)))

(use-package treemacs-evil
  :after treemacs evil)

(use-package treemacs-projectile
  :after treemacs projectile)

;;; elpy (python-IDE)
(use-package elpy)
(use-package exec-path-from-shell
  :init (exec-path-from-shell-initialize))

(elpy-enable)
(setenv "WORKON_HOME" "/home/azumi/.pyenv/versions/anaconda3-5.0.1")
(add-to-list 'exec-path "~/.pyenv/shims")
(setq elpy-rpc-backend "jedi")
(add-to-list 'load-path "~/.pyenv/versions/bin/")

;;; yasnipets
;; 自分用・追加用テンプレート -> mysnippetに作成したテンプレートが格納される
(use-package yasnippet)
(setq yas-snippet-dirs
      '("~/.emacs.d/mysnippets"
        "~/.emacs.d/yasnippets"))

;; 既存スニペットを挿入する
(define-key yas-minor-mode-map (kbd "C-x i i") 'yas-insert-snippet)
;; 新規スニペットを作成するバッファを用意する
(define-key yas-minor-mode-map (kbd "C-x i n") 'yas-new-snippet)
;; 既存スニペットを閲覧・編集する
(define-key yas-minor-mode-map (kbd "C-x i v") 'yas-visit-snippet-file)

(yas-global-mode 1)

;;; インデントをtabから半角スペースに変更
(setq-default indent-tabs-mode nil)

;;; sudo-edit
(use-package sudo-edit
  :bind ("C-c C-r" . sudo-edit))

;;; menu & tool bar are invisible
(menu-bar-mode 0)
(tool-bar-mode 0)

;;; open-junk-file
(use-package open-junk-file
  :bind ("C-x C-z" . 'open-junk-file))

;;; lispxmp
(use-package lispxmp
  :hook ((emacs-lisp-mode . lispxmp)
	 (lisp-interaction-mode . lispxmp)
	 (lisp-mode . lispxmp))
  :bind (:map emacs-lisp-mode-map
	      ("C-c C-d" . lispxmp)))

;;; paredit
(use-package paredit
  :hook ((emacs-lisp-mode . enable-paredit-mode)
	 (lisp-interaction-mode . enable-paredit-mode)
	 (lisp-mode . enable-paredit-mode)
	 (ielm-mode . enable-paredit-mode)))

(add-hook 'emacs-lisp-mode-hook 'turn-on-eldoc-mode)
(add-hook 'lisp-interaction-mode-hook 'turn-on-eldoc-mode)
(add-hook 'ielm-mode-hook 'turn-on-eldoc-mode)
(setq eldoc-idle-delay 0.2)            ; すぐに表示したい
(setq eldoc-minor-mode-string "")      ; モードラインにEldocと表示しない
;; 釣り合いの取れる括弧をハイライトする
(show-paren-mode 1)
;; 改行と同時にインデントも行う
(global-set-key "\C-m" 'newline-and-indent)
;; find-functionをキー割り当てする
(find-function-setup-keys)

;;; google translate
(use-package google-translate)
(defvar google-translate-english-chars "[:ascii:]’“”–"
  "これらの文字が含まれているときは英語とみなす")
(defun google-translate-enja-or-jaen (&optional string)
  "regionか、現在のセンテンスを言語自動判別でGoogle翻訳する。"
  (interactive)
  (setq string
        (cond ((stringp string) string)
              (current-prefix-arg
               (read-string "Google Translate: "))
              ((use-region-p)
               (buffer-substring (region-beginning) (region-end)))
              (t
               (save-excursion
                 (let (s)
                   (forward-char 1)
                   (backward-sentence)
                   (setq s (point))
                   (forward-sentence)
                   (buffer-substring s (point)))))))
  (let* ((asciip (string-match
                  (format "\\`[%s]+\\'" google-translate-english-chars)
                  string)))
    (run-at-time 0.1 nil 'deactivate-mark)
    (google-translate-translate
     (if asciip "en" "ja")
     (if asciip "ja" "en")
     string)))
(global-set-key (kbd "C-c t") 'google-translate-enja-or-jaen)

;;; speed-type
(use-package speed-type)

;;; bs-show
(global-set-key (kbd "C-x C-b") 'bs-show)

;;; browse-kill-ring kill-ringの一覧を表示
(use-package browse-kill-ring
  :bind ("M-y" . browse-kill-ring))

;;; smooth-scroll
(use-package smooth-scroll
  :config (smooth-scroll-mode t))

;;; 日付挿入
(defun insert-current-time ()
  (interactive)
  (insert (format-time-string "%Y-%m-%d(%a) %H:%M:%S" (current-time))))
(bind-key "C-c d" 'insert-current-time)


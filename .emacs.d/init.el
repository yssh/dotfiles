;; -*- Mode: Emacs-Lisp | Coding: utf-8 -*-

;; Common-Lisp
(require 'cl)

;; Emacsからの質問をy/nで回答する
(fset 'yes-or-no-p 'y-or-n-p)

;; スタートアップメッセージを非表示
(setq inhibit-startup-screen t)

;; scratchの初期メッセージ消去
(setq initial-scratch-message nil)

;; ビープ音の消去
(setq visible-bell t)
(setq ring-bell-function 'ignore)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ロードパスの追加
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Emacs 23より前のバージョン用
(when (< emacs-major-version 23)
  (defvar user-emacs-directory "~/.emacs.d/"))

;; load-path を追加する関数を定義
(defun add-to-load-path (&rest paths)
  (let (path)
    (dolist (path paths paths)
      (let ((default-directory
              (expand-file-name (concat user-emacs-directory path))))
        (add-to-list 'load-path default-directory)
        (if (fboundp 'normal-top-level-add-subdirs-to-load-path)
            (normal-top-level-add-subdirs-to-load-path))))))

;; 引数のディレクトリとそのサブディレクトリをload-pathに追加
(add-to-load-path "elisp" "public_repos")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; パッケージ管理
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; package.el
(when (require 'package nil t)
  ;; パッケージリポジトリにmeplaとmarmaladeを追加
  (add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/"))
  (add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
  ;; インストールしたパッケージにロードパスを通して読み込む
  (package-initialize))

;; auto-install
(when (require 'auto-install nil t)
  ;; インストールディレクトリを設定する 初期値は ~/.emacs.d/auto-install/
  (setq auto-install-directory "~/.emacs.d/elisp/")
  ;; EmacsWikiに登録されているelisp の名前を取得する
  ;; (auto-install-update-emacswiki-package-name t)
  ;; 必要であればプロキシの設定を行う
  ;; (setq url-proxy-services '(("http" . "localhost:8339")))
  ;; install-elisp の関数を利用可能にする
  (auto-install-compatibility-setup))

;; 新しいパッケージを読み込み
(setq load-prefer-newer t)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 環境に応じた設定の分岐
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; CUIとGUIによる分岐
(when window-system
  ;; tool-barを非表示
  (tool-bar-mode 0))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; キーバインドの設定
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Macのバックスラッシュ対応
(when (eq system-type 'darwin)
  (define-key global-map [?¥] [?\\])
  (define-key global-map [?\C-¥] [?\C-\\])
  (define-key global-map [?\M-¥] [?\M-\\])
  (define-key global-map [?\C-\M-¥] [?\C-\M-\\]))

;; 削除
(define-key global-map (kbd "C-h") 'delete-backward-char)
;; ヘルプ
(define-key global-map (kbd "M-?") 'help-for-help)
;; 最小化を無効
(define-key global-map (kbd "C-z") nil)

;; ウインドウ切り替え
(defun other-window-or-split (val)
  (interactive)
  (when (one-window-p)
    (split-window-vertically))
  (other-window val))
(define-key global-map (kbd "<C-tab>") (lambda () (interactive) (other-window-or-split 1)))
(define-key global-map (kbd "<C-S-tab>") (lambda () (interactive) (other-window-or-split -1)))

;; ウインドウを閉じる
(define-key global-map (kbd "C-x C-w") 'delete-window)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 環境変数の設定
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; パスの設定
(when (eq window-system 'ns)
  (exec-path-from-shell-initialize))

;; 文字コードを指定する
(set-language-environment "Japanese")
(prefer-coding-system 'utf-8)

;; Mac OS Xの場合のファイル名の設定
(when (eq system-type 'darwin)
  (require 'ucs-normalize)
  (set-file-name-coding-system 'utf-8-hfs)
  (setq locale-coding-system 'utf-8-hfs))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; インデントの設定
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; タブ文字の表示幅. 初期値は8
(setq-default tab-width 4)

;; インデントにタブ文字を使用しない
(setq-default indent-tabs-mode nil)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 大文字小文字の設定
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 検索時に大文字小文字を区別しない
(setq case-fold-search t)
(setq isearch-case-fold-search t)

;; バッファ、ファイル名補完時に大文字小文字を区別しない
(setq read-file-name-completion-ignore-case t)

;; 置換時に大文字小文字を区別する
(setq case-replace nil)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; フレームに関する設定
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; モードラインに行番号・カラム番号を表示
(line-number-mode t)
(column-number-mode t)

;; モードラインにファイルサイズを表示
(size-indication-mode t)

;; リージョン内の行数と文字数をモードラインに表示する（範囲指定時のみ）
;; http://d.hatena.ne.jp/sonota88/20110224/1298557375
(defun count-lines-and-chars ()
  (if mark-active
      (format "%d lines,%d chars "
              (count-lines (region-beginning) (region-end))
              (- (region-end) (region-beginning)))
      ;; これだとエコーエリアがチラつく
      ;;(count-lines-region (region-beginning) (region-end))
    ""))
(add-to-list 'default-mode-line-format
             '(:eval (count-lines-and-chars)))

;; タイトルバーにファイルのフルパスを表示
(setq frame-title-format "%f")

;; 行番号を常に表示する
(global-linum-mode t)
(setq linum-format "%4d")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 表示・装飾に関する設定
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 表示テーマの設定
;; (when (eq window-system 'ns)
;;   (load-theme 'zenburn t))

;; フォントの設定
(when (eq window-system 'ns)
  ;; asciiフォントをRictyに
  (set-face-attribute 'default nil
                      :family "Ricty"
                      :height 160)
  ;; 日本語フォントをヒラギノ角ゴシックProNに
  (set-fontset-font
   nil 'japanese-jisx0208
   (font-spec :family "Hiragino Kaku Gothic ProN")))

(when (eq window-system 'x)
  (add-to-list 'default-frame-alist '(font . "ricty-12")))

;; ウインドウの透明度設定
;; (when (eq window-system 'ns)
;;   (set-frame-parameter (selected-frame) 'alpha '(0.85 0.80)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ハイライトの設定
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 現在行のハイライト
(defface my-hl-line-face
  '((((class color) (background dark))
     (:background "NavyBlue" t))
    (((class color) (background light))
     (:background "#e6e6fa" t))
    (t (:bold t)))
  "hl-line's my face")
(setq hl-line-face 'my-hl-line-face)
(global-hl-line-mode t)

;; 括弧の対応関係のハイライト
(show-paren-mode t)
(setq show-paren-delay 0)
(setq show-paren-style 'mixed)
(set-face-background 'show-paren-match-face nil)
(set-face-underline-p 'show-paren-match-face "red")

;; 行末の空白を表示
(setq-default show-trailing-whitespace t)

;; 全角スペース/タブの可視化
(setq whitespace-style
      '(tabs tab-mark spaces space-mark))
(setq whitespace-space-regexp "\\(\x3000+\\)")
(setq whitespace-display-mappings
      '((space-mark ?\x3000 [?\□])
        (tab-mark   ?\t   [?\xBB ?\t])
        ))
(require 'whitespace)
(global-whitespace-mode t)
(set-face-foreground 'whitespace-space "LightSlateGray")
(set-face-background 'whitespace-space "DarkSlateGray")
(set-face-foreground 'whitespace-tab "LightSlateGray")
(set-face-background 'whitespace-tab "DarkSlateGray")

;; volatile-highlights
(when (require 'volatile-highlights nil t)
  (volatile-highlights-mode t))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; スクロールの設定
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; スクロールを1行毎に
(setq scroll-conservatively 35
      scroll-margin 0
      scroll-step 1)
(setq next-screen-context-lines 1)

;; トラックパッド用スクロール設定
(when (eq window-system 'ns)
  (defun scroll-down-with-lines ()
    "" (interactive) (scroll-down 3))
  (defun scroll-up-with-lines ()
    "" (interactive) (scroll-up 3))
  (global-set-key [wheel-up] 'scroll-down-with-lines)
  (global-set-key [wheel-down] 'scroll-up-with-lines)
  (global-set-key [double-wheel-up] 'scroll-down-with-lines)
  (global-set-key [double-wheel-down] 'scroll-up-with-lines)
  (global-set-key [triple-wheel-up] 'scroll-down-with-lines)
  (global-set-key [triple-wheel-down] 'scroll-up-with-lines))

;; smooth-scroll
;; (when (require 'smooth-scroll nil t)
;;   (smooth-scroll-mode t))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; バックアップとオートセーブ
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; バックアップとオートセーブファイルを~/.emacs.d/backups/へ集める
(add-to-list 'backup-directory-alist
             (cons "." "~/.emacs.d/backups/"))
(setq auto-save-file-name-transforms
      `((".*" ,(expand-file-name "~/.emacs.d/backups/") t)))

;; オートセーブファイル作成までの秒間隔
(setq auto-save-tmeout 15)

;; オートセーブファイル作成までのタイプ間隔
(setq auto-save-interval 60)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Helm
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(when (require 'helm-config nil t)
  (helm-mode t)

  ;; 色の変更
  (custom-set-faces
   '(helm-selection ((t (:background "#6495ED")))))

  ;; キーバインド
  (define-key global-map (kbd "M-x")     'helm-M-x)
  (define-key global-map (kbd "M-y")     'helm-show-kill-ring)
  (define-key global-map (kbd "C-x C-b") 'helm-buffers-list)
  (define-key global-map (kbd "C-x C-i") 'helm-imenu)
  (define-key global-map (kbd "C-x C-u") 'helm-recentf)

  (define-key helm-map (kbd "C-h") 'delete-backward-char)
  (define-key helm-map (kbd "C-w") 'backward-kill-word)
  (define-key helm-map (kbd "TAB") 'helm-execute-persistent-action)

  ;; kill-bufferでのhelmを抑制
  (add-to-list 'helm-completing-read-handlers-alist '(kill-buffer . nil))

  ;; ミニバッファでのC-kで現在位置から削除
  (setq helm-delete-minibuffer-contents-from-point t)
  (defadvice helm-delete-minibuffer-contents (before helm-emulate-kill-line activate)
    "Emulate `kill-line' in helm minibuffer"
    (kill-new (buffer-substring (point) (field-end))))

  ;; TABによる新規バッファ作成の抑制
  (defadvice helm-ff-kill-or-find-buffer-fname (around execute-only-if-exist activate)
    "Execute command only if CANDIDATE exists"
    (when (file-exists-p candidate)
      ad-do-it))

  ;; 自動補完を無効化
  (setq helm-ff-auto-update-initial-value nil)

  ;; バッファリストの設定
  (custom-set-variables
   '(helm-truncate-lines t)
   '(helm-boring-buffer-regexp-list '("^*"))
   '(helm-boring-file-regexp-list '("\\.elc$"))
   '(helm-skip-boring-buffers t)
   '(helm-skip-boring-files t))

  ;; バッファ名を表示する幅を調整
  (setq helm-buffer-max-length 50)

  ;; occur/moccur
  (eval-after-load "helm-regexp"
    #'(progn
        (define-key global-map (kbd "C-x C-o") 'helm-occur)
        (define-key isearch-mode-map (kbd "C-o") #'helm-occur-from-isearch)))

  (defun helm-moccur ()
    (interactive)
    (let ((buffers (moccur-filter-buffers (buffer-list))))
      ;; sort
      (setq buffers (sort buffers moccur-buffer-sort-method))
      (helm-multi-occur buffers)))

  (define-key global-map (kbd "C-x C-M-o") #'helm-moccur)

  (defun moccur-from-helm-moccur (arg)
    (interactive "P")
    (let ((f (if (string-equal "Occur" (helm-attr 'name))
                 #'occur-by-moccur #'moccur)))
      (helm-run-after-quit f helm-input arg)))

  (define-key helm-moccur-map (kbd "C-c C-p") #'moccur-from-helm-moccur)

  ;; swoop
  (when (require 'helm-swoop nil t)
    (define-key global-map (kbd "C-x C-p") 'helm-swoop)

    (define-key helm-swoop-map (kbd "C-r") 'helm-previous-line)
    (define-key helm-swoop-map (kbd "C-s") 'helm-next-line)

    (define-key helm-swoop-map (kbd "C-c C-p") 'helm-swoop-edit)
    (define-key helm-swoop-edit-map (kbd "C-c C-c") 'helm-swoop--edit-complete)
    (define-key helm-swoop-edit-map (kbd "C-c C-k") 'helm-swoop--edit-cancel)

    (define-key helm-multi-swoop-map (kbd "C-c C-p") 'helm-multi-swoop-edit)
    (define-key helm-multi-swoop-edit-map (kbd "C-c C-c") 'helm-multi-swoop--edit-complete)
    (define-key helm-multi-swoop-edit-map (kbd "C-c C-k") 'helm-multi-swoop--edit-cancel)

    (setq helm-multi-swoop-edit-save t)
    (setq helm-swoop-split-with-multiple-windows t)
    (setq helm-swoop-move-to-line-cycle nil)

    ;; ace-isearch
    ;; (global-ace-isearch-mode t)
    ))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 検索と置換
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; color-moccur
(when (require 'color-moccur nil t)
  ;; キーバインド
  (define-key global-map (kbd "C-o") 'occur-by-moccur)
  (define-key global-map (kbd "C-M-o") 'moccur)
  ;; スペース区切りでAND検索
  (setq moccur-split-word t)
  ;; 除外するバッファ名
  (custom-set-variables
   '(*moccur-buffer-name-exclusion-list*
     '("TAGS" "^*.*" "^[ ].+"))))

;; moccur-edit
(when (require 'moccur-edit nil t)
  (define-key moccur-mode-map (kbd "C-c C-p") 'moccur-edit-mode-in))

;; wgrep
(when (require 'wgrep nil t)
  (setq wgrep-enable-key (kbd "C-c C-p"))
  (setq wgrep-auto-save-buffer t))

;; ag
(when (require 'ag nil t)
  (custom-set-variables
   '(ag-highlight-search t)  ; 検索結果の中の検索語をハイライトする
   '(ag-reuse-window 'nil)   ; 現在のウィンドウを検索結果表示に使わない
   '(ag-reuse-buffers 'nil)) ; 現在のバッファを検索結果表示に使わない

  (define-key global-map (kbd "C-x C-a") 'ag)

  (when (require 'wgrep-ag nil t)
    (autoload 'wgrep-ag-setup "wgrep-ag")
    (add-hook 'ag-mode-hook 'wgrep-ag-setup)))

;; anzu
(when (require 'anzu nil t)
  (global-anzu-mode +1)

  (custom-set-variables
   '(anzu-mode-lighter "")
   '(anzu-deactivate-region t)
   '(anzu-search-threshold 1000)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 履歴管理
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; undohist
(when (require 'undohist nil t)
  (undohist-initialize))

;; undo-tree
(when (require 'undo-tree nil t)
  (global-undo-tree-mode)
  (define-key global-map (kbd "C-?") 'undo-tree-redo))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ファイル・ディレクトリ関連
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 同一ファイルの区別
(require 'uniquify)
(setq uniquify-buffer-name-style 'post-forward-angle-brackets)

;; recentf
(require 'recentf)
(setq recentf-save-file "~/.emacs.d/.recentf")
(setq recentf-max-saved-items 500)
(setq-default find-file-visit-truename t)
(recentf-mode t)

;; dired
(defun dired-open-in-accordance-with-situation ()
  (interactive)
  (let ((file (dired-get-filename)))
    (if (file-directory-p file)
        (dired-find-alternate-file)
      (dired-find-file))))

(put 'dired-find-alternate-file 'disabled nil)

(define-key global-map (kbd "C-x C-d") 'dired)
(define-key dired-mode-map (kbd "RET") 'dired-open-in-accordance-with-situation)
(define-key dired-mode-map "a" 'dired-find-file)

;; wdired
(require 'wdired)
(define-key dired-mode-map (kbd "C-c C-p") 'wdired-change-to-wdired-mode)
(setq wdired-allow-to-change-permissions t)

;; direx
(when (require 'direx nil t)
  (setq direx:leaf-icon "  "
        direx:open-icon "▾ "
        direx:closed-icon "▸ ")
  (define-key global-map (kbd "C-x C-j") 'direx:jump-to-directory-other-window)
  (define-key global-map (kbd "C-x C-M-j") 'direx:find-directory-other-window)
  (define-key global-map (kbd "C-x M-j") 'direx-project:jump-to-project-root-other-window))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ウィンドウ管理
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; elscreen
(when (and (>= emacs-major-version 24) (require 'elscreen nil t))
  (defmacro elscreen-create-automatically (ad-do-it)
    `(if (not (elscreen-one-screen-p))
         ,ad-do-it
       (elscreen-create)
       (elscreen-notify-screen-modification 'force-immediately)
       (elscreen-message "New screen is automatically created")))

  (defadvice elscreen-next (around elscreen-create-automatically activate)
    (elscreen-create-automatically ad-do-it))

  (defadvice elscreen-previous (around elscreen-create-automatically activate)
    (elscreen-create-automatically ad-do-it))

  (defadvice elscreen-toggle (around elscreen-create-automatically activate)
    (elscreen-create-automatically ad-do-it))

  (setq elscreen-prefix-key (kbd "C-t"))
  (elscreen-start)

  (custom-set-variables)
  (custom-set-faces
   '(elscreen-tab-background-face ((((class color)) (:background "gray95"))))
   '(elscreen-tab-current-screen-face ((((class color)) (:background "gray40" :foreground "gray95"))))
   '(elscreen-tab-other-screen-face ((((class color)) (:background "gray80" :foreground "gray10")))))

  (define-key elscreen-map (kbd "C-c") 'elscreen-create)
  (define-key elscreen-map (kbd "C-t") 'elscreen-next)
  (define-key elscreen-map (kbd "C-r") 'elscreen-previous)
  (define-key elscreen-map (kbd "C-w") 'elscreen-kill))

;; popwin
(when (require 'popwin nil t)
  (setq display-buffer-function 'popwin:display-buffer)
  (setq popwin:popup-window-position 'bottom)
  (setq popwin:popup-window-height 0.5)
  (push '("*Moccur*") popwin:special-display-config)
  (push '("*All*") popwin:special-display-config)
  (push '("*Compile-Log*") popwin:special-display-config)
  (push '("\\*Helm " :regexp t) popwin:special-display-config)
  (push '("\\*ag " :regexp t) popwin:special-display-config)
  (push '("\\*magit " :regexp t) popwin:special-display-config)
  (push '(direx:direx-mode :position left :width 30 :dedicated t)
        popwin:special-display-config)
  (push '(" *undo-tree*" :width 0.3 :position right) popwin:special-display-config)
  (define-key global-map (kbd "C-x p") 'popwin:display-last-buffer))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 入力の効率化
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; auto-complete
(when (require 'auto-complete-config nil t)
  (setq ac-auto-start 3)
  (setq ac-auto-show-menu 0.8)

  (setq ac-use-menu-map t)
  (setq ac-dwim t)
  (setq ac-ignore-case nil)

  (setq-default ac-sources
                '(ac-source-filename
                  ac-source-yasnippet
                  ac-source-dictionary
                  ac-source-words-in-same-mode-buffers))

  (global-auto-complete-mode t)

  ;; yasnippetのbindingを指定するとエラーが出るので回避
  (setf (symbol-function 'yas-active-keys)
        (lambda ()
          (remove-duplicates
           (mapcan #'yas--table-all-keys (yas--get-snippet-tables))))))

;; cua-mode
(cua-mode t)
(setq cua-enable-cua-keys nil)

;; electric-pair
(when (>= emacs-major-version 24)
  (electric-pair-mode t)
  (setq electric-pair-pairs '(
                              (?\' . ?\')
                              (?\{ . ?\})
                              )))

;; region-bindings-mode
(when (require 'region-bindings-mode nil t)
  (region-bindings-mode-enable))

;; expand-region
(when (require 'expand-region nil t)
  (global-set-key (kbd "C-@") 'er/expand-region)
  (global-set-key (kbd "C-M-@") 'er/contract-region))

;; multiple-cursors
(when (require 'multiple-cursors nil t)
  (define-key global-map (kbd "C-<") 'mc/mark-previous-like-this)
  (define-key global-map (kbd "C->") 'mc/mark-next-like-this)

  (define-key region-bindings-mode-map (kbd "a") 'mc/mark-all-like-this)
  (define-key region-bindings-mode-map (kbd "p") 'mc/mark-previous-like-this)
  (define-key region-bindings-mode-map (kbd "n") 'mc/mark-next-like-this)
  (define-key region-bindings-mode-map (kbd "m") 'mc/mark-more-like-this-extended))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; yasnippet
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(when (require 'yasnippet nil t)
  (setq yas-snippet-dirs '("~/.emacs.d/snippets"))
  (yas-global-mode t)

  ;; キーバインド
  (define-key yas-minor-mode-map (kbd "<tab>") nil)
  (define-key yas-minor-mode-map (kbd "TAB") nil)
  (define-key yas-minor-mode-map (kbd "C-o") 'yas-expand)
  (define-key yas-minor-mode-map (kbd "C-x i i") 'yas-insert-snippet)
  (define-key yas-minor-mode-map (kbd "C-x i n") 'yas-new-snippet)
  (define-key yas-minor-mode-map (kbd "C-x i v") 'yas-visit-snippet-file)

  ;; Helm連携
  (eval-after-load "helm"
    '(progn
       (defun my-yas/prompt (prompt choices &optional display-fn)
         (let* ((names (loop for choice in choices
                             collect (or (and display-fn (funcall display-fn choice))
                                         choice)))
                (selected (helm-other-buffer
                           `(((name . ,(format "%s" prompt))
                              (candidates . names)
                              (action . (("Insert snippet" . (lambda (arg) arg))))))
                           "*helm yas/prompt*")))
           (if selected
               (let ((n (position selected names :test 'equal)))
                 (nth n choices))
             (signal 'quit "user quit!"))))
       (custom-set-variables '(yas/prompt-functions '(my-yas/prompt))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Flycheck
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(when (require 'flycheck nil t)
  (add-hook 'after-init-hook #'global-flycheck-mode)

  (define-key flycheck-mode-map (kbd "M-n") 'flycheck-next-error)
  (define-key flycheck-mode-map (kbd "M-p") 'flycheck-previous-error)

  (when (require 'flycheck-pos-tip nil t)
    (eval-after-load 'flycheck
      '(custom-set-variables
        '(flycheck-display-errors-function #'flycheck-pos-tip-error-messages)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ctags
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(when (require 'ctags nil t)
  (when (eq system-type 'darwin)
    (setq ctags-command "/usr/local/bin/ctags -Re"))

  (when (eq system-type 'gnu/linux)
    (setq ctags-command "/usr/bin/ctags -Re"))

  (define-key global-map (kbd "<f5>") 'ctags-create-or-update-tags-table)
  (define-key global-map (kbd "C-x C-t") 'helm-etags-select)
  (define-key global-map (kbd "C-x C-y") 'pop-tag-mark))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Emacs-Lisp
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun my-elisp-mode-hook ()
  "lisp-mode-hooks"
  (when (require 'eldoc nil t)
    (set (make-local-variable 'eldoc-idle-delay) 0.2)
    (set (make-local-variable 'eldoc-echo-area-use-multiline-p) t)
    (turn-on-eldoc-mode)))

(add-hook 'emacs-lisp-mode-hook 'my-elisp-mode-hook)

;; elisp-completion
(defun elisp-completion-hook ()
  (when (require 'auto-complete nil t)
    (setq ac-sources
          '(ac-source-functions
            ac-source-variables
            ac-source-symbols
            ac-source-features
            ac-source-yasnippet
            ac-source-dictionary
            ac-source-words-in-same-mode-buffers))))

(add-hook 'emacs-lisp-mode-hook 'elisp-completion-hook)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Shell
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun my-shell-mode-hook ()
  (setq sh-basic-offset 4)
  (setq sh-indentation 4))

(add-hook 'sh-mode-hook 'my-shell-mode-hook)

(defun shell-completion-hook ()
  (when (require 'auto-complete nil t)
    (setq ac-sources
          '(ac-source-filename
            ac-source-yasnippet
            ac-source-dictionary))))

(add-hook 'sh-mode-hook 'shell-completion-hook)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Fortran
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun my-fortran-mode-hook ()
  (setq fortran-do-indent 2)
  (setq fortran-if-indent 2)
  (setq fortran-continuation-indent 6))

(add-hook 'fortran-mode-hook 'my-fortran-mode-hook)

(autoload 'fortran-mode "fortran" "major mode for FORTRAN(<=77)" t)
(add-to-list 'auto-mode-alist
             '("\\.\\(f\\|F\\)$" . fortran-mode))

(autoload 'f90-mode "f90" "major mode for FORTRAN(>=90)" t)
(add-to-list 'auto-mode-alist
             '("\\.\\(f90\\|F90\\|f95\\|F95\\|g90\\|g95\\)$" . f90-mode))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; C/C++
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun my-c-mode-hook ()
  "c/c++-mode-hooks"
  (when (require 'c-eldoc nil t)
    (setq c-eldoc-cpp-command "/usr/bin/g++")
    (set (make-local-variable 'eldoc-idle-delay) 0.2)
    (set (make-local-variable 'eldoc-echo-area-use-multiline-p) t)
    (c-turn-on-eldoc-mode)))

(add-hook 'c-mode-hook 'my-c-mode-hook)
(add-hook 'c++-mode-hook 'my-c-mode-hook)

;; C++-completion
(defun cpp-completion-hook ()
  (when (require 'auto-complete nil t)
    (add-to-list 'ac-sources 'ac-source-semantic)))

(add-hook 'c++-mode-hook 'cpp-completion-hook)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Perl
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; perl-modeをcperl-modeのエイリアスにする
(defalias 'perl-mode 'cperl-mode)

;; インデント設定
(setq cperl-indent-level 4 ; インデント幅を4にする
      cperl-continued-statement-offset 4 ; 継続する文のオフセット※
      cperl-brace-offset -4 ; ブレースのオフセット
      cperl-label-offset -4 ; labelのオフセット
      cperl-indent-parens-as-block t ; 括弧もブロックとしてインデント
      cperl-close-paren-offset -4 ; 閉じ括弧のオフセット
      cperl-tab-always-indent t ; TABをインデントにする
      cperl-highlight-variables-indiscriminately t) ; スカラを常にハイライトする

;; perl-completion
(defun perl-completion-hook ()
  (when (require 'perl-completion nil t)
    (perl-completion-mode t)
    (when (require 'auto-complete nil t)
      (setq ac-sources '(ac-source-perl-completion)))))

(add-hook 'cperl-mode-hook 'perl-completion-hook)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Python
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; jedi
(when (require 'jedi nil t)
  (add-hook 'python-mode-hook 'jedi:setup)
  (setq jedi:complete-on-dot t))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Ruby
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(add-to-list 'auto-mode-alist '("Rakefile$"  . ruby-mode))
(add-to-list 'auto-mode-alist '("Gemfile$"   . ruby-mode))
(add-to-list 'auto-mode-alist '("Capfile$"   . ruby-mode))
(add-to-list 'auto-mode-alist '("Guardfile$" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.rake$"   . ruby-mode))

(defun my-ruby-mode-hook ()
  (setq ruby-deep-indent-paren-style nil)
  (set (make-local-variable 'electric-pair-pairs) '((?\| . ?\|))))

(add-hook 'ruby-mode-hook 'my-ruby-mode-hook)

;; ruby-electric
(when (require 'ruby-electric nil t)
  (add-hook 'ruby-mode-hook
            '(lambda ()
               (ruby-electric-mode t)
               (when (>= emacs-major-version 24)
                 (set (make-local-variable 'electric-pair-mode) nil)))))

;; ruby-end
(require 'ruby-end nil t)

;; ruby-block
(when (require 'ruby-block nil t)
  (ruby-block-mode t)
  (setq ruby-block-highlight-toggle t))

;; inf-ruby
(autoload 'run-ruby "inf-ruby" "Run an inferior Ruby process")
(autoload 'inf-ruby-keys "inf-ruby" "Set local key defs for inf-ruby in ruby-mode")
(add-hook 'ruby-mode-hook 'inf-ruby-minor-mode)

;; robe
(add-hook 'ruby-mode-hook 'robe-mode)
(add-hook 'robe-mode-hook 'ac-robe-setup)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; PHP
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(when (require 'php-mode nil t)
  (add-to-list 'auto-mode-alist '("\\.ctp\\'" . php-mode))
  (setq php-search-url "http://jp.php.net/ja/")
  (setq php-manual-url "http://jp.php.net/manual/ja/"))

(defun my-php-mode-hook ()
  (setq c-basic-offset 4)
  ;; (c-set-offset 'case-label '+) ; switch文のcaseラベル
  (c-set-offset 'arglist-intro '+) ; 配列の最初の要素が改行した場合
  (c-set-offset 'arglist-close 0)) ; 配列の閉じ括弧

(add-hook 'php-mode-hook 'my-php-mode-hook)

;; php-completion
(defun php-completion-hook ()
  (when (require 'php-completion nil t)
    (php-completion-mode t)
    (define-key php-mode-map (kbd "C-o") 'phpcmp-complete)

    (when (require 'auto-complete nil t)
      (add-to-list 'ac-sources 'ac-source-php-completion))))

(add-hook 'php-mode-hook 'php-completion-hook)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; HTML/ERB
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(when (require 'web-mode nil t)
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-css-indent-offset 2)
  (setq web-mode-code-indent-offset 2)

  (add-to-list 'auto-mode-alist '("\\.erb$"   . web-mode))
  (add-to-list 'auto-mode-alist '("\\.html?$" . web-mode))
  (add-to-list 'ac-modes 'web-mode))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; JavaScrpt
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun my-js-mode-hook ()
  ;; インデント幅を2にする
  (setq js-indent-level 2
        js-expr-indent-offset 2))

;; js-modeの起動時にhookを追加
(add-hook 'js-mode-hook 'my-js-mode-hook)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; CSS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun my-css-mode-hook ()
  "css-mode hooks"
  ;; インデントをCスタイルにする
  (setq cssm-indent-function #'cssm-c-style-indenter)
  ;; インデント幅を2にする
  (setq cssm-indent-level 2)
  ;; 閉じ括弧の前に改行を挿入する
  (setq cssm-newline-before-closing-bracket t)

  (when (require 'auto-complete nil t)
    (add-to-list 'ac-sources 'ac-source-css-property)))

(add-hook 'css-mode-hook 'my-css-mode-hook)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; SCSS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(when (require 'scss-mode nil t)
  (add-to-list 'auto-mode-alist '("\\.scss$" . scss-mode)))

(defun my-scss-mode-hook ()
  "scss-mode-hook"
  ;; インデント幅を2にする
  (setq css-indent-offset 2)
  ;; 自動コンパイルをオフにする
  (setq scss-compile-at-save nil)

  (when (require 'auto-complete nil t)
    (add-to-list 'ac-sources 'ac-source-css-property)))

(add-hook 'scss-mode-hook 'my-scss-mode-hook)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Yaml
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(when (require 'yaml-mode nil t)
  (add-to-list 'auto-mode-alist '("\\.yml$\\|\\.yaml$" . yaml-mode)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Makefile
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(add-to-list 'auto-mode-alist '("Makefile\\..*$" . makefile-gmake-mode))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Git
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq vc-handled-backends '())
(eval-after-load "vc"
  '(remove-hook 'find-file-hooks 'vc-find-file-hook))

;; magit
(when (require 'magit nil t)
  (define-key global-map (kbd "C-x m") 'magit-status)
  (setq magit-auto-revert-mode nil)
  (setq magit-last-seen-setup-instructions "1.4.0"))

;; helm-ls-git
(when (require 'helm-ls-git nil t)
  (define-key global-map (kbd "C-x C-g") 'helm-ls-git-ls))

;; git-gutter
(when (require 'git-gutter nil t)
  (global-git-gutter-mode t)
  (git-gutter:linum-setup))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Subversion
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; dsvn
(autoload 'svn-status "dsvn" "Run `svn status'." t)
(autoload 'svn-update "dsvn" "Run `svn update'." t)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; シェルの利用
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; multi-termの設定
(when (require 'multi-term nil t)
  ;; 使用するシェルを指定
  (when (eq system-type 'darwin)
    (setq multi-term-program "/usr/local/bin/zsh")
    (setenv "TERMINFO" "~/.terminfo"))

  (when (eq system-type 'gnu/linux)
    (setq multi-term-program "/bin/bash"))

  (define-key global-map (kbd "C-M-m") 'multi-term)

  (add-hook 'term-mode-hook
            '(lambda ()
               (setq show-trailing-whitespace nil)
               (define-key term-raw-map (kbd "C-y") 'term-paste)
               (define-key term-raw-map (kbd "C-t")
                 (lookup-key (current-global-map) (kbd "C-t"))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; その他
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; エイリアス
(defalias 'dtw 'delete-trailing-whitespace)

;; ファイルが #! から始まる場合、+xを付けて保存する
;; (add-hook 'after-save-hook
;;           'executable-make-buffer-file-executable-if-script-p)

;;--------------------------------------------------
;; File name    :   init.el
;;              :   Emacs の基本的な設定
;;              :
;;--------------------------------------------------
;;
;=======================================================================
; このファイルが置かれているディレクトリ
;=======================================================================
(add-to-list 'load-path "~/.emacs.d")
(defvar siteinit-path (file-name-directory (locate-library "init")))

;;
;=======================================================================
; elisp の追加読み込み PATH
;=======================================================================
(add-to-list 'load-path (concat siteinit-path "elisp/init-loader") t)
(add-to-list 'load-path (concat siteinit-path "virtualenv") t)

;;
;=======================================================================
; Emacs Lisp Package Archive
;=======================================================================
(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t)
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
(package-initialize)

;;
;=======================================================================
; 設定ファイル分割フレームワーク
;
; - Project page
; https://github.com/emacs-jp/init-loader
;=======================================================================
(require 'init-loader)
;(init-loader-load "~/.emacs.d/inits")
(init-loader-load)

;;
;;
;=======================================================================
; grep-edit.el
; - grep の検索結果を直接編集する
;
; - Project homepage
; http://www.bookshelf.jp/soft/meadow_51.html#SEC782
;=======================================================================
;; M-x grep 後バッファを編集
;;   - C-c C-c で変更を適用 (or C-c C-e)
;;   - C-c C-u で変更の破棄
;;   - C-c C-r でリージョン内の変更の破棄
(require 'grep-edit)

(defun my-grep-edit-setup ()
  ;; (define-key grep-mode-map '[up] nil)
  (define-key grep-mode-map "\C-c\C-c" 'grep-edit-finish-edit)
  (message (substitute-command-keys "\\[grep-edit-finish-edit] to apply changes."))
  (set (make-local-variable 'inhibit-read-only) t)
  )
(add-hook 'grep-setup-hook 'my-grep-edit-setup t)

;;
;=======================================================================
; powershell-mode.el
; - Windows PowerShell 編集用のメジャーモード
;
; - Emacs wiki
; http://www.emacswiki.org/emacs/PowerShell
;=======================================================================
(require 'powershell-mode)
(add-to-list 'auto-mode-alist '("\\.ps1$" . powershell-mode))

;;
;=======================================================================
; minibuf-isearch.el
; - ミニバッファで isearch 的な検索をする
;
; - Project page
; http://www.sodan.org/~knagano/emacs/minibuf-isearch/
;=======================================================================
(require 'minibuf-isearch)
;; minibuf-isearch 中は migemo を利用しない
(setq minibuf-isearch-use-migemo nil)

;;
;=======================================================================
; session.el
; - ミニバッファの履歴をファイルに保存し、次回起動時にも持ち越せる
;
; - Project page
; http://emacs-session.sourceforge.net/
;=======================================================================
(require 'session)
(setq session-initialize '(de-saveplace session keys menus places)
      session-globals-include '((kill-ring 50)
                                (session-file-alist 500 t)
                                (file-name-history 10000)))
;; これがないと file-name-history に500個保存する前に max-string に達する
(setq session-globals-max-string 100000000)
;; デフォルトでは30!
(setq history-length t)
(add-hook 'after-init-hook 'session-initialize)

;;
;=======================================================================
; kill-summary.el
; - kill-ring の内容を一覧表示してそこから yank する
;
; - Project page
; http://emacs-session.sourceforge.net/
;=======================================================================
(autoload 'kill-summary "kill-summary" nil t)

;;
;=======================================================================
; dabbrev-ja.el
; - 日本語で dabbrev(動的略語補完) を使う
;=======================================================================
;(load "dabbrev-ja")

;;
;=======================================================================
; redo+.el
; - undo, redo を行えるようにする
;
; http://www11.atwiki.jp/s-irie/pages/18.html
;=======================================================================
;; 24.3 で動かなくなったのでコメントアウト
;(require 'redo+)
;(setq undo-no-redo t)

;;
;=======================================================================
; mercurial.el
; - Mercurial 付属の elisp
;
; C-c h h でキーバインディング一覧が見れる
;
; - 分かりやすい help
; http://www.lares.dti.ne.jp/~foozy/fujiguruma/scm/mercurial-emacs.html
;=======================================================================
(when (require 'mercurial nil t)
  ;; MQ (Mercurial Queue) で patch 管理をする
  (require 'mq nil t))

;;
;=======================================================================
; kanji-code.el
; - 日本語と Escaped Unicode を変換する
;=======================================================================
(autoload 'kanji-to-unicode-buffer "kanji-code" nil t)
(autoload 'kanji-to-unicode-region "kanji-code" nil t)
(autoload 'unicode-to-kanji-buffer "kanji-code" nil t)
(autoload 'unicode-to-kanji-region "kanji-code" nil t)

;;
;=======================================================================
; twittering-mode
;
; - Project wiki
; http://www.emacswiki.org/emacs/TwitteringMode/
;=======================================================================
(require 'twittering-mode)
(setq twittering-icon-mode t)                 ; Show icons

;;
;=======================================================================
; schema-mode using gauche
;=======================================================================
(setq scheme-program-name "gosh -i")
(autoload 'schema-mode "cmuscheme" "Major mode for Scheme." t)
(autoload 'run-mode "cmuscheme" "Run an inferior Scheme process." t)

(defun scheme-other-window ()
  "Run scheme on other window"
  (interactive)
  (split-window-horizontally)
  (switch-to-buffer-other-window
   (get-buffer-create "*scheme*"))
  (run-scheme scheme-program-name)
  (other-window -1))

;;
;=======================================================================
; キーカスタマイズ
;=======================================================================

(global-set-key "\C-h" 'backward-delete-char)  ;バックスペース
(global-set-key [zenkaku-hankaku]
                'toggle-input-method)          ;日本語入力
(global-set-key "\C-o" 'toggle-input-method)   ;日本語入力
(global-set-key "\C-s" 'isearch-forward-regexp);正規表現で検索
(global-set-key "\C-r" 'query-replace-regexp)  ;正規表現で置換
(global-set-key "\C-xt" 'create-tmp-buffer)    ;作業用バッファを作る
(global-set-key "\C-x\C-b" 'buffer-menu)       ;ウィンドウ分割しないバッファメニュー
(global-set-key "\C-\\" 'undo)                 ;undo
(global-set-key "\C-]" 'match-paren)           ;対応する括弧に移動
(global-set-key "\C-\M-y" 'kill-summary)       ;kill-ring 一覧から yank
(global-set-key "\M-k" '(lambda () (interactive) (kill-line 0))) ;;行頭まで削除
;(global-set-key "\M-d" 'kill-word-at-point)    ;カーソル下の単語を削除
(global-set-key "\M-g" 'goto-line)             ;指定行へ移動

(global-set-key [f1] 'help-for-help)           ;ヘルプ
(global-set-key [f7] 'next-error)              ;次のエラーを検索
(global-set-key [S-f7] 'previous-error)        ;前のエラーを検索
(global-set-key [f12] 'redo)                   ;redo
;(global-set-key [f12] 'setnu-mode)             ;行番号表示

(global-set-key "\C-cs" 'scheme-other-window)  ;scheme実行

;;
;=======================================================================
; Customizer
;=======================================================================

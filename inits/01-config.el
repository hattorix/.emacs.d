;;--------------------------------------------------
;; File name    :   01-init.el
;;              :   Emacs の基本的な設定
;;              :
;;--------------------------------------------------
;;
;=======================================================================
; 一般設定
;=======================================================================
(setq make-backup-files nil)                   ;バックアップファイルを作成しない
(setq auto-save-timeout 30)                    ;自動保存する間隔（秒）。
(setq auto-save-interval 300)                  ;300打鍵ごとに自動保存
(setq delete-auto-save-files t)                ;終了時にオートセーブファイルを消す
(setq visible-bell t)                          ;警告音を消す
(setq kill-whole-line t)                       ;カーソルが行頭にある場合は行全体を削除
(setq ring-bell-function 'ignore)              ;エラー音をならなくする
(setq delete-by-moving-to-trash t)             ;ごみ箱を有効
(setq-default indent-tabs-mode nil)            ;インデントにはスペースを使う
(setq require-final-newline t)                 ;ファイル末尾に改行追加
(setq mouse-drag-copy-region t)                ;マウス選択のみで kill-ring にコピー
(fset 'yes-or-no-p 'y-or-n-p)                  ;"yes or no"を"y or n"に
(setq load-prefer-newer t)                     ;.el と .elc の新しい方を読み込む

;; インデントに使用する関数を指定
(setq indent-line-function 'indent-relative-maybe)

;; yankのシステムへのコピー
(cond (window-system
       (setq x-select-enable-clipboard t)
       ))

;; ファイル名や URL の上で C-x C-f すると、そのファイル名や URL が開ける
(ffap-bindings)

;; Shift+カーソルキー で、分割したウィンドウ間を移動
(windmove-default-keybindings)
(setq windmove-wrap-around t)

;=======================================================================
; スクロール設定
;=======================================================================
;; スクロールの基本設定
(setq scroll-conservatively 15                ;画面の下端 (上端) で移動したときのスクロール量
      ;; scroll-step 1                           ;(同上)
      scroll-preserve-screen-position t       ;カーソル位置を維持する (Page-Up, Page-Down とか)
      scroll-margin 0                         ;上下マージン
      )

;; ホイールマウスでスクロールを有効に
(mouse-wheel-mode t)

;; スクロール時にカーソル位置を変えない
(setq scroll-preserve-screen-position t)

;; 画面スクロール時の重複行数
(setq next-screen-context-lines 1)

;; スクロールを加速させる
;(require 'scroll-speedup)

;; ホイールマウスのスクロール幅を設定（画面の８分の１）
(global-set-key [mouse-4] '(lambda () (interactive) (scroll-down (/ (window-height) 8))))
(global-set-key [mouse-5] '(lambda () (interactive) (scroll-up (/ (window-height) 8))))

;; 半ページ/スクロール
(global-set-key "\M-]" '(lambda () (interactive) (scroll-up (/ (window-height) 2))))
(global-set-key "\M-[" '(lambda () (interactive) (scroll-down (/ (window-height) 2))))

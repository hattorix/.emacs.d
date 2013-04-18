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

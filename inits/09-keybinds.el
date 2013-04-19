;;--------------------------------------------------
;; File name    :   09-keybinds.el
;;              :   キーバインド定義
;;              :
;;--------------------------------------------------
;;
;=======================================================================
; global key binding
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
(global-set-key "\M-g" 'goto-line)             ;指定行へ移動

(global-set-key [f1] 'help-for-help)           ;ヘルプ
(global-set-key [f7] 'next-error)              ;次のエラーを検索
(global-set-key [S-f7] 'previous-error)        ;前のエラーを検索

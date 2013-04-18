;;--------------------------------------------------
;; File name    :   windows-80-funcs.el
;;              :   NTEmacs の追加関数
;;              :
;;--------------------------------------------------
;;
;=======================================================================
; スクロール設定
;=======================================================================
;; ホイールマウスのスクロール幅を設定（画面の８分の１）
(global-set-key [wheel-up] '(lambda () (interactive) (scroll-down (/ (window-height) 8))))
(global-set-key [wheel-down] '(lambda () (interactive) (scroll-up (/ (window-height) 8))))

(setq twittering-curl-program
      (expand-file-name
       "curl.exe"
       (concat siteinit-path "windows")))


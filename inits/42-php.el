;;--------------------------------------------------
;; File name    :   42-php.el
;;              :   PHP のソースコード編集
;;              :
;;--------------------------------------------------
;;
;=======================================================================
; php-mode.el
; - php プログラミング用のメジャーモード
;
; - Project homepage
; http://sourceforge.net/projects/php-mode/
;=======================================================================
(require 'php-mode)

(add-hook 'php-mode-hook
          '(lambda ()
             ;; インデントスタイルの設定(gnu,bsd,k&r,stroustrup,linux,java)
             (c-set-style "stroustrup")
             ;; 連続する空白の一括削除
             (c-toggle-hungry-state t)
             ;; コメント行のインデント
             (setq c-comment-only-line-offset 0)
             ;; コメントのスタイル
             (setq comment-start "// "
                   comment-end   ""
                   comment-start-skip "// *")
             ;; `;' を入力したら、自動改行+インデント
             (c-toggle-auto-hungry-state 1)
             ;; インデント サイズの設定
             (setq tab-width 4
                   c-basic-offset 4
                   c-hanging-comment-ender-p nil
                   indent-tabs-mode nil)))

(add-to-list 'auto-mode-alist '("\\.php$" . php-mode))

;-----------------------------------------------
; PHP で flymake を使う
;-----------------------------------------------
(when (executable-find "php")
  (add-hook 'php-mode-hook 'flymake-mode-if-enable-buffer)
  (push '("\\.php[345]?$" flymake-php-init) flymake-allowed-file-name-masks)
  )


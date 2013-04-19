;;--------------------------------------------------
;; File name    :   42-javascript.el
;;              :   Java Script のソースコード編集
;;              :
;;--------------------------------------------------
;;
;=======================================================================
; js2-mode.el
; - java-script プログラミング用のメジャーモード
;
; - Project homepage
; http://code.google.com/p/js2-mode/
;=======================================================================
(add-to-list 'load-path (concat siteinit-path "elisp/js2-mode") t)
(autoload 'js2-mode "js2-mode" nil t)

;; インデントの関数の再設定
(add-hook 'js2-mode-hook
          #'(lambda ()
              (require 'js)

              ;; インデントスタイルを Google JavaScript Style に合わせる
              (set (make-local-variable 'indent-line-function) 'js-indent-line)

              ;; インデント サイズの設定
              (setq js-indent-level 2
                    js-expr-indent-offset 2
                    indent-tabs-mode nil)))

(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))

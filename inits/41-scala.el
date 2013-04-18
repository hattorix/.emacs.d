;;--------------------------------------------------
;; File name    :   41-scala.el
;;              :   Scala のソースコード編集
;;              :
;;--------------------------------------------------
;;
;=======================================================================
; scala-mode.el
; - Scala プログラミング用のメジャーモード
;=======================================================================
(add-to-list 'load-path (concat siteinit-path "scala-mode") t)
(require 'scala-mode-auto)

;-----------------------------------------------
; Scala で flymake を使う
;-----------------------------------------------
(when (executable-find "scalac")
  (defun flymake-scala-init ()
    (flymake-simple-generic-init
     "scalac" (list "-d" temporary-file-directory)))

  (push '("\\.scala$" flymake-scala-init) flymake-allowed-file-name-masks)
  ;; flymake-err-line-patterns は python の設定で吸収
  )

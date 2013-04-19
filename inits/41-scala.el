;;--------------------------------------------------
;; File name    :   41-scala.el
;;              :   Scala のソースコード編集
;;              :
;;--------------------------------------------------
;;
;=======================================================================
; scala-mode2
; - Scala プログラミング用のメジャーモード
;=======================================================================

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

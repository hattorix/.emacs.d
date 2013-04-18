;;
;=======================================================================
; cmake-mode.el
; - CMake ファイル編集用のメジャーモード
;=======================================================================
(when (require 'cmake-mode nil t)

   (add-to-list 'auto-mode-alist '("CMakeLists\\.txt$" . cmake-mode))
   (add-to-list 'auto-mode-alist '("\\.cmake$" . cmake-mode))
  )

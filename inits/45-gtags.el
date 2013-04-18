;;
;=======================================================================
; gtags.el
; - GNU Global を使ったタグジャンプ
;
; - Project page
; http://www.gnu.org/software/global/
;=======================================================================
(when (locate-library "gtags")
  (require 'gtags)

  ;; *GTAGS SELECT* バッファを一つしか生成しないようにする
  (setq gtags-select-buffer-single t)

  ;; gtags を使用するモード
  (add-hook 'java-mode-hook (lambda () (gtags-mode 1)))
  (add-hook 'c-mode-hook (lambda () (gtags-mode 1)))
  (add-hook 'c++-mode-hook (lambda () (gtags-mode 1)))
  )

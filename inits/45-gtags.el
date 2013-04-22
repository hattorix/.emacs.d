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

  ;; anything-gtags を有効にしている場合、
  ;; relative しないとおかしくなる
  (setq gtags-path-style 'relative)

  ;; gtags を使用するモード
  (add-hook 'java-mode-hook (lambda () (gtags-mode 1)))
  (add-hook 'c-mode-hook (lambda () (gtags-mode 1)))
  (add-hook 'c++-mode-hook (lambda () (gtags-mode 1)))
  )

;=======================================================================
; global key binding
;=======================================================================
(global-set-key [f10] 'gtags-find-tag-from-here);タグジャンプ
(global-set-key [S-f10] 'gtags-pop-stack)      ;バックタグジャンプ

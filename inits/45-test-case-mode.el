;;
;=======================================================================
; test-case-mode.el
; - テスト駆動開発支援
;
; - Project Homepage
; http://nschum.de/src/emacs/test-case-mode/
;=======================================================================
(require 'test-case-mode)
;(add-hook 'find-file-hook 'enable-test-case-mode-if-test)

;; コンパイル後に、テストの自動実行
(add-hook 'compilation-finish-functions 'test-case-compilation-finish-run-all)

;; test-case-mode のキーマップ
(define-key test-case-mode-map "\C-ct" 'test-case-run)

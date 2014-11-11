;;--------------------------------------------------
;; File name    :   20-helm.el
;;              :   helm 関連の設定
;;              :
;;--------------------------------------------------
;;
;=======================================================================
; helm.el
; - ファイルを開くを一元化
;
; - Github
; https://github.com/emacs-helm/helm
;=======================================================================
(add-to-list 'load-path (concat siteinit-path "elisp/helm") t)
(when (require 'helm-config nil t)
  ;;キーマップの変更とかをできるようにする
  (helm-mode 1)

  ;; git-ls
  (require 'helm-ls-git)

  ;; キーバインドを helm で表
  (require 'helm-descbinds)
  (helm-descbinds-mode 1)

  ;=====================================================================
  ; helm のキーマップ
  ;=====================================================================
  (define-key helm-map "\C-h" 'delete-backward-char)
  (define-key helm-find-files-map "\C-h" 'delete-backward-char)

  ;=====================================================================
  ; global key binding
  ;=====================================================================
  ;; helm の開始
  (global-set-key "\C-ch" 'helm-mini)
  (global-set-key (kbd "C-c o") 'helm-occur))

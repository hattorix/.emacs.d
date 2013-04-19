;;--------------------------------------------------
;; File name    :   43-powershell.el
;;              :   PowerShell のソースコード編集
;;              :
;;--------------------------------------------------
;;
;=======================================================================
; powershell-mode.el
; - Windows PowerShell 編集用のメジャーモード
;
; - Emacs wiki
; http://www.emacswiki.org/emacs/PowerShell
;=======================================================================
(autoload 'powershell-mode "powershell-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.ps1$" . powershell-mode))

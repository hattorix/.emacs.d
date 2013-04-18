;;--------------------------------------------------
;; File name    :   42-actionscript.el
;;              :   ActionScript のソースコード編集
;;              :
;;--------------------------------------------------
;;
;=======================================================================
; actionscript-mode
;
; - Project wiki
; http://www.emacswiki.org/emacs/ActionScriptMode/
;=======================================================================
(add-to-list 'auto-mode-alist '("\\.as\\'" . actionscript-mode))
(add-to-list 'auto-mode-alist '("\\.mxml\\'" . xml-mode))

;; mxml + Action Script
(mmm-add-classes
 '((embedded-as
    :submode actionscript-mode
    :face mmm-code-submode-face
    ;:front "<mx:Script[^>]*>\\n?\\s-*<!\\[CDATA\\["
    ;:back "]]>\\n?\\s-*</mx:Script>")))
    :front "<mx:Script>"
    :back "</mx:Script>")))
(mmm-add-mode-ext-class nil "\\.mxml\\'" 'embedded-as)


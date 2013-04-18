;;--------------------------------------------------
;; File name    :   01-display.el
;;              :   Emacs の表示設定
;;              :
;;--------------------------------------------------
;;
;=======================================================================
; 基本設定
;=======================================================================
(auto-compression-mode t)                      ;日本語infoの文字化け防止
(auto-image-file-mode t)                       ;画像ファイルを表示
(blink-cursor-mode 0)                          ;カーソルを点滅しないように
(global-font-lock-mode t)                      ;font-lockを有効
(set-scroll-bar-mode 'right)                   ;スクロールバーを右側に表示
(setq inhibit-startup-message t)               ;起動時の画面はいらない
(setq parse-sexp-ignore-comments t)            ;コメント内の括弧は無視
(setq show-paren-style 'mixed)                 ;対応する括弧がウィンドウ内に収まらないときに光らせる
(setq tab-stop-list '(4 8 12 16 20 24 28 32    ;タブストップ位置の設定
                        36 40 48 52 56 60 64
                        68 72 76 80))
(setq-default indicate-empty-lines t)          ;ファイルの終端以降を可視化
(setq-default indicate-buffer-boundaries 'right);右フリンジにバッファの開始と終端を表示
(setq-default tab-width 4)                     ;タブ幅を4に設定
(show-paren-mode t)                            ;対応する括弧をハイライト
(tool-bar-mode 0)                              ;ツールバーを表示しない

;; フレームのタイトル指定
(setq frame-title-format
      (concat "%b - emacs@" system-name))

;-----------------------------------------------
; 配色
;-----------------------------------------------

;; リージョン
(set-face-foreground 'region "black")
(set-face-background 'region "cyan2")
;; モードライン
(set-face-attribute  'mode-line nil
                     :foreground "#000000"
                     :background "grey"
                     :box nil)   ;モードラインを平面化
(set-face-attribute  'mode-line-inactive nil
                     :foreground "#000000"
                     :background "grey"
                     :box nil)

;-----------------------------------------------
; バッファのデフォルト表示設定
;-----------------------------------------------
(setq default-frame-alist
      (append (list
               '(top . 0)                      ;Y 表示位置
               '(left . 0)                     ;X 表示位置
               '(width . 110)                  ;フレームの幅
               '(height . 60)                  ;フレームの高さ
               '(alpha . 90)                   ;透明度
               '(foreground-color . "white")   ;文字色
               '(background-color . "#222222") ;背景色
               '(cursor-type  . box)           ;カーソルのタイプ
               '(cursor-color . "cyan2")       ;カーソル色
               '(border-color . "black")
               '(mouse-color . "white")
               )
              default-frame-alist))
(setq initial-frame-alist default-frame-alist)

;-----------------------------------------------
; モードライン
;-----------------------------------------------
;; 時計を表示
(setq display-time-24hr-format t)
(setq display-time-string-forms '(24-hours ":" minutes))
(display-time-mode t)

;; 行や列数号を表示
(line-number-mode t)
(column-number-mode t)

;-----------------------------------------------
; タブや全角スペース、行末のスペースを表示する
;
; - 行末のスペースは次のコマンドで削除できる
; M-x delete-trailing-whitespace
;-----------------------------------------------
(require 'whitespace)
(setq whitespace-style '(face
                         tabs tab-mark
                         spaces space-mark
                         newline newline-mark
                         trailing
                         ))
(setq whitespace-space-regexp "\\(\u3000+\\)")
(setq whitespace-display-mappings
      '((space-mark   ?\u3000 [?\u25a1])               ;全角スペース
        (tab-mark     ?\t     [?\xBB ?\t]   [?\\ ?\t]) ;タブ
        (newline-mark ?\n     [?\x21B5 ?\n] [?$ ?\n])  ;改行
        ))
(set-face-background 'whitespace-space 'nil)
(set-face-background 'whitespace-tab 'nil)
(set-face-attribute  'whitespace-trailing nil
                     :foreground "SteelBlue"
                     :background "#222222"
                     :underline  t)
(global-whitespace-mode t)

;上記のwhitespace-modeの改行表示とは共存不可
;(setq-default show-trailing-whitespace t)
;(set-face-attribute 'trailing-whitespace nil
;                    :foreground "SteelBlue"
;                    :background "#222222"
;                    :underline t)

;-----------------------------------------------
; カーソル行をハイライト
;-----------------------------------------------
(defface hlline-face
  '((((class color)
      (background dark))
     (:background "#004422"))
    (((class color)
      (background light))
     (:background "SkyBlue"))
    (t
     ()))
  "*Face used by hl-line.")
(setq hl-line-face 'hlline-face)  ; or 'underline
(global-hl-line-mode)

;-----------------------------------------------
; lineum
; - 行番号表示
;
; - Emacs wiki
; http://www.emacswiki.org/emacs/LineNumbers
; - 次のコマンドで行番号表示をトグル
; M-x linum-mode
;-----------------------------------------------
;(global-linum-mode 1)                   ;全バッファで行の表示

(eval-after-load 'linum
  '(linum-face-settings))

(defun linum-face-settings ()
  ;; 行部分のフォーマット
  (defun linum-format-func (line)
    (propertize (format
                 (let ((w (length (number-to-string
                                   (count-lines (point-min) (point-max))))))
                   (concat " %" (number-to-string w) "d")) line) 'face 'linum))
  (setq linum-format 'linum-format-func)

  ;; 色設定
  (set-face-attribute 'linum nil :background "SkyBlue4" :foreground "light gray")

  ;; linum-mode が重いのをどうにかする
  ;; - http://d.hatena.ne.jp/daimatz/20120215/1329248780
  (setq linum-delay t)
  (defadvice linum-schedule (around my-linum-schedule () activate)
    (run-with-idle-timer 0.2 nil #'linum-update-current))
  )


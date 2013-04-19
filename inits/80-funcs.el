;;--------------------------------------------------
;; File name    :   80-funcs.el
;;              :   関数定義
;;              :
;;--------------------------------------------------
;;
;=======================================================================
; function
;=======================================================================

;; カレントバッファを閉じる
(defun my-kill-buffer (all)
  (interactive "P")
  (if all
      ;prefix argument があれば全バッファを削除
      (loop for buffer being the buffers
            do (kill-buffer buffer))
    (kill-buffer nil)))

;; カレントバッファを閉じる
(defun my-revert-buffer (&optional coding-system)
  (interactive "zCoding system for visited file (default nil): \nP")
  (if coding-system
      ;prefix argument があればエンコード指定
      ;(revert-buffer-with-coding-system coding-system)
      t
    (revert-buffer t t)))

;; 作業用バッファを作る
(defvar tmp-buffer-count 0 "new bufferを作った数")
(defun create-tmp-buffer ()
  (interactive)
  (switch-to-buffer (if (= tmp-buffer-count 0)
                        "*tmp*"
                        ;TODO: 動的に数値を取得する
                        (concat "*tmp (" (number-to-string tmp-buffer-count) ")*")))
  (setq tmp-buffer-count (+ 1 tmp-buffer-count)))

;; カーソル位置の単語を検索
(defun search-word-cursor ()
  (interactive)
  (if (thing-at-point 'symbol)
      (occur (thing-at-point 'symbol))
    (call-interactively 'occur)))

;; カーソル位置の単語を削除
(defun kill-word-at-point ()
  (interactive)
  (let ((char (char-to-string (char-after (point)))))
    (cond
     ((string= " " char) (delete-horizontal-space))
     ((string-match "[\t\n -@\[-`{-~]" char) (kill-word 1))
     (t (forward-char) (backward-word) (kill-word 1)))))

;; 二分割されている画面を入れ替える
(defun swap-screen ()
  "Swap two screen,leaving cursor at current window."
  (interactive)
  (let ((thiswin (selected-window))
        (nextbuf (window-buffer (next-window))))
    (set-window-buffer (next-window) (window-buffer))
    (set-window-buffer thiswin nextbuf)))

;; 二分割されている画面、カーソルを入れ替える
(defun swap-screen-with-cursor ()
  "Swap two screen,with cursor in same buffer."
  (interactive)
  (let ((thiswin (selected-window))
        (thisbuf (window-buffer)))
    (other-window 1)
    (set-window-buffer thiswin (window-buffer))
    (set-window-buffer (selected-window) thisbuf)))

;; ウィンドウ二分割時に、縦分割<->横分割
(defun window-toggle-division ()
  "ウィンドウ 2 分割時に、縦分割<->横分割"
  (interactive)
  (unless (= (count-windows 1) 2)
    (error "ウィンドウが 2 分割されていません。"))
  (let (before-height (other-buf (window-buffer (next-window))))
    (setq before-height (window-height))
    (delete-other-windows)
    (if (= (window-height) before-height)
        (split-window-vertically)
      (split-window-horizontally)
      )
    (switch-to-buffer-other-window other-buf)
    (other-window -1)))

;; 対応する括弧に移動する
(defun match-paren (arg)
  "Go to the matching parenthesis if on parenthesis otherwise insert %."
  (interactive "p")
  (cond ((looking-at "\\s\(") (forward-list 1) (backward-char 1))
        ((looking-at "\\s\)") (forward-char 1) (backward-list 1))
;        (t (self-insert-command (or arg 1)))))
        ))

;; タブ幅の設定
(defun set-tab-width (num)
  (interactive "nTab Width: ")
  (make-local-variable 'tab-stop-list)
  (setq tab-width num)
  (setq tab-stop-list ())
  (while (< num 128)
    (add-to-list 'tab-stop-list num t)
    (setq num (+ num tab-width))))

;; 縦横幅に応じて画面を分割
(defun split-window-conditional ()
  (interactive)
  (if (> (* (window-height) 2) (window-width))
      (split-window-vertically)
    (split-window-horizontally)))

;; ウィンドウが分割されていたら次のウィンドウに移動、そうでなければ分割する
(defun other-window-or-split ()
  (interactive)
  (when (one-window-p)
    (split-window-conditional))
  (other-window 1))

;; 分割したウィンドウのリサイズ
(defun my-window-resizer ()
  "Control window size and position."
  (interactive)
  (let ((window-obj (selected-window))
        (current-width (window-width))
        (current-height (window-height))
        (dx (if (= (nth 0 (window-edges)) 0) 1
              -1))
        (dy (if (= (nth 1 (window-edges)) 0) 1
              -1))
        action c)
    (catch 'end-flag
      (while t
        (setq action
              (read-key-sequence-vector (format "size[%dx%d]"
                                                (window-width)
                                                (window-height))))
        (setq c (aref action 0))
        (cond ((= c ?l)
               (enlarge-window-horizontally dx))
              ((= c ?h)
               (shrink-window-horizontally dx))
              ((= c ?j)
               (enlarge-window dy))
              ((= c ?k)
               (shrink-window dy))
              ;; otherwise
              (t
               (let ((last-command-char (aref action 0))
                     (command (key-binding action)))
                 (when command
                   (call-interactively command)))
               (message "Quit")
               (throw 'end-flag t)))))))

;; 折り返し表示をトグルする
(defun toggle-truncate-lines ()
  "折り返し表示をトグルする."
  (interactive)
  (if truncate-lines
      (setq truncate-lines nil
            truncate-partial-width-windows nil)
    (setq truncate-lines t
          truncate-partial-width-windows t))
  (recenter))

;;----------------------------------------------
;; カーソル行を上下に移動
;;----------------------------------------------
; 下に移動
(defun transpose-lines-down (&optional n)
  "カーソル行を下に移動"
  (interactive "p")
  (if (not n)
      (setq n 1))
  (when (save-excursion (forward-line n))
    (let ((column (current-column))
          (beg (progn (move-beginning-of-line 1) (point)))
          (end (progn (forward-line 1) (point))))
      (insert (prog1
                  (buffer-substring beg end)
                (delete-region beg end)
                (forward-line n)))
      (forward-line -1)
      (move-to-column column))))

; 上に移動
(defun transpose-lines-up (&optional n)
  "カーソル行を上に移動"
  (interactive "p")
  (if (not n)
      (setq n 1))
  (transpose-lines-down (- n)))

;;
;=======================================================================
; キーカスタマイズ
;=======================================================================
(global-set-key "\C-cl" 'toggle-truncate-lines);折り返し表示のトグル
(global-set-key "\C-cr" 'my-window-resizer)    ;ウィンドウのリサイズ
(global-set-key "\C-x\C-g" 'my-revert-buffer)  ;カレントバッファを再読み込み
(global-set-key "\C-xk" 'my-kill-buffer)       ;カレントバッファを閉じる
(global-set-key [?\C-:] 'search-word-cursor)   ;カーソル下の単語で検索
(global-set-key [C-tab] 'other-window-or-split);次のウィンドウか分割か
(global-set-key [M-down] 'transpose-lines-down);カーソル行を下に移動
(global-set-key [M-up] 'transpose-lines-up)    ;カーソル行を上に移動

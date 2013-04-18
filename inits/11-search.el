;=======================================================================
; isearch
;=======================================================================
;; isearch 中に、カーソル付近の文字を 1 文字ずつ追加
(defun isearch-yank-char ()
  "Pull next character from buffer into search string."
  (interactive)
  (isearch-yank-string
   (save-excursion
     (and (not isearch-forward) isearch-other-end
          (goto-char isearch-other-end))
     (buffer-substring (point) (1+ (point))))))

;; C-w で追加した後でも、一文字ずつ消す
(defun isearch-real-delete-char ()
  (interactive)
  (setq isearch-string
        (if (< (length isearch-string) 1)
            ""
          (substring isearch-string 0 (- (length isearch-string) 1)))
        isearch-message isearch-string
        isearch-yank-flag t)
  (isearch-search-and-update))

;; isearch で検索している単語で occur をする
(defun isearch-occur ()
  "Invoke `occur' from within isearch."
  (interactive)
  (let ((case-fold-search isearch-case-fold-search))
    (occur
     (if isearch-regexp
         isearch-string (regexp-quote isearch-string)))))

(define-key isearch-mode-map "\C-f" 'isearch-yank-char)
(define-key isearch-mode-map "\C-h" 'isearch-real-delete-char)
(define-key isearch-mode-map "\C-l" 'isearch-edit-string)      ;C-lでキーワードの編集
(define-key isearch-mode-map "\C-o" 'isearch-occur)

;;; key-bind-logger.el

;; Author: Ryo Takaishi
;; Keywords: tools
;; Version: 0.1


(defgroup key-bind-logger nil
  "key bind logger"
  :group 'tools)

(defcustom key-bind-logger-save-directory "~/.emacs.d/"
  ""
  :group 'key-bind-logger
  :type 'directory)

(defcustom key-bind-logger-save-interval 10
  ""
  :group 'key-bind-logger
  :type 'integer)

(defcustom key-bind-logger-split-window-horizontally t
  ""
  :group 'key-bind-logger
  :type 'boolean)

(defcustom key-bind-logger-count-limit 100
  ""
  :group 'key-bind-logger
  :type 'integer)

(defconst key-bind-logger-log-file "key-bind-logger")
(defconst key-bind-logger-buf "*key-bind-logger*")
 
(defvar key-bind-logger-log-alist nil)
(defvar key-bind-logger-update-flag nil)
(defvar key-bind-logger-timer-obj nil)

;; require時にログファイルを読み込む
(let ((file (expand-file-name
	     key-bind-logger-log-file key-bind-logger-save-directory)))
  (when (file-exists-p file)
    (with-temp-buffer
      (insert-file-contents file)
      (setq key-bind-logger-log-alist (read (current-buffer))))))


  (add-hook 'post-command-hook 'key-bind-logger-count-command)
  (add-hook 'kill-emacs-hook 'key-bind-logger-save-file)

(defun key-bind-logger (&optional clear)
  (interactive "P")
  (key-bind-logger-get-key-bindings clear)
  (save-selected-window
    (unless (get-buffer-window key-bind-logger-buf)
      (split-window nil nil key-bind-logger-split-window-horizontally))
    (pop-to-buffer key-bind-logger-buf))
  (key-bind-logger-update-log)
  (when key-bind-logger-timer-obj
    (cancel-timer key-bind-logger-timer-obj))
  (setq key-bind-logger-timer-obj
	(run-at-time
	 t key-bind-logger-save-interval 'key-bind-logger-save-file)))

;;メジャーモードのバインディングを読み込み
(defun key-bind-logger-get-key-bindings (clear)
  (let* ((indent-tabs-mode t)   ;;字下げにはTABを使用
	 (buf (current-buffer))
	 (mode major-mode)
	(alist (assq mode key-bind-logger-log-alist))) 
  (when clear
    (setq key-bind-logger-log-alist (delq alist key-bind-logger-log-alist))
    (setq alist nil))
    ;;key-bind-logger-log-alistにmodeが無ければバインディングを読み込み
    (unless alist
      (with-temp-buffer
	(describe-buffer-bindings buf)
	(goto-char (point-min))
	(let ((beg (re-search-forward
		    "^Major Mode Bindings:\\(.\\|\n\\)+?-$" nil t))
	      (end (re-search-forward "^Global Bindings:" nil t))
	      l)
	  (when (and beg end)
	    (narrow-to-region beg end)
	    (goto-char (point-min))
	    ;;ここでバインディングを読み込み
	    (while (re-search-forward
		    "^\\([^\t\n]+\\)\t+\\([^\t\n]+\\)$" nil t)
	      (setq l (cons (list (match-string 2) (match-string 1) 0) l)))
	    ;;key-bind-logger-log-alistと結合
	    (setq key-bind-logger-log-alist
		  (cons (cons mode (nreverse l))
			key-bind-logger-log-alist))))))))

;;ログをファイルに保存
(defun key-bind-logger-save-file ()
  ;;key-bind-logger-update-flagがtの場合のみ
  (when key-bind-logger-update-flag
    (with-temp-file
	(expand-file-name
	 key-bind-logger-log-file key-bind-logger-save-directory)
      (prin1 key-bind-logger-log-alist (current-buffer)))
    (setq key-bind-logger-update-flag nil)))

;;コマンド実行時に実行回数を更新
(defun key-bind-logger-count-command ()
  (when (symbolp this-command)
    (let ((l (assoc (symbol-name this-command)
		    (cdr (assq major-mode key-bind-logger-log-alist)))))
      (when l
	(setcar (cddr l) (1+ (nth 2 l)))
	(setq key-bind-logger-update-flag t)))
    (unless (string-equal (buffer-name) key-bind-logger-buf)
      (key-bind-logger-update-log))))

;;コマンド実行時にログの更新
(defun key-bind-logger-update-log ()
  (let ((mode major-mode))
    (with-current-buffer (get-buffer-create key-bind-logger-buf)
      (erase-buffer)
      (dolist (l (cdr (assq mode key-bind-logger-log-alist)))
	;;一定回数以上使ったコマンドは表示しない
	(when (< (nth 2 l) key-bind-logger-count-limit)
	  (insert (format "%-16s %-50s %3d\n"
			  (nth 1 l) (nth 0 l) (nth 2 l))))))))

(provide 'key-bind-logger)

;; -*- Mode: Emacs-Lisp ; Coding: utf-8 -*-

;;; org-toodledo.el --- synchronize org-mode to toodledo

;; Copyright (C) 2009 Ryo Takaishi

;; Author: Ryo Takaishi <r_takaishi@eiliant.com>
;; Created: 14 Dec 2009
;; Version: 0.2
;; Keywords: org-mode toodledo todo

;; USAGE NOTES:
;; From Takaishi's web-site:
;; http://d.hatena.ne.jp/r_takaishi/?of=15
;; You need to look up the unique user ID in toodledo's account settings and
;; put this in your .emacs
;; (Require 'org-toodledo)
;; (setq org-toodledo-unique-uid "This unique user ID")
;; (setq org-toodledo-password "password ")
;; (setq org-toodledo-files' (" / here / file / path. org "))
;;
;; The following data can be synchronized:
;; Title
;; Tag
;; Start date
;; Period
;; Priority (only for stage 3. toodledo org-mode in three cases in the A, C -1 is now)
;;
;; Exposed commands:
;; org-toodledo-pull 
;; org-toodledo-push to push it 

;; The required "sexpath.el" is now available 
;; from http://cx4a.sakura.ne.jp/blog/2009/01/s-xpath-elisp.html
;; and on github.
;; See for author's page
;; http://www.cx4a.org/

;; ISSUES:
;; THIS IS NOT WORKING PROPERLY YET AND CAN EASILY DELETE ALL YOUR ORG-TOODLEDO-PUSH ENTRIES! BACKUP BEFORE USE.

;;; Code:

(require 'md5)
(require 'xml-parse)
(require 'sexpath)
(require 'url-http)

(defgroup org-toodledo nil
  "org toodledo"
  :group 'tools)

(defcustom org-toodledo-unique-uid ""
  "Toodledo unique user ID のユニークユーザID．"
  :group 'org-toodledo
  :type 'string)

(defcustom org-toodledo-password ""
  "Toodledo password のパスワード．"
  :group 'org-toodledo
  :type 'string)

(defcustom org-toodledo-files '("")
  "Synchronize Files 同期するファイル．"
  :group 'org-toodledo
  :type 'sexp)

(defvar org-toodledo-token nil)
(defvar org-toodledo-key nil)
(defvar org-toodledo-folders nil)
(defvar org-toodledo-tasks nil)
(defvar org-toodledo-folder-list nil)

(defun org-toodledo-get-token (org-toodledo-unique-uid)
  (let ((buf (url-retrieve-synchronously (concat
					  "http://api.toodledo.com/api.php?method=getToken;userid="
					  org-toodledo-unique-uid)))
	token)
    (setq token (with-current-buffer buf
		  (let ((coding-system-for-read 'utf-8-unix))
		    (goto-char (point-min))
		    (if (re-search-forward"<token>\\(.*\\)</token>"
					  nil t)
			(match-string 1)
		      nil))))
    (kill-buffer buf)
    token))

;;;; get key
(defun toodledo-get-key (org-toodledo-token)
  (md5 (md5 (concat (md5 org-toodledo-password) token org-toodledo-unique-uid))))


;;------------------------------------------------------------------------------
;; General API
;;------------------------------------------------------------------------------

;; Get Folder フォルダリストの取得
(defun org-toodledo-get-folders-list ()
  (let ((host "api.toodledo.com")
	(request-uri (concat "/api.php?method=getFolders;key="
			     org-toodledo-key))
	l)
    (with-temp-buffer
      (let ((buf (current-buffer)))
	(setq proc (open-network-stream
		    "*temp*"
		    buf
		    host
		    80))
	(set-process-coding-system proc 'binary 'binary)
	(display-buffer buf 'pop-up-window)
	(process-send-string
	 proc
	 (format (concat
		  "GET "
		  request-uri
		  " HTTP/1.0\r\n"
		  "MINE-Version: 1.0\r\n"
		  "\r\n")))
	(accept-process-output proc 10)
	(goto-char (point-min))
	(while (re-search-forward
		"<folder id=\"\\([0-9]*\\)\" private=\"[0-9]*\" archived=\"[0-9]*\" order=\"[0-9]*\">\\([a-zA-Z0-9]*\\)</folder>" nil t)
	    (setq l (cons (list (match-string 1) (match-string 2)) l)))
	l))))

  
;; Out of the Folder フォルダの取り出し
;; SXML Returns SXMLを返す
(defun toodledo-get-folders (org-toodledo-folder-list)
  (if (equal org-toodledo-folder-list nil)
      (setq org-toodledo-folder-list (org-toodledo-get-folders-list)))
  (let ((host "api.toodledo.com")
	(request-uri (concat "/api.php?method=getFolders;key=" org-toodledo-key)))
    (with-temp-buffer
      (let ((buf (current-buffer)))
	(setq proc (open-network-stream
		    "*temp*"
		    buf
		    host
		    80))
	(set-process-coding-system proc 'binary 'binary)
	(display-buffer buf 'pop-up-window)
	(process-send-string
	 proc
	 (format (concat
		  "GET "
		  request-uri
		  " HTTP/1.0\r\n"
		  "MINE-Version: 1.0\r\n"
		  "\r\n")))
	(accept-process-output proc 5)
	(goto-char (point-min))
	(re-search-forward
	 (concat "<folder id=\"\\([0-9]*\\)\" private=\"[0-9]*\" archived=\"[0-9]*\" order=\"[0-9]*\">" folder "</folder>"))
	(match-string 1)))))

;; Get Folder Name フォルダ名の取得
(defun org-toodledo-get-folder-name (folder-id)
  (if (equal org-toodledo-folder-list nil)
      (setq org-toodledo-folder-list (org-toodledo-get-folders-list)))
  (car (assoc folder-id org-toodledo-folder-list)))

;; Get Folder ID フォルダIDの取得
(defun org-toodledo-get-folder-id (folder-name)
  (if (equal org-toodledo-folder-list nil)
      (setq org-toodledo-folder-list (org-toodledo-get-folders-list)))
  (car (rassoc (list folder-name) org-toodledo-folder-list)))

(defun org-toodledo-check-setting ()
  ;; org-toodledo-unique-uid is set to ensure that... が設定されているか確認
  (when (string= org-toodledo-unique-uid nil)
    (error "org-toodledo-unique-uid is not set が設定さていません．"))
  ;; org-toodledo-password is set to ensure that... が設定されているか確認
  (when (string= org-toodledo-password nil)
    (error "org-toodledo-password has not been set が設定されていません．"))
  ;; org-toodledo-files verify that the set が設定されているかどうか確認
  (when (equal org-toodledo-files nil)
    (error "org-toodledo-files is not set が設定されていません．"))
  ;; org-toodledo-token verify that the obtained を取得しているかどうか確認
  (when (string= org-toodledo-token nil)
    (message "org-toodledo-token do not have to getを持っていないので取得します．")
    ;; org-toodledo-token acquisition の取得
    (setq org-toodledo-token (org-toodledo-get-token org-toodledo-unique-uid))
    (message "org-toodledo-token: %s" org-toodledo-token)
    ;; org-toodledo-key acquistion の取得
    (setq org-toodledo-key (md5 (concat (md5 org-toodledo-password) org-toodledo-token org-toodledo-unique-uid)))
    (message "key: %s" org-toodledo-key)))

;;------------------------------------------------------------------------------
;; Push
;;------------------------------------------------------------------------------

(defvar all-tasks nil)
(defun org-toodledo-push ()
;;  (error "THIS IS NOT WORKING PROPERLY YET AND CAN EASILY DELETE ALL YOUR ORG-TOODLEDO-PUSH ENTRIES! BACKUP BEFORE USE.")
  (interactive)
  (org-toodledo-check-setting)
  (mapcar 'org-toodledo-push-file org-toodledo-files))

(defun org-toodledo-push-file (file)
  (let ((buf (find-file-noselect file))
	;; All remote tasks (except tasks deleted) get リモートから全てのタスク(削除されたタスクは除く)を取得
	(tasks (org-toodledo-get-all-tasks))
	properties
	title
	id
	folder
	point
	url)
    (with-current-buffer buf
      ;; Search tasks that were deleted 削除されたタスクの探索
      (org-toodledo-push-search-deleted-task buf tasks)
      (goto-char (point-min))
      ;; Begin the task of checking each piece of local ローカルのタスクを一個ずつチェック開始
      (while (re-search-forward
	      "^\*+ \\(TODO\\|DONE\\) \\(\\[#.\\]\\)?[ ]?\\([^\t\n\r\f]*\\)\t*\\([^\t\n\r\f]*\\)$" nil t)
	(setq point (point))
	(setq url (concat "/api.php?method=addTask;key=" org-toodledo-key ";"))
	
	;;title acquisition の取得
	(if (setq title (match-string-no-properties 3))
	    (if (> (length title) 255)
		(error "Over 255 character!\n")
	      (progn
		(replace-regexp-in-string "&" "%26" title)
		(replace-regexp-in-string ";" "%3B" title)
		;; TODO probably also need "#" code
		(setq url (concat url "title=" (encode-coding-string title 'utf-8) ";")))))
	(setq properties (org-entry-properties (point) 'all))
	
	;; idを持っているかどうか．
	;; Id if you have.
	(if (not (equal (setq id (cdr (assoc "ID" properties))) nil))
	    (progn
	      ;;持っていた場合の動作
	      ;; Behavior when had
	      (message "tasks compared to, remotely update any Push. tasks と比較して，更新があればリモートにPush")
	      (compare-local-to-remote tasks properties title)
	      )
	  (progn
	    ;;持っていない場合の動作
	    ;; Operation does not have
	    (re-search-backward "^*+ \\([^\t\n\r\f ]*\\)$")
	    (setq folder (match-string-no-properties 1))
	    (setq url (concat url "folder=" (org-toodledo-get-folder-id folder) ";"))
	    (goto-char point)
	    ;; titleの取得
	    ;; Title acquisition
	    (org-toodledo-get-tags-local properties url)
	    (org-toodledo-get-todo-local properties url)
	    (org-toodledo-get-startdate properties url)
	    (org-toodledo-get-duedate properties url)
	    (org-toodledo-get-priority properties url)
	    (setq id (org-toodledo-http-push url id))
	    (org-entry-put (point) "ID" id)))))))
  

(defun org-toodledo-push-search-deleted-task (buf tasks)
  (mapcar (lambda (task)
	    (let ((id-remote (nth 1 (assoc "id" task)))
		  (flag nil))
	      (goto-char (point-min))
	      (while (re-search-forward
		      "^\*+ \\(TODO\\|DONE\\) \\(\\[#.\\] \\)?\\([^\t\n\r\f]*\\)\t*\\([^\t\n\r\f]*\\)$" nil t)
		(when (string= (org-entry-get (point) "ID") id-remote) 
		  (message (format "%s %s" (org-entry-get (point) "ID") id-remote))
		  (setq flag t)))
	      (if (eq flag nil)
		  (org-toodledo-delete-task-remote id-remote)))) tasks))

(defun org-toodledo-delete-task-remote (id)
  (let ((host "api.toodledo.com")
	(uri (concat "/api.php?method=deleteTask;" "key=" org-toodledo-key ";id=" id)))
    (with-temp-buffer
      (let ((coding-system-for-read 'utf-8-unix)
	    (buf (current-buffer)))
	(setq proc (open-network-stream
		    "*temp*"
		    buf
		    host
		    80))
	(set-process-coding-system proc 'binary 'binary)
	(display-buffer (current-buffer))
	(process-send-string
	 proc
	 (format (concat
		  "GET "
		  uri
		  " HTTP/1.0\r\n"
		  "MINE-Version: 1.0\r\n"
		  "\r\n")))
	(accept-process-output proc 5)
	(goto-char (point-min))
	(if (re-search-forward "<success>\\(.\\)</success>" nil t)
	    (match-string 1)
	  "error")))))

(defun compare-local-to-remote (tasks properties title-local)
  (mapcar '(lambda (task)
	     (let ((update-flag nil)
		   (buf nil)
		   (query nil)
		   (id-remote (nth 1 (assoc "id" (cdr task))))
		   (completed-remote (if (nth 1 (assoc "completed" (cdr task)))
					 "1"
				       "0"))
		   (title-remote (decode-coding-string (nth 1 (assoc "title" (cdr task))) 'utf-8))
		   (tags-remote (nth 1 (assoc "tasg" (cdr task))))
		   (startdate-remote (nth 1 (assoc "startdate" (cdr task))))
		   (duedate-remote (nth 1 (assoc "duedate" (cdr task))))
		   (priority-remote (nth 1 (assoc "priority" (cdr task))))
		   (id-local (cdr (assoc "ID" properties)))
		   (completed-local (org-toodledo-convert-completed (cdr (assoc "TODO" properties))))
		   (tags-local (cdr (assoc "TAGS" properties)))
		   (startdate-local (cdr (assoc "SCHEDULED" properties)))
		   (duedate-local (cdr (assoc "DEADLINE" properties)))
		   (priority-local (cdr (assoc "PRIORITY" properties))))
	       (when (string= id-remote id-local)
		 (setq query (concat "id=" id-remote ";"))
		 ;; IDが同じタスクがリモートにあった場合
		 ;; ID If the same tasks remotely
		 ;; タイトルの比較
		 ;; Compare Title
		 (unless (string= title-remote title-local)
		   (progn (concat "title=" (encode-coding-string title-local 'utf-8) ";")
			  (setq update-flag t)))
		 (unless (string= completed-remote completed-local)
		   (progn (setq query (org-toodledo-get-todo-local properties query))
			  (setq update-flag t)))
		 ;; タグの比較
		 ;; Comparison tags
		 (unless (string= tags-remote tags-local)
		   (progn (setq query (org-toodledo-get-tags-local properties query))
			  (setq update-flag t)))
		 ;; 開始日の比較
		 ;; Comparison start date
		 (unless (string= startdate-remote startdate-local)
		   (progn (setq query (org-toodledo-get-startdate properties query))
			  (setq update-flag t)))
		 ;; 終了日の比較
		 ;; Comparisons end date
		 (unless (string= duedate-remote duedate-local)
		   (progn (setq query (org-toodledo-get-duedate properties query))
			  (setq update-flag t)))
		 ;; 優先度の比較
		 ;; Comparison Priority
		 (unless (string= priority-remote priority-local)
		    (progn (setq query (org-toodledo-get-priority properties query))
			   (setq update-flag t)))
		 ;; (setq query (substring query 0 (- (length query) 1)))
		 (when (equal update-flag t)
		   (setq buf (url-retrieve-synchronously (concat "http://api.toodledo.com/api.php?method=editTask;key=" org-toodledo-key ";" query)))
		   (with-current-buffer buf
		     (display-buffer buf)
		     (goto-char (point-min))

		     (re-search-forward "<success>\\(.*\\)</success>" nil t)
		     (match-string 1))))))
	  tasks))


(defun org-toodledo-get-todo-local (properties url)
  (let (todo)
    (if (setq todo (cdr (assoc "TODO" properties)))
	(cond ((string= todo "TODO") (setq todo "1"))
	      ((string= todo "DONE") (setq todo "0")))
      (setq todo "0"))
      (setq url (concat url "completed=" todo))))
	      
	      

(defun org-toodledo-get-title-local (properties url)
  (let (title)
    (if (setq title (match-string-no-properties 2))
	(if (> (length title) 255)
	    (error "Over 255 character!\n")
	  (progn
	    (replace-regexp-in-string "&" "%26" title)
	    (replace-regexp-in-string ";" "%3B" title)
	    (setq url (concat url "title=" title ";")))))))

(defun org-toodledo-get-tags-local (properties url)
  (let (tags)
    (if (setq tags (cdr (assoc "TAGS" properties)))
	(if (> (length tags) 64)
	    (error "Over 64character!\n")
	  (progn
	    (replace-regexp-in-string "&" "%26" title)
	    (replace-regexp-in-string ";" "%3B" title)
	    (replace-regexp-in-string ":" " " tags)
	    (setq url (concat url "tag=" tags ";")))))))

(defun org-toodledo-get-startdate (proeprties url)
  (let (startdate)
    (when (setq startdate (cdr (assoc "SCHEDULED" properties)))
      (string-match "\\(....-..-..\\) ." startdate)
      (setq url (concat url "startdate=" (match-string 1 startdate) ";")))))


(defun org-toodledo-get-duedate (properties url)
  (let (duedate)
    (when (setq duedate (cdr (assoc "DEADLINE" properties)))
      (string-match "\\(....-..-..\\) ." duedate)
      (setq url (concat url "duedate=" (match-string 1 duedate) ";")))))

(defun org-toodledo-get-priority (properties url)
  (let (priority)
    (if (setq priority (cdr (assoc "PRIORITY" properties)))
 	(cond ((string= priority "A") (setq priority (replace-regexp-in-string "A" "2" priority)))
	      ((string= priority "B") (setq priority (replace-regexp-in-string "B" "1" priority)))
	      ((string= priority "C") (setq priority (replace-regexp-in-string "C" "0" priority))))
      (setq priority "1"))
    (setq url (concat url "priority=" priority))))


(defun org-toodledo-convert-priority (priority)
  (cond ((string= priority "A") (setq priority "2"))
	((string= priority "B") (setq priority "1"))
	((string= priority "C") (setq priority "0"))
	((string= priority "3") (setq priority "A"))
	((string= priority "2") (setq priority "A"))
	((string= priority "0") (setq priority "B"))
	((string= priority "1") (setq priority "C"))
	((string= priority "-1") (setq priority "C"))))

(defun org-toodledo-convert-completed (completed)
  (cond ((string= completed "TODO") (setq completed "1"))
	((string= completed "DONE") (setq completed "0"))
	((string= completed "1") (setq completed "DONE"))
	((string= completed "0") (setq completed "TODO"))))
	

(defun org-toodledo-http-push (request-uri id)
  (with-temp-buffer
    (let ((buf (current-buffer)))
      (setq proc (open-network-stream
		  "*temp*"
		  buf
		  "api.toodledo.com"
		  80))
      (set-process-coding-system proc 'binary 'binary)
      (display-buffer buf)
      ;;(display-buffer buf 'pop-up-window)
      (process-send-string
       proc
       (format (concat
		"GET "
		request-uri
		" HTTP/1.0\r\n"
		"MINE-Version: 1.0\r\n"
		"\r\n")))
      (accept-process-output proc 5)
      (goto-char (point-min))
      (re-search-forward
		  "<added>\\(.*\\)</added>" nil t)
;;      (goto-char (re-search-forward
;;		  "<added>\\(.*\\)</added>" nil t))
      (setq id (match-string 1)))))



;;------------------------------------------------------------------------------
;; Pull
;;------------------------------------------------------------------------------

(defun org-toodledo-pull ()
;;  (error "THIS IS NOT WORKING PROPERLY YET AND CAN EASILY DELETE ALL YOUR ORG-TOODLEDO-PULL ENTRIES! BACKUP BEFORE USE.")
  (interactive)
  (org-toodledo-check-setting)
  (mapcar 'org-toodledo-pull-file org-toodledo-files))

(defun org-toodledo-pull-file (file)
  (if (equal org-toodledo-folders nil)
      (setq org-toodledo-folders (org-toodledo-get-folders-list)))
  (let ((buf (find-file-noselect file))
	tasks
	ids)
    (with-current-buffer buf
      (setq tasks (org-toodledo-get-all-tasks))
      ;;(setq tasks all-tasks)
      (org-toodledo-pull-delete-task tasks)
      (mapcar 'org-toodledo-search-id-in-buffer tasks))))

(defun org-toodledo-pull-delete-task (tasks)
  ;; ローカルのタスクがリモートにあるか確認．
  ;; Check if the remote local tasks.
  ;; リモートにない場合，削除．
  ;; If it is not remote, removed.
  (let (id
	org-id
	properties
	start
	end
	point
	(flag nil))
    (goto-char (point-min))
    (while (re-search-forward
	    "^\*+ \\(TODO\\|DONE\\) \\(\\[#.\\] \\)?\\([^\t\n\r\f]*\\)\t*\\([^\t\n\r\f]*\\)$"
	    nil t)
      (message (match-string-no-properties 3))
      (setq point (point))
      (beginning-of-line nil)
      (setq start (point))
      (goto-char point)
      (setq org-id (org-entry-get (point) "ID"))
      (setq flag nil)
      (mapcar '(lambda (tasks)
		 (let (id)
		   (setq id (nth 1 (assoc "id" (cdr tasks))))
		   (when (string= id org-id)
		     (setq flag t))))
	      tasks)
      (when (equal flag nil)
	(re-search-forward
	 "^\*+ \\(TODO\\|DONE\\) \\(\\[#.\\] \\)?\\([^\t\n\r\f]*\\)\t*\\([^\t\r\f\n]*\\)$"
	 nil t)
	(beginning-of-line nil)
	(setq end (point))
	(delete-region start end)))))
	    
(defun org-toodledo-search-id-in-buffer (task)
  (let (id  
	org-id
	(folder (nth 1 (assoc (nth 1 (assoc "folder" (cdr task))) org-toodledo-folders))) ;; taskの所属フォルダ ;; task belongs folder
	properties
	(flag nil))

    ;; taskの所属フォルダを探索
    ;; Task belongs to explore a folder
    (goto-char (point-min))
    (unless (re-search-forward (concat "^*+ " folder "$") nil t)
      (goto-char (point-max))
      (insert (concat "* " folder))
      (newline))

    (setq id (nth 1 (assoc "id" (cdr task))))

    ;; taskを探索
    ;; Task to explore
    ;; バッファの終端まで順に探索
    ;; Discovery order until the end of the buffer
    (goto-char (point-min))
    (while (re-search-forward
	    "^\*+ \\(TODO\\|DONE\\) \\(\\[#.\\] \\)?\\([^\t\n\r\f]*\\)\t*\\([^\t\n\r\f]*\\)$"
	    nil t)
      ;; propertiesを取得
      ;; Properties get
      (setq properties (org-entry-properties (point) 'all))
      ;; IDを比較
      ;; ID comparison
      (when (setq org-id (cdr (assoc "ID" properties)))
	;; IDが同じ(バッファ中にtaskが存在する場合)
	;; ID are the same (if it exists in the buffer task)
	(when (string= id org-id)
	  ;; ローカルにタスクが存在している場合，各プロパティを比較して更新があるかどうか確認する．
	  ;; If there is a local task, to determine whether there is an update to compare each property.
	  (message "determine if there is an update to compare properties that match it. 一致したので各プロパティを比較して更新があるかどうか確認")
	  (org-toodledo-change-property task properties)
	  (setq flag t))))

    ;; ローカルにタスクが存在していない場合
    ;; If the task does not exist locally
    (when (equal flag nil)
      (goto-char (point-max))
      ;; 所属フォルダの行まで戻る
      ;; Folder back to the line belongs
      (re-search-backward (concat "^\*+ " folder "$"))
      ;; 行末へ移動
      ;; Go to end of line
      (end-of-line)
      ;; 改行の挿入
      ;; Insert line breaks
      (newline)
      (if (equal (nth 1 (assoc "title" (cdr task))) nil)
	(insert (concat "** TODO " ))
	(insert (concat "** TODO " (decode-coding-string (nth 1 (assoc "title" (cdr task))) 'utf-8-unix))))
      ;; idを追加
      ;; Id add
      (org-entry-put (point) "ID" id)
      ;; tagがあれば追加
      ;; Tag if there is additional
      (if (nth 1 (assoc "tag" task))
	  (org-set-tags-to (nth 1 (assoc "tag" task))))
      ;; startdateがあれば追加
      ;; Startdate if any additional
      (when (nth 1 (assoc "startdate" task))
	(org-add-planning-info 'scheduled (org-read-date nil t (nth 1 (assoc "startdate" task)) nil nil nil)))
      ;; duedateがあれば追加
      ;; Duedate if any additional
      (if (nth 1 (assoc '("duedate") task))
	  (org-add-planning-info 'deadline (org-read-date nil t (nth 1 (assoc '("duedate") task)) nil nil nil)))
      ;; ppriorityがあれば追加
      ;; ppriority if any additional
      (when (nth 1 (assoc "priority" task))
	(let ((priority (nth 1 (assoc "priority" task))))
	  (cond ((string= priority "3") (setq priority (replace-regexp-in-string "3" "A" priority)))
		 ((string= priority "2") (setq priority (replace-regexp-in-string "2" "A" priority)))
		 ((string= priority "1") (setq priority (replace-regexp-in-string "1" "B" priority)))
		 ((string= priority "0") (setq priority (replace-regexp-in-string "0" "C" priority)))
		 ((string= priority "-1") (setq priority (replace-regexp-in-string "-1" "C" priority))))
	  (org-entry-put (point) "PRIORITY" priority)))
      (unless (eq (nth 1 (assoc "completed" task)) nil)
	(org-todo "DONE"))
      (message "Add a new task 新規タスクとして追加"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Toodledoから全てのタスクを取得
;; Toodledo retrieve all tasks from
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun org-toodledo-get-all-tasks ()
  (let ((buf (url-retrieve-synchronously (concat "http://api.toodledo.com/api.php?method=getTasks;key="
						 org-toodledo-key
						 ";notcomp=0")))
	tasks)
    (setq tasks (with-current-buffer buf
		  (let ((coding-system-for-read 'utf-8-unix)
			(buf (current-buffer)))
		    (goto-char (point-min))
		    (sexpath-eval "\"task\"" (xml-parse-read 'tasks)))))
    (kill-buffer buf)
    tasks)
)

(defun org-toodledo-change-property (task properties)
  (let ((tag-remote (nth 1 (assoc "tag" task)))
	(tag-local (cdr (assoc "TAG" properties)))
	(startdate-remote (nth 1 (assoc "startdate" task)))
	(startdate-local (cdr (assoc "SCHEDULED" properties)))
	(duedate-remote (nth 1 (assoc "duedate" task)))
	(duedate-local (cdr (assoc "DEADLINE" properties)))
	(priority-remote (nth 1 (assoc "priority" (cdr task))))
	(priority-local (cdr (assoc "PRIORITY" properties)))
	(comp-remote (nth 1 (assoc "completed" task)))
	(comp-local (cdr (assoc "TODO" properties))))
    ;; tagを比較，変更
    ;; Tag compare changes
    (unless (string= tag-remote tag-local)
      (org-entry-delete (point) "TAG")
      (org-entry-put (point) "TAG" tag-remote))
    ;; startdateを比較，変更
    ;; Startdate compare changes
    (unless (string= startdate-remote startdate-local)
      (org-entry-delete (point) "SCHEDULED")
      (unless (equal startdate-remote nil)
	(org-add-planning-info 'scheduled (org-read-date nil t startdate-remote nil nil nil))))
    ;; duedateを比較，変更
    ;; Duedate compare changes
    (unless (string= duedate-remote duedate-local)
      (org-entry-delete (point) "DEADLINE")
      (unless (equal duedate-remote nil)
	(org-add-plannning-info 'deadline (org-read-date nil t duedate-remote nil nil nil))))
    ;; priorityを比較，変更
    ;; Priority compared changes
    (unless (string= priority-remote priority-local)
      (cond ((string= priority-remote "3") (setq priority-remote (replace-regexp-in-string "3" "A" priority-remote)))
	    ((string= priority-remote "2") (setq priority-remote (replace-regexp-in-string "2" "A" priority-remote)))
	    ((string= priority-remote "1") (setq priority-remote (replace-regexp-in-string "1" "B" priority-remote)))
	    ((string= priority-remote "0") (setq priority-remote (replace-regexp-in-string "0" "C" priority-remote)))
	    ((string= priority-remote "-1") (setq priority-remote (replace-regexp-in-string "-1" "C" priority-remote))))
      (org-entry-delete (point) "PRIORITY")
      (org-entry-put (point) "PRIORITY" priority-remote))
    ;; completedを比較，変更
    ;; Completed a comparison of changes
    (unless (eq comp-remote nil)
      (org-todo "DONE")))
  )




(provide 'org-toodledo)

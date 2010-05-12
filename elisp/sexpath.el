;;; sexpath.el --- Xpath expressions to extract s-like form factor

;; Copyright (C) 2010  

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; XPath expression to extract the S-like form factor with Emacs Lisp
;; S 式から XPath 風の書式で要素を抜き出す Emacs Lisp
;; from http://cx4a.sakura.ne.jp/blog/2009/01/s-xpath-elisp.html

;; tomo
;; Tomohiro Matsuyama
;; m2ym.pub@gmail.com
;; (2009年1月12日 19:13) 
;; ( January 12, 2009 19:13)

;; S 式から XPath 風の書式で要素を抜き出す Emacs Lisp を作ってみた（既出かもしれないけど）。
;; S XPath expression to extract the Emacs Lisp-like elements in the form I made (I might popup window.)

;; 町山さんの Podcast の RSS から色々情報を取ってみる。
;; Podcast's RSS try to get information from a wide variety.

;; (setq xml (xml-parse-file "tokuden.xml"))
;; タイトル取得
;; get title 

;; (sexpath-eval "rss/channel/title@2" xml)    ; => ("EnterJam　町山智浩のアメリカ映画特電")
;; アイテムの数を取得
;; get the number of items 

;; (length (sexpath-eval "%item" xml))    ; => 71
;; 三番目のアイテムのタイトルを取得
;; get a third title of the item 

;; (sexpath-eval "%item[2]/title@2" xml)    ; => ("第69回　アメリカではクリスマス映画の定番、日本では未公開の『クリスマス・ストーリー』と、知られざるクリスマス映画『真夜中の戦場』")
;; エンクロージャーの URL を全て取得
;; ("The 69th is a classic American film, Christmas in Japan Christmas Storyx unpublished and unknown Christmas movie [Midnight battlefield ")

;; (sexpath-eval "%item/enclosure@1/url@cdr" xml)    ; => ("http://...
;; 書式は以下のようになっている。
;; Get all the enclosure URL 

;; / 選択中の要素の子を選択する
;; % 選択中の要素の子孫を選択する
;; [<number>] 選択中の要素の number 番目を選択する
;; @<number> 選択中の要素の子の number 番目を選択する
;; @car 選択中の要素の car を選択する
;; @cdr 選択中の要素の cdr を選択する
;; <lisp-object> 選択中の要素の car が lisp-object に一致する場合にその要素を選択する
;; とりあえず実装は以下のようになっている。もう少し手入れしてちゃんと動くようになったらどこかに公開するかも。

;; The format is as follows:

;; / Select child element of the selected
;; % Select the descendants of the selected element
;; [ <number> ]Selected elements of numberthe second selecting
;; @ <number>Selected child element numberto select th
;; @ CarThe selected element carto select
;; @ CdrThe selected element cdrto select
;; <lisp-object>Selected element caris lisp-objectselected if the element matches

;; とりあえず実装は以下のようになっている。もう少し手入れしてちゃんと動くようになったらどこかに公開するかも。
;; Anyway, that is implemented as follows. Public or even somewhere as it works to become a little more care.

;;; Code:

(defvar sexpath-ignore-symbol-chars "[/%@\[]")

(defun sexpath-descendants-1 (nodes)
  (if (consp nodes)
      (while (consp nodes)
        (push (car nodes) descendants)
        (sexpath-descendants-1 (car nodes))
        (setq nodes (cdr nodes)))))

(defun sexpath-descendants (nodes)
  (let (descendants)
    (sexpath-descendants-1 nodes)
    (nreverse descendants)))

(defun sexpath-read ()
  (let* ((point (point))
         (object (read (current-buffer))))
    (when (and (symbolp object)
               (string-match sexpath-ignore-symbol-chars (symbol-name object)))
        (let ((end (point)))
          (goto-char point)
          (when (re-search-forward (format "[^\\]\\(%s\\)" sexpath-ignore-symbol-chars) end t)
            (setq object (car (read-from-string (buffer-substring point (match-beginning 1)))))
            (setq end (match-beginning 1)))
          (goto-char end)))
    object))

(defun sexpath-consume (regexp)
  (if (looking-at regexp)
      (goto-char (match-end 0))))

(defun sexpath-sequencep (node)
  (let ((sequencep (sequencep node)))
    (while (and node (consp node) (consp (consp (cdr node)))
                (setq node (car node)))
      (if (cdr node)
          (setq sequencep nil)))
    sequencep))

(defun sexpath-compile (expr)
  (let (selectors object)
    (with-temp-buffer
      (insert expr)
      (goto-char (point-min))
      (while (not (eobp))
        (cond
         ((sexpath-consume "%")         ; descendants selector
          (push `(sexpath-descendants nodes) selectors))
         ((sexpath-consume "/")         ; child selector
          (push `(apply 'append (mapcar (lambda (node) (if (sexpath-sequencep node) node)) nodes)) selectors))
         ((sexpath-consume "@car")      ; car selector
          (push `(mapcar 'car-safe nodes) selectors))
         ((sexpath-consume "@cdr")      ; cdr selector
          (push `(mapcar 'cdr-safe nodes) selectors))
         ((sexpath-consume "@\\([0-9]+\\)") ; elt selector
          (push `(mapcar '(lambda (node) (if (sequencep node) (ignore-errors (elt node ,(string-to-number (match-string 1)))))) nodes) selectors))
         ((sexpath-consume "\\[\\([0-9]+\\)\\]") ; node elt selector
          (push `(if (sequencep nodes) (ignore-errors (list (elt nodes ,(string-to-number (match-string 1)))))) selectors))
         ((setq object (sexpath-read))  ; car-value selector
          (push `(mapcar (lambda (node) (if (and (consp node) (equal ',object (car node))) node)) nodes) selectors))
         (t
          (error "Invalid syntax")))))
    `(let ((nodes sexp))
       ,@(let (forms)
           (dolist (selector (nreverse selectors))
             (push `(if nodes (setq nodes (delq nil ,selector))) forms))
           (nreverse forms))
       nodes)))

(defun sexpath-eval (expr sexp)
  (eval (sexpath-compile expr)))

(provide 'sexpath)

;; -*- coding: utf-8-unix -*-
;;; init.el --- Emacs initialization file -*- lical-binding: t -*-

;; emacs26  for devuan

;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
;;; base package control                                            ;;;
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
;; Next-generation, purely functional package manager for the Emacs hacker. 
;; https://github.com/raxod502/straight.el

;; straight.el自身のインストールと初期設定を行ってくれる
(let ((bootstrap-file (concat user-emacs-directory "straight/repos/straight.el/bootstrap.el"))
      (bootstrap-version 3))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;;To install a package permanently, place a call to straight-use-package in your init-file, like:
(straight-use-package 'el-patch)

;; use-packageをインストールする
(straight-use-package 'use-package)

;; オプションなしで自動的にuse-packageをstraight.elにフォールバックする
;; 本来は (use-package hoge :straight t) のように書く必要がある
(setq straight-use-package-by-default t)

;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(cperl-close-paren-offset -4)
 '(cperl-indent-parens-as-block t)
 '(cperl-indent-subs-specially nil)
 '(custom-enabled-themes (quote (tango-dark)))
 '(custom-safe-themes
   (quote
    ("3c83b3676d796422704082049fc38b6966bcad960f896669dfc21a7a37a748fa" "c74e83f8aa4c78a121b52146eadb792c9facc5b1f02c917e3dbb454fca931223" "a27c00821ccfd5a78b01e4f35dc056706dd9ede09a8b90c6955ae6a390eb1c1e" default)))
 '(desktop-save-mode t)
 '(font-use-system-font nil)
 '(org-agenda-files (quote ("~/org-mode/todo.org")))
 '(org-babel-load-languages
   (quote
    ((emacs-lisp . t)
     (scheme . t)
     (awk . t)
     (perl . t)
     (shell . t))))
 '(org-capture-templates
   (quote
    (("n" "etc notes" entry
      (file "~/org-mode/notes.org")
      "" :prepend t :jump-to-captured t :time-prompt t :tree-type week :kill-buffer t)
     ("t" "todo list" checkitem
      (file "~/org-mode/todo.org")
      "" :time-prompt t :tree-type week :kill-buffer t))))
 '(package-selected-packages
   (quote
    (ack smart-mode-line dired-open dired-subtree dired-filter systemd ag aggressive-indent pcre2el projectile golden-ratio magit-gh-pulls magit yasnippet yaml-mode web-mode use-package ripgrep rg recentf-ext rainbow-mode pony-mode pip-requirements phi-rectangle peg paredit paradox package-utils org-toodledo org-table-comment org-plus-contrib org-octopress org-bullets open-junk-file lispxmp grep-a-lot flx-ido exec-path-from-shell evil dired+ dash-functional browse-at-remote auto-async-byte-compile apache-mode anything adaptive-wrap ace-window)))
 '(safe-local-variable-values (quote ((lical-binding . t))))
 '(skk-annotation-other-sources
   (quote
    (ja\.wikipedia en\.wiktionary simple\.wikipedia en\.wikipedia)))
 '(skk-auto-insert-paren t)
 '(skk-auto-okuri-process nil)
 '(skk-auto-start-henkan nil)
 '(skk-aux-large-jisyo "/home/devuan/.emacs.d/skk-get-jisyo/SKK-JISYO.L")
 '(skk-cdb-large-jisyo nil)
 '(skk-check-okurigana-on-touroku (quote ask))
 '(skk-date-ad t)
 '(skk-delete-implies-kakutei t)
 '(skk-egg-like-newline t)
 '(skk-extra-jisyo-file-list
   (quote
    ("/home/devuan/.emacs.d/skk-get-jisyo/SKK-JISYO.jinmei" "/home/devuan/.emacs.d/skk-get-jisyo/SKK-JISYO.geo" "/home/devuan/.emacs.d/skk-get-jisyo/SKK-JISYO.pubdic+")))
 '(skk-henkan-okuri-strictly nil)
 '(skk-henkan-strict-okuri-precedence nil)
 '(skk-itaiji-jisyo "/home/devuan/.emacs.d/skk-get-jisyo/SKK-JISYO.itaiji")
 '(skk-j-mode-function-key-usage nil)
 '(skk-japanese-message-and-error t)
 '(skk-kakutei-early t)
 '(skk-preload t)
 '(skk-share-private-jisyo nil)
 '(skk-show-annotation (quote (not list)))
 '(skk-show-candidates-always-pop-to-buffer nil)
 '(skk-show-icon t)
 '(skk-show-inline nil)
 '(skk-show-japanese-menu t)
 '(skk-show-tooltip nil)
 '(skk-start-henkan-char 32)
 '(skk-use-color-cursor t)
 '(skk-use-face t)
 '(skk-use-jisx0201-input-method nil)
 '(skk-use-look t)
 '(skk-use-numeric-conversion t)
 '(skk-verbose t)
 '(smtpmail-smtp-server "smtp.lolipop.jp")
 '(smtpmail-smtp-service 587))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(aw-leading-char-face ((t (:inherit ace-jump-face-foreground :height 3.0)))))

;;;; Package Archives
(set-variable 'package-archives
              '(("gnu" . "http://elpa.gnu.org/packages/")
		;;                ("melpa" . "http://melpa.milkbox.net/packages/")
                ("melpa" . "http://melpa.org/packages/")
                ;; org-mode
                ("org"   . "http://orgmode.org/elpa/")
                ))

;;We can control the amount of output use-package generates by setting use-package-verbose to true.
(setq use-package-verbose t)

;; package-utilsをインストールする
(straight-use-package 'package-utils)

;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
;;; @ language - coding system                                      ;;;
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 重要　日本語Windowsの文字コード対策
;; https://www49.atwiki.jp/ntemacs/pages/16.html 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; ------------------------------------------------------------------------
;; @ character code (文字コード)
;; Setenv
(set-language-environment "Japanese")
(setenv "LANG" "ja_JP.UTF-8")

(when (equal system-type 'ns)
  (require 'ucs-normalize)
  (setq prefer-coding-system 'utf-8-unix)
  (setq locale-coding-system 'utf-8-unix)
  (setq default-process-coding-system '(utf-8-unix . cp932)) ;agで日本語検索させるためのおまじない

  (setq set-file-name-coding-system 'cp932)
  (setq set-keyboard-coding-system 'cp932)
  (setq set-terminal-coding-system 'cp932)
)
(require 'cl-lib)



;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
;;; @ language - coding system  on ubuntu18                         ;;;
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
(when (equal system-type 'gnu/linux)
  (require 'ucs-normalize)
  ;; (setq default-process-coding-system 'utf-8-unix)
  ;; プロセスが出力する文字oコードを判定して、process-coding-system の DECODING の設定値を決定する
  (setq default-process-coding-system '(undecided-dos . utf-8-unix))
  ;;  (setq default-process-coding-system '(utf-8-unix . cp932)) ;agで日本語検索 qさせるためのおまじない
  (setq prefer-coding-system 'utf-8-unix)
  (setq file-name-coding-system 'utf-8-unix)
  (setq locale-coding-system 'utf-8-unix)
  (setq set-default-coding-systems 'utf-8-unix)
  )

;; IME の設定をした後には実行しないこと
;;(set-language-environment 'Japanese)
;; ファイルのデフォルトの文字コード指定
;; 開いているバッファーのファイルの文字コードを変更する場合には set-buffer-file-coding-system (C-x RET f)を使います。
;; しかし、これを設定で書いても意味がありません。 バッファーごとの文字コード(buffer-file-coding-system) はバッファーローカルな値なので、 ロードしているバッファーの文字コードが変わるだけです。
;; 新規作成時のファイルのデフォルトを変える場合には set-default 関数を使って buffer-file-coding-system のデフォルト値を変更します。
(when (equal system-type 'gnu/linux)
  ;; ファイルのデフォルトを non-BOM UTF-8
  (set-default 'buffer-file-coding-system 'utf-8-unix)
  ;; BOM なし UTF-8 でなければならない言語のファイル文字固定
  (modify-coding-system-alist 'file "\\.org\\'" 'utf-8-unix)               ;; org-mode
  (modify-coding-system-alist 'file "\\.p?\\'" 'utf-8-unix)              ;; perl
  )

(when (equal system-type 'ns)

;; プロセスが出力する文字コードを判定して、process-coding-system の DECODING の設定値を決定する
  (setq default-process-coding-system '(undecided-dos . utf-8-unix)))

;; ldd の結果のキャッシュ
(defvar ldd-cache nil)


;;; ob-shell.el --- Babel Functions for Shell Evaluation 
;; Copyright (C) 2009-2020 Free Software Foundation, Inc.

;; Author: Eric Schulte
;; Keywords: literate programming, reproducible research
;; Homepage: https://orgmode.org

;; This file is part of GNU Emacs.

;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Org-Babel support for evaluating shell source code.

;;; Code:
(require 'ob)
(require 'org-macs)
(require 'shell)
(require 'cl-lib)

(declare-function org-babel-comint-in-buffer "ob-comint" (buffer &rest body)
		  t)
(declare-function org-babel-comint-wait-for-output "ob-comint" (buffer))
(declare-function org-babel-comint-buffer-livep "ob-comint" (buffer))
(declare-function org-babel-comint-with-output "ob-comint" (meta &rest body)
		  t)
(declare-function orgtbl-to-generic "org-table" (table params))

(defvar org-babel-default-header-args:shell '())
(defvar org-babel-shell-names)

(defun org-babel-shell-initialize ()
  "Define execution functions associated to shell names.
This function has to be called whenever `org-babel-shell-names'
is modified outside the Customize interface."
  (interactive)
  (dolist (name org-babel-shell-names)
    (eval `(defun ,(intern (concat "org-babel-execute:" name))
	       (body params)
	     ,(format "Execute a block of %s commands with Babel." name)
	     (let ((shell-file-name ,name))
	       (org-babel-execute:shell body params))))
    (eval `(defalias ',(intern (concat "org-babel-variable-assignments:" name))
	     'org-babel-variable-assignments:shell
	     ,(format "Return list of %s statements assigning to the block's \
variables."
		      name)))
    (eval `(defvar ,(intern (concat "org-babel-default-header-args:" name)) '()))))

(defcustom org-babel-shell-names
  '("sh" "bash" "zsh" "fish" "csh" "ash" "dash" "ksh" "mksh" "posh")
  "List of names of shell supported by babel shell code blocks.
Call `org-babel-shell-initialize' when modifying this variable
outside the Customize interface."
  :group 'org-babel
  :type '(repeat (string :tag "Shell name: "))
  :set (lambda (symbol value)
	 (set-default symbol value)
	 (org-babel-shell-initialize)))

(defun org-babel-execute:shell (body params)
  "Execute a block of Shell commands with Babel.
This function is called by `org-babel-execute-src-block'."
  (let* ((session (org-babel-sh-initiate-session
		   (cdr (assq :session params))))
	 (stdin (let ((stdin (cdr (assq :stdin params))))
                  (when stdin (org-babel-sh-var-to-string
                               (org-babel-ref-resolve stdin)))))
	 (cmdline (cdr (assq :cmdline params)))
         (full-body (org-babel-expand-body:generic
		     body params (org-babel-variable-assignments:shell params))))
    (org-babel-reassemble-table
     (org-babel-sh-evaluate session full-body params stdin cmdline)
     (org-babel-pick-name
      (cdr (assq :colname-names params)) (cdr (assq :colnames params)))
     (org-babel-pick-name
      (cdr (assq :rowname-names params)) (cdr (assq :rownames params))))))

(defun org-babel-prep-session:shell (session params)
  "Prepare SESSION according to the header arguments specified in PARAMS."
  (let* ((session (org-babel-sh-initiate-session session))
	 (var-lines (org-babel-variable-assignments:shell params)))
    (org-babel-comint-in-buffer session
      (mapc (lambda (var)
              (insert var) (comint-send-input nil t)
              (org-babel-comint-wait-for-output session)) var-lines))
    session))

(defun org-babel-load-session:shell (session body params)
  "Load BODY into SESSION."
  (save-window-excursion
    (let ((buffer (org-babel-prep-session:shell session params)))
      (with-current-buffer buffer
        (goto-char (process-mark (get-buffer-process (current-buffer))))
        (insert (org-babel-chomp body)))
      buffer)))


;;; Helper functions
(defun org-babel--variable-assignments:sh-generic
    (varname values &optional sep hline)
  "Return a list of statements declaring the values as a generic variable."
  (format "%s=%s" varname (org-babel-sh-var-to-sh values sep hline)))

(defun org-babel--variable-assignments:bash_array
    (varname values &optional sep hline)
  "Return a list of statements declaring the values as a bash array."
  (format "unset %s\ndeclare -a %s=( %s )"
	  varname varname
	  (mapconcat
	   (lambda (value) (org-babel-sh-var-to-sh value sep hline))
	   values
	   " ")))

(defun org-babel--variable-assignments:bash_assoc
    (varname values &optional sep hline)
  "Return a list of statements declaring the values as bash associative array."
  (format "unset %s\ndeclare -A %s\n%s"
    varname varname
    (mapconcat
     (lambda (items)
       (format "%s[%s]=%s"
	       varname
	       (org-babel-sh-var-to-sh (car items) sep hline)
	       (org-babel-sh-var-to-sh (cdr items) sep hline)))
     values
     "\n")))

(defun org-babel--variable-assignments:bash (varname values &optional sep hline)
  "Represent the parameters as useful Bash shell variables."
  (pcase values
    (`((,_ ,_ . ,_) . ,_)		;two-dimensional array
     (org-babel--variable-assignments:bash_assoc varname values sep hline))
    (`(,_ . ,_)				;simple list
     (org-babel--variable-assignments:bash_array varname values sep hline))
    (_					;scalar value
     (org-babel--variable-assignments:sh-generic varname values sep hline))))

(defun org-babel-variable-assignments:shell (params)
  "Return list of shell statements assigning the block's variables."
  (let ((sep (cdr (assq :separator params)))
	(hline (when (string= "yes" (cdr (assq :hlines params)))
		 (or (cdr (assq :hline-string params))
		     "hline"))))
    (mapcar
     (lambda (pair)
       (if (string-suffix-p "bash" shell-file-name)
	   (org-babel--variable-assignments:bash
            (car pair) (cdr pair) sep hline)
         (org-babel--variable-assignments:sh-generic
	  (car pair) (cdr pair) sep hline)))
     (org-babel--get-vars params))))

(defun org-babel-sh-var-to-sh (var &optional sep hline)
  "Convert an elisp value to a shell variable.
Convert an elisp var into a string of shell commands specifying a
var of the same value."
  (concat "'" (replace-regexp-in-string
	       "'" "'\"'\"'"
	       (org-babel-sh-var-to-string var sep hline))
	  "'"))

(defun org-babel-sh-var-to-string (var &optional sep hline)
  "Convert an elisp value to a string."
  (let ((echo-var (lambda (v) (if (stringp v) v (format "%S" v)))))
    (cond
     ((and (listp var) (or (listp (car var)) (eq (car var) 'hline)))
      (orgtbl-to-generic var  (list :sep (or sep "\t") :fmt echo-var
				    :hline hline)))
     ((listp var)
      (mapconcat echo-var var "\n"))
     (t (funcall echo-var var)))))

(defun org-babel-sh-initiate-session (&optional session _params)
  "Initiate a session named SESSION according to PARAMS."
  (when (and session (not (string= session "none")))
    (save-window-excursion
      (or (org-babel-comint-buffer-livep session)
          (progn
	    (shell session)
	    ;; Needed for Emacs 23 since the marker is initially
	    ;; undefined and the filter functions try to use it without
	    ;; checking.
	    (set-marker comint-last-output-start (point))
	    (get-buffer (current-buffer)))))))

(defvar org-babel-sh-eoe-indicator "echo 'org_babel_sh_eoe'"
  "String to indicate that evaluation has completed.")
(defvar org-babel-sh-eoe-output "org_babel_sh_eoe"
  "String to indicate that evaluation has completed.")

(defun org-babel-sh-evaluate (session body &optional params stdin cmdline)
  "Pass BODY to the Shell process in BUFFER.
If RESULT-TYPE equals `output' then return a list of the outputs
of the statements in BODY, if RESULT-TYPE equals `value' then
return the value of the last statement in BODY."
  (let* ((shebang (cdr (assq :shebang params)))
	 (results
	  (cond
	   ((or stdin cmdline)	       ; external shell script w/STDIN
	    (let ((script-file (org-babel-temp-file "sh-script-"))
		  (stdin-file (org-babel-temp-file "sh-stdin-"))
		  (padline (not (string= "no" (cdr (assq :padline params))))))
	      (with-temp-file script-file
		(when shebang (insert shebang "\n"))
		(when padline (insert "\n"))
		(insert body))
	      (set-file-modes script-file #o755)
	      (with-temp-file stdin-file (insert (or stdin "")))
	      (with-temp-buffer
		(call-process-shell-command
		 (concat (if shebang script-file
			   (format "%s %s" shell-file-name script-file))
			 (and cmdline (concat " " cmdline)))
		 stdin-file
		 (current-buffer))
		(buffer-string))))
	   (session			; session evaluation
	    (mapconcat
	     #'org-babel-sh-strip-weird-long-prompt
	     (mapcar
	      #'org-trim
	      (butlast
	       (org-babel-comint-with-output
		   (session org-babel-sh-eoe-output t body)
		 (dolist (line (append (split-string (org-trim body) "\n")
				       (list org-babel-sh-eoe-indicator)))
		   (insert line)
		   (comint-send-input nil t)
		   (while (save-excursion
			    (goto-char comint-last-input-end)
			    (not (re-search-forward
				  comint-prompt-regexp nil t)))
		     (accept-process-output
		      (get-buffer-process (current-buffer))))))
	       2))
	     "\n"))
	   ;; External shell script, with or without a predefined
	   ;; shebang.
	   ((org-string-nw-p shebang)
	    (let ((script-file (org-babel-temp-file "sh-script-"))
		  (padline (not (equal "no" (cdr (assq :padline params))))))
	      (with-temp-file script-file
		(insert shebang "\n")
		(when padline (insert "\n"))
		(insert body))
	      (set-file-modes script-file #o755)
	      (org-babel-eval script-file "")))
	   (t
	    (org-babel-eval shell-file-name (org-trim body))))))
    (when results
      (let ((result-params (cdr (assq :result-params params))))
        (org-babel-result-cond result-params
          results
          (let ((tmp-file (org-babel-temp-file "sh-")))
            (with-temp-file tmp-file (insert results))
            (org-babel-import-elisp-from-file tmp-file)))))))

(defun org-babel-sh-strip-weird-long-prompt (string)
  "Remove prompt cruft from a string of shell output."
  (while (string-match "^% +[\r\n$]+ *" string)
    (setq string (substring string (match-end 0))))
  string)

(provide 'ob-shell)



;;; ob-shell.el ends here

;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
;;; @ Ido itself                                      ;;;
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
;;https://github.com/DarwinAwardWinner/ido-completing-read-plus
					;; ido enhance

(ido-mode 1)
(ido-everywhere 1)

(setq ido-enable-flex-matching t) ;; 中間/あいまい一致

;; Fix ibuffer to use ido-find-file
(require 'ibuffer)
(define-key ibuffer-mode-map (kbd "C-x C-f") #'ido-find-file)
;; Always use ibuffer
(global-set-key [remap list-buffers] #'ibuffer)


(use-package flx-ido
					;idoのあいまいマッチを改善する
  :ensure t
  :init (setq ido-auto-merge-work-directories-length -1
              ido-create-new-buffer 'never
              ido-enable-flex-matching t
              ido-enable-last-directory-history t
              ido-use-faces nil)
  :config (progn
            (ido-mode 1)
         ;;   (ido-everywhere 0)
            (flx-ido-mode 1)))

;;
(use-package smex
  ;; ido のインターフェイスを利用してM-xを再定義
  :ensure t
  :init (smex-initialize)
       ;; Can be omitted. This might cause a (minimal) delay
                  ;; when Smex is auto-initialized on its first run.
  :config (progn
      (global-set-key (kbd "M-x") 'smex)
      (global-set-key (kbd "M-X") 'smex-major-mode-commands)
      ;; This is your old M-x.
      (global-set-key (kbd "C-c C-c M-x") 'execute-extended-command)
      ))
;;
(use-package ido-completing-read+
  :ensure t)

;;ido-vertical-mode
    ;; 横並びの選択候補を、縦並びにするパッケージ
    ;; デフォルトは C-s/C-r が候補選択だが、見た目とずれてて使いづらいので C-n/C-p に変更するとよい

(use-package ido-vertical-mode
  :config
  (ido-vertical-mode 1)
  (setq ido-vertical-define-keys 'C-n-and-C-p-only)    ;; C-n/C-pで候補選択する
  (setq ido-vertical-show-count t)
  )

;; Directory Caching on Windows
;; On Windows operating systems it can be unreliable to cache directory listings: the directory may not appear to be modified even though files have been added or removed. Ido caches directory listings by default, which may cause confusion on Windows. You can disable caching:

(when (equal system-type 'windows-nt)
  (setq ido-max-dir-file-cache 0)) ; caching unreliable

;; Specifying sort-order
;; If you'd like to tweak the default file sorting, like making Org-files appear first, tell ido which files to give a higher sort priority:
(setq ido-file-extensions-order '(".org" ".txt" ".pl" ".emacs" ".xml" ".el" ".scm"
				  ".ini" ".cfg" ".conf" ".pm" ".t" ))

;; Access File Bookmarks from `C-x C-f'
 (defun bookmark-to-abbrevs ()
   "Create abbrevs based on `bookmark-alist'."
   (dolist (bookmark bookmark-alist)
   (let* ((name (car bookmark))
          (file (bookmark-get-filename name)))
     (define-abbrev global-abbrev-table name file))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;; http://emacs.rubikitch.com/global-hl-line-mode-timer/
;; 遅い場合は以下

(require 'hl-line)
;;; hl-lineを無効にするメジャーモードを指定する
(defvar global-hl-line-timer-exclude-modes '(todotxt-mode))
(defun global-hl-line-timer-function ()
  (unless (memq major-mode global-hl-line-timer-exclude-modes)
    (global-hl-line-unhighlight-all)
    (let ((global-hl-line-mode t))
      (global-hl-line-highlight))))
(setq global-hl-line-timer
      (run-with-idle-timer 0.03 t 'global-hl-line-timer-function))
;; (cancel-timer global-hl-line-timer)

;;;;;
;;スクロールを鮮やかにする
;;https://github.com/k-talo/smooth-scroll.el
					;(require 'smooth-scroll)
					;(smooth-scroll-mode t)

;; スクロールした際のカーソルの移動行数
(setq scroll-conservatively 1)

;; スクロール開始のマージンの行数
(setq scroll-margin 10)

;; 1 画面スクロール時に重複させる行数
(setq next-screen-context-lines 10)

;; 1 画面スクロール時にカーソルの画面上の位置をなるべく変えない
(setq scroll-preserve-screen-position t)

;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
;;; @ NTwindows                                                     ;;;
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;

;; ;; MSYS2 のコマンドを使えるようにする.
;; ;[url=https://www.emacswiki.org/emacs/GrepMode#toc5][Home] Grep Mode[/url]
;; ; etag.exe ctag.exe path
(when (equal system-type 'windows-nt)
  (setenv "PATH"
          (concat
	   ;; 下記の行に MSYS2 のコマンドの実行可能ファイルがある場所を設定してください. スラッシュが2つ連続することに注意！
					;     "C:\\strawberry\\perl\\bin;"
           "C:\\ProgramData\\tools\\msys2\\usr\\bin;" ; msys git 
	 ;  "C:\\ProgramData\\chocolatey\\lib\\ag\\tools;"
	   "C:\\ProgramData\\tools\\msys2\\mingw64\\bin;" ; msys ag
	   (getenv "PATH"))))

(when (equal system-type 'windows-nt)
;;; @ language - fontset                                            ;;;
  ;; ;; デフォルト フォント
  (set-face-attribute 'default nil :family "Migu 1M" :height 110)
  ;; (set-face-font 'default "Migu 1M-11:antialias=standard")

  ;; ;; プロポーショナル フォント
  (set-face-attribute 'variable-pitch nil :family "Migu 1M" :height 110)
  ;; (set-face-font 'variable-pitch "Migu 1M-11:antialias=standard")

  ;; ;; 等幅フォント
  (set-face-attribute 'fixed-pitch nil :family "Migu 1M" :height 110)
  ;; (set-face-font 'fixed-pitch "Migu 1M-11:antialias=standard")

  ;; ;; ツールチップ表示フォント
  (set-face-attribute 'tooltip nil :family "Migu 1M" :height 90)
  ;; (set-face-font 'tooltip "Migu 1M-9:antialias=standard")

  ;; ;; ;; フォントサイズ リセット
  (global-set-key (kbd "M-0") '(lambda() (interactive) (text-scale-set 0))))
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
;;; @ screen - buffer                                               ;;;
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;

;; バッファ画面外文字の切り詰め表示（有効：t、無効：nil）
(setq truncate-lines t)

;; ウィンドウ縦分割時のバッファ画面外文字の切り詰め表示（有効：t、無効：nil）
(setq truncate-partial-width-windows t)

;; 同一バッファ名にディレクトリ付与
(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)
(setq uniquify-buffer-name-style 'post-forward-angle-brackets)
(setq uniquify-ignore-buffers-re "*[^*]+*")


 '(package-selected-packages
   (quote
    (exec-path-from-shell dired-quick-sort yatex ace-window web-mode yaml-mode flycheck flx-ido diff-hl paredit lispxmp rg ht ## recentf-ext )))
 '(send-mail-function (quote smtpmail-send-it))

(add-hook 'cperl-mode-hook
       (lambda ()
         (local-set-key (kbd "C-h f") 'cperl-perldoc)))


;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
;;; @ アクティベーション                                            ;;;
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;

;; 警告音の代わりに画面フラッシュ
(setq visible-bell t)

;;; 最近使ったファイルをメニューに表示
(recentf-mode 1)
(setq recentf-max-menu-items 20)

;; 最近のファイル500個を保存する
(setq recentf-max-saved-items 500)

(use-package recentf
  :config
  ;; 最近使ったファイルに加えないファイルを正規表現で指定する
  (setq recentf-exclude '("/TAGS$" "/var/tmp/"))
  )
(use-package recentf-ext :ensure t)    ;; recentf-ext 自体はこの１行でOK


;; コマンド（org-store-link, org-capture, org-agenda, org-iswitchb）、グローバルキーを割り当

(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-cc" 'org-capture)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cb" 'org-iswitchb)




;;eshellのときだけ行番号を表示しない
(global-linum-mode 1)
(add-hook 'eshell-mode-hook (lambda () (linum-mode -1)))

;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
;;; dired edit                                                    ;;;
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
;; dired-hacks
;; https://qiita.com/ballforest/items/0ddbdfeaa9749647b488
(use-package dash
					; this lib need to dired-hacks 
  :ensure t
  )

;; / n 	名前でフィルタリング (dired-filter-by-name)
;; / r 	正規表現でフィルタリング (dired-filter-by-regexp)
;; / . 	拡張子でフィルタリング (dired-filter-by-extension)
;; / h 	ドットで始まるファイルを隠す (dired-filter-by-dot-files)
;; / f 	ファイルだけ表示 (dired-filter-by-file)
;; / d 	ディレクトリだけ表示 (dired-filter-by-directory)
(use-package dired-filter
  :ensure t
  )
					;diredデフォルトではiを押すことでサブディレクトリを展開するが，
					;別ディレクトリとしてdiredバッファの下側に中身が表示される．
(use-package dired-subtree
  :ensure t
  )

(use-package dired-open
  :ensure t
  )

;; diredを2つのウィンドウで開いている時に、デフォルトの移動orコピー先をもう一方のdiredで開いているディレクトリにする
(setq dired-dwim-target t)
;; ディレクトリを再帰的にコピーする
(setq dired-recursive-copies 'always)
;; diredバッファでC-sした時にファイル名だけにマッチするように
(setq dired-isearch-filenames t)
;; .zipで終わるファイルをZキーで展開できるように
(add-to-list 'dired-compress-file-suffixes '("\\.zip\\'" ".zip" "unzip"))

					; Use dired-jump, which is bound to C-x C-j by default to show the current file in a dired buffer. 
;;(require 'dired-x)
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
;;; @ rubikichi                                                     ;;;
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;

;; ;; 試行錯誤用ファイルを開くための設定
;; (require 'open-junk-file)
;; ;; C-x C-zで試行錯誤ファイルを開く
;; (global-set-key (kbd "C-x C-z") 'open-junk-file)

;; ;; 式の評価結果を注釈するための設定
;; (require 'lispxmp)
;; ;; emacs-lisp-mode でC-c C-dを押すと注釈される
;; (define-key emacs-lisp-mode-map (kbd "C-c C-d") 'lispxmp)

;; 括弧の対応を保持して編集する設定
(use-package paredit
  :ensure t
  :config
  (require 'paredit)
  (add-hook 'emacs-lisp-mode-hook 'enable-paredit-mode)
  (add-hook 'lisp-interaction-mode-hook 'enable-paredit-mode)
  (add-hook 'lisp-mode-hook 'enable-paredit-mode)
  (add-hook 'scheme-mode-hook 'enable-paredit-mode)
  (add-hook 'ielm-mode-hook 'enable-paredit-mode)
  (add-hook 'cperl-mode-hook 'enable-paredit-mode)  )


(setq eldoc-idle-delay 0.2) ; すぐに表示したい
(setq eldoc-minor-mode-string "") ; モードラインにElDocと表示しない

;; 釣り合いのとれる括弧をハイライトにする
(show-paren-mode 1)

;; 改行と同時にインデントも行う
(global-set-key "\C-m" 'newline-and-indent)

;; find-functionをキー割り当てする
(find-function-setup-keys)

;; org-mode
;;(require 'org)

;; 日本語info設定
;;; ~/info/以下をinfoファイル検索ディレクトリに加える
;;(add-to-list 'Info-directory-list "~/info/")

;; Emacs M-x toggle-truncate-lines: 長い行の折り返し表示を切り換える
;;(global-set-key (kbd "C-c t") 'toggle-truncate-lines)

;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
;;; @ ddskk rubikichi                                               ;;;
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
				;	==
;; Windows 環境だと [noconvert]
(setq skk-sticky-key [muhenkan])
(when (equal system-type 'windows-nt)
  (setq skk-sticky-key [noconvert])
  )

(require 'skk-hint)
;;　muhenkanなどのキー名はどうやって取得するのかというと、 <f1> c を使います。その後に無変換キーを押せば「<muhenkan> is undefined」と出てきます。

(when (require 'skk nil t)
  (global-set-key (kbd "C-x j") 'skk-auto-fill-mode) ;;良い感じに改行を自動入力してくれる機能
  (setq default-input-method "japanese-skk")         ;;emacs上での日本語入力にskkをつかう
  (require 'skk-study))                              ;;変換学習機能の追加

;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
;;; @ LogFile                                                       ;;;
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
;;;;; ログ・ファイル出力
(defvar logger-process nil)
(defun logger (&rest msg)
  (unless logger-process
    (setq logger-process (start-process-shell-command "logger" nil (concat "cat >> ~/log.txt"))))
  (process-send-string logger-process (concat (apply 'format msg) "\n")))


;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
;;; @ scheme-mode
;;; http://www.math.s.chiba-u.ac.jp/~matsu/emacs/emacs21/scheme.html;;;
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;

;; (setq scheme-program-name "/usr/local/bin/gosh")
;; (autoload 'run-scheme "cmuscheme" "Run an inferior Scheme process." t)
;; (setq cmuscheme-load-hook
;;       '((lambda () (define-key inferior-scheme-mode-map "\C-c\C-t"
;;      'favorite-cmd))))


;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
;;; @ cperl-mode
;;; https://www.emacswiki.org/emacs/CPerlMode                       ;;;
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;

;;; cperl-mode is preferred to perl-mode
;;; "Brevity is the soul of wit" <foo at acm.org>
(defalias 'perl-mode 'cperl-mode)

;;; (add-to-list 'auto-mode-alist '("\\.\\([pP][Llm]\\|al\\)\\'" . cperl-mode))
(add-to-list 'auto-mode-alist '("\\.\\([pP][Llm]\\|t\\)\\'" . cperl-mode))
(add-to-list 'interpreter-mode-alist '("perl" . cperl-mode))
(add-to-list 'interpreter-mode-alist '("perl5" . cperl-mode))
(add-to-list 'interpreter-mode-alist '("miniperl" . cperl-mode))

;; (cperl-set-style "PerlStyle")

;; Emacs M-x toggle-truncate-lines: 長い行の折り返し表示を切り換える
(global-set-key (kbd "C-c t") 'toggle-truncate-lines)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;; custom
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


 ;;load cperl, then work around indent issue
 (load-library "cperl-mode")
 (defun cperl-backward-to-start-of-continued-exp (lim)
   (goto-char (1+ lim))
   (forward-sexp)
   (beginning-of-line)
   (skip-chars-forward " \t")
 )
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(add-hook 'cperl-mode-hook
       (lambda ()
         (local-set-key (kbd "C-h f") 'cperl-perldoc)))

;;exec-path-from-shell.el
  ;;shell のパスをそのまま通す　重要 ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package exec-path-from-shell
   ; :no-require t
   ; :defer t
    :ensure t
    )
(exec-path-from-shell-initialize)
   
  ;;; perl v path ;;;;  equal system-type 'ns
    (
     (when (equal system-type '(mac ns))
  (exec-path-from-shell-initialize)
  exec-path	(split-string (getenv "PATH") ":")
 ; /home/blue/.plenv/versions/5.28.0/lib/perl5/5.28.0/

   	; /home/kato/	.	plenv/versions/5.27.2/bin
  (let ((path exec-path))
    (format "  exec-path: %s\n" exec-path))))

;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
;;  pcre2el rxt-mode http://emacs.rubikitch.com/pcre2el/
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
;; 正規表現変換・解説
;; M-x rxt-mode でRegular eXpression Translationマイナーモード
;; C-c / /
;;     rxt-explain 正規表現を解説
;;; regexp perl
(use-package pcre2el
  :ensure t
  :config
  (add-hook 'prog-mode-hook 'rxt-mode)
  (setq reb-re-syntax 'pcre)
  )

;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
;; 日本語grep対策　
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;

(when (equal system-type 'windows-nt)
  (setq
   find-program "C:\\ProgramData\\tools\\msys64\\usr\\bin\\find.exe"
   grep-program "C:\\ProgramData\\tools\\msys64\\usr\\bin\\grep.exe"
   ;;      grep-program "C:\\tools\\msys64\\usr\\bin\\rg.exe"
   )
  )
;; Emacs Interface to Ack-like Tools
;; This package brings the full power of ack to emacs by allowing you to run it seamlessly with its large set of options. Ack-like tools such as the silver searcher and git/hg/bzr grep are well supported too.

(use-package ack
  :ensure t
 ;; :config
 )
  (defvar ack-history nil
  "History for the `ack' command.")
  (defun ack (command-args)
  (interactive
   (let ((ack-command "ack --nofilter --nogroup --with-filename "))
     (list (read-shell-command "Run ack (like this): "
                               ack-command
                               'ack-history))))
  (let ((compilation-disable-input t))
    (compilation-start (concat command-args " < " null-device)
                       'grep-mode))) 

 (add-hook 'ack-minibuffer-setup-hook 'ack-skel-vc-grep t)
 (add-hook 'ack-minibuffer-setup-hook 'ack-yank-symbol-at-point t)
;; (grep-apply-setting 'grep-command "ack --with-filename --nofilter --nogroup ")


;; Ag.el
;; https://agel.readthedocs.io/en/latest/installation.html#emacs
;; Afterwards, you can install ag.el from MELPA (the recommended approach).
;; :: Functions are autoloaded, so (require 'ag) is unnecessary.
;; silver_searcher https://github.com/ggreer/the_silver_searcher\
;; ag
(use-package ag
  :ensure t
  :init
  (setq ag-highlight-search nil)  ; 検索キーワードをハイライト
  (setq ag-reuse-buffers nil)     ; 検索用バッファを使い回す (検索ごとに新バッファを作らない)
  )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 日本語環境 windows grep対策
;; https://qiita.com/ignorant/items/76e4c162cedc47336e75#%E5%85%B1%E9%80%9A%E3%81%AE%E8%A8%AD%E5%AE%9A
;; https://extra-vision.blogspot.jp/2016/01/ntemacs-ag-silver-searcher.html
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 重要
;;Emacs でファイルの文字コードを変換するときの覚書Add Stargito2morygonzalez
;; 基本
;; 以下の2つを覚えておけばまず大丈夫。
;;     文字化けしてるとき (UTF-8 のファイルなのに SJIS で開いちゃったとき)
;;         「C-x RET r utf-8」
;;     文字化けしてないとき (SJIS のファイルを UTF-8 で保存したいとき)
;;         「C-x RET f utf-8」

;; "-*-"（ハイフンとアスタリスクとハイフン）という文字列で囲んで、文字コードを指定することができます。 （emacsのマニュアルであればspecify codingの章に記載があります。）
;; 以下のような文字列をファイルの先頭に記載しておくことで、 文字化けを防ぐことが出来ます。
;; -*- coding: utf-8-unix -*-
;; http://emacs.clickyourstyle.com/articles/299

;; todo:: ripgrepで日本語検索可能だが、ファイル文字コードがshit-jisだと読まれない
;;

;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
;;   Basic config                                                   ;;;
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
;; Global minor modes
					;(setq column-number-mode t)
(delete-selection-mode 1) ;テキスト入力するとモードが取り消されるらしい
(global-linum-mode 1) ; 行番号の表示
					;(global-subword-mode 1)
					;(setq comment-auto-fill-only-comments t)
					;(add-hook 'text-mode-hook #'turn-on-flyspell)
					;(add-hook 'prog-mode-hook #'flyspell-prog-mode)

;; Auto revert mode バッファの自動再読み込み　必要
(require 'autorevert)
(setq global-auto-revert-non-file-buffers t)
(setq auto-revert-verbose nil)
(global-auto-revert-mode 1)

;; Save place mode 同じファイルにアクセスしたときの最後の場所にポイントが移動します。
(require 'saveplace)
(setq-default save-place t)
(savehist-mode 1)


;; Always kill the current buffer without asking
(defun kill-buffer-now (&optional buffer-or-name)
  "Kill the buffer specified by BUFFER-OR-NAME without asking."
  (interactive)
  (let ((kill-buffer-query-functions nil))
    (kill-buffer buffer-or-name)))
(global-set-key (kbd "C-x k") #'kill-buffer-now)
(global-set-key (kbd "C-x C-k") #'kill-buffer-now)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;バッファを3分割しても4分割しても編集したいバッファだけ黄金比にしてくれる

(use-package golden-ratio
  :ensure t
  :config (golden-ratio-mode 1))

(use-package s
  ;;:ensure t
  :no-require t )

(use-package yaml-mode 
  :ensure t 
  :no-require t)


;; Fix ibuffer to use ido-find-file
(require 'ibuffer)
(define-key ibuffer-mode-map (kbd "C-x C-f") #'ido-find-file)
;; Always use ibuffer
(global-set-key [remap list-buffers] #'ibuffer)


(use-package web-mode
  :ensure t
  :config
  (add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.vue?\\'" . web-mode))
  (setq web-mode-engines-alist
	'(("django"    . "\\.html\\'")))
  (setq web-mode-ac-sources-alist
	'(("css" . (ac-source-css-property))
	  ("vue" . (ac-source-words-in-buffer ac-source-abbrev))
	  ("html" . (ac-source-words-in-buffer ac-source-abbrev))))
  (setq web-mode-enable-auto-closing t))
(setq web-mode-enable-auto-quoting t) ; this fixes the quote problem I mentioned

;; unset key 
(global-unset-key (kbd "C-z"))
(global-unset-key (kbd "C-x C-c"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;; Writing Mail  via emacswiki ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Mutt support.
(setq auto-mode-alist (append '(("/tmp/mutt.*" . mail-mode)) auto-mode-alist))

; Gnus:
(setq mail-user-agent 'message-user-agent)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
;;; @vc-dir  https://joppot.info/2018/01/18/4112
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
(require 'vc-dir)
;; In vc-git and vc-dir for git buffers, make (C-x v) a run git add, u run git
;; reset, and r run git reset and checkout from head.
(defun my-vc-git-command (verb fn)
  (let* ((fileset-arg (or vc-fileset (vc-deduce-fileset nil t)))
         (backend (car fileset-arg))
         (files (nth 1 fileset-arg)))
    (if (eq backend 'Git)
        (progn (funcall fn files)
               (message (concat verb " " (number-to-string (length files))
                                " file(s).")))
      (message "Not in a vc git buffer."))))

(defun my-vc-git-add (&optional revision vc-fileset comment)
  (interactive "P")
  (my-vc-git-command "Staged" 'vc-git-register))

(defun my-vc-git-reset (&optional revision vc-fileset comment)
  (interactive "P")
  (my-vc-git-command "Unstaged"
		     (lambda (files) (vc-git-command nil 0 files "reset" "-q" "--"))))

(eval-after-load "vc-dir"
  '(progn
     (define-key vc-prefix-map [(r)] 'vc-revert-buffer)
     (define-key vc-dir-mode-map [(r)] 'vc-revert-buffer)
     (define-key vc-prefix-map [(a)] 'my-vc-git-add)
     (define-key vc-dir-mode-map [(a)] 'my-vc-git-add)
     (define-key vc-prefix-map [(u)] 'my-vc-git-reset)
     (define-key vc-dir-mode-map [(u)] 'my-vc-git-reset)
     
     ;; hide up to date files after refreshing in vc-dir
     (define-key vc-dir-mode-map [(g)]
       (lambda () (interactive) (vc-dir-refresh) (vc-dir-hide-up-to-date)))))


;; - git remote add origin https://github.com/kawabata/hoge.git ..
;;  "origin" という名前で "http://.../" をアップストリームリポジトリに
;;   追加
;; - git push -u origin master

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; modeline  https://www.emacswiki.org/emacs/HeaderLine
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defconst my-header "some long header line string ...")
(setq header-line-format '(:eval (substring my-header
                                            (min (length my-header)
                                                 (window-hscroll)))))

;; (setq mode-line-format
;;          (list
;;           ;; value of `mode-name'
;;           "%m: "
;;           ;; value of current buffer name
;;           "buffer %b, "
;;           ;; value of current line number
;;           "line %l "
;;           "-- user: "
;;           ;; value of user
;;           (getenv "USER")))

;;display file path in header line

(defmacro with-face (str &rest properties)
  `(propertize ,str 'face (list ,@properties)))

(defun sl/make-header ()
  ""
  (let* ((sl/full-header (abbreviate-file-name buffer-file-name))
         (sl/header (file-name-directory sl/full-header))
         (sl/drop-str "[...]"))
    (if (> (length sl/full-header)
           (window-body-width))
        (if (> (length sl/header)
               (window-body-width))
            (progn
              (concat (with-face sl/drop-str
                                 :background "blue"
                                 :weight 'bold
                                 )
                      (with-face (substring sl/header
                                            (+ (- (length sl/header)
                                                  (window-body-width))
                                               (length sl/drop-str))
                                            (length sl/header))
                                 ;; :background "red"
                                 :weight 'bold
                                 )))
          (concat (with-face sl/header
                             ;; :background "red"
                             :foreground "#8fb28f"
                             :weight 'bold
                             )))
      (concat (with-face sl/header
                         ;; :background "green"
                         ;; :foreground "black"
                         :weight 'bold
                         :foreground "#8fb28f"
                         )
              (with-face (file-name-nondirectory buffer-file-name)
                         :weight 'bold
                         ;; :background "red"
                         )))))

(defun sl/display-header ()
  (setq header-line-format
        '("" ;; invocation-name
          (:eval (if (buffer-file-name)
                     (sl/make-header)
                   "%b")))))

(add-hook 'buffer-list-update-hook
          'sl/display-header)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ace-window
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package ace-window
  :ensure t
  :init
  (progn
    (setq aw-scope 'frame)
    (global-set-key (kbd "C-x O") 'other-frame)
  ;;  (global-set-key (kbd "M-o") 'hydra-frame-window)
    (global-set-key (kbd "M-o") 'ace-window)
    (custom-set-faces
     '(aw-leading-char-face
       ((t (:inherit ace-jump-face-foreground :height 3.0)))))
    ))

;;;;;;;;;;;;;;;;
(use-package hydra :ensure t)


;; hydra-frame-window is designed from ace-window (C-x o) and
;; matches aw-dispatch-alist with a few extra
(defhydra hydra-frame-window (:color red :hint nil)
  "
^Delete^                       ^Frame resize^             ^Window^                Window Size^^^^^^   ^Text^                         (__)
_0_: delete-frame              _g_: resize-frame-right    _t_: toggle               ^ ^ _k_ ^ ^        _K_                           (oo)
_1_: delete-other-frames       _H_: resize-frame-left     _e_: ace-swap-win         _h_ ^+^ _l_        ^+^                     /------\\/
_2_: make-frame                _F_: fullscreen            ^ ^                       ^ ^ _j_ ^ ^        _J_                    / |    ||
_d_: kill-and-delete-frame     _n_: new-frame-right       _w_: ace-delete-window    _b_alance^^^^      ^ ^                   *  /\\---/\\  ~~  C-x f ;
"
  ("0" delete-frame :exit t)
  ("1" delete-other-frames :exit t)
  ("2" make-frame  :exit t)
  ("b" balance-windows)
  ("d" kill-and-delete-frame :exit t)
  ("e" ace-swap-window)
  ("F" toggle-frame-fullscreen)   ;; is <f11>
  ("g" resize-frame-right :exit t)
  ("H" resize-frame-left :exit t)  ;; aw-dispatch-alist uses h, I rebind here so hjkl can be used for size
  ("n" new-frame-right :exit t)
  ;; ("r" reverse-windows)
  ("t" toggle-window-spilt)
  ("w" ace-delete-window :exit t)
  ("x" delete-frame :exit t)
  ("K" text-scale-decrease)
  ("J" text-scale-increase)
  ("h" shrink-window-horizontally)
  ("k" shrink-window)
  ("j" enlarge-window)
  ("l" enlarge-window-horizontally))
(global-set-key (kbd "C-x o") 'hydra-frame-window/body)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; hydra
;; https://www.reddit.com/r/emacs/comments/8of6tx/tip_how_to_be_a_beast_with_hydra/
;; (use-package hydra
;;   :defer 2
;;   :bind ("C-c t" . hydra-clock/body))

(defhydra hydra-clock (:color blue)
    "
    ^
    ^Clock^             ^Do^
    ^─────^─────────────^──^─────────
    _q_ quit            _c_ cancel
    ^^                  _d_ display
    ^^                  _e_ effort
    ^^                  _i_ in
    ^^                  _j_ jump
    ^^                  _o_ out
    ^^                  _r_ report
    ^^                  ^^
    "
    ("q" nil)
    ("c" org-clock-cancel :color pink)
    ("d" org-clock-display)
    ("e" org-clock-modify-effort-estimate)
    ("i" org-clock-in)
    ("j" org-clock-goto)
    ("o" org-clock-out)
    ("r" org-clock-report))
(global-set-key (kbd "C-c C-t") 'hydra-clock/body)

;;

;;
(defhydra hydra-zoom ()
  "zoom"
  ("g" text-scale-increase "in")
  ("l" text-scale-decrease "out"))
(global-set-key (kbd "C-x f") 'hydra-zoom/body)

;;

(eval-after-load 'gnus-group
  '(progn
     (defhydra hydra-gnus-group (:color blue)
       "Do?"
       ("a" gnus-group-list-active "REMOTE groups A A")
       ("l" gnus-group-list-all-groups "LOCAL groups L")
       ("c" gnus-topic-catchup-articles "Read all c")
       ("G" gnus-group-make-nnir-group "Search server G G")
       ("g" gnus-group-get-new-news "Refresh g")
       ("s" gnus-group-enter-server-mode "Servers")
       ("m" gnus-group-new-mail "Compose m OR C-x m")
       ("#" gnus-topic-mark-topic "mark #")
       ("q" nil "cancel"))
     ;; y is not used by default
     (define-key gnus-group-mode-map "y" 'hydra-gnus-group/body)))

;; gnus-summary-mode
(eval-after-load 'gnus-sum
  '(progn
     (defhydra hydra-gnus-summary (:color blue)
       "Do?"
       ("s" gnus-summary-show-thread "Show thread")
       ("h" gnus-summary-hide-thread "Hide thread")
       ("n" gnus-summary-insert-new-articles "Refresh / N")
       ("f" gnus-summary-mail-forward "Forward C-c C-f")
       ("!" gnus-summary-tick-article-forward "Mail -> disk !")
       ("p" gnus-summary-put-mark-as-read "Mail <- disk")
       ("c" gnus-summary-catchup-and-exit "Read all c")
       ("e" gnus-summary-resend-message-edit "Resend S D e")
       ("R" gnus-summary-reply-with-original "Reply with original R")
       ("r" gnus-summary-reply "Reply r")
       ("W" gnus-summary-wide-reply-with-original "Reply all with original S W")
       ("w" gnus-summary-wide-reply "Reply all S w")
       ("#" gnus-topic-mark-topic "mark #")
       ("q" nil "cancel"))
     ;; y is not used by default
     (define-key gnus-summary-mode-map "y" 'hydra-gnus-summary/body)))

;; gnus-article-mode
(eval-after-load 'gnus-art
  '(progn
     (defhydra hydra-gnus-article (:color blue)
       "Do?"
       ("f" gnus-summary-mail-forward "Forward")
       ("R" gnus-article-reply-with-original "Reply with original R")
       ("r" gnus-article-reply "Reply r")
       ("W" gnus-article-wide-reply-with-original "Reply all with original S W")
       ("o" gnus-mime-save-part "Save attachment at point o")
       ("w" gnus-article-wide-reply "Reply all S w")
       ("q" nil "cancel"))
     ;; y is not used by default
     (define-key gnus-article-mode-map "y" 'hydra-gnus-article/body)))

(eval-after-load 'message
  '(progn
     (defhydra hydra-message (:color blue)
       "Do?"
       ("ca" mml-attach-file "Attach C-c C-a")
       ("cc" message-send-and-exit "Send C-c C-c")
       ("q" nil "cancel"))
     (global-set-key (kbd "C-c C-y") 'hydra-message/body)))

;;
;; This is the basic launcher code. It makes a main differentiation according to the Emacs major-mode and for org uses the org-element-context and org-element-property functions to react according to the specific context element/type.

(defun dfeich/context-hydra-launcher ()
  "A launcher for hydras based on the current context."
  (interactive)
  (cl-case major-mode
    ('org-mode (let* ((elem (org-element-context))
                      (etype (car elem))
                      (type (org-element-property :type elem)))
                 (cl-case etype
                   (src-block (hydra-babel-helper/body))
                   (link (hydra-org-link-helper/body))
                   ((table-row table-cell) (hydra-org-table-helper/body) )
                   (t (message "No specific hydra for %s/%s" etype type)
                      (hydra-org-default/body))))
               )
    ('bibtex-mode (org-ref-bibtex-hydra/body))
    ('ibuffer-mode (hydra-ibuffer-main/body))
    (t (message "No hydra for this major mode: %s" major-mode))))

(global-set-key (kbd "<f9> <f9>") 'dfeich/context-hydra-launcher)

;;
(defhydra hydra-yank-pop ()
  "yank"
  ("C-y" yank nil)
  ("M-y" yank-pop nil)
  ("y" (yank-pop 1) "next")
  ("Y" (yank-pop -1) "prev")
  ("l" helm-show-kill-ring "list" :color blue))   ; or browse-kill-ring
(global-set-key (kbd "M-y") #'hydra-yank-pop/yank-pop)
(global-set-key (kbd "C-y") #'hydra-yank-pop/yank)

;;Movement
;;For many Emacs users, basic movement commands are the most frequently used! Set up a movement group that means we don't need to hold the control key.

(global-set-key
 (kbd "C-n")
 (defhydra hydra-move
   (:body-pre (next-line))
   "move"
   ("n" next-line)
   ("p" previous-line)
   ("f" forward-char)
   ("b" backward-char)
   ("a" beginning-of-line)
   ("e" move-end-of-line)
   ("v" scroll-up-command)
   ;; Converting M-v to V here by analogy.
   ("V" scroll-down-command)
   ("l" recenter-top-bottom)))

;; Org mode の C-c C-s で挿入する日付の曜日、英語曜日表記を強制する。

(setq system-time-locale "C")



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; yatex to latex 野鳥起動のための設定
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(use-package yatex
  :ensure t
 ; :config
)
 ;; (setq auto-mode-alist
 ;; 		(cons (cons "\\.tex$" ’yatex-mode) auto-mode-alist))
 ;;  (autoload ’yatex-mode "yatex" "Yet Another LaTeX mode" t)

  (setq load-path (cons (expand-file-name "~/Documents/latex/yatex") load-path))  
(autoload 'yatex-mode "yatex" "Yet Another LaTeX mode" t)

;; YaTeX-mode
;; (setq auto-mode-alist
;;       (cons (cons "\\.tex$" 'yatex-mode) auto-mode-alist))
;; (setq dvi2-command "xdvi"
;;       tex-command "platex"
;;       dviprint-command-format "dvips %s | lpr"
;;       YaTeX-kanji-code nil)

;; ;; YaHtml-mode
;; (setq auto-mode-alist
;;       (cons (cons "\\.html$" 'yahtml-mode) auto-mode-alist))
;; (autoload 'yahtml-mode "yahtml" "Yet Another HTML mode" t)
;; (setq yahtml-www-browser "netscape")

;; (setq mac-option-modifier 'meta)

;; (setq indent-tabs-mode nil)

;;;;;;;;;  ;;;;;;;;;;;;;;;;;;;  https://texwiki.texjp.org/?YaTeX#e2cbcccd  ;;;;;;;;;;;;;;

(autoload 'yatex-mode "yatex" "Yet Another LaTeX mode" t)
(setq auto-mode-alist
      (append '(("\\.tex$" . yatex-mode)
                ("\\.ltx$" . yatex-mode)
                ("\\.cls$" . yatex-mode)
                ("\\.sty$" . yatex-mode)
                ("\\.clo$" . yatex-mode)
                ("\\.bbl$" . yatex-mode)) auto-mode-alist))
(setq YaTeX-inhibit-prefix-letter t)
(setq YaTeX-kanji-code nil)
(setq YaTeX-latex-message-code 'utf-8)
(setq YaTeX-use-LaTeX2e t)
(setq YaTeX-use-AMS-LaTeX t)
(setq YaTeX-dvi2-command-ext-alist
      '(("TeXworks\\|texworks\\|texstudio\\|mupdf\\|SumatraPDF\\|Preview\\|Skim\\|TeXShop\\|evince\\|atril\\|xreader\\|okular\\|zathura\\|qpdfview\\|Firefox\\|firefox\\|chrome\\|chromium\\|MicrosoftEdge\\|microsoft-edge\\|Adobe\\|Acrobat\\|AcroRd32\\|acroread\\|pdfopen\\|xdg-open\\|open\\|start" . ".pdf")))
(setq tex-command "ptex2pdf -u -l -ot '-synctex=1'")
;(setq tex-command "lualatex -synctex=1")
;(setq tex-command "latexmk")
;(setq tex-command "latexmk -e '$latex=q/uplatex %O -synctex=1 %S/' -e '$bibtex=q/upbibtex %O %B/' -e '$biber=q/biber %O --bblencoding=utf8 -u -U --output_safechars %B/' -e '$makeindex=q/upmendex %O -o %D %S/' -e '$dvipdf=q/dvipdfmx %O -o %D %S/' -norc -gg -pdfdvi")
;(setq tex-command "latexmk -e '$lualatex=q/lualatex %O -synctex=1 %S/' -e '$bibtex=q/upbibtex %O %B/' -e '$biber=q/biber %O --bblencoding=utf8 -u -U --output_safechars %B/' -e '$makeindex=q/upmendex %O -o %D %S/' -norc -gg -pdflua")
(setq bibtex-command "latexmk -e '$latex=q/uplatex %O -synctex=1 %S/' -e '$bibtex=q/upbibtex %O %B/' -e '$biber=q/biber %O --bblencoding=utf8 -u -U --output_safechars %B/' -e '$makeindex=q/upmendex %O -o %D %S/' -e '$dvipdf=q/dvipdfmx %O -o %D %S/' -norc -gg -pdfdvi")
(setq makeindex-command  "latexmk -e '$latex=q/uplatex %O -synctex=1 %S/' -e '$bibtex=q/upbibtex %O %B/' -e '$biber=q/biber %O --bblencoding=utf8 -u -U --output_safechars %B/' -e '$makeindex=q/upmendex %O -o %D %S/' -e '$dvipdf=q/dvipdfmx %O -o %D %S/' -norc -gg -pdfdvi")
;(setq dvi2-command "xdg-open")
(setq dvi2-command "evince")
;(setq dvi2-command "atril")
;(setq dvi2-command "okular --unique")
;(setq dvi2-command "zathura -x \"emacsclient --no-wait +%{line} %{input}\"")
;(setq dvi2-command "qpdfview --unique")
;(setq dvi2-command "texworks")
;(setq dvi2-command "texstudio --pdf-viewer-only")
;(setq tex-pdfview-command "xdg-open")
(setq tex-pdfview-command "evince")
;(setq tex-pdfview-command "atril")
;(setq tex-pdfview-command "okular --unique")
;(setq tex-pdfview-command "zathura -x \"emacsclient --no-wait +%{line} %{input}\"")
;(setq tex-pdfview-command "qpdfview --unique")
;(setq tex-pdfview-command "texworks")
;(setq tex-pdfview-command "texstudio --pdf-viewer-only")
(setq dviprint-command-format "wine cmd /c start AcroRd32.exe `echo %s | sed -e \"s/\\.[^.]*$/\\.pdf/\"`")

(require 'dbus)

(defun un-urlify (fname-or-url)
  "A trivial function that replaces a prefix of file:/// with just /."
  (if (string= (substring fname-or-url 0 8) "file:///")
      (substring fname-or-url 7)
    fname-or-url))

(defun evince-inverse-search (file linecol &rest ignored)
  (let* ((fname (decode-coding-string (url-unhex-string (un-urlify file)) 'utf-8))
         (buf (find-file fname))
         (line (car linecol))
         (col (cadr linecol)))
    (if (null buf)
        (message "[Synctex]: %s is not opened..." fname)
      (switch-to-buffer buf)
      (goto-line (car linecol))
      (unless (= col -1)
        (move-to-column col))
      (x-focus-frame (selected-frame)))))

(dbus-register-signal
 :session nil "/org/gnome/evince/Window/0"
 "org.gnome.evince.Window" "SyncSource"
 'evince-inverse-search)

(with-eval-after-load 'yatexprc
  (defun YaTeX-preview-jump-line ()
    "Call jump-line function of various previewer on current main file"
    (interactive)
    (save-excursion
      (save-restriction
        (widen)
        (let*((pf (or YaTeX-parent-file
                      (save-excursion (YaTeX-visit-main t) (buffer-file-name))))
              (pdir (file-name-directory pf))
              (bnr (substring pf 0 (string-match "\\....$" pf)))
              ;(cf (file-relative-name (buffer-file-name) pdir))
              (cf (buffer-file-name)) ;2016-01-08
              (buffer (get-buffer-create " *preview-jump-line*"))
              (line (count-lines (point-min) (point-end-of-line)))
              (previewer (YaTeX-preview-default-previewer))
              (cmd (cond
                    ((string-match "Skim" previewer)
                     (format "%s %d '%s.pdf' '%s'"
                             YaTeX-cmd-displayline line bnr cf))
                    ((string-match "evince" previewer)
                     (format "%s '%s.pdf' %d '%s'"
                             "fwdevince" bnr line cf))
                    ((string-match "sumatra" previewer)
                     (format "%s \"%s.pdf\" -forward-search \"%s\" %d"
                             previewer bnr cf line))
                    ((string-match "zathura" previewer)
                     (format "%s --synctex-forward '%d:0:%s' '%s.pdf'"
                             previewer line cf bnr))
                    ((string-match "qpdfview" previewer)
                     (format "%s '%s.pdf#src:%s:%d:0'"
                             previewer bnr cf line))
                    ((string-match "okular" previewer)
                     (format "%s '%s.pdf#src:%d %s'"
                             previewer bnr line (expand-file-name cf)))
                    )))
          (YaTeX-system cmd "jump-line" 'noask pdir))))))

(add-hook 'yatex-mode-hook
          '(lambda ()
             (auto-fill-mode -1)))

;;
;; RefTeX with YaTeX
;;
;(add-hook 'yatex-mode-hook 'turn-on-reftex)
(add-hook 'yatex-mode-hook
          '(lambda ()
             (reftex-mode 1)
             (define-key reftex-mode-map (concat YaTeX-prefix ">") 'YaTeX-comment-region)
             (define-key reftex-mode-map (concat YaTeX-prefix "<") 'YaTeX-uncomment-region)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; ox-org : org-mode export to latex
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;
;; Org mode
;;
(require 'ox-latex)
(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))
(setq org-latex-default-class "bxjsarticle")
(setq org-latex-pdf-process '("latexmk -e '$latex=q/uplatex %S/' -e '$bibtex=q/upbibtex %B/' -e '$biber=q/biber --bblencoding=utf8 -u -U --output_safechars %B/' -e '$makeindex=q/upmendex -o %D %S/' -e '$dvipdf=q/dvipdfmx -o %D %S/' -norc -gg -pdfdvi %f"))
;(setq org-latex-pdf-process '("latexmk -e '$lualatex=q/lualatex %S/' -e '$bibtex=q/upbibtex %B/' -e '$biber=q/biber --bblencoding=utf8 -u -U --output_safechars %B/' -e '$makeindex=q/upmendex -o %D %S/' -norc -gg -pdflua %f"))
;(setq org-export-in-background t)
(setq org-file-apps
      '(("pdf" . "evince %s")))

(add-to-list 'org-latex-classes
             '("bxjsarticle"
               "\\documentclass[autodetect-engine,dvi=dvipdfmx,11pt,a4paper,ja=standard]{bxjsarticle}
[NO-DEFAULT-PACKAGES]
\\usepackage{amsmath}
\\usepackage{newtxtext,newtxmath}
\\usepackage{graphicx}
\\usepackage{hyperref}
\\ifdefined\\kanjiskip
  \\usepackage{pxjahyper}
  \\hypersetup{colorlinks=true}
\\else
  \\ifdefined\\XeTeXversion
      \\hypersetup{colorlinks=true}
  \\else
    \\ifdefined\\directlua
      \\hypersetup{pdfencoding=auto,colorlinks=true}
    \\else
      \\hypersetup{unicode,colorlinks=true}
    \\fi
  \\fi
\\fi"
               ("\\section{%s}" . "\\section*{%s}")
               ("\\subsection{%s}" . "\\subsection*{%s}")
               ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
               ("\\paragraph{%s}" . "\\paragraph*{%s}")
               ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))

(add-to-list 'org-latex-classes
             '("jlreq"
               "\\documentclass[11pt,paper=a4]{jlreq}
[NO-DEFAULT-PACKAGES]
\\usepackage{amsmath}
\\usepackage{newtxtext,newtxmath}
\\ifdefined\\kanjiskip
  \\usepackage[dvipdfmx]{graphicx}
  \\usepackage[dvipdfmx]{hyperref}
  \\usepackage{pxjahyper}
  \\hypersetup{colorlinks=true}
\\else
  \\usepackage{graphicx}
  \\usepackage{hyperref}
  \\hypersetup{pdfencoding=auto,colorlinks=true}
\\fi"
               ("\\section{%s}" . "\\section*{%s}")
               ("\\subsection{%s}" . "\\subsection*{%s}")
               ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
               ("\\paragraph{%s}" . "\\paragraph*{%s}")
               ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))

(add-to-list 'org-latex-classes
             '("jlreq-tate"
               "\\documentclass[tate,11pt,paper=a4]{jlreq}
[NO-DEFAULT-PACKAGES]
\\usepackage{amsmath}
\\usepackage{newtxtext,newtxmath}
\\ifdefined\\kanjiskip
  \\usepackage[dvipdfmx]{graphicx}
  \\usepackage[dvipdfmx]{hyperref}
  \\usepackage{pxjahyper}
  \\hypersetup{colorlinks=true}
\\else
  \\usepackage{graphicx}
  \\usepackage{hyperref}
  \\hypersetup{pdfencoding=auto,colorlinks=true}
\\fi"
               ("\\section{%s}" . "\\section*{%s}")
               ("\\subsection{%s}" . "\\subsection*{%s}")
               ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
               ("\\paragraph{%s}" . "\\paragraph*{%s}")
               ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))

;
;; ↑
;; 使い方 †
;; ↑
;; bxjsarticle を使用する場合 †

;; org ファイルの先頭に

;; #+TITLE: hoge
;; #+AUTHOR: fuga
;; #+LATEX_CLASS: bxjsarticle p
 
;; を追加します．
;; ↑
;; jlreq の横書きを使用する場合 †

;; org ファイルの先頭に

;; #+TITLE: hoge
;; #+AUTHOR: fuga
;; #+LATEX_CLASS: jlreq

;; を追加します．
;; ↑
;; jlreq の縦書きを使用する場合 †

;; org ファイルの先頭に

;; #+TITLE: hoge
;; #+AUTHOR: fuga
;; #+LATEX_CLASS: jlreq-tate

;; を追加します．

;;



;;;;;;;;;;;;;;;; eof


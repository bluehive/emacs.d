;;; init.el --- Emacs initialization file -*- lical-binding: t -*-

;; thankyou :: Jon Dufresne <jon@jondufresne.org>

;;; Commentary:
;;;; Emacsおよび関連ソフトのインストール方法
;;;;; Ubuntu
;;sudo add-apt-repository ppa:kelleyk/emacs
;;sudo apt-get update
;;    % sudo apt-get install emacs25

;;;;; Windows ;;;;;;;;;;;;;;;;;;;
;; Chocolateyを使った環境構築の時のメモ - Qiita
;; - http://qiita.com/konta220/items/95b40b4647a737cb51aa
;; まずはこれで一括してインストール可能である
;; emacs25 Msys2 ripgrep etc
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; NTEmacs64(64bit 版 version 25.1)
;; GitHub - chuntaro/NTEmacs64: Windows 版 Emacs (通称 NTEmacs) の 64bit 版
;; - https://github.com/chuntaro/NTEmacs64
;;
;; - MSYS2 homepage - http://www.msys2.org/
;; 下記の例は C:\Programs\ の下に MSYS2 をインストールした場合です。
;; このパスは、ご自身のインストールパスに合わせて変更してください。
;; ;; MSYS2 のコマンドを使えるようにする.
;; (setenv "PATH"
;; (concat
;; ;; 下記の行に MSYS2 のコマンドの実行可能ファイルがある場所を設定してください. スラッシュが2つ連続することに注意！
;; "C:\\Programs\\msys64\\usr\\bin;"
;; (getenv "PATH")))

;; Msys2 Update the package database and core system packages with:
 ;; - pacman -Syu
 ;; If needed, close MSYS2, run it again from Start menu. Update the rest with:
 ;; - pacman -Su
;; 更新について　Arch Linuxと同様に、単純に、
;; $ pacman -Syuu
;; で、パッケージデータベースの更新(-Sy)とパッケージの更新(-Suu)をまとめて行えます。
;; -Suではなく、-Suuとしているのはダウングレードも許可するためです。
;;  警告: terminate MSYS2 without returning to shell and check for updates again
;;  警告: for example close your terminal window instead of calling exit
;; 更新するパッケージがなくなるまで、何回かこの手順を繰り返してください。
;; なお、2回目以降は、パッケージデータベースが更新済みなので、
;;  $ pacman -Suu
;; でも問題ありません。

;;;;;;;;;;;;;;;;;;;;;;;;
;; package initialize ;;
;(require 'autoinsert) ; abc-mode-autoloads.el 対策。
(package-initialize)
(setq package-enable-at-startup nil) ; 初期化済みなので自動初期化は停止。

;; パッケージの情報は、~/.emacs.d/elpa/archives/ に格納される。自分で
;; パッケージを作る場合は、 package-x.el の、
;; `package-upload-{buffer,file}' を利用する。archive-contents ファイ
;; ルが自動生成される。

;; パッケージ読み込み後は、読み込みライブラリを表示する。
;; （繁雑な XXXX-autoload は表示させない。）

;; ただし、package-menu-execute 時のみ、(XXX-autoload.elを) 表示させない。
(defadvice package-menu-execute (around tkw-package-menu-execute-suppress-load-messages)
  "Suppress displaying load file messages."
  (let ((force-load-messages nil))
    ad-do-it))
(ad-activate 'package-menu-execute)

;;;; Package Archives
(set-variable 'package-archives
              '(("gnu" . "http://elpa.gnu.org/packages/")
;;                ("melpa" . "http://melpa.milkbox.net/packages/")
                ("melpa" . "http://melpa.org/packages/")
                ;; sunrise-commander
                ;; ("SC"   . "http://joseito.republika.pl/sunrise-commander/")
                ;; org-mode
                ("org"   . "http://orgmode.org/elpa/")
                ))

;;;; 大きすぎるファイルでは適用しないモードの判定
(defun tkw-large-file-p (&optional char-size)
  "現在のバッファの文字数が CHAR-SIZE を越えるか判定する.
デフォルト値は 100,000."
  (< (or char-size 100000) (point-max)))

;;;; 重要
;;;;This vulnerability was introduced in Emacs 19.29. To work around that in Emacs versions before 25.3, append the following to your ~/.emacs init file:

(eval-after-load "enriched"
  '(defun enriched-decode-display-prop (start end &optional param)
     (list start end)))

;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
;;; @ use-packageで可読性の高いinit.elを書く -
;; Qiita - http://qiita.com/kai2nenobu/items/5dfae3767514584f5220#use-package-%E3%81%8C%E5%AD%98%E5%9C%A8%E3%81%97%E3%81%AA%E3%81%84%E5%A0%B4%E5%90%88
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;

;;;; use-package
;; - url :: https://github.com/jwiegley/use-package
;; 非標準パッケージは use-package で管理する。
;; （標準ライブラリは use-package では管理しない）
;; これを利用できない環境では、パッケージ管理のソフトはインストールしない。

;;;; Emacs 25 対策
;; 2015年1月時点では、use-package の前にこれを読まないと Emacs 25 ではエラーになる。
(require 'xref nil t)

;;;;; 起動時の use-package の抑止
;; - init.el を外部に持ちだした時など、use-package を抑止したいときは
;;   Emacs を、オプション "--qq" で起動する。
;; - use-package が未インストールか、抑止されている場合は空マクロにする。
(eval-and-compile
(when (or (member "--qq" command-line-args)
          (null (require 'use-package nil t)))
  (warn "`use-package' is unavailable!  Please install it via `M-x list-packages' if possible.")
  (defmacro use-package (&rest _args))))
;; 後の startup.el におけるオプション認識エラーを防止
(add-to-list 'command-switch-alist '("--qq" . (lambda (switch) nil)))

;;;;; 形式的宣言
(use-package use-package :no-require t :ensure t :defer t)

;;We can control the amount of output use-package generates by setting use-package-verbose to true.
(setq use-package-verbose t)

;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
;;; @ language - coding system                                      ;;;
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;

;; ------------------------------------------------------------------------
;; @ character code (文字コード)
;; Setenv
(set-language-environment "Japanese")

;; (when (equal system-type 'ns)
;;   (require 'ucs-normalize)
;;   (prefer-coding-system 'utf-8)
;;   (setq file-name-coding-system 'utf-8)
;;   (setq locale-coding-system 'utf-8)
;; 					;         (prefer-coding-system 'shift_jis)
;;   (set-default-coding-systems 'utf-8)
;; 					;         (setq file-name-coding-system 'shift_jis) (setq
;; 					;         default-file-name-coding-system 'shift_jis)
;;   (setq default-process-coding-system '(utf-8-hfs . cp932)) ;agで日本語検索させるためのおまじない
;;   )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; 重要　日本語Windowsの文字コード対策
;; https://www49.atwiki.jp/ntemacs/pages/16.html 

(require 'cl-lib)

(setenv "LANG" "ja_JP.UTF-8")

;; IME の設定をした後には実行しないこと
;; (set-language-environment 'Japanese)

(prefer-coding-system 'utf-8-unix)
(set-file-name-coding-system 'cp932)
(setq locale-coding-system 'utf-8)

;; プロセスが出力する文字コードを判定して、process-coding-system の DECODING の設定値を決定する
(setq default-process-coding-system '(undecided-dos . utf-8-unix))

;; ldd の結果のキャッシュ
(defvar ldd-cache nil)

;; filename が cygwin のプログラムかどうか判定する
;; (defun cygwin-program-p (filename)
;;   (let ((target (and filename (executable-find filename))))
;;     (when target
;;       (cdr (or (assoc target ldd-cache)
;;                (car (push (cons target
;;                                 (with-temp-buffer
;;                                   (let ((w32-quote-process-args nil)) ; advice 中で再帰しないよう nil
;;                                     ;; cygwin のライブラリをロードしているか判定
;;                                     (when (eq (call-process "ldd" nil t nil (concat "\"" target "\"")) 0)
;;                                       (goto-char (point-min))
;;                                       (number-or-marker-p
;;                                        (re-search-forward "cygwin[0-9]+\.dll" nil t))))))
;;                           ldd-cache)))))))

;; ;; サブプロセスに渡すパラメータに SJIS のダメ文字対策を行い、さらに文字コードを cp932 にする
;; (defun convert-process-args (orig-fun prog-pos args-pos args)
;;   (let ((cygwin-quote (and w32-quote-process-args ; cygwin-program-p の再帰防止
;;                            (cygwin-program-p (nth prog-pos args)))))
;;     (setf (nthcdr args-pos args)
;;           (mapcar (lambda (arg)
;;                     (when w32-quote-process-args
;;                       (setq arg
;;                             (concat "\""
;;                                     (if cygwin-quote
;;                                         (replace-regexp-in-string "[\"\\\\]"
;;                                                                   "\\\\\\&"
;;                                                                   arg)
;;                                       (replace-regexp-in-string "\\(\\(\\\\\\)*\\)\\(\"\\)"
;;                                                                 "\\1\\1\\\\\\3"
;;                                                                 arg))
;;                                     "\"")))
;;                     (if (multibyte-string-p arg)
;;                         (encode-coding-string arg 'cp932)
;;                       arg))
;;                   (nthcdr args-pos args))))

;;   (let ((w32-quote-process-args nil))
;;     (apply orig-fun args)))

;; (cl-loop for (func prog-pos args-pos) in '((call-process        0 4)
;;                                            (call-process-region 2 6)
;;                                            (start-process       2 3))
;;          do (eval `(advice-add ',func
;;                                :around (lambda (orig-fun &rest args)
;;                                          (convert-process-args orig-fun
;;                                                                ,prog-pos ,args-pos
;;                                                                args))
;;                                '((depth . 99)))))

;; end of 日本語文字コード　設定
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
;;; @ helm                                                          ;;;
;;; https://qiita.com/jabberwocky0139/items/86df1d3108e147c69e2c#%E3%82%AB%E3%83%BC%E3%82%BD%E3%83%AB%E4%BB%98%E8%BF%91%E3%81%AE%E3%83%86%E3%82%AD%E3%82%B9%E3%83%88
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;


(use-package helm
  :defer t
  :ensure t)

(require 'helm)
(require 'helm-config)

(global-set-key (kbd "C-x b") 'helm-mini)
;あいまい一致を有効にするには以下を追加しましょう:
(setq helm-buffers-fuzzy-matching t
      helm-recentf-fuzzy-match    t)

;; The default "C-x c" is quite close to "C-x C-c", which quits Emacs.
;; Changed to "C-c h". Note: We must set "C-c h" globally, because we
;; cannot change `helm-command-prefix-key' once `helm-config' is loaded.
(global-set-key (kbd "C-c h") 'helm-command-prefix)
(global-unset-key (kbd "C-x c"))

(define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action) ; rebind tab to run persistent action
(define-key helm-map (kbd "C-i") 'helm-execute-persistent-action) ; make TAB work in terminal
(define-key helm-map (kbd "C-z")  'helm-select-action) ; list actions using C-z

(when (executable-find "curl")
  (setq helm-google-suggest-use-curl-p t))

(setq helm-split-window-in-side-p           t ; open helm buffer inside current window, not occupy whole other window
      helm-move-to-line-cycle-in-source     t ; move to end or beginning of source when reaching top or bottom of source.
      helm-ff-search-library-in-sexp        t ; search for library in `require' and `declare-function' sexp.
      helm-scroll-amount                    8 ; scroll 8 lines other window using M-<next>/M-<prior>
      helm-ff-file-name-history-use-recentf t
      helm-echo-input-in-header-line t)

(defun spacemacs//helm-hide-minibuffer-maybe ()
  "Hide minibuffer in Helm session if we use the header line as input field."
  (when (with-helm-buffer helm-echo-input-in-header-line)
    (let ((ov (make-overlay (point-min) (point-max) nil nil t)))
      (overlay-put ov 'window (selected-window))
      (overlay-put ov 'face
                   (let ((bg-color (face-background 'default nil)))
                     `(:background ,bg-color :foreground ,bg-color)))
      (setq-local cursor-type nil))))


(add-hook 'helm-minibuffer-set-up-hook
          'spacemacs//helm-hide-minibuffer-maybe)

(setq helm-autoresize-max-height 0)
(setq helm-autoresize-min-height 20)
(helm-autoresize-mode 1)

(helm-mode 1)

;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
;;; @ anything                                                      ;;;
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;

;; (use-package anything
;;   :defer t
;;   :ensure t)
;; (require 'anything-config)
;; (setq anything-enable-shortcuts 'prefix)
;; (define-key anything-map (kbd "@") 'anything-select-with-prefix-shortcut)
;; (global-set-key (kbd "C-x b") 'anything-mini)

;; ;;
;; (defun my-anything ()
;;   (interactive)
;;   (anything-other-buffer
;;    '( ;anything-c-source-buffers
;;      anything-c-source-file-name-history
;;      anything-c-source-info-pages
;;      anything-c-source-info-elisp
;;      anything-c-source-man-pages
;;      anything-c-source-locate
;;      anything-c-source-emacs-commands)
;;    " *my-anything*"))


;; ;; Then type M-x my-anything to use sources.
;; ;;
;; ;; Defining own command is better than setup `anything-sources'
;; ;; directly, because you can define multiple anything commands with
;; ;; different sources. Each anything command should have own anything
;; ;; buffer, because M-x anything-resume revives anything command.


;; (global-set-key (kbd "C-:") 'my-anything)

;; ;; ;少し複雑な設定例として
;; ;; 1. 場所elinitは~/.emacs.d/のみ
;; ;;    - ~/.emacs.d/以下でrgを動かす
;; ;; 2. 場所melmagは~/memo/emacs-melmag/のみ
;; ;;    - ~/memo/emacs-melmag/以下でrgを動かす
;; ;; 3. 場所memoは~/sync/think/と~/memo/
;; ;;    - andgrepスクリプトを2つのディレクトリで動かす


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
    (exec-path-from-shell dired-quick-sort dired+ ace-window web-mode yaml-mode systemd projectile pony-mode pip-requirements grep-a-lot flycheck flx-ido diff-hl apache-mode auto-async-byte-compile paredit lispxmp open-junk-file ripgrep rg ht yasnippet rainbow-mode ## recentf-ext anything)))
 '(send-mail-function (quote smtpmail-send-it))

(add-hook 'cperl-mode-hook
       (lambda ()
         (local-set-key (kbd "C-h f") 'cperl-perldoc)))


;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
;;; @ アクティベーション                                            ;;;
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;

;; 警告音の代わりに画面フラッシュ
(setq visible-bell t)

;; ウィンドウの右端で改行をするかどうかを切り替えるための設定

(global-set-key (kbd "M-<return>") 'toggle-truncate-lines)

;; ;;; 括弧の範囲内を強調表示
;; (setq show-paren-delay 0)
;; (show-paren-mode t)
;; (setq show-paren-style 'expression)

;; ;; 括弧の範囲色
;; (set-face-background 'show-paren-match-face "#800")

;; ;;; 選択領域の色
;; (set-face-background 'region "#555")

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


(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(aw-leading-char-face ((t (:inherit ace-jump-face-foreground :height 3.0)))))


;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
;;; dired edit                                                    ;;;
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;

; https://www.emacswiki.org/emacs/DiredPlus
;; (use-package dired+
;;   :ensure t
;;   :init
;;   ;; diredを2つのウィンドウで開いている時に、デフォルトの移動orコピー先をもう一方のdiredで開いているディレクトリにする
;;   (setq dired-dwim-target t)
;; ; (diredp-hide-details-initially-flag nil)
;;   :config (require 'dired+)
;;   ;; .zipで終わるファイルをZキーで展開できるように
;;   (add-to-list 'dired-compress-file-suffixes '("\\.zip\\'" ".zip" "unzip"))
;;   )

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
  :config
  )

; 
;diredデフォルトではiを押すことでサブディレクトリを展開するが，
;別ディレクトリとしてdiredバッファの下側に中身が表示される．
(use-package dired-subtree
  :ensure t
  :config
  )

(use-package dired-open
  :ensure t
  :config
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
(require 'dired-x)


;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
;;; @ ddskk rubikichi                                               ;;;
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
				;	==
;; Windows 環境だと [noconvert]
(setq skk-sticky-key [muhenkan])
;==
(require 'skk-hint)

;;　muhenkanなどのキー名はどうやって取得するのかというと、 <f1> c を使います。その後に無変換キーを押せば「<muhenkan> is undefined」と出てきます。

(when (require 'skk nil t)
  (global-set-key (kbd "C-x j") 'skk-auto-fill-mode) ;;良い感じに改行を自動入力してくれる機能
  (setq default-input-method "japanese-skk")         ;;emacs上での日本語入力にskkをつかう
  (require 'skk-study))                              ;;変換学習機能の追加


;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
;;; @ rubikichi                                                     ;;;
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;

;;;== List2:C-x C-bを置き換える
;;(global-set-key (kbd "C-x C-b") 'bs-show)


;; eww google
(setq eww-search-prefix "http://www.google.co.jp/search?q=")

;; eww colore
(defvar eww-disable-colorize t)
(defun shr-colorize-region--disable (orig start end fg &optional bg &rest _)
  (unless eww-disable-colorize
    (funcall orig start end fg)))
(advice-add 'shr-colorize-region :around 'shr-colorize-region--disable)
(advice-add 'eww-colorize-region :around 'shr-colorize-region--disable)
(defun eww-disable-color ()
  "eww で文字色を反映させない"
  (interactive)
  (setq-local eww-disable-colorize t)
  (eww-reload))
(defun eww-enable-color ()
  "eww で文字色を反映させる"
  (interactive)
  (setq-local eww-disable-colorize nil)
  (eww-reload))

;; 試行錯誤用ファイルを開くための設定
(require 'open-junk-file)
;; C-x C-zで試行錯誤ファイルを開く
(global-set-key (kbd "C-x C-z") 'open-junk-file)

;; ;; 式の評価結果を注釈するための設定
(use-package lispxmp
  :ensure t
  :config
  )

;(require 'lispxmp)
;; emacs-lisp-mode でC-c C-dを押すと注釈される
(define-key emacs-lisp-mode-map (kbd "C-c C-d") 'lispxmp)

;; 括弧の対応を保持して編集する設定
(use-package paredit
  :ensure t
  :config
  )
(require 'paredit)
(add-hook 'emacs-lisp-mode-hook 'enable-paredit-mode)
(add-hook 'lisp-interaction-mode-hook 'enable-paredit-mode)
(add-hook 'lisp-mode-hook 'enable-paredit-mode)
(add-hook 'ielm-mode-hook 'enable-paredit-mode)
(add-hook 'cperl-mode-hook 'enable-paredit-mode)

;; 自動バイトコンパイルを無効にするファイル名の正規表現
;; (require 'auto-async-byte-compile)
;; (setq auto-async-byte-compile-exclude-file-regexp "/junk/")
;; (add-hook 'emacs-lisp-mode-hook 'enable-auto-async-byte-compile-mode)
;; (add-hook 'emacs-lisp-mode-hook 'turn-on-eldoc-mode)
;; (add-hook 'lisp-interaction-mode-hook 'turn-on-eldoc-mode)
;; (add-hook 'ielm-mode-hook 'turn-on-eldoc-mode)

(setq eldoc-idle-delay 0.2) ; すぐに表示したい
(setq eldoc-minor-mode-string "") ; モードラインにElDocと表示しない

;; 釣り合いのとれる括弧をハイライトにする
(show-paren-mode 1)

;; 改行と同時にインデントも行う
(global-set-key "\C-m" 'newline-and-indent)

;; find-functionをキー割り当てする
(find-function-setup-keys)

;; org-mode
(require 'org)

;; 日本語info設定
;;; ~/info/以下をinfoファイル検索ディレクトリに加える
;(add-to-list 'Info-directory-list "~/info/")

;; Emacs M-x toggle-truncate-lines: 長い行の折り返し表示を切り換える
(global-set-key (kbd "C-c t") 'toggle-truncate-lines)

;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
;;; @ cperl-mode
;;; https://www.emacswiki.org/emacs/CPerlMode                       ;;;
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;

;;; cperl-mode is preferred to perl-mode
;;; "Brevity is the soul of wit" <foo at acm.org>
(defalias 'perl-mode 'cperl-mode)

(add-to-list 'auto-mode-alist '("\\.\\([pP][Llm]\\|al\\)\\'" . cperl-mode))
(add-to-list 'interpreter-mode-alist '("perl" . cperl-mode))
(add-to-list 'interpreter-mode-alist '("perl5" . cperl-mode))
(add-to-list 'interpreter-mode-alist '("miniperl" . cperl-mode))

					;(cperl-set-style "PerlStyle")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;; custom
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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
 '(org-agenda-files (quote ("~/org/todo.org")))
 '(org-babel-load-languages (quote ((emacs-lisp . t) (awk . t) (perl . t) (shell . t))))
 '(org-capture-templates
   (quote
    (("n" "etc notes" entry
      (file "~/org/notes.org")
      "" :prepend t :jump-to-captured t :clock-in t :clock-keep t)
     ("t" "todo list" checkitem
      (file "~/org/todo.org")
      "" :empty-lines 1 :clock-in t))))
 '(package-selected-packages
   (quote
    (smart-mode-line dired-open dired-subtree dired-filter systemd ag aggressive-indent pcre2el projectile golden-ratio magit-gh-pulls magit yasnippet yaml-mode web-mode use-package ripgrep rg recentf-ext rainbow-mode pony-mode pip-requirements phi-rectangle peg paredit paradox package-utils org-toodledo org-table-comment org-plus-contrib org-octopress org-bullets open-junk-file lispxmp grep-a-lot flx-ido exec-path-from-shell evil dired+ dash-functional browse-at-remote auto-async-byte-compile apache-mode anything adaptive-wrap ace-window)))
 '(safe-local-variable-values (quote ((lical-binding . t))))
 '(skk-annotation-other-sources
   (quote
    (ja\.wikipedia en\.wiktionary simple\.wikipedia en\.wikipedia)))
 '(skk-auto-insert-paren t)
 '(skk-auto-okuri-process nil)
 '(skk-auto-start-henkan nil)
 '(skk-aux-large-jisyo "/home/blue/.emacs.d/skk-get-jisyo/SKK-JISYO.L")
 '(skk-cdb-large-jisyo nil)
 '(skk-check-okurigana-on-touroku (quote ask))
 '(skk-date-ad t)
 '(skk-delete-implies-kakutei t)
 '(skk-egg-like-newline t)
 '(skk-extra-jisyo-file-list
   (quote
    ("/home/blue/.emacs.d/skk-get-jisyo/SKK-JISYO.jinmei" "/home/blue/.emacs.d/skk-get-jisyo/SKK-JISYO.geo" "/home/blue/.emacs.d/skk-get-jisyo/SKK-JISYO.pubdic+")))
 '(skk-henkan-okuri-strictly nil)
 '(skk-henkan-strict-okuri-precedence nil)
 '(skk-itaiji-jisyo "/home/blue/.emacs.d/skk-get-jisyo/SKK-JISYO.itaiji")
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
 '(skk-verbose t))

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

;;; perl v path ;;;;
;(when (memq window-system '(mac ns))
  (exec-path-from-shell-initialize)
  exec-path	(split-string (getenv "PATH") ":")
 ; /home/blue/.plenv/versions/5.28.0/lib/perl5/5.28.0/

   	; /home/kato/	.	plenv/versions/5.27.2/bin
  (let ((path exec-path))
    (format "  exec-path: %s\n" exec-path))
  ;;exec-path-from-shell.el
  ;;shell のパスをそのまま通す　重要 ;;
  (use-package exec-path-from-shell
   ; :no-require t
    :defer t
    :ensure t
    :init		(exec-path-from-shell-initialize)
    )
 ;; )
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
;;  pcre2el rxt-mode http://emacs.rubikitch.com/pcre2el/
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
;; 正規表現変換・解説
;; M-x rxt-mode でRegular eXpression Translationマイナーモード
;; C-c / /
;;     rxt-explain 正規表現を解説


;; ;;; regexp perl
;; (require 'pcre2el)
;; (add-hook 'prog-mode-hook 'rxt-mode)
;; (setq reb-re-syntax 'pcre)


;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
;; 日本語grep対策　
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;

(when (equal system-type 'windows-nt)
  (setq
   find-program "C:\\tools\\msys64\\usr\\bin\\find.exe"
;;   grep-program "C:\\tools\\msys64\\usr\\bin\\grep.exe"
      grep-program "C:\\tools\\msys64\\usr\\bin\\rg.exe")
)

;; 重要　gitなどパスを通す　Windows
;; (setq exec-path (cons "C:\\tools\\msys64\\usr\\bin" exec-path))

;; Ag.el
;; https://agel.readthedocs.io/en/latest/installation.html#emacs
;; Afterwards, you can install ag.el from MELPA (the recommended approach).
;;:: Functions are autoloaded, so (require 'ag) is unnecessary.
;; silver_searcher https://github.com/ggreer/the_silver_searcher\
; ag
;(require 'ag)
;(setq ag-highlight-search nil)  ; 検索キーワードをハイライト
;(setq ag-reuse-buffers nil)     ; 検索用バッファを使い回す (検索ごとに新バッファを作らない)

; wgrep
;; (add-hook 'ag-mode-hook '(lambda ()
;;                            (require 'wgrep-ag)
;;                            (setq wgrep-auto-save-buffer t)  ; 編集完了と同時に保存
;;                            (setq wgrep-enable-key "r")      ; "r" キーで編集モードに
;;                            (wgrep-ag-setup)))


;;;;ripgrep.el
;;;;[url=http://emacs.rubikitch.com/ripgrep/]ripgrep.el :
;;;【agより、ずっとはやい!!】超音速grepとEmacsインターフェース(Windows安心)[/url]
;;; rgバイナリの位置
;;  (setq ripgrep-executable  "C:\\tools\\msys64\\usr\\bin\\rg")
;; ;;; rgに渡すオプション
;;  (setq ripgrep-arguments '("-S"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 日本語環境 windows grep対策
;; https://qiita.com/ignorant/items/76e4c162cedc47336e75#%E5%85%B1%E9%80%9A%E3%81%AE%E8%A8%AD%E5%AE%9A
;; https://extra-vision.blogspot.jp/2016/01/ntemacs-ag-silver-searcher.html
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(when (equal system-type 'ns)
 ; (require 'ucs-normalize)
 ; (set-language-environment "Japanese")
 ; (prefer-coding-system 'utf-8-unix)
 ; (set-file-name-coding-system 'cp932)
  (set-keyboard-coding-system 'cp932)
  (set-terminal-coding-system 'cp932)
;  (setq default-process-coding-system '(utf-8-unix . cp932)) ;agで日本語検索させるためのおまじないだが効果なし
  )

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
;; -*- coding: utf-8 -*-
;; http://emacs.clickyourstyle.com/articles/299

;; todo:: ripgrepで日本語検索可能だが、ファイル文字コードがshit-jisだと読まれない
;;
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
;; Basic config;;; @
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

;; Buffer clean up
;; (prefer-coding-system 'utf-8)
;; (require 'whitespace)
;; (defun cleanup-buffer ()
;;   "Set the preferred style upon save."
;;   (set-buffer-file-coding-system 'utf-8)
;;   (let ((whitespace-style '(empty trailing)))
;;     (whitespace-cleanup)))
;; (add-hook 'before-save-hook #'cleanup-buffer)

;; Fix ibuffer to use ido-find-file
(require 'ibuffer)
(define-key ibuffer-mode-map (kbd "C-x C-f") #'ido-find-file)
;; Always use ibuffer
(global-set-key [remap list-buffers] #'ibuffer)

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
            (ido-everywhere 1)
            (flx-ido-mode 1)))

;;;;; jump use avy
(use-package avy
  :ensure t
  :demand
  :commands (avy-goto-char
             avy-goto-char-2
             avy-goto-word-1)
  :config
  ;; Darken background
  (setq avy-background t)
  ;;
  ;;
  (global-set-key (kbd "C-M-;") 'avy-goto-char-timer)
  )

; (use-package s :ensure t)

; (use-package systemd :ensure t)

;; (use-package undo-tree :ensure t
;;   :config (global-undo-tree-mode 1))

 (use-package yaml-mode :ensure t)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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


(use-package ace-window
:ensure t
:init
(progn
(setq aw-scope 'frame)
(global-set-key (kbd "C-x O") 'other-frame)
  (global-set-key [remap other-window] 'ace-window)
  (custom-set-faces
   '(aw-leading-char-face
     ((t (:inherit ace-jump-face-foreground :height 3.0)))))
  ))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;; Writing Mail  via emacswiki ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
					; Gnus:
(setq mail-user-agent 'message-user-agent)

					; Rmail:
;(setq mail-user-agent 'sendmail-user-agent)

					; MH-E:
;(setq mail-user-agent 'mh-e-user-agent)

					; Gnus is using MessageMode part:
(setq message-send-mail-function 'smtpmail-send-it)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;; message mode configuration ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;(setq smtpmail-smtp-server "smtp.somewhere.jp.com")
(setq message-send-mail-function 'message-smtpmail-send-it)

(add-hook 'message-mode-hook 'toggle-input-method)
(setq smtpmail-debug-info t)
(setq message-default-mail-headers "Cc: \nBcc:\n")
(setq message-auto-save-directory "~/Mail/drafts")


;; Additional extensions.
;;(require 'myproject)



;;;  
;; You don't need to unset a key before you rebind it to something else. This should do what you want:

;; ;; create a new prefix map
;; (define-prefix-command 'my-keymap)
;; ;; bind the new keymap to C-e 
;; (global-set-key "\C-e" my-keymap)
;; ;; bind the individual commands:
;; (define-key my-keymap "e" 'move-end-of-line)
;; (define-key my-keymap "r" 'end-of-buffer)

;; Now hitting C-e is a prefix, and C-e e calls end-of-line etc.

;; unset key 
(global-unset-key (kbd "C-z"))
(global-unset-key (kbd "C-x C-c"))


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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;smart-mode-lineは，特にMode Line用に色々とlispを書かずともいい感じにMode Lineを見やすく&使いやすくしてくれるpackageです！

;; (use-package smart-mode-line
;; 					; :no-require t
;;   :ensure t
;;   :init
;;   )
;; (require 'smart-mode-line)
;; (defvar sml/no-confirm-load-theme t )
;;   (defvar sml/shorten-directory -1) ;; directory pathはフルで表示されたいので

;;; これを入れないとsmart-mode-lineを読み込むたびに
;;; Loading a theme can run Lisp code.  Really load? (y or n)
;;; と聞いてくる。
;(setq sml/no-confirm-load-theme t)
;(sml/setup)
;(sml/apply-theme 'respectful)
;;; その他のthemeを設定
;(sml/apply-theme 'dark)
 
;;;;;;;;;;;;;;;;

(prefer-coding-system 'utf-8)
(setq default-process-coding-system 'utf-8)


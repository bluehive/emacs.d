;;jdufresne/emacs-init
;@jdufresne jdufresne Remove custom shit

;;; init.el --- Emacs initialization file -*- lexical-binding: t -*-

;; Author: Jon Dufresne <jon@jondufresne.org>

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Initialize Emacs the way I like it.

;;; Code:

;;
;; keep it simple !!
;;

					;use-packege ; init.el

;;; .emacs --- dot emacs file
;; This file is NOT part of GNU Emacs.gti

;;; Commentary:

;;;; Emacsおよび関連ソフトのインストール方法
;;;;; Ubuntu
;;    % sudo add-apt-repository ppa:cassou/emacs
;;    % sudo apt-get update
;;    % sudo apt-get install emacs-snapshot
;;    % emacs-snapshot

;;;;; Windows ;;;;;;;;;;;;;;;;;;;
;; Chocolateyを使った環境構築の時のメモ - Qiita
;; - http://qiita.com/konta220/items/95b40b4647a737cb51aa
;; まずはこれで一括してインストール可能である
;; emacs25 Msys2 ripgrep etc
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; NTEmacs64(64bit 版 version 25.1)
;; GitHub - chuntaro/NTEmacs64: Windows 版 Emacs (通称 NTEmacs) の 64bit 版
;; - https://github.com/chuntaro/NTEmacs64
;; 起動方法
;; emacs-25.1-IME-patched.zip を展開すると emacs-25.1/ フォルダが出来るので emacs-25.1/bin/runemacs.exe を実行します。
;; Emacsのパスを通す
;; Git BashはWindows側で通したパスにある実行ファイルも直接実行できます。
;; ですので、ここでEmacsの実行ファイルがあるディレクトリ
;; (最初は(解凍して出てきたディレクトリ)\bin)にパスを通します。
;; システム環境変数側の「Path」を選択し、「編集(I)」を押します。
;; 変数値のパスの後ろにEmacsの実行ファイルがあるパスを追記します。(前のパスとは「;」で区切る)
;; Unix系のパスは「:」で区切りますが、Windowsでは「;」で区切ることに注意してください。
;; 追記したらOKを押して、環境変数・システムのプロパティダイアログもOKで閉じます。
;; これでパスが通りました。
;; これ以降コマンドプロンプト・PowershellからEmacsを別ウインドウで起動したいときは、
;; と入力すると起動できます。(Unix系のemacs &に相当)
;; emacs -nwは文字コードの面から非推奨です。

;; 注意事項
;;     DDSKK はバージョン 16.1 以降を使用してください
;;     ダイナミックモジュールの機能は、一旦有効にしてビルドすると設定ファイルで無効にする事が出来ません
;;     load-path 上に DLL があるか気になる方は以下のコードを実行すると確認出来ます (あると警告が表示されます)
  ;; (dolist (dir load-path)
  ;;   (dolist (dll (directory-files dir t "\\.dll$"))
  ;;     (warn dll)))
;;
;; - MSYS2 homepage - http://www.msys2.org/
;; Cygwin は POSIX 互換を目指しているため機能盛りだくさんで、
;; 「Cygwin はちょっと重い、でかすぎる・・・」という方もいるかと思います。
;; そういう場合は MSYS2 を使うのが良いでしょう。MSYS2 は、プログラム開発に
;; 必要なだけの機能を取り入れた、いわば Cygwin のコンパクト版です。
;; と言っても、普通にWindows 上で使う分には充分な機能を持っています。
;; 初期化ファイル（.emacs や .emacs.d/init.el）に、下記の設定を追加する。
;; 下記の例は C:\Programs\ の下に MSYS2 をインストールした場合です。
;; このパスは、ご自身のインストールパスに合わせて変更してください。
;; ;; MSYS2 のコマンドを使えるようにする.
;; (setenv "PATH"
;; (concat
;; ;; 下記の行に MSYS2 のコマンドの実行可能ファイルがある場所を設定してください. スラッシュが2つ連続することに注意！
;; "C:\\Programs\\msys64\\usr\\bin;"
;; (getenv "PATH")))
 ;; Update the package database and core system packages with:
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


;; Emacs日本語info設定:
;;
;; 25.1のバグフィクス版です。
;; https://ayatakesi.github.io/emacs/25.1/emacs-ja.info
;; ダウンロードしてください。
;; たとえばwgetで ~/info/emacs251-ja.info という
;; ファイル名でダウンロードするならば
;; ================================================================
;; $ mkdir -p ~/info
;; $ wget -O ~/info/emacs251-ja.info https://ayatakesi.github.io/emacs/25.1/emacs-ja.info
;; ================================================================



;;; Code:
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;
;;;; 初期化 https://github.com/kawabata/dotfiles/blob/master/.emacs.d/init.el
;;;
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;; Emacsは init.el 読み込み後に各パッケージへのload-path設定を行い
;; XXX-autoloads.el を読み込む。このままでは init の段階では
;; require/locate-library ができないため、(package-initialize) を事前
;; に実行する。
;
;# (eval-when-compile (require 'cl))
;# (setq init-file-debug t)
;# (cd "~/") ; ホームディレクトリにcd
;# (setq force-load-messages t)
;# (message (format "Startup time: %s" (format-time-string "%Y/%m/%d %H:%M:%S")))
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
;; ローカルレポジトリを追加
;; (when (file-exists-p "~/.emacs.d/local-packages/archive-contents")
;;   (pushnew '("local" . "~/.emacs.d/local-packages/")
;;               package-archives :test 'equal))


;; ;;;;;;;;;;;;;;;;;;;;;; try
;; Magit を使ってみる
;; - Qiita - http://qiita.com/ignorant/items/86d353e3ada299f12836
;; インストール下準備init.el に下記の設定を追加して，
;; package.el と use-package を使用できるようにする．
;; try は，M-x try Enter package 名 で package をインストールして手軽に
;; 試せます．Emacs を再起動すると try から導入した package は使えなくなります．;; いらないパッケージが残らないので精神衛生上助かりますね．
;;ちなみに，TEMP フォルダに展開されるので，気になるなら消しましょう．私は，そこまでは気にならないので，自動削除される日までほっときます．

;; ;;; package.el
;; (require 'package)
;; (setq package-enable-at-startup nil)
;; (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))

;; ;;(package-refresh-contents)
;; (package-initialize)

;; ;;; Bootstrap 'use-package
;; (unless (package-installed-p 'use-package)
;; (package-refresh-contents)
;; (package-install 'use-package))
;; (use-package try
;; :ensure t)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;; 関数のコマンド化
;(defmacro command (&rest body)
;  `(lambda () (interactive) ,@body))

;;;; 大きすぎるファイルでは適用しないモードの判定
(defun tkw-large-file-p (&optional char-size)
  "現在のバッファの文字数が CHAR-SIZE を越えるか判定する.
デフォルト値は 100,000."
  (< (or char-size 100000) (point-max)))


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

;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
;;; @ language - coding system                                      ;;;
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;

;; ------------------------------------------------------------------------
;; @ character code (文字コード)
;; Setenv
(set-language-environment "Japanese")
(when (equal system-type 'windows-nt)
        (prefer-coding-system 'shift_jis)
        (set-default-coding-systems 'shift_jis)
        (setq file-name-coding-system 'shift_jis)
        (setq default-file-name-coding-system 'shift_jis)
        (setq locale-coding-system 'shift_jis))

(when (equal system-type 'ns)
         (require 'ucs-normalize)
         (prefer-coding-system 'utf-8-hfs)
         (setq file-name-coding-system 'utf-8-hfs)
         (setq locale-coding-system 'utf-8-hfs))


;; (set-language-environment "Japanese")
;; (let ((ws window-system))
;;   (cond ((eq ws 'w32)
;;          (prefer-coding-system 'shift_jis)
;;          (set-default-coding-systems 'shift_jis)
;;          (setq file-name-coding-system 'shift_jis)
;;          (setq default-file-name-coding-system 'shift_jis)
;;          (setq locale-coding-system 'shift_jis))
;;         ((eq ws 'ns)
;;          (require 'ucs-normalize)
;;          (prefer-coding-system 'utf-8-hfs)
;;          (setq file-name-coding-system 'utf-8-hfs)
;;          (setq locale-coding-system 'utf-8-hfs))))

;(setenv "LANG" "ja_JP.UTF-8")
;(set-language-environment "Japanese")
;(let ((ws window-system))
;  (cond ((eq ws 'w32)
;         (prefer-coding-system 'utf-8-unix)
;         (set-default-coding-systems 'utf-8-unix)
;         (setq file-name-coding-system 'sjis)
;         (setq default-file-name-coding-system 'shift_jis)
;         (setq locale-coding-system 'utf-8))
;        ((eq ws 'ns)
;         (require 'ucs-normalize)
;         (prefer-coding-system 'utf-8-hfs)
;         (setq file-name-coding-system 'utf-8-hfs)
;         (setq locale-coding-system 'utf-8-hfs))))

;; ;; ;;; @ screen - minibuffer                                           ;;;
;; ;; ;; minibufferのアクティブ時、IMEを無効化
;;  (add-hook 'minibuffer-setup-hook
;;            (lambda ()
;;              (deactivate-input-method)))
;;  (wrap-function-to-control-ime 'y-or-n-p nil nil)
;;  (wrap-function-to-control-ime 'map-y-or-n-p nil nil)
;;  (wrap-function-to-control-ime 'read-char nil nil)

;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
;;; @ anything                                                      ;;;
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;

(use-package anything :no-require t :defer t :ensure t)
(require 'anything-config)
(setq anything-enable-shortcuts 'prefix)
(define-key anything-map (kbd "@") 'anything-select-with-prefix-shortcut)
(global-set-key (kbd "C-x b") 'anything-mini)

;; ;少し複雑な設定例として
;; 1. 場所elinitは~/.emacs.d/のみ
;;    - ~/.emacs.d/以下でrgを動かす
;; 2. 場所melmagは~/memo/emacs-melmag/のみ
;;    - ~/memo/emacs-melmag/以下でrgを動かす
;; 3. 場所memoは~/sync/think/と~/memo/
;;    - andgrepスクリプトを2つのディレクトリで動かす

;================================================================
(setq anything-grep-alist
      '(("elinit"
         ("rg -n --colors match:fg:red --smart-case --no-heading -g '*.el' %s" "~/.emacs.d/"))
        ("drorg"
;         ("rg -n --colors match:fg:red --smart-case --no-heading -g '*.org' %s" "/c/Users/bluehive/Dropbox/org/")
	 )
        ("junk"
         ("ruby ~/.emacs.d/bin/andgrep --with-title %s" "~/junk/2017/")
         ("ruby ~/.emacs.d/bin/andgrep --with-title %s" "~/junk/2017/08/"))))
;================================================================
;設定が終わったら
;M-x anything-grep-by-nameを実行します。
;するとクエリと場所を聞かれるのでそれぞれ入力します。



;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
;;; @ windows                                                     ;;;
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;

;;;; setup-cygwin
;; Windows用のシンボリックリンクの設定など
(when (equal system-type 'windows-nt)
  (require 'setup-cygwin nil :no-error))

;; MSYS2 のコマンドを使えるようにする.
;[url=https://www.emacswiki.org/emacs/GrepMode#toc5][Home] Grep Mode[/url]

(when (equal system-type 'windows-nt)
  (setenv "PATH"
          (concat
   ;; 下記の行に MSYS2 のコマンドの実行可能ファイルがある場所を設定してください. スラッシュが2つ連続することに注意！
           "C:\\msys64\\usr\\bin;"
           "C:\\Users\\bluehive\\AppData\\Roaming\\.emacs.d\\elpa\\anything-20161127.2357;"
           (getenv "PATH")))
)

;;日本語grep対策　
(when (equal system-type 'windows-nt)
  (setq
   find-program "C:\\msys64\\usr\\bin\\find.exe"
   grep-program "C:\\msys64\\usr\\bin\\grep.exe")
)

;;;;ripgrep.el
;;;;[url=http://emacs.rubikitch.com/ripgrep/]ripgrep.el :
;;;【agより、ずっとはやい!!】超音速grepとEmacsインターフェース(Windows安心)[/url]
;;; rgバイナリの位置
;; (setq ripgrep-executable  "C:\\msys64\\usr\\bin\\rg")
;; ;;; rgに渡すオプション
;; (setq ripgrep-arguments '("-S"))

;; ;;; @ language - fontset                                            ;;;
;; ;; ;; デフォルト フォント
;; (set-face-attribute 'default nil :family "Migu 1M" :height 110)
;; ;; (set-face-font 'default "Migu 1M-11:antialias=standard")

;; ;; ;; プロポーショナル フォント
;; (set-face-attribute 'variable-pitch nil :family "Migu 1M" :height 110)
;; ;; (set-face-font 'variable-pitch "Migu 1M-11:antialias=standard")

;; ;; ;; 等幅フォント
;; (set-face-attribute 'fixed-pitch nil :family "Migu 1M" :height 110)
;; ;; (set-face-font 'fixed-pitch "Migu 1M-11:antialias=standard")

;; ;; ;; ツールチップ表示フォント
;; (set-face-attribute 'tooltip nil :family "Migu 1M" :height 90)
;; ;; (set-face-font 'tooltip "Migu 1M-9:antialias=standard")

;; ;;; fontset

;; ;; ;; フォントサイズ調整
;; ;; (global-set-key (kbd "C-<wheel-up>")   '(lambda() (interactive) (text-scale-increase 1)))
;; ;; (global-set-key (kbd "C-=")            '(lambda() (interactive) (text-scale-increase 1)))
;; ;; (global-set-key (kbd "C-<wheel-down>") '(lambda() (interactive) (text-scale-decrease 1)))
;; ;; (global-set-key (kbd "C--")            '(lambda() (interactive) (text-scale-decrease 1)))

;; ;; ;; フォントサイズ リセット
(global-set-key (kbd "M-0") '(lambda() (interactive) (text-scale-set 0)))


;; ;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
;; ;;;     IMEを有効にするには以下の設定が必要です                     ;;;
;; ;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;

(when (equal system-type 'windows-nt)
;(set-language-environment "UTF-8") ;; UTF-8 でも問題ないので適宜コメントアウトしてください
  (setq default-input-method "W32-IME")
  (setq-default w32-ime-mode-line-state-indicator "[--]")
(setq w32-ime-mode-line-state-indicator-list '("[--]" "[あ]" "[--]"))
  (w32-ime-initialize)
  ;; 日本語入力時にカーソルの色を変える設定 (色は適宜変えてください)
  (add-hook 'w32-ime-on-hook '(lambda () (set-cursor-color "coral4")))
  (add-hook 'w32-ime-off-hook '(lambda () (set-cursor-color "black")))

  ;; 以下はお好みで設定してください
  ;; 全てバッファ内で日本語入力中に特定のコマンドを実行した際の日本語入力無効化処理です
  ;; ミニバッファに移動した際は最初に日本語入力が無効な状態にする
  (add-hook 'minibuffer-setup-hook 'deactivate-input-method)

  ;; isearch に移行した際に日本語入力を無効にする
  (add-hook 'isearch-mode-hook '(lambda ()
                                  (deactivate-input-method)
                                  (setq w32-ime-composition-window (minibuffer-window))))
  (add-hook 'isearch-mode-end-hook '(lambda () (setq w32-ime-composition-window nil)))

;;   ;; helm 使用中に日本語入力を無効にする
;;   (advice-add 'helm :around '(lambda (orig-fun &rest args)
;;                                (let ((select-window-functions nil)
;;                                      (w32-ime-composition-window (minibuffer-window)))
;;                                  (deactivate-input-method)
;;                                  (apply orig-fun args))));; テーマ格納ディレクトリのパス追加
;; (add-to-list 'custom-theme-load-path
;;              (file-name-as-directory (concat user-emacs-directory "theme"))
;;              )
)

;; ;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
;; ;;; @ screen - エンコード                                           ;;;
;; ;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;

;; ;; cp932エンコードの表記変更
;; (coding-system-put 'cp932 :mnemonic ?P)
;; (coding-system-put 'cp932-dos :mnemonic ?P)
;; (coding-system-put 'cp932-unix :mnemonic ?P)
;; (coding-system-put 'cp932-mac :mnemonic ?P)

;; ;; UTF-8エンコードの表記変更
;; (coding-system-put 'utf-8 :mnemonic ?U)
;; (coding-system-put 'utf-8-with-signature :mnemonic ?u)

;; ;; 改行コードの表記追加
;; (setq eol-mnemonic-dos       ":Dos ")
;; (setq eol-mnemonic-mac       ":Mac ")
;; (setq eol-mnemonic-unix      ":Unx ")
;; (setq eol-mnemonic-undecided ":??? ")


;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
;;; @ screen - buffer                                               ;;;
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;

;; バッファ画面外文字の切り詰め表示（有効：t、無効：nil）
(setq truncate-lines nil)

;; ウィンドウ縦分割時のバッファ画面外文字の切り詰め表示（有効：t、無効：nil）
(setq truncate-partial-width-windows t)

;; 同一バッファ名にディレクトリ付与
(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)
(setq uniquify-buffer-name-style 'post-forward-angle-brackets)
(setq uniquify-ignore-buffers-re "*[^*]+*")


;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
;;; @ アクティベーション                                            ;;;
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;

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
(setq recentf-max-menu-items 10)
(setq recentf-max-saved-items 10)


;; コマンド（org-store-link, org-capture, org-agenda, org-iswitchb）、グローバルキーを割り当

(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-cc" 'org-capture)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cb" 'org-iswitchb)


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-enabled-themes (quote (manoj-dark)))
 '(org-agenda-files (quote ("~/todo.org")))
 '(org-capture-templates
   (quote
    (("t" "New TODO" entry
      (file+headline "~/todo.org" "予定")
      "* TODO %?

")
     ("m" "Memo" entry
      (file+headline "~/memo.org" "メモ")
      "* %U%? memo
%i
%a"))) t)
 '(package-selected-packages
   (quote
    (auto-async-byte-compile paredit lispxmp open-junk-file ripgrep rg ht yasnippet ## recentf-ext anything))))
;; custom-set-variables was added by Custom.
;; If you edit it by hand, you could mess it up, so be careful.
;; Your init file should contain only one such instance.
;; If there is more than one, they won't work right.
'(desktop-save-mode t)
'(org-agenda-files
   (quote
    ("~/Dropbox/org/checklog2016-3.org" "~/Dropbox/org/Dtodo.org" "~/todo.org")))
'(org-babel-load-languages (quote ((awk . t) (emacs-lisp . t) (perl . t))))
;;  '(package-selected-packages (quote (##)))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;;  ;; org-captureで2種類のメモを扱うようにする
;; (setq org-capture-templates
;;       '(("t" "New TODO" entry
;;          (file+headline "~/org/todo.org" "予定")
;;          "* TODO %?\n\n")
;;         ("m" "Memo" entry
;;          (file+headline "~/org/memo.org" "メモ")
;;          "* %U%?\n%i\n%a")))
;; ;; org-agendaでaを押したら予定表とTODOリストを表示
;; (setq org-agenda-custom-commands
;;       '(("a" "Agenda and TODO"
;;          ((agenda "")
;;           (alltodo "")))))
;; ;; org-agendaで扱うファイルは複数可だが、
;; ;; TODO・予定用のファイルのみ指定
;; (setq org-agenda-files '("~/org/todo.org"))
;; ;; TODOリストに日付つきTODOを表示しない
;; ;; (setq org-agenda-todo-ignore-with-date t)
;; ;; 今日から予定を表示させる
;; (setq org-agenda-start-on-weekday nil)

;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
;;; @ rubikichi                                                     ;;;
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;

;;;== List2:C-x C-bを置き換える
;;(global-set-key (kbd "C-x C-b") 'bs-show)

;;;== List5:recentfの設定
;; 最近のファイル500個を保存する
(setq recentf-max-saved-items 500)
;; 最近使ったファイルに加えないファイルを
;; 正規表現で指定する
(setq recentf-exclude
      '("/TAGS$" "/var/tmp/"))
;;(require 'recentf-ext)

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

;; 式の評価結果を注釈するための設定
(require 'lispxmp)
;; emacs-lisp-mode でC-c C-dを押すと注釈される
(define-key emacs-lisp-mode-map (kbd "C-c C-d") 'lispxmp)

;; 括弧の対応を保持して編集する設定
(require 'paredit)
(add-hook 'emacs-lisp-mode-hook 'enable-paredit-mode)
(add-hook 'lisp-interaction-mode-hook 'enable-paredit-mode)
(add-hook 'lisp-mode-hook 'enable-paredit-mode)
(add-hook 'ielm-mode-hook 'enable-paredit-mode)

;; ;; 自動バイトコンパイルを無効にするファイル名の正規表現
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

;; ;;;日本語info設定
;; ;;; ~/info/以下をinfoファイル検索ディレクトリに加える
;; (add-to-list 'Info-directory-list "~/info/")
;; ;;; emacs→emacs251-jaにリダイレクトする
;; (defun Info-find-node--info-ja (orig-fn filename &rest args)
;;   (apply orig-fn
;;          (pcase filename
;;            ("emacs" "emacs251-ja")
;;            (t filename))
;;          args))
;; (advice-add 'Info-find-node :around 'Info-find-node--info-ja)



;; org-mode 仕事術用設定
(require 'org)

;; (unless (member "CLOCK" org-special-properties)
;;   (defun org-get-CLOCK-property (&optional pom)
;;   (org-with-wide-buffer
;;    (org-with-point-at pom
;;      (when (and (derived-mode-p 'org-mode)
;;                 (ignore-errors (org-back-to-heading t))
;;                 (search-forward org-clock-string
;;                                 (save-excursion (outline-next-heading) (point))
;;                                 t))
;;        (skip-chars-forward " ")
;;        (cons "CLOCK"  (buffer-substring-no-properties (point) (point-at-eol)))))))
;;   (defadvice org-entry-properties (after with-CLOCK activate)
;;     "special-propertyにCLOCKを復活させorg習慣仕事術を最新版orgで動かす"
;;     (let ((it (org-get-CLOCK-property (ad-get-arg 0))))
;;       (setq ad-return-value
;;             (if it
;;                 (cons it ad-return-value)
;;               ad-return-value)))))





;;;;


;; ;;; Bootstrap 'use-package
;; (unless (package-installed-p 'use-package)
;;   (package-refresh-contents)
;;   (package-install 'use-package))

;; (use-package try
;;   :ensure t)



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

;; (cperl-set-style "PerlStyle")
;; (custom-set-variables
;;  '(cperl-indent-parens-as-block t)
;;  '(cperl-close-paren-offset -4)
;;  '(cperl-indent-subs-specially nil))

(add-hook 'cperl-mode-hook
       (lambda ()
         (local-set-key (kbd "C-h f") 'cperl-perldoc)))


;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
;;; @ 非標準ライブラリ                                          ;;;
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;

;;;; s
;(use-package s :no-require t :defer t :ensure t)

;;;; f
;(use-package f :no-require t :defer t :ensure t)

;;;; ht
;(use-package ht :no-require t :defer t :ensure t)

;;;; yasnippet
;; TODO: 重要 yasnippet 読み込み時にエラーが出たら、とりあえず
;; (set-variable 'clojure-snippets-dir nil) を実行してみること。
;;
;; official doc: https://capitaomorte.github.io/yasnippet
;;   http://yasnippet-doc-jp.googlecode.com/svn/trunk/doc-jp/snippet-expansion.html
;; - snippets を使うときは、M-x yas-minor-mode
;;   + キーワードを入力して、<tab>キーを押す。
;;   + キーワード一覧が分からなくなったときはメニューで確認。
;; - snippets を編集したら、 M-x yas-reload-all でリロード。
;; - snippets の呼び出しは、 M-x yas-insert-snippet (C-c & C-s)
;; - snippets の展開は、M-x yas-expand (<tab>)

;; 日本語文章の入力においては、空白で区切ってキーワードを入力することができない。
;; そのため、snippetは、bindingのショートカットキーで呼び出す。
;; - helm との連携 ::  <先頭文字をタイプ>,  M-x helm-c-yasnippet (M-X y)
;; clojure-snippet で出るエラーについて
;; これらは、yas-minor-mode を実行すると、yasnippet がロードされ、その
;; 結果、eval-after-load で、そのバッファからsnippetを読み込もうとして
;; エラーになる。なぜ何度も yasnippet がロードされようとするのかは不明。
;; (yas-global-mode)
(use-package yasnippet :no-require t :ensure t
  :commands snippet-mode
;;  :config
;;  ;; 他スニペットのダウンロード (~/.emacs.d/snippets-3rd-party/)
;;  (dolist (snip-dir (directory-files
;;                     (locate-user-emacs-file "snippets-3rd-party") t "^[^.]"))
;;    (when (file-directory-p snip-dir)
;;      (add-to-list 'yas-snippet-dirs snip-dir t)
;;      ;;(yas-load-directory snip-dir)
  ;;      )))
)
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
;;; @ magit                                                         ;;;
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
;;;; magit
;;;;; Unicodeファイル名問題
;; % git config --global core.precomposeunicode true
;;;;; 1.4.0 の仕様変更
;; https://raw.githubusercontent.com/magit/magit/next/Documentation/RelNotes/1.4.0.txt
;;;;; ドキュメント ([[info:magit#Top]] 参照)
;; http://matome.naver.jp/odai/2136491451473222801 が一番良いまとめ
;; - ワークツリー <-(checkout) ステージングエリア <-(reset) Gitレポジトリ
;; - HEAD :: Gitのレポジトリが見ている最新のcommit位置。
;; - original :: githubのリモートの典型的なレポジトリ名
;; - master :: デフォルトのブランチ名
;; M-x magit-status (.git がなければ git init をすることが可能)
;; C-u で、ファイル名入力などが可能。
;; | コマンド | gitコマンド・その他             |                                                                               |
;; |----------+---------------------------------+-------------------------------------------------------------------------------|
;; | -status  | git init, git status            |                                                                               |
;; |----------+---------------------------------+-------------------------------------------------------------------------------|
;; | 1,2,3,4  | 表示レベル                      |                                                                               |
;; | M-1,2,.. | 全ファイルで対象                |                                                                               |
;; | M-H      | 全て隠す                        |                                                                               |
;; | TAB      | ファイルのdiff表示              |                                                                               |
;; | S-TAB    | diffレベルの切り替え            |                                                                               |
;; |----------+---------------------------------+-------------------------------------------------------------------------------|
;; | s        | git add <file>                  | ステージング                                                                  |
;; | C-u S    | git add .                       | 全ステージの登録                                                              |
;; | u        | git -- reset <file>             | アンステージング                                                              |
;; | i        | .gitignore へ追加（無視）       |                                                                               |
;; | C-- i    | .gitignore へワイルドカード追加 |                                                                               |
;; | I        | .git/info/exclude へ追加        |                                                                               |
;; | k        | rm / git rm <file>              |                                                                               |
;; |          | git mv <file>                   |                                                                               |
;; | c        | git commit <file>               |                                                                               |
;; | C        | git commit <file> / changelog   |                                                                               |
;; |----------+---------------------------------+-------------------------------------------------------------------------------|
;; | l        | git log                         | l (short) L (Long) f (File log) rl (ranged short)  rL (long)                  |
;; |          | Reflogs                         | h (Head Reflog)                                                               |
;; |----------+---------------------------------+-------------------------------------------------------------------------------|
;; | v        | Show Commit                     |                                                                               |
;; |----------+---------------------------------+-------------------------------------------------------------------------------|
;; | d        | git diff <rev>                  | git diff HEAD                                                                 |
;; | D        | git diff ???                    | s (set) d (set default) c (save default) r (reset to default) h (toggle hunk) |
;; |----------+---------------------------------+-------------------------------------------------------------------------------|
;; | t        | Tagging                         | t (lightweight)  a (annotation)                                               |
;; |----------+---------------------------------+-------------------------------------------------------------------------------|
;; | X        | git reset --hard HEAD <file>    | ステージ前の状態に戻す。                                                      |
;; | x        | git reset --soft HEAD <file>    |                                                                               |
;; |          | git revert <commit>             |                                                                               |
;; |----------+---------------------------------+-------------------------------------------------------------------------------|
;; | z        | Stashing                        | z (save) s (snapshot)                                                         |
;; |----------+---------------------------------+-------------------------------------------------------------------------------|
;; | P P      | git push <remote> <refspec>     | git push origin master                                                        |
;; | P t/T    | git push <remote> tag(s)        |                                                                               |
;; |----------+---------------------------------+-------------------------------------------------------------------------------|
;; | f f      | git fetch <remote> <refspec>    | f (current) a (all) o (other)                                                 |
;; | F F      | git pull <remote> <refspec>     | git pull origin master -r (--rebase)                                          |
;; |----------+---------------------------------+-------------------------------------------------------------------------------|
;; | b v      | git branch -a                   |                                                                               |
;; | b c      | git branch <branch>             | ブランチを作成                                                                |
;; | b b      | git checkout <branch>           | ブランチを切り替え。(HEADを<branch>へ移動)                                    |
;; |          | git checkout -b <new> <old>     | ブランチの作成＋切り替え                                                      |
;; | b r      |                                 | ブランチ名を変更                                                              |
;; | b k      |                                 | ブランチの削除                                                                |
;; |----------+---------------------------------+-------------------------------------------------------------------------------|
;; | m m      | git merge <branch>              | -ff (fast-forward) -nf (non fast-forward)                                     |
;; |          | git rebase <branch>             |                                                                               |
;; |----------+---------------------------------+-------------------------------------------------------------------------------|
;; | M a      | git remote add <name> <url>     |                                                                               |
;; | y        | git cherry-pick                 | 狙ったコミットの変更内容だけを現在のブランチに取り込む操作                    |
;; |----------+---------------------------------+-------------------------------------------------------------------------------|
;; | B        | Bisecting                       | b (bad) g (good) k (skip) r (reset) s (start) r (reset) u (run)               |
;; | c        | Committing                      | c (commit) a (amend) e (extend) r (reword) f (fixup) s (squash)               |
;; | g        | Refresh Buffers                 |                                                                               |
;; | o        | Submoduling                     |                                                                               |
;; | r        | Rewriting                       |                                                                               |
;; | s        | Show Status                     |                                                                               |
;; | S        | Stage all                       |                                                                               |
;; | U        | Unstage all                     |                                                                               |
;; | V        | Show File                       |                                                                               |
;; | w        | Wazzup                          |                                                                               |
;; | !        | Running                         |                                                                               |
;; | $        | Show Process                    |                                                                               |

;; - git remote add origin https://github.com/kawabata/hoge.git ..
;;  "origin" という名前で "http://.../" をアップストリームリポジトリに
;;   追加
;; - git push -u origin master
;;;;; その他
;; - タグの追加とPush
;;   + t <tag_name>
;;   + P t <tag_name>
(use-package magit :no-require t :ensure t
  :bind (("M-g s" . magit-status)
         ("M-g b" . magit-blame-mode))
  :config
  (set-variable 'magit-process-find-password-functions
                '(magit-process-password-auth-source))
  )

(use-package magit-gh-pulls
  :no-require t
  :ensure t
  :config
  (add-hook 'magit-mode-hook 'turn-on-magit-gh-pulls)
 )


(use-package browse-at-remote
  :no-require t
  :ensure t
  :bind (("C-c g g" . browse-at-remote))
 )


;;;; magithub (obsolete)
;; magit の仕様変更に追随できない場合が多いので使用停止。
;;(lazyload () "magit-key-mode"
;;  (set-variable 'magit-log-edit-confirm-cancellation nil))

;;;; malyon
;; Z-Machine Interpreter for Emacs
;; M-x malyon で Z仮想機械ファイルを指定してゲーム開始。
;;(use-package malyon :no-require t :defer t)

;;;; mark-multiple (obsolete)
;; → multipel-cursors に名称変更。

;;;; mdfind-dired
;; https://gist.github.com/Kouzuka/900452
;; TODO check

;;;; melpa (obsolete)
;; パッケージ管理システム。ブラックリストパッケージの管理等。
;;(lazyload () "melpa"
;;  (add-to-list 'package-archive-exclude-alist '(("melpa" bbdb-vcard))))
;;(require 'melpa nil :no-error)

;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
;;; @                                                          ;;;
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;

;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
;;; @                                                          ;;;
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;

;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
;;; @                                                          ;;;
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;




;;; (provide 'init)
;;; init.el ends here

;; Local Variables:
;; coding: utf-8
;; mode: emacs-lisp
;; End:



;; Basic config

;;   (require 'color-theme)
;;   (setq my-color-themes (list 'color-theme-billw 'color-theme-jsc-dark
;;                               'color-theme-sitaramv-solaris 'color-theme-resolve
;;                               'color-theme-classic 'color-theme-jonadabian-slate
;;                               'color-theme-kingsajz 'color-theme-shaman
;;                               'color-theme-subtle-blue 'color-theme-snowish
;;                               'color-theme-sitaramv-nt 'color-theme-wheat))
;; ;;;;


(require 'sort)
(setq sort-fold-case t)

(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)

;; Default dictionary
(require 'ispell)
(setq ispell-dictionary "english")

;; Enable functions
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)

;; Global minor modes
(setq column-number-mode t)
(show-paren-mode 1)
(delete-selection-mode 1)
(global-linum-mode 1)
(global-subword-mode 1)
(setq comment-auto-fill-only-comments t)
(add-hook 'text-mode-hook #'turn-on-flyspell)
(add-hook 'prog-mode-hook #'flyspell-prog-mode)

;; Auto revert mode
(require 'autorevert)
(setq global-auto-revert-non-file-buffers t)
(setq auto-revert-verbose nil)
(global-auto-revert-mode 1)

;; Save place mode
(require 'saveplace)
(setq-default save-place t)
(savehist-mode 1)

;; Buffer clean up
(prefer-coding-system 'utf-8)
(require 'whitespace)
(defun cleanup-buffer ()
  "Set the preferred style upon save."
  (set-buffer-file-coding-system 'utf-8)
  (let ((whitespace-style '(empty trailing)))
    (whitespace-cleanup)))
(add-hook 'before-save-hook #'cleanup-buffer)

;; Fix ibuffer to use ido-find-file
(require 'ibuffer)
(define-key ibuffer-mode-map (kbd "C-x C-f") #'ido-find-file)
;; Always use ibuffer
(global-set-key [remap list-buffers] #'ibuffer)

(require 'nxml-mode)
(setq nxml-child-indent 4)

;; Remove annoying keys
;(global-unset-key (kbd "<insert>"))
;(global-unset-key (kbd "C-z"))
;(global-unset-key (kbd "C-x C-z"))

;; Always kill the current buffer without asking
(defun kill-buffer-now (&optional buffer-or-name)
  "Kill the buffer specified by BUFFER-OR-NAME without asking."
  (interactive)
  (let ((kill-buffer-query-functions nil))
    (kill-buffer buffer-or-name)))
(global-set-key (kbd "C-x k") #'kill-buffer-now)
(global-set-key (kbd "C-x C-k") #'kill-buffer-now)

;; ;; Python
;; (require 'python)
;; (setq python-shell-interpreter "python3")
;; (global-set-key (kbd "<f9>") #'run-python)

;; ;; SQL
;; (require 'sql)

;; (defun project-config ()
;;   "Read and return JSON project config."
;;   (json-read-file (projectile-expand-root "erezlife/config.json")))

;; (defun database ()
;;   "Return the name of the database for the current project."
;;   (ignore-errors
;;     (let* ((config (project-config))
;;            (database-config (cdr (assoc 'database config)))
;;            (database (cdr (assoc 'name database-config))))
;;       database)))

;; (defun project-sql (product)
;;   "Run PRODUCT database with default database for current project."
;;   (let ((default-directory (expand-file-name "~"))
;;         (sql-database (database)))
;;     (sql-product-interactive product)))

;; (defun project-sql-postgres ()
;;   "Run PostgreSQL with default database for current project."
;;   (interactive)
;;   (project-sql 'postgres))
;; (global-set-key (kbd "<f12>") #'project-sql-postgres)

;; (defun init-sqli-mode ()
;;   "Initialize SQLi-MODE.
;; Turn off LINUM-MODE, as the buffer can be extremely large."
;;   (linum-mode 0))
;; (add-hook 'sql-interactive-mode-hook #'init-sqli-mode)

;; (defun init-sql-mode ()
;;   "Initialize SQL-MODE."
;;   (setq sql-buffer (get-buffer "*SQL*")))
;; (add-hook 'sql-mode-hook #'init-sql-mode)

;; (defun unfill-paragraph ()
;;   "Unfill paragraph at or after point."
;;   (interactive)
;;   (let ((fill-column (point-max)))
;;     (fill-paragraph nil)))

;; (defun unfill-region ()
;;   "Unfill each of the paragraphs in the region."
;;   (interactive)
;;   (let ((fill-column (point-max)))
;;     (fill-region (region-beginning) (region-end) nil)))

;; (global-set-key (kbd "M-Q") #'unfill-paragraph)
;; (global-set-key (kbd "C-M-Q") #'unfill-region)

;; (defun insert-file-name ()
;;   "Insert the buffer's file name sans final extension at point."
;;   (interactive)
;;   (when (buffer-file-name)
;;     (insert (file-name-base (buffer-file-name)))))

;; ;; Speed up large files such as SQL backups
;; (defun init-large-buffer ()
;;   "Setup large buffers to better handle large buffers."
;;   (when (> (buffer-size) large-file-warning-threshold)
;;     (setq buffer-read-only t)
;;     (buffer-disable-undo)
;;     (linum-mode 0)))
;; (add-hook 'find-file-hook #'init-large-buffer)

;; (defvar kill-all-global-buffers
;;   '("*compilation*"))

;; (defun kill-all-buffers ()
;;   "Kill all buffers except global buffers."
;;   (interactive)
;;   (dolist (buffer (buffer-list))
;;     (unless (and (string-match "^\\*.*\\*$" (buffer-name buffer))
;;                  (not (member (buffer-name buffer) kill-all-global-buffers)))
;;       (kill-buffer buffer)))
;;   (grep-a-lot-clear-stack))

;; Third party libraries.
;; (require 'package)
;; (add-to-list 'package-archives
;;              '("melpa" . "https://melpa.org/packages/"))
;; (package-initialize)

;; (unless (package-installed-p 'use-package)
;;   (package-refresh-contents)
;;   (package-install 'use-package))

;; (require 'use-package)
;; (setq use-package-always-ensure t
;;       use-package-verbose t)

;; (use-package apache-mode
;;   :mode ("\\.conf\\'" . apache-mode))

;; (use-package crontab-mode
;;   :mode (("\\.cron\\(tab\\)?\\'" . crontab-mode)
;;          ("cron\\(tab\\)?\\." . crontab-mode)))

(use-package diff-hl
  :config (global-diff-hl-mode 1))

(use-package flx-ido
  :init (setq ido-auto-merge-work-directories-length -1
              ido-create-new-buffer 'never
              ido-enable-flex-matching t
              ido-enable-last-directory-history t
              ido-use-faces nil)
  :config (progn
            (ido-mode 1)
            (ido-everywhere 1)
            (flx-ido-mode 1)))

(use-package flycheck
  :init (progn
          (setq flycheck-highlighting-mode 'lines
                flycheck-display-errors-function nil)
          (setq-default flycheck-javascript-jshint-executable (expand-file-name "~/node_modules/.bin/jshint")
                        flycheck-javascript-eslint-executable (expand-file-name "~/node_modules/.bin/eslint")
                        flycheck-json-jsonlint-executable (expand-file-name "~/node_modules/.bin/jsonlint")
                        flycheck-python-flake8-executable (expand-file-name "~/venv/bin/flake8")
                        flycheck-disabled-checkers '(php-phpmd php-phpcs)))
  :config (global-flycheck-mode 1))

;; (defun init-git-commit-mode ()
;;   "Initialize GIT-COMMIT-MODE."
;;   (setq fill-column 72))

;; (use-package git-commit
;;   :config (progn
;;             (global-git-commit-mode 1)
;;             (add-hook 'git-commit-mode-hook #'init-git-commit-mode)))

;; (use-package grep-a-lot
;;   :config (grep-a-lot-setup-keys))

;;(use-package groovy-mode)

;;(use-package less-css-mode)


;(use-package pip-requirements)

;(use-package pony-mode)

;; (use-package projectile
;;   :config (progn
;;             (add-to-list 'projectile-globally-ignored-directories "_build")
;;             (add-to-list 'projectile-globally-ignored-directories "bower_components")
;;             (add-to-list 'projectile-globally-ignored-directories "legacy/vendor")
;;             (add-to-list 'projectile-globally-ignored-directories "vendor")
;;             (add-to-list 'projectile-globally-ignored-directories "node_modules")
;;             (add-to-list 'projectile-globally-ignored-directories "venv")
;;             (add-to-list 'projectile-globally-ignored-file-suffixes ".d")
;;             (add-to-list 'projectile-globally-ignored-file-suffixes ".map")
;;             (add-to-list 'projectile-globally-ignored-file-suffixes ".min.css")
;;             (add-to-list 'projectile-globally-ignored-file-suffixes ".min.js")
;;             (add-to-list 'projectile-globally-ignored-file-suffixes ".svg")
;;             (add-to-list 'projectile-globally-ignored-files "ansible.log")
;;             (add-to-list 'projectile-globally-ignored-files "urlconf.php")
;;             (projectile-mode 1)))

;; (use-package s)

;; (use-package systemd)

(use-package undo-tree
  :config (global-undo-tree-mode 1))

(use-package yaml-mode)



;; Additional extensions.
;;(require 'myproject)

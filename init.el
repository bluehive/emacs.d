;;; init.el --- My init.el  -*- lexical-binding: t; -*-

;; Copyright (C) bluehive@github.
;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:
;; My init.el.
;; byte compile ;;;;;;;;;;;;;; emacs --batch -f batch-byte-compile init.el
;;; Code:

;; this enables this running method
;;   emacs -q -l ~/.debug.emacs.d/init.el

;; GCの設定
;; 起動にも影響するのでleaf無しで最初にやります
;; https://github.com/ncaq/.emacs.d/blob/master/init.el
(setq gc-cons-threshold 200000000)            ; 200MB
(run-with-idle-timer 120 t #'garbage-collect) ; 2分のアイドル時間ごとに明示的にガベージコレクトを呼び出す

(eval-and-compile
  (when (or load-file-name byte-compile-current-file)
    (setq user-emacs-directory
	  (expand-file-name
	   (file-name-directory (or load-file-name byte-compile-current-file))))))

(customize-set-variable
 'package-archives '(("gnu"   . "https://elpa.gnu.org/packages/")
		     ;;      ("melpa" . "http://melpa.milkbox.net/packages/")
		     ("melpa" . "https://melpa.org/packages/")
		     ;;     ("melpa-stable" . "https://stable.melpa.org/packages/")
		     ("org"   . "https://orgmode.org/elpa/")))
(package-initialize)
(unless (package-installed-p 'leaf)
  (package-refresh-contents)
  (package-install 'leaf))

;;

(leaf leaf-keywords
      :ensure t
      :init
      ;; optional packages if you want to use :hydra, :el-get, :blackout,,,
      (leaf hydra :ensure t)
      (leaf el-get :ensure t)
      (leaf blackout :ensure t)

      :config
      ;; initialize leaf-keywords.el
      (leaf-keywords-init))


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
  (setq prefer-coding-system 'utf-8)
  (setq locale-coding-system nil)
  (setq default-process-coding-system '(utf-8 . cp932)) ;agで日本語検索させるためのおまじない

  (setq set-file-name-coding-system 'cp932)
  (setq set-keyboard-coding-system 'cp932)
  (setq set-terminal-coding-system 'cp932)
  )
(require 'cl-lib)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Encoding
;; UTF-8 as the default coding system

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(when (fboundp 'set-charset-priority)
  (set-charset-priority 'unicode))

;; Explicitly set the prefered coding systems to avoid annoying prompt
;; from emacs (especially on Microsoft Windows)
(prefer-coding-system 'utf-8)
(setq locale-coding-system 'utf-8)

(set-language-environment 'utf-8)
(set-default-coding-systems 'utf-8)
(set-buffer-file-coding-system 'utf-8)
(set-clipboard-coding-system 'utf-8)
(set-file-name-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(modify-coding-system-alist 'process "*" 'utf-8)

;; Environment
;;(when (or sys/mac-x-p sys/linux-x-p)
;;  (use-package exec-path-from-shell
;;    :init
;;    (setq exec-path-from-shell-check-startup-files nil
;;          exec-path-from-shell-variables '("PATH" "MANPATH")
;;         exec-path-from-shell-arguments '("-l"))
;;  (exec-path-from-shell-initialize)))


;; 環境を日本語、UTF-8にする
					;(set-locale-environment nil)
(setq buffer-file-coding-system 'utf-8)
					;font
					;(set-fontset-font t 'japanese-jisx0208 "IPAPGothic-11")

;;
;;;;;;;;;;;;;;;;;;;;;;
;; Colors and Fonts ;;
;; https://github.com/cdepillabout/docs/blob/53086c3cd34db01d001997e24d79ad9e0ec4cc8e/dot_files/dot_emacs
;;;;;;;;;;;;;;;;;;;;;;

(load-theme 'manoj-dark)

;; Set the default font.
(set-face-attribute 'default nil
		    :family "Source Code Pro"
		    :height 140
		    :weight 'normal
		    :width 'normal)

;; Set the cursor color to red to match Vim in the terminal.
(set-cursor-color "red")

;; Set the EOL whitespace to be colored in white.
(set-face-attribute 'trailing-whitespace nil
		    :background "white")

;; Set the default font for Japanese characters.
(set-fontset-font t 'japanese-jisx0208 (font-spec :family "IPAPGothic"))

;; Style the tab-bar so it looks like my Vim tab-bar.
;; (set-face-attribute 'tab-bar nil
;;   :background "white"
;;   :foreground "black")
;; (set-face-attribute 'tab-bar-tab nil
;;   :background "deep sky blue"
;;   :foreground "white"
;;   :box 'nil
;;   :weight 'bold)
;; (set-face-attribute 'tab-bar-tab-inactive nil
;;   ;; :background "deep sky blue"
;;   :foreground "black"
;;   :box 'nil
;;   :weight 'normal
;;   )

(with-eval-after-load "org"
  (if (display-graphic-p)

      ;; faces to set if we are in the GUI
      (progn
	(set-face-attribute 'org-level-2 nil :foreground "dark goldenrod" :weight 'bold)
	(set-face-attribute 'org-level-3 nil :foreground "firebrick" :weight 'bold)
	(set-face-attribute 'org-special-keyword nil :foreground "light gray" :weight 'light)
	(set-face-attribute 'org-date nil :foreground "dark magenta" :underline nil :weight 'normal)
	(set-face-attribute 'org-tag nil :foreground "cornflower blue" :weight 'light)
	)

    ;; faces to set if we are in the CUI
    (set-face-attribute 'org-level-2 nil :foreground "color-116" :weight 'bold)
    (set-face-attribute 'org-level-3 nil :foreground "color-41" :weight 'bold)
    (set-face-attribute 'org-level-4 nil :weight 'bold)
    (set-face-attribute 'org-level-5 nil :weight 'bold)
    (set-face-attribute 'org-special-keyword nil :foreground "color-95" :weight 'light)
    )
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ここにいっぱい設定を書く
;; https://emacs-jp.github.io/tips/emacs-in-2020
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;leafの :custom で設定するとinit.elにcustomが勝手に設定を追記します。 この状況になると、変数の二重管理になってしまうので、customがinit.elに追記しないように設定します。
(leaf cus-edit
      :doc "tools for customizing Emacs and Lisp packages"
      :tag "builtin" "faces" "help"
      :custom `((custom-file . ,(locate-user-emacs-file "custom.el"))))

;;cus-start.c EmacsのC言語部分で定義されている変数をcustomで扱えるようにまとめているファイルです。 私の設定を書いておくので、取捨選択して頂ければと思います。変数の説明は F1 v で確認できます。

(leaf cus-start
      :doc "define customization properties of builtins"
      :tag "builtin" "internal"
      :preface
      (defun c/redraw-frame nil
	(interactive)
	(redraw-frame))

      :bind (("M-ESC ESC" . c/redraw-frame))
      :custom '(;;(user-full-name . "Naoya Yamashita")
		;;(user-mail-address . "conao3@gmail.com")
		;;(user-login-name . "conao3")
		(create-lockfiles . nil)
		(debug-on-error . t)
		(init-file-debug . t)
		(frame-resize-pixelwise . t)
		(enable-recursive-minibuffers . t)
		(history-length . 1000)
		(history-delete-duplicates . t)
		(scroll-preserve-screen-position . t)
		(scroll-conservatively . 100)
		(mouse-wheel-scroll-amount . '(1 ((control) . 5)))
		(ring-bell-function . 'ignore)
		(text-quoting-style . 'straight)
		(truncate-lines . t)
		;; (use-dialog-box . nil)
		;; (use-file-dialog . nil)
		(menu-bar-mode . t)
		;; (tool-bar-mode . nil)
		(scroll-bar-mode . nil)
		(indent-tabs-mode . nil))
      :config
      (defalias 'yes-or-no-p 'y-or-n-p)
      (keyboard-translate ?\C-h ?\C-?))

;;
(eval-and-compile
  (leaf bytecomp
	:doc "compilation of Lisp code into byte code"
	:tag "builtin" "lisp"
	:custom (byte-compile-warnings . '(cl-functions))))


;;Emacsの外でファイルが書き変わったときに自動的に読み直すマイナーモードです。 もちろん、Emacsで編集している場合は外の変更で上書きされることはありません。
(leaf autorevert
      :doc "revert buffers when files on disk change"
      :tag "builtin"
      :custom ((auto-revert-interval . 0.3)
	       (auto-revert-check-vc-info . t))
      :global-minor-mode global-auto-revert-mode)


;;delsel
;;選択している状態で入力したときに、regionを削除して挿入するマイナーモードです。 おそらくこの挙動のほうが現代人の意図に合っていると思います。

(leaf delsel
      :doc "delete selection if you insert"
      :tag "builtin"
      :global-minor-mode delete-selection-mode)


;;paren
;;対応するカッコを強調表示するマイナーモードです。

(leaf paren
      :doc "highlight matching paren"
      :tag "builtin"
      :custom ((show-paren-delay . 0.1))
      :global-minor-mode show-paren-mode)


;;simple
;;kill-ringの数を制御したり、kill-lineの挙動を変更したりします。

(leaf simple
      :doc "basic editing commands for Emacs"
      :tag "builtin" "internal"
      :custom ((kill-ring-max . 100)
	       (kill-read-only-ok . t)
	       (kill-whole-line . t)
	       (eval-expression-print-length . nil)
	       (eval-expression-print-level . nil)))

;;files
;;Emacsで好みが分かれる設定として、バックアップファイルを開いているファイルと同じディレクトリに作成するという挙動があります。 実際、このバックアップファイルに助けられることもあるので、 .emacs.d 以下にディレクトリを掘って、そこに保存するようにします。

(leaf files
      :doc "file input and output commands for Emacs"
      :tag "builtin"
      :custom `((auto-save-timeout . 15)
		(auto-save-interval . 60)
		(auto-save-file-name-transforms . '((".*" ,(locate-user-emacs-file "backup/") t)))
		(backup-directory-alist . '((".*" . ,(locate-user-emacs-file "backup"))
					    (,tramp-file-name-regexp . nil)))
		(version-control . t)
		(delete-old-versions . t)))


;;startup
;;自動保存されたファイルのリストです。 .emacs.d/backup 以下にまとめて保存するようにします。

(leaf startup
      :doc "process Emacs shell arguments"
      :tag "builtin" "internal"
      :custom `((auto-save-list-file-prefix . ,(locate-user-emacs-file "backup/.saves-"))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; add package
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

					;2.2 utility https://github.com/takeokunn/.emacs.d/blob/master/index.org

(leaf dash :ensure t)
(leaf dash-functional :ensure t)
(leaf s :ensure t)
(leaf f :ensure t)
(leaf ht :ensure t)
(leaf general :ensure t)
;;(leaf org-mode :ensure t)

					;6.14 File;;6.14.1 recentf

(leaf recentf
      :ensure t
      :setq-default ((recentf-max-saved-items . 10000)
		     (recentf-auto-cleanup quote never)
		     (recentf-save-file . "~/.emacs.d/.recentf")
		     (recentf-exclude quote
				      (".recentf")))
      :config
      (recentf-mode 1))

;; https://emacs-jp.github.io/tips/emacs-in-2020 ;;
;;ivyはミニバッファの補完を強化するパッケージです。
;;補完が強化され、 M-x はこのような表示になります。 コマンドの断片で検索できるようになるので、あえてキーバインドを与えず、 M-x から起動する方法も便利です。 この補完では正規表現が使えるので、 ^ivy- をクエリーを入力すれば、 ivy パッケージのインタラクティブ関数が一覧できます。

(leaf ivy
      :doc "Incremental Vertical completYon"
      :req "emacs-24.5"
      :tag "matching" "emacs>=24.5"
      :url "https://github.com/abo-abo/swiper"
      :emacs>= 24.5
      :ensure t
      :blackout t
      :leaf-defer nil
      :custom ((ivy-initial-inputs-alist . nil)
	       (ivy-re-builders-alist . '((t . ivy--regex-fuzzy)
					  (swiper . ivy--regex-plus)))
	       (ivy-use-selectable-prompt . t))
      :global-minor-mode t
      :config
      (leaf swiper
	    :doc "Isearch with an overview. Oh, man!"
	    :req "emacs-24.5" "ivy-0.13.0"
	    :tag "matching" "emacs>=24.5"
	    :url "https://github.com/abo-abo/swiper"
	    :emacs>= 24.5
	    :ensure t
	    :bind (("C-s" . swiper)))

      (leaf counsel
	    :doc "Various completion functions using Ivy"
	    :req "emacs-24.5" "swiper-0.13.0"
	    :tag "tools" "matching" "convenience" "emacs>=24.5"
	    :url "https://github.com/abo-abo/swiper"
	    :emacs>= 24.5
	    :ensure t
	    :blackout t
	    :bind (("C-S-s" . counsel-imenu)
		   ("C-x C-r" . counsel-recentf))
	    :custom `((counsel-yank-pop-separator . "\n----------\n")
		      (counsel-find-file-ignore-regexp . ,(rx-to-string '(or "./" "../") 'no-group)))
	    :global-minor-mode t))

(leaf ivy-rich
      :doc "More friendly display transformer for ivy."
      :req "emacs-24.5" "ivy-0.8.0"
      :tag "ivy" "emacs>=24.5"
      :emacs>= 24.5
      :ensure t
      :after ivy
      :global-minor-mode t)

(leaf prescient
      :doc "Better sorting and filtering"
      :req "emacs-25.1"
      :tag "extensions" "emacs>=25.1"
      :url "https://github.com/raxod502/prescient.el"
      :emacs>= 25.1
      :ensure t
      :commands (prescient-persist-mode)
      :custom `((prescient-aggressive-file-save . t)
		(prescient-save-file . ,(locate-user-emacs-file "prescient")))
      :global-minor-mode prescient-persist-mode)

(leaf ivy-prescient
      :doc "prescient.el + Ivy"
      :req "emacs-25.1" "prescient-4.0" "ivy-0.11.0"
      :tag "extensions" "emacs>=25.1"
      :url "https://github.com/raxod502/prescient.el"
      :emacs>= 25.1
      :ensure t
      :after prescient ivy
      :custom ((ivy-prescient-retain-classic-highlighting . t))
      :global-minor-mode t)


;; ;;flycheckはリアルタイムにソースのエラーやワーニングを表示するマイナーモードです。
;; (leaf flycheck
;;       :doc "On-the-fly syntax checking"
;;       :req "dash-2.12.1" "pkg-info-0.4" "let-alist-1.0.4" "seq-1.11" "emacs-24.3"
;;       :tag "minor-mode" "tools" "languages" "convenience" "emacs>=24.3"
;;       :url "http://www.flycheck.org"
;;       :emacs>= 24.3
;;       :ensure nil
;;       :bind (("M-n" . flycheck-next-error)
;; 	     ("M-p" . flycheck-previous-error))
;;       :global-minor-mode global-flycheck-mode)


(leaf company
      :doc "Modular text completion framework"
      :req "emacs-24.3"
      :tag "matching" "convenience" "abbrev" "emacs>=24.3"
      :url "http://company-mode.github.io/"
      :emacs>= 24.3
      :ensure nil
      :blackout t
      :leaf-defer nil
      :bind ((company-active-map
	      ("M-n" . nil)
	      ("M-p" . nil)
	      ("C-s" . company-filter-candidates)
	      ("C-n" . company-select-next)
	      ("C-p" . company-select-previous)
	      ("<tab>" . company-complete-selection))
	     (company-search-map
	      ("C-n" . company-select-next)
	      ("C-p" . company-select-previous)))
      :custom ((company-idle-delay . 0)
	       (company-minimum-prefix-length . 1)
	       (company-transformers . '(company-sort-by-occurrence)))
      :global-minor-mode global-company-mode)

(leaf company-c-headers
      :doc "Company mode backend for C/C++ header files"
      :req "emacs-24.1" "company-0.8"
      :tag "company" "development" "emacs>=24.1"
      :added "2020-03-25"
      :emacs>= 24.1
      :ensure t
      :after company
      :defvar company-backends
      :config
      (add-to-list 'company-backends 'company-c-headers))
;;companyは入力補完のためのパッケージです。


;;3.9 time locale

(setq system-time-locale "C")

;;3.10 Font

;;(set-fontset-font t 'japanese-jisx0208 "TakaoPGothic")
;;(add-to-list 'face-font-rescale-alist '(".*Takao P.*" . 0.85))

;;;;;;;;;;;;;;;;;;;
;; カットペーストなど挿入削除時にハイライト
;;https://github.com/ncaq/.emacs.d/blob/master/init.el
(leaf volatile-highlights :ensure t :config (volatile-highlights-mode t))

;; 置換の動きを可視化
;;https://github.com/ncaq/.emacs.d/blob/master/init.el

(leaf anzu
      :ensure t
      :custom (global-anzu-mode . t)
      :bind
      ([remap query-replace] . anzu-query-replace)
      ([remap query-replace-regexp] . anzu-query-replace-regexp))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; orgmode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

					;7 Org Mode
					;7.1 Settinngs

(setq-default org-use-speed-commands t
	      org-agenda-todo-ignore-with-date t
	      org-directory "~/org"
	      org-agenda-files '("~/org/"  )
	      org-todo-keywords '((sequence "TODO(t)" "WAIT(w)" "|" "DONE(d)"))
	      org-capture-templates '(("t" "Todo" entry (file+datetree "~/org/todo.org")
				       "* TODO %? %U %i\n %a")
				      ;;("b" "Blog" entry (file "~/org/blog.org")
				      ;; "* %?")
				      ("m" "Memo" entry (file "~/org/memo.org")
				       "* %? %U\n %i")))
(leaf org-temp)


(general-define-key
 :prefix "C-c"
 "a" 'org-agenda
 "b" 'counsel-bookmark
 "c" 'org-capture)

(general-define-key
 :keymaps 'org-mode-map
 "C-m" nil)

;; tags
(setq org-tag-alist '(
		      ("@office".?o)
		      ("@home".?h)
		      ("@blog".?b)
		      ("notes".?n)
		      ("awak".?a)
		      ("7yr".?7)
		      ("14yr".?1)
		      ("READing".?r)))

;; ;7.2 org Keybind

;; (general-define-key
;;  :prefix "C-c"
;;  "a" 'org-agenda
;;  "b" 'counsel-bookmark
;;  "c" 'org-capture)

;; (general-define-key
;;  :keymaps 'org-mode-map
;;  "C-m" nil)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; doom themes https://takeokunn.github.io/.emacs.d/#org1bc4f66
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;6.1.2 all-the-icons-ivy

(leaf all-the-icons-ivy
      :ensure t
      :after all-the-icons
      :config
      (all-the-icons-ivy-setup))

;;6.1.3 doom-modeline

(leaf doom-modeline
      :ensure t
      :hook (after-init-hook)
      :custom ((doom-modeline-buffer-file-name-style quote truncate-with-project)
	       (doom-modeline-icon . t)
	       (doom-modeline-major-mode-icon)
	       (doom-modeline-minor-modes))
      :config
      (with-eval-after-load 'doom-modeline
	(line-number-mode 0)
	(column-number-mode 0)))

;;6.1.4 doom-theme

(leaf doom-themes
      :ensure t
      :after neotree
      :custom-face ((doom-modeline-bar quote
				       ((t
					 (:background "#6272a4")))))
      :config
      (load-theme 'tango-dark t)
      (doom-themes-neotree-config)
      (doom-themes-org-config))


;;6.1.8 nyan-mode

(leaf nyan-mode
      :ensure t
      :after doom-modeline
      :hook (doom-modeline-mode-hook)
      :custom ((nyan-cat-face-number . 4)
	       (nyan-animate-nyancat . t)))


					;6.17.1 ace-window

(leaf ace-window
      :ensure t
      :custom ((aw-keys '(97 115 100 102 103 104 106 107 108))))

					;6.17.2 dashboard

(leaf dashboard
      :ensure t
      :config
      (dashboard-setup-startup-hook))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ddskk
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(leaf ddskk
      :ensure t
      :bind
      ("C-x j" . skk-mode))

;;(leaf skk-study  :ensure t)
;;(leaf skk-hint  :ensure t)

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




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; yatex to latex 野鳥起動のための設定
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(leaf yatex
      :ensure t
      :config )
;; (setq auto-mode-alist
;; (cons (cons "\\.tex$" ’yatex-mode) auto-mode-alist))
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
					;(setq tex-command "ptex2pdf -u -l -ot '-synctex=1'")
					;(setq tex-command "lualatex -synctex=1")
(setq tex-command "llmk")
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
;; (setq dviprint-command-format "wine cmd /c start AcroRd32.exe `echo %s | sed -e \"s/\\.[^.]*$/\\.pdf/\"`")

;; (require 'dbus)

;; (defun un-urlify (fname-or-url)
;;   "A trivial function that replaces a prefix of file:/// with just /."
;;   (if (string= (substring fname-or-url 0 8) "file:///")
;;       (substring fname-or-url 7)
;;     fname-or-url))

;; (defun evince-inverse-search (file linecol &rest ignored)
;;   (let* ((fname (decode-coding-string (url-unhex-string (un-urlify file)) 'utf-8))
;; 	 (buf (find-file fname))
;; 	 (line (car linecol))
;; 	 (col (cadr linecol)))
;;     (if (null buf)
;; 	(message "[Synctex]: %s is not opened..." fname)
;;       (switch-to-buffer buf)
;;       (goto-line (car linecol))
;;       (unless (= col -1)
;; 	(move-to-column col))
;;       (x-focus-frame (selected-frame)))))

;; (dbus-register-signal
;;  :session nil "/org/gnome/evince/Window/0"
;;  "org.gnome.evince.Window" "SyncSource"
;;  'evince-inverse-search)

;; (with-eval-after-load 'yatexprc
;;   (defun YaTeX-preview-jump-line ()
;;     "Call jump-line function of various previewer on current main file"
;;     (interactive)
;;     (save-excursion
;;       (save-restriction
;; 	(widen)
;; 	(let*((pf (or YaTeX-parent-file
;; 		      (save-excursion (YaTeX-visit-main t) (buffer-file-name))))
;; 	      (pdir (file-name-directory pf))
;; 	      (bnr (substring pf 0 (string-match "\\....$" pf)))
;; 					;(cf (file-relative-name (buffer-file-name) pdir))
;; 	      (cf (buffer-file-name)) ;2016-01-08
;; 	      (buffer (get-buffer-create " *preview-jump-line*"))
;; 	      (line (count-lines (point-min) (point-end-of-line)))
;; 	      (previewer (YaTeX-preview-default-previewer))
;; 	      (cmd (cond
;; 		    ((string-match "Skim" previewer)
;; 		     (format "%s %d '%s.pdf' '%s'"
;; 			     YaTeX-cmd-displayline line bnr cf))
;; 		    ((string-match "evince" previewer)
;; 		     (format "%s '%s.pdf' %d '%s'"
;; 			     "fwdevince" bnr line cf))
;; 		    ((string-match "sumatra" previewer)
;; 		     (format "%s \"%s.pdf\" -forward-search \"%s\" %d"
;; 			     previewer bnr cf line))
;; 		    ((string-match "zathura" previewer)
;; 		     (format "%s --synctex-forward '%d:0:%s' '%s.pdf'"
;; 			     previewer line cf bnr))
;; 		    ((string-match "qpdfview" previewer)
;; 		     (format "%s '%s.pdf#src:%s:%d:0'"
;; 			     previewer bnr cf line))
;; 		    ((string-match "okular" previewer)
;; 		     (format "%s '%s.pdf#src:%d %s'"
;; 			     previewer bnr line (expand-file-name cf)))
;; 		    )))
;; 	  (YaTeX-system cmd "jump-line" 'noask pdir))))))

;; (add-hook 'yatex-mode-hook
;; 	  '(lambda ()
;; 	     (auto-fill-mode -1)))

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
	                      "\\documentclass[luatexjp,12pt,paper=a4]{jlreq}
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
	                      "\\documentclass[luatexjp,tate,12pt,paper=a4]{jlreq}
[NO-DEFAULT-PACKAGES]
\\usepackage{amsmath}
\\usepackage{kocho}
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; devuan PATH  ;;;;;;;;;;
;; (when (equal system-type 'windows-nt)
;; ;
;;  )
;; (setenv "PATH"
;;         (concat
;;  ;  "C:\\ProgramData\\chocolatey\\lib\\ag\\tools;"
;;    "/usr/bin"
;;    "/usr/local/texlive/2019/bin/x86_64-linux"
;;    "/bin"
;;    "/app/bin"
;;    (getenv "PATH")))

(setenv "PATH" (concat (getenv "PATH") ":/usr/bin"))
(setq exec-path (append exec-path '("/usr/bin")))
(setenv "PATH" (concat (getenv "PATH") ":/app/bin"))
(setq exec-path (append exec-path '("/app/bin")))
(setenv "PATH" (concat (getenv "PATH") ":/usr/local/texlive/2020/bin/x86_64-linux"))
(setq exec-path (append exec-path '("/usr/local/texlive/2020/bin/x86_64-linux")))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; org-agenda config
;; http://www.i3s.unice.fr/~malapert/emacs_orgmode.html
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (setq org-agenda-ndays 7)
;; (setq org-agenda-show-all-dates t)
;; (setq org-agenda-skip-deadline-if-done t)
;; (setq org-agenda-skip-scheduled-if-done t)
;; (setq org-agenda-start-on-weekday nil)
;; (setq org-deadline-warning-days 14)
;; (setq org-agenda-custom-commands
;;       '(("g" . "GTD contexts")
;;         ("gh" "Home" tags-todo "HOME")
;;         ("gu" "Urgent" tags-todo "URGENT")
;;         ("G" "GTD Block Agenda"
;;          ((todo "STARTED")
;;           (tags-todo "URGENT")
;;           (todo "NEXT"))
;;          ((org-agenda-prefix-format "[ ] %T: ")
;;           (org-agenda-with-colors nil)
;;           (org-agenda-compact-blocks t)
;;           (org-agenda-remove-tags t)
;;           (ps-number-of-columns 2)
;;           (ps-landscape-mode t))
;;          ;;nil                      ;; i.e., no local settings
;;          ("~/next-actions.txt"))
;;         ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; hydra
;; https://www.reddit.com/r/emacs/comments/8of6tx/tip_how_to_be_a_beast_with_hydra/
;; https://github.com/abo-abo/hydra/wiki/Org-clock-and-timers
;;
;; (leaf hydra
;;   :ensure t
;;   :defer 2
;;   :bind ("C-c t" . hydra-clock/body))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (defhydra hydra-org-agenda-clock (:color blue :hint nil)
;;   ("i" org-agenda-clock-in)
;;   ("o" org-agenda-clock-out)
;;   ("q" org-agenda-clock-cancel)
;;   ("g" org-agenda-clock-goto))
;; (bind-keys ("C-c w" . hydra-org-clock/body)
;;            :map org-agenda-mode-map
;;            ("C-c w" . hydra-org-agenda-clock/body))


;; https://github.com/abo-abo/hydra/wiki/Info
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
(global-set-key (kbd "C-c t") 'hydra-clock/body)

;;(defhydra hydra-zoom ()
;;   "zoom"
;;   ("g" text-scale-increase "in")
;;   ("l" text-scale-decrease "out"))
;; (global-set-key (kbd "C-x f") 'hydra-zoom/body)
;;
;;;;

(define-key Info-mode-map (kbd "?") #'hydra-info/body)
(defhydra hydra-info (:color blue
			     :hint nil)
        "
Info-mode:

  ^^_]_ forward  (next logical node)       ^^_l_ast (←)        _u_p (↑)                             _f_ollow reference       _T_OC
  ^^_[_ backward (prev logical node)       ^^_r_eturn (→)      _m_enu (↓) (C-u for new window)      _i_ndex                  _d_irectory
  ^^_n_ext (same level only)               ^^_H_istory         _g_oto (C-u for new window)          _,_ next index item      _c_opy node name
  ^^_p_rev (same level only)               _<_/_t_op           _b_eginning of buffer                virtual _I_ndex          _C_lone buffer
  regex _s_earch (_S_ case sensitive)      ^^_>_ final         _e_nd of buffer                      ^^                       _a_propos

  _1_ .. _9_ Pick first .. ninth item in the node's menu.

"
	("]"   Info-forward-node)
	("["   Info-backward-node)
	("n"   Info-next)
	("p"   Info-prev)
	("s"   Info-search)
	("S"   Info-search-case-sensitively)

	("l"   Info-history-back)
	("r"   Info-history-forward)
	("H"   Info-history)
	("t"   Info-top-node)
	("<"   Info-top-node)
	(">"   Info-final-node)

	("u"   Info-up)
	("^"   Info-up)
	("m"   Info-menu)
	("g"   Info-goto-node)
	("b"   beginning-of-buffer)
	("e"   end-of-buffer)

	("f"   Info-follow-reference)
	("i"   Info-index)
	(","   Info-index-next)
	("I"   Info-virtual-index)

	("T"   Info-toc)
	("d"   Info-directory)
	("c"   Info-copy-current-node-name)
	("C"   clone-buffer)
	("a"   info-apropos)

	("1"   Info-nth-menu-item)
	("2"   Info-nth-menu-item)
	("3"   Info-nth-menu-item)
	("4"   Info-nth-menu-item)
	("5"   Info-nth-menu-item)
	;; ("6"   Info-nth-menu-item)
	;; ("7"   Info-nth-menu-item)
	;; ("8"   Info-nth-menu-item)
	;; ("9"   Info-nth-menu-item)

	("?"   Info-summary "Info summary")
	("h"   Info-help "Info help")
	("q"   Info-exit "Info exit")
	("C-g" nil "cancel" :color blue))
;;;;;;;;
;; https://github.com/abo-abo/hydra/wiki/Org-mode-block-templates
;;;;;;;

;;  (defhydra hydra-org-template (:color blue :hint nil)
;;     "
;;  _c_enter  _q_uote     _e_macs-lisp    _L_aTeX:
;;  _l_atex   _E_xample   _p_erl          _i_ndex:
;;  _a_scii   _v_erse     _P_erl tangled  _I_NCLUDE:
;;  _s_rc     _n_ote      plant_u_ml      _H_TML:
;;  _h_tml    ^ ^         ^ ^             _A_SCII:
;; "
;;     ("s" (hot-expand "<s"))
;;     ("E" (hot-expand "<e"))
;;     ("q" (hot-expand "<q"))
;;     ("v" (hot-expand "<v"))
;;     ("n" (hot-expand "<not"))
;;     ("c" (hot-expand "<c"))
;;     ("l" (hot-expand "<l"))
;;     ("h" (hot-expand "<h"))
;;     ("a" (hot-expand "<a"))
;;     ("L" (hot-expand "<L"))
;;     ("i" (hot-expand "<i"))
;;     ("e" (hot-expand "<s" "emacs-lisp"))
;;     ("p" (hot-expand "<s" "perl"))
;;     ("u" (hot-expand "<s" "plantuml :file CHANGE.png"))
;;     ("P" (hot-expand "<s" "perl" ":results output :exports both :shebang \"#!/usr/bin/env perl\"\n"))
;;     ("I" (hot-expand "<I"))
;;     ("H" (hot-expand "<H"))
;;     ("A" (hot-expand "<A"))
;;     ("<" self-insert-command "ins")
;;     ("o" nil "quit"))

;;   (require 'org-tempo) ; Required from org 9 onwards for old template expansion
;;   ;; Reset the org-template expnsion system, this is need after upgrading to org 9 for some reason
;;   (setq org-structure-template-alist (eval (car (get 'org-structure-template-alist 'standard-value))))
;;   (defun hot-expand (str &optional mod header)
;;     "Expand org template.

;; STR is a structure template string recognised by org like <s. MOD is a
;; string with additional parameters to add the begin line of the
;; structure element. HEADER string includes more parameters that are
;; prepended to the element after the #+HEADER: tag."
;;     (let (text)
;;       (when (region-active-p)
;;         (setq text (buffer-substring (region-beginning) (region-end)))
;;         (delete-region (region-beginning) (region-end))
;;         (deactivate-mark))
;;       (when header (insert "#+HEADER: " header) (forward-line))
;;       (insert str)
;;       (org-tempo-complete-tag)
;;       (when mod (insert mod) (forward-line))
;;       (when text (insert text))))

;;   (define-key org-mode-map "<"
;;     (lambda () (interactive)
;;       (if (or (region-active-p) (looking-back "^"))
;;           (hydra-org-template/body)
;;         (self-insert-command 1))))

;;   (eval-after-load "org"
;;     '(cl-pushnew
;;     '("not" . "note")
;;       org-structure-template-alist))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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


;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;
;; https://github.com/abo-abo/hydra/wiki/Version-Control
;; Navigation among git hunks
;;This hydra allows navigating between git diff hunks in the buffer and acting on them (staging, reverting, etc). It requires git-gutter or git-gutter-fringe.
;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;

(defhydra hydra-git-gutter (:body-pre (git-gutter-mode 1)
				      :hint nil)
    "
Git gutter:
  _j_: next hunk        _s_tage hunk     _q_uit
  _k_: previous hunk    _r_evert hunk    _Q_uit and deactivate git-gutter
  ^ ^                   _p_opup hunk
  _h_: first hunk
  _l_: last hunk        set start _R_evision
"
    ("j" git-gutter:next-hunk)
    ("k" git-gutter:previous-hunk)
    ("h" (progn (goto-char (point-min))
		(git-gutter:next-hunk 1)))
    ("l" (progn (goto-char (point-min))
		(git-gutter:previous-hunk 1)))
    ("s" git-gutter:stage-hunk)
    ("r" git-gutter:revert-hunk)
    ("p" git-gutter:popup-hunk)
    ("R" git-gutter:set-start-revision)
    ("q" nil :color blue)
    ("Q" (progn (git-gutter-mode -1)
		;; git-gutter-fringe doesn't seem to
		;; clear the markup right away
		(sit-for 0.1)
		(git-gutter:clear))
     :color blue))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; prog lang mode
;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; https://github.com/takeokunn/.emacs.d/blob/35f6254f5a73c8d8969796962086f4d2a6341d03/index.org
(leaf web-mode
      :ensure t
      :bind ("C-j" . web-mode-comment-indent-new-line)
      :mode ("\\.html?\\'" "\\.erb\\'" "\\.gsp\\'" "\\.tsx\\'"))
;;4.37 yaml-mode
(leaf yaml-mode
      :ensure t
      :mode ("\\.ya?ml$"))

;; Org mode の C-c C-s で挿入する日付の曜日、英語曜日表記を強制する。

(setq system-time-locale "C")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; vcs git{attributes,config,ignore}-mode, git-commit
;;; https://uwabami.github.io/cc-env/Emacs.html
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(leaf *git
      :emacs>= 25
      :init
      (leaf git-commit :ensure t)
      (leaf gitattributes-mode :ensure t)
      (leaf gitconfig-mode :ensure t)
      (leaf gitignore-mode :ensure t)
      )


;;Git Gutter+
(leaf git-gutter+
      :emacs>= 25
      :ensure t
;      :blackout `((git-gutter+-mode
;		   . ,(format "%s" (all-the-icons-octicon "git-merge"))))
      :bind ("C-x G" . global-git-gutter+-mode)
      )

;; magit
;; https://uwabami.github.io/cc-env/Emacs.html

;; (leaf magit
;;   :bind (("C-x g" . magit-status))
;;   :ensure t
;;   :init
;;   (leaf transient
;;     :custom
;;     `((transient-history-file
;;        . ,(expand-file-name "transient-history.el" my:d:tmp))
;;       (transient-levels-file
;;        . ,(expand-file-name "transient-levels.el" my:d:tmp))
;;       (transient-values-file
;;        . ,(expand-file-name "transient-values.el" my:d:tmp))
;;       (transient-force-fixed-pitch . t))
;;     )
;;   :config
;;   (setq magit-completing-read-function 'ivy
;;         magit-refs-show-commit-count   'all
;;         magit-log-buffer-file-locked   t
;;         magit-revision-show-gravatars  nil
;;         )
;;   )
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ack grep A minimalistic interface to Ack is:
;;; https://www.emacswiki.org/emacs/Ack
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; AWK Source Code Blocks in Org Mode
;; https://orgmode.org/worg/org-contrib/babel/languages/ob-doc-awk.html
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(org-babel-do-load-languages
 'org-babel-load-languages
 '((awk . t)))

;; makefile

(setq org-src-preserve-indentation t)

;; Currently, there is no need to activate makefile with org-babel-do-load-languages, but it won't hurt to do so.

(org-babel-do-load-languages
 'org-babel-load-languages
 '((makefile . t)))


;;https://emacs-jp.github.io/tips/emacs-in-2020
;;    対応する括弧の自動挿入
(leaf electric
      :doc "window maker and Command loop for `electric' modes"
      :tag "builtin"
      :added "2020-08-27"
      :init (electric-pair-mode 1))

;;   ispell を aspell で使う
(leaf ispell
      :doc "interface to spell checkers"
      :tag "builtin"
      :added "2020-08-27"
      :setq-default (ispell-program-name . "aspell"))


;;4.9 emacs-lisp-mode
(leaf emacs-lisp-mode
      :mode ("\\.skk$"))


;;cc-mode
(leaf cc-mode
      :doc "major mode for editing C and similar languages"
      :tag "builtin"
      :defvar (c-basic-offset)
      :bind (c-mode-base-map
	     ("C-c c" . compile))
      :mode-hook
      (c-mode-hook . ((c-set-style "bsd")
		      (setq c-basic-offset 4)))
      (c++-mode-hook . ((c-set-style "bsd")
			(setq c-basic-offset 4))))

;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
;;; @ cperl-mode
;;; https://www.emacswiki.org/emacs/CPerlMode                       ;;;
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;

;; ;;; cperl-mode is preferred to perl-mode
;; ;;; "Brevity is the soul of wit" <foo at acm.org>
;; (defalias 'perl-mode 'cperl-mode)

;; ;;; (add-to-list 'auto-mode-alist '("\\.\\([pP][Llm]\\|al\\)\\'" . cperl-mode))
;; (add-to-list 'auto-mode-alist '("\\.\\([pP][Llm]\\|t\\)\\'" . cperl-mode))
;; (add-to-list 'interpreter-mode-alist '("perl" . cperl-mode))
;; (add-to-list 'interpreter-mode-alist '("perl5" . cperl-mode))
;; (add-to-list 'interpreter-mode-alist '("miniperl" . cperl-mode))

;; ;; (cperl-set-style "PerlStyle")

;; ;; Emacs M-x toggle-truncate-lines: 長い行の折り返し表示を切り換える
;; (global-set-key (kbd "C-c t") 'toggle-truncate-lines)

;; ;;;;;; perl custom

;;  ;;load cperl, then work around indent issue
;;  (load-library "cperl-mode")
;;  (defun cperl-backward-to-start-of-continued-exp (lim)
;;    (goto-char (1+ lim))
;;    (forward-sexp)
;;    (beginning-of-line)
;;    (skip-chars-forward " \t")
;;  )

;;フォントの設定を忘れてた。
;;設定の仕方は、シンプルに

;; (setq default-frame-alist
;;       '(
;;         (font . "Cica 16")))

;;
;;;; https://www.gnu.org/software/guile/manual/html_node/Using-Guile-in-Emacs.html
;;scheme-mode
;;
;; (leaf geiser
;;       :ensure t
;;       :config
;;       (setq geiser-active-implementations '(guile)))


;; ;;
;; ;;https://github.com/emacsmirror/geiser-gauche
;; (leaf geiser-gauche
;;   :ensure t
;;   :after geiser
;;   :init (add-to-list 'geiser-active-implementations 'gauche))

;; scheme mode
;; https://haskell.hatenablog.com/entry/Settings-to-use-Gauche_Scheme-with-Emacs#Emacse%E3%81%A7Gauche%E3%82%92%E4%BD%BF%E3%81%86%E8%A8%AD%E5%AE%9A

(setq scheme-program-name "gosh -i")
(autoload 'scheme-mode "cmuscheme" "Major mode for scheme." t)
(autoload 'run-scheme "cmuscheme" "Run a n inferior Scheme process." t)

(defun scheme-other-window ()
  "Run scheme on other window"
  (interactive)
  (switch-to-buffer-other-window
    (get-buffer-create "*scheme*"))
  (run-scheme scheme-program-name))

(define-key global-map
  "\C-cs" 'scheme-other-window)


;;
(leaf paredit
      :ensure t
      :commands enable-paredit-mode
      :hook ((emacs-lisp-mode-hook . enable-paredit-mode)
	     (lisp-mode-hook . enable-paredit-mode)
	     (lisp-interacton-mode-hook . enable-paredit-mode)
	     (scheme-mode-hook . enable-paredit-mode))
      :bind
      ("C-<right>" . paredit-forward-slurp-sexp)
      ("C-c f" . paredit-forward-slurp-sexp)
      ("C-<left>" . paredit-forward-barf-sexp)
      ("C-c b" . paredit-forward-barf-sexp))

;;5.1.2 rainbow-delimiters
(leaf rainbow-delimiters
      :ensure t
      :hook (prog-mode-hook))

;; GNU-Emacsには、Lispを起動するコマンドがあります。
;; Lisp起動コマンドでgclを起動するには、
;; .emacs中などで
;;(setq inferior-lisp-program "/usr/bin/gcl")
;;(setq inferior-lisp-program "/usr/bin/clisp")
(setq inferior-lisp-program "/usr/bin/ecl")

;;slime-mode
;;https://github.com/exot/.emacs.d/blob/9cf17c973f889621e2cd6452bcfe3b20d36a072f/init.el
;;
;; (leaf slime
;;   :ensure t
;;   :commands (slime slime-mode slime-connect)
;;   :init     (progn
;;               (setq inferior-lisp-program "/usr/bin/ecl" ;;"sbcl --noinform --no-linedit"
;;                     slime-compile-file-options '(:fasl-directory "/home/mevius/tmp/slime-fasls/")
;;                     slime-net-coding-system 'utf-8-unix
;;                     slime-completion-at-point-functions 'slime-fuzzy-complete-symbol
;;                     slime-lisp-implementations '((gcl ("gcl") :coding-system utf-8-unix)
;;                                                  (clisp ("clisp") :coding-system utf-8-unix)
;;                                                  (ecl ("ecl") :coding-system utf-8-unix))
;;                     slime-repl-history-remove-duplicates t
;;                     slime-repl-history-trim-whitespaces t)
;;               (add-hook 'lisp-mode-hook '(lambda () (slime-mode +1)) t))
;;   :config   (progn
;;               (make-directory "/home/mevius/tmp/slime-fasls/" t)
;;               (slime-setup '(slime-repl slime-fancy slime-autodoc))
;;               (add-hook 'slime-mode-hook 'slime-redirect-inferior-output)))

;; ;;5.2.2 slime
;; ;;https://github.com/takeokunn/.emacs.d/blob/35f6254f5a73c8d8969796962086f4d2a6341d03/index.org
;; (with-eval-after-load 'slime
;;  ; (load (expand-file-name "~/.roswell/helper.el"))
;;   (add-hook 'slime-mode-hook 'set-up-slime-ac)
;;   (add-hook 'slime-repl-mode-hook 'set-up-slime-ac))
;; ;;
;; (leaf elisp-slime-nav :ensure t)

;;https://www.ncaq.net/2020/04/05/22/33/21/

;; (leaf helpful
;;       :ensure t
;;       :require t
;;       :bind
;;       ([remap describe-function] . helpful-callable)
;;       ([remap describe-key]      . helpful-key)
;;       ([remap describe-variable] . helpful-variable)
;;       :defvar helpful-mode-map
;;       :config (ncaq-set-key helpful-mode-map))


;; ;3.4 行番号を表示する
;; (global-linum-mode t)
;;   行番号の表示を linum で表示

;; (leaf linum
;;   :doc "display line numbers in the left margin"
;;   :tag "builtin"
;;   :added "2020-08-27"
;;   :init (global-linum-mode 1))

;; emacs 26で入ったdisplay-line-numbersを利用する
;;emacsで左側に行数を表示するlinum-modeは重いことで有名だった。 軽くするためにはいろいろと設定しなくてはいけなかった.
;;しかし, emacs26でついに行数表示のネイティブ実装であるdiplay-line-numbers-modeが実装された.

(if (version<= "26.0.50" emacs-version)
    (global-display-line-numbers-mode))
;;個人的にはemacs -nwで起動したときに行数表示の色が見にくかったので以下のようにしている

;; (if (version<= "26.0.50" emacs-version)
;;     (progn
;;       (global-display-line-numbers-mode)
;;       (defun display-line-numbers-color-on-after-init (frame)
;;         "Hook function executed after FRAME is generated."
;;         (unless (display-graphic-p frame)
;;           (set-face-background
;;            'line-number
;;            (plist-get base16-solarized-dark-colors :base01))))
;;       (add-hook 'after-make-frame-functions
;;                 (lambda (frame)
;;                   (display-line-numbers-color-on-after-init frame)))
;;       ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; マシーン依存の個別path
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; ;; Texinfo my-path
;; (require 'info)
;; (setq Info-directory-list
;;  (cons (expand-file-name "/home/mevius/info")
;;        Info-directory-list))
;;実際にInfoディレクトリを追加してみる.emacs.el設定

;; Emacs and Other Info files
(require 'info)
(setq Info-default-directory-list
      (cons (expand-file-name "/home/mevius/info/")
	    Info-default-directory-list))

;;;;;;;;;;;;;;;;;;;;;;;;;; test  ;;;;;;;;;;;;;;;;;
;; https://uwabami.github.io/cc-env/Emacs.html

;; ;;;###autoload
;; (defun my:load-window-config ()
;;   "load window-system specific settings"
;;   (interactive)
;;   (progn
;;     (set-face-attribute 'default nil
;;                         :family "FSMRMP"
;;                         :height 135)
;;     (set-face-attribute 'fixed-pitch nil
;;                         :family "FSMRMP"
;;                         :height 135)
;;     (set-face-attribute 'variable-pitch nil
;;                         :family "FSMRMP"
;;                         :height 135)
;;     ;; Math symbols
;;     (set-fontset-font nil
;;                       '(#x2200 . #x22FF)
;;                       (font-spec :family "FSMRMP" :height 135))
;;     ;; Greek
;;     (set-fontset-font nil
;;                       '(#x0370 . #x03FF)
;;                       (font-spec :family "FSMRMP" :height 135))
;;     ;; Some Icons
;;     (set-fontset-font nil
;;                       '(#xE0A0 . #xEEE0)
;;                       (font-spec :family "FSMRMP" :height 135))
;;     (setq use-default-font-for-symbols t)
;;     ))
;; ;;;###autoload
;; (defun my:load-side-window-config ()
;;   "load window-system specific settings"
;;   (interactive)
;;   (progn
;;     (set-face-attribute 'default nil
;;                         :family "FSMRMP"
;;                         :height 120)
;;     (set-face-attribute 'fixed-pitch nil
;;                         :family "FSMRMP"
;;                         :height 120)
;;     (set-face-attribute 'variable-pitch nil
;;                         :family "FSMRMP"
;;                         :height 120)
;;     ;; Math symbols
;;     (set-fontset-font nil
;;                       '(#x2200 . #x22FF)
;;                       (font-spec :family "FSMRMP" :height 120))
;;     ;; Greek
;;     (set-fontset-font nil
;;                       '(#x0370 . #x03FF)
;;                       (font-spec :family "FSMRMP" :height 120))
;;     ;; Some Icons
;;     (set-fontset-font nil
;;                       '(#xE0A0 . #xEEE0)
;;                       (font-spec :family "FSMRMP" :height 120))
;;     ))
;; (leaf *gui
;;   :if window-system
;;   :config
;;   (set-frame-parameter nil 'alpha 95)
;;   (setq use-default-font-for-symbols nil)
;; ;;  (my:load-window-config)
;;   )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(leaf exec-path-from-shell
      :doc "Get environment variables such as $PATH from the shell"
      :req "emacs-24.1"
      :tag "environment" "unix" "emacs>=24.1"
      :added "2020-08-27"
      :url "https://github.com/purcell/exec-path-from-shell"
      :emacs>= 24.1
      :ensure t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(provide 'init)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(exec-path-from-shell-initialize)
(server-start)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Local Variables:
;; indent-tabs-mode: nil
;; End:

;;; init.el ends here


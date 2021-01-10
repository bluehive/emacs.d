;;; init.el --- My init.el  -*- lexical-binding: t; -*-

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; My init.el.
;; byte compile ;;;;;;;;;;;;;; emacs --batch -f batch-byte-compile init.el
;;; Code:

;; this enables this running method
;;   emacs -q -l ~/.debug.emacs.d/init.el
(eval-and-compile
  (when (or load-file-name byte-compile-current-file)
    (setq user-emacs-directory
          (expand-file-name
           (file-name-directory (or load-file-name byte-compile-current-file))))))

(eval-and-compile
  (customize-set-variable
   'package-archives '(("gnu"   . "https://elpa.gnu.org/packages/")
                       ("melpa" . "https://melpa.org/packages/")
                       ("org"   . "https://orgmode.org/elpa/")))
  (package-initialize)
  (unless (package-installed-p 'leaf)
    (package-refresh-contents)
    (package-install 'leaf))

  (leaf leaf-keywords
    :ensure t
    :init
    ;; optional packages if you want to use :hydra, :el-get, :blackout,,,
    (leaf hydra :ensure t)
    (leaf el-get :ensure t)
    (leaf blackout :ensure t)

    :config
    ;; initialize leaf-keywords.el
    (leaf-keywords-init)))


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

;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ここにいっぱい設定を書く
;; https://emacs-jp.github.io/tips/emacs-in-2020
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(leaf cus-edit
  :doc "tools for customizing Emacs and Lisp packages"
  :tag "builtin" "faces" "help"
  :custom `((custom-file . ,(locate-user-emacs-file "custom.el"))))



(eval-and-compile
  (leaf bytecomp
    :doc "compilation of Lisp code into byte code"
    :tag "builtin" "lisp"
    :custom (byte-compile-warnings . '(cl-functions))))



(leaf autorevert
  :doc "revert buffers when files on disk change"
  :tag "builtin"
  :custom ((auto-revert-interval . 0.3)
           (auto-revert-check-vc-info . t))
  :global-minor-mode global-auto-revert-mode)


;;delsel

(leaf delsel
  :doc "delete selection if you insert"
  :tag "builtin"
  :global-minor-mode delete-selection-mode)

;;選択している状態で入力したときに、regionを削除して挿入するマイナーモードです。 おそらくこの挙動のほうが現代人の意図に合っていると思います。

;;paren

(leaf paren
  :doc "highlight matching paren"
  :tag "builtin"
  :custom ((show-paren-delay . 0.1))
  :global-minor-mode show-paren-mode)

;;対応するカッコを強調表示するマイナーモードです。

;;simple

(leaf simple
  :doc "basic editing commands for Emacs"
  :tag "builtin" "internal"
  :custom ((kill-ring-max . 100)
           (kill-read-only-ok . t)
           (kill-whole-line . t)
           (eval-expression-print-length . nil)
           (eval-expression-print-level . nil)))

;;kill-ringの数を制御したり、kill-lineの挙動を変更したりします。
;;files

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

;;Emacsで好みが分かれる設定として、バックアップファイルを開いているファイルと同じディレクトリに作成するという挙動があります。 実際、このバックアップファイルに助けられることもあるので、 .emacs.d 以下にディレクトリを掘って、そこに保存するようにします。

;;startup

(leaf startup
  :doc "process Emacs shell arguments"
  :tag "builtin" "internal"
  :custom `((auto-save-list-file-prefix . ,(locate-user-emacs-file "backup/.saves-"))))

;;自動保存されたファイルのリストです。 .emacs.d/backup 以下にまとめて保存するようにします。


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

;;ivyはミニバッファの補完を強化するパッケージです。

;;補完が強化され、 M-x はこのような表示になります。 コマンドの断片で検索できるようになるので、あえてキーバインドを与えず、 M-x から起動する方法も便利です。 この補完では正規表現が使えるので、 ^ivy- をクエリーを入力すれば、 ivy パッケージのインタラクティブ関数が一覧できます。


(leaf flycheck
  :doc "On-the-fly syntax checking"
  :req "dash-2.12.1" "pkg-info-0.4" "let-alist-1.0.4" "seq-1.11" "emacs-24.3"
  :tag "minor-mode" "tools" "languages" "convenience" "emacs>=24.3"
  :url "http://www.flycheck.org"
  :emacs>= 24.3
  :ensure nil
  :bind (("M-n" . flycheck-next-error)
         ("M-p" . flycheck-previous-error))
  :global-minor-mode global-flycheck-mode)

;;flycheckはリアルタイムにソースのエラーやワーニングを表示するマイナーモードです。


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


;;3.11 GC

(setq gc-cons-threshold (* 128 1024 1024)) ;; 128MB
(setq garbage-collection-messages t)


;3.4 行番号を表示する

(global-linum-mode t)


 ;;   行番号の表示を linum で表示

(leaf linum
  :doc "display line numbers in the left margin"
  :tag "builtin"
  :added "2020-08-27"
  :init (global-linum-mode 1))

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



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; orgmode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;7 Org Mode
;7.1 Settinngs

(setq-default org-use-speed-commands t
		org-agenda-todo-ignore-with-date t
		org-directory "~/org"
		org-agenda-files '("~/org/todo.org" "~/Documents/organized/blog-3-0.org" "~/Documents/organized/blog-2.org" )
		org-todo-keywords '((sequence "TODO(t)" "WAIT(w)" "|" "DONE(d)"))
		org-capture-templates '(("t" "Todo" entry (file+datetree "~/org/todo.org")
					 "* %?")
					("b" "Blog" entry (file "~/org/blog.org")
					 "* %?")
					("m" "Memo" entry (file "~/org/memo.org")
					 "* %?")))
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
                      ("READing".?R)))

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

;(leaf skk-study  :ensure t)
;(leaf skk-hint  :ensure t)

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

;;;;;; perl custom

 ;;load cperl, then work around indent issue
 (load-library "cperl-mode")
 (defun cperl-backward-to-start-of-continued-exp (lim)
   (goto-char (1+ lim))
   (forward-sexp)
   (beginning-of-line)
   (skip-chars-forward " \t")
 )

;;フォントの設定を忘れてた。
;;設定の仕方は、シンプルに

;; (setq default-frame-alist
;;       '(
;;         (font . "Cica 16")))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; yatex to latex 野鳥起動のための設定
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(leaf yatex
  :ensure t
  :config )
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
  ;; 	 ;  "C:\\ProgramData\\chocolatey\\lib\\ag\\tools;"
  ;; 	   "/usr/bin"
  ;; 	   "/usr/local/texlive/2019/bin/x86_64-linux"
  ;; 	   "/bin"
  ;; 	   "/app/bin"
  ;; 	   (getenv "PATH")))

    (setenv "PATH" (concat (getenv "PATH") ":/usr/bin"))
    (setq exec-path (append exec-path '("/usr/bin")))
    (setenv "PATH" (concat (getenv "PATH") ":/app/bin"))
    (setq exec-path (append exec-path '("/app/bin")))
    (setenv "PATH" (concat (getenv "PATH") ":/usr/local/texlive/2020/bin/x86_64-linux"))
    (setq exec-path (append exec-path '("/usr/local/texlive/2020/bin/x86_64-linux")))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; ob-shell.el --- Babel Functions for Shell Evaluation 
;; Copyright (C) 2009-2020 Free Software Foundation, Inc.

;; Author: Eric Schulte
;; Keywords: literate programming, reproducible research
;; Homepage: https://orgmode.org
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; This file is part of GNU Emacs.
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

;(provide 'ob-shell)
;;; ob-shell.el ends here

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; org-agenda config
;; http://www.i3s.unice.fr/~malapert/emacs_orgmode.html
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq org-agenda-ndays 7)
(setq org-agenda-show-all-dates t)
(setq org-agenda-skip-deadline-if-done t)
(setq org-agenda-skip-scheduled-if-done t)
(setq org-agenda-start-on-weekday nil)
(setq org-deadline-warning-days 14)
(setq org-agenda-custom-commands
      '(("g" . "GTD contexts")
        ("gh" "Home" tags-todo "HOME")
        ("gu" "Urgent" tags-todo "URGENT")
        ("G" "GTD Block Agenda"
         ((todo "STARTED")
          (tags-todo "URGENT")
          (todo "NEXT"))
         ((org-agenda-prefix-format "[ ] %T: ")
          (org-agenda-with-colors nil)
          (org-agenda-compact-blocks t)
          (org-agenda-remove-tags t)
          (ps-number-of-columns 2)
          (ps-landscape-mode t))
         ;;nil                      ;; i.e., no local settings
         ("~/next-actions.txt"))
        ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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

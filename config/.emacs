;; Packages

;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
;;(package-initialize)

;; Suppress warnings
;; https://andrewjamesjohnson.com/suppressing-ad-handle-definition-warnings-in-emacs/
(setq ad-redefinition-action 'accept)

;; Set default font size
;;(set-face-attribute 'default nil :height 140)

;; There are some precompilation warnings that are suppressed
;; only by vacuously declaring some variables.
;; I keep these declarations in defvars.el
(load "~/.emacs.d/config/defvars.el")

;; Initialize package system and declare/install all packages we use
(load "~/.emacs.d/config/general.el")

;; User-specific settings (files containing secret things are pgp encrypted)
(let ((config-path (concat "~/.emacs.d/config/" (getenv "USER"))))
  (if (file-exists-p config-path)
      (dolist (file-name (directory-files config-path))
	(if (or (string-match-p "\.el$" file-name)
		(string-match-p "\.el.gpg$" file-name))
	    (load (concat config-path "/" file-name))))))

 
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:foreground "#e4e4ef" :background "#181818"))))
 ;;'(fixed-pitch ((t (:family "Consolas" :slant normal :weight normal :height 0.9 :width normal))))
 '(org-block ((t (:inherit fixed-pitch))))
 '(org-document-info-keyword ((t (:inherit (shadow fixed-pitch)))))
 '(org-level-1 ((t (:foreground "linen" :weight normal :height 1.2))))
 '(org-level-2 ((t (:foreground "MediumPurple3" :weight bold :height 1.0))))
 '(org-level-3 ((t (:foreground "#6ac214" :weight normal :height 1.0))))
 '(org-property-value ((t (:inherit fixed-pitch))) t)
 '(org-special-keyword ((t (:inherit (font-lock-comment-face fixed-pitch)))))
 '(org-tag ((t (:inherit (shadow fixed-pitch) :weight bold))))
 '(org-verbatim ((t (:inherit (shadow fixed-pitch)))))
 ;;'(variable-pitch ((t (:family "Source Sans Pro" :height 120 :weight light)))))
;;  '(term-color-black ((t (:foreground "#313131" :background "#141414"))))
;;  '(term-color-blue ((t (:foreground "#7587A6" :background "#5d6c84"))))
;;  '(term-color-cyan ((t (:foreground "#5d8084" :background "#41595c"))))
;;  '(term-color-green ((t (:foreground "#8F9D6A" :background "#646d4a"))))
;;  '(term-color-magenta ((t (:foreground "#EE799F" :background "#EF2929"))))
;;  '(term-color-red ((t (:foreground "#CF6A4C" :background "#a3472c"))))
;;  '(term-color-white ((t (:foreground "#CACACA" :background "#5F5A60"))))
;;  '(term-color-yellow ((t (:foreground "#efa510" :background "#CDA869"))))
;;  '(term-default-bg-color ((t (:inherit term-color-black))))
;;  '(term-default-fg-color ((t (:inherit term-color-white)))))

)
 (set-cursor-color "#954020")

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default default default italic underline success warning error])
 '(ansi-color-names-vector
   ["#ffffff" "#37ffff" "#e074e3" "#3732ff" "#ffff0b" "#37ff3c" "#ff400b" "#848088"])
 '(column-number-mode t)
 '(company-quickhelp-color-background "#4F4F4F")
 '(company-quickhelp-color-foreground "#DCDCCC")
;; '(custom-enabled-themes (quote (org-beautify tangotango)))
 '(custom-enabled-themes (quote (tangotango)))
 '(custom-safe-themes
   (quote
    ("62a6731c3400093b092b3837cff1cb7d727a7f53059133f42fcc57846cfa0350" "880f541eabc8c272d88e6a1d8917fe743552f17cedd8f138fe85987ee036ad08" "76935a29af65f8c915b1b3b4f6326e2e8d514ca098bd7db65b0caa533979fc01" "53de65a1e7300e0f1a4f8bf317530a5008e9d06a0e2f8863b80dc56d77f844cf" "a621dd9749f2651e357a61f8d8d2d16fb6cacde3b3784d02151952e1b9781f05" "1a2cde373eff9ffd5679957c7ecfc6249d353e1ee446d104459e73e924fe0d8a" "938f120eeda938eef2c36b4cc9609d1ad91b3a3666cd63a4be5b70b739004942" "45482e7ddf47ab1f30fe05f75e5f2d2118635f5797687e88571842ff6f18b4d5" "bce1c321471d37b875f99c83cb7b451fd8386001259e1c0909d6e078ea60f00b" "f19d195fa336e9904303eea20aad35036b79cfde72fa6e76b7462706acd52920" "2ea9afebc23cca3cd0cd39943b8297ce059e31cb62302568b8fa5c25a22db5bc" "1342a81078bdac27f80b86807b19cb27addc1f9e4c6a637a505ae3ba4699f777" "80a23d559a5c5343a0882664733fd2c9e039b4dbf398c70c424c8d6858b39fc5" "1c10e946f9a22b28613196e4c02b6508970e3b34660282ec92d9a1c293ee81bb" "68b847fac07094724e552eeaf96fa4c7e20824ed5f3f225cad871b8609d50ace" "a5a2954608aac5c4dcf9659c07132eaf0da25a8f298498a7eacf97e2adb71765" "cc2f32f5ee19cbd7c139fc821ec653804fcab5fcbf140723752156dc23cdb89f" "d422c7673d74d1e093397288d2e02c799340c5dabf70e87558b8e8faa3f83a6c" "c51e302edfe6d2effca9f7c9a8a8cfc432727efcf86246002a3b45e290306c1f" "b48599e24e6db1ea612061252e71abc2c05c05ac4b6ad532ad99ee085c7961a7" "daeaa8249f0c275de9e32ed822e82ff40457dabe07347fe06afc67d962a3b1e9" "ff6a8955945028387ed1a2b0338580274609fbb0d40cd011b98ca06bd00d9233" "5e402ccb94e32d7d09e300fb07a62dc0094bb2f16cd2ab8847b94b01b9d5e866" "6a674ffa24341f2f129793923d0b5f26d59a8891edd7d9330a258b58e767778a" "4e7e04c4b161dd04dc671fb5288e3cc772d9086345cb03b7f5ed8538905e8e27" "701b4b4e7989329a0704b92fc17e6600cc18f9df4f2466617ec91c932b5477eb" "b71da830ae97a9b70d14348781494b6c1099dbbb9b1f51494c3dfa5097729736" "1127f29b2e4e4324fe170038cbd5d0d713124588a93941b38e6295a58a48b24f" "abd7719fd9255fcd64f631664390e2eb89768a290ee082a9f0520c5f12a660a8" "0973b33d2f15e6eaf88400eee3dc8357ad8ae83d2ca43c125339b25850773a70" "deb7ae3a735635a85c984ece4ce70317268df6027286998b0ea3d10f00764c9b" "e26e879d250140e0d4c4d5ab457c32bcb29742599bd28c1ce31301344c6f2a11" "cdc2a7ba4ecf0910f13ba207cce7080b58d9ed2234032113b8846a4e44597e41" "a02c000c95c43a57fe1ed57b172b314465bd11085faf6152d151385065e0e4b1" "0e8c264f24f11501d3f0cabcd05e5f9811213f07149e4904ed751ffdcdc44739" "fb09acc5f09e521581487697c75b71414830b1b0a2405c16a9ece41b2ae64222" "72c530c9c8f3561b5ab3bf5cda948cd917de23f48d9825b7a781fe1c0d737f2f" "76c5b2592c62f6b48923c00f97f74bcb7ddb741618283bdb2be35f3c0e1030e3" "ff8c6c2eb94e776c9eed9299a49e07e70e1b6a6f926dec429b99cf5d1ddca62a" "44f5578eccb2cde3b196dfa86a298b75fe39ceff975110c091fa8c874c338b50" "b8c5adfc0230bd8e8d73450c2cd4044ad7ba1d24458e37b6dec65607fc392980" "1a094b79734450a146b0c43afb6c669045d7a8a5c28bc0210aba28d36f85d86f" "6213a6047cc19f580c37ef3f6d47fd5a55ebdf9b5590475d8f7a6aecd79a1cc0" "05d009b7979e3887c917ef6796978d1c3bbe617e6aa791db38f05be713da0ba0" "3fe4861111710e42230627f38ebb8f966391eadefb8b809f4bfb8340a4e85529" "5c83b15581cb7274085ba9e486933062652091b389f4080e94e4e9661eaab1aa" "da8e6e5b286cbcec4a1a99f273a466de34763eefd0e84a41c71543b16cd2efac" "77515a438dc348e9d32310c070bfdddc5605efc83671a159b223e89044e4c4f1" "a513bb141af8ece2400daf32251d7afa7813b3a463072020bb14c82fd3a5fe30" "9bd5ee2b24759fbc97f86c2783d1bf8f883eb1c0dd2cf7bda2b539cd28abf6a9" "39a854967792547c704cbff8ad4f97429f77dfcf7b3b4d2a62679ecd34b608da" "2d5c40e709543f156d3dee750cd9ac580a20a371f1b1e1e3ecbef2b895cf0cd2" "392f19e7788de27faf128a6f56325123c47205f477da227baf6a6a918f73b5dc" "7bd626fcc9fbfb44186cf3f08b8055d5a15e748d5338e47f9391d459586e20db" "be5b03913a1aaa3709d731e1fcfd4f162db6ca512df9196c8d4693538fa50b86" "a455366c5cdacebd8adaa99d50e37430b0170326e7640a688e9d9ad406e2edfd" "d8dc153c58354d612b2576fea87fe676a3a5d43bcc71170c62ddde4a1ad9e1fb" "7153b82e50b6f7452b4519097f880d968a6eaf6f6ef38cc45a144958e553fbc6" "5e3fc08bcadce4c6785fc49be686a4a82a356db569f55d411258984e952f194a" "a0feb1322de9e26a4d209d1cfa236deaf64662bb604fa513cca6a057ddf0ef64" "ab04c00a7e48ad784b52f34aa6bfa1e80d0c3fcacc50e1189af3651013eb0d58" "04dd0236a367865e591927a3810f178e8d33c372ad5bfef48b5ce90d4b476481" "7356632cebc6a11a87bc5fcffaa49bae528026a78637acd03cae57c091afd9b9" "bf798e9e8ff00d4bf2512597f36e5a135ce48e477ce88a0764cfb5d8104e8163" "36ca8f60565af20ef4f30783aa16a26d96c02df7b4e54e9900a5138fb33808da" "c9ddf33b383e74dac7690255dd2c3dfa1961a8e8a1d20e401c6572febef61045" "24fc62afe2e5f0609e436aa2427b396adf9a958a8fa660edbaab5fb13c08aae6" "e7b49145d311e86da34a32a7e1f73497fa365110a813d2ecd8105eaa551969da" "2593436c53c59d650c8e3b5337a45f0e1542b1ba46ce8956861316e860b145a0" "6e32d7aab92ad2ad4d3a915cd9ace5dc1d9d8f0486b785bdb86c79ff5ca0c189" "74f16f67a9a9b44ca05d67027bbc62af0921c0535dc57bd6f3ee0a459599e54b" "3f5f69bfa958dcf04066ab2661eb2698252c0e40b8e61104e3162e341cee1eb9" "672bb062b9c92e62d7c370897b131729c3f7fd8e8de71fc00d70c5081c80048c" "d8a7a7d2cffbc55ec5efbeb5d14a5477f588ee18c5cddd7560918f9674032727" "1a212b23eb9a9bedde5ca8d8568b1e6351f6d6f989dd9e9de7fba8621e8ef82d" "713f898dd8c881c139b62cf05b7ac476d05735825d49006255c0a31f9a4f46ab" "4639288d273cbd3dc880992e6032f9c817f17c4a91f00f3872009a099f5b3f84" default)))
 '(diary-entry-marker (quote font-lock-variable-name-face))
 '(display-time-mode t)
 '(emms-mode-line-icon-image-cache
   (quote
    (image :type xpm :ascent center :data "/* XPM */
static char *note[] = {
/* width height num_colors chars_per_pixel */
\"    10   11        2            1\",
/* colors */
\". c #358d8d\",
\"# c None s None\",
/* pixels */
\"###...####\",
\"###.#...##\",
\"###.###...\",
\"###.#####.\",
\"###.#####.\",
\"#...#####.\",
\"....#####.\",
\"#..######.\",
\"#######...\",
\"######....\",
\"#######..#\" };")))
 '(fci-rule-color "#383838")
 '(frame-background-mode (quote dark))
 '(frame-brackground-mode (quote dark))
 '(gnus-logo-colors (quote ("#0d7b72" "#adadad")) t)
 '(gnus-mode-line-image-cache
   (quote
    (image :type xpm :ascent center :data "/* XPM */
static char *gnus-pointer[] = {
/* width height num_colors chars_per_pixel */
\"    18    13        2            1\",
/* colors */
\". c #358d8d\",
\"# c None s None\",
/* pixels */
\"##################\",
\"######..##..######\",
\"#####........#####\",
\"#.##.##..##...####\",
\"#...####.###...##.\",
\"#..###.######.....\",
\"#####.########...#\",
\"###########.######\",
\"####.###.#..######\",
\"######..###.######\",
\"###....####.######\",
\"###..######.######\",
\"###########.######\" };")) t)
 '(hl-sexp-background-color "#33323e")
 '(menu-bar-mode nil)
 '(mouse-wheel-progressive-speed 1)
 '(nrepl-message-colors
   (quote
    ("#CC9393" "#DFAF8F" "#F0DFAF" "#7F9F7F" "#BFEBBF" "#93E0E3" "#94BFF3" "#DC8CC3")))
 '(org-export-backends (quote (ascii beamer html icalendar latex md odt org)))
 '(package-selected-packages
   (quote
    (org-fancy-priorities org-num occidental-theme obsidian-theme dakrone-theme badger-theme arc-dark-theme ample-theme alect-themes w3m clipetty clippety ddg circe lui websocket org-gcal oauth2 default-text-scale org-ehtml ox-twbs ox-minutes ox-epub ox-clip ox-asciidoc ox-pandoc ox-jira ox-slack org-alert org-pdfview org-jira org-beautify-theme color-theme-modern tangotango-theme leuven-theme zenburn-theme abyss-theme ox-odt highlight-indent-guides multi-term dash-functional ox-confluence htmlize ox-md ox-markdown ob-clojure expand-region powerline writeroom-mode pandoc pandoc-mode groovy-mode kubernetes k8s-mode dockerfile-mode nov markdown-mode jira-markup-mode yaml-mode cider magit ace-window bbdb-vcard bbdb-csv-import bbdb-ext bbdb edit-server gruber-darker-theme quelpa use-package slime)))
 '(pdf-view-midnight-colors (quote ("#DCDCCC" . "#383838")))
 '(safe-local-variable-values
   (quote
    ((epa-encrypt-to "marcus@pemer.io")
     (auto-revert-mode . 1))))
 '(show-paren-mode t)
 '(sql-database "atgprd")
 '(sql-oracle-login-params (quote (user password database)))
 '(tool-bar-mode nil)
 '(vc-annotate-background "#2B2B2B")
 '(vc-annotate-color-map
   (quote
    ((20 . "#BC8383")
     (40 . "#CC9393")
     (60 . "#DFAF8F")
     (80 . "#D0BF8F")
     (100 . "#E0CF9F")
     (120 . "#F0DFAF")
     (140 . "#5F7F5F")
     (160 . "#7F9F7F")
     (180 . "#8FB28F")
     (200 . "#9FC59F")
     (220 . "#AFD8AF")
     (240 . "#BFEBBF")
     (260 . "#93E0E3")
     (280 . "#6CA0A3")
     (300 . "#7CB8BB")
     (320 . "#8CD0D3")
     (340 . "#94BFF3")
     (360 . "#DC8CC3"))))
 '(vc-annotate-very-old-color "#DC8CC3")
 '(xterm-mouse-mode t))


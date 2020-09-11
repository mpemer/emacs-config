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
 
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default default default italic underline success warning error])
 '(ansi-color-names-vector
   ["black" "red3" "ForestGreen" "yellow3" "blue" "magenta3" "DeepSkyBlue" "gray50"])
 '(column-number-mode t)
 '(company-quickhelp-color-background "#4F4F4F")
 '(company-quickhelp-color-foreground "#DCDCCC")
 '(custom-enabled-themes (quote (monokai)))
 '(custom-safe-themes
   (quote
    ("0cd00c17f9c1f408343ac77237efca1e4e335b84406e05221126a6ee7da28971" "5f4dfda04fbf7fd55228266c8aab73953d3087cea7fd06dd7f8ff1e4a497c739" "e6e07c74bee845f48da5f46843a241d71d4ee3bf945fb506c05a08e5c872df3e" "047ec205dcb5edbb94b35800110150a6e41e6cc92c0ccfb2ed25ac3df94331a6" "41a5a10e5221572b54c478eba62b51c510277100c9553d198fb1f4d56c45c6da" "9ac11c78f208abf58e5b313a33147cbf209ad9dc9cb169bf82464b043b45ad7a" "018d40c4ffe70e1863259f0b4614c52bab0cf8e04aeff812b9236fbb184e282e" "9939e735844cb24144d29ddf03fadf86a2d455758afeeee30372258e8a6401bb" "0ea5731605469a81203311ad7b4b3db03d6d7249ce621e3f2793ae3613d165ed" "8530b2f7b281ea6f263be265dd8c75b502ecd7a30b9a0f28fa9398739e833a35" "abd7719fd9255fcd64f631664390e2eb89768a290ee082a9f0520c5f12a660a8" "a11043406c7c4233bfd66498e83600f4109c83420714a2bd0cd131f81cbbacea" "31772cd378fd8267d6427cec2d02d599eee14a1b60e9b2b894dd5487bd30978e" "9d9b2cf2ced850aad6eda58e247cf66da2912e0722302aaa4894274e0ea9f894" "ec0c9d1715065a594af90e19e596e737c7b2cdaa18eb1b71baf7ef696adbefb0" "76935a29af65f8c915b1b3b4f6326e2e8d514ca098bd7db65b0caa533979fc01" "880f541eabc8c272d88e6a1d8917fe743552f17cedd8f138fe85987ee036ad08" "1a2cde373eff9ffd5679957c7ecfc6249d353e1ee446d104459e73e924fe0d8a" "c51e302edfe6d2effca9f7c9a8a8cfc432727efcf86246002a3b45e290306c1f" "ff6a8955945028387ed1a2b0338580274609fbb0d40cd011b98ca06bd00d9233" "5e402ccb94e32d7d09e300fb07a62dc0094bb2f16cd2ab8847b94b01b9d5e866" "f831c1716ebc909abe3c851569a402782b01074e665a4c140e3e52214f7504a0" "fe349b21bb978bb1f1f2db05bc87b2c6d02f1a7fe3f27584cd7b6fbf8e53391a" "fc1137ae841a32f8be689e0cfa07c872df252d48426a47f70dba65f5b0f88ac4" "09feeb867d1ca5c1a33050d857ad6a5d62ad888f4b9136ec42002d6cdf310235" "9dc64d345811d74b5cd0dac92e5717e1016573417b23811b2c37bb985da41da2" "595099e6f4a036d71de7e1512656e9375dd72cf60ff69a5f6d14f0171f1de9c1" "b4fd44f653c69fb95d3f34f071b223ae705bb691fb9abaf2ffca3351e92aa374" "5c83b15581cb7274085ba9e486933062652091b389f4080e94e4e9661eaab1aa" "ed92c27d2d086496b232617213a4e4a28110bdc0730a9457edf74f81b782c5cf" "5eb4b22e97ddb2db9ecce7d983fa45eb8367447f151c7e1b033af27820f43760" "b8c5adfc0230bd8e8d73450c2cd4044ad7ba1d24458e37b6dec65607fc392980" "4c8372c68b3eab14516b6ab8233de2f9e0ecac01aaa859e547f902d27310c0c3" "77515a438dc348e9d32310c070bfdddc5605efc83671a159b223e89044e4c4f1" "392f19e7788de27faf128a6f56325123c47205f477da227baf6a6a918f73b5dc" "0c5204945ca5cdf119390fe7f0b375e8d921e92076b416f6615bbe1bd5d80c88" "1a094b79734450a146b0c43afb6c669045d7a8a5c28bc0210aba28d36f85d86f" "6213a6047cc19f580c37ef3f6d47fd5a55ebdf9b5590475d8f7a6aecd79a1cc0" "a621dd9749f2651e357a61f8d8d2d16fb6cacde3b3784d02151952e1b9781f05" "6291d73aaeb6a3d7e455d85ca3865260a87afe5f492b6d0e2e391af2d3ea81dd" "4e7e04c4b161dd04dc671fb5288e3cc772d9086345cb03b7f5ed8538905e8e27" "701b4b4e7989329a0704b92fc17e6600cc18f9df4f2466617ec91c932b5477eb" "ff8c6c2eb94e776c9eed9299a49e07e70e1b6a6f926dec429b99cf5d1ddca62a" "b71da830ae97a9b70d14348781494b6c1099dbbb9b1f51494c3dfa5097729736" "70b2d5330a8dd506accac4b51aaa7e43039503d000852d7d152aec2ce779d96d" "0973b33d2f15e6eaf88400eee3dc8357ad8ae83d2ca43c125339b25850773a70" "deb7ae3a735635a85c984ece4ce70317268df6027286998b0ea3d10f00764c9b" "e26e879d250140e0d4c4d5ab457c32bcb29742599bd28c1ce31301344c6f2a11" "cdc2a7ba4ecf0910f13ba207cce7080b58d9ed2234032113b8846a4e44597e41" "a02c000c95c43a57fe1ed57b172b314465bd11085faf6152d151385065e0e4b1" "0e8c264f24f11501d3f0cabcd05e5f9811213f07149e4904ed751ffdcdc44739" "fb09acc5f09e521581487697c75b71414830b1b0a2405c16a9ece41b2ae64222" "72c530c9c8f3561b5ab3bf5cda948cd917de23f48d9825b7a781fe1c0d737f2f" "d8a7a7d2cffbc55ec5efbeb5d14a5477f588ee18c5cddd7560918f9674032727" "1a212b23eb9a9bedde5ca8d8568b1e6351f6d6f989dd9e9de7fba8621e8ef82d" "4639288d273cbd3dc880992e6032f9c817f17c4a91f00f3872009a099f5b3f84" "713f898dd8c881c139b62cf05b7ac476d05735825d49006255c0a31f9a4f46ab" "f3ab34b145c3b2a0f3a570ddff8fabb92dafc7679ac19444c31058ac305275e1" default)))
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
 '(org-tags-column 80)
 '(package-selected-packages
   (quote
    (monokai-theme darkokai-theme afternoon-theme org-num w3m clipetty ddg circe lui websocket org-gcal oauth2 default-text-scale org-ehtml ox-twbs ox-minutes ox-epub ox-clip ox-asciidoc ox-pandoc ox-jira ox-slack org-alert org-pdfview org-jira ox-odt highlight-indent-guides multi-term dash-functional ox-confluence htmlize ox-md ox-markdown ob-clojure expand-region powerline writeroom-mode pandoc pandoc-mode groovy-mode kubernetes k8s-mode dockerfile-mode nov markdown-mode jira-markup-mode yaml-mode cider magit ace-window bbdb-vcard bbdb-csv-import bbdb-ext bbdb edit-server quelpa use-package slime)))
 '(pdf-view-midnight-colors (quote ("#DCDCCC" . "#383838")))
 '(safe-local-variable-values
   (quote
    ((epa-encrypt-to "marcus@pemer.io")
     (auto-revert-mode . 1))))
 '(show-paren-mode t)
 '(sql-database "atgprd")
 '(sql-oracle-login-params (quote (user password database)))
 '(tool-bar-mode nil)
 '(xterm-mouse-mode t))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :stipple nil :background "#202115" :foreground "#F8F8F2" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 112 :width normal :foundry "Bits" :family "Bitstream Vera Sans Mono"))))
 '(org-level-1 ((t (:inherit default :foreground "seashell4" :height 1.3)))))

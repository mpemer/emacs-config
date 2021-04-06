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
   ["#272822" "#F92672" "#A6E22E" "#E6DB74" "#66D9EF" "#FD5FF0" "#A1EFE4" "#F8F8F2"])
 '(column-number-mode t)
 '(compilation-message-face 'default)
 '(custom-enabled-themes '(gruber-darker use-package))
 '(custom-safe-themes
   '("ec38d891ab6805f112b47c18149190a50bf1e2967b6e9fc84453989b6f67b2f6" "42d116236094bbb32033705237877aa3e83d436649c92e541e941347f0a7e605" "c51e302edfe6d2effca9f7c9a8a8cfc432727efcf86246002a3b45e290306c1f" "1048305cc1ac0f9088768641fb1c64756ee82b9d0b088b2950f6bae961dccc19" "499c3dd62c262b0bbb3ea0f5c83e92db5eac4a2a58468b51900e0ca706a7ad12" "ded82bed6a96cb8fdc7a547ef148679e78287664a5236e9c694e917383b052d7" "2ea9afebc23cca3cd0cd39943b8297ce059e31cb62302568b8fa5c25a22db5bc" "c29d6bc4bb6ee61149ce0fce6bc2bea539dcab11f0fb7bb0f1e2d8f5fb817a8e" "45482e7ddf47ab1f30fe05f75e5f2d2118635f5797687e88571842ff6f18b4d5" "14afbe72aeadc67fb3061ddaaaad924c58b45dc6c604fa31675222d851b70161" "dbcaaf13f0d62b577dedefe6e1a9ae3e57ac86f316fea6ae83341db916fcb9c7" "57db540d6a8cc20d2e2f20bd63dc3af4eb9e4bbfa7252a0ee877c99b577996c4" "7c83927617afdf71cc90801ffee5965939505b7cfecdcbe15961fc32a0d9bd02" "026eff18f1c0fda8f9bf5738343019a8ebe8db55c1bd9bcbd4536d8ed112d5df" "9685cefcb4efd32520b899a34925c476e7920725c8d1f660e7336f37d6d95764" "b71da830ae97a9b70d14348781494b6c1099dbbb9b1f51494c3dfa5097729736" "06e0662b31a2ae8da5c6b5e9a05b25fabd1dc8dd3c3661ac194201131cafb080" "cdc2a7ba4ecf0910f13ba207cce7080b58d9ed2234032113b8846a4e44597e41" "7de92d9e450585f9f435f2d9b265f34218cb235541c3d0d42c154bbbfe44d4dd" "c02b12444e027c332e58a7fb46ffd870df9e00123fd61e675288439b22c3c2a1" "0ec7094cc0a201c1d6f7c37f2414595d6684403b89b6fd74dcc714b5d41cd338" "087af241cc83130395244db7fca84b545fef75da92d63462436f3ffef916fa33" "5516001c13a43f1f5be2d7df8035445f8d7c73179f637e40c1503afb184d98f2" "6e03b7f86fcca5ce4e63cda5cd0da592973e30b5c5edf198eddf51db7a12b832" "67b11ee5d10f1b5f7638035d1a38f77bca5797b5f5b21d16a20b5f0452cbeb46" "3fe4861111710e42230627f38ebb8f966391eadefb8b809f4bfb8340a4e85529" "58ef0d9d877d9b1c19183f4215de272fa2938744f077e3a69ea12b8b6e852155" "d9c957b0e8d2d7f1bbb781fc729e06598017ade2d0c18611e5abbdde0f65d981" "a4ef58c2fb31b065ad09fa3029adba5eb518e42ef104cf9acf7c409abf40ca55" "1319bfcf42cefb12fad274e5763d0eae0d0401cddc9738bdf220fe89447e9292" "8c867d38498653a86c1071714502d01eabb8d8c9ec976a7ddc4d04c23a991a97" "6213a6047cc19f580c37ef3f6d47fd5a55ebdf9b5590475d8f7a6aecd79a1cc0" "9939e735844cb24144d29ddf03fadf86a2d455758afeeee30372258e8a6401bb" "9ac11c78f208abf58e5b313a33147cbf209ad9dc9cb169bf82464b043b45ad7a" "20ad8133a73088c0ce4f26d106c3e70cae4e10c7e613c9b9e17a962eb194a24f" "c3fa63eab93d1f0b4bf9f60a98a2848ba29c34cc6f2ef5cf4076d9c190a47a6c" "5c5de678730ceb4e05794431dd65f30ffe9f1ed6c016fa766cdf909ba03e4df4" "be0efbaebc85494f3c1c06e320fd13a24abf485d5f221a90fe811cea9a39ed85" "5e769f0dc4c262f216c2a30ca8bf55ff2ebc164f779bd2f32ce989290dc13485" "e3d4bc76062df5a4b72d0ec8abd04233ea3d33a8418430a719bd533e0e7c36ee" "46f6f73fb47a2a19b6ee1a49781f835fd73a185674268d4e048acf6feac9c55d" "317a45f190eaa3ccf8af6168aa89112d9cb794f87f409bc7a0638edee20d07fd" "39a854967792547c704cbff8ad4f97429f77dfcf7b3b4d2a62679ecd34b608da" "10551f0a24d0ac97a109f02178e9e34b448ee12a52357911cf3362a6b249cae6" "dd7213b37f448685f41e28b83a497f78fdefeeef0d47531fc24e99f576a7a191" "34c1b320a9d35318ca01660d533eee299d538f5a0c505c076511493b0a4f093d" "294c4b6a955149c93945f901a284140df29963a57939e9ed2fc4ad79b3605080" "ff8be9ed2696bf7bc999423d909a603cb23a9525bb43135c0d256b0b9377c958" "fc6697788f00629cd01f4d2cc23f1994d08edb3535e4c0facef6b7247b41f5c7" "96998f6f11ef9f551b427b8853d947a7857ea5a578c75aa9c4e7c73fe04d10b4" "c48551a5fb7b9fc019bf3f61ebf14cf7c9cdca79bcb2a4219195371c02268f11" "987b709680284a5858d5fe7e4e428463a20dfabe0a6f2a6146b3b8c7c529f08b" "e0d42a58c84161a0744ceab595370cbe290949968ab62273aed6212df0ea94b4" "3cd28471e80be3bd2657ca3f03fbb2884ab669662271794360866ab60b6cb6e6" "3cc2385c39257fed66238921602d8104d8fd6266ad88a006d0a4325336f5ee02" "e9776d12e4ccb722a2a732c6e80423331bcb93f02e089ba2a4b02e85de1cf00e" "72a81c54c97b9e5efcc3ea214382615649ebb539cb4f2fe3a46cd12af72c7607" "58c6711a3b568437bab07a30385d34aacf64156cc5137ea20e799984f4227265" "3d5ef3d7ed58c9ad321f05360ad8a6b24585b9c49abcee67bdcbb0fe583a6950" "b3775ba758e7d31f3bb849e7c9e48ff60929a792961a2d536edec8f68c671ca5" "9b59e147dbbde5e638ea1cde5ec0a358d5f269d27bd2b893a0947c4a867e14c1" "d9646b131c4aa37f01f909fbdd5a9099389518eb68f25277ed19ba99adeb7279" "5f824cddac6d892099a91c3f612fcf1b09bb6c322923d779216ab2094375c5ee" default))
 '(darkroom-margins 0.2)
 '(debug-on-quit nil)
 '(diary-entry-marker 'font-lock-variable-name-face)
 '(display-time-mode t)
 '(emms-mode-line-icon-image-cache
   '(image :type xpm :ascent center :data "/* XPM */
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
\"#######..#\" };"))
 '(fci-rule-color "#3C3D37")
 '(font-use-system-font t)
 '(frame-brackground-mode 'dark)
 '(gnus-mode-line-image-cache
   '(image :type xpm :ascent center :data "/* XPM */
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
\"###########.######\" };") t)
 '(highlight-changes-colors '("#FD5FF0" "#AE81FF"))
 '(highlight-tail-colors
   '(("#3C3D37" . 0)
     ("#679A01" . 20)
     ("#4BBEAE" . 30)
     ("#1DB4D0" . 50)
     ("#9A8F21" . 60)
     ("#A75B00" . 70)
     ("#F309DF" . 85)
     ("#3C3D37" . 100)))
 '(linum-format " %7i ")
 '(magit-diff-use-overlays nil)
 '(menu-bar-mode nil)
 '(mouse-wheel-progressive-speed 1)
 '(org-export-backends '(ascii beamer html icalendar latex md odt org))
 '(org-tags-column 40)
 '(package-selected-packages
   '(graphviz-dot-mode zoom-window ox-hugo org-beautify-theme sublime-themes monokai-theme ivy mu4e-views mu4e flycheck-clj-kondo darkroom org-num w3m clipetty ddg circe lui websocket org-gcal oauth2 default-text-scale org-ehtml ox-twbs ox-minutes ox-epub ox-clip ox-asciidoc ox-pandoc ox-jira ox-slack org-alert org-pdfview org-jira ox-odt highlight-indent-guides multi-term dash-functional ox-confluence htmlize ox-md ox-markdown ob-clojure expand-region powerline writeroom-mode pandoc pandoc-mode groovy-mode kubernetes k8s-mode dockerfile-mode nov markdown-mode jira-markup-mode yaml-mode cider magit ace-window bbdb-vcard bbdb-csv-import bbdb-ext bbdb edit-server quelpa use-package slime))
 '(pos-tip-background-color "#FFFACE")
 '(pos-tip-foreground-color "#272822")
 '(safe-local-variable-values
   '((epa-encrypt-to "marcus@pemer.io")
     (auto-revert-mode . 1)))
 '(show-paren-mode t)
 '(sql-database "atgprd")
 '(sql-oracle-login-params '(user password database))
 '(tool-bar-mode nil)
 '(vc-annotate-background nil)
 '(vc-annotate-color-map
   '((20 . "#F92672")
     (40 . "#CF4F1F")
     (60 . "#C26C0F")
     (80 . "#E6DB74")
     (100 . "#AB8C00")
     (120 . "#A18F00")
     (140 . "#989200")
     (160 . "#8E9500")
     (180 . "#A6E22E")
     (200 . "#729A1E")
     (220 . "#609C3C")
     (240 . "#4E9D5B")
     (260 . "#3C9F79")
     (280 . "#A1EFE4")
     (300 . "#299BA6")
     (320 . "#2896B5")
     (340 . "#2790C3")
     (360 . "#66D9EF")))
 '(vc-annotate-very-old-color nil)
 '(weechat-color-list
   '(unspecified "#272822" "#3C3D37" "#F70057" "#F92672" "#86C30D" "#A6E22E" "#BEB244" "#E6DB74" "#40CAE4" "#66D9EF" "#FB35EA" "#FD5FF0" "#74DBCD" "#A1EFE4" "#F8F8F2" "#F8F8F0"))
 '(xterm-mouse-mode t)
 '(zoom-window-mode-line-color "DarkGreen"))

;;    (setf epa-encrypt-to "1D151FF890EE620251BC79A4E594D6C2CC9E1BAA")

(if (daemonp)
   (add-hook 'after-make-frame-functions
             (lambda (frame)
               (with-selected-frame frame
                 (set-cursor-color "#eebbaa"))))
 (set-cursor-color "#eebbaa"))
(provide '.emacs)
;;; .emacs ends here
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:background nil)))))

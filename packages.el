;; -*- no-byte-compile: t; -*-
;;; ~/.config/doom/packages.el

;; completion/company
(package! prescient)
(package! company-prescient)

;; completion/ivy
(package! all-the-icons-ivy)
(package! counsel-tramp)

;; emacs/dired
(package! peep-dired)
(package! diredfl)

;; feature/snippets
(package! yasnippet-snippets)

;; lang/apache
(package! apache-mode)

;; lang/pkgbuild
(package! pkgbuild-mode)

;; lang/nginx
(package! nginx-mode)

;; lang/org
(package! ob-http)

;; lang/systemd
(package! systemd)

;; lang/sh
(package! flycheck-checkbashisms)

;; tools/tldr
(package! tldr)

;; ui/doom
;; (package! doom-themes :recipe (:fetcher github :repo "brettm12345/emacs-doom-themes" :files ("*.el" "themes/*.el")))
;; (package! doom-palenight-theme :recipe (:fetcher github :repo "brettm12345/doom-palenight-theme"))

;; ui/modeline
(package! doom-modeline)
(package! anzu)
(package! evil-anzu)

(package! om :recipe (:repo "https://github.com/ndwarshuis/om.el.git"))

;; ui/indent-guides
(package! highlight-indent-guides)

(package! org-super-agenda)
(package! dired-quick-sort) ;; :recipe (:host gitlab :repo "xuhdev/dired-quick-sort"))
(package! org-starter)
(package! org-reverse-datetree)
(package! org-web-tools)

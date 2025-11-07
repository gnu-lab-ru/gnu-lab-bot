(use-modules (guix profiles))
(packages->manifest
 (map specification->package
      '("emacs"
        "emacs-minimal"
        "coreutils"
        "util-linux"
        "curl"
        "guile"
        "guix"
        "git"
        "bubblewrap"
        "sqlite"
        "make"
        "emacs-package-lint")))

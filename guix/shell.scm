(use-modules (guix profiles))
(packages->manifest
 (map specification->package
      '("emacs"
        "emacs-minimal"
        "coreutils"
        "util-linux"
        "curl"
        "guile"
        "git"
        "bubblewrap"
        "sqlite"
        "make")))

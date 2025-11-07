;; guix deploy machines spec
(list (machine
       (system "x86_64-linux")
       (host-name "example.org")
       (user "deploy")
       (identity "ssh-key")
       (configuration (local-file "guix/system.scm"))))

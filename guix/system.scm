;; Example Guix System configuration snippet
(use-modules (gnu) (guix))
(use-service-modules shepherd)
(use-package-modules)

(operating-system
  (host-name "gnu-lab-host")
  (timezone "Etc/UTC")
  (locale "en_US.utf8")
  (bootloader (bootloader-configuration (bootloader grub-efi-bootloader) (targets '("/boot/efi"))))
  (file-systems (cons (file-system (device "/dev/sda1") (mount-point "/") (type "ext4")) %base-file-systems))
  (services
   (append
    (list (service gnu-lab-bot-service-type '("./secrets/telegram.token")))
    %base-services)))

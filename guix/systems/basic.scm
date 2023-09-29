;; This is an operating system configuration generated
;; by the graphical installer.
;;
;; Once installation is complete, you can learn and modify
;; this file to tweak the system configuration, and pass it
;; to the 'guix system reconfigure' command to effect your
;; changes.

;; Indicate which modules to import to access the variables
;; used in this configuration.
(use-modules
  (gnu)
)
(use-service-modules
  cups
  desktop
  networking
  ssh
  xorg
)

(operating-system
  (locale "en_US.utf-8")
  (timezone "America/Argentina/Buenos_Aires")
  (host-name "guixvm")

  ;; The list of user accounts ('root' is implicit).
  (users (cons* (user-account
                  (name "user")
                  (comment "user")
                  (group "users")
                  (supplementary-groups '("wheel" "netdev" "audio" "video")))
                %base-user-accounts))

  ;; Packages installed system-wide. Users can also install packages
  ;; under their own account: use 'guix search KEYWORD' to search
  ;; for packages and 'guix install PACKAGE' to install a package.
  (packages (append (list (specification->package "i3-wm")
			  (specification->package "i3status")
			  (specification->package "dmenu")
			  (specification->package "st")
			  (specification->package "rxvt-unicode")
			  (specification->package "glibc-locales")
			  ;; Files that we need to use in the dotfiles
			  (specification->package "git")
			  (specification->package "unzip")
			  (specification->package "stow")
			  (specification->package "fish")
			  (specification->package "nss-certs"))
                    %base-packages))

  ;; Below is the list of system services. To search for available
  ;; services, run 'guix system search KEYWORD' in a terminal.
  (services
    (append (list

                  ;; To configure OpenSSH, pass an 'openssh-configuration'
		  ;; record as a second argument to 'service' below.
		  (service openssh-service-type)
		  (set-xorg-configuration
		    (xorg-configuration (keyboard-layout keyboard-layout))))

	    ;; This is the default list of services we
	    ;; are appending to.
            %desktop-services))

  ;; EFI bootloader
  (bootloader (bootloader-configuration
                 (bootloader grub-bootloader)
                 (targets (list "/dev/sda"))
		 ;;(keyboard-layout keyboard-layout)))

  ;; Specify a mapped device for the encrypted root partition.
  (mapped-devices (list (mapped-device
                           (source "/dev/sda3")
                           (target "cryptroot")
                           (type luks-device-mapping))))

    ;; The list of file systems that get "mounted". The unique
    ;; file system identifiers there ("UUIDs") can be obtained
    ;; by running 'blkid' in a terminal.
    (file-systems (cons* (file-system
                           (mount-point "/")
                           (device "/dev/mapper/cryptroot")
                           (type "ext4")
                           (dependencies mapped-devices))
            %base-file-systems)))


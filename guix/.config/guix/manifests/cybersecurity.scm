(define-module (home manifests cybersecurity)
  #:use-module (gnu packages))

(specifications->manifest
    '(
        ;; glibc-locales is always needed
        "glibc-locales"
        "netcat"
	"nmap"
	"radare2"
	"wireshark"))


#include /etc/firejail/firefox.profile

private
private-dev
private-tmp

caps.drop all
seccomp
nonewprivs

netfilter ~/.config/firejail/bna.net

#include /etc/firejail/firefox.profile

private-home .mozilla,.cache,.config
private-dev
private-tmp

private-bin firefox

caps.drop all
seccomp
nonewprivs

netfilter ~/.config/firejail/bna.net

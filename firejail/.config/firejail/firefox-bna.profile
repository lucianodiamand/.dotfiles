#include /etc/firejail/firefox.profile

private-home .mozilla,.cache,.config
private-dev
private-tmp

whitelist /run/current-system
whitelist /nix/store

caps.drop all
seccomp
nonewprivs

netfilter ~/.config/firejail/bna.net

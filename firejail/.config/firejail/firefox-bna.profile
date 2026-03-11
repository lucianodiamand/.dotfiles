#include /etc/firejail/firefox.profile

private
private-dev
private-tmp

whitelist /home/user/.nix-profile

caps.drop all
seccomp
nonewprivs

netfilter ~/.config/firejail/bna.net

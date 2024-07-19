#!/bin/bash

PRESEED_SERVER=http://192.168.0.187
POSTINSTALL_DIR=/tmp/post-install
OS_ARCH=`uname -m`
INSTANT_CLIENT='instantclient-basic-linux.x64-23.4.0.24.05.zip'

function create-user-dirs() {
	rm -rf $POSTINSTALL_DIR > /dev/null
	mkdir -p $POSTINSTALL_DIR
	mkdir -p /home/user/dev
	mkdir -p /home/user/documents
	mkdir -p /home/user/downloads
	mkdir -p /home/user/bin
}

function apt-init() {
	sudo rm -r /var/lib/apt/lists/* 
	#curl $PRESEED_SERVER/files/etc/apt/apt.conf.d/00InstallRecommends > $POSTINSTALL_DIR/00InstallRecommends
	#sudo cp $POSTINSTALL_DIR/00InstallRecommends /etc/apt/apt.conf.d/00InstallRecommends
	#sudo chown root:root /etc/apt/apt.conf.d/00InstallRecommends
	sudo apt-get update
	sudo apt-get -y autoremove
}

function apt-init-remove() {
	sudo apt-get -y remove --purge \
		nano \
		task-english \
		ispell \
		wamerican \
		ienglish-common \
		iamerican \
		ibritish \
		dictionaries-common \
		util-linux-locales \
		vim-tiny \
		vim-common \
		manpages
	sudo apt-get update
	sudo apt-get -y autoremove
}

function apt-init-install() {
	LINUX_HEADERS=linux-headers-686-pae
	if [ ${OS_ARCH} == 'x86_64' ]; then
		LINUX_HEADERS=linux-headers-amd64
	fi
}

function guix-init() {
	#sudo rm -r /var/lib/apt/lists/* 
	#curl $PRESEED_SERVER/files/etc/apt/apt.conf.d/00InstallRecommends > $POSTINSTALL_DIR/00InstallRecommends
	#sudo cp $POSTINSTALL_DIR/00InstallRecommends /etc/apt/apt.conf.d/00InstallRecommends
	#sudo chown root:root /etc/apt/apt.conf.d/00InstallRecommends
	sudo -i guix pull
	sudo service guix-daemon restart
        sudo -i guix install glibc-locales
	sudo -i guix install unzip

	guix pull
	guix upgrade
}

function guix-init-install() {
	guix install glibc-locales
	guix install git
	guix install stow
	
	guix install password-store
	guix install pass-otp
	guix install zbar
	guix install gnupg
	guix install pinentry-tty
	guix install meld
	guix install docker
	guix install docker-cli
	guix install bat
	guix install tig
	guix install zoxide
	guix install unzip

	guix install neovim
	guix install emacs
	guix install ed

	guix install fish

	guix install eza
	guix install fzf
	guix install ripgrep
	guix install xclip

	guix install curl

	guix install openfortivpn

	guix install gcc-toolchain
	guix install luarocks
	guix install lua@5.1.5

	guix install openjdk@21.0.2:jdk
	guix install openjdk@21.0.2:doc
	guix install openjdk@21.0.2:out

	guix install maven
	guix install clang

	guix install mysql

	guix install vimb
}

function setup-home() {
	cat >> /home/user/.bashrc << EOF

GUIX_PROFILE="/home/user/.config/guix/current"
. "$GUIX_PROFILE/etc/profile"
EOF

	source "$GUIX_PROFILE/etc/profile"

	git clone https://github.com/lucianodiamand/.dotfiles.git
	mv /home/user/.config/i3/config /home/user/.config/i3/config.bak
	cd /home/user/.dotfiles
	stow i3
	stow i3status
	stow alacritty
	stow nvim
	stow vimb
	stow emacs
}

function install-nodejs() {
	#curl -sL https://deb.nodesource.com/setup_7.x | sudo -E bash -
	#sudo apt-get install -y nodejs
	# nvm install
	curl -o- https://raw.githubusercontent.com/nvm-sh/nvm/v0.39.7/install.sh | bash
	nvm install v20
	nvm use v20
}

function apt-finish() {
	sudo apt-get update
	sudo apt-get -y upgrade
	sudo apt-get -y dist-upgrade
	sudo apt-get -y autoremove
}

function install-fonts() {
	# Nerd fonts instalation
	wget -c https://github.com/ryanoasis/nerd-fonts/releases/download/v3.2.1/Hack.zip -P /tmp
        # Create destination folder
	mkdir -p /home/user/.local/share/fonts
	unzip /tmp/Hack.zip -d /home/user/.local/share/fonts
	fc-cache -fv
}

function enable-color-prompt() {
	sed -i -e 's/#force_color_prompt=yes/force_color_prompt=yes/g' /home/user/.bashrc
}

function restore-user-profile() {
        echo restore-user-profile
	#sudo rm -rf /home/user/.config > /dev/null
	#curl $PRESEED_SERVER/files/backups/user-profile.zip > $POSTINSTALL_DIR/user-profile.zip
	#unzip -u -q $POSTINSTALL_DIR/user-profile.zip -d /home/user1
	#icon_files=( exo-terminal-emulator eclipse palemoon )
	#for i in "${icon_files[@]}"
	#do
	#	chmod +x /home/user1/Desktop/$i.desktop
	#done
}

function enable-auto-login() {
	echo enable-auto-login
	# http://forums.debian.net/viewtopic.php?f=16&t=123694
	#curl $PRESEED_SERVER/files/etc/systemd/system/getty@tty1.service.d/override.conf > $POSTINSTALL_DIR/autologin-override.conf
	#sudo mkdir -p /etc/systemd/system/getty@tty1.service.d
	#sudo cp $POSTINSTALL_DIR/autologin-override.conf /etc/systemd/system/getty@tty1.service.d/override.conf
	#sudo chown root:root /etc/systemd/system/getty@tty1.service.d/override.conf
	#sudo systemctl set-default multi-user.target
	#printf "\n\n[[ -z \$DISPLAY && \$XDG_VTNR -eq 1 ]] && exec startx\n\n" >> /home/user1/.profile
}

function install-oracle-client() {
	wget -c https://download.oracle.com/otn_software/linux/instantclient/2340000/${INSTANT_CLIENT} -P /tmp
	sudo mkdir -p /opt/oracle
	sudo -i unzip /tmp/${INSTANT_CLIENT} -d /opt/oracle
}

create-user-dirs
apt-init
apt-init-remove
apt-init-install
guix-init
guix-init-install
setup-home
install-nodejs
apt-finish
install-fonts
enable-color-prompt
restore-user-profile
restore-desktop-icons
enable-auto-login
install-oracle-client

sudo rm /home/user/post-install.sh > /dev/null
history -cw

sudo reboot


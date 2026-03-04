#!/bin/sh
# Postinstall FreeBSD para entorno i3 + dotfiles + ssh + nerd fonts

set -e

# Variables
USER_NAME="user"   # CAMBIAR por tu username real si no es "user"
DOTFILES_REPO="https://github.com/lucianodiamand/.dotfiles.git"
DOTFILES_DIR="/home/$USER_NAME/.dotfiles"

# Actualizar sistema
echo "==> Actualizando pkg"
pkg update
pkg upgrade -y

# Instalar paquetes básicos
echo "==> Instalando paquetes básicos"
pkg install -y \
  i3 i3status dmenu xorg xinit xrandr xset xsetroot \
  rxvt-unicode urxvt-perls urxvt-font-size \
  i3lock \
  cups cups-filters \
  alsa-utils pulseaudio pulseaudio-utils \
  git tig meld \
  bat \
  zoxide \
  stow \
  tldr \
  exa \
  fzf \
  ripgrep \
  xclip \
  wipe \
  fdupes \
  unzip \
  smartmontools \
  neovim \
  emacs-devel \
  tmux \
  firefox \
  libreoffice \
  zathura zathura-pdf-poppler \
  ghostscript \
  curl \
  gcc gmake cmake ninja pkgconf \
  python3 py38-venv \
  node npm \
  openssh-portable \
  sudo \
  security/ssh-copy-id

# Instalar NVM
echo "==> Instalando NVM"
su - "$USER_NAME" -c '
  export NVM_DIR="$HOME/.nvm"
  git clone https://github.com/nvm-sh/nvm.git "$NVM_DIR"
  cd "$NVM_DIR" && git checkout v0.39.7
  . "$NVM_DIR/nvm.sh"
  nvm install --lts
'

# Instalar Oh My Zsh
echo "==> Instalando Oh My Zsh"
su - "$USER_NAME" -c '
  if [ ! -d "$HOME/.oh-my-zsh" ]; then
    sh -c "$(curl -fsSL https://raw.githubusercontent.com/ohmyzsh/ohmyzsh/master/tools/install.sh)" "" --unattended
  fi
  chsh -s $(which zsh)
'

# Instalar Nerd Fonts
echo "==> Instalando Nerd Fonts (Hack)"
su - "$USER_NAME" -c '
  fetch -o /tmp/Hack.zip https://github.com/ryanoasis/nerd-fonts/releases/download/v3.3.0/Hack.zip
  mkdir -p $HOME/.local/share/fonts
  unzip -o /tmp/Hack.zip -d $HOME/.local/share/fonts
  fc-cache -fv
'

# Grupos
echo "==> Agregando usuario a grupos wheel y dialer"
pw groupmod wheel -m "$USER_NAME"
pw groupmod dialer -m "$USER_NAME"

# Git config
echo "==> Configurando Git global"
su - "$USER_NAME" -c '
  git config --global user.name "Luciano Daniel Diamand"
  git config --global user.email "lucianodiamand@gmail.com"
'

# Crear /home/user/dev/src
su - "$USER_NAME" -c 'mkdir -p $HOME/dev/src'

# Crear SSH keys y config
echo "==> Generando claves SSH"
su - "$USER_NAME" -c '
  mkdir -p $HOME/.ssh
  chmod 700 $HOME/.ssh

  ssh-keygen -t ed25519 -f $HOME/.ssh/hostinger.server.key -C "Hostinger vps server"
  ssh-keygen -t ed25519 -f $HOME/.ssh/cpt-prod.server.key -C "Donweb vps server"
  ssh-keygen -t ed25519 -f $HOME/.ssh/github.personal.repo.key -C "Personal github repo"
  ssh-keygen -t ed25519 -f $HOME/.ssh/bitbucket.personal.repo.key -C "Personal bitbucket repo"
  ssh-keygen -t ed25519 -f $HOME/.ssh/bitbucket.l2.repo.key -C "L2 bitbucket repo"
  ssh-keygen -t ed25519 -f $HOME/.ssh/proxmox.local.server.key -C "Personal proxmox server"

  cat << EOF > $HOME/.ssh/config
# Hosts

Host hostinger
  HostName 82.180.131.121
  User root
  IdentityFile ~/.ssh/hostinger.server.key

Host cptprod
  HostName 179.43.117.86
  User root
  IdentityFile ~/.ssh/cpt-prod.server.key
  Port 4626

Host github.com
  IdentityFile ~/.ssh/github.personal.repo.key
  
Host bitbucket-personal
  HostName bitbucket.org
  IdentityFile ~/.ssh/bitbucket.personal.repo.key
  IdentitiesOnly yes

Host bitbucket-l2
  HostName bitbucket.org
  IdentityFile ~/.ssh/bitbucket.l2.repo.key
  IdentitiesOnly yes

Host proxmox
  HostName 10.10.10.20
  IdentityFile ~/.ssh/proxmox.local.server.key
  User root
EOF

  chmod 600 $HOME/.ssh/config
'

# ssh-copy-id a proxmox
echo "==> Copiando clave SSH a proxmox"
su - "$USER_NAME" -c '
  ssh-copy-id -o IdentitiesOnly=yes -i $HOME/.ssh/proxmox.local.server.key.pub root@10.10.10.20
'

# Clonar dotfiles y aplicar stow
echo "==> Clonando dotfiles y aplicando stow"
su - "$USER_NAME" -c '
  git clone '"$DOTFILES_REPO"' $HOME/.dotfiles
  cd $HOME/.dotfiles
  stow i3
  stow i3status
  $HOME/.dotfiles/i3status/.config/i3status/generate.sh
  stow rxvt
  stow nvim
  stow emacs
'

# Crear dirs de mail
su - "$USER_NAME" -c '
  mkdir -p $HOME/.mail/{ips,personal-gmail,thelabtech,yahoo,frro,fceia}
'

# Configurar devd para Android
echo "==> Configurando devd para Android"
cat << EOF > /usr/local/etc/devd/android.conf
notify 100 {
    match "system"          "USB";
    match "subsystem"       "DEVICE";
    match "type"            "ATTACH";
    match "vendor"          "0x18d1";
    action "chmod 0666 /dev/\$cdev";
};

notify 101 {
    match "system"          "USB";
    match "subsystem"       "DEVICE";
    match "type"            "ATTACH";
    match "vendor"          "0x0fce";
    action "chmod 0666 /dev/\$cdev";
};

notify 102 {
    match "system"          "USB";
    match "subsystem"       "DEVICE";
    match "type"            "ATTACH";
    match "vendor"          "0x22b8";
    action "chmod 0666 /dev/\$cdev";
};
EOF

service devd restart

# Configurar ejemplo de autorandr (xrandr script)
echo "==> Configurando ejemplo de autorandr (xrandr)"
su - "$USER_NAME" -c '
  mkdir -p $HOME/.screenlayout
  cat << EOFRANDR > $HOME/.screenlayout/default.sh
#!/bin/sh
xrandr --output eDP-1 --primary --mode 1920x1080 --rate 60
EOFRANDR
  chmod +x $HOME/.screenlayout/default.sh
'

# Configurar .Xresources para urxvt
echo "==> Configurando .Xresources para urxvt + Nerd Fonts"
su - "$USER_NAME" -c '
  cat << EOFRES > $HOME/.Xresources
URxvt.font: xft:Hack Nerd Font Mono:pixelsize=14
URxvt.perl-ext-common: default,resize-font
URxvt.keysym.C-Up: resize-font:increase
URxvt.keysym.C-Down: resize-font:decrease
EOFRES

  xrdb -merge $HOME/.Xresources
'

# Recordatorio de .xinitrc
echo "==> Recordatorio: agregar en ~/.xinitrc:"
echo '
xrdb -merge ~/.Xresources
~/.screenlayout/default.sh &
exec i3
'

echo "==> Postinstall terminado correctamente!"


#!/usr/bin/env bash
set -euo pipefail

# Ajustá si querés
HOSTNAME="${HOSTNAME:-vnixos}"
TIMEZONE="${TIMEZONE:-America/Argentina/Buenos_Aires}"
LOCALE="${LOCALE:-en_US.UTF-8}"

# Detectar disco principal.
# En esta VM normalmente va a ser /dev/sda con virtio-scsi-pci,
# pero preferimos by-id si existe.
DISK="$(readlink -f /dev/disk/by-id/scsi-0QEMU_QEMU_HARDDISK_drive-scsi0 2>/dev/null || true)"
if [ -z "${DISK}" ]; then
  DISK="/dev/sda"
fi

echo "Usando disco: ${DISK}"
lsblk "${DISK}"

read -r -p "Esto va a borrar COMPLETAMENTE ${DISK}. Escribí YES para continuar: " CONFIRM
if [ "${CONFIRM}" != "YES" ]; then
  echo "Abortado."
  exit 1
fi

swapoff -a || true
umount -R /mnt 2>/dev/null || true

echo "Particionando..."
parted -s "${DISK}" -- mklabel gpt
parted -s "${DISK}" -- mkpart ESP fat32 1MiB 512MiB
parted -s "${DISK}" -- set 1 esp on
parted -s "${DISK}" -- mkpart primary ext4 512MiB 100%

ESP_PART="${DISK}1"
ROOT_PART="${DISK}2"

# Para discos NVMe el nombre sería nvme0n1p1 / nvme0n1p2
if [[ "${DISK}" == *"nvme"* ]]; then
  ESP_PART="${DISK}p1"
  ROOT_PART="${DISK}p2"
fi

echo "Formateando..."
mkfs.fat -F 32 -n EFI "${ESP_PART}"
mkfs.ext4 -L nixos "${ROOT_PART}"

echo "Montando..."
mount "${ROOT_PART}" /mnt
mkdir -p /mnt/boot
mount "${ESP_PART}" /mnt/boot

echo "Generando configuración..."
nixos-generate-config --root /mnt

cat > /mnt/etc/nixos/configuration.nix <<EOF
{ config, pkgs, lib, ... }:

{
  imports = [
    ./hardware-configuration.nix
  ];

  system.stateVersion = "25.11";

  networking.hostName = "${HOSTNAME}";
  networking.useDHCP = lib.mkDefault true;

  time.timeZone = "${TIMEZONE}";
  i18n.defaultLocale = "${LOCALE}";

  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;

  services.qemuGuest.enable = true;
  services.openssh.enable = true;

  users.users.root.initialPassword = "nixos";

  environment.systemPackages = with pkgs; [
    vim
    git
    curl
    htop
  ];

  nix.settings.experimental-features = [ "nix-command" "flakes" ];
}
EOF

echo "Instalando NixOS..."
nixos-install --no-root-password

echo
echo "Instalación terminada."
echo "Ahora apagá la VM, quitá la ISO y dejá boot desde scsi0."
echo
echo "Usuario root con password temporal: nixos"

#!/usr/bin/env bash
set -euo pipefail

# Uso:
#   ./crear-vm-nixos.sh <VMID> <NOMBRE> [RAM_MB] [CORES] [DISK_GB]
#
# Ejemplo:
#   ./crear-vm-nixos.sh 900 nixos-test 4096 2 32

VMID="${1:-130}"
NAME="${2:-nixos-vm}"
RAM="${3:-4096}"
CORES="${4:-2}"
DISK_GB="${5:-32}"

# Ajustá estos valores a tu entorno
BRIDGE="vmbr0"
DISK_STORAGE="local-lvm"
ISO_STORAGE="nfs-server"
ISO_FILE="iso/nixos-minimal-25.11.7421.0590cd39f728-x86_64-linux.iso"

echo "Creando VM ${VMID} (${NAME})..."

# Si ya existe, aborta
if qm status "${VMID}" >/dev/null 2>&1; then
  echo "Error: la VM ${VMID} ya existe."
  exit 1
fi

qm create "${VMID}" \
  --name "${NAME}" \
  --memory "${RAM}" \
  --cores "${CORES}" \
  --cpu x86-64-v2-AES \
  --machine q35 \
  --bios ovmf \
  --agent 1 \
  --serial0 socket \
  --vga std \
  --net0 virtio,bridge="${BRIDGE}" \
  --scsihw virtio-scsi-pci

# Disco EFI para UEFI
qm set "${VMID}" \
  --efidisk0 "${DISK_STORAGE}:1,efitype=4m,pre-enrolled-keys=0"

# Disco principal
qm set "${VMID}" \
  --scsi0 "${DISK_STORAGE}:${DISK_GB},discard=on,ssd=1"

# ISO de instalación
qm set "${VMID}" \
  --ide2 "${ISO_STORAGE}:${ISO_FILE}",media=cdrom

# Orden de booteo: primero ISO, luego disco
qm set "${VMID}" \
  --boot "order=ide2;scsi0"

# Opcional pero útil
qm set "${VMID}" \
  --ostype l26

echo
echo "VM creada."
echo "Iniciando..."
qm start "${VMID}"

echo
echo "Abrí la consola de Proxmox."
echo "Cuando bootee la ISO mínima de NixOS, instalá con el script del paso 2."


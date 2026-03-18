#!/usr/bin/env bash
set -euo pipefail

# Usage:
#   ./create-nixos-vm.sh <VMID> <NOMBRE> [RAM_MB] [CORES] [DISK_GB]
#
# Example:
#   ./create-nixos-vm.sh 900 nixos-test 4096 2 32

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

echo "Creating VM ${VMID} (${NAME})..."

if qm status "${VMID}" >/dev/null 2>&1; then
  echo "Error: the VM ${VMID} already exists."
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
  --vga std \
  --net0 virtio,bridge="${BRIDGE}" \
  --scsihw virtio-scsi-pci

qm set "${VMID}" \
  --efidisk0 "${DISK_STORAGE}:1,efitype=4m,pre-enrolled-keys=0"

qm set "${VMID}" \
  --scsi0 "${DISK_STORAGE}:${DISK_GB},discard=on,ssd=1"

qm set "${VMID}" \
  --ide2 "${ISO_STORAGE}:${ISO_FILE}",media=cdrom

qm set "${VMID}" \
  --boot "order=ide2;scsi0"

qm set "${VMID}" \
  --ostype l26

SNAPSHOT_NAME="fresh-vm"

echo
echo "VM created."
echo "Creating snapshot ${SNAPSHOT_NAME}..."
qm snapshot "${VMID}" "${SNAPSHOT_NAME}"

echo "Initializing..."
qm start "${VMID}"

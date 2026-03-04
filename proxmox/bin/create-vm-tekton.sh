#!/bin/bash
set -e

# === CONFIGURACIÓN GENERAL ===
CLOUD_IMG_URL="https://cloud.debian.org/images/cloud/bookworm/latest/debian-12-genericcloud-amd64.qcow2"
CLOUD_IMG_FILE="debian-12-genericcloud-amd64.qcow2"
TEMPLATE_ID=9010
TEMPLATE_NAME="tekton-poc"
TEMPLATE_MEMORY=12288
STORAGE="local-lvm"
BRIDGE="vmbr0"
CSV_FILE="vms.csv"

# === Crear plantilla si no existe ===
if ! qm status $TEMPLATE_ID &>/dev/null; then
  echo ">>> Creando plantilla VM $TEMPLATE_NAME ($TEMPLATE_ID)..."
  qm create $TEMPLATE_ID --name $TEMPLATE_NAME --memory $TEMPLATE_MEMORY --cores 4 --net0 virtio,bridge=$BRIDGE
  qm importdisk $TEMPLATE_ID "$CLOUD_IMG_FILE" $STORAGE
  qm set $TEMPLATE_ID --scsihw virtio-scsi-pci --scsi0 $STORAGE:vm-${TEMPLATE_ID}-disk-0
  qm set $TEMPLATE_ID --agent enabled=1
  qm set $TEMPLATE_ID --ide2 $STORAGE:cloudinit
  qm set $TEMPLATE_ID --boot c --bootdisk scsi0
  qm set $TEMPLATE_ID --serial0 socket --vga serial0
  qm template $TEMPLATE_ID
else
  echo ">>> Plantilla $TEMPLATE_ID ya existe."
fi


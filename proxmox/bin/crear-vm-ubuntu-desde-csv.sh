#!/bin/bash
set -e

# === CONFIGURACIÓN GENERAL ===
CLOUD_IMG_URL="https://cloud-images.ubuntu.com/jammy/current/jammy-server-cloudimg-amd64.img"
CLOUD_IMG_FILE="ubuntu-22.04-cloudimg-amd64.img"
TEMPLATE_ID=9100
TEMPLATE_NAME="ubuntu2204-cloud"
STORAGE="local-lvm"
BRIDGE="vmbr0"
CSV_FILE="vms.csv"

# === SSH Key ===
if [ ! -f ~/.ssh/id_rsa.pub ]; then
  echo ">>> Generando clave SSH..."
  ssh-keygen -t rsa -N "" -f ~/.ssh/id_rsa
fi
SSH_KEY_PATH="$HOME/.ssh/id_rsa.pub"

# === Verificar imagen ===
if [ ! -f "$CLOUD_IMG_FILE" ]; then
  echo ">>> Descargando imagen cloud-init de Ubuntu 22.04..."
  wget "$CLOUD_IMG_URL" -O "$CLOUD_IMG_FILE"
else
  echo ">>> Imagen $CLOUD_IMG_FILE ya existe."
fi

# === Verificar que virt-customize esté disponible ===
if ! command -v virt-customize &>/dev/null; then
  echo "ERROR: virt-customize no está instalado. Ejecutá: apt install libguestfs-tools"
  exit 1
fi

# === Verificar si cloud-init ya está instalado en la imagen (simple heurística) ===
if ! strings "$CLOUD_IMG_FILE" | grep -q cloud-init; then
  echo ">>> Instalando cloud-init y guest agent dentro de la imagen .qcow2 con virt-customize..."
  virt-customize -a "$CLOUD_IMG_FILE" \
    --install cloud-init,qemu-guest-agent,git,unzip \
    --run-command 'systemctl enable cloud-init' \
    --run-command 'systemctl enable qemu-guest-agent'
else
  echo ">>> La imagen parece tener cloud-init ya instalado."
fi

# === Crear plantilla si no existe ===
if ! qm status $TEMPLATE_ID &>/dev/null; then
  echo ">>> Creando plantilla VM $TEMPLATE_NAME ($TEMPLATE_ID)..."
  qm create $TEMPLATE_ID --name $TEMPLATE_NAME --memory 1024 --net0 virtio,bridge=$BRIDGE
  qm importdisk $TEMPLATE_ID "$CLOUD_IMG_FILE" $STORAGE
  qm set $TEMPLATE_ID --scsihw virtio-scsi-pci --scsi0 $STORAGE:vm-${TEMPLATE_ID}-disk-0
  qm set $TEMPLATE_ID --ide2 $STORAGE:cloudinit
  qm set $TEMPLATE_ID --boot c --bootdisk scsi0
  qm set $TEMPLATE_ID --serial0 socket --vga serial0
  qm template $TEMPLATE_ID
else
  echo ">>> Plantilla $TEMPLATE_ID ya existe."
fi

# === Leer archivo CSV y crear clones ===
if [ ! -f "$CSV_FILE" ]; then
  echo "ERROR: No se encontró el archivo $CSV_FILE"
  exit 1
fi

echo ">>> Creando clones desde $CSV_FILE..."

while IFS=, read -r VMID NAME IPADDR SIZE; do
  [[ "$VMID" =~ ^#.*$ || -z "$VMID" ]] && continue

  if qm status $VMID &>/dev/null; then
    echo ">>> VM $VMID ya existe. Eliminando..."
    qm destroy $VMID --purge
  fi

  # Verificar si existe el volumen lógico huérfano de cloud-init
  CLOUDINIT_VOL="vm-${VMID}-cloudinit"
  LV_PATH="/dev/pve/$CLOUDINIT_VOL"
  [ -e "$LV_PATH" ] && lvremove -y "$LV_PATH"

  # Verificar si quedó un archivo de configuración colgado
  CONF_FILE="/etc/pve/qemu-server/${VMID}.conf"
  [ -f "$CONF_FILE" ] && rm -f "$CONF_FILE"

  echo ">>> Creando VM $NAME ($VMID) con IP $IPADDR..."
  qm clone $TEMPLATE_ID $VMID --name $NAME
  qm set $VMID \
    --memory 2048 \
    --cores 2 \
    --net0 virtio,bridge=$BRIDGE \
    --ipconfig0 ip=$IPADDR,gw=10.10.10.1 \
    --ciuser cpt \
    --sshkey "$SSH_KEY_PATH" \
    --agent enabled=1 \
    --serial0 socket --vga serial0

  if [[ "$SIZE" =~ ^[0-9]+$ ]]; then
    echo ">>> Redimensionando disco de VM $VMID a ${SIZE}G..."
    qm resize "$VMID" scsi0 "${SIZE}G"
  else
    echo ">>> Tamaño de disco no especificado o inválido para VM $VMID. Se omite resize."
  fi

  qm start $VMID

done < "$CSV_FILE"

echo ">>> Todas las VMs fueron creadas y encendidas."


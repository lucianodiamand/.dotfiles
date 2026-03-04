#!/usr/bin/env bash
set -euo pipefail

# ========= CONFIGURACIÓN =========
# IDs de Proxmox
TEMPLATE_ID=8000
TEMPLATE_NAME="debian12-cloud"
VM_IDS=(801 802 803)
VM_NAMES=("k8s-cp-1" "k8s-w-1" "k8s-w-2")

# Recursos de las VMs (mismas longitudes que VM_IDS/VM_NAMES)
VM_CORES=(2 2 2)
VM_MEMORY_MB=(8192 6144 6144)   # 8GB/6GB/6GB

# Red y storage en Proxmox
BRIDGE="vmbr0"
VLAN_TAG=""                      # ejemplo: "20" (vacío = sin tag)
STORAGE="local-lvm"              # ej: local-lvm, zfs-pool, etc.

# Red estática (asumo /24 y gw 10.10.10.1)
CIDR_BITS=24
GATEWAY="10.10.10.1"
DNS_SERVER="10.10.10.1"     # podés usar 1.1.1.1 o 8.8.8.8 si querés
VM_IPS=("10.10.10.81" "10.10.10.82" "10.10.10.83")

# Cloud-init / acceso
CI_USER="user"   # debian              # usuario que crea cloud-init
SSH_PUBKEY_PATH="/root/.ssh/id_rsa.pub"  # cambia si querés otra key

# Disco base y tamaño final
DISK_SIZE_GI=20                  # tamaño final del disco
SCSI_CONTROLLER="virtio-scsi-pci"

# Imagen Debian 12 (genericcloud)
IMG_URL="https://cloud.debian.org/images/cloud/bookworm/latest/debian-12-genericcloud-amd64.qcow2"
IMG_DIR="/mnt/pve/nfs-server/template/iso" # /var/lib/vz/template/iso"     # solo para guardar el qcow2
IMG_FILE="${IMG_DIR}/debian-12-genericcloud-amd64.qcow2"

# ========= FIN CONFIG =========

echo "==> Verificando llave SSH pública en ${SSH_PUBKEY_PATH}"
if [[ ! -f "$SSH_PUBKEY_PATH" ]]; then
  echo "ERROR: No existe ${SSH_PUBKEY_PATH}. Generá una con: ssh-keygen -t ed25519"
  exit 1
fi

echo "==> Creando directorio para imagen: ${IMG_DIR}"
mkdir -p "${IMG_DIR}"

if [[ ! -f "${IMG_FILE}" ]]; then
  echo "==> Descargando imagen Debian 12 Generic Cloud..."
  curl -fL "${IMG_URL}" -o "${IMG_FILE}"
else
  echo "==> Imagen ya existe: ${IMG_FILE} (omitido download)"
fi

# Borra template previo si existe (opcional; comentar si no querés borrar)
if qm status "${TEMPLATE_ID}" &>/dev/null; then
  echo "==> Existe VM/Template con ID ${TEMPLATE_ID}. Eliminando..."
  qm destroy "${TEMPLATE_ID}" --purge || true
fi

echo "==> Creando VM base (ID ${TEMPLATE_ID})"
qm create "${TEMPLATE_ID}" \
  --name "${TEMPLATE_NAME}" \
  --memory 2048 \
  --cores 2 \
  --net0 "virtio,bridge=${BRIDGE}$( [[ -n "${VLAN_TAG}" ]] && printf ',tag=%s' "${VLAN_TAG}" )" \
  --agent enabled=1 \
  --ostype l26

echo "==> Importando disco QCOW2 al storage ${STORAGE}"
qm importdisk "${TEMPLATE_ID}" "${IMG_FILE}" "${STORAGE}"

echo "==> Configurando disco y boot"
qm set "${TEMPLATE_ID}" \
  --scsihw "${SCSI_CONTROLLER}" \
  --scsi0 "${STORAGE}:0,import-from=${IMG_FILE}" \
  --ide2 "${STORAGE}:cloudinit" \
  --boot c --bootdisk scsi0

echo "==> Habilitando consola serie (útil para cloud images) y VGA por serie"
qm set "${TEMPLATE_ID}" --serial0 socket --vga serial0

echo "==> Redimensionando disco a ${DISK_SIZE_GI}G"
qm resize "${TEMPLATE_ID}" scsi0 "${DISK_SIZE_GI}G"

echo "==> Ajustando cloud-init: usuario y llave SSH"
qm set "${TEMPLATE_ID}" \
  --ciuser "${CI_USER}" \
  --sshkey "${SSH_PUBKEY_PATH}"

echo "==> Convirtiendo a plantilla"
qm template "${TEMPLATE_ID}"

echo "==> Clonando 3 VMs desde plantilla"
for i in "${!VM_IDS[@]}"; do
  VMID="${VM_IDS[$i]}"
  NAME="${VM_NAMES[$i]}"
  CORES="${VM_CORES[$i]}"
  MEM="${VM_MEMORY_MB[$i]}"
  IP="${VM_IPS[$i]}"

  # Si ya existe, lo destruimos para evitar conflictos
  if qm status "${VMID}" &>/dev/null; then
    echo "   - VM ${VMID} ya existe. Eliminando..."
    qm destroy "${VMID}" --purge || true
  fi

  echo "   - Clonando ${NAME} (ID ${VMID})"
  qm clone "${TEMPLATE_ID}" "${VMID}" --name "${NAME}" --full 1

  echo " - Configurando recursos/red e IP estática ${IP}/${CIDR_BITS} gw ${GATEWAY}"
  qm set "${VMID}" \
    --cores "${CORES}" \
    --memory "${MEM}" \
    --net0 "virtio,bridge=${BRIDGE}$( [[ -n "${VLAN_TAG}" ]] && printf ',tag=%s' "${VLAN_TAG}" )" \
    --agent enabled=1 \
    --ipconfig0 "ip=${IP}/${CIDR_BITS},gw=${GATEWAY}"

  # DNS/searchdomain (opcional pero útil)
  qm set "${VMID}" --nameserver "${DNS_SERVER}"

  # Reaplicar cloud-init (usuario y llave)
  qm set "${VMID}" \
    --ciuser "${CI_USER}" \
    --sshkey "${SSH_PUBKEY_PATH}"

  echo "   - Iniciando ${NAME}"
  qm start "${VMID}"
done

echo "==> Listo."
echo "    Template: ${TEMPLATE_ID} (${TEMPLATE_NAME})"
printf "    VMs: "; paste -d' ' <(printf "%s\n" "${VM_IDS[@]}") <(printf "%s\n" "${VM_NAMES[@]}") | sed 's/^/[/; s/ /] /'
echo
echo "IPs asignadas:"
paste -d' ' <(printf "%s\n" "${VM_NAMES[@]}") <(printf "%s\n" "${VM_IPS[@]}")
echo "Recordá: gateway=${GATEWAY} / DNS=${DNS_SERVER} / ${CIDR_BITS}."


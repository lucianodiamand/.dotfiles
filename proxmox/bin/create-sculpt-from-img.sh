#!/usr/bin/env bash
set -euo pipefail

URL_DEFAULT="https://genode.org/files/sculpt/sculpt-25-10.img"
SHA256_DEFAULT="0530fe9b464e717c1b6114d57893783c00946f3fe53a18721b5560ae1fd247ad"

usage() {
  cat <<'EOF'
Uso:
  create-sculpt-from-img.sh --vmid 120 --name sculpt \
    [--url URL] [--sha256 HASH] [--storage local-lvm] [--bridge vmbr0] \
    [--memory 2048] [--cores 2] [--bios seabios|ovmf] [--machine q35|i440fx] \
    [--diskbus sata|scsi|ide] [--data-disk-gb 32] [--workdir /root] [--no-verify]

Ejemplo:
  ./create-sculpt-from-img.sh --vmid 120 --name sculpt --storage local-lvm
EOF
}

# Defaults
URL="$URL_DEFAULT"
SHA256="$SHA256_DEFAULT"
VMID=""
NAME="sculpt"
STORAGE="local-lvm"
BRIDGE="vmbr0"
MEMORY="2048"
CORES="2"
BIOS="seabios"
MACHINE="q35"
DISKBUS="sata"
DATA_DISK_GB="32"
WORKDIR="/root"
NO_VERIFY="0"

while [[ $# -gt 0 ]]; do
  case "$1" in
    --vmid) VMID="$2"; shift 2;;
    --name) NAME="$2"; shift 2;;
    --url) URL="$2"; shift 2;;
    --sha256) SHA256="$2"; shift 2;;
    --storage) STORAGE="$2"; shift 2;;
    --bridge) BRIDGE="$2"; shift 2;;
    --memory) MEMORY="$2"; shift 2;;
    --cores) CORES="$2"; shift 2;;
    --bios) BIOS="$2"; shift 2;;
    --machine) MACHINE="$2"; shift 2;;
    --diskbus) DISKBUS="$2"; shift 2;;
    --data-disk-gb) DATA_DISK_GB="$2"; shift 2;;
    --workdir) WORKDIR="$2"; shift 2;;
    --no-verify) NO_VERIFY="1"; shift 1;;
    -h|--help) usage; exit 0;;
    *) echo "Arg desconocido: $1"; usage; exit 1;;
  esac
done

[[ -z "$VMID" ]] && { echo "Falta --vmid"; usage; exit 1; }

command -v qm >/dev/null || { echo "No encuentro 'qm'. Corré esto en un nodo Proxmox."; exit 1; }
command -v curl >/dev/null || { echo "Falta 'curl'."; exit 1; }
command -v sha256sum >/dev/null || { echo "Falta 'sha256sum'."; exit 1; }

if qm status "$VMID" &>/dev/null; then
  echo "Ya existe una VM con VMID=$VMID. Elegí otro VMID."
  exit 1
fi

mkdir -p "$WORKDIR"
IMG_NAME="$(basename "$URL")"
IMG_PATH="$WORKDIR/$IMG_NAME"

maybe_verify() {
  local path="$1"
  if [[ "$NO_VERIFY" == "1" ]]; then
    echo "==> Verificación SHA256 desactivada (--no-verify)"
    return 0
  fi
  echo "==> Verificando SHA256"
  echo "${SHA256}  ${path}" | sha256sum -c -
}

if [[ -f "$IMG_PATH" ]]; then
  echo "==> Encontré la imagen local: $IMG_PATH"
else
  echo "==> No está en local. Descargando: $URL"
  curl -L --fail -o "$IMG_PATH" "$URL"
fi

maybe_verify "$IMG_PATH"

echo "==> Creando VM $VMID ($NAME)"
qm create "$VMID" \
  --name "$NAME" \
  --memory "$MEMORY" \
  --cores "$CORES" \
  --cpu host \
  --machine "$MACHINE" \
  --bios "$BIOS" \
  --net0 "e1000,bridge=${BRIDGE}" \
  --vga std

echo "==> Importando IMG como disco en storage '$STORAGE'"
qm importdisk "$VMID" "$IMG_PATH" "$STORAGE"

UNUSED_LINE="$(qm config "$VMID" | awk -F': ' '/^unused[0-9]+: /{print $0; exit}')"
[[ -z "$UNUSED_LINE" ]] && { echo "No encuentro disco importado (unusedX)."; qm config "$VMID"; exit 1; }

VOLID="${UNUSED_LINE#*: }"
echo "==> Disco importado: $VOLID"

case "$DISKBUS" in
  sata)
    qm set "$VMID" --sata0 "$VOLID"
    qm set "$VMID" --boot order=sata0
    ;;
  scsi)
    qm set "$VMID" --scsihw virtio-scsi-pci
    qm set "$VMID" --scsi0 "$VOLID"
    qm set "$VMID" --boot order=scsi0
    ;;
  ide)
    qm set "$VMID" --ide0 "$VOLID"
    qm set "$VMID" --boot order=ide0
    ;;
  *) echo "diskbus inválido: $DISKBUS (sata|scsi|ide)"; exit 1;;
esac

if [[ "$DATA_DISK_GB" != "0" ]]; then
  echo "==> Creando disco extra vacío: ${DATA_DISK_GB}G"
  case "$DISKBUS" in
    sata) qm set "$VMID" --sata1 "${STORAGE}:${DATA_DISK_GB}" ;;
    scsi) qm set "$VMID" --scsi1 "${STORAGE}:${DATA_DISK_GB}" ;;
    ide)  qm set "$VMID" --ide1  "${STORAGE}:${DATA_DISK_GB}" ;;
  esac
fi

echo
echo "Listo. VM creada: $VMID ($NAME)"
echo "Arrancar: qm start $VMID"
echo "Config:   qm config $VMID"
echo "En VM -> Options -> Use tablet for pointer: No"


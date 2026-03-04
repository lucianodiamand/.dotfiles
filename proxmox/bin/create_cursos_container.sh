#!/bin/bash
set -e

# === VALIDACIÓN DE ARGUMENTOS ===
if [[ $# -lt 3 ]]; then
  echo "Uso: $0 <hostname> <ip/cidr,gw=...> <ctid>"
  echo "Ejemplo: $0 mis-cursos 10.10.10.110/24,gw=10.10.10.1 112"
  exit 1
fi

# === CONFIGURACIÓN INICIAL ===
HOSTNAME="$1"
IP="$2"
CTID="$3"

STORAGE=local-lvm
DISK_SIZE=12
MEMORY=2048
SWAP=512
BRIDGE=vmbr0

# === OBTENER TEMPLATE MÁS RECIENTE ===
TEMPLATE=$(ls /mnt/pve/nfs-server/template/cache/ubuntu-22.04-standard_*.tar.zst | sort -V | tail -n1)
TEMPLATE="nfs-server:vztmpl/$(basename "$TEMPLATE")"

# === PEDIR Y CONFIRMAR CONTRASEÑA ===
while true; do
  echo -n "Ingresá la contraseña de root para el contenedor: "
  read -s PASSWORD1
  echo
  echo -n "Confirmá la contraseña: "
  read -s PASSWORD2
  echo
  if [[ "$PASSWORD1" == "$PASSWORD2" && -n "$PASSWORD1" ]]; then
    PASSWORD="$PASSWORD1"
    break
  else
    echo "Las contraseñas no coinciden o están vacías. Intentá de nuevo."
  fi
done

# === DESTRUIR CONTENEDOR SI EXISTE ===
if pct status $CTID &>/dev/null; then
  echo "Ya existe el CTID $CTID. ¿Deseás eliminarlo y reemplazarlo? (s/N)"
  read -r CONFIRM
  if [[ "$CONFIRM" == "s" || "$CONFIRM" == "S" ]]; then
    pct stop $CTID || true
    pct destroy $CTID
  else
    echo "Abortado."
    exit 1
  fi
fi

# === CREAR CONTENEDOR ===
echo "==> Creando contenedor CT $CTID con hostname $HOSTNAME"
pct create $CTID $TEMPLATE \
  --hostname $HOSTNAME \
  --password $PASSWORD \
  --rootfs ${STORAGE}:${DISK_SIZE} \
  --memory $MEMORY \
  --swap $SWAP \
  --net0 name=eth0,bridge=$BRIDGE,ip=$IP \
  --unprivileged 1

# === INICIAR CONTENEDOR ===
pct start $CTID
echo "==> Esperando que arranque el contenedor..."
sleep 5

# === INSTALAR SSH ===
pct exec $CTID -- bash -c "
  export DEBIAN_FRONTEND=noninteractive &&
  apt update &&
  apt install --no-install-recommends -y openssh-server git-core inkscape texlive-latex-base texlive-extra texlive-font-utils dia python3-pygments texlive-fonts-recommended texlive-fonts-extra make texlive-xetex texlive-extra-utils fonts-inconsolata fonts-liberation xfonts-scalable lmodern texlive-science texlive-plain-generic texlive-lang-french ghostscript &&
  systemctl enable ssh &&
  systemctl start ssh
"

# === PERMITIR ACCESO SSH COMO ROOT ===
pct exec $CTID -- sed -i 's/^#\?\s*PermitRootLogin.*/PermitRootLogin yes/' /etc/ssh/sshd_config
pct exec $CTID -- systemctl restart ssh


# === MOSTRAR RESULTADO ===
echo "==> Contenedor $CTID ($HOSTNAME) listo:"
echo "- IP: $IP"
echo "- Acceso SSH: usuario root / $PASSWORD"


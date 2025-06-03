#!/bin/bash
set -e

# === VALIDACIÓN DE ARGUMENTOS ===
if [[ $# -lt 2 ]]; then
  echo "Uso: $0 <hostname> <ip/cidr,gw=...>"
  echo "Ejemplo: $0 mysql-arq 10.10.10.110/24,gw=10.10.10.1"
  exit 1
fi

# === CONFIGURACIÓN INICIAL ===
CTID=110
HOSTNAME="$1"
IP="$2"
STORAGE=local-lvm
DISK_SIZE=12
MEMORY=1024
SWAP=512
BRIDGE=vmbr0

# === OBTENER TEMPLATE MÁS RECIENTE ===
TEMPLATE=$(ls /mnt/pve/nfs-server/template/cache/ubuntu-22.04-standard_*.tar.zst | sort -V | tail -n1)
TEMPLATE="nfs-server:vztmpl/$(basename "$TEMPLATE")"

# === PEDIR CONTRASEÑA ===
echo -n "Ingresá la contraseña de root para el contenedor y MySQL: "
read -s PASSWORD
echo

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

# === INSTALAR SSH Y MYSQL ===
pct exec $CTID -- bash -c "
  export DEBIAN_FRONTEND=noninteractive &&
  apt update &&
  apt install -y openssh-server mysql-server &&
  systemctl enable ssh &&
  systemctl start ssh &&
  systemctl enable mysql &&
  systemctl start mysql
"

# === PERMITIR ACCESO REMOTO A MYSQL ===
pct exec $CTID -- bash -c "
  sed -i 's/^bind-address.*/bind-address = 0.0.0.0/' /etc/mysql/mysql.conf.d/mysqld.cnf &&
  systemctl restart mysql
"

# === CONFIGURAR MYSQL ===
echo "==> Configurando MySQL root user..."
pct exec $CTID -- mysql -u root -e "
  ALTER USER 'root'@'localhost' IDENTIFIED WITH mysql_native_password BY '$PASSWORD';
  CREATE USER IF NOT EXISTS 'root'@'%' IDENTIFIED WITH mysql_native_password BY '$PASSWORD';
  GRANT ALL PRIVILEGES ON *.* TO 'root'@'%' WITH GRANT OPTION;
  FLUSH PRIVILEGES;
"

# === MOSTRAR RESULTADO ===
echo "==> Contenedor $CTID ($HOSTNAME) con MySQL instalado y listo:"
echo "- IP: $IP"
echo "- Acceso SSH: usuario root, misma contraseña"
echo "- MySQL accesible desde red externa con root / $PASSWORD"

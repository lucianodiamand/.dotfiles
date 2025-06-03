#!/bin/bash
set -e

# === CONFIGURACIÓN DEL CONTENEDOR ===
CTID=110
HOSTNAME=mysql-arq
PASSWORD=user1234
TEMPLATE=$(ls /mnt/pve/nfs-server/template/cache/ubuntu-22.04-standard_*.tar.zst | sort -V | tail -n1)
TEMPLATE="nfs-server:vztmpl/$(basename "$TEMPLATE")"
STORAGE=local-lvm
DISK_SIZE=12
MEMORY=1024
SWAP=512
BRIDGE=vmbr0
IP=10.10.10.150/24,gw=10.10.10.1 # dhcp  # o: 192.168.1.110/24,gw=192.168.1.1

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

# === INSTALAR MYSQL ===
echo "==> Instalando MySQL..."
pct exec $CTID -- bash -c "
  export DEBIAN_FRONTEND=noninteractive &&
  apt update &&
  apt install -y mysql-server &&
  systemctl enable mysql &&
  systemctl start mysql
"

# === CONFIGURAR MYSQL ===
echo "==> Configurando MySQL root user..."
pct exec $CTID -- mysql -u root -e \
  \"ALTER USER 'root'@'localhost' IDENTIFIED WITH mysql_native_password BY 'root123'; FLUSH PRIVILEGES;\"

echo "==> Contenedor $CTID con MySQL listo."


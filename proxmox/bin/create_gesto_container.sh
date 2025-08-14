#!/bin/bash
set -e

# === VALIDACIÓN DE ARGUMENTOS ===
if [[ $# -lt 3 ]]; then
  echo "Uso: $0 <hostname> <ip/cidr,gw=...> <ctid>"
  echo "Ejemplo: $0 mysql-arq 10.10.10.110/24,gw=10.10.10.1 112"
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
  echo -n "Ingresá la contraseña de root para el contenedor y MySQL: "
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

# === PEDIR Y CONFIRMAR CONTRASEÑA PARA USUARIO DE LA APP ===
while true; do
  echo -n "Ingresá la contraseña del usuario de la base de datos (ej. appuser): "
  read -s APP_PASSWORD1
  echo
  echo -n "Confirmá la contraseña: "
  read -s APP_PASSWORD2
  echo
  if [[ "$APP_PASSWORD1" == "$APP_PASSWORD2" && -n "$APP_PASSWORD1" ]]; then
    APP_PASSWORD="$APP_PASSWORD1"
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

# === INSTALAR SSH, MYSQL Y PHP ===
pct exec $CTID -- bash -c "
  export DEBIAN_FRONTEND=noninteractive &&
  apt update &&
  apt install -y software-properties-common &&
  add-apt-repository ppa:ondrej/php &&
  apt update &&
  apt install -y openssh-server &&
  apt install -y apache2 &&
  apt install -y mysql-server &&
  apt install -y php7.2 php7.2-common php7.2-mysql libapache2-mod-php php-mysql &&
  apt install -y pure-ftpd &&
  systemctl enable ssh &&
  systemctl start ssh &&
  systemctl enable mysql &&
  systemctl start mysql &&
  systemctl enable apache2 &&
  systemctl start apache2 &&
  systemctl enable pure-ftpd &&
  systemctl start pure-ftpd
"

# === PERMITIR ACCESO SSH COMO ROOT ===
pct exec $CTID -- sed -i 's/^#\?\s*PermitRootLogin.*/PermitRootLogin yes/' /etc/ssh/sshd_config
pct exec $CTID -- systemctl restart ssh

# === PERMITIR ACCESO REMOTO A MYSQL ===
pct exec $CTID -- bash -c "
  sed -i 's/^bind-address.*/bind-address = 0.0.0.0/' /etc/mysql/mysql.conf.d/mysqld.cnf &&
  systemctl restart mysql
"

# === CONFIGURAR USUARIOS DE MYSQL ===
echo "==> Configurando MySQL root user..."
pct exec $CTID -- mysql -u root -e "
  ALTER USER 'root'@'localhost' IDENTIFIED WITH mysql_native_password BY '$PASSWORD';
  CREATE USER IF NOT EXISTS 'root'@'%' IDENTIFIED WITH mysql_native_password BY '$PASSWORD';
  GRANT ALL PRIVILEGES ON *.* TO 'root'@'%' WITH GRANT OPTION;
  FLUSH PRIVILEGES;
"

# === CREAR BASE DE DATOS Y USUARIO DE APLICACIÓN ===
DB_NAME="tecnicos"
DB_USER="cptros"

echo "==> Creando base de datos '$DB_NAME' y usuario '$DB_USER'..."
pct exec $CTID -- mysql -u root -p"$PASSWORD" -e "
  CREATE DATABASE IF NOT EXISTS \`$DB_NAME\` CHARACTER SET latin1 COLLATE latin1_swedish_ci;
  CREATE USER IF NOT EXISTS '$DB_USER'@'%' IDENTIFIED WITH mysql_native_password BY '$APP_PASSWORD';
  GRANT ALL PRIVILEGES ON \`$DB_NAME\`.* TO '$DB_USER'@'%';
  FLUSH PRIVILEGES;
"

# === HABILITAR ACCESO DE USUARIOS DEL SISTEMA EN PURE-FTPD ===
pct exec $CTID -- bash -c "
  echo yes > /etc/pure-ftpd/conf/UnixAuthentication &&
  systemctl restart pure-ftpd
"

# === CREAR DIRECTORIO FTP POR DEFECTO PARA root ===
pct exec $CTID -- bash -c "
  mkdir -p /srv/ftp &&
  chown root:root /srv/ftp &&
  chmod 755 /srv/ftp
"

# === MOSTRAR RESULTADO ===
echo "==> Contenedor $CTID ($HOSTNAME) con MySQL instalado y listo:"
echo "- IP: $IP"
echo "- Acceso SSH: usuario root / $PASSWORD"
echo "- MySQL accesible desde red externa con root / $PASSWORD"


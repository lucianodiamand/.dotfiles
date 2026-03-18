#!/usr/bin/env bash
set -euo pipefail

# Uso:
#   ./create-ubuntu-php56-vm.sh <VMID> <NOMBRE> [RAM_MB] [CORES] [DISK_GB]
#
# Ejemplo:
#   ./create-ubuntu-php56-vm.sh 160 ubuntu-legacy-php56 4096 2 32

VMID="${1:-160}"
NAME="${2:-ubuntu-legacy-php56}"
RAM="${3:-4096}"
CORES="${4:-2}"
DISK_GB="${5:-32}"

# Ajustá estos valores a tu entorno Proxmox
BRIDGE="${BRIDGE:-vmbr0}"
STORAGE="${STORAGE:-local-lvm}"
CI_STORAGE="${CI_STORAGE:-local-lvm}"
SNIPPET_STORAGE="${SNIPPET_STORAGE:-local}"   # storage tipo Directory con soporte snippets
IMAGE_DIR="${IMAGE_DIR:-/var/lib/vz/template/qemu}"
SNIPPET_DIR="${SNIPPET_DIR:-/var/lib/vz/snippets}"

# Imagen cloud oficial de Ubuntu 16.04 (Xenial)
IMAGE_URL="${IMAGE_URL:-https://cloud-images.ubuntu.com/xenial/current/xenial-server-cloudimg-amd64-disk1.img}"
IMAGE_FILE="${IMAGE_DIR}/xenial-server-cloudimg-amd64-disk1.img"

# Usuario inicial
CIUSER="${CIUSER:-ubuntu}"
CIPASSWORD="${CIPASSWORD:-ubuntu123}"

# Base de datos de prueba
MYSQL_ROOT_PASS="${MYSQL_ROOT_PASS:-rootlegacy123}"
APP_DB="${APP_DB:-legacyapp}"
APP_DB_USER="${APP_DB_USER:-legacyuser}"
APP_DB_PASS="${APP_DB_PASS:-legacypass123}"

IPCONFIG0="${IPCONFIG0:-ip=dhcp}"

mkdir -p "${IMAGE_DIR}" "${SNIPPET_DIR}"

if [[ ! -f "${IMAGE_FILE}" ]]; then
  echo "Descargando imagen cloud de Ubuntu 16.04..."
  wget -O "${IMAGE_FILE}" "${IMAGE_URL}"
fi

USERDATA_FILE="${SNIPPET_DIR}/user-data-${VMID}.yaml"

cat > "${USERDATA_FILE}" <<EOF
#cloud-config
package_update: false
package_upgrade: false

write_files:
  - path: /root/provision-legacy.sh
    permissions: '0755'
    content: |
      #!/usr/bin/env bash
      set -euo pipefail
      export DEBIAN_FRONTEND=noninteractive

      echo "[1/8] Reconfigurando APT para old-releases..."
      cat > /etc/apt/sources.list <<'APT'
      deb http://old-releases.ubuntu.com/ubuntu/ xenial main restricted universe multiverse
      deb http://old-releases.ubuntu.com/ubuntu/ xenial-updates main restricted universe multiverse
      deb http://old-releases.ubuntu.com/ubuntu/ xenial-security main restricted universe multiverse
      APT

      apt-get update

      echo "[2/8] Preseed de MySQL..."
      echo "mysql-server mysql-server/root_password password ${MYSQL_ROOT_PASS}" | debconf-set-selections
      echo "mysql-server mysql-server/root_password_again password ${MYSQL_ROOT_PASS}" | debconf-set-selections

      echo "[3/8] Instalando Apache + PHP 5.6 + MySQL..."
      apt-get install -y \
        apache2 \
        mysql-server \
        php5 \
        php5-mysql \
        libapache2-mod-php5 \
        php5-cli \
        php5-common \
        curl \
        unzip

      echo "[4/8] Configurando PHP legacy..."
      PHPINI_APACHE="/etc/php5/apache2/php.ini"
      PHPINI_CLI="/etc/php5/cli/php.ini"

      sed -i 's/^short_open_tag = .*/short_open_tag = On/' "\$PHPINI_APACHE" || true
      sed -i 's/^short_open_tag = .*/short_open_tag = On/' "\$PHPINI_CLI" || true

      grep -q '^short_open_tag = On' "\$PHPINI_APACHE" || echo 'short_open_tag = On' >> "\$PHPINI_APACHE"
      grep -q '^short_open_tag = On' "\$PHPINI_CLI" || echo 'short_open_tag = On' >> "\$PHPINI_CLI"

      grep -q '^display_errors =' "\$PHPINI_APACHE" && \
        sed -i 's/^display_errors = .*/display_errors = On/' "\$PHPINI_APACHE" || \
        echo 'display_errors = On' >> "\$PHPINI_APACHE"

      grep -q '^error_reporting =' "\$PHPINI_APACHE" && \
        sed -i 's/^error_reporting = .*/error_reporting = E_ALL \& ~E_NOTICE \& ~E_DEPRECATED \& ~E_STRICT/' "\$PHPINI_APACHE" || \
        echo 'error_reporting = E_ALL & ~E_NOTICE & ~E_DEPRECATED & ~E_STRICT' >> "\$PHPINI_APACHE"

      echo "[5/8] Configurando Apache..."
      a2enmod rewrite
      systemctl enable apache2
      systemctl restart apache2

      echo "[6/8] Configurando MySQL..."
      systemctl enable mysql
      systemctl restart mysql

      mysql -uroot -p'${MYSQL_ROOT_PASS}' <<SQL
      CREATE DATABASE IF NOT EXISTS ${APP_DB} CHARACTER SET latin1 COLLATE latin1_swedish_ci;
      CREATE USER IF NOT EXISTS '${APP_DB_USER}'@'localhost' IDENTIFIED BY '${APP_DB_PASS}';
      GRANT ALL PRIVILEGES ON ${APP_DB}.* TO '${APP_DB_USER}'@'localhost';
      FLUSH PRIVILEGES;
      SQL

      mysql -uroot -p'${MYSQL_ROOT_PASS}' ${APP_DB} <<SQL
      CREATE TABLE IF NOT EXISTS usuarios (
        Id_usuario VARCHAR(50) NOT NULL PRIMARY KEY,
        Nombre VARCHAR(100) NOT NULL,
        CGP VARCHAR(255) NOT NULL,
        Distrito VARCHAR(50),
        Permisos CHAR(1),
        Nivel_Acceso_Descargas INT DEFAULT 0,
        Nivel_Acceso_Listas INT DEFAULT 0
      );
      SQL

      echo "[7/8] Preparando directorio web..."
      mkdir -p /var/www/html/app
      chown -R www-data:www-data /var/www/html/app

      cat > /var/www/html/app/info.php <<'PHP'
      <?php phpinfo();
      PHP

      cat > /var/www/html/app/index.php <<'PHP'
      <?
      echo "<h1>Entorno legacy listo</h1>";
      echo "<p>Apache + PHP 5.6 + MySQL instalados.</p>";
      echo "<p>Copiá tu aplicación a /var/www/html/app</p>";
      ?>
      PHP

      cat > /root/LEGACY-README.txt <<TXT
      VM legacy lista.

      URL:
        http://<IP-DE-LA-VM>/app/
        http://<IP-DE-LA-VM>/app/info.php

      Usuario del sistema:
        ${CIUSER} / ${CIPASSWORD}

      Base de datos:
        DB: ${APP_DB}
        USER: ${APP_DB_USER}
        PASS: ${APP_DB_PASS}

      MySQL root:
        root / ${MYSQL_ROOT_PASS}

      Directorio web:
        /var/www/html/app

      Recordá copiar TODOS los archivos del proyecto:
        - fp01.php
        - funciones_conexion_lib.php
        - funciones_database_lib.php
        - pwd.class.php
        - funciones_form_lib.php
        - master.css
        - y cualquier otro include
      TXT

      echo "[8/8] Provisioning terminado."

runcmd:
  - [ bash, /root/provision-legacy.sh ]

final_message: "VM legacy con PHP 5.6 provisionada."
EOF

echo "Eliminando VM previa si existía..."
qm destroy "${VMID}" --purge 1 >/dev/null 2>&1 || true

echo "Creando VM ${VMID} (${NAME})..."
qm create "${VMID}" \
  --name "${NAME}" \
  --memory "${RAM}" \
  --cores "${CORES}" \
  --cpu x86-64-v2-AES \
  --machine q35 \
  --bios ovmf \
  --agent 1 \
  --ostype l26 \
  --scsihw virtio-scsi-pci \
  --net0 virtio,bridge="${BRIDGE}" \
  --serial0 socket \
  --vga serial0

qm set "${VMID}" --efidisk0 "${STORAGE}:0,efitype=4m,pre-enrolled-keys=0"

echo "Importando disco cloud..."
qm importdisk "${VMID}" "${IMAGE_FILE}" "${STORAGE}"
qm set "${VMID}" --scsi0 "${STORAGE}:vm-${VMID}-disk-1"
qm set "${VMID}" --boot "order=scsi0"
qm resize "${VMID}" scsi0 "${DISK_GB}G"

qm set "${VMID}" --ide2 "${CI_STORAGE}:cloudinit"
qm set "${VMID}" --ciuser "${CIUSER}"
qm set "${VMID}" --cipassword "${CIPASSWORD}"
qm set "${VMID}" --ipconfig0 "${IPCONFIG0}"
qm set "${VMID}" --cicustom "user=${SNIPPET_STORAGE}:snippets/$(basename "${USERDATA_FILE}")"

echo "Iniciando VM..."
qm start "${VMID}"

cat <<TXT

VM creada.

Acceso inicial:
  usuario: ${CIUSER}
  password: ${CIPASSWORD}

Cuando arranque:
  - entrá por consola o SSH
  - revisá /root/LEGACY-README.txt
  - copiá tu proyecto a /var/www/html/app

TXT

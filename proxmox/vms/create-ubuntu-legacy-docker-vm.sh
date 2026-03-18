#!/usr/bin/env bash
set -euo pipefail

VMID="${1:-180}"
NAME="${2:-ubuntu-legacy-docker}"
RAM="${3:-4096}"
CORES="${4:-2}"
DISK_GB="${5:-32}"

BRIDGE="${BRIDGE:-vmbr0}"
STORAGE="${STORAGE:-local-lvm}"
CI_STORAGE="${CI_STORAGE:-local-lvm}"
SNIPPET_STORAGE="${SNIPPET_STORAGE:-local}"
IMAGE_DIR="${IMAGE_DIR:-/var/lib/vz/template/qemu}"
SNIPPET_DIR="${SNIPPET_DIR:-/var/lib/vz/snippets}"

IMAGE_URL="${IMAGE_URL:-https://cloud-images.ubuntu.com/jammy/current/jammy-server-cloudimg-amd64.img}"
IMAGE_FILE="${IMAGE_DIR}/jammy-server-cloudimg-amd64.img"

VM_USER="${VM_USER:-ubuntu}"
VM_PASS="${VM_PASS:-ubuntu123}"

MYSQL_ROOT_PASS="${MYSQL_ROOT_PASS:-rootlegacy123}"
APP_DB="${APP_DB:-legacyapp}"
APP_DB_USER="${APP_DB_USER:-legacyuser}"
APP_DB_PASS="${APP_DB_PASS:-legacypass123}"

IPCONFIG0="${IPCONFIG0:-ip=dhcp}"

mkdir -p "${IMAGE_DIR}" "${SNIPPET_DIR}"

if [[ ! -f "${IMAGE_FILE}" ]]; then
  echo "Descargando imagen cloud de Ubuntu 22.04..."
  wget -O "${IMAGE_FILE}" "${IMAGE_URL}"
fi

USERDATA_BASENAME="user-data-${VMID}.yaml"
USERDATA_FILE="${SNIPPET_DIR}/${USERDATA_BASENAME}"

cat > "${USERDATA_FILE}" <<EOF
#cloud-config
hostname: ${NAME}
manage_etc_hosts: true

users:
  - name: ${VM_USER}
    sudo: ALL=(ALL) NOPASSWD:ALL
    groups: sudo,docker
    shell: /bin/bash
    lock_passwd: false

ssh_pwauth: true
disable_root: false

chpasswd:
  expire: false
  list: |
    ${VM_USER}:${VM_PASS}

package_update: true
package_upgrade: true
packages:
  - docker.io
  - docker-compose
  - curl
  - unzip

write_files:
  - path: /root/provision-legacy.sh
    permissions: '0755'
    content: |
      #!/usr/bin/env bash
      set -euxo pipefail
      exec > >(tee -a /var/log/provision-legacy.log) 2>&1

      systemctl enable docker
      systemctl restart docker

      mkdir -p /opt/legacy-app/www
      mkdir -p /opt/legacy-app/mysql
      mkdir -p /opt/legacy-app/php

      cat > /opt/legacy-app/php/php.ini <<'PHPINI'
      short_open_tag=On
      display_errors=On
      error_reporting=E_ALL & ~E_NOTICE & ~E_DEPRECATED & ~E_STRICT
      date.timezone=America/Argentina/Cordoba
      PHPINI

      cat > /opt/legacy-app/docker-compose.yml <<'COMPOSE'
      version: '3.8'
      services:
        db:
          image: mysql:5.7
          container_name: legacy-mysql57
          restart: unless-stopped
          environment:
            MYSQL_ROOT_PASSWORD: ${MYSQL_ROOT_PASS}
            MYSQL_DATABASE: ${APP_DB}
            MYSQL_USER: ${APP_DB_USER}
            MYSQL_PASSWORD: ${APP_DB_PASS}
          command: --character-set-server=latin1 --collation-server=latin1_swedish_ci
          ports:
            - "3306:3306"
          volumes:
            - /opt/legacy-app/mysql:/var/lib/mysql

        web:
          image: php:5.6-apache
          container_name: legacy-php56
          restart: unless-stopped
          depends_on:
            - db
          ports:
            - "80:80"
          volumes:
            - /opt/legacy-app/www:/var/www/html
            - /opt/legacy-app/php/php.ini:/usr/local/etc/php/conf.d/legacy.ini:ro
          working_dir: /var/www/html
      COMPOSE

      cat > /opt/legacy-app/www/index.php <<'PHP'
      <?
      echo "<h1>Entorno legacy listo</h1>";
      echo "<p>PHP: " . phpversion() . "</p>";
      echo "<p>Probá tu app copiándola en /opt/legacy-app/www</p>";
      ?>
      PHP

      cat > /opt/legacy-app/www/info.php <<'PHP'
      <?php phpinfo();
      PHP

      docker compose -f /opt/legacy-app/docker-compose.yml up -d

      sleep 20

      docker exec legacy-mysql57 mysql -uroot -p'${MYSQL_ROOT_PASS}' <<SQL
      CREATE DATABASE IF NOT EXISTS ${APP_DB} CHARACTER SET latin1 COLLATE latin1_swedish_ci;
      CREATE USER IF NOT EXISTS '${APP_DB_USER}'@'%' IDENTIFIED BY '${APP_DB_PASS}';
      GRANT ALL PRIVILEGES ON ${APP_DB}.* TO '${APP_DB_USER}'@'%';
      FLUSH PRIVILEGES;
      USE ${APP_DB};
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

      cat > /root/LEGACY-README.txt <<TXT
      VM legacy lista.

      Usuario VM:
        ${VM_USER} / ${VM_PASS}

      Web:
        http://<IP-DE-LA-VM>/
        http://<IP-DE-LA-VM>/info.php

      Directorio para copiar tu app:
        /opt/legacy-app/www

      Base de datos:
        host: 127.0.0.1
        db: ${APP_DB}
        user: ${APP_DB_USER}
        pass: ${APP_DB_PASS}

      MySQL root:
        root / ${MYSQL_ROOT_PASS}

      Contenedores:
        docker ps
        docker logs legacy-php56
        docker logs legacy-mysql57
      TXT

runcmd:
  - [ bash, /root/provision-legacy.sh ]

final_message: "Provisioning legacy por Docker finalizado."
EOF

echo "Eliminando VM previa si existía..."
qm destroy "${VMID}" --purge 1 >/dev/null 2>&1 || true

echo "Creando VM ${VMID} (${NAME})..."
qm create "${VMID}" \
  --name "${NAME}" \
  --memory "${RAM}" \
  --cores "${CORES}" \
  --cpu x86-64-v2-AES \
  --machine pc \
  --bios seabios \
  --agent 1 \
  --ostype l26 \
  --scsihw virtio-scsi-pci \
  --net0 virtio,bridge="${BRIDGE}" \
  --vga std

echo "Importando disco cloud..."
qm importdisk "${VMID}" "${IMAGE_FILE}" "${STORAGE}"
qm set "${VMID}" --scsi0 "${STORAGE}:vm-${VMID}-disk-0"
qm set "${VMID}" --boot order=scsi0
qm resize "${VMID}" scsi0 "${DISK_GB}G"

qm set "${VMID}" --ide2 "${CI_STORAGE}:cloudinit"
qm set "${VMID}" --ipconfig0 "${IPCONFIG0}"
qm set "${VMID}" --cicustom "user=${SNIPPET_STORAGE}:snippets/${USERDATA_BASENAME}"

qm cloudinit update "${VMID}"

echo "Configuración final:"
qm config "${VMID}"
echo "===== user-data efectivo ====="
qm cloudinit dump "${VMID}" user || true

echo "Iniciando VM..."
qm start "${VMID}"

cat <<TXT

VM creada.

Acceso esperado:
  usuario: ${VM_USER}
  password: ${VM_PASS}

Cuando arranque:
  - esperá 1 o 2 minutos
  - entrá por consola serial o SSH
  - verificá:
      docker ps
      cat /root/LEGACY-README.txt

TXT


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
APP_DB="${APP_DB:-uv0062_cadsp}"
APP_DB_USER="${APP_DB_USER:-uv0062_root}"
APP_DB_PASS="${APP_DB_PASS:-V8vmkxXZlXev4sLK}"

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

write_files:
  - path: /root/provision-legacy.sh
    permissions: '0755'
    content: |
      #!/usr/bin/env bash
      set -euxo pipefail
      export DEBIAN_FRONTEND=noninteractive
      exec > >(tee -a /var/log/provision-legacy.log) 2>&1

      echo "[0/12] Esperando red..."
      for i in \$(seq 1 60); do
        if getent hosts archive.ubuntu.com >/dev/null 2>&1; then
          break
        fi
        sleep 2
      done

      echo "[1/12] APT update..."
      apt-get update -o Acquire::Retries=10 -o Acquire::ForceIPv4=true

      echo "[2/12] Instalando prerequisitos de Docker..."
      apt-get install -y ca-certificates curl gnupg lsb-release unzip

      echo "[3/12] Agregando keyring de Docker..."
      install -m 0755 -d /etc/apt/keyrings
      curl -fsSL https://download.docker.com/linux/ubuntu/gpg -o /etc/apt/keyrings/docker.asc
      chmod a+r /etc/apt/keyrings/docker.asc

      echo "[4/12] Agregando repo oficial de Docker..."
      ARCH="\$(dpkg --print-architecture)"
      CODENAME="\$(. /etc/os-release && echo "\${UBUNTU_CODENAME:-\$VERSION_CODENAME}")"
      echo "deb [arch=\${ARCH} signed-by=/etc/apt/keyrings/docker.asc] https://download.docker.com/linux/ubuntu \${CODENAME} stable" \
        > /etc/apt/sources.list.d/docker.list

      echo "[5/12] Actualizando índices..."
      apt-get update -o Acquire::Retries=10 -o Acquire::ForceIPv4=true

      echo "[6/12] Instalando Docker Engine + Compose..."
      apt-get install -y docker-ce docker-ce-cli containerd.io docker-buildx-plugin docker-compose-plugin

      systemctl enable docker
      systemctl restart docker

      echo "[7/12] Preparando directorios..."
      mkdir -p /opt/legacy-app/www
      mkdir -p /opt/legacy-app/mysql
      mkdir -p /opt/legacy-app/php

      echo "[8/12] php.ini legacy..."
      cat > /opt/legacy-app/php/php.ini <<'PHPINI'
      short_open_tag=On
      display_errors=On
      error_reporting=E_ALL & ~E_NOTICE & ~E_DEPRECATED & ~E_STRICT
      date.timezone=America/Argentina/Cordoba
      PHPINI

      echo "[9/12] Dockerfile PHP 5.6..."
      cat > /opt/legacy-app/Dockerfile <<'DOCKERFILE'
      FROM php:5.6-apache
      RUN docker-php-ext-install mysql mysqli pdo pdo_mysql
      DOCKERFILE

      echo "[10/12] Compose..."
      cat > /opt/legacy-app/docker-compose.yml <<'COMPOSE'
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
          build:
            context: /opt/legacy-app
            dockerfile: Dockerfile
          container_name: legacy-php56
          restart: unless-stopped
          depends_on:
            - db
          ports:
            - "80:80"
          volumes:
            - /opt/legacy-app/www:/var/www/html
            - /opt/legacy-app/php/php.ini:/usr/local/etc/php/conf.d/legacy.ini:ro
      COMPOSE

      echo "[11/12] Web inicial..."
      cat > /opt/legacy-app/www/index.php <<'PHP'
      <?
      echo "<h1>Entorno legacy listo</h1>";
      echo "<p>PHP: " . phpversion() . "</p>";
      echo "<p>mysql extension: " . (function_exists('mysql_query') ? 'OK' : 'NO') . "</p>";
      ?>
      PHP

      cat > /opt/legacy-app/www/info.php <<'PHP'
      <?php phpinfo();
      PHP

      echo "[12/12] Levantando contenedores..."
      cd /opt/legacy-app
      docker compose up -d --build

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

IMPORTED_VOL="$(qm config "${VMID}" | awk '/^unused0: / {print $2}')"
if [[ -z "${IMPORTED_VOL}" ]]; then
  echo "No pude detectar el volumen importado en unused0"
  qm config "${VMID}"
  exit 1
fi

qm set "${VMID}" --scsi0 "${IMPORTED_VOL}"
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

# docker compose ps
# docker compose up -d --build
#
# docker ps -a
# docker rm -f legacy-mysql57 legacy-php56 2>/dev/null || true
# docker network prune -f
# docker compose up -d --build

# docker exec -it legacy-php56 php -r 'var_dump(function_exists("mysql_query"));'

# http://10.10.10.88
# http://10.10.10.88/info.php

# scp -r ./tu-proyecto/* ubuntu@10.10.10.88:/tmp/app/
# cp -r /tmp/app/* /opt/legacy-app/www/

# docker compose restart web

# docker exec -it legacy-php56 getent hosts db
# docker exec -it legacy-php56 php -r '$fp = fsockopen("db", 3306, $errno, $errstr, 5); var_dump((bool)$fp, $errno, $errstr); if ($fp) fclose($fp);'

# Renombrar en funciones_conexion_lib.php
# localhost por db.

# scp uv0062_cadsp.sql

# docker exec -it legacy-mysql57 mysql -uroot -prootlegacy123 -e "CREATE DATABASE uv0062_cadsp CHARACTER SET latin1 COLLATE latin1_swedish_ci;"
# docker exec -it legacy-mysql57 mysql -uroot -prootlegacy123 -e "CREATE USER 'uv0062_root'@'%' IDENTIFIED BY 'V8vmkxXZlXev4sLK';"
# docker exec -it legacy-mysql57 mysql -uroot -prootlegacy123 -e "GRANT ALL PRIVILEGES ON uv0062_cadsp.* TO 'uv0062_root'@'%'; FLUSH PRIVILEGES;"
# docker exec -i legacy-mysql57 mysql -uuv0062_root -pV8vmkxXZlXev4sLK uv0062_cadsp < ~/uv0062_cadsp.sql


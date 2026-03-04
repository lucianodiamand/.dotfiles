#!/usr/bin/env bash
set -euo pipefail

# Parámetros de auto-extend: cuando el pool llegue al 80%, crecerá un 20%
THRESHOLD="${THIN_POOL_AUTOEXTEND_THRESHOLD:-80}"
PERCENT="${THIN_POOL_AUTOEXTEND_PERCENT:-20}"

CONF_DIR="/etc/lvm/lvm.conf.d"
CONF_FILE="${CONF_DIR}/10-thin-autoextend.conf"

echo "==> Configurando auto-extend de LVM thin pools (threshold=${THRESHOLD} / percent=${PERCENT})"

# 1) Drop-in de configuración
mkdir -p "${CONF_DIR}"
cat > "${CONF_FILE}" <<EOF
activation {
  thin_pool_autoextend_threshold = ${THRESHOLD}
  thin_pool_autoextend_percent   = ${PERCENT}
}
EOF
echo "   - Escribí ${CONF_FILE}"

# 2) Habilitar servicio de monitor (si existe) para dmeventd
if systemctl list-unit-files | grep -q '^lvm2-monitor\.service'; then
  systemctl enable --now lvm2-monitor >/dev/null 2>&1 || true
  echo "   - lvm2-monitor habilitado (si aplica)"
fi

# 3) Detectar thin pools y habilitar monitoreo en cada uno (requerido para auto-extend)
#    Filtramos LV de tipo 't' (thin-pool). Salida: VG/LV
mapfile -t THIN_POOLS < <(lvs --noheadings -o vg_name,lv_name,lv_attr \
  | awk '$3 ~ /t/ {print $1"/"$2}')

if [[ "${#THIN_POOLS[@]}" -eq 0 ]]; then
  echo "==> No se encontraron thin pools LVM. Nada que hacer."
  exit 0
fi

echo "==> Thin pools detectados:"
printf '   - %s\n' "${THIN_POOLS[@]}"

for pool in "${THIN_POOLS[@]}"; do
  echo "   - Habilitando monitoreo (dmeventd) en ${pool}"
  lvchange --monitor y "${pool}"
done

# 4) Mostrar estado resumido
echo "==> Estado post-configuración:"
lvs -o vg_name,lv_name,lv_attr,data_percent,metadata_percent,seg_monitor --units g --noheadings

echo "==> Listo."
echo "   • Auto-extend configurado en ${CONF_FILE}"
echo "   • Asegurate de que haya espacio libre en el VG para poder extender los pools."


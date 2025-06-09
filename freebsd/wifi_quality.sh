#!/bin/sh

WIFI_IFACE=${1:-wlan0}

RSSI=$(ifconfig "$WIFI_IFACE" list sta 2>/dev/null | grep -v '^ADDR' | awk '{print $5}' | head -n1)

if [ -z "$RSSI" ]; then
    echo "No Wi-Fi"
    exit 0
fi

# Aceptar floats tipo 23.0
if ! echo "$RSSI" | grep -qE '^-?[0-9]+(\.[0-9]+)?$'; then
    echo "Invalid RSSI"
    exit 1
fi

# Redondear a entero
RSSI_INT=$(printf "%.0f" "$RSSI")

# Calcular quality %
QUALITY=$(( (100 * (RSSI_INT + 80)) / 50 ))

# Clamp
[ "$QUALITY" -lt 0 ] && QUALITY=0
[ "$QUALITY" -gt 100 ] && QUALITY=100

echo "${QUALITY}%"


#!/bin/sh

WIFI_IFACE=${1:-wlan0}

RSSI=$(ifconfig "$WIFI_IFACE" list sta 2>/dev/null | awk 'NR==2 {print $5}')

if [ -z "$RSSI" ]; then
    echo "No Wi-Fi"
    exit 0
fi

# Convert RSSI dBm to "quality" %
# This is arbitrary mapping:
# -30 dBm → 100%
# -80 dBm → 0%

QUALITY=$(( (100 * (RSSI + 80)) / 50 ))

# Clamp between 0 and 100
[ "$QUALITY" -lt 0 ] && QUALITY=0
[ "$QUALITY" -gt 100 ] && QUALITY=100

echo "${QUALITY}%"

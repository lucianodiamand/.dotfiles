#!/bin/bash
TEMPLATE="${HOME}/.config/i3status/i3status.conf.template"
TARGET="${HOME}/.config/i3status/config"

WIFI_IFACE=$(nmcli -t -f DEVICE,TYPE,STATE device | grep ':wifi:connected' | cut -d: -f1 | head -n1)
[ -z "$WIFI_IFACE" ] && WIFI_IFACE=$(iw dev | awk '$1=="Interface"{print $2}' | head -n1)

ETH_IFACE=$(nmcli -t -f DEVICE,TYPE,STATE device | grep ':ethernet:connected' | cut -d: -f1 | head -n1)
[ -z "$ETH_IFACE" ] && ETH_IFACE=$(ip -o link show | awk -F': ' '{print $2}' | grep -E '^e' | head -n1)

cp "$TEMPLATE" "$TARGET"
sed -i "s/_WIFI_/${WIFI_IFACE:-wlan0}/g" "$TARGET"
sed -i "s/_ETH_/${ETH_IFACE:-eth0}/g" "$TARGET"

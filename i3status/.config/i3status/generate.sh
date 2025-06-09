#!/bin/sh

TEMPLATE="${HOME}/.config/i3status/i3status.conf.template"
TARGET="${HOME}/.config/i3status/config"

OS=$(uname)

if [ "$OS" = "FreeBSD" ]; then
    # FreeBSD
    WIFI_IFACE=$(ifconfig -l | tr ' ' '\n' | grep ^wlan | while read IF; do
        if ifconfig "$IF" | grep -q 'status: associated'; then
            echo "$IF"
            break
        fi
    done)

    ETH_IFACE=$(ifconfig -l | tr ' ' '\n' | grep -E '^em|^re|^ue|^bge|^igb' | while read IF; do
        if ifconfig "$IF" | grep -q 'status: active'; then
            echo "$IF"
            break
        fi
    done)

    # sed -i '' para FreeBSD
    cp "$TEMPLATE" "$TARGET"
    sed -i '' "s/_WIFI_/${WIFI_IFACE:-wlan0}/g" "$TARGET"
    sed -i '' "s/_ETH_/${ETH_IFACE:-em0}/g" "$TARGET"

else
    # Linux (Debian, etc.)
    if command -v nmcli >/dev/null 2>&1; then
        WIFI_IFACE=$(nmcli -t -f DEVICE,TYPE,STATE device | grep ':wifi:connected' | cut -d: -f1 | head -n1)
        ETH_IFACE=$(nmcli -t -f DEVICE,TYPE,STATE device | grep ':ethernet:connected' | cut -d: -f1 | head -n1)
    fi

    [ -z "$WIFI_IFACE" ] && WIFI_IFACE=$(iw dev 2>/dev/null | awk '$1=="Interface"{print $2}' | head -n1)
    [ -z "$ETH_IFACE" ] && ETH_IFACE=$(ip -o link show | awk -F': ' '{print $2}' | grep -E '^e' | head -n1)

    # sed -i para GNU sed (Linux)
    cp "$TEMPLATE" "$TARGET"
    sed -i "s/_WIFI_/${WIFI_IFACE:-wlan0}/g" "$TARGET"
    sed -i "s/_ETH_/${ETH_IFACE:-eth0}/g" "$TARGET"
fi

#!/usr/bin/env bash
set -euo pipefail

# ===================== CONFIGURACIÓN =====================
# Red del cluster
CP_IP="10.10.10.81"
WORKER_IPS=("10.10.10.82" "10.10.10.83")
CP_HOSTNAME="k8s-cp-1"
WORKER_HOSTNAMES=("k8s-w-1" "k8s-w-2")
GATEWAY="10.10.10.1"
DNS_SERVER="10.10.10.1"         # opcional (no lo tocamos si ya está ok)

# Usuario creado por cloud-init (según tu template)
SSH_USER="user"    # "debian"

# Rango de IPs para MetalLB (ajusta a tu LAN)
METALLB_POOL_START="10.10.10.50"
METALLB_POOL_END="10.10.10.70"

# Versión de K3s (vacío = latest estable). Ej: "v1.30.5+k3s1"
K3S_VERSION=""

# SSH (si necesitás una clave específica, cámbiala)
SSH_KEY="/root/.ssh/id_rsa"
SSH_OPTS="-i ${SSH_KEY} -o StrictHostKeyChecking=no -o UserKnownHostsFile=/dev/null"
# =================== FIN CONFIGURACIÓN ===================

die() { echo "ERROR: $*" >&2; exit 1; }

ssh_do() {
  local ip="$1"; shift
  ssh ${SSH_OPTS} "${SSH_USER}@${ip}" "$@"
}

scp_to() {
  local ip="$1"; local src="$2"; local dst="$3"
  scp ${SSH_OPTS} "$src" "${SSH_USER}@${ip}:$dst"
}

wait_for_ssh() {
  local ip="$1"
  echo "Esperando SSH en ${ip} ..."
  for i in {1..60}; do
    if ssh ${SSH_OPTS} -o ConnectTimeout=3 "${SSH_USER}@${ip}" "true" 2>/dev/null; then
      echo "SSH OK en ${ip}"
      return 0
    fi
    sleep 2
  done
  die "No pude conectar por SSH a ${ip}"
}

setup_vm_common() {
  local ip="$1" hostname="$2"
  echo "[${ip}] Setup común + hostname=${hostname}"
  ssh_do "${ip}" "sudo bash -s" <<EOF
set -euo pipefail
# Paquetes básicos
sudo apt-get update -y
sudo DEBIAN_FRONTEND=noninteractive apt-get install -y qemu-guest-agent curl git
sudo systemctl enable --now qemu-guest-agent || true

# Desactivar swap para K8s
sudo swapoff -a
sudo sed -i.bak '/[[:space:]]swap[[:space:]]/ s/^/#/' /etc/fstab

# Hostname
echo "${hostname}" | sudo tee /etc/hostname >/dev/null
sudo hostnamectl set-hostname "${hostname}"

# (Opcional) Asegurar DNS
# echo "nameserver ${DNS_SERVER}" | sudo tee /etc/resolv.conf >/dev/null

# Locales (evita warnings setlocale)
sudo apt-get -y install locales
sudo sed -i 's/^# *en_US.UTF-8/en_US.UTF-8/' /etc/locale.gen
echo 'LANG=en_US.UTF-8' | sudo tee /etc/default/locale >/dev/null
sudo locale-gen

sudo timedatectl set-ntp true || true
sudo apt-get -y install chrony
sudo systemctl enable --now chrony
sudo chronyc -a makestep || true

# Kernel params recomendados (cgroups ya vienen correctos en Debian 12)
sudo sysctl -w net.ipv4.ip_forward=1
sudo sed -i '/net.ipv4.ip_forward/d' /etc/sysctl.conf
echo 'net.ipv4.ip_forward=1' | sudo tee -a /etc/sysctl.conf >/dev/null
EOF
}

install_k3s_server() {
  local ip="$1"
  echo "[${ip}] Instalando K3s (server)"
  local vers=""
  [[ -n "${K3S_VERSION}" ]] && vers="INSTALL_K3S_VERSION=${K3S_VERSION}"
  ssh_do "${ip}" "sudo bash -s" <<EOF
set -euxo pipefail
${vers} curl -sfL https://get.k3s.io | sh -s - server \
  --tls-san ${CP_IP} \
  --write-kubeconfig-mode 644
# Ajustar server en kubeconfig para acceso externo
sudo sed -i "s#https://127.0.0.1:6443#https://${CP_IP}:6443#g" /etc/rancher/k3s/k3s.yaml
sudo systemctl enable --now k3s
EOF

  # Esperar que el API responda
  echo "Esperando API k3s en ${ip} ..."
  for i in {1..40}; do
    if ssh_do "${ip}" "sudo kubectl get nodes >/dev/null 2>&1"; then
      echo "API K3s OK en ${ip}"
      break
    fi
    sleep 3
  done
}

get_k3s_token() {
  local ip="$1"
  ssh_do "${ip}" "sudo cat /var/lib/rancher/k3s/server/node-token"
}

install_k3s_agent() {
  local ip="$1" server_url="https://${CP_IP}:6443" token="$2"
  echo "[${ip}] Instalando K3s (agent) contra ${server_url}"
  local vers=""
  [[ -n "${K3S_VERSION}" ]] && vers="INSTALL_K3S_VERSION=${K3S_VERSION}"
  ssh_do "${ip}" "sudo bash -s" <<EOF
set -euxo pipefail
${vers} curl -sfL https://get.k3s.io | K3S_URL='${server_url}' K3S_TOKEN='${token}' sh -s - agent
sudo systemctl enable --now k3s-agent
EOF
}

kubectl_cp() {
  # Ejecuta kubectl en la control-plane
  local args=("$@")
  ssh_do "${CP_IP}" "sudo kubectl ${args[*]}"
}

install_metallb() {
  local start="$1" end="$2"
  echo "[${CP_IP}] Instalando MetalLB con pool ${start}-${end}"

  # Instalar manifiestos
  kubectl_cp apply -f https://raw.githubusercontent.com/metallb/metallb/v0.14.5/config/manifests/metallb-native.yaml

  # Esperar a que el controller y speaker estén listos
  kubectl_cp -n metallb-system rollout status deploy/controller --timeout=180s
  kubectl_cp -n metallb-system rollout status ds/speaker --timeout=180s

  # Esperar a que el webhook tenga endpoints
  echo "Esperando endpoints del webhook..."
  for i in {1..60}; do
    EP=$(kubectl_cp -n metallb-system get endpoints metallb-webhook-service \
          -o jsonpath='{.subsets[0].addresses[0].ip}' 2>/dev/null || true)
    if [ -n "$EP" ]; then
      echo "Webhook listo en $EP"
      break
    fi
    sleep 2
  done

  # Crear recursos de pool/anuncio
  ssh ${SSH_OPTS} "${SSH_USER}@${CP_IP}" "cat | sudo tee /root/metallb-pool.yaml >/dev/null" <<YAML
apiVersion: metallb.io/v1beta1
kind: IPAddressPool
metadata:
  name: lan-pool
  namespace: metallb-system
spec:
  addresses:
    - ${start}-${end}
---
apiVersion: metallb.io/v1beta1
kind: L2Advertisement
metadata:
  name: lan-l2
  namespace: metallb-system
spec:
  ipAddressPools:
    - lan-pool
YAML

  kubectl_cp apply -f /root/metallb-pool.yaml
}

deploy_smoketest() {
  echo "[${CP_IP}] Desplegando smoke test (nginx + LoadBalancer)"
  kubectl_cp create deploy hello --image=nginx:1.25 || true
  kubectl_cp expose deploy hello --port=80 --type=LoadBalancer || true
  echo "Esperando EXTERNAL-IP de 'hello'..."
  for i in {1..40}; do
    ip=$(kubectl_cp get svc hello -o jsonpath='{.status.loadBalancer.ingress[0].ip}' 2>/dev/null || true)
    if [[ -n "$ip" ]]; then
      echo "hello EXTERNAL-IP: $ip"
      break
    fi
    sleep 3
  done
}

# ------------------- MAIN -------------------
echo "==> Verificando conectividad SSH"
wait_for_ssh "${CP_IP}"
for wip in "${WORKER_IPS[@]}"; do wait_for_ssh "${wip}"; done

echo "==> Setup común en VMs"
setup_vm_common "${CP_IP}" "${CP_HOSTNAME}"
for idx in "${!WORKER_IPS[@]}"; do
  setup_vm_common "${WORKER_IPS[$idx]}" "${WORKER_HOSTNAMES[$idx]}"
done

echo "==> Instalando K3s server en control-plane"
install_k3s_server "${CP_IP}"

echo "==> Obteniendo token de unión"
TOKEN="$(get_k3s_token "${CP_IP}")"
[[ -n "${TOKEN}" ]] || die "No pude leer el token del server"

echo "==> Instalando K3s agents en workers"
for wip in "${WORKER_IPS[@]}"; do
  install_k3s_agent "${wip}" "${TOKEN}"
done

echo "==> Verificando nodos Ready"
for i in {1..60}; do
  if kubectl_cp get nodes >/dev/null 2>&1; then
    ready=$(kubectl_cp get nodes --no-headers | awk 'BEGIN{c=0} $2=="Ready"{c++} END{print c+0}')
  else
    ready=0
  fi
  echo "Ready=$ready (intento $i/60)"
  if [ "${ready:-0}" -ge 3 ]; then
    kubectl_cp get nodes -o wide
    break
  fi
  sleep 3
done

echo "==> Instalando MetalLB"
install_metallb "${METALLB_POOL_START}" "${METALLB_POOL_END}"

echo "==> Smoke test"
deploy_smoketest

echo "==> Copiando kubeconfig a Proxmox (para usar kubectl localmente)"
scp ${SSH_OPTS} "${SSH_USER}@${CP_IP}:/etc/rancher/k3s/k3s.yaml" "/tmp/k3s.yaml"
# Ajustamos el server a la IP del CP
sed -i "s#https://127.0.0.1:6443#https://${CP_IP}:6443#g" /tmp/k3s.yaml
echo "Listo. Podés hacer:  KUBECONFIG=/tmp/k3s.yaml kubectl get nodes"

echo "==> Fin OK"


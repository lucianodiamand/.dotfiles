#!/usr/bin/env bash
set -euo pipefail
source ./poc-config.sh

SSH_OPTS="-o StrictHostKeyChecking=no -o UserKnownHostsFile=/dev/null"
strip_cidr(){ echo "$1" | cut -d/ -f1; }
CP_IP="$(strip_cidr "$CP1_IP")"

ssh $SSH_OPTS "${CI_USER}@${CP_IP}" 'set -euo pipefail
export KUBECONFIG=/etc/rancher/k3s/k3s.yaml

kubectl create ns apps >/dev/null 2>&1 || true

# Limpieza previa
kubectl -n apps delete pod tracegen --ignore-not-found=true >/dev/null 2>&1 || true

# Pod que emite trazas a OTel Collector
# - OTLP/HTTP endpoint: http://otel-collector.observability.svc.cluster.local:4318
# - Genera varias trazas con spans
cat <<EOF | kubectl apply -n apps -f -
apiVersion: v1
kind: Pod
metadata:
  name: tracegen
  labels:
    app: tracegen
spec:
  restartPolicy: Never
  containers:
  - name: tracegen
    image: ghcr.io/open-telemetry/opentelemetry-collector-contrib/tracegen:latest
    env:
    - name: OTEL_EXPORTER_OTLP_ENDPOINT
      value: http://otel-collector.observability.svc.cluster.local:4318
    - name: OTEL_EXPORTER_OTLP_PROTOCOL
      value: http/protobuf
    - name: OTEL_SERVICE_NAME
      value: tracegen
    - name: OTEL_RESOURCE_ATTRIBUTES
      value: service.namespace=apps,service.version=0.1,env=poc
    args:
      # Cantidad de trazas / ritmo / spans por traza
      - "-traces"
      - "20"
      - "-rate"
      - "5"
      - "-spans"
      - "6"
EOF

echo "[*] waiting tracegen to complete..."
kubectl -n apps wait --for=condition=Ready pod/tracegen --timeout=120s >/dev/null 2>&1 || true
kubectl -n apps wait --for=jsonpath="{.status.phase}"=Succeeded pod/tracegen --timeout=300s

echo
echo "== tracegen logs =="
kubectl -n apps logs pod/tracegen --tail=200 || true

# Intento extraer un TraceID (depende de la salida del binario)
TRACE_ID="$(kubectl -n apps logs pod/tracegen --tail=200 2>/dev/null | grep -Eo "trace[_-]?id[:= ]+[0-9a-f]{16,32}" | head -n 1 | grep -Eo "[0-9a-f]{16,32}" || true)"

echo
if [ -n "$TRACE_ID" ]; then
  echo "[✓] TraceID detectado: $TRACE_ID"
  echo "    Buscalo en Grafana -> Explore -> Tempo (query por TraceID)"
else
  echo "[!] No pude extraer TraceID de los logs automáticamente."
  echo "    Igual se emitieron trazas. Abrí Grafana -> Explore -> Tempo y buscá por service.name=tracegen"
fi

# Mostrar también que Tempo está listo
echo
echo "== tempo ready (port-forward quick check) =="
kubectl -n observability port-forward svc/tempo 3200:3200 >/tmp/pf-tempo.log 2>&1 & PF=$!
sleep 1
curl -sf http://127.0.0.1:3200/ready && echo "OK" || echo "FAIL"
kill $PF >/dev/null 2>&1 || true

echo
echo "[✓] smoke otlp done"
'

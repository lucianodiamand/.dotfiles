#!/usr/bin/env bash
set -euo pipefail

# ===================== CONFIGURACIÓN =====================
# Dirección del control-plane (K3s server)
CP_IP="10.10.10.81"
SSH_USER="user"   # "debian"
SSH_KEY="/root/.ssh/id_rsa"
SSH_OPTS="-i ${SSH_KEY} -o StrictHostKeyChecking=no -o UserKnownHostsFile=/dev/null"

# Tekton: instala Pipelines + Triggers (Chains opcional)
INSTALL_TEKTON_CHAINS="false"   # "true" para instalar Chains también

# Argo CD: demo app (Git público) para smoke test
DEMO_GIT_URL="https://github.com/argoproj/argocd-example-apps"
DEMO_GIT_PATH="guestbook"
DEMO_GIT_BRANCH="HEAD"

# (Opcional) Forzar una IP específica de MetalLB para Argo CD (si tu MetalLB la soporta)
# Dejalo vacío para que MetalLB asigne una del pool automáticamente.
ARGOCD_EXTERNAL_IP=""   # ej: "10.10.10.60"

# =================== FIN CONFIGURACIÓN ===================

die(){ echo "ERROR: $*" >&2; exit 1; }

ssh_do() { ssh ${SSH_OPTS} "${SSH_USER}@${CP_IP}" "$@"; }

kubectl_cp() { ssh_do "sudo kubectl $*"; }

wait_for_ns_ready() {
  local ns="$1"
  echo "Esperando pods Ready en namespace ${ns} ..."
  for i in {1..80}; do
    not_ready=$(kubectl_cp get pods -n "${ns}" --no-headers 2>/dev/null | awk '{print $3}' | egrep -vc '^(Running|Completed)$' || true)
    total=$(kubectl_cp get pods -n "${ns}" --no-headers 2>/dev/null | wc -l || true)
    if [[ -n "${total}" && "${total}" -gt 0 && "${not_ready}" -eq 0 ]]; then
      kubectl_cp get pods -n "${ns}"
      return 0
    fi
    sleep 3
  done
  die "Timeout esperando namespace ${ns}"
}

wait_for_svc_ip() {
  local ns="$1" name="$2"
  echo "Esperando EXTERNAL-IP para ${ns}/${name} ..."
  for i in {1..60}; do
    ip=$(kubectl_cp get svc -n "${ns}" "${name}" -o jsonpath='{.status.loadBalancer.ingress[0].ip}' 2>/dev/null || true)
    if [[ -n "${ip}" ]]; then
      echo "EXTERNAL-IP de ${ns}/${name}: ${ip}"
      return 0
    fi
    sleep 3
  done
  die "No obtuvo EXTERNAL-IP el servicio ${ns}/${name}"
}

ensure_connectivity() {
  echo "Chequeando SSH al control-plane ${CP_IP} ..."
  for i in {1..30}; do
    if ssh ${SSH_OPTS} -o ConnectTimeout=3 "${SSH_USER}@${CP_IP}" "true" 2>/dev/null; then
      echo "SSH OK."
      return 0
    fi
    sleep 2
  done
  die "No puedo conectar por SSH a ${CP_IP}"
}

install_tekton() {
  echo "Instalando Tekton Pipelines ..."
  kubectl_cp apply -f https://storage.googleapis.com/tekton-releases/pipeline/latest/release.yaml

  # Esperar deployments clave
  kubectl_cp -n tekton-pipelines rollout status deploy/tekton-pipelines-controller --timeout=300s
  kubectl_cp -n tekton-pipelines rollout status deploy/tekton-pipelines-webhook --timeout=300s

  # Esperar endpoints del webhook
  echo "Esperando endpoints de tekton-pipelines-webhook ..."
  for i in {1..60}; do
    EP=$(kubectl_cp -n tekton-pipelines get endpoints tekton-pipelines-webhook \
          -o jsonpath='{.subsets[0].addresses[0].ip}' 2>/dev/null || true)
    if [ -n "$EP" ]; then
      echo "Webhook listo en $EP"
      break
    fi
    sleep 2
  done

  echo "Instalando Tekton Triggers ..."
  kubectl_cp apply -f https://storage.googleapis.com/tekton-releases/triggers/latest/release.yaml
  kubectl_cp apply -f https://storage.googleapis.com/tekton-releases/triggers/latest/interceptors.yaml

  # Esperar que Triggers quede Running
  kubectl_cp -n tekton-pipelines rollout status deploy/tekton-triggers-controller --timeout=300s
  kubectl_cp -n tekton-pipelines rollout status deploy/tekton-triggers-webhook --timeout=300s

  if [[ "${INSTALL_TEKTON_CHAINS}" == "true" ]]; then
    echo "Instalando Tekton Chains ..."
    kubectl_cp apply -f https://storage.googleapis.com/tekton-releases/chains/latest/release.yaml
    kubectl_cp -n tekton-chains rollout status deploy/tekton-chains-controller --timeout=300s
  fi

  echo "Tekton OK."
}

install_argocd() {
  echo "Instalando Argo CD ..."
  kubectl_cp create namespace argocd || true
  kubectl_cp apply -n argocd -f https://raw.githubusercontent.com/argoproj/argo-cd/stable/manifests/install.yaml

  kubectl_cp -n argocd rollout status statefulset/argocd-application-controller --timeout=300s
  kubectl_cp -n argocd rollout status deploy/argocd-repo-server --timeout=300s
  kubectl_cp -n argocd rollout status deploy/argocd-server --timeout=300s

  echo "Exponiendo argocd-server como LoadBalancer ..."
  if [[ -n "${ARGOCD_EXTERNAL_IP}" ]]; then
    # forzar IP específica del pool de MetalLB
    kubectl_cp -n argocd patch svc argocd-server -p \
      "{\"spec\":{\"type\":\"LoadBalancer\",\"loadBalancerIP\":\"${ARGOCD_EXTERNAL_IP}\"}}"
  else
    kubectl_cp -n argocd patch svc argocd-server -p '{"spec":{"type":"LoadBalancer"}}'
  fi

  echo "Esperando EXTERNAL-IP ..."
  for i in {1..60}; do
    ip=$(kubectl_cp -n argocd get svc argocd-server -o jsonpath='{.status.loadBalancer.ingress[0].ip}' 2>/dev/null || true)
    [[ -z "$ip" ]] && ip=$(kubectl_cp -n argocd get svc argocd-server -o jsonpath='{.spec.loadBalancerIP}' 2>/dev/null || true)
    [[ -n "$ip" ]] && { echo "Argo CD en https://$ip"; break; }
    sleep 3
  done

  echo "Password admin:"
  ssh_do "sudo bash -lc \"kubectl -n argocd get secret argocd-initial-admin-secret -o jsonpath='{.data.password}' | base64 -d\""
}

create_argocd_demo_app() {
  echo "Creando proyecto y aplicación demo en Argo CD ..."
  ssh_do "cat | sudo tee /root/argocd-demo.yaml >/dev/null" <<YAML
apiVersion: argoproj.io/v1alpha1
kind: AppProject
metadata:
  name: demo
  namespace: argocd
spec:
  destinations:
    - namespace: default
      server: https://kubernetes.default.svc
  sourceRepos:
    - '*'
---
apiVersion: argoproj.io/v1alpha1
kind: Application
metadata:
  name: demo-guestbook
  namespace: argocd
spec:
  project: demo
  source:
    repoURL: ${DEMO_GIT_URL}
    targetRevision: ${DEMO_GIT_BRANCH}
    path: ${DEMO_GIT_PATH}
  destination:
    server: https://kubernetes.default.svc
    namespace: default
  syncPolicy:
    automated:
      prune: true
      selfHeal: true
YAML
  kubectl_cp apply -f /root/argocd-demo.yaml
  echo "Esperá unos segundos y verificá: kubectl get deploy,svc -n default"
}

tekton_smoketest() {
  echo "Creando Task + Pipeline mínimas de Tekton (smoke test) ..."
  ssh_do "cat | sudo tee /root/tekton-smoke.yaml >/dev/null" <<'YAML'
apiVersion: tekton.dev/v1
kind: Task
metadata:
  name: hello-task
  namespace: default
spec:
  steps:
    - name: say-hello
      image: alpine:3.20
      script: |
        #!/bin/sh
        echo "Hello from Tekton at $(date)"
---
apiVersion: tekton.dev/v1
kind: Pipeline
metadata:
  name: hello-pipeline
  namespace: default
spec:
  tasks:
    - name: run-hello
      taskRef:
        name: hello-task
---
apiVersion: tekton.dev/v1
kind: PipelineRun
metadata:
  generateName: hello-pipelinerun-
  namespace: default
spec:
  pipelineRef:
    name: hello-pipeline
YAML
  kubectl_cp apply -f /root/tekton-smoke.yaml

  echo "Esperando a que el PipelineRun finalice ..."
  for i in {1..40}; do
    status=$(kubectl_cp get pr -n default -o jsonpath='{.items[0].status.conditions[0].reason}' 2>/dev/null || true)
    if [[ "${status}" == "Succeeded" ]]; then
      kubectl_cp get pr -n default
      echo "Tekton OK (PipelineRun Succeeded)."
      return 0
    fi
    sleep 3
  done
  echo "Aviso: Tekton tardó más de lo esperado; revisá 'kubectl describe pr -n default'"
}

main() {
  ensure_connectivity
  echo "==> Instalando Tekton ..."
  install_tekton
  echo "==> Instalando Argo CD ..."
  install_argocd
  echo "==> App demo en Argo CD ..."
  create_argocd_demo_app
  echo "==> Smoke test de Tekton ..."
  tekton_smoketest

  echo
  echo "===== RESUMEN ====="
  echo "- Tekton Pipelines/Triggers instalados (ns: tekton-pipelines)."
  if [[ "${INSTALL_TEKTON_CHAINS}" == "true" ]]; then
    echo "- Tekton Chains instalado (ns: tekton-chains)."
  fi
  echo "- Argo CD instalado (ns: argocd) y expuesto por LoadBalancer."
  echo "  Usuario: admin"
  echo "  Password: (ver arriba)"
  echo "- App demo (guestbook) sincronizada por Argo CD en 'default'."
  echo
  echo "Comandos útiles:"
  echo "  # Ver IP de Argo CD:"
  echo "  kubectl --kubeconfig=/tmp/k3s.yaml -n argocd get svc argocd-server"
  echo "  # Ver Tekton:"
  echo "  kubectl --kubeconfig=/tmp/k3s.yaml get pr,po -A | egrep 'tekton|default'"
  echo
  echo "Listo."
}

main


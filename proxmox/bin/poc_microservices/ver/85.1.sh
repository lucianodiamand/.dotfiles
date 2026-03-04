#!/usr/bin/env bash
set -euo pipefail
source ./poc-config.sh

SSH_OPTS="-o StrictHostKeyChecking=no -o UserKnownHostsFile=/dev/null"
strip_cidr(){ echo "$1" | cut -d/ -f1; }
CP_IP="$(strip_cidr "$CP1_IP")"

APP_NS="apps"
OBS_NS="observability"

ssh $SSH_OPTS "${CI_USER}@${CP_IP}" "set -euo pipefail
export KUBECONFIG=/etc/rancher/k3s/k3s.yaml

kubectl create ns ${APP_NS} >/dev/null 2>&1 || true

# (opcional) limpiar intentos previos si querés ir a cero
# kubectl -n ${APP_NS} delete ingress/apps-ingress svc/svc-a svc/svc-b deploy/svc-a deploy/svc-b cm/demo-svc-a cm/demo-svc-b cm/demo-node-common --ignore-not-found=true

############################
# Code: shared (otel + logger)
############################
cat <<'YAML' | kubectl -n ${APP_NS} apply -f -
apiVersion: v1
kind: ConfigMap
metadata:
  name: demo-node-common
data:
  package.json: |
    {
      \"name\": \"demo-otel-node\",
      \"version\": \"0.1.0\",
      \"private\": true,
      \"type\": \"commonjs\",
      \"dependencies\": {
        \"express\": \"^4.19.2\",
        \"pino\": \"^9.4.0\",
        \"pino-pretty\": \"^11.2.2\",
        \"@opentelemetry/api\": \"^1.9.0\",
        \"@opentelemetry/sdk-node\": \"^0.57.2\",
        \"@opentelemetry/auto-instrumentations-node\": \"^0.57.2\",
        \"@opentelemetry/exporter-trace-otlp-grpc\": \"^0.57.2\",
        \"@opentelemetry/resources\": \"^1.30.1\",
        \"@opentelemetry/semantic-conventions\": \"^1.30.1\"
      }
    }
  otel.js: |
    const { NodeSDK } = require('@opentelemetry/sdk-node');
    const { getNodeAutoInstrumentations } = require('@opentelemetry/auto-instrumentations-node');
    const { OTLPTraceExporter } = require('@opentelemetry/exporter-trace-otlp-grpc');
    const { resourceFromAttributes } = require('@opentelemetry/resources');
    const { SEMRESATTRS_SERVICE_NAME, SEMRESATTRS_SERVICE_VERSION, SEMRESATTRS_DEPLOYMENT_ENVIRONMENT } =
      require('@opentelemetry/semantic-conventions');

    const serviceName = process.env.OTEL_SERVICE_NAME || 'demo-service';
    const exporter = new OTLPTraceExporter({
      url: process.env.OTEL_EXPORTER_OTLP_ENDPOINT || 'http://otel-collector.${OBS_NS}.svc.cluster.local:4317'
    });

    const sdk = new NodeSDK({
      resource: resourceFromAttributes({
        [SEMRESATTRS_SERVICE_NAME]: serviceName,
        [SEMRESATTRS_SERVICE_VERSION]: process.env.SERVICE_VERSION || '0.1',
        [SEMRESATTRS_DEPLOYMENT_ENVIRONMENT]: process.env.ENVIRONMENT || 'poc'
      }),
      traceExporter: exporter,
      instrumentations: [getNodeAutoInstrumentations()]
    });

    sdk.start();

    process.on('SIGTERM', async () => {
      try { await sdk.shutdown(); } catch (e) {}
      process.exit(0);
    });

  logger.js: |
    const pino = require('pino');
    const pretty = process.env.LOG_PRETTY === '1';
    const destination = pretty ? pino.transport({ target: 'pino-pretty', options: { colorize: false } }) : undefined;

    const logger = pino({ level: process.env.LOG_LEVEL || 'info' }, destination);

    function withTraceFields(fields, traceCtx) {
      if (!traceCtx) return fields;
      return Object.assign({}, fields, {
        trace_id: traceCtx.traceId,
        span_id: traceCtx.spanId
      });
    }

    module.exports = { logger, withTraceFields };
YAML

############################
# Service B
############################
cat <<'YAML' | kubectl -n ${APP_NS} apply -f -
apiVersion: v1
kind: ConfigMap
metadata:
  name: demo-svc-b
data:
  server.js: |
    require('./otel');
    const express = require('express');
    const { context, trace } = require('@opentelemetry/api');
    const { logger, withTraceFields } = require('./logger');

    const app = express();

    function traceCtx() {
      const span = trace.getSpan(context.active());
      return span ? span.spanContext() : null;
    }

    app.get('/health', (req, res) => res.status(200).send('ok'));

    app.get('/work', async (req, res) => {
      const tc = traceCtx();
      logger.info(withTraceFields({ msg: 'svc-b work', path: req.path }, tc));
      const ms = Math.floor(50 + Math.random() * 150);
      await new Promise(r => setTimeout(r, ms));
      logger.info(withTraceFields({ msg: 'svc-b done', ms }, tc));
      res.json({ service: 'b', ok: true, ms });
    });

    const port = process.env.PORT || 3000;
    app.listen(port, () => logger.info({ msg: 'svc-b listening', port }));
---
apiVersion: apps/v1
kind: Deployment
metadata:
  name: svc-b
spec:
  replicas: 1
  selector:
    matchLabels: { app: svc-b }
  template:
    metadata:
      labels: { app: svc-b }
    spec:
      containers:
      - name: svc-b
        image: node:22-alpine
        env:
        - name: OTEL_SERVICE_NAME
          value: "svc-b"
        - name: OTEL_EXPORTER_OTLP_ENDPOINT
          value: "http://otel-collector.observability.svc.cluster.local:4317"
        - name: LOG_LEVEL
          value: "info"
        - name: LOG_PRETTY
          value: !!str 0
        - name: PORT
          value: !!str 3000
        ports:
        - containerPort: 3000
        readinessProbe:
          httpGet: { path: /health, port: 3000 }
          initialDelaySeconds: 5
          periodSeconds: 5
        volumeMounts:
        - name: app
          mountPath: /app
        workingDir: /app
        command: ["/bin/sh","-lc"]
        args:
          - |
            set -e
            cd /app
            if [ ! -d node_modules ]; then
              echo "[*] installing deps..."
              npm install --silent
            fi
            node server.js
      volumes:
      - name: app
        projected:
          sources:
          - configMap:
              name: demo-node-common
          - configMap:
              name: demo-svc-b
---
apiVersion: v1
kind: Service
metadata:
  name: svc-b
spec:
  selector: { app: svc-b }
  ports:
  - name: http
    port: 80
    targetPort: 3000
YAML

############################
# Service A (calls B)
############################
cat <<'YAML' | kubectl -n ${APP_NS} apply -f -
apiVersion: v1
kind: ConfigMap
metadata:
  name: demo-svc-a
data:
  server.js: |
    require('./otel');
    const express = require('express');
    const http = require('http');
    const { context, trace } = require('@opentelemetry/api');
    const { logger, withTraceFields } = require('./logger');

    const app = express();

    function traceCtx() {
      const span = trace.getSpan(context.active());
      return span ? span.spanContext() : null;
    }

    function callSvcB() {
      return new Promise((resolve, reject) => {
        const req = http.request(
          {
            hostname: 'svc-b.apps.svc.cluster.local',
            port: 80,
            path: '/work',
            method: 'GET',
            timeout: 2000
          },
          (res) => {
            let data = '';
            res.on('data', (c) => (data += c));
            res.on('end', () => {
              try { resolve(JSON.parse(data)); }
              catch (e) { resolve({ raw: data }); }
            });
          }
        );
        req.on('error', reject);
        req.on('timeout', () => req.destroy(new Error('timeout calling svc-b')));
        req.end();
      });
    }

    app.get('/health', (req, res) => res.status(200).send('ok'));

    app.get('/do-work', async (req, res) => {
      const tc = traceCtx();
      logger.info(withTraceFields({ msg: 'svc-a start', path: req.path }, tc));
      const b = await callSvcB();
      logger.info(withTraceFields({ msg: 'svc-a got response from b', b }, tc));
      res.json({ service: 'a', ok: true, fromB: b });
    });

    const port = process.env.PORT || 3000;
    app.listen(port, () => logger.info({ msg: 'svc-a listening', port }));
---
apiVersion: apps/v1
kind: Deployment
metadata:
  name: svc-a
spec:
  replicas: 1
  selector:
    matchLabels: { app: svc-a }
  template:
    metadata:
      labels: { app: svc-a }
    spec:
      containers:
      - name: svc-a
        image: node:22-alpine
        env:
        - name: OTEL_SERVICE_NAME
          value: "svc-a"
        - name: OTEL_EXPORTER_OTLP_ENDPOINT
          value: "http://otel-collector.observability.svc.cluster.local:4317"
        - name: LOG_LEVEL
          value: "info"
        - name: LOG_PRETTY
          value: !!str 0
        - name: PORT
          value: !!str 3000
        ports:
        - containerPort: 3000
        readinessProbe:
          httpGet: { path: /health, port: 3000 }
          initialDelaySeconds: 5
          periodSeconds: 5
        volumeMounts:
        - name: app
          mountPath: /app
        workingDir: /app
        command: ["/bin/sh","-lc"]
        args:
          - |
            set -e
            cd /app
            if [ ! -d node_modules ]; then
              echo "[*] installing deps..."
              npm install --silent
            fi
            node server.js
      volumes:
      - name: app
        projected:
          sources:
          - configMap:
              name: demo-node-common
          - configMap:
              name: demo-svc-a
---
apiVersion: v1
kind: Service
metadata:
  name: svc-a
spec:
  selector: { app: svc-a }
  ports:
  - name: http
    port: 80
    targetPort: 3000
YAML

############################
# Ingress (paths)
############################
cat <<'YAML' | kubectl -n ${APP_NS} apply -f -
apiVersion: networking.k8s.io/v1
kind: Ingress
metadata:
  name: apps-ingress
spec:
  ingressClassName: nginx
  rules:
  - http:
      paths:
      - path: /a
        pathType: Prefix
        backend:
          service:
            name: svc-a
            port:
              number: 80
      - path: /b
        pathType: Prefix
        backend:
          service:
            name: svc-b
            port:
              number: 80
YAML

echo '[*] waiting rollouts... (primer arranque puede tardar por npm install)'
kubectl -n ${APP_NS} rollout status deploy/svc-b --timeout=15m
kubectl -n ${APP_NS} rollout status deploy/svc-a --timeout=15m

echo
kubectl -n ${APP_NS} get pods -o wide
echo
kubectl -n ${APP_NS} get ingress -o wide
echo
echo '[✓] apps deployed: /a/do-work and /b/work via ingress'
"


#!/usr/bin/env bash
set -euo pipefail

ROOT="${1:-embedded-analysis}"

mkdir -p "$ROOT"/{hardware/{peripherals,schematics},boot/{uboot/{drivers,logs},logs},kernel/{drivers,logs},rootfs,experiments,notes,images}

# .gitignore
cat > "$ROOT/.gitignore" <<'EOF'
# Org export / cache
*.html
*.pdf
*.tex
*.org-id-locations
/.dir-locals.el

# Logs, capturas, binarios y dumps
**/logs/
images/**/*.bin
images/**/*.img
images/**/*.elf
images/**/*.hex
images/**/*.pcap
images/**/*.pcapng
**/*.log
**/*.dmp
**/*.dump

# Construcción
build/
out/
dist/

# SO/editor
.DS_Store
Thumbs.db
*~
.#*
EOF

# Índice
cat > "$ROOT/00-index.org" <<'EOF'
#+title: Índice del proyecto (embedded)
#+author: Luciano
#+roam_tags: index
#+startup: overview

* Propósito
Documentar sistemáticamente el análisis de una placa embebida: bootloader, kernel, DT, FS, y hardware.

* Mapa del repositorio
- [[file:hardware/soc-overview.org][SoC overview]]
- [[file:hardware/memory-map.org][Mapa de memoria]]
- [[file:boot/boot-sequence.org][Secuencia de arranque]]
- [[file:boot/uboot/build-notes.org][U-Boot: notas de build]]
- [[file:kernel/config-analysis.org][Kernel: configuración]]
- [[file:kernel/device-tree.org][Device Tree]]
- [[file:rootfs/init-system.org][RootFS / init]]
- [[file:experiments/README.org][Experimentos (bitácora)]]
- [[file:notes/glossary.org][Glosario]]
- [[file:notes/references.org][Referencias]]
- [[file:notes/tasks.org][Tareas]]

* Estado general
- HW principal: ::
- Bootloader: ::
- Kernel: ::
- RootFS: ::

* Próximos pasos
** TODO Completar tabla de memoria en ~hardware/memory-map.org~
** TODO Diagrama de arranque en ~boot/boot-sequence.org~
EOF

# Hardware
cat > "$ROOT/hardware/soc-overview.org" <<'EOF'
#+title: SoC overview
#+filetags: :hardware:soc:

* Identificación
- Modelo SoC: ::
- CPU / ISA: ::
- Endianness: ::
- Relojes: ::

* Buses y bloques
- Interconexión: ::
- Controladores: ::

* Periféricos relevantes
- [[file:hardware/peripherals/uart.org][UART]]
- [[file:hardware/peripherals/ethernet.org][Ethernet / MAC + PHY]]
- [[file:hardware/peripherals/nand.org][NAND / MTD]]
- [[file:hardware/peripherals/gpio.org][GPIO]]
EOF

cat > "$ROOT/hardware/memory-map.org" <<'EOF'
#+title: Mapa de memoria
#+filetags: :hardware:memmap:

* Resumen
| Región        | Dirección base | Tamaño  | Descripción                  |
|---------------+-----------------+---------+------------------------------|
| ROM/Boot ROM  | 0xXXXXXXXX      | XX MiB  |                              |
| RAM (DDR/SDR) | 0xXXXXXXXX      | XX MiB  |                              |
| MMIO SoC      | 0xXXXXXXXX      | XX MiB  | Periféricos                  |
| Flash NAND    | 0xXXXXXXXX      | XX MiB  | (si mapeada)                 |

* Notas
- Segmentos KSEGx (si MIPS) / mapeos no caché, etc.
- Punteros a datasheets.
EOF

# Periféricos
cat > "$ROOT/hardware/peripherals/uart.org" <<'EOF'
#+title: UART
#+filetags: :hardware:uart:

* Direcciones y registros
- Base MMIO: ::
- Baud, divisor, FIFO, interrupciones.

* Pruebas
#+begin_src text
log: boot/ logs de UART de ROM / U-Boot / Kernel
#+end_src
EOF

cat > "$ROOT/hardware/peripherals/ethernet.org" <<'EOF'
#+title: Ethernet (MAC/PHY)
#+filetags: :hardware:ethernet:net:

* MAC
- Dirección base: ::
- Registers key: TX/RX, ISR/IMR, modo 8/16/32-bit.

* PHY
- Bus: MII/RMII/RGMII
- Registros MII: BMCR, BMSR, ANAR, etc.

* Capturas
Colocar pcap/pcapng en =images/= y referenciar aquí.
EOF

cat > "$ROOT/hardware/peripherals/nand.org" <<'EOF'
#+title: NAND / MTD
#+filetags: :hardware:nand:mtd:

* Parámetros físicos
- Tamaño página: ::
- OOB: ::
- Tamaño bloque (eraseblock): ::
- ECC requerido: ::

* Particionado (MTD)
| mtd | tamaño   | erasesize | nombre        |
|-----+----------+-----------+---------------|
| 0   |          |           |               |
EOF

cat > "$ROOT/hardware/peripherals/gpio.org" <<'EOF'
#+title: GPIO
#+filetags: :hardware:gpio:

* Bancos y pines
- Banco A/B/C ...:
- Polaridad, pulls, funciones alternativas.

* Uso en la placa
- LEDs, botones, resets de periféricos.
EOF

# Boot
cat > "$ROOT/boot/boot-sequence.org" <<'EOF'
#+title: Secuencia de arranque
#+filetags: :boot:sequence:

* Flujo general
ROM → Bootloader → Kernel → Init

* Detalle
1. ROM: dónde busca, vector reset, reloj.
2. U-Boot: inicialización DRAM, relojes, consola.
3. Carga del kernel: medio (NAND/NET/USB), DTB, cmdline.
4. RootFS: initramfs / rootfs en NAND/UBI/UBIFS/EXT.

* Diagramas
- Añadir diagrama exportable (Org -> PlantUML/Mermaid si usás).
EOF

cat > "$ROOT/boot/rom-analysis.org" <<'EOF'
#+title: ROM / First-stage
#+filetags: :boot:rom:

* Observaciones
- Dirección de reset:
- Qué periféricos toca:
- Mensajes iniciales por UART (si hay):
EOF

cat > "$ROOT/boot/uboot/build-notes.org" <<'EOF'
#+title: U-Boot: Notas de compilación
#+filetags: :boot:uboot:build:

* Toolchain y versión
- Toolchain: ::
- U-Boot: ::

* Config y targets
#+begin_src bash
make <board>_defconfig
make -j$(nproc)
#+end_src

* Variables de entorno útiles
Ver [[file:boot/uboot/env-variables.org][env-variables]].
EOF

cat > "$ROOT/boot/uboot/env-variables.org" <<'EOF'
#+title: U-Boot: variables de entorno
#+filetags: :boot:uboot:env:

* Red
- ipaddr, serverip, ethaddr

* Boot
- bootcmd, bootargs, bootdelay
EOF

cat > "$ROOT/boot/uboot/drivers/net-dm9000.org" <<'EOF'
#+title: U-Boot: driver DM9000
#+filetags: :boot:uboot:driver:ethernet:dm9000:

* Conexión CPU <-> DM9000
- Modo bus: 8/16-bit
- Líneas: CMD, CS, RD/WR, INT

* Registro de inicialización
- NSR/ISR, IMR, RCR/TCR, PHY via MII

* Issues / hallazgos
- udelay(), tiempos, autoneg, link
- Capturas TFTP
EOF

# Kernel
cat > "$ROOT/kernel/config-analysis.org" <<'EOF'
#+title: Kernel: configuración
#+filetags: :kernel:config:

* Versión de kernel
* Opciones clave (anotar sólo lo relevante)
- MTD/UBI/UBIFS
- NET/DM9000/PHY
- USB/MSC (si aplica)
EOF

cat > "$ROOT/kernel/device-tree.org" <<'EOF'
#+title: Device Tree (DTS)
#+filetags: :kernel:dts:devicetree:

* Archivos y nodos principales
- SoC, memory, chosen, clocks, gpio, uart, ethernet, mtd

* Notas
- Compatibles, phandles, clocks, resets, regulators, pinctrl.
EOF

cat > "$ROOT/kernel/drivers/mtd.org" <<'EOF'
#+title: Kernel: MTD
#+filetags: :kernel:mtd:

* Controladores
- SPI-NAND / Raw NAND / ONFI / ECC
EOF

cat > "$ROOT/kernel/drivers/net.org" <<'EOF'
#+title: Kernel: red
#+filetags: :kernel:net:

* MAC + PHY
- Driver, MDIO bus, PHY-ID, quirks.
EOF

cat > "$ROOT/kernel/drivers/serial.org" <<'EOF'
#+title: Kernel: serial
#+filetags: :kernel:serial:

* Consola
- ttyS*, earlycon, parámetros.
EOF

# RootFS
cat > "$ROOT/rootfs/init-system.org" <<'EOF'
#+title: RootFS: sistema de inicio
#+filetags: :rootfs:init:

* init
- BusyBox / systemd (según caso)
- rcS, scripts, servicios

* Fstab / puntos de montaje
EOF

cat > "$ROOT/rootfs/filesystems.org" <<'EOF'
#+title: Filesystems
#+filetags: :rootfs:fs:

* Opciones
- JFFS2 / YAFFS2 / UBI+UBIFS / EXT*

* Selección y trade-offs
- Tiempo de booteo, robustez ante power-loss
EOF

cat > "$ROOT/rootfs/startup-scripts.org" <<'EOF'
#+title: Scripts de arranque
#+filetags: :rootfs:scripts:

* Secuencia propia
- qué, cuándo y por qué
EOF

cat > "$ROOT/rootfs/binaries.md" <<'EOF'
# Binarios relevantes
- busybox --list
- herramientas de red
- utilidades de MTD/UBI
EOF

# Experiments
cat > "$ROOT/experiments/README.org" <<'EOF'
#+title: Experimentos (bitácora)
#+filetags: :experiments:

* Convención
Un archivo por experimento: =YYYY-MM-DD-descripcion.org=.

* Índice rápido
- Enlazar aquí los experimentos más útiles.
EOF

# Ejemplo de experimento con plantilla
TODAY="$(date +%F)"
cat > "$ROOT/experiments/${TODAY}-tftp-debug.org" <<'EOF'
#+title: TFTP debug
#+filetags: :experiments:net:tftp:
#+property: header-args:bash :results output :exports both

* Contexto
:PROPERTIES:
:HW: 
:FW: U-Boot 
:KERNEL: 
:END:

* Objetivo
Analizar transferencia TFTP y tiempos en MAC/PHY.

* Procedimiento
1) Capturar con tcpdump/wireshark.
2) Verificar bloques, ACKs, retransmisiones.

* Resultados
- Velocidad observada:
- Retransmisiones:
- Posible causa:

* Próximos pasos
** TODO Validar ~udelay()~ en driver
** TODO Forzar ~blksize~ y comparar
EOF

# Notes
cat > "$ROOT/notes/glossary.org" <<'EOF'
#+title: Glosario
#+filetags: :notes:glossary:

* Términos
- CLE/ALE/CE#/WP#/RB#: Señales NAND
- BMCR/BMSR: Registros básicos PHY
- MTD/UBI/UBIFS/JFFS2/YAFFS2
EOF

cat > "$ROOT/notes/references.org" <<'EOF'
#+title: Referencias
#+filetags: :notes:refs:

* Datasheets / Manuales
- SoC:
- PHY:
- NAND:

* Enlaces útiles
- 
EOF

cat > "$ROOT/notes/tasks.org" <<'EOF'
#+title: Tareas
#+filetags: :notes:tasks:
#+todo: TODO(t) NEXT(n) WAIT(w@) BLOCK(b@) | DONE(d) CANCELED(c@)

* Backlog
** TODO Completar ~kernel/device-tree.org~
** TODO Esquematizar ~boot/boot-sequence.org~
** TODO Medir tiempos de ~udelay()~ en DM9000

* En curso
* Hecho
EOF

# Imagen de README rápido
cat > "$ROOT/README.md" <<'EOF'
# Embedded analysis + Org-mode

Estructura inicial para documentar una placa embebida con Org/Org-roam.

## Uso
- Editá `00-index.org` como puerta de entrada.
- Agregá experimentos en `experiments/YYYY-MM-DD-nombre.org`.
- Poné logs crudos en las subcarpetas `logs/` correspondientes.
EOF

# Git opcional (no falla si ya existe)
if ! git -C "$ROOT" rev-parse >/dev/null 2>&1; then
  (cd "$ROOT" && git init -q && git add . && git commit -qm "Estructura inicial Org-mode para análisis embebido")
fi

echo "Proyecto creado en: $ROOT"


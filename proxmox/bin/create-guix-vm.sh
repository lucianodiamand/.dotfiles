Creando VM 131 (guix-vm)...
update VM 131: -efidisk0 local-lvm:0,efitype=4m,pre-enrolled-keys=0
  Rounding up size to full physical extent 4.00 MiB
  Logical volume "vm-131-disk-0" created.
transferred 0.0 B of 528.0 KiB (0.00%)
transferred 528.0 KiB of 528.0 KiB (100.00%)
transferred 528.0 KiB of 528.0 KiB (100.00%)
efidisk0: successfully created disk 'local-lvm:vm-131-disk-0,efitype=4m,pre-enrolled-keys=0,size=4M'
update VM 131: -scsi0 local-lvm:32,discard=on,ssd=1
  Logical volume "vm-131-disk-1" created.
scsi0: successfully created disk 'local-lvm:vm-131-disk-1,discard=on,size=32G,ssd=1'
update VM 131: -ide2 nfs-server:iso/guix-system-install-1.5.0.x86_64-linux.iso,media=cdrom
update VM 131: -boot order=ide2;scsi0
update VM 131: -vga std

VM creada correctamente.

Resumen:
agent: 1
bios: ovmf
boot: order=ide2;scsi0
cores: 2
cpu: x86-64-v2-AES
efidisk0: local-lvm:vm-131-disk-0,efitype=4m,pre-enrolled-keys=0,size=4M
ide2: nfs-server:iso/guix-system-install-1.5.0.x86_64-linux.iso,media=cdrom,size=1160412K
machine: q35
memory: 4096
meta: creation-qemu=9.2.0,ctime=1773367051
name: guix-vm
net0: virtio=BC:24:11:F0:E3:58,bridge=vmbr0
onboot: 1
ostype: l26
scsi0: local-lvm:vm-131-disk-1,discard=on,size=32G,ssd=1
scsihw: virtio-scsi-pci
smbios1: uuid=363d7dfb-543d-434e-81e6-8c9e55d3b867
sockets: 1
vga: std
vmgenid: 64c4584b-4e10-44c8-92d6-838d2c6bc27e

Para iniciarla:
  qm start 131

Luego abrí la consola desde Proxmox y hacé la instalación de Guix.
Cuando termines de instalar dentro del guest, podés quitar el ISO y dejar boot desde scsi0:
  qm set 131 --boot order=scsi0
  qm set 131 --delete ide2
root@proxmox:~# cat create-guix-vm.sh 
#!/usr/bin/env bash
set -euo pipefail

# =========================
# Configuración
# =========================
VMID="${VMID:-131}"
NAME="${NAME:-guix-vm}"

RAM="${RAM:-4096}"          # MB
CORES="${CORES:-2}"
SOCKETS="${SOCKETS:-1}"
DISK_SIZE_GIB="${DISK_SIZE_GIB:-32}"

BRIDGE="${BRIDGE:-vmbr0}"

# Storage donde querés crear el disco de la VM
DISK_STORAGE="${DISK_STORAGE:-local-lvm}"

# Storage NFS donde ya tenés subido el ISO
ISO_STORAGE="${ISO_STORAGE:-nfs-server}"

# Volumen ISO dentro del storage NFS.
# Ajustalo al nombre real que ves en: pvesm list <storage>
ISO_VOLID="${ISO_VOLID:-${ISO_STORAGE}:iso/guix-system-install-1.5.0.x86_64-linux.iso}"

# UEFI / BIOS
BIOS_TYPE="${BIOS_TYPE:-ovmf}"     # ovmf | seabios
MACHINE_TYPE="${MACHINE_TYPE:-q35}" # q35 | i440fx

# Si querés que arranque automáticamente al boot del nodo
ONBOOT="${ONBOOT:-1}"

# =========================
# Validaciones
# =========================
if qm status "$VMID" >/dev/null 2>&1; then
  echo "Error: ya existe una VM con VMID=$VMID" >&2
  exit 1
fi

if ! pvesm status | awk '{print $1}' | grep -qx "$ISO_STORAGE"; then
  echo "Error: no existe el storage ISO '$ISO_STORAGE'" >&2
  exit 1
fi

if ! pvesm status | awk '{print $1}' | grep -qx "$DISK_STORAGE"; then
  echo "Error: no existe el storage de discos '$DISK_STORAGE'" >&2
  exit 1
fi

# Verifica que el ISO exista en el storage
if ! pvesm list "$ISO_STORAGE" | awk '{print $1}' | grep -qx "$ISO_VOLID"; then
  echo "Error: no encontré el ISO '$ISO_VOLID' en el storage '$ISO_STORAGE'" >&2
  echo "Tip: ejecutá 'pvesm list $ISO_STORAGE' para ver el nombre exacto." >&2
  exit 1
fi

# =========================
# Crear VM
# =========================
echo "Creando VM $VMID ($NAME)..."

qm create "$VMID" \
  --name "$NAME" \
  --memory "$RAM" \
  --cores "$CORES" \
  --sockets "$SOCKETS" \
  --cpu x86-64-v2-AES \
  --machine "$MACHINE_TYPE" \
  --bios "$BIOS_TYPE" \
  --ostype l26 \
  --scsihw virtio-scsi-pci \
  --net0 virtio,bridge="$BRIDGE" \
  --agent 1 \
  --onboot "$ONBOOT"

# =========================
# UEFI: agregar efidisk
# =========================
if [ "$BIOS_TYPE" = "ovmf" ]; then
  qm set "$VMID" --efidisk0 "${DISK_STORAGE}:0,efitype=4m,pre-enrolled-keys=0"
fi

# =========================
# Crear disco principal
# =========================
qm set "$VMID" --scsi0 "${DISK_STORAGE}:${DISK_SIZE_GIB},discard=on,ssd=1"

# =========================
# Adjuntar ISO
# =========================
qm set "$VMID" --ide2 "$ISO_VOLID",media=cdrom

# =========================
# Orden de arranque
# =========================
qm set "$VMID" --boot "order=ide2;scsi0"

# =========================
# Consola gráfica estándar
# =========================
qm set "$VMID" --vga std

echo
echo "VM creada correctamente."
echo
echo "Resumen:"
qm config "$VMID"

echo
echo "Para iniciarla:"
echo "  qm start $VMID"
echo
echo "Luego abrí la consola desde Proxmox y hacé la instalación de Guix."
echo "Cuando termines de instalar dentro del guest, podés quitar el ISO y dejar boot desde scsi0:"
echo "  qm set $VMID --boot order=scsi0"
echo "  qm set $VMID --delete ide2"


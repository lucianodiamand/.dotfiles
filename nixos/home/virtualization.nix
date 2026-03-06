{ config, pkgs, lib, ... }:

{
  home.packages = with pkgs; [
    qemu
    virt-manager
    virt-viewer
  ];

  xdg.configFile."libvirt/libvirt.conf".text = ''
    uri_default = "qemu:///system"
  '';
}

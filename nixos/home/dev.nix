{ config, pkgs, lib, ... }:

{
  home.packages = with pkgs; [
    nodejs_22
    yarn
    python3
    meld
    gcc
    gcc.man
    (pkgs.nodePackages.typescript-language-server)
    (pkgs.nodePackages.typescript)
    lua-language-server
    tree-sitter
    guile
    kicad
    arduino
    dbeaver-bin
    mariadb.client
    cargo
    pkgs.jdk21
    jdt-language-server
    universal-ctags
    cscope
    entr
    ghidra-bin
  ];
}

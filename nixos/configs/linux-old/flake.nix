{
  description = "Legacy Linux kernel build shell";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-24.05";
  };

  outputs = { self, nixpkgs }:
    let
      systems = [ "x86_64-linux" "aarch64-linux" ];
      forAllSystems = f: nixpkgs.lib.genAttrs systems (system: f system);
    in
    {
      devShells = forAllSystems (system:
        let
          pkgs = import nixpkgs { inherit system; };
        in
        {
          default = pkgs.mkShell {
            packages = with pkgs; [
              gcc
              binutils
              gnumake
              bc
              bison
              flex
              perl
              openssl
              pkg-config
              ncurses
              elfutils
              dwarves
              xz
              zstd
              cpio
              rsync
              git
              wget
            ];

            shellHook = ''
              export CC=${CC:-gcc}
              export HOSTCC=${HOSTCC:-gcc}
            '';
          };
        });
    };
}

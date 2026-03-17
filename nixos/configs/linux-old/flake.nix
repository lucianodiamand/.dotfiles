{
  description = "Legacy Linux kernel build shell";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-24.05";
    nixpkgs-perl = {
      url = "github:NixOS/nixpkgs/f22817d8d2bc17d2bcdb8ac4308a4bce6f5d1d2b";
      flake = false;
    };
  };

  outputs = { self, nixpkgs, nixpkgs-perl }:
    let
      systems = [ "x86_64-linux" "aarch64-linux" ];
      forAllSystems = f: nixpkgs.lib.genAttrs systems (system: f system);
    in
    {
      devShells = forAllSystems (system:
        let
          pkgs = import nixpkgs { inherit system; };
          pkgsPerl = import nixpkgs-perl { inherit system; };
          perlPkg =
            if pkgsPerl ? perl520 then pkgsPerl.perl520
            else if pkgsPerl ? perl5_20 then pkgsPerl.perl5_20
            else if pkgsPerl ? perl522 then pkgsPerl.perl522
            else pkgsPerl.perl;
        in
        {
          default = pkgs.mkShell {
            buildInputs = with pkgs; [
              gcc49
              binutils
              gnumake
              bc
              bison
              flex
              perlPkg
              openssl
              pkg-config
              ncurses
              ncurses.dev
              elfutils
              xz
              zstd
              cpio
              rsync
              git
              wget
            ];

            shellHook = ''
              export PATH=${perlPkg}/bin:${pkgs.gcc49}/bin:$PATH
              export CC=${pkgs.gcc49}/bin/gcc
              export HOSTCC=${pkgs.gcc49}/bin/gcc
              export PERL=${perlPkg}/bin/perl
              export HOSTPERL=${perlPkg}/bin/perl
              export NIX_HARDENING_ENABLE=""
              export HARDENING_DISABLE="all"
              export NIX_CFLAGS_COMPILE=""
              export NIX_LDFLAGS=""
              export KCFLAGS="-fno-pic -fno-pie -fno-stack-protector"
              export HOSTCFLAGS="-fno-pic -fno-pie"
              if command -v pkg-config >/dev/null 2>&1; then
                nc_cflags="$(pkg-config --cflags ncursesw 2>/dev/null)"
                nc_libs="$(pkg-config --libs ncursesw 2>/dev/null)"
                if [ -z "$nc_libs" ]; then
                  nc_libs="-lncursesw -ltinfo"
                fi
                export HOSTCFLAGS="$HOSTCFLAGS $nc_cflags"
                export HOSTLDFLAGS="$nc_libs"
                export HOSTLDLIBS="$nc_libs"
                export HOSTCFLAGS_mconf="$HOSTCFLAGS $nc_cflags"
                export HOSTLDLIBS_mconf="$nc_libs"
              fi
            '';
          };
        });
    };
}

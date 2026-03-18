{
  description = "Dev shell with build tools";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-25.11";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = import nixpkgs {
          inherit system;
        };
      in {
        devShells.default = pkgs.mkShell {
          packages = with pkgs; [
            autoconf
            flex
            texinfo
            unzip
            help2man
            libtool
            ncurses
            bison
            tree
            git
            gawk
            gnumake
            gcc
            bzip2
            rsync
          ];
        };
      });
}

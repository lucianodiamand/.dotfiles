{
  description = "C project template";
  inputs.nixpkgs.url = "github:NixOS/nixpkgs/nixos-25.05";
  inputs.flake-utils.url = "github:numtide/flake-utils";
  outputs = { self, nixpkgs, flake-utils, ... }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};

        # TeX Live combinado (incluye xelatex y texfot)
        tex = pkgs.texlive.withPackages (ps: [
          ps.collection-latex
          ps.collection-latexextra
          ps.collection-fontutils
          ps.collection-fontsrecommended
          ps.collection-fontsextra
          ps.collection-xetex
          ps.collection-binextra
          ps.texfot
        ]);
      in {
        devShells.default = pkgs.mkShell {
          packages = with pkgs; [
            gcc gnumake gdb valgrind clang-tools
            tex
            python3Packages.pygments
            inconsolata liberation_ttf lmodern
            ghostscript

            # extras útiles para C:
            pkg-config
            binutils
          ];

          shellHook = ''
            echo "Entorno C listo: gcc, make, gdb, valgrind, xelatex/texfot"
            echo "Prueba: texfot --version && xelatex --version"
          '';
        };
      });
 }


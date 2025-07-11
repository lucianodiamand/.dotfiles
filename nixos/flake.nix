{
  description = "Home Manager + NixOS config with dotfiles";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-25.05";

    home-manager = {
      url = "github:nix-community/home-manager";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    rcmd.url = "github:lucianodiamand/rcmd";
  };

  outputs = { nixpkgs, home-manager, rcmd, ... }:
    let
      system = "x86_64-linux";
      pkgs = import nixpkgs {
        inherit system;
      };
    in {
      homeConfigurations.user = home-manager.lib.homeManagerConfiguration {
        inherit pkgs;
        modules = [
          ./home.nix
        ];
        extraSpecialArgs = {
          inherit rcmd;
        };
      };
    };
}


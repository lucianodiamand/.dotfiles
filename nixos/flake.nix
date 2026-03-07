{
  description = "Home Manager + NixOS config with dotfiles";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-25.05";

    home-manager = {
      url = "github:nix-community/home-manager";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    sops-nix = {
      url = "github:Mic92/sops-nix";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    secrets = {
      url = "path:/home/user/.dotfiles-secrets";
      flake = false;
    };

    rcmd.url = "github:lucianodiamand/rcmd";
  };

  outputs = { nixpkgs, home-manager, sops-nix, secrets, rcmd, ... }:
    let
      system = "x86_64-linux";
      pkgs = import nixpkgs {
        inherit system;
      };
    in {
      homeConfigurations.user = home-manager.lib.homeManagerConfiguration {
        inherit pkgs;
        modules = [
          sops-nix.homeManagerModules.sops
          ./home.nix
        ];
        extraSpecialArgs = {
          inherit rcmd secrets;
        };
      };
    };
}

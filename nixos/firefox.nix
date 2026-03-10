{ pkgs, ... }:

let
  addons = pkgs.nur.repos.rycee.firefox-addons;
in {
  programs.firefox = {
    enable = true;

    profiles.privacy = {
      isDefault = true;
      extensions = with addons; [
        ublock-origin
        privacy-badger
        clearurls
        localcdn
        cookie-autodelete
      ];
      settings = {
        "extensions.autoDisableScopes" = 0;
        "extensions.enabledScopes" = 15;
      };
    };
  };
}

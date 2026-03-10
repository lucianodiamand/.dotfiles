{ pkgs, ... }:

{
  programs.firefox = {
    enable = true;

    profiles.privacy = {
      isDefault = true;
      extensions = with pkgs.firefox-addons; [
        ublock-origin
        privacy-badger
        clearurls
        localcdn
        cookie-autodelete
      ];
    };
  };
}

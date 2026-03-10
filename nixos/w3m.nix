{ pkgs, lib, ... }:

let
  dotfiles = ../.;
in {
  home.packages = [ pkgs.w3m ];

  home.file.".w3m/config".source = "${dotfiles}/w3m/.w3m/config";
  home.file.".w3m/keymap".source = "${dotfiles}/w3m/.w3m/keymap";


  programs.zsh.initContent = lib.mkAfter ''
    # URL-encode simple (suficiente para texto normal)
    urlencode() {
      local s="$*"
      s=''${s//%/%25}
      s=''${s// /%20}
      s=''${s//\"/%22}
      s=''${s//\'/%27}
      s=''${s//\#/%23}
      s=''${s//\?/%3F}
      s=''${s//\&/%26}
      s=''${s//\+/%2B}
      s=''${s//\=/%3D}
      s=''${s//\//%2F}
      s=''${s//\:/%3A}
      s=''${s//\;/%3B}
      s=''${s//\,/%2C}
      s=''${s//\(/%28}
      s=''${s//\)/%29}
      print -r -- "$s"
    }

    # ? algo  -> búsqueda web
    web_w3m() {
      local q
      q="$(urlencode "$*")"
      w3m "https://duckduckgo.com/html/?q=$q"
    }

    alias '?'='web_w3m'
  '';
}

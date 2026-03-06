{ config, pkgs, lib, ... }:

let
  dotfiles = ../../.;
in {
  programs.direnv.enable = true;
  programs.direnv.nix-direnv.enable = true;

  programs.zsh = {
    enable = true;
    oh-my-zsh = {
      enable = true;
      theme = "bira";
      plugins = [ "git" "direnv" "sudo" ];
    };

    enableCompletion = true;
    autosuggestion.enable = true;
    syntaxHighlighting.enable = true;

    shellAliases = {
      rgrep = "grep -r";
    };
    initContent = ''
      eval "$(dircolors ~/.dir_colors)"

      # Asegurar que ~/bin esta en el PATH incluso en shells interactivos no-login
      if [[ ":$PATH:" != *":$HOME/bin:"* ]]; then
        export PATH="$HOME/bin:$PATH"
      fi

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

      # ? algo  -> busqueda web
      web_w3m() {
        local q
        q="$(urlencode "$*")"
        w3m "https://duckduckgo.com/html/?q=$q"
      }

      OPENAI_PASS_ENTRY="sites/openai-api-key"

      gpt_term() {
        if ! command -v pass >/dev/null 2>&1; then
          print -u2 "ERROR: falta pass"
          return 1
        fi
        if ! command -v jq >/dev/null 2>&1; then
          print -u2 "ERROR: falta jq"
          return 1
        fi
        if ! command -v curl >/dev/null 2>&1; then
          print -u2 "ERROR: falta curl"
          return 1
        fi

        local entry
        entry="$OPENAI_PASS_ENTRY"
        if [[ -z "$entry" ]]; then
          entry="openai/api-key"
        fi

        local api_key
        api_key="$(pass show "$entry" 2>/dev/null | head -n1)"
        if [[ -z "$api_key" ]]; then
          print -u2 "ERROR: no pude leer la key desde pass ($entry)"
          return 1
        fi

        local prompt
        prompt="$*"
        if [[ -z "$prompt" ]]; then
          print -u2 "Uso: ?? <pregunta>"
          return 1
        fi

        local tmp http_status
        tmp="$(mktemp)"

        http_status="$(curl -sS -o "$tmp" -w "%{http_code}" https://api.openai.com/v1/responses \
          -H "Content-Type: application/json" \
          -H "Authorization: Bearer $api_key" \
          -d "$(jq -n --arg input "$prompt" '{model:"gpt-5.2", input:$input}')" )"

        if [[ "$http_status" != 2* ]]; then
          print -u2 "HTTP $http_status"
          jq -r 'if .error and .error.message then .error.message else . end' < "$tmp" >&2
          rm -f "$tmp"
          return 1
        fi

        jq -r 'if .output_text then .output_text else "SIN output_text. Dump de .output:\n" + (.output|tostring) end' < "$tmp"
        rm -f "$tmp"
      }

      # ahora si: los comandos exactos
      alias '?'='web_w3m'
      alias '??'='gpt_term'
    '';
  };

  home.file.".dir_colors".source = "${dotfiles}/dir_colors/.dir_colors";
}

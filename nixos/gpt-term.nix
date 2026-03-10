{ lib, ... }:

{
  programs.zsh.initContent = lib.mkAfter ''
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

    # ahora sí: los comandos exactos
    alias '??'='gpt_term'
  '';
}

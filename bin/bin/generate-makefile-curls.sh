#!/bin/bash

> Makefile

echo '.PHONY: help' >> Makefile
echo '' >> Makefile
echo 'help:' >> Makefile
echo -e "\t@echo \"Tareas disponibles:\"" >> Makefile
echo -e "\t@grep -E '^[a-zA-Z0-9_-]+:.*?## ' Makefile | \\" >> Makefile
echo -e "\t\tawk -F ':|##' '{ printf \"  \033[36m%-20s\033[0m %s\\\\n\", \$\$1, \$\$3 }'" >> Makefile
echo '' >> Makefile

in_block=false
cmd_name=""
description=""
block_lines=()

while IFS= read -r line || [[ -n "$line" ]]; do
  if [[ "$line" =~ ^###[[:space:]]+([a-zA-Z0-9_-]+)[[:space:]]*-[[:space:]]*(.*)$ ]]; then
    if $in_block; then
      echo "$cmd_name: ## $description" >> Makefile
      for cmd in "${block_lines[@]}"; do
        echo -e "\t$cmd" >> Makefile
      done
      echo ".PHONY: $cmd_name" >> Makefile
      echo "" >> Makefile
    fi
    in_block=true
    cmd_name="${BASH_REMATCH[1]}"
    description="${BASH_REMATCH[2]}"
    block_lines=()
  elif $in_block; then
    block_lines+=("$line")
  fi
done < curl.http

# Último bloque
if $in_block; then
  echo "$cmd_name: ## $description" >> Makefile
  for cmd in "${block_lines[@]}"; do
    echo -e "\t$cmd" >> Makefile
  done
  echo ".PHONY: $cmd_name" >> Makefile
  echo "" >> Makefile
fi


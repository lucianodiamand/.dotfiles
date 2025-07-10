#!/usr/bin/env zsh

# Ruta del directorio donde están las plantillas
TEMPLATES_DIR=~/.project-templates

# Buscar y seleccionar template con fzf
template=$(find "$TEMPLATES_DIR" -maxdepth 1 -mindepth 1 -type d | fzf --prompt="Elige un template: ")

# Cancelado
[[ -z "$template" ]] && echo "Cancelado." && exit 1

# Leer nombre del nuevo proyecto
read "project_name?Nombre del nuevo proyecto: "
[[ -z "$project_name" ]] && echo "Nombre no válido. Cancelado." && exit 1

# Validar existencia previa
if [[ -e "$project_name" ]]; then
  echo "Error: Ya existe un directorio llamado '$project_name'"
  exit 1
fi

# Copiar y activar
cp -r "$template" "$project_name"
cd "$project_name" || exit 1

# Activar direnv si está instalado
if command -v direnv >/dev/null; then
  direnv allow
fi

echo "✅ Proyecto '$project_name' creado a partir de '$(basename $template)'."


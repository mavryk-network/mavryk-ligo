#!/bin/bash

# Get all files that match the pattern *_commands_registration.ml
files=$(find . -type f -name "*_commands_registration.ml")

# Rename each file
for file in $files; do
  new_file=${file%_commands_registration.ml}_comreg.ml
  mv "$file" "$new_file"
done

#!/bin/bash

# Get all files that match the pattern *_commands_registration.ml
files=$(find src -type f -name "*tp_environment*")

# Rename each file
for file in $files; do
  new_file=$(echo $file | sed s/tp_environment/tpenv/)
  mv "$file" "$new_file"
done

#!/bin/bash

# Get all files that match the pattern *_commands_registration.ml
files=$(find src -type f -name "*tpenv*")

# Rename each file
for file in $files; do
  new_file=$(echo $file | sed s/tpenv/tpenv/)
  mv "$file" "$new_file"
done

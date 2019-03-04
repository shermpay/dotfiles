#!/bin/bash
# Setup emacs configuration

# Get path of script directory
DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" >/dev/null 2>&1 && pwd )"

for file in $DIR/*; do
  ln -s "$file" "$HOME/.emacs.d/$(basename $file)" -i
done
 

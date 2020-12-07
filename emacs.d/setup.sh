#!/bin/bash
# Setup emacs configuration

# Get path of script directory
DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" >/dev/null 2>&1 && pwd )"

mkdir -p "$HOME/.emacs.d/lisp/"

for file in $DIR/lisp/*; do
  ln -sf "$file" "$HOME/.emacs.d/lisp/$(basename $file)" -i
done

ln -sf "$DIR/config.org" "$HOME/.emacs.d/config.org" -i
ln -sf "$DIR/init.el" "$HOME/.emacs.d/init.el" -i
 

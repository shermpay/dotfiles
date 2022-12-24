#!/bin/bash
# Setup emacs configuration

# Get path of script directory
DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" >/dev/null 2>&1 && pwd )"

mkdir -p "$HOME/.emacs.d/lisp/"

for file in $DIR/lisp/*; do
  ln -sf -i "$file" "$HOME/.emacs.d/lisp/$(basename $file)"
done

ln -sf -i "$DIR/config.org" "$HOME/.emacs.d/config.org"
ln -sf -i "$DIR/init.el" "$HOME/.emacs.d/init.el"
 

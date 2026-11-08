#!/bin/bash
# Script for testing emacs init

DIR=/tmp/emacs.d
ELPACA_SRC_DIR="$HOME/.emacs.d/elpaca/sources/elpaca"

EMACS=$(command -v emacs)
EMACS_EXISTS="$?"

if [[ "$EMACS_EXIST" -ne 0 ]]; then 
  echo "`emacs` command does not exist" >&2
  exit 1
fi


cleanup() {
  rm -rf "$DIR"
}

copy_configs() {
  mkdir -p "$DIR/lisp"
  cp "$HOME/.emacs.d/init.el" "$HOME/.emacs.d/early-init.el" "$DIR"
  cp "$HOME/.emacs.d/lisp/bootstrap-elpaca.el" "$DIR/lisp"
}

link_elpaca() {
  mkdir -p "$DIR/elpaca/sources"
  ln -s "$ELPACA_SRC_DIR" -t "$DIR/elpaca/sources"
}


byte_compile_elpaca() {
  "$EMACS" -Q -L "$ELPACA_SRC_DIR" --batch --eval "(byte-recompile-directory \"$ELPACA_SRC_DIR\" 0 'force)"
}

cleanup
copy_configs

# Comment out link_elpaca and byte_compile_elpaca to test with a fresh checkout of elpaca
link_elpaca
byte_compile_elpaca

"$EMACS" --init-directory="$DIR" --debug-init
exit 0

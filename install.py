#!/usr/bin/env python3


"""OS agnostic dotfiles installation script.

Requires Python3.6 and above.
"""

import sys
if sys.version_info < (3, 6):
    print("Python version too low", sys.version_info)
    sys.exit(1)

import os
import pathlib


HOME = pathlib.Path.home()


def check_dir(path):
    if not path.is_dir():
        print(f"ERROR: {path} does not exist!")
        sys.exit(2)


def symlink(dest, src):
    if dest.is_symlink():
        dest.unlink(missing_ok=True)
    dest.symlink_to(src)
    print(f"symlinked: {dest} --> {src}")

def symlink_all(dest, src):
    print(f"installing directories: {dest} --> {src}")
    if dest.exists():
        print(f"WARNING: destination directory '{dest}' exists")
        done = False
        while not done:
            resp = input("(S)kip creation/(D)elete/(Q)uit? ")
            if resp == 'S':
                break
            elif resp == 'D':
                dest.rmdir()
            elif resp == 'Q':
                sys.exit(3)
            else:
                print(f"Unknown input: '{resp}'")
                continue
            print(f"mkdir {dest}")
            dest.mkdir()
            done = True
    for path in src.iterdir():
        if path.is_file():
            symlink(pathlib.Path(dest, path.name), pathlib.Path(src, path.name))
        elif path.is_dir():
            symlink_all(pathlib.Path(dest, path.name), pathlib.Path(src, path.name))
        else:
            print(f"ERROR: Unknown file type: {path}")
            sys.exit(2)


def setup_emacs():
    print("Installing emacs configs...")
    EMACS_D = pathlib.Path(HOME, ".emacs.d")
    config_src_dir = pathlib.Path(pathlib.Path(__file__).parent, "emacs.d")
    symlink_all(EMACS_D, config_src_dir)


def main():
    # TODO: Parse args
    setup_emacs()

if __name__ == "__main__":
    main()

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
from shutil import which

HOME = pathlib.Path.home()


class color:
    HEADER = '\033[95m'
    BLUE = '\033[94m'
    CYAN = '\033[96m'
    OK = '\033[92m'
    WARN = '\033[93m'
    FAIL = '\033[91m'
    ENDC = '\033[0m'
    BOLD = '\033[1m'
    UNDERLINE = '\033[4m'


def check_dir(path):
    if not path.is_dir():
        print(f"ERROR: {path} does not exist!")
        sys.exit(2)


def symlink(dest, src):
    if dest.is_symlink():
        dest.unlink(missing_ok=True)
    dest.symlink_to(src)
    print(f"symlinked: {dest} --> {src}")


def symlink_all(dest, src, hide=False):
    print(f"installing directories: {dest} --> {src}")
    if dest.exists() and dest != HOME:
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
            done = True
    dest.mkdir(exist_ok=True)
    for path in src.iterdir():
        if path.is_file():
            dest_name = path.name
            if hide:
                dest_name = "." + dest_name
            symlink(pathlib.Path(dest, dest_name), pathlib.Path(src, path.name))
        elif path.is_dir():
            symlink_all(pathlib.Path(dest, path.name), pathlib.Path(src, path.name))
        else:
            print(f"ERROR: Unknown file type: {path}")
            sys.exit(2)


def setup_emacs():
    print(f"{color.HEADER}[BEGIN]{color.ENDC} setup_emacs")
    emacs_path = which("emacs")
    if emacs_path:
        print(f"{color.OK}[OK]{color.ENDC} Emacs installed at '{emacs_path}'.")
    else:
        print(f"{color.WARN}[NOT_FOUND]{color.ENDC} Emacs not found!")
    EMACS_D = pathlib.Path(HOME, ".emacs.d")
    config_src_dir = pathlib.Path(pathlib.Path(__file__).parent, "emacs.d")
    symlink_all(EMACS_D, config_src_dir)
    print(f"{color.CYAN}[END]{color.ENDC} setup_emacs")
    print()

def setup_sh():
    print(f"{color.HEADER}[BEGIN]{color.ENDC} setup_sh")
    zsh_path = which("zsh")
    if zsh_path:
        print(f"{color.OK}[OK]{color.ENDC} zsh installed at '{zsh_path}'.")
    else:
        print(f"{color.WARN}[NOT_FOUND]{color.ENDC} zsh not found!")
    config_src_dir = pathlib.Path(pathlib.Path(__file__).parent, "zsh")
    symlink_all(HOME, config_src_dir, hide=True)
    print(f"{color.CYAN}[END]{color.ENDC} setup_sh")
    print()

def check_env_vars():
    print(f"{color.HEADER}[BEGIN]{color.ENDC} check_env_vars")
    missing_vars = []
    MY_DRIVE = os.environ.get("MY_DRIVE")
    clr = color.CYAN
    if MY_DRIVE is None:
        MY_DRIVE = ""
    if MY_DRIVE == "":
        clr = color.WARN
        missing_vars.append("MY_DRIVE")

    print(f"{clr}MY_DRIVE='{MY_DRIVE}'{color.ENDC}")
    print(f"{color.WARN}WARNING: The following environment variables are required but not set!{color.ENDC}")
    for var in missing_vars:
        print(f"\t{var}")
    print(f"{color.CYAN}[END]{color.ENDC} check_env_vars")
    print()

def main():
    check_env_vars()
    setup_sh()
    setup_emacs()

if __name__ == "__main__":
    main()

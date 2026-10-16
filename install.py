#!/usr/bin/env python3


"""OS agnostic dotfiles installation script.

Requires Python3.11 and above.
"""

import sys
if sys.version_info < (3, 11):
    print("Python version too low", sys.version_info)
    sys.exit(1)

import argparse
import enum
import os
import pathlib
from shutil import which

HOME = pathlib.Path.home()


class color(enum.StrEnum):
    HEADER = '\033[95m'
    BLUE = '\033[94m'
    CYAN = '\033[96m'
    OK = '\033[92m'
    WARN = '\033[93m'
    FAIL = '\033[91m'
    ENDC = '\033[0m'
    BOLD = '\033[1m'
    UNDERLINE = '\033[4m'

    def __call__(self, s: str) -> str:
        return f"{self.value}{s}{color.ENDC}"

def check_dir(path):
    if not path.is_dir():
        print(f"ERROR: {path} does not exist!")
        sys.exit(2)


def symlink(dest, src):
    if dest.is_symlink():
        dest.unlink(missing_ok=True)
    dest.symlink_to(src)
    print(f"symlinked: {dest} --> {src}")


def symlink_all(dest, src, hide=False, skip_mkdir=False, make_executable=False):
    print(f"installing directories: {dest} --> {src}")
    if dest.exists() and dest != HOME:
        print(f"WARNING: destination directory '{dest}' exists")
        done = False
        while not done:
            if skip_mkdir:
                print(f"skip_mkdir={skip_mkdir}, skipping...")
                break
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
            dest_path = dest / dest_name
            src_path = src / path.name
            symlink(dest_path, src_path)
            if make_executable:
                chmod_flag = 0o755
                print("making executable via chmod", color.UNDERLINE(dest_path), color.BOLD(oct(chmod_flag)))
                dest_path.chmod(chmod_flag)

        elif path.is_dir():
            symlink_all(pathlib.Path(dest, path.name), pathlib.Path(src, path.name), hide=hide, skip_mkdir=skip_mkdir, make_executable=make_executable)
        else:
            print(f"ERROR: Unknown file type: {path}")
            sys.exit(2)

def setup(func):
    name = func.__name__
    def new_func(*args, **kwargs):
        print(color.HEADER("[BEGIN]"), name)
        func(*args, **kwargs)
        print(color.CYAN("[END]"), name)
        print()
    return new_func

@setup
def setup_emacs(args):
    emacs_path = which("emacs")
    if emacs_path:
        print(color.OK("[OK]"), f"Emacs installed at '{emacs_path}'.")
    else:
        print(color.WARN("[NOT_FOUND]"), "Emacs not found!")
    EMACS_D = pathlib.Path(HOME, ".emacs.d")
    config_src_dir = pathlib.Path(pathlib.Path(__file__).parent, "emacs.d")
    symlink_all(EMACS_D, config_src_dir, skip_mkdir=args.skip_mkdir)

@setup
def setup_sh(args):
    zsh_path = which("zsh")
    if zsh_path:
        print(color.OK("[OK]"), f"zsh installed at '{zsh_path}'.")
    else:
        print(color.WARN("[NOT_FOUND]"), f"zsh not found!")
    config_src_dir = pathlib.Path(pathlib.Path(__file__).parent, "zsh")
    symlink_all(HOME, config_src_dir, hide=True, skip_mkdir=args.skip_mkdir)

@setup
def setup_bin(args):
    config_src_dir = pathlib.Path(__file__).parent / "bin"
    symlink_all(HOME / ".local/bin", config_src_dir, skip_mkdir=args.skip_mkdir, make_executable=True)

def check_env_vars():
    print(color.HEADER("[BEGIN]"), f"check_env_vars")
    missing_vars = []
    MY_DRIVE = os.environ.get("MY_DRIVE")
    clr = color.CYAN
    if MY_DRIVE is None:
        MY_DRIVE = ""
    if MY_DRIVE == "":
        clr = color.WARN
        missing_vars.append("MY_DRIVE")

    print(clr(f"MY_DRIVE='{MY_DRIVE}'"))
    print(color.WARN("WARNING: The following environment variables are required but not set!"))
    for var in missing_vars:
        print(f"\t{var}")
    print(color.CYAN("[END]"), "check_env_vars")
    print()

def main():
    parser = argparse.ArgumentParser("install.py")
    parser.add_argument("-s", "--skip-mkdir", action="store_true", dest="skip_mkdir", help="skip directory creation")
    args = parser.parse_args(sys.argv[1:])

    check_env_vars()
    setup_sh(args)
    setup_emacs(args)
    setup_bin(args)

    print(color.BOLD(f"{sys.argv[0]} completed successfully!"))

if __name__ == "__main__":
    main()

# !/bin/sh
# Backup and symlink the corresponding bash configs

BASH_ALIASES="$HOME/.bash_aliases"
BASHRC="$HOME/.bashrc"
BASH_PROFILE="$HOME/.bash_profile"
PROFILE="$HOME/.profile"


backup_and_link()
{
    if [ $# -ne 2 ]; then
        echo "Invalid num args: $#. Expected 2."
        return
    fi

    cp $2 .
    rm $2
    ln -s $1 $2
}

backup_and_link bash_aliases "$BASH_ALIASES"
backup_and_link bashrc "$BASHRC"
backup_and_link bash_profile "$BASH_PROFILE"
backup_and_link profile "$PROFILE"

#!/bin/sh
# Sherman Pay Jing Hao
# Sunday, 02. March 2014
# Installation scripts
# TODO: 1) mkdir for appropriate directory.
# TODO: 2) Modularize with config files.
# TODO: 3) Error handling

# Checks and cds into right directory
# Has to be in ~/dotfiles

DOTFILES="${HOME}/.dotfiles"

install_config()
{
    if [ $# -ne 2 ]; then
        echo "Invalid num args: $#. Expected 2."
        return
    fi

    echo "link $2 -> $1"
    ln -Tfns "$1" "$2"
}

dir_check() {
    # try to cd into right dir
    cd "$DOTFILES"

    # Make sure in right dir
    if [ $(pwd) == "$DOTFILES" ]; then
	echo 'You are currently in ~/dotfiles. Installation will commence.'
    else
	echo "You are in $(pwd). This script requires you to clone the repo into $DOTFILES"
	exit 1
    fi
}

install_shell() {
    echo "Installing bash tools"
    # Install essential xubuntu packages
    sudo apt-get update
    sudo apt-get -y install git curl xclip tmux zsh
    install_config "${DOTFILES}/zsh/zshenv" "${HOME}/.zshenv"
    install_config "${DOTFILES}/zsh/zshrc" "${HOME}/.zshrc"
}

install_lang() {
    echo "Installing programming languages and tools"
    # Install languages and its tools
    sudo apt-get -y install clojure # Clojure
    wget https://raw.github.com/technomancy/leiningen/stable/bin/lein -O ${HOME}/bin
    chmod a+x ${HOME}/bin
    sudo apt-get -y install python3 # Python
    sudo apt-get -y install ant maven # Java
    sudo apt-get -y install clang	  # Clang

}

install_emacs() {
    echo "Installing emacs and emacs packages"
    # Install emacs and its packages
    sudo apt-get -y install emacs
    if hash emacs; then
	EMACS_CONFS=emacs-config
	echo emacs --script ${EMACS_CONFS}/package-install.el
    else
	'Emacs did not install successfully'
    fi
}

exit_script() {
    echo "You have exited the script"
    exit $1
}

for i in $@; do
    case "$1" in
	-h|--help)
	    echo "Config and installation script."
	    echo "Usage: ./install.sh [OPTION]"
	    echo "options:"
	    echo "-h, --help        outputs this help message"
	    echo "-a, --all         Sets up all tools EXCEPT cloning git repos"
	    echo "-e, --emacs       Installs gnu emacs. Downloads packages and sets it up"
	    echo "-s, --shell       Installs commonly used shell tools"
	    echo "-p, --project     Installs project repos. In ${HOME}/Programming/Projects dir"
	    echo "-l, --lang        Installs programming languages and its relevant tools"
	    echo "--FULL-SUITE      Installs the entire suite. Might take awhile."
	    exit 0
	    ;;
	--FULL-SUITE)
	    dir_check
	    echo "This will install everything and clone all git projects."
	    echo "Are you sure you want to do this? If you just want tools and config use option: -a instead"
	    printf "Please enter [yes/no] "
	    read user_input
	    if [ "$user_input"=="yes"]; then
		echo mkdir ${HOME}/Programming/Projects
		install_shell
		install_shell
		install_lang
		install_emacs
	    else
		exit_script 1
	    fi
	    ;;
	-a|--all)
	    printf "Install all tools? Please enter [y/n] "
	    read user_input
	    if [ "$user_input" == "y" ]; then
		dir_check
		install_shell
		install_lang
		install_emacs
	    else
		exit_script 1
	    fi
	    ;;
	-s|--shell)
	    install_shell
	    ;;
	-l|--lang)
	    install_lang
	    ;;
	-e|--emacs)
	    install_emacs
	    ;;
    esac
    exit 0
done

echo "Did not supply arguments. Run ./install.sh --help for arg list."

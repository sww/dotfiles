#!/bin/bash

case $OSTYPE in
    darwin* )
        if [ -d /usr/local/bin ]; then
            PATH=/usr/local/bin:$PATH
        fi

        if [ ! command -v brew > /dev/null 2>&1 ]; then
            # If brew isn't installed, then skip all the brew setup.
            break
        fi

        if [ -f $(brew --prefix)/etc/bash_completion ]; then
            . $(brew --prefix)/etc/bash_completion
        fi
    ;;
esac

#############################################
# Misc.
#############################################

export EDITOR="emacs -q"
export VISUAL="emacs -q"
alias ls='ls -G'
alias ll='ls -Gl'

export COPYFILE_DISABLE=true # Prevent OSX from writing ._ files?
export HISTIGNORE=$'[ \t]*:&:[fb]g:exit:ls:mv:rm *'
export HISTCONTROL=ignoreboth # Both ignorespace and ignoredups.
export GREP_OPTIONS="--color=auto"

#############################################
# Bash prompt stuff.
#############################################

# Colours for bash prompt.
if [ `command -v tput` ]; then
    red=$(tty -s && tput setaf 1)
    green=$(tty -s && tput setaf 2)
    yellow=$(tty -s && tput setaf 3)
    blue=$(tty -s && tput setaf 4)
    magenta=$(tty -s && tput setaf 5)
    cyan=$(tty -s && tput setaf 6)
    white=$(tty -s && tput setaf 7)
    norm=$(tty -s && tput sgr0)
    purple=$(tty -s && tput setaf 147)
else
    red="\033[1;31m";
    purple="\033[1;35m";
    cyan="\033[1;36m";
    blue="\033[1;34m";
    yellow="\033[1;33m";
    norm="\033[0;39m";
fi

if [ "$PS1" ]; then
    if [[ $UID -eq 0 ]]; then
        # If logged in as root, set red colour prompt.
        PS1="\\[$red\\]\\u@\\h:\\w #\\[$(tput sgr0)\\]"
    else
        # normal
        PS1="\[$cyan\]\u\[$norm\]@\[$blue\]\h:\[$purple\]\w\[$norm\]\$ "
    fi
    export PS1=$PS1
fi


# Python.
if [ -e "$HOME/.pythonrc" ]; then
    export PYTHONSTARTUP=$HOME/.pythonrc
fi


#############################################
# Functions.
#############################################

function up() {
    # Goes up a directory n times.
    if [ -z "$1" ]
    then
        n=1
    else
        n=$1
    fi

    cmd=""

    for (( i=0; i<n; i++ ))
    do
        cmd+="../"
    done

    cd $cmd
}

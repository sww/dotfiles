#!/bin/bash

#############################################
# Misc.
#############################################

export EDITOR="emacs -no-site-file"
export VISUAL="emacs -no-site-file"
alias df='df -h'
alias ls='ls -G'
alias ll='ls -Gl'

export COPYFILE_DISABLE=true # Prevent OSX from writing ._ files?
export HISTIGNORE=$'[ \t]*:&:[fb]g:exit:ls:rm *' # Ignore the ls command as well
export HISTCONTROL=ignoreboth # Both ignorespace and ignoredups.
export GREP_OPTIONS="--color=auto"

#############################################
# Bash prompt stuff.
#############################################

# Colours for bash prompt.
if [ `command -v tput` ]; then
    red=$(tty -s && tput setaf 1)
    cyan=$(tty -s && tput setaf 6)
    blue=$(tty -s && tput setaf 4)
    yellow=$(tty -s && tput setaf 3)
    norm=$(tty -s && tput sgr0)
else
    red="\033[1;31m";
    purp="\033[1;35m";
    cyan="\033[1;36m";
    blue="\033[1;34m";
    yellow="\033[1;33m";
    norm="\033[0;39m";
fi

if [ "$PS1" ]; then
    if [[ $UID -eq 0 ]]; then
        # you are root, set red colour prompt
        PS1="\\[$(tput setaf 1)\\]\\u@\\h:\\w #\\[$(tput sgr0)\\]"
    else
        # normal
        PS1="\[$cyan\]\u\[$norm\]@\[$blue\]\h:\w\[$norm\]\$ "
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

# find pattern in file
# Usage: f filename pattern
function f() {
    find . -name "$1" -print0 | xargs -0 grep -nH --color "${@:2}"
}

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

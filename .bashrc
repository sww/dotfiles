#############################################
# Misc.
#############################################

export EDITOR="emacs -no-site-file -f flyspell-mode"
export VISUAL="emacs -no-site-file -f flyspell-mode"
alias df='df -h'
alias ls='ls -G'
alias ll='ls -Gl'

# Prevent OSX from writing ._ files?
export COPYFILE_DISABLE=true
export HISTIGNORE=$'[ \t]*:&:[fb]g:exit:ls' # Ignore the ls command as well
export GREP_OPTIONS="--color=auto"

#############################################
# Bash prompt stuff.
#############################################

# Colours for bash prompt.
if [ `tty -s` ]; then
    red=$(tput setaf 1)
    cyan=$(tput setaf 6)
    blue=$(tput setaf 4)
    yellow=$(tput setaf 3)
    norm=$(tput sgr0)
else
    red="\033[1;31m";
    cyan="\033[1;36m";
    blue="\033[1;34m";
    yellow="\033[1;33m";
    norm="\033[1;39m";
fi

if [ "$PS1" ]; then
    PS1="\[$cyan\]\u\[$norm\]@\[$blue\]\h:\w\[$norm\]\$ "
    export PS1=$PS1
fi

#############################################
# Functions.
#############################################

# find pattern in file
# Usage: f filename pattern
function f() {
    find . -name "$1" -print0 | xargs -0 grep -nH --color "${@:2}"
}
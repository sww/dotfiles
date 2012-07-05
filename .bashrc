#############################################
# Misc.
#############################################

export EDITOR="emacs -no-site-file"
export VISUAL="emacs -no-site-file"
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
red=$(tput setaf 1)
cyan=$(tput setaf 6)
blue=$(tput setaf 4)
yellow=$(tput setaf 3)
norm=$(tput sgr0)

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

#############################################
# Functions.
#############################################

# find pattern in file
# Usage: f filename pattern
function f() {
    find . -name "$1" -print0 | xargs -0 grep -nH --color "${@:2}"
}
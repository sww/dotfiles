set -e

DIR=$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )

DEST_ROOT="$HOME"
REPO_FILES=".emacs .emacs.d/lisp .emacs.d/modes"

for repo_filename in $REPO_FILES; do
    if [ -e "$DEST_ROOT/$repo_filename" ]; then
        echo "\"$DEST_ROOT/$repo_filename\" exists already."
    else
        ln -s $(pwd)/$repo_filename $DEST_ROOT/$repo_filename
        echo "Sym linked $DEST_ROOT/$repo_filename"
    fi
done

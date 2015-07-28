set -e

DIR=$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )

FILES=".emacs .emacs.d"

for filename in $FILES; do
    REPO_VERSION=$DIR/$filename
    LOCAL_VERSION=$HOME/$filename

    if [ "$LOCAL_VERSION" -ot "$REPO_VERSION" ]; then
        cp -R $REPO_VERSION $LOCAL_VERSION
        echo "Copied $REPO_VERSION to $LOCAL_VERSION"
    else
        echo "Local version is newer than repo version!"
        read -p "Overwrite $LOCAL_VERSION with $REPO_VERSION? [yN] " yn
        case $yn in
            [Yy]* ) cp -R $REPO_VERSION $LOCAL_VERSION; echo "Copied $REPO_VERSION to $LOCAL_VERSION"
                    ;;
            *) exit;;
        esac
    fi
done

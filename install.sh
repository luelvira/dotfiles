#!/usr/bin/env bash
#!/usr/bin/env bash
################################################################################
#
# Script: Some description
# Author: Lucas Elvira Martín
# Changelog:
#
#
################################################################################

function usage {
    path=$(readlink -f $0)
    dir=$(dirname ${path})
    name=$(basename ${path})
    echo "
    usage: $name [options]

    -h  optional Print this help
    "
    exit 0
}

## Process flags
## uncomment (and customice) if you need that
#while getopt  OPTSTRING args; do
#    case $args in
#        p) echo "p is ${OPTARGS}"
#            ;;
#        h)
#            usage
#            ;;
#    esac
#done
function usage {
    path=$(readlink -f $0)
    dir=$(dirname ${path})
    name=$(basename ${path})
    echo "
    usage: `basename ${(readlink -f $0)}` [options]

    -h  optional Print this help
    "
    exit 0
}


function iterFolder {
    if [ -z $1 ]; then
        echo "Invalid argument"
        exit
    fi
    path=$(realpath ${1})
    rel_path=$(echo $path | sed "s|$_PWD/||g")
    dest=$2
    if [ -d "$path" ]; then
        for file in $(find $path); do
            if [[ -d $file && "$file" != $path ]]; then
                folder_name=$(echo $file | sed "s|$_PWD/||g")
                echo "Create folder $dest/$folder_name"
                #mkdir "$dest/$folder_name/"
                iterFolder $file $dest
            fi
            # [ -f $file ] && ln -s "$file" "$HOME/$2"
        done
    else
        echo "Error $1 is not a folder"
        exit -1
    fi
}

_PWD=$(dirname $(readlink -f $0))
echo $_PWD

echo "First symlink the bash folder"
iterFolder "bash" ""

ln -s $_PWD/.pyenv ~
ln -s $_PWD/.nodenv ~

echo "Iter over the config folder"
iterFolder "config" "~/.config"


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

#!/usr//bin/bash

if [ -z $1 ]; then
    project="/home/lucas/Documents/Org"
else
    project=$1
fi

cd $project

if [[ `git status --porcelain` ]]; then
    git add .
    git commit -m "Commit at $(date '%Y-%m-%d %H:%M:%S') from $HOSTNAME"
fi

localcommit=$(git rev-parse @)
remotcommit=$(git rev-parse @{u})
basecommit=$(git merge-base @ @{u})

if [ "$localcommit" == "$remotcommit" ]; then
    exit 0
elif [ "$localcommit" == "$basecommit" ]; then
    git pull origin main
elif [ "$remotcommit" == "$basecommit" ]; then
    git push -u origin main
else
    notify-send "Sync error" "The remote repo and the local have divergences"
    exit 1
fi

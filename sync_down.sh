#!/bin/bash

PROJECT_DEST_FOLDER="/home/gce/projects/drake-template/"
USER="gce"
PARAMS="--progress --delete -avrzhe ssh --exclude=input_all --exclude=.Rproj.user $USER@$1:$PROJECT_DEST_FOLDER ."

if [ "$#" -eq 1 ]
then
    echo "dry run"
    rsync --dry-run $PARAMS
    exit 1
fi

if [ "$#" -ge 2 ]
then
    echo "downloading..."
    rsync $PARAMS
    exit 1
fi



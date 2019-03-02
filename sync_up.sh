#!/bin/bash

PROJECT_SRC_FOLDER="drake-template"
PROJECT_DEST_FOLDER="/home/gce/projects"
USER="gce"
PARAMS="--progress --delete -avrzhe ssh --exclude=.Rproj.user ../$PROJECT_SRC_FOLDER $USER@$1:$PROJECT_DEST_FOLDER"

if [ "$#" -eq 1 ]
then
    echo "dry run"
    rsync --dry-run $PARAMS
    exit 1
fi

if [ "$#" -ge 2 ]
then
    echo "uploading..."
    rsync $PARAMS
    exit 1
fi



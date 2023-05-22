#! /bin/bash

NEW_NAME=$1
OLD_NAME=$2

git add .
git commit -m "$NEW_NAME -> $OLD_NAME"

git status

#! /bin/bash

COMMIT=$1

PATCH_FILE=$(git format-patch -p1 $COMMIT)
gsed -i 's/proto_015_PtLimaPt/proto_016_PtMumbai/g' $PATCH_FILE
gsed -i 's/proto_015_PtMumbai/proto_016_PtNairob/g' $PATCH_FILE

patch -p1 < $PATCH_FILE

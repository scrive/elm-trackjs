#!/bin/bash

VERSION_ELM_JSON=$(grep '"version"' elm.json | grep -o '[0-9\.]*')
VERSION_IN_CODE=$(tail -n 1 src/TrackJS/Internal.elm | grep -o '[0-9\.]*')

if [ "$VERSION_ELM_JSON" = "$VERSION_IN_CODE" ]; then
    echo "Versions match"
    exit 0
else
    echo "Versions do not match"
    echo "elm.json has: $VERSION_ELM_JSON"
    echo "Code has: $VERSION_IN_CODE"
    exit 1
fi

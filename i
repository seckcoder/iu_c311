#!/bin/bash

SCHEME=${SCHEME:-guile}

case $SCHEME in
    guile)
        $SCHEME -L lib
        ;;
    petite)
        $SCHEME --libdirs lib
        ;;
esac

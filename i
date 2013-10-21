#!/bin/bash

SCHEME=${SCHEME:-petite}

case $SCHEME in
    guile)
        $SCHEME -L lib
        ;;
    petite)
        $SCHEME --libdirs lib
        ;;
esac

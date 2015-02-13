#!/bin/sh

# Via https://github.com/MozVR/webvr-oculus-addon/blob/master/build/build-osx.sh

# Dumps the needed files from the Oculus SDK, such that they can be added
# to bindings-Oculus.cabal under c-sources and baked into the cabal library.
# (This only needs to be used when updating to a new version of the SDK)

SDKPATH=OculusSDK-0.4.4
SOURCES=$(find $SDKPATH/LibOVR/Src -name \*.cpp -or -name \*.mm -or -name \*.c )

# Strip some junk out
SOURCES=`echo "$SOURCES" | sed 's,[^ ]*OVR_OSX_FocusObserver.mm,,g' | sed 's,[^ ]*OVR_OSX_FocusReader.mm,,g'`

echo "$SOURCES"
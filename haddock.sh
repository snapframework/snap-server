#!/bin/sh

set -x

HADDOCK_OPTS='--html-location=http://hackage.haskell.org/packages/archive/$pkg/latest/doc/html'

cabal haddock $HADDOCK_OPTS --hyperlink-source $@

cp extra/logo.gif dist/doc/html/snap-server/haskell_icon.gif
cp extra/hscolour.css dist/doc/html/snap-server/src/

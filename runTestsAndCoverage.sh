#!/bin/sh

set -e

export LC_ALL=C
export LANG=C

rm -f testsuite.tix

./dist/build/testsuite/testsuite -j4 -a1000 $*

DIR=dist/hpc

rm -Rf $DIR
mkdir -p $DIR

EXCLUDES='Main
Paths_snap_server
Snap.Internal.Http.Server.Address.Tests
Snap.Internal.Http.Server.Parser.Tests
Snap.Internal.Http.Server.Session.Tests
Snap.Internal.Http.Server.TimeoutManager.Tests
Snap.Test.Common
System.SendFile.Tests
Test.Blackbox
Test.Common.Rot13
Test.Common.TestHandler
System.IO.Streams.Network.Internal.Address
System.IO.Streams.Network.HAProxy
'

# ^^^ not sure why I have to add the HAProxy modules here, maybe cabal sandbox?

EXCL=""

for m in $EXCLUDES; do
    EXCL="$EXCL --exclude=$m"
done

hpc markup $EXCL --destdir=$DIR testsuite >/dev/null 2>&1

rm -f testsuite.tix

cat <<EOF

Test coverage report written to $DIR.
EOF

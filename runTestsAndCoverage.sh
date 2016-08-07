#!/bin/sh

set -e

export LC_ALL=C
export LANG=C

rm -Rf testsuite.tix

# TODO How do we find the executable without knowing the version number in dist-newstyle?
./dist-newstyle/build/snap-server-1.0.0.0/build/testsuite/testsuite -j4 -a1000 $*

DIR="./dist-newstyle/hpc"

rm -Rf $DIR
mkdir -p $DIR
mkdir -p out

EXCLUDES='Main
Paths_snap_server
Snap.Internal.Http.Server.Address.Tests
Snap.Internal.Http.Server.Parser.Tests
Snap.Internal.Http.Server.Session.Tests
Snap.Internal.Http.Server.Socket.Tests
Snap.Internal.Http.Server.TimeoutManager.Tests
Snap.Test.Common
System.SendFile.Tests
Test.Blackbox
Test.Common.Rot13
Test.Common.TestHandler
'

EXCL=""

for m in $EXCLUDES; do
    EXCL="$EXCL --exclude=$m"
done

rm -rf deps

hpc markup $EXCL --destdir=$DIR testsuite

rm -f testsuite.tix

#TODO only copy hpc results if this script is called from deploy_hpc.sh
cp -r $DIR out/

cat <<EOF

Test coverage report written to $DIR.
EOF

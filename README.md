Snap Framework HTTP Server Library 0.1.1
----------------------------------------

This is the first developer prerelease of the Snap Framework HTTP Server
library.  For more information about Snap, read the `README.SNAP.md` or visit
the Snap project website at http://www.snapframework.com/.

The Snap HTTP server is a high performance, epoll-enabled, iteratee-based web
server library written in Haskell. Together with the `snap-core` library upon
which it depends, it provides a clean and efficient Haskell programming
interface to the HTTP protocol. Higher-level facilities for building web
applications (like user/session management, component interfaces, data
modeling, etc.) are not yet implemented, so this release will mostly be of
interest for those who:

  * need a fast and minimal HTTP API at roughly the same level of abstraction
    as Java servlets,

or

  * are interested in contributing to the Snap Framework project.


Building snap-server
--------------------

## Dependencies

To build the Snap HTTP server, you need to `cabal install` the `snap-core`
library (which should have come with this package).

The snap-server library can optionally use the
[libev](http://software.schmorp.de/pkg/libev.html) for high-speed, O(1)
scalable socket event processing.

If you decide to use the libev backend, you will also need to download and
install the darcs head version of the
[hlibev](http://hackage.haskell.org/package/hlibev) library:

    $ darcs get --lazy http://code.haskell.org/hlibev/
    $ cd hlibev
    $ cabal install -O2    (or "cabal install -O2 -p" for profiling support)

It has some new patches that we rely upon.


## Building snap-server

The snap-server library is built using [Cabal](http://www.haskell.org/cabal/)
and [Hackage](http://hackage.haskell.org/packages/hackage.html). Just run

    cabal install

for the "stock" version of Snap or

    cabal install -flibev

for the libev-based backend.


## Building the Haddock Documentation

The haddock documentation can be built using the supplied `haddock.sh` shell
script:

    ./haddock.sh

The docs get put in `dist/doc/html/`.


## Building the testsuite

Snap is still in its very early stages, so most of the "action" (and a big
chunk of the code) right now is centred on the test suite. Snap aims for 100%
test coverage, and we're trying hard to stick to that.

To build the test suite, `cd` into the `test/` directory and run

    $ cabal configure            # for the stock backend
    $ cabal configure -flibev    # for the libev backend
    $ cabal build

From here you can invoke the testsuite by running:

    $ ./runTestsAndCoverage.sh 


The testsuite generates an `hpc` test coverage report in `test/dist/hpc`.

The test `cabal` project also builds an executable called "pongserver" which is
a test HTTP server, hardcoded to run on port 8000:

    $ ./dist/build/pongserver/pongserver +RTS -A4M -N4 -qg0 -qb -g1

(Those are the RTS settings that give me the highest performance on my
quad-core Linux box running GHC 6.12.1, your mileage may vary.)

This server just outputs "PONG" but it is a complete example of an HTTP
application (FIXME: currently this isn't true, we need to make pongserver run
in the still-incomplete Snap monad):

    $ curl -i http://localhost:8000
    HTTP/1.1 200 OK
    Content-Length: 4
    Date: Sun, 14 Mar 2010 03:17:45 GMT
    Server: Snap/0.pre-1

    PONG

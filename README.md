Snap Framework HTTP Server Library
----------------------------------

This is the Snap Framework HTTP Server library.  For more information about
Snap, read the `README.SNAP.md` or visit the Snap project website at
http://www.snapframework.com/.

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

### Optional dependencies

The `snap-server` library can optionally use the
[libev](http://software.schmorp.de/pkg/libev.html) for high-speed, O(1)
scalable socket event processing.

If you would like SSL support, `snap-server` requires the
[gnutls](http://www.gnu.org/software/gnutls/) library.


## Building snap-server

The snap-server library is built using [Cabal](http://www.haskell.org/cabal/)
and [Hackage](http://hackage.haskell.org/packages/hackage.html). Just run

    cabal install

to get the "stock" version of Snap. If you would like to try the optional
`libev` backend, pass the `libev` flag to `cabal install`:

    cabal install -flibev

And if you would like SSL support, pass the `gnutls` flag to `cabal install`:

    cabal install -fgnutls

Note that the "`-flibev`" and "`-fgnutls`" flags are not mutually-exclusive,
and if you would like you can use them together.


## Building the Haddock Documentation

The haddock documentation can be built using the supplied `haddock.sh` shell
script:

    ./haddock.sh

The docs get put in `dist/doc/html/`.


## Building the testsuite

The `snap-server` has a fairly comprehensive test suite. To build and run it,
`cd` into the `test/` directory and run

    $ cabal configure            # for the stock backend, or..
    $ cabal configure -flibev    # for the libev backend, and/or..
    $ cabal configure -fgnutls   # for the SSL backend
    $ cabal build

From here you can invoke the testsuite by running:

    $ ./runTestsAndCoverage.sh 

The testsuite generates an `hpc` test coverage report in `test/dist/hpc`.

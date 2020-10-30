## 1.1.2.0

- behaviour change: remove maximum chunk size limitation (thanks @hvr)

- bugfix: The OpenSSL backend was broken for `network >= 3`

- allow random 1.2

- allow base16-bytestring 1.0

## 1.1.1.2

- Allow network 3.1
- Updates for GHC 8.8 and 8.10 support
- Allow clock 0.8


## 1.1.1.1
- Fix for older versions of `network` (sorry about that)

## 1.1.1.0
- Allow `network` 3.0.

## 1.1.0.0

- Add support for GHC 8.4

- Bump lower bound on base.  GHC 7.4 is no longer supported.

- Make version bounds DRY

## 1.0.3.3

Disable sendfile() on OSX (it's causing problems lately).

## 1.0.3.2

Fix a timeout cancellation bug. (#104)

## 1.0.3.1

Bump for `io-streams` 1.5.

## 1.0.3.0
### Dep bumps
  - criterion, time

### Fixes
  - TLS backend: close the socket if accept throws.

  - Use user-modified request during access logging. Fixes logging when used
    with X-Forwarded-For.

  - Don't listen on default port if unix socket is specified in the config.

  - Turn off `sendfile()` support on OSX (it doesn't seem to work correctly
    anymore).


## 1.0.2.2
### Fixes
  - Make sure we call TimeoutManager.cancel when threads die. Closes #99.

## 1.0.2.1

  - Bump io-streams.

## 1.0.2.0

### Dep bumps
  - directory 1.3, vector 0.12, time

### Improvements
  - efficiency improvement to timeout manager

### Fixes
  - blind request post params when dumping requests to error logs.


## 1.0.1.1
### Fixes
- fix sendfile() on FreeBSD.


## 1.0.1.0
### Fixes
- fix a byte counting bug.

## 1.0.0.0

snap-server 1.0.0.0 released. See git history for older changelogs.

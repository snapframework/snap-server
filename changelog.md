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

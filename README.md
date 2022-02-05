# hdav

[![GitHub CI](https://github.com/fisx/hdav/workflows/CI/badge.svg)](https://github.com/fisx/hdav/actions)
[![Hackage](https://img.shields.io/hackage/v/hdav.svg?logo=haskell)](https://hackage.haskell.org/package/hdav)
[![AGPL-3.0-only license](https://img.shields.io/badge/license-AGPL--3.0--only-blue.svg)](LICENSE)

## status

Experimental.

## remarks

This is an implementation of just enough pieces of the standards to
get some things done on a nextcloud I'm working with.  The dream is
that at some point, this will be a library useful for implementing any
WebDAV/CardDAV/CalDAV client or server, but that won't happen any time
soon.

Backwards compatibility between major versions is not guaranteed or
even a goal (yet).

There is also [DAV](http://hackage.haskell.org/package/DAV), which is
more mature than this code, but not very mature either, and hacking
the things that are missing into the existing code there was less fun
than trying my own approach.

## resources

- [WebDAV rfc](https://tools.ietf.org/html/rfc4918)
- [CardDAV rfc](https://tools.ietf.org/html/rfc6352)
- [CalDAV rfc](https://tools.ietf.org/html/rfc4791)
- [vCard rfc](https://datatracker.ietf.org/doc/html/rfc2426)
- [cool CardDAV tutorial](https://sabre.io/dav/building-a-carddav-client/)
- [cool CalDAV tutorial](https://sabre.io/dav/building-a-caldav-client/)
- [DAV](http://hackage.haskell.org/package/DAV) (another haskell library and cli tool; I stole a lot from that)
- [calendly](https://calendly.com/) (closed-source calendar app with nice features and nice UI)

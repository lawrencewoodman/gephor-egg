Gephor
======

A [Gopher](https://en.wikipedia.org/wiki/Gopher_(protocol)) server module written in [Chicken Scheme](https://call-cc.org/).

The module makes it easy to create simple or complex gopher servers by importing this module and then configuring the server as needed.  The server can act as an embedded server within a program or as a standalone server.

This aims to conform to [RFC 1436 - The Internet Gopher Protocol](https://datatracker.ietf.org/doc/html/rfc1436).  There is one exception and that is that '`./r/n`' terminates menus but isn't added to any other files to indicate EOF, instead the port is just closed once transmission has finished.  This is because clients are not consistent in how they handle this and the RFC isn't entirely clear around this requirement so it seems better to omit this rather than corrupting files.


Requirements
------------
* Chicken Scheme 5.3+
* libmagic and associated development files for the magic egg

The following eggs need to be installed:
* srfi-1
* srfi-13
* srfi-14
* srfi-18
* magic
* logfmt-logger


Logging
-------
The module uses [logfmt-logger](https://github.com/lawrencewoodman/logfmt-logger-egg) which can be configured by the calling code to decide where the logger will output and what log level will be logged.


Signal Handling
---------------
Some server modules install signal handling code within the server.  The decision has been made that this would be better handled in the calling program which can call `stop-server` to tell the server thread to stop.  This could easily be done with code such as the following:

```
;; Call this with the result of 'start-server' to set a signal handler
(define (set-termination-handler server-thread)
  (let ((termination-handler (lambda (signum)
          (stop-server server-thread)
          (exit))))
    (set-signal-handler! signal/hup termination-handler)
    (set-signal-handler! signal/int termination-handler)
    (set-signal-handler! signal/term termination-handler) ) )
```

Selectors
---------
When selectors are read from the client they are stripped of any whitespace or '/' characters at the start or end of the selector.


Testing
-------
There is a testsuite in `tests/`.  To run it:

    $ csi tests/run


Licence
-------
Copyright (C) 2024-2025 Lawrence Woodman <https://lawrencewoodman.github.io/>

This software is licensed under an MIT Licence.  Please see the file, [LICENCE.md](https://github.com/lawrencewoodman/gephor-egg/blob/master/LICENCE.md), for details.

Haroonga-httpd
===

[![Build Status](https://travis-ci.org/cosmo0920/haroonga-httpd.svg?branch=master)](https://travis-ci.org/cosmo0920/haroonga-httpd)

Yet another Groonga HTTP server written in Haskell.

## Usage

How to Use:

```bash
$ cabal install --only-dependencies
$ cabal build
$ ./dist/build/haroonga-httpd/haroonga-httpd [--port 3000] --dbpath target_database
$ curl http://127.0.0.1[:3000]/d/<groonga command>
```

## LICENSE

[LGPL-2.1](LICENSE).

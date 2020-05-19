# MMUX Cyclone Scmobj

## Introduction

This package installs libraries for  the Cyclone language; it implements
an  object  system  for  Scheme   that  provides:  simple  and  multiple
inheritance, generic  functions, multimethods.   It resembles  CLOS (the
Common Lisp  Object System),  but it  does not  implement a  meta object
protocol  (MOP).   Cyclone  is  a Scheme-to-C  compiler  supporting  the
language features as defined in the ``Revised^7 Report on Scheme''.

The library  is a  reorganisation and repackaging  of ScmObj,  an object
system for  Scheme implemented by  Dorai Sitaram.  This library  has the
main purpose  of keeping the  source code simple and  understandable, so
that it  can be taken  as starting  point for more  sophisticated object
systems.

The package targets  POSIX systems.  To run the test  suite this package
depends upon the external package: MMUX Cyclone Checks.

The package uses the GNU Autotools and it is tested, using Travis CI, on
both Ubuntu GNU+Linux systems and OS X systems.

This package  should work  with Cyclone  version 0.17.

## License

Copyright (c) 2008, 2009, 2020 Marco Maggi `mrc.mgg@gmail.com`<br/>
Copyright (c) 1996 Dorai Sitaram
All rights reserved.

This program is free software: you  can redistribute it and/or modify it
under the terms of the GNU Lesser General Public License as published by
the Free  Software Foundation, either version  3 of the License,  or (at
your option) any later version.

This program  is distributed  in the  hope that it  will be  useful, but
WITHOUT   ANY   WARRANTY;  without   even   the   implied  warranty   of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Lesser
General Public License for more details.

You should have received a copy of the GNU Lesser General Public License
along with this program.  If not, see <http://www.gnu.org/licenses/>.

## Install

To install from a proper release tarball, do this:

```
$ cd mmux-cyclone-scmobj-0.1.0
$ mkdir build
$ cd build
$ ../configure
$ make
$ make check
$ make install
```

to inspect the available configuration options:

```
$ ../configure --help
```

The Makefile is designed to allow parallel builds, so we can do:

```
$ make -j4 all && make -j4 check
```

which,  on  a  4-core  CPU,   should  speed  up  building  and  checking
significantly.

The Makefile supports the DESTDIR  environment variable to install files
in a temporary location, example: to see what will happen:

```
$ make -n install DESTDIR=/tmp/mmux-cyclone-scmobj
```

to really do it:

```
$ make install DESTDIR=/tmp/mmux-cyclone-scmobj
```

After the  installation it is  possible to verify the  installed library
against the test suite with:

```
$ make installcheck
```

From a repository checkout or snapshot  (the ones from the Github site):
we  must install  the GNU  Autotools  (GNU Automake,  GNU Autoconf,  GNU
Libtool), then  we must first run  the script `autogen.sh` from  the top
source directory, to generate the needed files:

```
$ cd mmux-cyclone-scmobj
$ sh autogen.sh

```

After this  the procedure  is the same  as the one  for building  from a
proper release tarball, but we have to enable maintainer mode:

```
$ ../configure --enable-maintainer-mode [options]
$ make
$ make check
$ make install
```

When compiling the environment  variable `CYCLONE_FLAGS` is available to
hand options to the compiler:

```
$ make CYCLONE_FLAGS='-O3'
```

Shared libraries will be installed under `$libdir`.

## Usage

Read the documentation generated from  the Texinfo sources.  The package
installs the documentation  in Info format; we can  generate and install
documentation in HTML format by running:

```
$ make html
$ make install-html
```

## Credits

The  stuff  was written  by  Marco  Maggi reorganising  and  repackaging
ScmObj, an  object system for  Scheme implemented by Dorai  Sitaram.  If
this package exists it's because of the great GNU software tools that he
uses all the time.  Cyclone is an original creation of Justin Ethier.

## Bugs, vulnerabilities and contributions

Bug  and vulnerability  reports are  appreciated, all  the vulnerability
reports  are  public; register  them  using  the  Issue Tracker  at  the
project's GitHub  site.  For  contributions and  patches please  use the
Pull Requests feature at the project's GitHub site.

## Resources

The latest release of this package can be downloaded from:

[https://bitbucket.org/marcomaggi/mmux-cyclone-scmobj/downloads](https://bitbucket.org/marcomaggi/mmux-cyclone-scmobj/downloads)

development takes place at:

[http://github.com/marcomaggi/mmux-cyclone-scmobj/](http://github.com/marcomaggi/mmux-cyclone-scmobj/)

and as backup at:

[https://bitbucket.org/marcomaggi/mmux-cyclone-scmobj/](https://bitbucket.org/marcomaggi/mmux-cyclone-scmobj/)

the documentation is available online:

[http://marcomaggi.github.io/docs/mmux-cyclone-scmobj.html](http://marcomaggi.github.io/docs/mmux-cyclone-scmobj.html)

the GNU Project software can be found here:

[http://www.gnu.org/](http://www.gnu.org/)

we can download Cyclone from:

[https://github.com/justinethier/cyclone](https://github.com/justinethier/cyclone)

The original code is available at (last checked May 13, 2020):

[http://www.ccs.neu.edu/home/dorai/scmobj/scmobj.html](http://www.ccs.neu.edu/home/dorai/scmobj/scmobj.html)

The package MMUX Cyclone Checks is available from:

[https://github.com/marcomaggi/mmux-cyclone-checks/](https://github.com/marcomaggi/mmux-cyclone-checks/)


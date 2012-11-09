=====
emacs
=====

supports emacs 24
supports gnu/linux & osx

prepare version control tools
=============================

so el-get can manage the external elisp bits and pieces upon which you
depend.

* git: http://git-scm.com/
* cvs: http://www.nongnu.org/cvs/ (for auctex)
* svn: http://subversion.apache.org/
* hg: http://mercurial.selenic.com/

prepare python
=====================

flymake needs python packages to be installed in your system or
virtualenv site-packages folder::

    $ pip install pylint pep8 pyflakes nose nose_machineout

**NOTE**: This of course requires pip: http://pypi.python.org/pypi/pip

prepare tex
===========

* arch: https://wiki.archlinux.org/index.php/TeX_Live
* ubuntu: https://help.ubuntu.com/community/LaTeX
* osx: http://www.tug.org/mactex/
* osx path tip: http://www.tug.org/mactex/faq/index.html#qm03

prepare big huge thesaurus
==========================

get an api key for http://words.bighugelabs.com/ and export the
`BIGHUGETHESAURUS` variable in your `~/.bashrc`::

    export BIGHUGETHESAURUS="apikey"

extra credit
============

launch emacs as a daemon on your distro/OS for wicked
fast startup speeds: http://www.emacswiki.org/emacs/EmacsAsDaemon


clone this repo
===============

easy mobisi::

    $ git clone https://github.com/rizumu/rizumu-emacs.git ~/.emacs.d

flight plans
============

copy my user file to one of your own. must be the same filename as
the username or hostname of your system, see
https://github.com/rizumu/rizumu-emacs/blob/master/init.el#L222  ::

    $ cp ~/.emacs.d/rizumu.el ~/.emacs.d/user.el

takeoff
=======

You need to open emacs twice and handle any prompts that pause the
installation process.


el-get is automatically installed on first launch::

    $ emacs -nw

all el-get packages are installed on second launch::

    # agree to the prompt to store abbrevs in ~/.emacs.d/abbrev_defs
    $ emacs -nw

takeoff::

    $ emacs takeoff.py
    .              _   _                       .
                   -   -           *                        .
    .       . ________________                 .
            ,(_(_)________(_)_)                     *
    o      (,| ^ Milchstrasse |                o
           | |__|GESPERRT!|___|       *
    o ((   | |  |UMLEITUNG|   |   ))           o       o     .   .
     .     | |   ueber M31    |           .
           | |________________|                               *
     *     |,'_______________,'
                   _   _                            *
                   -   - _..------.._
         *             ,'    .__,    `.
                    __/______(__)______\__   *
    .           __,'______________________`.__  .         .    *   .
   -o-      ,-'' __    __    _____   __    __ ``'.       -o-      -o-
    '      (    (__)  (__)  |_SSt_| (__)  (__)    )       '        '
        *   `-..______________________________...'        ,--.
    .             `.___________________ __,'    .        (    )
   -o-       .        `-.-.--------.-.-'       -o-        `--'
    '             *                             '                *

   $ exit

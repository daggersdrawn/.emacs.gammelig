=====
emacs
=====

a batteries included emacs config which supports emacs24, gnu/linux and osx.


prepare el-get
=============================

el-get manages the external elisp bits and pieces upon which we
depend. el-get depends upon version control tools to install packages.
you need at least git, hg and cvs to use this repo as is, but it is
better to future proof your system now. you can learn more about
el-get here: https://github.com/dimitri/el-get

* git: http://git-scm.com/
* hg: http://mercurial.selenic.com/
* cvs: http://www.nongnu.org/cvs/ (for auctex)
* bzr: http://bazaar.canonical.com/en/
* fossil: http://fossil-scm.org/
* svn: http://subversion.apache.org/
* darcs: http://darcs.net/

**NOTE**: el-get uses gnu/tar regularly and ftp on occasion.


prepare python
==============

flymake needs python packages to be installed in your system or
virtualenv site-packages folder. i've tried both and recommend using
your distro's package manager (or sudo pip) to install to your system
site-packages::

    $ pip install pylint pep8 pyflakes nose nose_machineout

**NOTE**: if you want to use pip http://pypi.python.org/pypi/pip that requires you to install pip.


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

launch emacs as a daemon http://www.emacswiki.org/emacs/EmacsAsDaemon
on your distro/OS for wicked fast startup speeds.


clone this repo
===============

easy mobisi::

    $ git clone https://github.com/rizumu/rizumu-emacs.git ~/.emacs.d


flight plans
============

copy my user file to one of your own. must  the same filename as
the username or hostname of your system, see
https://github.com/rizumu/rizumu-emacs/blob/master/init.el#L222  ::

    $ cp ~/.emacs.d/rizumu.el ~/.emacs.d/user.el


takeoff
=======

You need to open emacs twice and handle any prompts that pause the
installation process.


el-get is automatically installed on first launch::

    $ emacs -nw

**NOTE**: --nw is short for --no-window-system

all el-get packages are installed on second launch::

    # agree to the prompt to store abbrevs in ~/.emacs.d/abbrev_defs
    $ emacs -nw

off to the stars::

    $ emacs -nw .
                   _   _                            *
                   -   - _..------.._
         *             ,'    .__,    `.
                    __/______(__)______\__   *
    .           __,'______________________`.__  .         .    *   .
   -o-      ,-'' __    __    _____   __    __ ``'.       -o-      -o-
    '      (    (__)  (__)  |emacs| (__)  (__)    )       '        '
        *   `-..______________________________...'        ,--.
    .             `.___________________ __,'    .        (    )
   -o-       .        `-.-.--------.-.-'       -o-        `--'
    '             *                             '                *

   $ exit


learn keybindings
=================

emacs shortcut to open ~/.emacs.d/keybindings.org::

    C-c C-k

**NOTE**: this is a .org file, so learn that pressing <tab> expands and collapses the nodes in the tree.


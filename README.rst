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
* hg (mercurial): http://mercurial.selenic.com/
* cvs: http://www.nongnu.org/cvs/
* bzr (bazaar): http://bazaar.canonical.com/en/
* fossil: http://fossil-scm.org/
* svn (subversion): http://subversion.apache.org/
* darcs: http://darcs.net/

**NOTE**: el-get uses gnu/tar regularly and ftp on occasion.

**NOTE**: osx users can find many of these packages in homebrew http://mxcl.github.com/homebrew/

**NOTE**: darcs may be installed with cabal and likewise mercurial with python

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


prepare spellchecker
====================

install gnu/aspell http://aspell.net/

**NOTE**: osx users can find aspell in homebrew http://mxcl.github.com/homebrew/ and can install with brew install aspell --lang=en


prepare big huge thesaurus
==========================

get an api key for http://words.bighugelabs.com/ and export the
`BIGHUGETHESAURUS` variable in your `~/.bashrc`::

    export BIGHUGETHESAURUS="apikey"


extra credit
============

launch emacs as a daemon http://www.emacswiki.org/emacs/EmacsAsDaemon
on your distro/os for wicked fast startup speeds.


prepare font
============

by default, this config uses the inconsolata font
http://levien.com/type/myfonts/inconsolata.html for alternative
choices that work well see http://hivelogic.com/articles/top-10-programming-fonts/


clone this repo
===============

easy mobisi::

    $ git clone https://github.com/nillab/.emacs.d.git ~/.emacs.d


flight plans
============

copy my scratchpad.el to one of your own so that you may add
customizations. if your username was yulka, you would do the
following::

    $ mkdir ~/.emacs.d/configs/yulka/
    $ cp ~/.emacs.d/configs/rizumu/scratchpad.el ~/.emacs.d/configs/yulka/

takeoff
=======

el-get and desired recipes are automatically installed on first launch::

    $ emacs -nw

**NOTE**: --nw is short for --no-window-system

all el-get packages are installed on second launch::

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


change theme
============

if you don't like the zenburn theme installed by default, you can use
color-theme http://emacswiki.org/emacs/ColorTheme to select an
alternative. Make your choice permanent by setting load-theme
https://github.com/nillab/.emacs.d/blob/master/rizumu.el#L3


installing packages
===================

read the el-get basic usage docs to understand how you use it to manage
your elisp dependencies https://github.com/dimitri/el-get#usage

this repo installs a lot of elisp dependencies by default in the
~/.emacs.d/init.el and does not provide an easy way for you to remove
or add new ones without forking. the same is true for the
configuration files found in ~/.emacs.d/configs/cfg_something.el and
the ~/.emacs.d/snippets/ yasnippets.

a solution to this is forthcoming which will simply involve a set of
defaults in init.el with customizations to add/remove dependencies and
configs specified in your username.el or hostname.el


learn keybindings
=================

emacs shortcut to open ~/.emacs.d/keybindings.org::

    C-c C-k

**NOTE**: this is a .org file, so learn that pressing <tab> expands and collapses the nodes in the tree.


new to emacs?
=============

* learn the keybindings for what you want to do, many of the most
  useful ones are documented in ~/.emacs.d/keybindings.org

* the peepcode 'meet emacs' screencast is a great way to start:  https://peepcode.com/products/meet-emacs

* rtfm: http://www.gnu.org/software/emacs/manual/emacs.html

* emacs has builtin help, learn how to use it: http://www.gnu.org/software/emacs/manual/html_node/emacs/Help.html

* emacs keywiz is a key sequence game for improving memory (installed by default): http://www.emacswiki.org/KeyWiz

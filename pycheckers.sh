#!/bin/bash
pylint --rcfile=~/.pylintrc "$1" 2>/dev/null
pyflakes "$1"
pep8 --repeat "$1"
true

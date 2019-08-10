#!/bin/bash
# MAKE SURE YOU ARE USING WINDOWS POWERSHELL

echo 'installing scoop'
set-executionpolicy remotesigned -scope currentuser
iex (new-object net.webclient).downloadstring('https://get.scoop.sh')

echo 'installing packages via scoop'
scoop install git gcc yarn nodejs vim vimtutor curl





#!/bin/bash
# MAKE SURE YOU ARE USING WINDOWS POWERSHELL

echo 'installing scoop'
set-executionpolicy remotesigned -scope currentuser
iex (new-object net.webclient).downloadstring('https://get.scoop.sh')

echo 'installing packages via scoop'
scoop install git gcc yarn nodejs vim vimtutor curl make
scoop install cmake
scoop install ripgrep 
scoop install fzf

# uncomment if you need these installs
# echo 'installing global yarn packages'
# yarn global add netlify-cli prettier @angular/cli http-server requirejs ngrok

echo 'installing choco`
Set-ExecutionPolicy Bypass -Scope Process -Force; iex ((New-Object System.Net.WebClient).DownloadString('https://chocolatey.org/install.ps1'))
choco install poshgit ag



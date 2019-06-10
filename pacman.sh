#!/bin/bash

## Script is made to be used for ARCH BASED DISTROS with PACMAN PACKAGE MANAGER ONLY

echo "Updating before installing packages..."
sudo pacman -Sy

echo "installing fonts..."
sudo pacman -S ttf-hack ttf-fira-code adobe-source-code-pro-fonts

echo "installing neofeth..."
sudo pacman -S neofetch

echo "installing git,gdb,curl,ranger, and nnn..."
sudo pacman -S git gdb curl ranger nnn

echo "installing keyboard configuration tools..."
sudo pacman -S xorg-xmodmap xkeycaps interception-caps2esc xcape

echo "installing tools for C++..."
sudo pacman -S base-devel clang cmake llvm lldb ctags clang-format

echo "installing tools for Python..."
sudo pacman -S python3 python-pip pyenv
echo "installing python tools using pip..."
sudo pip install rope jedi flake8 autopep8 yapf pygments virtualenv virtualenvwrapper powerline-shell

echo "installing tools for Javascript..."
sudo pacman -S nodejs npm
echo "installing tools for Javascript using npm..."
sudo npm install npm@latest -g
sudo npm install -g tern eslint react react-dom create-react-app typescript @angluar/cli reactstrap webpack

echo "installing browser and video players..."
sudo pacman -S google-chrome chrome-widevine pepper-flash vlc

echo "installing emacs, vim, and cask"
sudo pacman -S emacs auctex vim
curl -fsSL https://raw.githubusercontent.com/cask/cask/master/go | python

echo "preparation for i3 configuring..."
sudo pacman -S feh compton i3-gaps i3status trash-cli






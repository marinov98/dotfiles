#!/bin/bash

## Script is made to be used for ARCH BASED DISTROS with PACMAN PACKAGE MANAGER ONLY

echo "Updating before installing packages..."
sudo pacman -Sy

echo "installing fonts..."
sudo pacman -S ttf-hack ttf-fira-code adobe-source-code-pro-fonts ttf-inconsolata

echo "installing neofeth..."
sudo pacman -S neofetch

echo "installing git,gdb,curl,ranger, and nnn..."
sudo pacman -S git gdb curl ranger nnn wireless_tools

echo "installing keyboard configuration tools..."
sudo pacman -S xorg-xmodmap xkeycaps interception-caps2esc xcape xorg-xprop xorg-xrandr arandr xorg-xrefresh

echo "installing tools for C++..."
sudo pacman -S base-devel clang cmake llvm lldb ctags clang-format

echo "x stuff"
sudo pacman -S xf86-input-libinput xf86-input-wacom xf86-video-fbdev xf86-video-intel xf86-video-vesa xfsprogs xorg-bdftopcf xorg-font-util xorg-fonts-encodings xorg-iceauth xorg-luit xorg-mkfontscale xorg-server xorg-sessreg

echo "installing tools for Python..."
sudo pacman -S python3 python-pip pyenv
echo "installing python tools using pip..."
pip install --user rope jedi flake8 autopep8 yapf pygments virtualenv virtualenvwrapper powerline-shell

echo "installing tools for Javascript..."
sudo pacman -S nodejs npm
sudo chown -R $(whoami) ~/.npm
echo "installing tools for Javascript using npm..."
npm install npm@latest -g
sudo chown -R $(whoami) /usr/local/lib/node_modules
npm i -g tern jquery @fortawesome/free-solid-svg-icons eslint angular http-server react react-dom create-react-app prop-types express @angular/core typescript requirejs reactstrap webpack

echo "installing browser and video players..."
sudo pacman -S google-chrome chrome-widevine pepper-flash vlc

echo "installing emacs, vim, and cask"
sudo pacman -S yarn emacs auctex vim
curl -fsSL https://raw.githubusercontent.com/cask/cask/master/go | python

echo "preparation for i3 configuring..."
sudo pacman -S feh compton i3-gaps i3status dmenu trash-cli

echo "Latex setup"
sudo pacman -S texlive-core texline-bin biber texlive-fontsextra texlive-bibtexextra texlive-formatsextra texlive-science

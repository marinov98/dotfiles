#!/bin/bash

## Script is made to be used for ARCH BASED DISTROS with PACMAN PACKAGE MANAGER ONLY

echo "Updating before installing packages..."
sudo pacman -Sy

echo "installing fonts..."
sudo pacman -S base base-devel font-config aspell ttf-hack ttf-fira-code adobe-source-code-pro-fonts ttf-inconsolata ttf-font-awesome

echo "installing neofetch,tmux,htop..."
sudo pacman -S neofetch tmux htop

echo "installing version control, debuggers and file mamagers.."
sudo pacman -S git gdb curl ranger wireless_tools gnupg wget the_silver_searcher

echo "power management"
# pamac stuff to be done when OS is installed
sudo pacman -Syu pamac-gtk 
pamac install tlp tlpui

echo "installing keyboard configuration tools..."
sudo pacman -S xorg-xmodmap xkeycaps interception-caps2esc xcape xorg-xprop xorg-xrandr arandr xorg-xrefresh

echo "installing tools for C++..."
sudo pacman -S clang cmake llvm lldb ctags clang-format ncurses

echo "x stuff..."
sudo pacman -S xf86-input-libinput xbacklight xf86-input-wacom xf86-video-fbdev xf86-video-intel xf86-video-vesa xfsprogs xorg-bdftopcf xorg-font-util xorg-fonts-encodings xorg-iceauth xorg-luit xorg-mkfontscale xorg-server xorg-sessreg

echo "installing tools for Python..."
sudo pacman -S python3 python-pip pyenv

echo "installing tools for Javascript..."
sudo pacman -S nodejs npm postgresql

echo "installing browser and video players..."
sudo pacman -S google-chrome chrome-widevine pepper-flash vlc blueman

echo "installing emacs, vim, and cask"
sudo pacman -S yarn emacs auctex gvim
curl -fsSL https://raw.githubusercontent.com/cask/cask/master/go | python

echo "preparation for i3 configuring..."
sudo pacman -S feh compton i3-gaps i3status i3lock dmenu rofi trash-cli

echo "Latex setup"
sudo pacman -S texlive-core texline-bin biber texlive-fontsextra texlive-bibtexextra texlive-formatsextra texlive-science

echo "Package install complete, transfering appropriate files now"
sudo chmod +x configure.sh
./configure.sh

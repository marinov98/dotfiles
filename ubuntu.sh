#!/bin/bash

## Scipt is to be used with Ubuntu linux distro OR Linux distributions that contain the apt package manager!

echo "updating..."
sudo apt update

echo "installing necessities..."
sudo apt install curl git gdb net-tools neofetch feh compton trash-cli chromium-browser htop tmux texlive-full blueman aspell xbacklight
curl -fsSL https://raw.githubusercontent.com/cask/cask/master/go | python

echo "keyboard configurations..."
sudo add-apt-repository universe
sudo apt install gnome-tweak-tool xcape 

echo "fonts..."
sudo apt install xlappearance fonts-font-awesome 
echo "displays..."
sudo apt install xrandr arandr

echo "installing programming essentials..."
sudo apt install gummi gcc build-essential exuberant-ctags clang libclang-dev lldb ctags libncurses5-dev libncursesw5-dev cmake ranger clang-format python3-pip
sudo apt install python3 clang-tools-8
sudo update-alternatives --install /usr/bin/clangd clangd /usr/bin/clangd-8 100

echo "javascript..."
sudo apt install nodejs npm 
sudo apt install postgresql-10 postgresql postgresql-contrib

echo "emacs installation..."
sudo snap install emacs --classic
sudo apt install auctex

echo "vim installation..."
sudo apt install vim vim-gtk

echo "battery management..."
sudo apt install tlp && sudo apt install tlpui

echo "Browsing and music..."
sudo apt install snapd snapd-xdg-open yarn
sudo snap install spotify postman
sudo snap install --classic heroku

echo "i3..."
sudo apt install i3 i3status i3lock suckless-tools rofi

echo "Package install complete, transfering appropriate files now"
sudo chmod +x configure.sh
./configure.sh

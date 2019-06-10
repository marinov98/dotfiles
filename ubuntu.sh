#!/bin/bash

## Scipt is to be used with Ubuntu linux distro OR Linux distributions that contain the apt package manager!

echo "updating..."
sudo apt update

echo "installing necessities (curl,git,gdb,net-tools,feh,compton,trash-cli)"
sudo apt install curl git gdb net-tools neofetch feh compton trash-cli 
curl -fsSL https://raw.githubusercontent.com/cask/cask/master/go | python

echo "keyboard configurations..."
sudo add-apt-repository universe
sudo apt install gnome-tweak-tool xcape 

echo "fonts..."
sudo apt install xlappearance fonts-font-awesome 
echo "displays..."
sudo apt install xrandr arandr

echo "installing programming essentials..."
sudo apt install gummi gcc clang libclang-dev lldb ctags libncurses5-dev libncursesw5-dev cmake ranger nnn clang-format python-pip 
echo "python tools using pip..."
sudo pip install virtualenv texlive-full 
sudo pip install rope jedi flake8 autopep8 yapf pygments virtualenvwrapper powerline-shell

echo "javascript..."
sudo apt install nodejs  npm 
echo "npm installations..."
sudo npm install npm@latest -g
sudo npm -g install tern eslint react react-dom create-react-app typescript @anglular/cli reactrap webpack

echo "emacs installation..."
sudo apt install emacs26 auctex

echo "battery management..."
sudo apt install tlp && sudo apt install tlpui

echo "Browsing and music..."
sudo apt install snapd spotify chromium-browser

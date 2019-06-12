#!/bin/bash

## Scipt is to be used with Ubuntu linux distro OR Linux distributions that contain the apt package manager!

echo "updating..."
sudo apt-get update

echo "installing necessities (curl,git,gdb,net-tools,feh,compton,trash-cli)"
sudo apt-get install curl git gdb net-tools neofetch feh compton trash-cli 
curl -fsSL https://raw.githubusercontent.com/cask/cask/master/go | python

echo "keyboard configurations..."
sudo add-apt-repository universe
sudo apt-get install gnome-tweak-tool xcape 

echo "fonts..."
sudo apt-get install xlappearance fonts-font-awesome 
echo "displays..."
sudo apt-get install xrandr arandr

echo "installing programming essentials..."
sudo apt-get install gummi gcc clang libclang-dev lldb ctags libncurses5-dev libncursesw5-dev cmake rangerclang-format python-pip 
echo "python tools using pip..."
sudo chown -R $(whoami) /usr/local/bin/
pip install --user virtualenv texlive-full 
pip install --user rope jedi flake8 autopep8 yapf pygments virtualenvwrapper powerline-shell

echo "javascript..."
sudo apt-get install nodejs npm 
sudo apt-get install postgresql-10 postgresql postgresql-contrib
echo "npm installations..."
sudo chown -R $(whoami) ~/.npm
npm install npm@latest -g
sudo chown -R $(whoami) /usr/local/lib/node_modules
npm i -g core-js tern pg jquery @fortawesome/free-solid-svg-icons sequelize-cli eslint angular @angular/cli http-server react react-dom create-react-app prop-types express @angular/core typescript tslint requirejs reactstrap webpack

echo "emacs installation..."
sudo apt-get install emacs26 auctex

echo "battery management..."
sudo apt-get install tlp && sudo apt install tlpui

echo "Browsing and music..."
sudo apt-get install snapd spotify chromium-browser

echo "i3..."
sudo apt-get install i3 i3status i3lock suckless-tools rofi

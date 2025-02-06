#!/bin/bash

## Scipt is to be used with Ubuntu linux distro OR Linux distributions that contain the apt package manager!


apt-install-all() {
  echo "updating..."
  sudo apt update

  echo "installing necessities..."
  sudo apt install curl git gdb net-tools neofetch feh compton trash-cli chromium-browser htop tmux texlive-full blueman aspell xbacklight

  echo "keyboard configurations..."
  sudo apt install fonts-firacode
  sudo apt install fonts-inconsolata

  echo "fonts..."
  sudo apt install xlappearance fonts-font-awesome 
  echo "displays..."
  sudo apt install xrandr arandr

  echo "installing programming essentials..."
  sudo apt install gcc exuberant-ctags clang libclang-dev ctags libncurses5-dev libncursesw5-dev cmake clang-format
  sudo apt install python-is-python3
  sudo apt install ripgrep fzf
  sudo apt install fd-find

  echo "battery management..."
  sudo apt install tlp && sudo apt install tlpui

  echo "Package install complete, transfering appropriate files now"
  sudo chmod +x configure.sh
}

apt-install-mini() {
  sudo apt update
  sudo apt install -y ripgrep fzf

  sudo apt install -y fonts-firacode fonts-jetbrains-mono
}

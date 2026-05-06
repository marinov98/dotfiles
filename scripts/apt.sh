#!/bin/bash

apt_install_mini() {
  sudo apt update
  sudo apt install -y curl ripgrep fzf

  sudo apt install -y fonts-firacode fonts-jetbrains-mono
}

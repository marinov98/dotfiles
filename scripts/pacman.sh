#!/bin/bash

pacman_install_mini() {
  sudo pacman -Sy
  sudo pacman -S ripgrep fzf
  sudo pacman -S ttf-fira-code ttf-jetbrains-mono
}

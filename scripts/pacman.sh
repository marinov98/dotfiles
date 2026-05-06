#!/bin/bash

pacman_install_mini() {
  sudo pacman -Sy
  sudo pacman -S curl ripgrep fzf
  sudo pacman -S ttf-fira-code ttf-jetbrains-mono
}

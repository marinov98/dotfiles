#!/bin/bash

if [[ $(uname -s) == Darwin ]]; then
  echo "Detected macOS, running brew installer..."
  source scripts/brew.sh
  brew_install_mini
elif [[ $(uname -s) == Linux ]]; then
  if [[ -f /etc/os-release ]]; then
    source /etc/os-release
    case "$ID" in
      ubuntu|debian)
        echo "Detected Debian/Ubuntu, running apt installer..."
        source scripts/apt.sh
        apt_install_mini
        ;;
      arch)
        echo "Detected Arch, running pacman installer..."
        source scripts/pacman.sh
        pacman_install_mini
        ;;
      *)
        echo "Unsupported Linux distribution: $ID"
        ;;
    esac
  fi
fi

source scripts/omni_configurator.sh
copy_dotfiles_to_home


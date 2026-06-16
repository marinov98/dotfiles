#!/bin/bash

brew_install_mini() {
  xcode-select --install
  /bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh)"
  brew install curl ripgrep fzf
  brew install font-fira-code font-jetbrains-mono
}

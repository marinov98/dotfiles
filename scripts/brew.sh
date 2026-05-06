#!/bin/bash

brew_install_mini() {
  xcode-select --install
  ruby -e "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/master/install)" < /dev/null 2> /dev/null
  brew install curl ripgrep fzf
  brew install font-fira-code font-jetbrains-mono
}

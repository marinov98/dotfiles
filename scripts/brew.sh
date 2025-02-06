#!/bin/bash
brew_install_all() {
  echo "installing xcode tools"
  xcode-select --install

  echo "Installing brew"
  ruby -e "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/master/install)" < /dev/null 2> /dev/null

  echo "installing packages with brew now"

  echo "installing terminal essentials..."
  brew install curl tmux
  brew install ripgrep
  brew install reattach-to-user-namespace

  echo "installing programming essentials..."
  brew install llvm
  brew install clang-format gcc ctags aspell
  brew install yarn
  brew install fzf
  brew install cmake node npm gdb neofetch


  echo "Installing Postgres"
  brew uninstall --force posrgresql
  rm -rf /usr/local/var/postgres
  brew install postgres postgis
}

brew_install_mini() {
  xcode-select --install
  ruby -e "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/master/install)" < /dev/null 2> /dev/null
  brew install ripgrep fzf
  brew install font-fira-code font-jetbrains-mono
}

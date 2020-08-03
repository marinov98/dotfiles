#!/bin/bash
echo "installing xcode tools"
xcode-select --install

echo "Installing brew"
ruby -e "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/master/install)" < /dev/null 2> /dev/null

echo "installing packages with brew now"

echo "installing terminal essentials..."
brew install curl ranger tmux
brew install ripgrep
brew install reattach-to-user-namespace

echo "installing programming essentials..."
brew install llvm
brew install python3
brew install clang-format gcc ctags aspell 
brew install yarn 
brew install fzf
brew install cmake node npm gdb neofetch 
brew install pyenv pyenv-virtualenv pyenv-virtualenvwrapper
brew tap heroku/brew && brew install heroku

echo "installing latex packages"
# brew install tap
# brew cask install mactex

echo "installing emacs and vim "
brew cask install emacs
brew install vim
brew unlink vim
brew install macvim
brew link macvim

echo "Installing Postgres"
brew uninstall --force posrgresql
rm -rf /usr/local/var/postgres
brew install postgres postgis

echo "all packages installed, time to copy appropriate files..."
sudo chmod +x configure.sh
./configure.sh

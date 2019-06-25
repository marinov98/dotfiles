#!/bin/bash
echo "installing xcode tools"
xcode-select --install


echo "Installing brew"
ruby -e "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/master/install)" < /dev/null 2> /dev/null

echo "installing packages with brew now"

echo "installing terminal essentials..."
brew install curl cask ranger tmux
brew install reattach-to-user-namespace

echo "installing programming essentials..."
brew install --with-toolchain llvm
brew install python3
brew install clang-format gcc ctags aspell 
brew install yarn 
brew install cmake node npm gdb neofetch 
brew install pyenv pyenv-virtualenv pyenv-virtualenvwrapper

echo "installing latex packages"
brew install tap
breww tap caskroom/cask
brew cask install mactex

echo "installing emacs and vim "
brew tap d12frosted/emacs-plus
brew install emacs-plus 
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

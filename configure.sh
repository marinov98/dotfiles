#!/bin/bash

# WARNING: script has not been tested

# BASH profiles
echo "Copying bash profiles..."
\cp .bashrc ~/
\cp .profile ~/
\cp .bash_profile ~/

# powerline-shell
echo "setting up powerline-shell"
mkdir -p ~/.config/powerline-shell && powerline-shell --generate-config > ~/.config/powerline-shell/config.json
cp .config/powerline-shell/config.json ~/.config/powerline-shell/


# FORMATTERS
echo "Copying formatters..."
cp .clang-format ~/
cp .jsbeautifyrc ~/

# EMACS
echo "setting up emacs..."
mkdir -p ~/.emacs.d
cp .emacs.d/* ~/.emacs.d/

# Vim
echo "setting up Vim..."
cp .vimrc ~/
mkdir -p ~/.vim
cp .vim/* ~/.vim/

# I3 
echo "setting up i3 window manager configuration..."
mkdir -p ~/.config
cp .config/* ~/.config/

# XORG
echo "creating xorg configuration..."
cd
mkdir -p /etc/X11
cd dotfiles/
cp /etc/X11/* /etc/X11/

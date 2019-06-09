#!/bin/bash

# WARNING: script has not been tested

# BASH profiles
echo "Copying bash profiles..."
\cp .bashrc ~/
\cp .profile ~/
\cp .bash_profile ~/
echo "bash done"

# powerline-shell
echo "setting up powerline-shell"
mkdir -p ~/.config/powerline-shell 
cp .config/powerline-shell/config.json ~/.config/powerline-shell/
echo "powerline-shell done"

# FORMATTERS
echo "Copying formatters..."
cp .clang-format ~/
cp .jsbeautifyrc ~/
echo "formatters done"

# EMACS
echo "setting up emacs..."
mkdir -p ~/.emacs.d
cp .emacs.d/* ~/.emacs.d/
echo "emacs finished"

# Vim
echo "setting up Vim..."
cp .vimrc ~/
mkdir -p ~/.vim
cp .vim/.ycm_extra_conf.py ~/.vim/
echo "vim finished"

# i3wn 
echo "setting up i3 window manager..."
mkdir -p ~/.config/i3
mkdir -p ~/.config/i3status
cp .config/i3/config ~/.config/i3/
cp .config/i3status/config ~/.config/i3status/
echo "i3 finished"

# XORG
echo "creating xorg configuration..."
cd
sudo mkdir -p /etc/X11
cd dotfiles/
sudo cp /etc/X11/* /etc/X11/
echo "xorg finished"

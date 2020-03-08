#!/bin/bash

## Script works best when directory is cloned in home not desktop

## IMPORTANT ensure appropriate packages are installed for terminal : neofetch, powerline shell, xcape before using script!

# BASH profiles
echo "Resolving bash configs..."
echo "Certain bash configurations will be copied based on the system"
if [[ $(uname -s) == Linux ]]
then
    echo "machine found to be linux"
    \cp bash/.bashrc ~/
    \cp bash/.profile ~/
    \cp keyboard/.xprofile ~/
else
    echo "Machine found to not be linux "
    \cp bash/.bash_profile ~/
fi
echo "bash done"

echo "copying git autocompletions"
cp git/git-completion.bash ~/
cp git/.gitconfig ~/

# FORMATTERS
echo "Copying formatters,tmux..."
cp code-formatters/.clang-format ~/
cp code-formatters/.prettierrc ~/
cp tmux/.tmux.conf ~/
echo "formatters & tmux done"

# EMACS
echo "setting up emacs..."
mkdir -p ~/.emacs.d
cp emacs/MarinMacs.org ~/.emacs.d/
cp emacs/init.el ~/.emacs.d/
echo "emacs finished"

# Vim
echo "setting up Vim..."
cp vim/.vimrc ~/
mkdir -p ~/.vim
cp vim/coc-settings.json ~/.vim/
echo "vim finished"

# i3wn 
echo "setting up i3 window manager,powerline-shell,alacritty and ranger ..."
mkdir -p ~/Pictures
mkdir -p ~/.config/{i3,i3status,powerline-shell,alacritty,ranger}
cp config/i3/config ~/.config/i3/
cp config/i3status/config ~/.config/i3status/
cp config/powerline-shell/config.json ~/.config/powerline-shell/
cp config/alacritty/alacritty.yml ~/.config/alacritty/alacritty.yml
cp config/ranger/rc.conf ~/.config/ranger/
cp config/i3/img/* ~/Pictures/
echo "finished"

echo "setting up compton..."
cp config/compton/compton.conf  ~/.config/
echo "Config transfer complete"

# XORG
# echo "creating xorg configuration..."
# cd
# sudo mkdir -p /etc/X11
# cd dotfiles/
# sudo cp /etc/X11/xorg.conf /etc/X11/
# echo "xocrg finished"

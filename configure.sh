#!/bin/bash

## Script works best when directory is cloned in home not desktop

## IMPORTANT ensure appropriate packages are installed for terminal : neofetch, powerline shell, xcape before using script!

# BASH profiles
echo "Resolving bash configs..."
echo "Certain bash configurations will be copied based on the system"
if [[ $(uname -s) == Linux ]]
then
    echo "machine found to be linux"
    \cp terminal/.bashrc ~/
    \cp terminal/.profile ~/
    \cp keyboard/.xprofile ~/
else
    echo "Machine found to not be linux "
    \cp terminal/.bash_profile ~/
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
cp emacs/* ~/.emacs.d/
echo "emacs finished"

# Vim
echo "setting up Vim..."
cp vim/.vimrc ~/
mkdir -p ~/.vim
cp vim/coc-settings.json ~/.vim/
echo "vim finished"

# i3wn 
echo "setting up i3 window manager,powerline-shell,alacritty and ranger ..."
mkdir -p ~/.config/{i3,i3status,powerline-shell,alacritty,ranger}
cp config/i3/config ~/.config/i3/
cp config/i3status/config ~/.config/i3status/
cp config/powerline-shell/config.json ~/.config/powerline-shell/
cp config/alacritty/alacritty.yml ~/.config/alacritty/alacritty.yml
cp config/ranger/rc.conf ~/.config/ranger/
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

echo "Files copied, now installing packages with pip and npm"

echo "python tools using pip..."
sudo chown -R $(whoami) /usr/local/bin/
pip3 install rope jedi pylint flake8 autopep8 yapf pygments virtualenv virtualenvwrapper powerline-shell pynvim

echo "npm installations..."

sudo chown -R $(whoami) ~/.npm
sudo chown -R $(whoami) /user/lib/node_modules
sudo chown -R $(whoami) /usr/local/lib/node_modules


npm i -g netlify-cli prettier @angular/cli http-server requirejs ngrok 
echo "installing language servers..."
npm i -g typescript-language-server vscode-json-languageserver vscode-html-languageserver-bin yaml-language-server vscode-css-languageserver-bin bash-language-server

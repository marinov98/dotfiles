#!/bin/bash

## Script works best when directory is cloned in home not desktop

## IMPORTANT ensure appropriate packages are installed for terminal : neofetch, powerline shell, xcape before using script!

# BASH profiles
echo "Resolving bash configs..."
echo "Certain bash configurations will be copied based on the system"
if [[ $(uname -s) == Linux ]]
then
    echo "machine found to be linux"
    \cp .bashrc ~/
    \cp .profile ~/
    
else
    echo "Machine found to not be linux "
    \cp .bash_profile ~/
fi
echo "bash done"

echo "copying git autocompletions"
cp git-completion.bash ~/

# FORMATTERS
echo "Copying formatters,tmux..."
cp .clang-format ~/
cp .prettierrc ~/
cp .jsbeautifyrc ~/
cp .tmux.conf ~/
echo "formatters & tmux done"

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
echo "setting up i3 window manager,powerline-shell,and ranger ..."
mkdir -p ~/.config/{i3,i3status,powerline-shell,ranger}
cp .config/i3/config ~/.config/i3/
cp .config/i3status/config ~/.config/i3status/
cp .config/powerline-shell/config.json ~/.config/powerline-shell/
cp .config/ranger/rc.conf ~/.config/ranger/
echo "finished"

echo "setting up compton..."
cp .config/compton.conf  ~/.config/
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
pip3 install rope jedi flake8 autopep8 yapf pygments virtualenv virtualenvwrapper powerline-shell

echo "npm installations..."

sudo chown -R $(whoami) ~/.npm
sudo chown -R $(whoami) /user/lib/node_modules
sudo chown -R $(whoami) /usr/local/lib/node_modules


npm i -g babel-cli @babel/core babel-runtime babel-preset-env @babel/generator @babel/register @babel/preset-react esm core-js@3 concurrently nodemon express-generator tern pg jquery bootstrap prettier sequelize-cli eslint heroku angular @angular/cli http-server react react-dom create-react-app prop-types express @angular/core typescript tslint requirejs reactstrap webpack graphql apollo console-info console-warn console-error

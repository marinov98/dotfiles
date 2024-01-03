#!/bin/bash

if [[ $(uname -s) == Linux ]] || [[ $(uname -s) == Darwin ]]
then
    echo "machine found to be linux or Mac will install language servers with admin permissions"
    sudo npm i -g pyright typescript typescript-language-server vscode-langservers-extracted yaml-language-server@next
    sudo pip install black
else
    echo "system not found to be linux or mac will not install language servers"
fi

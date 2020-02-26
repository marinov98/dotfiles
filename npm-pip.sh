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

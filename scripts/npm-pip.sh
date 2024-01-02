echo "python tools using pip..."
python -m pip install rope jedi pylint flake8 autopep8 yapf pygments virtualenv virtualenvwrapper powerline-shell pynvim

echo "npm installations..."
mkdir ~/.npm-global
npm config set prefix '~/.npm-global'
export PATH=~/.npm-global/bin:$PATH
source ~/.profile

npm i -g netlify-cli prettier ngrok 
echo "installing language servers..."
npm i -g typescript-language-server vscode-json-languageserver vscode-html-languageserver-bin yaml-language-server vscode-css-languageserver-bin bash-language-server

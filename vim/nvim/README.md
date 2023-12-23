# Neovim version of my vim configuration

# Nvim Lua 12/23/2023
- I have moved to the latest version of neovim and will be building my config using `lua`, all `vim` configurations have been placed in the `old` folder

# Old
## Getting Started
- `curl -fLo ~/.config/nvim/autoload/plug.vim --create-dirs https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim`
- `mkdir ~/.config/nvim/vim-plug`
- `touch ~/.config/nvim/vim-plug/plugins.vim`
- For js/ts developers run `npm i -g neovim`, for python developers run `python -m pip install neovim` or run both commands if you develop in both languages
- Copy `init.vim` file (make sure it is in ~/.config/nvim) and run `:PlugInstall`

## Experimental folder
- Includes an `init.vim` that contains packages that I am testing to see if they make my workflow better. 

instructions for installing and setting up plugins/autocomplete are the same as described in the [Vim README](https://github.com/marinov98/dotfiles/blob/master/vim/README.md)

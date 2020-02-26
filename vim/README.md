# My Vim configuration 
- I have a detailed [Emacs](<https://github.com/marinov98/dotfiles/blob/master/emacs/MarinMacs.org>) config guide where I use the more powerful extensibility of emacs and combine it with vim bindings. 

## Prereqs 
- [Nodejs](<https://nodejs.org/en/>) (need this to setup auto-complete, if you dont want it you can skip this part)

## Setup:
1. create .vimrc file in home directory then copy my config, then exit and enter vim again and everything should install. If you prefer to do this manually continue to step 2. 
    - regardless of what you do, you still have to setup [autocomplete](#Autocomplete)
2. Install [vim-plug](https://github.com/junegunn/vim-plug)
3. Either copy what I have in my [.vimrc](<https://github.com/marinov98/dotfiles/blob/master/vim/.vimrc>) or take whatever you need 
3. Save and quit .vimrc
4. Go back into the .vimrc (it's okay if there are errors) and  run `:PlugInstall`
6. [Directions for autocompletion](#Autocomplete) below 

## General settings
- Tab space 4
- html,css,js,ts modes are set to tab space of 2
- relative line number
- Syntax Highlighting
- Find-File

## Packages

### Themes
- solarized
- palenight
- jellybeans
- space-theme


### Coding Convenience
- Vim-snippets
- Vim-Utils
- vim-polygot (langauage pack for syntax highlighting)
- ctrlp (Fuzzy file, buffer, mru, tag, etc finder.)
- Nerdtree (file browser)
- super tab (use tab for autocomplete)
- vim-repeat (allows you to repeat commands)
- vim-multiple-cursors (change multiple things at the same time) 
- vim-surround (amazing new keybindings)
- clang-format (format c++ code)


### Web-dev
- Emmet 
- Css-color 
- vim-css
- auto-close tag (for html)
- Prettier (format js,ts,html,css code)

### Modeline
- lightline

### Autocomplete
- I use [Conqueror of Completion](<https://github.com/neoclide/coc.nvim>) as my auto-complete plugin which lets me use language servers for syntax checking and highlighting
- While in vim, after you have installed all packages with `PlugInstall`, run `:CocConfig` and copy and past what you see in [coc-setting.json](<https://github.com/marinov98/dotfiles/blob/master/vim/coc-settings.json>)
- language servers I use (there are many others):
    - C++: Install [clangd](<https://clang.llvm.org/extra/clangd/Installation.html>)
    - Python: 
     - `python3 -m pip install --upgrade pip setuptools wheel`
     - `python3 -m pip install 'python-language-server[all]'`
    - Javascript/TypeScript: `npm i -g typescript-language-server`
    
### Git
- git branch

# My Vim configuration 
- I also have a detailed [Emacs](<https://github.com/marinov98/dotfiles/blob/master/emacs/>) configuration guide with evil-mode
- If you are using vim inside [VS Code](https://github.com/marinov98/dotfiles/blob/master/config/vscode/settings.json) I have a configuration for that as well that tries to match the one I have for vim in terms of custom keybindings.

## Prereqs 
- [Nodejs](<https://nodejs.org/en/>) (need this to setup auto-complete, you can skip this part if you don't want it)

## Options 
 - If you want vim without plugins then copy what is inside `no-plugins-config.vim` in the `alt`
 - For a more minimal configuration, copy `minimal.vim` located in the `alt` folder 
 - For those who use or want to try **neovim** look inside the `nvim` folder

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

### Coding Convenience
- Vim-snippets (snippets)
- Vim-Utils (more snippet support)
- vim-polygot (langauage pack for syntax highlighting)
- ctrlp (Fuzzy file, buffer, mru, tag, etc finder.)
- super tab (allows you to use <Tab> for all your insert completion needs)
- vim-repeat (allows you to repeat commands)
- vim-multiple-cursors (change multiple things at the same time) 
- vim-surround (new keybindings all about "surroundings": parentheses, brackets, quotes, XML tags,etc.)
- clang-format (format c++ code)
- vim-easymotion (makes moving around text even faster)
- vim-commentary (easily comment lines)


### Web-dev
- Emmet (set of plug-ins for text editors that allow for high-speed coding and editing in HTML, XML, XSL, and other structured code formats via content assist.)
- Css-color (display hex values with color)
- vim-css (css syntax)
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
- git branch (show what branch you are on in the modeline)
- vim-fugitive (Git wrapper)

if empty(glob('~/.vim/autoload/plug.vim'))
  silent !curl -fLo ~/.vim/autoload/plug.vim --create-dirs
    \ https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim
  autocmd VimEnter * PlugInstall --sync | source $MYVIMRC
endif

"Vim plug
call plug#begin('~/.vim/plugged')

"snippets and utensils
Plug 'SirVer/ultisnips' | Plug 'honza/vim-snippets'

" Solarized Theme
Plug 'https://github.com/altercation/vim-colors-solarized'

" Base 16
Plug 'chriskempson/base16-vim'

" JellyBeans Theme
Plug 'https://github.com/nanotech/jellybeans.vim'

"Gruvbox Theme 
Plug 'https://github.com/morhetz/gruvbox'

"Cobalt2 Theme
Plug 'gertjanreynaert/cobalt2-vim-theme'

" Zenburn Theme
Plug 'https://github.com/jnurmine/Zenburn'

"Dracula
Plug 'dracula/vim', { 'as': 'dracula' }

Plug 'https://github.com/itchyny/vim-gitbranch'

"Linter
Plug 'w0rp/ale'
let g:ale_sign_error = '●' " Less aggressive than the default '>>'
let g:ale_sign_warning = '.'
let g:ale_lint_on_enter = 0 " Less distracting when opening a new file

" Modeline
Plug 'itchyny/lightline.vim'
let g:lightline = {
	  \ 'colorscheme': 'jellybeans'	,
      \ 'active': {
      \   'left': [ [ 'mode', 'paste' ],
      \             [ 'gitbranch', 'readonly', 'filename', 'modified' ] ]
      \ },
      \ 'component_function': {
      \   'gitbranch': 'gitbranch#name'
      \ },
      \ }


" Surround
Plug 'https://github.com/tpope/vim-surround'

" Prettier
Plug 'prettier/vim-prettier', {
  \ 'do': 'yarn install',
  \ 'for': ['javascript', 'typescript', 'css', 'less', 'scss', 'json', 'graphql', 'markdown', 'vue', 'yaml', 'html'] }

" CSS - Color
Plug 'https://github.com/skammer/vim-css-color'
let g:cssColorVimDoNotMessMyUpdatetime = 1

" Vim-repeat
Plug 'https://github.com/tpope/vim-repeat'

" Multiple Cursor 
Plug 'terryma/vim-multiple-cursors'

let g:multi_cursor_use_default_mapping=0

let g:multi_cursor_start_word_key      = '<C-n>'
let g:multi_cursor_select_all_word_key = '<A-n>'
let g:multi_cursor_start_key           = 'g<C-n>'
let g:multi_cursor_select_all_key      = 'g<A-n>'
let g:multi_cursor_next_key            = '<C-n>'
let g:multi_cursor_prev_key            = '<C-p>'
let g:multi_cursor_skip_key            = '<C-x>'
let g:multi_cursor_quit_key            = '<Esc>'

" Fizzy file find
Plug 'junegunn/fzf', { 'dir': '~/.fzf', 'do': './install --all' }
Plug 'junegunn/fzf.vim'

" Nerd Tree
Plug 'https://github.com/scrooloose/nerdtree'
" If you want to have nerd tree toggled always:
"autocmd vimenter * NERDTree
map <C-t> :NERDTreeToggle<CR>

" Ranger
Plug 'francoiscabrol/ranger.vim'

" Bar Utility
Plug 'majutsushi/tagbar'
Plug 'ervandew/supertab'

" Auto Close Braces
Plug 'https://github.com/jiangmiao/auto-pairs'

" Auto-complete SHOULD BE INSTALLED MANUALLY
" Plug 'Valloric/YouCompleteMe', {'do': './install.py'}
" Make YCM not use tab so that UtilSnippets work properly
let g:ycm_global_ycm_extra_conf = "~/.vim/.ycm_extra_conf.py"

" make YCM compatible with UltiSnips (using supertab)
let g:ycm_key_list_select_completion = ['<C-n>', '<Down>']
let g:ycm_key_list_previous_completion = ['<C-p>', '<Up>']
let g:SuperTabDefaultCompletionType = '<C-n>'

" better key bindings for UltiSnipsExpandTrigger
let g:UltiSnipsExpandTrigger = "<tab>"
let g:UltiSnipsJumpForwardTrigger = "<tab>"
let g:UltiSnipsJumpBackwardTrigger = "<s-tab>"

"Web-dev
Plug 'https://github.com/pangloss/vim-javascript'
Plug 'mxw/vim-jsx'
Plug 'https://github.com/hail2u/vim-css3-syntax'
Plug 'alvan/vim-closetag'
" Emmet 
Plug 'mattn/emmet-vim'
let g:user_emmet_leader_key='<Tab>'
let g:user_emmet_settings = {
  \  'javascript.jsx' : {
    \      'extends' : 'jsx',
    \  },
  \}



"Github integration
Plug 'jreybert/vimagit'
Plug 'airblade/vim-gitgutter'

call plug#end()
 
" Syntax highlighting 
syntax enable

" Color theme
set background=dark
colorscheme jellybeans

" Indentation
set tabstop=4
set softtabstop=4
set shiftwidth=4
set noexpandtab

" line numbers
set number relativenumber
set nu rnu 

" directory navigations
set foldenable
set incsearch
set hlsearch
set showmatch
set wildmenu
augroup project
    autocmd!
    autocmd BufRead,BufNewFile *.h,*.c set filetype=c.doxygen
augroup END

" auto-close brackets
let g:AutoPairsFlyMode = 0
let g:AutoPairsShortcutBackInsert = '<M-b>'

" Enable ctags
set tags=tags

" Make backspace work
set bs=2

"Enable the use of the mouse
set mouse=a

" Copy/Paste from anywhere
"set clipboard=unnamed
set clipboard=unnamedplus

" Commands
command! W :w

" Force Minimum window length
set winwidth=110

" Always show statusline
set laststatus=2

" no need to show extra modes
set noshowmode

" Disable Opacity while in vim
"highlight Normal ctermbg=Black
"highlight NonText ctermbg=Black

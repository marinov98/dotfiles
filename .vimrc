if empty(glob('~/.vim/autoload/plug.vim'))
  silent !curl -fLo ~/.vim/autoload/plug.vim --create-dirs
    \ https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim
  autocmd VimEnter * PlugInstall --sync | source $MYVIMRC
endif

call plug#begin('~/.vim/plugged')
""""""""""""""""""""""
""""""" THEMES
""""""""""""""""""""""
Plug 'https://github.com/altercation/vim-colors-solarized'
Plug 'drewtempelmeyer/palenight.vim'
Plug 'https://github.com/nanotech/jellybeans.vim'
Plug 'croaker/mustang-vim'
""""""""""""""""""""""
""""""" File Managment
""""""""""""""""""""""
Plug 'ctrlpvim/ctrlp.vim'
""""""""""""""""""""""
""""""" CODING
""""""""""""""""""""""
Plug 'SirVer/ultisnips' | Plug 'honza/vim-snippets'
Plug 'dense-analysis/ale'
Plug 'prettier/vim-prettier', {
  \ 'do': 'yarn install',
  \ 'for': ['javascript', 'typescript', 'css', 'less', 'scss', 'json', 'graphql', 'markdown', 'vue', 'yaml', 'html'] }
Plug 'https://github.com/rhysd/vim-clang-format'
Plug 'https://github.com/scrooloose/nerdtree'
Plug 'https://github.com/jiangmiao/auto-pairs'
Plug 'ervandew/supertab'
Plug 'Valloric/YouCompleteMe', {'do': './install.py'}
Plug 'https://github.com/gcmt/taboo.vim'
""""""""""""""""""""""
""""""" MODELINE
""""""""""""""""""""""
Plug 'itchyny/lightline.vim'
""""""""""""""""""""""
""""""" VIM UTILITY
""""""""""""""""""""""
Plug 'https://github.com/tpope/vim-repeat'
Plug 'https://github.com/tpope/vim-surround'
Plug 'terryma/vim-multiple-cursors'
""""""""""""""""""""""
""""""" WEB-DEV
""""""""""""""""""""""
Plug 'https://github.com/pangloss/vim-javascript'
Plug 'mxw/vim-jsx'
Plug 'https://github.com/hail2u/vim-css3-syntax'
Plug 'alvan/vim-closetag'
Plug 'mattn/emmet-vim'
Plug 'https://github.com/ap/vim-css-color'
""""""""""""""""""""""
""""""" GITHUB
""""""""""""""""""""""
Plug 'https://github.com/itchyny/vim-gitbranch'
call plug#end()


""""""""""""""""""""""
""""""" LINTER
""""""""""""""""""""""
let g:ale_linters = {
\   'javascript': ['eslint'],
\   'python': ['flake8','pylint']
\}

let g:ale_linters_explicit = 1
let g:ale_sign_error = '●' " Less aggressive than the default '>>'
let g:ale_sign_warning = '.'
let g:ale_lint_on_enter = 0 " Less distracting when opening a new file
let g:ale_open_list = 0
let g:ale_pattern_options = {
\ '\.min\.js$': {'ale_linters': [], 'ale_fixers': []},
\ '\.min\.css$': {'ale_linters': [], 'ale_fixers': []},
\}
let g:ale_lint_delay = 500
let g:ale_lint_on_text_changed = 'normal'

""""""""""""""""""""""
""""""" PRETTIER 
""""""""""""""""""""""
let g:prettier#exec_cmd_path = "~/.prettierrc"
let g:prettier#quickfix_enabled = 0
let g:prettier#autoformat = 0
autocmd BufWritePre *.js,*.jsx,*.mjs,*.ts,*.tsx,*.css,*.less,*.scss,*.json,*.graphql,*.md,*.vue,*.yaml,*.html Prettier

" Clang-Format
autocmd FileType c,cpp,objc ClangFormatAutoEnable
		
" Nerd Tree
map <C-t> :NERDTreeToggle<CR>
map <C-f> :NERDTree<CR>
let NERDTreeMinimalUI = 1
let NERDTreeDirArrows = 1
let NERDTreeAutoDeleteBuffer = 1
let g:NERDTreeDirArrowExpandable = '▸'
let g:NERDTreeDirArrowCollapsible = '▾'

""""""""""""""""""""""
""""""" AUTO-COMPLETE 
""""""""""""""""""""""
let g:ycm_global_ycm_extra_conf = "~/.vim/.ycm_extra_conf.py"
let g:ycm_enable_diagnostic_signs = 0 

" make YCM compatible with UltiSnips (using supertab)
let g:ycm_key_list_select_completion = ['<C-n>', '<Down>']
let g:ycm_key_list_previous_completion = ['<C-p>', '<Up>']
let g:SuperTabDefaultCompletionType = '<C-n>'

" better key bindings for UltiSnipsExpandTrigger
let g:UltiSnipsExpandTrigger = "<tab>"
let g:UltiSnipsJumpForwardTrigger = "<tab>"
let g:UltiSnipsJumpBackwardTrigger = "<s-tab>"

" Modeline
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

" Multiple-cursors
let g:multi_cursor_use_default_mapping=0
let g:multi_cursor_start_word_key      = '<C-n>'
let g:multi_cursor_select_all_word_key = '<A-n>'
let g:multi_cursor_start_key           = 'g<C-n>'
let g:multi_cursor_select_all_key      = 'g<A-n>'
let g:multi_cursor_next_key            = '<C-n>'
let g:multi_cursor_prev_key            = '<C-p>'
let g:multi_cursor_skip_key            = '<C-x>'
let g:multi_cursor_quit_key            = '<Esc>'

" CSS and Emmet
autocmd FileType css set omnifunc=csscomplete#CompleteCSS
" make it work for scss files
autocmd BufNewFile,BufRead *.scss set ft=scss.css
let g:user_emmet_settings = {
  \  'javascript.jsx' : {
    \      'extends' : 'jsx',
    \  },
  \}
let g:user_emmet_install_global = 0
autocmd FileType html,js,jsx,css EmmetInstall

" auto-close brackets
let g:AutoPairsFlyMode = 0
let g:AutoPairsShortcutBackInsert = '<M-b>'

"CtrlP
let g:ctrlp_map = '<C-a>'
let g:ctrlp_custom_ignore = '\v[\/]\.(git|hg|svn)$'
let g:ctrlp_custom_ignore = {
  \ 'dir':  '\v[\/]\.(git|hg|svn)$',
  \ 'file': '\v\.(exe|so|dll)$',
  \ 'link': 'some_bad_symbolic_links',
  \ }
let g:ctrlp_user_command = ['.git', 'cd %s && git ls-files -co --exclude-standard']
 
""""""""""""""""""""""
""""""" BASICS
""""""""""""""""""""""
syntax enable
set nocompatible

" Color theme
set background=dark
colorscheme palenight

" Indentation
set tabstop=2
set softtabstop=2
set shiftwidth=2
set noexpandtab

" line numbers and tabs
set number relativenumber
set nu rnu 
set completeopt-=preview
set guioptions-=e
set sessionoptions+=tabpages,globals

" directory navigations and settings
set foldenable
set incsearch
set hlsearch
set showmatch
set wildmenu
set noswapfile
set tags=tags
set bs=2
set mouse=a
set clipboard=unnamedplus
set winwidth=125
set laststatus=2
set noshowmode
set t_Co=256

" Don't offer to open certain files/directories
set wildignore+=*.bmp,*.gif,*.ico,*.jpg,*.png,*.ico
set wildignore+=*.pdf,*.psd
set wildignore+=node_modules/*,bower_components/*
set wildignore+=*/tmp/*,*.so,*.swp,*.zip     " MacOSX/Linux

augroup project
    autocmd!
    autocmd BufRead,BufNewFile *.h,*.c set filetype=c.doxygen
augroup END

" Commands
command! W :w

 " Search stops highlighting after you press ESC twice
nnoremap <C-s> :noh<return>
nnoremap Y y$

if empty(glob('~/.vim/autoload/plug.vim'))
  silent !curl -fLo ~/.vim/autoload/plug.vim --create-dirs
    \ https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim
  autocmd VimEnter * PlugInstall --sync | source $MYVIMRC
endif

call plug#begin('~/.vim/plugged')
""""""""""""""""""""""
""""""" THEMES:
""""""""""""""""""""""
"Plug 'https://github.com/nanotech/jellybeans.vim'
Plug 'liuchengxu/space-vim-dark'
""""""""""""""""""""""
""""""" File Search:
""""""""""""""""""""""
Plug 'ctrlpvim/ctrlp.vim'
""""""""""""""""""""""
""""""" CODING:
""""""""""""""""""""""
Plug 'SirVer/ultisnips'| Plug 'honza/vim-snippets'
Plug 'prettier/vim-prettier', {
  \ 'do': 'yarn install',
  \ 'for': ['javascript', 'typescript', 'css', 'less', 'scss', 'json', 'graphql', 'markdown', 'vue', 'yaml', 'html'] }
Plug 'https://github.com/rhysd/vim-clang-format'
Plug 'https://github.com/jiangmiao/auto-pairs'
Plug 'ervandew/supertab'
Plug 'neoclide/coc.nvim', {'branch': 'release'}
Plug 'octol/vim-cpp-enhanced-highlight'
""""""""""""""""""""""
""""""" MODELINE:
""""""""""""""""""""""
Plug 'itchyny/lightline.vim'
""""""""""""""""""""""
""""""" VIM UTILITY:
""""""""""""""""""""""
Plug 'https://github.com/tpope/vim-repeat'
Plug 'https://github.com/tpope/vim-surround'
Plug 'terryma/vim-multiple-cursors'
""""""""""""""""""""""
""""""" WEB DEV:
""""""""""""""""""""""
Plug 'https://github.com/pangloss/vim-javascript'
Plug 'mxw/vim-jsx'
Plug 'https://github.com/hail2u/vim-css3-syntax'
Plug 'alvan/vim-closetag'
Plug 'mattn/emmet-vim'
Plug 'https://github.com/ap/vim-css-color'
Plug 'leafgarland/typescript-vim'
Plug 'peitalin/vim-jsx-typescript'
""""""""""""""""""""""
""""""" GITHUB:
""""""""""""""""""""""
Plug 'https://github.com/itchyny/vim-gitbranch'
call plug#end()

""""""""""""""""""""""
""""""" PRETTIER:
""""""""""""""""""""""
let g:prettier#exec_cmd_path = "~/.prettierrc"
let g:prettier#quickfix_enabled = 0
let g:prettier#autoformat = 0
autocmd BufWritePre *.js,*.jsx,*.mjs,*.ts,*.tsx,*.css,*.less,*.scss,*.json,*.graphql,*.md,*.vue,*.yaml,*.html Prettier

" Clang-Format
autocmd FileType c,cpp,objc ClangFormatAutoEnable
		
" File Browsing netrw
let g:netrw_banner=0 " Disable annoying banner
let g:netrw_browse_split=4 " open in prior window
let g:netrw_altv=1          " open splits to the right
let g:netrw_liststyle=3     " tree view
let g:netrw_winsize = 25    " take 25 % of window
let g:netrw_list_hide=netrw_gitignore#Hide()
let g:netrw_list_hide.=',\(^\|\s\s\)\zs\.\S\+'
let g:NetrwIsOpen=0

function! ToggleNetrw()
    if g:NetrwIsOpen
        let i = bufnr("$")
        while (i >= 1)
            if (getbufvar(i, "&filetype") == "netrw")
                silent exe "bwipeout " . i 
            endif
            let i-=1
        endwhile
        let g:NetrwIsOpen=0
    else
        let g:NetrwIsOpen=1
        silent Lexplore
    endif
endfunction

" Add your own mapping. For example:
noremap <silent> <C-T> :call ToggleNetrw()<CR>

""""""""""""""""""""""
""""""" AUTOCOMPLETE:
""""""""""""""""""""""
" better key bindings for UltiSnipsExpandTrigger
let g:UltiSnipsExpandTrigger = "<tab>"
let g:UltiSnipsJumpForwardTrigger = "<tab>"
let g:UltiSnipsJumpBackwardTrigger = "<s-tab>"

""""""""" COC CONFIG
let g:coc_global_extensions = [
			\ 'coc-eslint', 'coc-prettier',
			\ 'coc-tsserver', 'coc-tslint','coc-html',
			\'coc-css', 'coc-json', 'coc-python', 'coc-yaml']
" if hidden is not set, TextEdit might fail.
set hidden

" Some servers have issues with backup files, see #649
set nobackup
set nowritebackup

" You will have bad experience for diagnostic messages when it's default 4000.
set updatetime=300

" don't give |ins-completion-menu| messages.
set shortmess+=c

" always show signcolumns
set signcolumn=yes
"""""""""""

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
""""""" BASICS:
""""""""""""""""""""""
syntax enable
set nocompatible

" Color theme
set background=dark
colorscheme space-vim-dark

" Indentation
set tabstop=4
set softtabstop=4
set shiftwidth=4
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


" MODE SPECIFIC SETTINGS:
autocmd FileType html setlocal ts=2 sts=2 sw=2
autocmd FileType css setlocal ts=2 sts=2 sw=2
autocmd FileType javascript setlocal ts=2 sts=2 sw=2
autocmd FileType jsx setlocal ts=2 sts=2 sw=2
autocmd FileType typescript setlocal ts=2 sts=2 sw=2
autocmd FileType tsx setlocal ts=2 sts=2 sw=2


" Don't offer to open certain files/directories
set wildignore+=*.bmp,*.gif,*.ico,*.jpg,*.png,*.ico
set wildignore+=*.pdf,*.psd
set wildignore+=node_modules/*,bower_components/*
set wildignore+=*/tmp/*,*.so,*.swp,*.zip     " MacOSX/Linux

" FINDING FILES:

" Search down into subfolders
" Provides tab-completion for all file-related tasks
set path+=**

augroup project
    autocmd!
    autocmd BufRead,BufNewFile *.h,*.c set filetype=c.doxygen
augroup END

" Commands
command! W :w

 " Search stops highlighting after you press ESC twice
nnoremap <C-s> :noh<return>
nnoremap Y y$

""""""""""""""""""""""
"""""" LEADER KEY:
""""""""""""""""""""""
map <SPACE> <Leader>

""""""""""""""""""""""
""""""" BASICS:
""""""""""""""""""""""

" auto-close brackets
let g:AutoPairsFlyMode = 0
let g:AutoPairsShortcutBackInsert = '<M-b>'

set nobackup
set nowritebackup

" Better display for messages
set cmdheight=2

" You will have bad experience for diagnostic messages when it's default 4000.
set updatetime=300

" don't give |ins-completion-menu| messages.
set shortmess+=c

" always show signcolumns
set signcolumn=yes

syntax enable
set nocompatible

"Color theme
set background=dark
colorscheme industry

" Cursor line
augroup CursorLine
  au!
  au VimEnter,WinEnter,BufWinEnter * setlocal cursorline
  au WinLeave * setlocal nocursorline
augroup END

"indentation
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

" Don't offer to open certain files/directories
set wildignore+=*.bmp,*.gif,*.ico,*.jpg,*.png,*.ico
set wildignore+=*.pdf,*.psd
set wildignore+=node_modules/*,bower_components/*
set wildignore+=*/tmp/*,*.so,*.swp,*.zip     " MacOSX/Linux
"
augroup project
     autocmd!
         autocmd BufRead,BufNewFile *.h,*.c set filetype=c.doxygen
         augroup END

" Commands
command! W :w

"""""""""""""""""""""""
""""" PERSONAL BINDINGS:
"""""""""""""""""""""""
nnoremap <leader>n :noh<CR>
nnoremap Y y$
nnoremap <leader>s :w<CR>
nnoremap <leader>q :q<CR>
nnoremap <leader>Q :q!<CR>

"""""""""""""""""""""""
""""" WINDOW MANAGEMENT:
"""""""""""""""""""""""
" Fix splitting
set splitbelow splitright

nnoremap <leader>j <C-W>j
nnoremap <leader>k <C-W>k
nnoremap <leader>l <C-W>l
nnoremap <leader>h <C-W>h

" Shortcut split opening
nnoremap <leader>2 :split<CR>
nnoremap <leader>3 :vsplit<CR>>

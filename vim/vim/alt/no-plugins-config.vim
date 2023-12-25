""""""""""""""""""""""
"""""" LEADER KEY:
""""""""""""""""""""""
map <SPACE> <Leader>

""""""""""""""""""""""
"""""" SETTINGS:
""""""""""""""""""""""

" if hidden is not set, TextEdit might fail.
set hidden

" Some servers have issues with backup files, see #649
set nobackup
set nowritebackup
set showcmd

set updatetime=300

" don't give |ins-completion-menu| messages.
set shortmess+=c

" always show signcolumns
set signcolumn=yes

syntax enable
set nocompatible

"Color theme
set background=dark
colorscheme slate

"indentation
set tabstop=4
set softtabstop=4
set shiftwidth=4
set noexpandtab
set smartindent
set smartcase

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
set cursorline
set noswapfile
set tags=tags
set bs=2
set mouse=a
"set clipboard=unnamed "Windows
set clipboard=unnamedplus " Linux and Mac
set laststatus=2
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

" Search down into subfolders
" Provides tab-completion for all file-related tasks
set path+=**

"" NETRW:
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

noremap <silent> <leader>tn :call ToggleNetrw()<CR>

"""""""""""""""""""""""
""""" PERSONAL BINDINGS:
"""""""""""""""""""""""
nnoremap <leader>n :noh<CR>
nnoremap Y y$
nnoremap <leader>s :w<CR>
nnoremap <leader>k :tabclose<CR>
nnoremap <leader>q :q<CR>
nnoremap <leader>Q :q!<CR>

nnoremap <leader>cs :terminal<CR>
nnoremap <leader>cm :%s//gc<Left><Left><Left>
nnoremap <leader>* :%s/<C-r><C-w>/<C-r><C-w>/gI<Left><Left><Left>
nnoremap <leader>x :!chmod +x %<CR>
" command to grep word under cursor
nnoremap <leader>/ :grep! "\b<C-R><C-W>\b"<CR>:cw<CR>

nnoremap <leader>bb <C-^>
nnoremap <leader>bt :tabnew<CR>
nnoremap <leader>bn :tabnext<CR>
nnoremap <leader>bp :tabprevious<CR>


"""""""""""""""""""""""
""""" WINDOW MANAGEMENT:
"""""""""""""""""""""""
" Fix splitting
set splitbelow splitright

" Auto close braces
inoremap " ""<left>
inoremap ' ''<left>
inoremap ( ()<left>
inoremap [ []<left>
inoremap { {}<left>
inoremap {<CR> {<CR>}<ESC>O
inoremap {;<CR> {<CR>};<ESC>O

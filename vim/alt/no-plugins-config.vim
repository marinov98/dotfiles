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

" if hidden is not set, TextEdit might fail.
set hidden

" Some servers have issues with backup files, see #649
set nobackup
set nowritebackup
set showcmd

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
set guicursor+=a:blinkon0
set mouse=a
set clipboard=unnamed "Windows
"set clipboard=unnamedplus " Linux and Mac
set laststatus=2
set noshowmode
set t_Co=256



" MODE SPECIFIC SETTINGS:
autocmd BufEnter *.tsx set filetype=typescript
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
"
augroup project
     autocmd!
         autocmd BufRead,BufNewFile *.h,*.c set filetype=c.doxygen
         augroup END


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

" Add your own mapping. For example:
noremap <silent> <leader>t :call ToggleNetrw()<CR>

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

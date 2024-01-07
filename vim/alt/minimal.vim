call plug#begin('~/.vim/plugged')
""""""" THEMES:
Plug 'morhetz/gruvbox'
""""""" File Search:
Plug 'ctrlpvim/ctrlp.vim'
""""""" CODING:
Plug 'jiangmiao/auto-pairs'
""""""" MODELINE:
Plug 'itchyny/lightline.vim'
""""""" VIM UTILITY:
Plug 'tpope/vim-repeat'
Plug 'tpope/vim-surround'
Plug 'tpope/vim-commentary'
""""""" WEB DEV:
Plug 'alvan/vim-closetag'
Plug 'mattn/emmet-vim'
Plug 'sheerun/vim-polyglot'
call plug#end()

""""""""""""""""""""""
"""""" LEADER KEY:
""""""""""""""""""""""
map <SPACE> <Leader>

" Modeline
let g:lightline = {
	\ 'colorscheme': 'gruvbox',
    \ 'active': {
    \   'left': [ 
	\		[ 'mode',  ],
	\		['filename', 'modified']
    \           ],
	\   'right': [
	\		["lineinfo"],
	\		["readonly", "fileformat", "fileencoding"],
	\   ]
    \ },
    \ }

"CtrlP
let g:ctrlp_map = '<leader>f'

" Ripgrep 
if executable('rg')
	set grepprg=rg\ --color=never
	let g:ctrlp_user_command = 'rg %s --files --hidden --color=never --glob "!.git/"'
	let g:ctrlp_use_caching = 0
elseif executable('ag') " Try The Silver Searcher if ripgrep not found
	" Use ag over grep
	set grepprg=ag\ --nogroup\ --nocolor
	let g:ctrlp_user_command = 'ag %s -l --nocolor -g ""'
	 " ag is fast enough that CtrlP doesn't need to cache
	let g:ctrlp_use_caching = 0

else " Else use old configuration
	let g:ctrlp_custom_ignore = '\v[\/]\.(git|hg|svn)$'
	let g:ctrlp_custom_ignore = {
		  \ 'dir':  '\v[\/]\.(git|hg|svn)$',
		  \ 'file': '\v\.(exe|so|dll)$',
		  \ 'link': 'some_bad_symbolic_links',
		  \ }
	let g:ctrlp_user_command = ['.git', 'cd %s && git ls-files -co --exclude-standard']
endif
" bind K to grep word under cursor
nnoremap K :grep! "\b<C-R><C-W>\b"<CR>:cw<CR>


""""""""""""""""""""""
""""""" BASICS:
""""""""""""""""""""""

" auto-close brackets
let g:AutoPairsFlyMode = 0
let g:AutoPairsShortcutBackInsert = '<M-b>'


" don't give |ins-completion-menu| messages.
set shortmess+=c

" always show signcolumns
set signcolumn=yes

syntax enable
set nocompatible

"Color theme
set background=dark
let g:gruvbox_contrast_dark = "hard"
colorscheme gruvbox


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
set hidden
set nobackup
set nowritebackup
set showcmd
set cursorline
set foldenable
set incsearch
set nohlsearch
set noerrorbells
set showmatch
set wildmenu
set noswapfile
set updatetime=300
set tags=tags
set bs=2
set mouse=a
"set clipboard=unnamed "Windows
set clipboard=unnamedplus " Linux and Mac
set laststatus=2
set noshowmode
set t_Co=256

" Search down into subfolders
" Provides tab-completion for all file-related tasks
set path+=**

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
nnoremap <leader>k :bd<CR>
nnoremap <leader>x :tabclose<CR>
nnoremap <leader>q :q<CR>
nnoremap <leader>Q :q!<CR>

nnoremap <leader>cs :terminal<CR>
nnoremap <leader>cm :%s//gc<Left><Left><Left>
vnoremap <leader>cm :s//gc<Left><Left><Left>
nnoremap <leader>* :%s/<C-r><C-w>/<C-r><C-w>/gI<Left><Left><Left>
nnoremap <leader>/ :grep! "\b<C-R><C-W>\b"<CR>:cw<CR>
nnoremap <leader>X :!chmod +x %<CR>
nnoremap <leader>uc  :! 

nnoremap <leader>a <C-^>
nnoremap <leader>bn :bnext<CR>
nnoremap <leader>bp :bprevious<CR>

nnoremap <leader>tt :tabnew<CR>
nnoremap <leader>tj :tabnext<CR>
nnoremap <leader>tk :tabprevious<CR>

"""""""""""""""""""""""
""""" WINDOW MANAGEMENT:
"""""""""""""""""""""""
" Fix splitting
set splitbelow splitright

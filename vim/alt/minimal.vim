call plug#begin('~/.vim/plugged')

""""""""""""""""""""""
""""""" THEMES:
""""""""""""""""""""""
Plug 'morhetz/gruvbox'
""""""""""""""""""""""
""""""" File Search:
""""""""""""""""""""""
Plug 'ctrlpvim/ctrlp.vim'
""""""""""""""""""""""
""""""" CODING:
""""""""""""""""""""""
Plug 'easymotion/vim-easymotion'
Plug 'https://github.com/jiangmiao/auto-pairs'
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
Plug 'alvan/vim-closetag'
Plug 'mattn/emmet-vim'
Plug 'sheerun/vim-polyglot'
""""""""""""""""""""""
""""""" GITHUB:
""""""""""""""""""""""
Plug 'https://github.com/itchyny/vim-gitbranch'
call plug#end()



""""""""""""""""""""""
"""""" LEADER KEY:
""""""""""""""""""""""
map <SPACE> <Leader>

" Modeline
let g:lightline = {
  \ 'colorscheme': 'seoul256',
    \ 'active': {
    \   'left': [ [ 'mode', 'paste' ],
    \             [ 'gitbranch', 'readonly', 'filename', 'modified' ] ]
    \ },
    \ 'component_function': {
    \   'gitbranch': 'gitbranch#name'
    \ },
    \ }

"CtrlP
let g:ctrlp_map = '<leader>f'

" Ripgrep 
if executable('rg')
	set grepprg=rg\ --color=never
	let g:ctrlp_user_command = 'rg %s --files --color=never --glob ""'
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
colorscheme gruvbox

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
"set clipboard=unnamed "Windows
set clipboard=unnamedplus " Linux and Mac
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

" Easy Motion:
map  <Leader>c <Plug>(easymotion-bd-f)
nmap <Leader>c <Plug>(easymotion-overwin-f)

" Move to line
map <Leader>L <Plug>(easymotion-bd-jk)
nmap <Leader>L <Plug>(easymotion-overwin-line)

" Move to word
map  <Leader>w <Plug>(easymotion-bd-w)
nmap <Leader>w <Plug>(easymotion-overwin-w)


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

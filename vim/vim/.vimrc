if empty(glob('~/.vim/autoload/plug.vim'))
  silent !curl -fLo ~/.vim/autoload/plug.vim --create-dirs
    \ https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim
  autocmd VimEnter * PlugInstall --sync | source $MYVIMRC
endif

call plug#begin('~/.vim/plugged')
""""""" THEMES:
Plug 'morhetz/gruvbox'
""""""" File Search:
Plug 'ctrlpvim/ctrlp.vim'
""""""" CODING:
Plug 'SirVer/ultisnips'| Plug 'honza/vim-snippets'
Plug 'rhysd/vim-clang-format'
Plug 'jiangmiao/auto-pairs'
Plug 'ervandew/supertab'
Plug 'neoclide/coc.nvim', {'branch': 'release'}
Plug 'octol/vim-cpp-enhanced-highlight'
Plug 'sheerun/vim-polyglot'
""""""" MODELINE:
Plug 'itchyny/lightline.vim'
""""""" GITHUB:
Plug 'itchyny/vim-gitbranch'
Plug 'tpope/vim-fugitive'
""""""" VIM UTILITY:
Plug 'tpope/vim-repeat'
Plug 'tpope/vim-surround'
Plug 'tpope/vim-commentary'
""""""" WEB DEV:
Plug 'alvan/vim-closetag'
Plug 'mattn/emmet-vim'
Plug 'ap/vim-css-color'
call plug#end()

""""""""""""""""""""""
"""""" LEADER KEY:
""""""""""""""""""""""
map <SPACE> <Leader>

""""" Indentation:
set tabstop=4
set softtabstop=4
set shiftwidth=4
set noexpandtab
set smartindent
set smartcase

"""" PREFFERED DEFAULTS:
set foldenable
set incsearch
set nohlsearch
set showmatch
set wildmenu

set noswapfile
set noerrorbells

set tags=tags
set bs=2 " make backspace work
set mouse=a " use mouse in vim

set cursorline " highlight current row
set clipboard=unnamedplus " allow copy and pasting anymore
set laststatus=2 " show modeline
set noshowmode
set t_Co=256 " Make colors work with tmux
set number relativenumber
set nu rnu 
set completeopt-=preview
set guioptions-=e
set sessionoptions+=tabpages,globals

""""""" MODE SPECIFIC SETTINGS:
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
set wildignore+=*/tmp/*,*.so,*.swp,*.zip " MacOSX/Linux

" FINDING FILES:

" Search down into subfolders
" Provides tab-completion for all file-related tasks
set path+=**

""""""""""""""""""""""
""""" PERSONAL BINDINGS:
""""""""""""""""""""""
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

nnoremap <leader>a <C-^>
nnoremap <leader>bn :bnext<CR>
nnoremap <leader>bp :bprevious<CR>

nnoremap <leader>tt :tabnew<CR>
nnoremap <leader>tj :tabnext<CR>
nnoremap <leader>tk :tabprevious<CR>

nnoremap <Leader>+ :vertical resize +5<CR>
nnoremap <Leader>- :vertical resize -5<CR>

" Fix splitting
set splitbelow splitright

""""""""""""""""""""""
"""""" THEME:
""""""""""""""""""""""
set background=dark
colorscheme gruvbox

""""""""""""""""""""""
"""""" PACKAGE CONFIG
""""""""""""""""""""""
""""""" PRETTIER:
command! -nargs=0 Prettier :call CocAction('runCommand', 'prettier.formatFile')

" Clang-Format
autocmd FileType c,cpp,objc ClangFormatAutoEnable

"""""""  NETRW:
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


""""""""""""""""""""""
""""""" AUTOCOMPLETE:
""""""""""""""""""""""

" better key bindings for UltiSnipsExpandTrigger
let g:UltiSnipsExpandTrigger = "<tab>"
let g:UltiSnipsJumpForwardTrigger = "<tab>"
let g:UltiSnipsJumpBackwardTrigger = "<s-tab>"

""""""""" COC CONFIG:
let g:coc_global_extensions = [
			\ 'coc-eslint', 'coc-prettier', 'coc-java',
			\ 'coc-tsserver', 'coc-tslint','coc-html',
			\'coc-css', 'coc-json', 'coc-python', 'coc-yaml']

" Below settings are reccommended by coc
set hidden

set nobackup
set nowritebackup
set showcmd

set updatetime=900
set shortmess+=c
set signcolumn=yes

" Use <c-space> to trigger completion.
inoremap <silent><expr> <c-space> coc#refresh()

nmap <silent> [d <Plug>(coc-diagnostic-prev)
nmap <silent> ]d <Plug>(coc-diagnostic-next)

" GoTo code navigation.
nmap <silent> gd <Plug>(coc-definition) 
nmap <silent> gy <Plug>(coc-type-definition)
nmap <silent> gi <Plug>(coc-implementation)
nmap <silent> gr <Plug>(coc-references)
nmap <silent> gs :CocSearch 

" Symbol renaming.
nmap <leader>lc <Plug>(coc-rename)
""""""""""" AUTOCOMPLETE END

""""""""""" Modeline:
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

" CSS and Emmet
autocmd FileType css set omnifunc=csscomplete#CompleteCSS
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

""""""""""" CtrlP:
let g:ctrlp_map = '<leader>f'

if executable('rg')
	" Let ctrlp utilize ripgrep
	set grepprg=rg\ --color=never
	let g:ctrlp_user_command = 'rg %s --files --color=never --glob ""'
	let g:ctrlp_use_caching = 0
elseif executable('ag') " Try The Silver Searcher if ripgrep not found
	" Use ag over grep
	set grepprg=ag\ --nogroup\ --nocolor
	let g:ctrlp_user_command = 'ag %s -l --nocolor -g ""'
	let g:ctrlp_use_caching = 0
else " Else use grep configuration
	let g:ctrlp_custom_ignore = '\v[\/]\.(git|hg|svn)$'
	let g:ctrlp_custom_ignore = {
				  \ 'dir':  '\v[\/]\.(git|hg|svn)$',
				  \ 'file': '\v\.(exe|so|dll)$',
				  \ 'link': 'some_bad_symbolic_links',
				  \ }
	let g:ctrlp_user_command = ['.git', 'cd %s && git ls-files -co --exclude-standard']
endif



""""""""""" FUGITIVE:
nmap <leader>gg :G<CR>
nmap <leader>gs :Git
nmap <leader>gc :Gcommit<CR>
nmap <leader>gj :diffget //3<CR>
nmap <leader>gf :diffget //2<CR>
" You can change the below command to :Gpush if you don't have to type any credentials when pushing
nmap <leader>gp :terminal git push

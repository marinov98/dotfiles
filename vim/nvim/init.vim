if empty(glob('~/.config/nvim/autoload/plug.vim'))
  silent !curl -fLo ~/.config/nvim/autoload/plug.vim --create-dirs
      \ https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim
 " autocmd VimEnter * PlugInstall --sync | source $MYVIMRC
endif

call plug#begin('~/.config/nvim/autoload/plugged')
""""""" THEMES:
Plug 'morhetz/gruvbox'
""""""" File Search:
Plug 'ctrlpvim/ctrlp.vim'
""""""" CODING:
Plug 'easymotion/vim-easymotion'
Plug 'SirVer/ultisnips'| Plug 'honza/vim-snippets'
Plug 'prettier/vim-prettier', {
  \ 'do': 'yarn install',
  \ 'for': ['javascript', 'typescript', 'css', 'less', 'scss', 'json', 'graphql', 'markdown', 'vue', 'yaml', 'html'] }
Plug 'https://github.com/rhysd/vim-clang-format'
Plug 'https://github.com/jiangmiao/auto-pairs'
Plug 'ervandew/supertab'
Plug 'neoclide/coc.nvim', {'branch': 'release'}
Plug 'octol/vim-cpp-enhanced-highlight'
""""""" MODELINE:
Plug 'itchyny/lightline.vim'
""""""" GITHUB:
Plug 'https://github.com/itchyny/vim-gitbranch'
""""""" VIM UTILITY:
Plug 'https://github.com/tpope/vim-repeat'
Plug 'https://github.com/tpope/vim-surround'
Plug 'terryma/vim-multiple-cursors'
""""""" WEB DEV:
Plug 'alvan/vim-closetag'
Plug 'mattn/emmet-vim'
Plug 'https://github.com/ap/vim-css-color'
Plug 'sheerun/vim-polyglot'
call plug#end()

source $HOME/.config/nvim/vim-plug/plugins.vim

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
set hlsearch
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
nnoremap <leader>q :q<CR>
nnoremap <leader>Q :q!<CR>

" Fix splitting
set splitbelow splitright

nnoremap <leader>j <C-W>j
nnoremap <leader>k <C-W>k
nnoremap <leader>l <C-W>l
nnoremap <leader>h <C-W>h

" Shortcut split opening
nnoremap <leader>2 :split<CR>
nnoremap <leader>3 :vsplit<CR>>

""""""""""""""""""""""
"""""" THEME:
""""""""""""""""""""""
set background=dark
colorscheme gruvbox

""""""""""""""""""""""
"""""" PACKAGE CONFIG
""""""""""""""""""""""

""""""" PRETTIER:
let g:prettier#exec_cmd_path = "~/.prettierrc"
let g:prettier#quickfix_enabled = 0
let g:prettier#autoformat = 0
autocmd BufWritePre *.js,*.jsx,*.mjs,*.ts,*.tsx,*.css,*.less,*.scss,*.json,*.graphql,*.md,*.vue,*.yaml,*.html Prettier

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

noremap <silent> <leader>t :call ToggleNetrw()<CR>


""""""""""""""""""""""
""""""" AUTOCOMPLETE:
""""""""""""""""""""""

" better key bindings for UltiSnipsExpandTrigger
let g:UltiSnipsExpandTrigger = "<tab>"
let g:UltiSnipsJumpForwardTrigger = "<tab>"
let g:UltiSnipsJumpBackwardTrigger = "<s-tab>"

""""""""" COC CONFIG:
let g:coc_global_extensions = [
			\ 'coc-eslint', 'coc-prettier',
			\ 'coc-tsserver', 'coc-tslint','coc-html',
			\'coc-css', 'coc-json', 'coc-python', 'coc-yaml']

" Below settings are reccommended by coc
set hidden

set nobackup
set nowritebackup
set showcmd

set updatetime=300
set shortmess+=c
set signcolumn=yes

" Use <c-space> to trigger completion.
inoremap <silent><expr> <c-space> coc#refresh()

nmap <silent> [g <Plug>(coc-diagnostic-prev)
nmap <silent> ]g <Plug>(coc-diagnostic-next)

" GoTo code navigation.
nmap <silent> gd <Plug>(coc-definition) 
nmap <silent> gy <Plug>(coc-type-definition)
nmap <silent> gi <Plug>(coc-implementation)
nmap <silent> gr <Plug>(coc-references)

" Symbol renaming.
nmap <leader>r <Plug>(coc-rename)
""""""""""" AUTOCOMPLETE END

""""""""""" Easy Motion:
map  <Leader>c <Plug>(easymotion-bd-f)
nmap <Leader>c <Plug>(easymotion-overwin-f)

" Move to line
map <Leader>L <Plug>(easymotion-bd-jk)
nmap <Leader>L <Plug>(easymotion-overwin-line)

" Move to word
map  <Leader>w <Plug>(easymotion-bd-w)
nmap <Leader>w <Plug>(easymotion-overwin-w)

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


" bind K to grep word under cursor
nnoremap K :grep! "\b<C-R><C-W>\b"<CR>:cw<CR>
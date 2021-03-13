call plug#begin('~/.config/nvim/autoload/plugged')
""""""" THEMES:
Plug 'rakr/vim-one'
""""""" File Search:
Plug 'junegunn/fzf', { 'do': { -> fzf#install()  }  }
Plug 'junegunn/fzf.vim'
""""""" CODING:
Plug 'easymotion/vim-easymotion'
Plug 'SirVer/ultisnips'| Plug 'honza/vim-snippets'
Plug 'rhysd/vim-clang-format'
Plug 'jiangmiao/auto-pairs'
Plug 'ervandew/supertab'
Plug 'neoclide/coc.nvim', {'branch': 'release'}
Plug 'octol/vim-cpp-enhanced-highlight'
Plug 'sheerun/vim-polyglot'
""""""" MODELINE:
Plug 'vim-airline/vim-airline'
Plug 'vim-airline/vim-airline-themes'
Plug 'edkolev/tmuxline.vim'
""""""" GITHUB:
Plug 'itchyny/vim-gitbranch'
Plug 'tpope/vim-fugitive'
""""""" VIM UTILITY:
Plug 'tpope/vim-repeat'
Plug 'tpope/vim-surround'
Plug 'terryma/vim-multiple-cursors'
Plug 'tpope/vim-commentary'
""""""" WEB DEV:
Plug 'alvan/vim-closetag'
Plug 'mattn/emmet-vim'
Plug 'ap/vim-css-color'
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
	\'coc-css', 'coc-json', 'coc-python','coc-yaml']

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
nmap <silent> [g <Plug>(coc-diagnostic-prev)
nmap <silent> ]g <Plug>(coc-diagnostic-next)

" GoTo code navigation.
nmap <silent> gd <Plug>(coc-definition) 
nmap <silent> gy <Plug>(coc-type-definition)
nmap <silent> gi <Plug>(coc-implementation) 
nmap <silent> gr <Plug>(coc-references)
nmap <silent> gs :CocSearch 

" Symbol renaming.
nmap <leader>r <Plug>(coc-rename)

""""""""""" AUTOCOMPLETE END

""""""""""" Easy Motion
map  <Leader>c <Plug>(easymotion-bd-f)
nmap <Leader>c <Plug>(easymotion-overwin-f)

" Move to line
map <Leader>L <Plug>(easymotion-bd-jk)
nmap <Leader>L <Plug>(easymotion-overwin-line)

" Move to word
map  <Leader>w <Plug>(easymotion-bd-w)
nmap <Leader>w <Plug>(easymotion-overwin-w)

"""" PREFFERED DEFAULTS:
set foldenable
set showmatch

set noswapfile
set noerrorbells

set tags=tags
set bs=2 " make backspace work

set cursorline " highlight current row
set clipboard=unnamedplus " allow copy and pasting anywhere
set noshowmode
set t_Co=256 " Make colors work with tmux
set number relativenumber
set nu rnu 
set completeopt-=preview
set guioptions-=e
set guioptions-=r
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
nnoremap <Leader>+ :vertical resize +5<CR>
nnoremap <Leader>- :vertical resize -5<CR>

" Fix splitting
set splitbelow splitright

nnoremap <leader>j <C-W>j
nnoremap <leader>k <C-W>k
nnoremap <leader>l <C-W>l
nnoremap <leader>h <C-W>h
nnoremap <leader>b <C-^>
nnoremap <leader>o <C-W>o

" Shortcut split opening
nnoremap <leader>2 :split<CR>
nnoremap <leader>3 :vsplit<CR>>

""""""""""""""""""""""
"""""" THEME:
""""""""""""""""""""""

if (has("nvim"))
	let $NVIM_TUI_ENABLE_TRUE_COLOR=1
endif
if (has("termguicolors"))
	set termguicolors
endif

colorscheme one
set background=dark

""""""""""""""""""""""
""""" PACKAGE CONFIG
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

noremap <silent> <leader>t :call ToggleNetrw()<CR>

"""" Modeline

let g:airline_theme='one'
let g:airline#extensions#tabline#enabled = 1
let g:airline#extensions#tabline#formatter = 'default'
let g:airline_powerline_fonts = 1

"""" Emmett

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

""""""""""" FZF:
nnoremap <silent> <leader>f :Files<CR>
nnoremap <silent> <leader>/ :Rg<CR>

" bind K to grep word under cursor
nnoremap K :grep! "\b<C-R><C-W>\b"<CR>:cw<CR>

""""""""""" FUGITIVE:
nmap <leader>gg :G<CR>
nmap <leader>gs :Git
nmap <leader>gc :Gcommit<CR>
nmap <leader>gj :diffget //3<CR>
nmap <leader>gf :diffget //2<CR>
" You can change the below command to :Gpush if you don't have to type any credentials when pushing
nmap <leader>gp :terminal git push

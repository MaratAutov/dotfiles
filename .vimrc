" 
let mapleader = " "

nmap \q :nohlsearch<CR>

nmap j gj
nmap k gk

" move between open buffers.
nmap <C-n> :bnext<CR>
nmap <C-p> :bprev<CR>

vnoremap > >gv
vnoremap < <gv

nmap <C-j> <C-W>j
nmap <C-k> <C-W>k
nmap <C-h> <C-W>h
nmap <C-l> <C-W>l

"
" Options
"
set nocompatible

set autoindent
set autoread
set autowrite
fixdel
set backspace=eol,indent,start
set cindent
set cinkeys-=0#
set cino=
set commentstring=\ \ #%s
set copyindent
set directory-=.
set encoding=utf8
set expandtab
set fileformats=unix,dos,mac

set hidden
set history=200
set hlsearch
set ignorecase
set infercase
set laststatus=2
set linebreak
set number relativenumber
set visualbell t_vb=
set nowritebackup
set ruler
set scroll=4
set scrolloff=15
set shiftround
set shiftwidth=4
set shortmess+=A
set showbreak=
set showmatch
set sidescrolloff=3
set noswapfile
set smartcase
set softtabstop=4
set tabstop=4
set textwidth=120
set wildmenu
set wildmode=list:longest,full

set nowrap
set mouse=a
set path+=**
set cb+=unnamed
"
syntax enable
filetype plugin indent on
"
" Colors
set background=dark
colorscheme PaperColor

"
if has("gui_running")
	set guioptions-=T
endif

silent! nohlsearch

let g:netrw_banner=0
let g:netrw_browse_split=4
let g:netrw_altv=1
let g:netrw_liststyle=3
let g:netrw_list_hide=',\(^\|\s\s\)\zs\.\S\+'



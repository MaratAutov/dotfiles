" 
let mapleader = " "

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
set cursorline
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
set scroll=4 scrolloff=4
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
set autochdir

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

" Netrw {{{
let g:netrw_banner=0
let g:netrw_browse_split=4
let g:netrw_altv=1
let g:netrw_liststyle=3 " tree view
let g:netrw_list_hide=',\(^\|\s\s\)\zs\.\S\+'
" }}}

" Incsearch {{{
let g:incsearch#auto_nohlsearch = 1                   " auto unhighlight after searching
let g:incsearch#do_not_save_error_message_history = 1 " do not store incsearch errors in history
let g:incsearch#consistent_n_direction = 1            " when searching backward, do not invert meaning of n and N
set incsearch

" remap
map <C-z> <Nop>
nmap \q :nohlsearch<CR>

nmap j gj
nmap k gk

vnoremap > >gv
vnoremap < <gv
nnoremap <Tab>   >>
nnoremap <S-Tab> <<
vnoremap <Tab>   >><Esc>gv
vnoremap <S-Tab> <<<Esc>gv

nmap <C-j> <C-W>j
nmap <C-k> <C-W>k
nmap <C-h> <C-W>h
nmap <C-l> <C-W>l
nmap <C-n> :bnext<CR>

" ctrl-p
let g:ctrlp_map = '<c-p>'
let g:ctrlp_cmd = 'CtrlP'
let g:ctrlp_working_path_mode = 'ra'
let g:ctrlp_custom_ignore = {
	\ 'dir':  '\v[\/]\.(git|hg|svn)$',
	\ 'file': '\v\.(exe|so|dll)$',
	\ 'link': 'SOME_BAD_SYMBOLIC_LINKS',
	\ }

if has("win64") || has("win32") || has("win16") 
    set wildignore+=*\\tmp\\*,*.swp,*.zip,*.exe  " Windows
    let g:ctrlp_user_command = 'dir %s /-n /b /s /a-d'
else
    set wildignore+=*/tmp/*,*.so,*.swp,*.zip     " Linux/MacOSX
    let g:ctrlp_user_command = 'find %s -type f'
endif


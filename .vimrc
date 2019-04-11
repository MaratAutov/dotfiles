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
set scroll=4
set scrolloff=4
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

" Lightline {{{
let s:base1   = '#C8CACB'
let s:base0   = '#AEB0B1'
let s:base00  = '#949697'
let s:base02  = '#626465'
let s:base023 = '#484A4B'
let s:base03  = '#2F3132'
let s:red     = '#cd3f45'
let s:orange  = '#db7b55'
let s:yellow  = '#e6cd69'
let s:green   = '#9fca56'
let s:cyan    = '#55dbbe'
let s:blue    = '#55b5db'
let s:magenta = '#a074c4'

let s:p                 = {'normal': {}, 'inactive': {}, 'insert': {}, 'replace': {}, 'visual': {}, 'tabline': {}}
let s:p.normal.left     = [ [ s:blue, s:base03 ],    [ s:base03, s:blue ] ]
let s:p.normal.middle   = [ [ s:base1, s:base03 ] ]
let s:p.normal.right    = [ [ s:base03, s:blue ],   [ s:base00, s:base03 ] ]
let s:p.normal.error    = [ [ s:red, s:base023 ] ]
let s:p.normal.warning  = [ [ s:yellow, s:base02 ] ]

let s:p.inactive.left   = [ [ s:base1,   s:base03 ],  [ s:base03, s:base03 ] ]
let s:p.inactive.middle = [ [ s:base03, s:base03 ] ]
let s:p.inactive.right  = [ [ s:base03,  s:base03 ],  [ s:base03, s:base03 ] ]

let s:p.insert.left     = [ [ s:green, s:base03 ],   [ s:base03,  s:green ] ]
let s:p.insert.right    = [ [ s:base03, s:green ],    [ s:base00, s:base03 ] ]
let s:p.replace.left    = [ [ s:orange, s:base03 ],  [ s:base03,  s:orange ] ]
let s:p.replace.right   = [ [ s:base03, s:orange ],    [ s:base00, s:base03 ] ]
let s:p.visual.left     = [ [ s:magenta, s:base03 ], [ s:base03,  s:magenta ] ]
let s:p.visual.right    = [ [ s:base03, s:magenta ],    [ s:base00, s:base03 ] ]

let g:lightline#colorscheme#base16_seti#palette = lightline#colorscheme#fill(s:p)
let g:lightline = {
      \ 'colorscheme':      'base16_seti',
      \ 'separator':        { 'left': "\ue0b0", 'right': "\ue0b2" },
      \ 'subseparator':     { 'left': "\ue0b1", 'right': "\ue0b3" },
      \ 'active': {
      \   'left': [ [ 'mode', 'paste' ],
      \             [ 'modified', 'fugitive', 'label' ] ],
      \   'right': [ [ 'lineinfo'],
      \              [ 'fileformat', 'fileencoding', 'filetype' ] ]
      \ },
      \ 'component': {
      \   'mode':     '%{lightline#mode()[0]}',
      \   'readonly': '%{&filetype=="help"?"":&readonly?"!":""}',
      \   'modified': '%{&filetype=="help"?"":&modified?"+":&modifiable?"":"-"}',
      \   'fugitive': '%{exists("*fugitive#head")?fugitive#head():""}',
      \   'label':    '%{substitute(expand("%"), "NetrwTreeListing \\d\\+", "netrw", "")}'
      \ },
      \ 'component_visible_condition': {
      \   'paste':    '(&paste!="nopaste")',
      \   'readonly': '(&filetype!="help"&& &readonly)',
      \   'modified': '(&filetype!="help"&&(&modified||!&modifiable))',
      \   'fugitive': '(exists("*fugitive#head") && ""!=fugitive#head())'
      \ }
      \ }
" }}}
" remap
map <C-z> <Nop>
nmap \q :nohlsearch<CR>

nmap j gj
nmap k gk

" move between open buffers.
nmap <C-n> :bnext<CR>
nmap <C-p> :bprev<CR>

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

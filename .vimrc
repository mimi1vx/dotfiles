colorscheme monokai

set incsearch
set nocompatible

set backspace=indent,eol,start
set autoindent

set history=1000
set ruler
set number 		" show line numbers
set showcmd 		" show command in bottom bar
set cursorline          " highlight current line

set showmode

map Q gq

"tabs expand
set tabstop=4       " number of visual spaces per TAB
set softtabstop=4   " number of spaces in tab when editing
set expandtab       " tabs are spaces

"filetype detection
filetype indent on
filetype plugin on

syntax on
set incsearch           " search as characters are entered
set hlsearch            " highlight matches

" turn off search highlight
nnoremap <leader><space> :nohlsearch<CR>

"haskell plugin needs
let g:haddock_browser="/usr/bin/chrome"

set wildmenu

"regexp magic ...
set magic
"show brackets ..
set showmatch " highlight matching [{()}]

set backup
set backupdir=~/.vim-tmp,~/.tmp,~/tmp,/var/tmp,/tmp
set backupskip=/tmp/*,/private/tmp/*
set directory=~/.vim-tmp,~/.tmp,~/tmp,/var/tmp,/tmp
set writebackup

set mouse=a

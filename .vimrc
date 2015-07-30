set nocompatible
filetype off

set rtp+=~/.vim/bundle/Vundle.vim
call vundle#begin()
"from here add plugins
"
Plugin 'gmarik/Vundle.vim'
Plugin 'tomasr/molokai'
Plugin 'haskell.vim'
Plugin 'lukerandall/haskellmode-vim'
Plugin 'davidhalter/jedi-vim'
Plugin 'klen/python-mode'
Plugin 'mkitt/tabline.vim'
Plugin 'ctrlpvim/ctrlp.vim'
Plugin 'scrooloose/nerdtree'
Plugin 'bling/vim-airline'
Plugin 'eagletmt/neco-ghc'
Plugin 'Rip-Rip/clang_complete'
Plugin 'tpope/vim-fugitive'
Plugin 'scrooloose/syntastic'
Plugin 'ervandew/supertab'
Plugin 'vim-pandoc/vim-pandoc'
Plugin 'vim-pandoc/vim-pandoc-syntax'

"las line for plugins
call vundle#end()

colorscheme molokai
set encoding=utf-8

set backspace=indent,eol,start
set autoindent

set history=1000
set ruler
set number relativenumber	" show line numbers
set showcmd 		" show command in bottom bar
set cursorline      " highlight current line

set showmode

"map Q gq

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
":let g:airline_powerline_fonts = 1
set laststatus=2
:let g:airline_theme='molokai'

"markdown ... i not use modula
au BufRead,BufNewFile *.md set filetype=markdown

"collums

set colorcolumn=78,85
highlight ColorColumn ctermbg=Magenta

" ctrlp
let g:ctrlp_map = '<c-p>'
let g:ctrlp_cmd = 'CtrlP'

" NerdTree
map <F2> :NERDTreeToggle<CR>

" Python-mode
" Keys:
" K             Show python docs
" <Ctrl-Space>  Rope autocomplete
" <Ctrl-c>g     Rope goto definition
" <Ctrl-c>d     Rope show documentation
" <Ctrl-c>f     Rope find occurrences
" <Leader>b     Set, unset breakpoint (g:pymode_breakpoint enabled)
" [[            Jump on previous class or function (normal, visual, operator modes)
" ]]            Jump on next class or function (normal, visual, operator modes)
" [M            Jump on previous class or method (normal, visual, operator modes)
" ]M            Jump on next class or method (normal, visual, operator modes)

"disable rope -- JEDI
let g:pymode_rope = 0

" Documentation
let g:pymode_doc = 1
let g:pymode_doc_key = 'K'

"Linting
let g:pymode_lint = 1
let g:pymode_lint_checker = "pyflakes,pep8"
" Auto check on save
let g:pymode_lint_write = 1

" Support virtualenv
let g:pymode_virtualenv = 1

" Enable breakpoints plugin
let g:pymode_breakpoint = 1
let g:pymode_breakpoint_bind = '<leader>b'

" syntax highlighting
let g:pymode_syntax = 1
let g:pymode_syntax_all = 1
let g:pymode_syntax_indent_errors = g:pymode_syntax_all
let g:pymode_syntax_space_errors = g:pymode_syntax_all

" Don't autofold code
let g:pymode_folding = 0

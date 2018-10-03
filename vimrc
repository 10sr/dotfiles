" vimrc
" SETUP_LOAD: if filereadable(expand('DOTFILES_DIR/vimrc'))
" SETUP_LOAD:     source DOTFILES_DIR/vimrc
" SETUP_LOAD: endif
if !isdirectory(expand('~/.vim'))
call mkdir(expand('~/.vim'))
endif

"""""""""""""""""""""""""""""""""""
set compatible " vi compatible
" Directory to put backup file
if !isdirectory(expand('~/.vim/backup'))
    call mkdir(expand('~/.vim/backup'))
endif
" Enable backup
set backup
set backupdir=$HOME/.vim/backup
" Directory for swap file
set swapfile
set directory=$HOME/.vim/backup
" viminfo
set viminfo+=n~/.vim/viminfo
" Wrap when moving cursor
"set whichwrap=b,s,h,l,<,>,[,]
set wildmode=longest,list,full
" Able to switch to another buffer even when editting a file
set hidden
" BS can erase these things
"set backspace=indent,eol,start
" Automatically change current dir according to current file. cant use with mac
"set autochdir
" Do not use mouse
set mouse=h
" Use x clipboard, seems not to work?
set clipboard+=unnamed
" Default dir for Explorer
set browsedir=buffer

" Encoding
set encoding=utf-8
set fileencodings=utf-8,shift-jis,euc-jp,latin1

" Display
set showmode
" Show editting file on titlebar
set title
" Show spcial letters such as newline or whitespace
set list
" Change letters for displaying special letter
set listchars=tab:>-,extends:<,trail:-,eol:$
" Display current line and column
set ruler
" Do not show line number at left side
set nonumber
" Always show status line
"set laststatus=2
" Scroll offset
set scrolloff=2
set showcmd
" Dont beep
set visualbell
" Enable syntax highlight
syntax enable
" Highlight matching paren
set showmatch

" Searching
"set incsearch
" Enable wrap search
set wrapscan
set ignorecase
" Case-sensitive only when capital letters appear
set smartcase
set incsearch

" Tab and Indentation
" Tab width for displaying
set tabstop=4
set softtabstop=4
" Width of indent
set shiftwidth=4
" Expand tab to space
set expandtab
set autoindent
set smartindent
set cindent
filetype plugin indent on

let g:netrw_liststyle = 1
let g:netrw_list_hide = '\(^\|\s\s\)\zs\.\S\+'

" For gvim
if has('gui_running')
    " Hide toolbar and scroll bar
    set guioptions-=T
    set guioptions-=r
    set lines=45
    set columns=110
    set guifont=DejaVu\ Sans\ Mono\ 9
endif

if has('win32')
    " Prefs for Windows
endif

"""""""""""""""""""""""""""""""""""""""
" mappings
" imap <c-j> <esc> でC-Jをescにできる。?
" nmap
" vmap
" map でそれぞれのモードでマップを指定
" キーマッピングには大きく分けて map と noremap の 2 つの種類があります。
" * map はキーシーケンスを展開したあと、さらに別のマップを適用しようとします。
" * noremap は一度だけ展開します。
" →マップを再帰的に展開するときはmap、決め打ちはnoremap（キーの入れ替えなど）
" Save file on exiting insert mode
inoremap <ESC> <ESC>:<C-u>w<CR>
inoremap <C-c> <ESC>:<C-u>w<CR>
noremap <C-c> <ESC>:<C-u>w<CR>

" Highlight current line
" set cursorline
" show cursor line only in current window
" not work in term-mode of emacs
" augroup cch
"     autocmd! cch
"     autocmd WinLeave * set nocursorline
"     autocmd WinEnter,BufRead * set cursorline
" augroup END

hi clear CursorLine
highlight CursorLine term=underline cterm=underline gui=underline

" Change status line color when in insert mode
augroup InsertHook
    autocmd!
    autocmd InsertEnter * highlight StatusLine guifg=#ccdc90 guibg=#2E4340
    autocmd InsertLeave * highlight StatusLine guifg=#2E4340 guibg=#ccdc90
augroup END

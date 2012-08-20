" load external file
" if filereadable(expand('~/.dotfiles/vimrc'))
" source ~/.dotfiles/vimrc
" endif
if !isdirectory(expand('~/.vim'))
   call mkdir(expand('~/.vim'))
endif

"""""""""""""""""""""""""""""""""""
set compatible " vi compatible
" directory to put backup file
if !isdirectory(expand('~/.vim/backup'))
   call mkdir(expand('~/.vim/backup'))
endif
set backup " enable backup
set backupdir=$HOME/.vim/backup
set swapfile " directory for swap file
set directory=$HOME/.vim/backup
set viminfo+=n~/.vim/viminfo " viminfo
"set whichwrap=b,s,h,l,<,>,[,] " wrap when moving cursor
set wildmode=longest,list,full
set hidden " can switch to another buffer even when editting a file
"set backspace=indent,eol,start " backspace can erase these things
"set autochdir " automatically change current dir according to current file. cant use with mac
set mouse=h " do not use mouse
set clipboard+=unnamed " use x clipboard, seems not to work
set browsedir=buffer " default dir for Explorer

" encoding
set encoding=utf-8
set fileencodings=utf-8,shift-jis,euc-jp,latin1

" display
set showmode
set title " display editting file on titlebar
set list " display spcial letters such as newline or whitespace
set listchars=tab:>-,extends:<,trail:-,eol:$ " set letters for displaying special letter
set ruler " display current line and column
set nonumber "  do not show line number at left side
"set laststatus=2 " always show status line
set scrolloff=2 " scroll offset
set showcmd
set visualbell " dont beep
syntax enable " enable syntax highlight

" searching
"set incsearch
set wrapscan " wrap search
set showmatch " highlight matching paren
set ignorecase
set smartcase " case sensitive only when capital letter is used
set incsearch

" tab and indent
set tabstop=4 " tab width for displaying
set softtabstop=4
set shiftwidth=4 " width of indent
set expandtab " expand tab to space
set autoindent
set smartindent
set cindent
filetype plugin indent on

let g:netrw_liststyle = 1
let g:netrw_list_hide = '\(^\|\s\s\)\zs\.\S\+'

" i dont use gvimrc
if has('gui_running')
  " hide toolbar and scroll bar
  set guioptions-=T
  set guioptions-=r
  set lines=45
  set columns=110
  set guifont=DejaVu\ Sans\ Mono\ 9
endif

if has('win32')
" prefs for Windows
endif

"""""""""""""""""""""""""""""""""""""""
" マップ
" imap <c-j> <esc> でC-Jをescにできる。?
" nmap
" vmap
" map でそれぞれのモードでマップを指定
" キーマッピングには大きく分けて map と noremap の 2 つの種類があります。
" * map はキーシーケンスを展開したあと、さらに別のマップを適用しようとします。
" * noremap は一度だけ展開します。
" →マップを再帰的に展開するときはmap、決め打ちはnoremap（キーの入れ替えなど）
" 割と保存
inoremap <ESC> <ESC>:<C-u>w<CR>
inoremap <C-c> <ESC>:<C-u>w<CR>
noremap <C-c> <ESC>:<C-u>w<CR>

" highlight current line
" set cursorline
" show cursor line only in current window
augroup cch
  autocmd! cch
  autocmd WinLeave * set nocursorline
  autocmd WinEnter,BufRead * set cursorline
augroup END

hi clear CursorLine
highlight CursorLine term=underline cterm=underline gui=underline

" change status line color when in insert mode
augroup InsertHook
autocmd!
autocmd InsertEnter * highlight StatusLine guifg=#ccdc90 guibg=#2E4340
autocmd InsertLeave * highlight StatusLine guifg=#2E4340 guibg=#ccdc90
augroup END

" save window position and size
if has('gui_running')
    let g:save_window_file = expand('~/.vimwinpos')
    augroup SaveWindow
      autocmd!
      autocmd VimLeavePre * call s:save_window()
      function! s:save_window()
        let options = [
          \ 'set columns=' . &columns,
          \ 'set lines=' . &lines,
          \ 'winpos ' . getwinposx() . ' ' . getwinposy(),
          \ ]
        call writefile(options, g:save_window_file)
      endfunction
    augroup END

    if filereadable(g:save_window_file)
      execute 'source' g:save_window_file
    endif
endif

" 外部ファイル読み込み
" if filereadable(expand('~/filepath'))
" source ~/filepath
" endif
if !isdirectory(expand('~/.vim'))
   call mkdir(expand('~/.vim'))
endif

"""""""""""""""""""""""""""""""""""
set compatible " vi compatible
" バックアップファイルを作るディレクトリ
if !isdirectory(expand('~/.vim/backup'))
   call mkdir(expand('~/.vim/backup'))
endif
set backup " バックアップ
set backupdir=$HOME/.vim/backup
set swapfile " スワップファイル用のディレクトリ
set directory=$HOME/.vim/backup
set viminfo+=n~/.vim/viminfo " viminfo
set list " タブ文字、行末など不可視文字を表示する
set listchars=tab:>-,extends:<,trail:-,eol:$ " listで表示される文字のフォーマットを指定する
set showmatch " 閉じ括弧が入力されたとき、対応する括弧を表示する
set ignorecase
set smartcase " 検索時に大文字を含んでいたら大/小を区別
set whichwrap=b,s,h,l,<,>,[,] " カーソルを行頭、行末で止まらないようにする
set showmode " モード表示
set notitle "  do not display editting file on titlebar
set incsearch
set wrapscan " 折り返し検索
set showmatch " 括弧を閉じた時、対になる括弧を一瞬ハイライトする
set wildmode=longest,list,full
set ruler " 行番号、カーソル位置を表示
set nonumber "  do not show line number at left side
set laststatus=2 " ステータスラインを常に表示
set showcmd
set scrolloff=2 " 常に前後2行を表示
syntax enable " シンタックス
set hidden " 編集中でも他ファイルへ移動可
set backspace=indent,eol,start " バックスペースでなんでも消せるように
" set autochdir " 自動的にカレントディレクトリを現在のファイルのディレクトリにする macだと出来ないってゆわれた
set encoding=utf-8 " 言語設定
set fileencodings=utf-8,shift-jis,euc-jp,latin1
set mouse=h " マウス使わない
set clipboard+=unnamed " クリップボードをWindowsと連携
set visualbell " ビープしない
set browsedir=buffer " バッファで開いているファイルのディレクトリ

set tabstop=4 " タブの画面上での幅
set softtabstop=4
set shiftwidth=4 " width of indent
set expandtab " タブをスペースに展開する
set autoindent " オートインデント
set smartindent
filetype plugin indent on

let g:netrw_liststyle = 1
let g:netrw_list_hide = '\(^\|\s\s\)\zs\.\S\+'

" gvimrc分けるのめんどい
if has('gui_running')
  " ツールバー、スクロールバー消す
  set guioptions-=T
  set guioptions-=r
  set lines=45
  set columns=110
  set guifont=DejaVu\ Sans\ Mono\ 9
endif

if has('win32')
" Windows 用の設定
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

" 方向キーでバッファ操作
" nnoremap <LEFT> <C-c>:bp!<CR>
" nnoremap <RIGHT> <C-c>:bn!<CR>
" nnoremap <UP> <C-c>:ls!<CR>
" nnoremap <DOWN> <C-c>:bd
" nnoremap <C-m>cd <C-c>:cd%:h<CR>
nnoremap <C-o> <C-c>:Explore<CR>

" http://d.hatena.ne.jp/yuroyoro/20101104/1288879591
" 見た目で行移動
nnoremap j gj
nnoremap k gk

" highlight current line
" set cursorline
" カレントウィンドウにのみ罫線を引く
augroup cch
  autocmd! cch
  autocmd WinLeave * set nocursorline
  autocmd WinEnter,BufRead * set cursorline
augroup END

hi clear CursorLine
highlight CursorLine term=underline cterm=underline gui=underline

" 入力モード時、ステータスラインのカラーを変更
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

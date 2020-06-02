"-----PLUGINS---------
set nocompatible              " be iMproved, required
filetype off                  " required

" set the runtime path to include Vundle and initialize
set rtp+=~/.vim/bundle/Vundle.vim
call vundle#begin()
" alternatively, pass a path where Vundle should install plugins
"call vundle#begin('~/some/path/here')

" let Vundle manage Vundle, required
Plugin 'VundleVim/Vundle.vim'
Plugin 'ycm-core/YouCompleteMe'

call vundle#end()            " required
filetype plugin indent on    " required
" To ignore plugin indent changes, instead use:
"filetype plugin on
"
" Brief help
" :PluginList       - lists configured plugins
" :PluginInstall    - installs plugins; append `!` to update or just :PluginUpdate
" :PluginSearch foo - searches for foo; append `!` to refresh local cache
" :PluginClean      - confirms removal of unused plugins; append `!` to auto-approve removal
"
" see :h vundle for more details or wiki for FAQ
" Put your non-Plugin stuff after this line 
" *******VIM-PLUG**********
call plug#begin('~/.vim/plugged')
Plug 'morhetz/gruvbox'
Plug 'xuhdev/vim-latex-live-preview', { 'for': 'tex' }
Plug 'terryma/vim-multiple-cursors'
Plug 'w0rp/ale' "linting
Plug 'kien/ctrlp.vim'
Plug 'vim-airline/vim-airline'
Plug 'vim-airline/vim-airline-themes'
Plug 'preservim/nerdtree'
Plug 'frazrepo/vim-rainbow' 
Plug 'airblade/vim-gitgutter'
Plug 'preservim/nerdcommenter'
Plug 'jiangmiao/auto-pairs'
Plug 'tpope/vim-fugitive'
Plug 'tpope/vim-surround'
Plug 'ryanoasis/vim-devicons'
Plug 'godlygeek/tabular'
Plug 'yuttie/comfortable-motion.vim'
Plug 'jeetsukumaran/vim-buffergator'
Plug 'yggdroot/indentline'
call plug#end()



"------THEME------

colorscheme gruvbox 
set background=dark

"------SETTINGS------

syntax on
set hidden "permite editar outro arquivo sem ter salvado o atual
set number
set relativenumber
set updatetime=1000
set laststatus=2        " faz a barra de status aparecer
set wildmenu	        " visual complete for command menu
set showmatch           " highlight matching [{()}]
set incsearch           " search as characters are entered
set hlsearch            " highlight matches
set tabstop=2       " number of visual spaces per TAB
set tabstop=2       " number of visual spaces per TAB
set expandtab       " tabs are spaces

"------PLUGINSETTINGS------
au FileType c,cpp,objc,objcpp call rainbow#load()
"*****AIRLINE******
let g:airline#extensions#tabline#enabled = 1
let g:airline#extensions#tabline#left_sep = ' '
let g:airline#extensions#tabline#left_alt_sep = '|'
let g:airline#extensions#tabline#formatter = 'default' 
let g:airline_powerline_fonts = 1
"*****CTRLP*******
let g:ctrlp_map = '<c-p>'
let g:ctrlp_cmd = 'CtrlP'
"******************
"------REMAPS------

let mapleader="\<space>"

inoremap jk <esc> 
nnoremap <leader>sv :source $MYVIMRC<cr>
nnoremap <leader>f :NERDTreeToggle<cr>
nnoremap <leader>s :w<cr>
nnoremap <leader>q :q<cr>
nnoremap <leader>sq :wq<cr>

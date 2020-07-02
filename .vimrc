"       _
"__   _(_)_ __ ___  _ __ ___
"\ \ / / | '_ ` _ \| '__/ __|
" \ V /| | | | | | | | | (__
"  \_/ |_|_| |_| |_|_|  \___|

"Arquivo de Configuração de Gabriel Nogueira
"-----PLUGINS---------
"*******VIM-PLUG**********
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
Plug 'godlygeek/tabular'
Plug 'yuttie/comfortable-motion.vim'
Plug 'jeetsukumaran/vim-buffergator'
Plug 'yggdroot/indentline'
Plug 'ap/vim-css-color'
Plug 'iamcco/markdown-preview.nvim', { 'do': { -> mkdp#util#install() }, 'for': ['markdown', 'vim-plug']}
Plug 'ryanoasis/vim-devicons'

call plug#end()



"------THEME------

colorscheme gruvbox 
set background=dark
"------SETTINGS------

syntax on
set hidden          "  permite editar outro arquivo sem ter salvado o atual
set number
set relativenumber
set autoindent
set updatetime=1000
set laststatus=2    "  faz a barra de status aparecer
set wildmenu        "  visual complete for command menu
set wildmode=full
set showmatch       "  highlight matching [{()}]
set incsearch       "  search as characters are entered
set hlsearch        "  highlight matches
set tabstop=4       "  number of visual spaces per TAB
set softtabstop=4   " number of spaces in tab when editing
set shiftwidth=4
set expandtab       "  tabs are spaces
set splitbelow      "  esse e o seguinte fazem com que a tela do split surja à direita ou abaixo"
set splitright
set confirm         "  menu ao sair sem salvar

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
nnoremap <leader>w :w<cr>
nnoremap <leader>e :q<cr>
nnoremap <leader>q :bd<cr>
nnoremap <leader>l <c-w>l
nnoremap <leader>h <c-w>h
nnoremap <leader>j <c-w>j
nnoremap <leader>k <c-w>k
cnoremap <C-p> <Up>
cnoremap <C-n> <Down>

hi Normal guibg=NONE ctermbg=NONE  
"faz com que o fundo fique transparente

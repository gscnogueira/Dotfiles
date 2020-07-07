"author:Gabriel S. C. Nogueira
"e-mail:gab.nog94@gmail.com
"github:https://github.com/nosgueira_
"
"__   _(_)_ __ ___  _ __ ___
"\ \ / / | '_ ` _ \| '__/ __|
" \ V /| | | | | | | | | (__
"  \_/ |_|_| |_| |_|_|  \___|


"-----PLUGINS---------
packadd minpac
call minpac#init()
call minpac#add('k-takata/minpac', {'type': 'opt'})
call minpac#add('tpope/vim-unimpaired')
call minpac#add( 'morhetz/gruvbox')
call minpac#add('w0rp/ale' )
call minpac#add( 'vim-airline/vim-airline')
call minpac#add( 'vim-airline/vim-airline-themes')
call minpac#add( 'preservim/nerdtree')
call minpac#add( 'preservim/nerdcommenter')
call minpac#add( 'jiangmiao/auto-pairs') 
call minpac#add( 'tpope/vim-surround')
call minpac#add( 'godlygeek/tabular')
call minpac#add('iamcco/markdown-preview.nvim', {'do': 'call mkdp#util#install()'})
call minpac#add( 'ap/vim-css-color')
call minpac#add( 'ryanoasis/vim-devicons')
"------THEME------
colorscheme gruvbox 
set background=dark
"------SETTINGS------
syntax on
set hidden          "  permite editar outro arquivo sem ter salvado o atual
set spell
set number
set ignorecase
set smartcase
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
"*****AIRLINE******
let g:airline#extensions#tabline#enabled = 1
let g:airline#extensions#tabline#left_sep = ' '
let g:airline#extensions#tabline#left_alt_sep = '|'
let g:airline#extensions#tabline#formatter = 'default' 
let g:airline_powerline_fonts = 1
"------REMAPS------

command! PackUpdate call minpac#update()
command! PackClean call minpac#clean()
let mapleader="\<space>"
autocmd FileType markdown nnoremap <leader>m :MarkdownPreview<cr>
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

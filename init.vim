"author:Gabriel S. C. Nogueira
"e-mail:gab.nog94@gmail.com
"github:https://github.com/nosgueira_
"
"                          _         
"   ____  ___  ____ _   __(_)___ ___ 
"  / __ \/ _ \/ __ \ | / / / __ `__ \
" / / / /  __/ /_/ / |/ / / / / / / /
"/_/ /_/\___/\____/|___/_/_/ /_/ /_/ 
                                    

"*****PLUGINS*************
call plug#begin('~/.vim/plugged')
Plug 'tpope/vim-unimpaired'
Plug 'tpope/vim-surround'
Plug 'xuyuanp/nerdtree-git-plugin'
Plug 'yggdroot/indentline'
Plug 'tpope/vim-fugitive'
Plug 'airblade/vim-gitgutter'
Plug 'majutsushi/tagbar'
Plug 'morhetz/gruvbox'
Plug 'w0rp/ale'
Plug 'valloric/youcompleteme'
Plug 'vim-airline/vim-airline'
Plug 'terryma/vim-multiple-cursors'
Plug 'vim-airline/vim-airline-themes'
Plug 'preservim/nerdtree'
Plug 'preservim/nerdcommenter'
Plug 'jiangmiao/auto-pairs'
Plug 'godlygeek/tabular'
Plug 'ap/vim-css-color'
Plug 'ryanoasis/vim-devicons'
Plug 'junegunn/fzf'
Plug 'tpope/vim-projectionist'
Plug 'tpope/vim-dispatch'
Plug 'radenling/vim-dispatch-neovim'
Plug 'mhinz/vim-grepper'
Plug 'vim-scripts/restore_view.vim'
Plug 'iamcco/markdown-preview.nvim', { 'do': 'cd app & yarn install'  }
call plug#end()
"*****PLUGINSETTINGS******
 
"*****AIRLINE******
let g:airline#extensions#tabline#enabled = 1
let g:airline#extensions#tabline#left_sep = ' '
let g:airline#extensions#tabline#left_alt_sep = '|'
let g:airline#extensions#tabline#formatter = 'default' 
let g:airline_powerline_fonts = 1

"********ALE*******
let g:ale_linters={
\    'javascript':['eslint'],
\ }
"*****SETTINGS************
syntax on
set background=dark
colorscheme gruvbox 
set hidden          "  permite editar outro arquivo sem ter salvado o atual
set spell
set number
set ignorecase
set smartcase
set relativenumber
set autoindent
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
"*****COMMANDS************
"
if has('nvim') && executable('nvr')
  let $VISUAL="nvr -cc split --remote-wait +'set bufhidden=wipe'"
endif

let mapleader="\<space>"

"faz com que o fundo fique transparente:
hi Normal guibg=NONE ctermbg=NONE  
"*****KEYMAPS*************
"--------GENERAL----------------------------
autocmd FileType rmd map <F5> : !echo<space>"require(rmarkdown);<space>render('<c-r>%')"<space>\|<space>R<space>--vanilla<enter>
"--------INSERT-MODE------------------------
inoremap jk <esc> 
"------NORMAL-MODE------------------------
nnoremap <leader>sv :source $MYVIMRC<cr>
nnoremap <leader>f :NERDTreeToggle<cr>
nnoremap <leader>w :w<cr>
nnoremap <leader>e :q<cr>
nnoremap <leader>q :bd<cr>
nnoremap <leader>l <c-w>l
nnoremap <leader>h <c-w>h
nnoremap <leader>j <c-w>j
nnoremap <leader>k <c-w>k
nnoremap <C-p> :<C-u>FZF<CR>
nmap <silent> [W <Plug>(ale_first)
nmap <silent> [w <Plug>(ale_previous)
nmap <silent> ]w <Plug>(ale_next)
nmap <silent> ]W <Plug>(ale_last)
nmap <F8> :TagbarToggle<CR>
autocmd FileType markdown nnoremap <leader>m :MarkdownPreview<cr>
"--------COMMAND-LINE-MODE------------------
cnoremap <C-p> <Up>
cnoremap <C-n> <Down>
"--------TERMINAL-MODE----------------------
if has('nvim')
    tnoremap <Esc> <C-\><C-n>
    tnoremap <C-v><Esc> <Esc>
endif

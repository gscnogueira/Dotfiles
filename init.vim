"author:Gabriel S. C. Nogueira
"e-mail:gab.nog94@gmail.com
"github:https://github.com/nosgueira_
"
"__   _(_)_ __ ___  _ __ ___
"\ \ / / | '_ ` _ \| '__/ __|
" \ V /| | | | | | | | | (__
"  \_/ |_|_| |_| |_|_|  \___|


"-----PLUGINS---------
call plug#begin('~/.vim/plugged')
Plug 'tpope/vim-unimpaired'
Plug 'morhetz/gruvbox'
Plug 'w0rp/ale'
Plug 'vim-airline/vim-airline'
Plug 'vim-airline/vim-airline-themes'
Plug 'preservim/nerdtree'
Plug 'preservim/nerdcommenter'
Plug 'jiangmiao/auto-pairs'
Plug 'tpope/vim-surround'
Plug 'godlygeek/tabular'
Plug 'ap/vim-css-color'
Plug 'ryanoasis/vim-devicons'
Plug 'junegunn/fzf'
Plug 'tpope/vim-projectionist'
Plug 'tpope/vim-dispatch'
Plug 'radenling/vim-dispatch-neovim'
Plug 'mhinz/vim-grepper'
Plug 'iamcco/markdown-preview.nvim', { 'do': 'cd app & yarn install'  }
call plug#end()


if has('nvim') && executable('nvr')
  let $VISUAL="nvr -cc split --remote-wait +'set bufhidden=wipe'"
endif

"------THEME------
colorscheme gruvbox 
"------SETTINGS------
syntax on
set background=dark
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

"------PLUGINSETTINGS------
 
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
"------COMMANDS-------
command! PackUpdate call minpac#update()
command! PackClean call minpac#clean()

let mapleader="\<space>"

"faz com que o fundo fique transparente:
hi Normal guibg=NONE ctermbg=NONE  

"------REMAPS------
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
nnoremap <C-p> :<C-u>FZF<CR>
cnoremap <C-p> <Up>
cnoremap <C-n> <Down>
autocmd FileType markdown nnoremap <leader>m :MarkdownPreview<cr>
nmap <silent> [W <Plug>(ale_first)
nmap <silent> [w <Plug>(ale_previous)
nmap <silent> ]w <Plug>(ale_next)
nmap <silent> ]W <Plug>(ale_last)
if has('nvim')
    tnoremap <Esc> <C-\><C-n>
    tnoremap <C-v><Esc> <Esc>
endif

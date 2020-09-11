"author:Gabriel S. C. Nogueira
"e-mail:gab.nog94@gmail.com
"github:https://github.com/nosgueira_
"
"                          _         
"   ____  ___  ____ _   __(_)___ ___ 
"  / __ \/ _ \/ __ \ | / / / __ `__ \
" / / / /  __/ /_/ / |/ / / / / / / /
"/_/ /_/\___/\____/|___/_/_/ /_/ /_/ 
                                    

"--------------------------------------------------
"-------------------PLUGINS------------------------
"--------------------------------------------------

call plug#begin('~/.vim/plugged')
Plug 'tpope/vim-unimpaired'
Plug 'tpope/vim-surround'
Plug 'tpope/vim-commentary'
Plug 'tpope/vim-repeat'
Plug 'lervag/vimtex' , { 'for' : 'tex'  }
Plug 'tpope/vim-fugitive' , { 'on' : 'Git' }
Plug 'itchyny/lightline.vim'
Plug 'airblade/vim-gitgutter'
Plug 'dracula/vim',
Plug 'w0rp/ale'
Plug 'terryma/vim-multiple-cursors'
Plug 'godlygeek/tabular' , { 'on' : 'Tabularize' }
Plug 'ap/vim-css-color'
Plug 'jiangmiao/auto-pairs'
Plug 'junegunn/fzf'
Plug 'iamcco/markdown-preview.nvim', { 'do': 'cd app & yarn install'  }
Plug 'neoclide/coc.nvim', {'branch': 'release'}
Plug 'sheerun/vim-polyglot'
Plug 'vimwiki/vimwiki'

call plug#end()

"--------------------------------------------------
"-----------------PLUGINSETTINGS-------------------
"--------------------------------------------------
 
"-------------------LIGHT-LINE---------------------
let g:lightline = {
    \ 'colorscheme': 'dracula',
    \ 'active': {
    \   'left': [ [ 'mode', 'paste' ],
    \             [ 'gitbranch', 'readonly', 'filename', 'modified' ] ],
    \   'right': [ [ 'lineinfo' ],
    \              [ 'percent' ],
    \              [ 'fileformat', 'fileencoding', 'filetype', 'charvaluehex' ] ]
    \ },
    \'component': {
    \   'charvaluehex': '0x%B'
    \ },
    \ 'component_function': {
    \   'gitbranch': 'FugitiveHead'
    \ },
    \ }
"-------------------VIM-POLYGLOT-------------------
let g:vim_markdown_conceal_code_blocks = 0
"-----------------------ALE------------------------
let g:ale_linters={
\    'javascript':['eslint'],
\ }
"--------------------VIM-WIKI----------------------
let g:vimwiki_list = [{'auto_diary_index': 1}]
"-----------------MARKDOWN-PREVIEW-----------------
let g:mkdp_browser = 'firefox'

"--------------------------------------------------
"-------------------SETTINGS-----------------------
"--------------------------------------------------
colorscheme dracula 
set autoindent
set background=dark
set confirm         "  menu ao sair sem salvar
set expandtab       "  tabs are spaces
set hidden          "  permite editar outro arquivo sem ter salvado o atual
set ignorecase
set laststatus=2    "  faz a barra de status aparecer
set mouse=a
set number
set relativenumber
set undodir=$HOME/.undodir
set undofile
set shiftwidth=4
set smartcase
set softtabstop=4   " number of spaces in tab when editing
set splitbelow      "  esse e o seguinte fazem com que a tela do split surja à direita ou abaixo"
set splitright
set tabstop=4       "  number of visual spaces per TAB
set wildmenu        "  visual complete for command menu
set wildmode=full
syntax on

"--------------------------------------------------
"-------------------SCRIPTS------------------------
"--------------------------------------------------
"
if has('nvim') && executable('nvr')
  let $VISUAL="nvr -cc split --remote-wait +'set bufhidden=wipe'"
endif


"transparent background:
hi Normal guibg=NONE ctermbg=NONE

"--------------------------------------------------
"--------------------KEYMAPS-----------------------
"--------------------------------------------------

let mapleader="\<space>"

"--------------------GENERAL-----------------------
autocmd FileType tex map <F5> :!pdflatex<space>%<cr>
"------------------INSERT-MODE---------------------
inoremap jk <esc>
"------------------NORMAL-MODE---------------------
autocmd FileType markdown nnoremap <leader>p :MarkdownPreview<cr>
nmap <F8> :TagbarToggle<CR>
nmap <silent> [W <Plug>(ale_first)
nmap <silent> [w <Plug>(ale_previous)
nmap <silent> ]W <Plug>(ale_last)
nmap <silent> ]w <Plug>(ale_next)
nnoremap <C-p> :<C-u>FZF<CR>
nnoremap <leader>e :q<cr>
nnoremap <leader>f :NERDTreeToggle<cr>
nnoremap <leader>h <c-w>h
nnoremap <leader>wv :vsp ~/vimwiki/index.wiki<cr>
nnoremap <leader>m :set mouse=a<cr>
nnoremap <leader>j <c-w>j
nnoremap <leader>k <c-w>k
nnoremap <leader>l <c-w>l
nnoremap <leader>q :bd<cr>
nnoremap <leader>sv :source %<cr>
nnoremap <leader>w :w<cr>
"---------------COMMAND-LINE-MODE------------------
cnoremap <C-p> <Up>
cnoremap <C-n> <Down>
"-----------------TERMINAL-MODE--------------------
if has('nvim')
    tnoremap <Esc> <C-\><C-n>
    tnoremap <C-v><Esc> <Esc>
endif

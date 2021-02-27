"set relativenumber author:Gabriel S. C. Nogueira
" e-mail:gab.nog94@gmail.com
" github:https://github.com/nosgueira
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
Plug 'airblade/vim-gitgutter'
Plug 'ap/vim-css-color'
Plug 'arcticicestudio/nord-vim'
Plug 'dracula/vim',
Plug 'godlygeek/tabular' , { 'on' : 'Tabularize' }
Plug 'iamcco/markdown-preview.nvim', { 'do': 'cd app & yarn install'  }
Plug 'itchyny/lightline.vim'
Plug 'jiangmiao/auto-pairs'
Plug 'junegunn/fzf'
Plug 'kien/rainbow_parentheses.vim'
Plug 'lervag/vimtex' , { 'for' : 'tex'  }
Plug 'neoclide/coc.nvim', {'branch': 'release'}
Plug 'pangloss/vim-javascript'
Plug 'ryanoasis/vim-devicons'
Plug 'sheerun/vim-polyglot'
Plug 'terryma/vim-multiple-cursors'
Plug 'tpope/vim-commentary'
Plug 'tpope/vim-fugitive' , { 'on' : 'Git' }
Plug 'tpope/vim-repeat'
Plug 'tpope/vim-surround'
Plug 'tpope/vim-unimpaired'
Plug 'w0rp/ale'

call plug#end()

"--------------------------------------------------
"-----------------PLUGINSETTINGS-------------------
"--------------------------------------------------
 
"-------------------LIGHT-LINE---------------------
let g:lightline = {
    \ 'colorscheme': 'nord',
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
"-----------------RAINBOW-PARENTHESES--------------
"#B48EAD"
let g:rbpt_colorpairs = [
    \ ['brown',       '#88C0D0'],
    \ ['Darkblue',    '#BF616A'],
    \ ['darkgray',    '#b48EAD'],
    \ ['darkgreen',   '#A3BE8C'],
    \ ['darkcyan',    '#EBCB8B'],
    \ ['darkred',     '#D08770'],
    \ ['darkmagenta', '#81A1C1'],
    \ ['brown',       '#5E81AC'],
    \ ['gray',        '#81A1C1'],
    \ ['black',       '#88C0D0'],
    \ ['darkmagenta', '#BF616A'],
    \ ['Darkblue',    '#b48EAD'],
    \ ['darkgreen',   '#A3BE8C'],
    \ ['darkcyan',    '#EBCB8B'],
    \ ['darkred',     '#D08770'],
    \ ['red',         '#81A1C1'],
    \ ]
au VimEnter * RainbowParenthesesToggle
au Syntax * RainbowParenthesesLoadRound
au Syntax * RainbowParenthesesLoadSquare
au Syntax * RainbowParenthesesLoadBraces
"--------------------------------------------------
"-------------------SETTINGS-----------------------
"--------------------------------------------------
colorscheme nord 
set autoindent
set confirm         "  menu ao sair sem salvar
set expandtab       "  tabs are spaces
set hidden          "  permite editar outro arquivo sem ter salvado o atual
set ignorecase
set laststatus=2    "  faz a barra de status aparecer
set mouse=a
set number
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
set autoread
autocmd FocusGained * silent! checktime
syntax on
set clipboard+=unnamedplus " makes nvim use systems clipboard
" italic comments:
highlight Comment cterm=italic gui=italic 
if (has("nvim"))
  let $NVIM_TUI_ENABLE_TRUE_COLOR=1
endif
if (has("termguicolors"))
  set termguicolors
endif

"--------------------------------------------------
"-------------------SCRIPTS------------------------
"--------------------------------------------------
"
if has('nvim') && executable('nvr')
  let $VISUAL="nvr -cc split --remote-wait +'set bufhidden=wipe'"
endif


"transparent background:
hi Normal guibg=NONE ctermbg=NONE
if (has("termguicolors"))
  set termguicolors
endif
"--------------------------------------------------
"--------------------KEYMAPS-----------------------
"--------------------------------------------------

let mapleader="\<space>"

"--------------------GENERAL-----------------------
autocmd FileType tex map <F5> :w \| !pdflatex<space>%<cr>
autocmd FileType javascript map <F5> :w  \| !node<space>%<cr>
autocmd FileType python map <F5> :w  \| !python3<space>%<cr>
"------------------INSERT-MODE---------------------
" inoremap jk <esc>
inoremap <C-l> <C-o>zz
"------------------NORMAL-MODE---------------------
autocmd FileType markdown nnoremap <leader>p :MarkdownPreview<cr>
nmap <Tab> gt
nmap <S-Tab> gT
nmap <silent> [W <Plug>(ale_first)
nmap <silent> [w <Plug>(ale_previous)
nmap <silent> ]W <Plug>(ale_last)
nmap <silent> ]w <Plug>(ale_next)
nmap <leader>f :CocCommand explorer<CR>
nmap <leader>1 1gt
nmap <leader>2 2gt
nmap <leader>3 3gt
nmap <leader>4 4gt
nmap <leader>5 5gt
nmap <leader>6 6gt
nmap <leader>7 7gt
nmap <leader>8 8gt
nmap <leader>9 9gt
nnoremap <C-p> :<C-u>FZF<CR>
nnoremap <leader>h <c-w>h
nnoremap <leader>wv :vsp ~/vimwiki/index.wiki<cr>
nnoremap <leader>j <c-w>j
nnoremap <leader>k <c-w>k
nnoremap <leader>l <c-w>l
nnoremap <leader>q :bd<cr>
nnoremap <leader>sv :source %<cr>
nnoremap <leader>w :wa<cr>
"---------------COMMAND-LINE-MODE------------------
cnoremap <C-p> <Up>
cnoremap <C-n> <Down>
"-----------------TERMINAL-MODE--------------------
if has('nvim')
    tnoremap <Esc> <C-\><C-n>
    tnoremap <C-v><Esc> <Esc>
endif


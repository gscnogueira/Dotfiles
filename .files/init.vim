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

let g:vim_markdown_conceal_code_blocks = 0

let g:ale_linters={
\    'javascript':['eslint'],
\ }

let g:mkdp_browser = 'firefox'

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

set confirm         "  menu ao sair sem salvar
set mouse=a
set laststatus=2    "  faz a barra de status aparecer
set number
set splitbelow      "  tela do split surge abaixo
set splitright      "  tela do split surge a direita
set wildmenu        "  visual complete for command menu
set wildmode=full
syntax on
highlight Comment cterm=italic gui=italic " italic comments

set autoindent
set expandtab       "  tabs are spaces
set shiftwidth=4
set softtabstop=4   " number of spaces in tab when editing
set tabstop=4       "  number of visual spaces per TAB
set ignorecase
set smartcase

set clipboard+=unnamedplus " makes nvim use systems clipboard

if (has("nvim"))
  let $NVIM_TUI_ENABLE_TRUE_COLOR=1
endif
if (has("termguicolors"))
  set termguicolors
endif

set autoread
autocmd FocusGained * silent! checktime

colorscheme nord 
"transparent background:
hi Normal guibg=NONE ctermbg=NONE
if (has("termguicolors"))
  set termguicolors
endif

if has('nvim') && executable('nvr')
  let $VISUAL="nvr -cc split --remote-wait +'set bufhidden=wipe'"
endif

let mapleader="\<space>"

autocmd FileType tex map <F5> :w \| !pdflatex<space>%<cr>
autocmd FileType javascript map <F5> :w  \| !node<space>%<cr>
autocmd FileType python map <F5> :w  \| !python3<space>%<cr>

inoremap <C-l> <C-o>zz

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
nnoremap <leader>sv :source %<cr>
nnoremap <leader>w :wa<cr>

cnoremap <C-p> <Up>
cnoremap <C-n> <Down>

if has('nvim')
    tnoremap <Esc> <C-\><C-n>
    tnoremap <C-v><Esc> <Esc>
endif

let @h = 'ggOauthor:€kb€kb€kb€kb€kb€kb€kbauthor: Gabr S. C;€kb. Nogueiraoe-mail: gab.nog94@gmail.comogithub: https://github.conm€kb€kbm/nosgueirao'

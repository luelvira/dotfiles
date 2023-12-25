source $VIMRUNTIME/defaults.vim

syntax on
filetype on
filetype plugin indent on

set modelines=1
set number
set nohlsearch
set incsearch
set mouse=c
set nowrap
set cc=80
set listchars+=space:·,tab:»\ ,trail:~,extends:>,precedes:<,nbsp:·
set ruler
set termguicolors

" indent
set expandtab
set copyindent
set preserveindent
set softtabstop=4
set tabstop=4
set shiftwidth=4
set autoindent 

" foldint
set foldenable
set foldmethod=syntax
set foldlevel=99
set foldnestmax=10

" status bar
set noshowmode
set noshowcmd
set laststatus=0

set nocompatible

" disable arrows keys in insert mode
inoremap <left> <nop>
inoremap <right> <nop>
inoremap <up> <nop>
inoremap <down> <nop>

vnoremap <C-r> "hy:%s/<C-r>h//gc<Left><left><left>
vnoremap <silent> <Leader>y :w !xclip -i -sel c<CR>
nnoremap <F3> :setlocal relativenumber!<CR>
nnoremap <F4> :setlocal list!<CR>
nnoremap <F5> :setlocal spell<CR>:set spelllang=es<CR>
nnoremap <F6> :setlocal spell<CR>:set spelllang=en<CR>
nnoremap <F7> :setlocal spell!<CR>

call plug#begin()

" snippets
Plug 'SirVer/ultisnips'
Plug 'honza/vim-snippets'
"Plug 'mattn/emmet-vim'

Plug 'tpope/vim-surround'
Plug 'tpope/vim-repeat'

"git
Plug 'airblade/vim-gitgutter'
Plug 'tpope/vim-fugitive'

" navigation
" Plug 'preservim/nerdtree'
" Plug 'christoomey/vim-tmux-navigator'

" lsp
" Plug 'neoclide/coc.nvim', {'branch': 'release'}
" Plug 'dense-analysis/ale'

" languages
" Plug 'lervag/vimtex'
Plug 'sheerun/vim-polyglot'
Plug 'davidhalter/jedi-vim'
Plug 'aklt/plantuml-syntax'


" Personal wiki
Plug 'lervag/wiki.vim'
" Plug 'lervag/lists.vim'
" Plug 'vimwiki/vimwiki'
" Plug 'blindFS/vim-taskwarrior'
" Plug 'tools-life/taskwiki'

"Plug 'vimwiki/vimwiki'

" Time managment

" themes
Plug 'dracula/vim', { 'as': 'dracula' }
Plug 'arcticicestudio/nord-vim'
Plug 'morhetz/gruvbox'

" fluzzy finder
Plug 'junegunn/fzf', { 'do': { -> fzf#install() } }
Plug 'junegunn/fzf.vim'

" project management
Plug 'mhinz/vim-startify'
Plug 'tpope/vim-obsession'

" ctags
" Plug 'ludovicchabant/vim-gutentags'


" vim-table-mode
Plug 'dhruvasagar/vim-table-mode'


" custom pluging
Plug 'luelvira/vim-tangle'
call plug#end()

let g:UltiSnipsExpandTrigger="<tab>"
let g:UltiSnipsJumpForwardTrigger="<tab>"
let g:UltiSnipsJumpBackwardTrigger="<S-tab>"
let g:UltiSnipsListSnippets="<C-l>"

silent! call repeat#set("\<Plug>MyWonderfulMap", v:count)

let g:vimtex_view_method = 'zathura'
let g:vimtex_quickfix_mode = 0
let g:tex_flavor = 'latex'
" hidden latex code when the pointer is over it
set conceallevel=1

let g:javascript_plugin_jsdoc = 1

autocmd BufNewFile *.sh,bash 0r ~/.vim/templates/bash_template.sh
autocmd BufNewFile *.html 0r ~/.vim/templates/index.html

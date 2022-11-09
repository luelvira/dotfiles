set nohlsearch
set incsearch
set mouse=c
set updatetime=1000

set number
set nowrap
"set cc=80

set foldenable
set foldmethod=syntax
set foldlevel=99
set noshowmode

set expandtab
set tabstop=4
set softtabstop=4
set shiftwidth=4
set copyindent
set autoindent

set laststatus=0
set noshowcmd

set lcs+=space:·
syntax on

inoremap <left> <nop>
inoremap <right> <nop>
inoremap <up> <nop>
inoremap <down> <nop>

nnoremap <F3> :set relativenumber!<CR>
inoremap <F3> :set relativenumber!<CR>
nnoremap <F4> :set list!<CR>
inoremap <F4> :set list!<CR>
vnoremap <F4> :set list!<CR>
nnoremap <F5> :set spell!<CR>:set spelllang=es<CR>
inoremap <F5> :set spell!<CR>:set spelllang=es<CR>
vnoremap <F5> :set spell!<CR>:set spelllang=es<CR>

" plugins
call plug#begin()
Plug 'preservim/nerdcommenter'
"Plug 'neoclide/coc.nvim'
Plug 'ap/vim-css-color'
Plug 'sheerun/vim-polyglot'

Plug 'SirVer/ultisnips'
Plug 'honza/vim-snippets'
Plug 'tpope/vim-fugitive'
Plug 'scrooloose/nerdtree'
Plug 'neovim/nvim-lspconfig'
Plug 'norcalli/nvim-colorizer.lua'
call plug#end()



let g:UltiSnipsExpandTrigger="<tab>"
let g:UltiSnipsJumpForwardTrigger="<tab>"
let g:UltiSnipsJumpBackwardTrigger="<S-tab>"
"
" nerdtree
" Exit Vim if NERDTree is the only window remaining in the only tab.
autocmd BufEnter * if tabpagenr('$') == 1 && winnr('$') == 1 && exists('b:NERDTree') && b:NERDTree.isTabTree() | quit | endif
" Close the tab if NERDTree is the only window remaining in it.
autocmd BufEnter * if winnr('$') == 1 && exists('b:NERDTree') && b:NERDTree.isTabTree() | quit | endif
" Open the existing NERDTree on each new tab.
autocmd BufWinEnter * if getcmdwintype() == '' | silent NERDTreeMirror | endif
nnoremap <C-n> :NERDTreeToggle<CR>



au BufNewFile,BufRead *.md,*.tex
	\ set textwidth=79
	\ fileformat=unix
	\ autoindent
	\ wrap
	

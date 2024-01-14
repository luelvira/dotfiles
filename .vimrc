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

silent! call repeat#set("\<Plug>MyWonderfulMap", v:count)

let g:javascript_plugin_jsdoc = 1

autocmd BufNewFile *.sh,bash 0r ~/.vim/templates/bash_template.sh
autocmd BufNewFile *.html 0r ~/.vim/templates/index.html

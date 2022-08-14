syntax on

filetype plugin indent on

set modelines=0

set mouse=c

set number

set noshowmode
set laststatus=0
set noshowcmd
set cmdheight=1

set nowrap

" highlight OverLength ctermbg=red ctermfg=white guibg=#592929
" highlight OverLength ctermbg=red ctermfg=white guibg=#ff0000
" match OverLength /\%81v.\+/


inoremap <left> <nop>
inoremap <right> <nop>
inoremap <up> <nop>
inoremap <down> <nop>


set lcs+=space:·
" search/replace visual content highligh

vnoremap <C-r> "hy:%s/<C-r>h//gc<Left><left><left>

" #colorscheme monokai
" set termguicolors
" set t_Co=256
" set t_ut=

set foldenable
set foldmethod=syntax
set foldlevel=99
"set foldlevelstart=20

"
"set tabstop=4
"set softtabstop=4
"set expandtab
"set shiftwidth=4

"keyboard shortcut

nnoremap <F2> :set invpaste paste?<CR>
imap <F2> <C-O>:set invpaste paste?<CR>
set pastetoggle=<F2>

nnoremap <F3> :set relativenumber!<CR>
nnoremap <F4> :set list!<CR>

set noexpandtab
set copyindent
set preserveindent
set softtabstop=0
set tabstop=4
set shiftwidth=4


call plug#begin()
Plug 'SirVer/ultisnips'
Plug 'honza/vim-snippets'
call plug#end()

let g:UltiSnipsExpandTrigger="<tab>"
let g:UltiSnipsJumpForwardTrigger="<tab>"
let g:UltiSnipsJumpBackwardTrigger="<S-tab>"


" style mode
set cc=80
set title

set incsearch

au BufNewFile,BufRead *.py 
	\ set tabstop=4 
	\ softtabstop=4 
	\ shiftwidth=4 
	\ textwidth=79 
	\ expandtab 
	\ autoindent 
	\ fileformat=unix 


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

"set cursorline
"set cursorlineopt=number

call plug#begin()
Plug 'SirVer/ultisnips'
Plug 'honza/vim-snippets'
Plug 'preservim/nerdtree'
call plug#end()

let g:UltiSnipsExpandTrigger="<tab>"
let g:UltiSnipsJumpForwardTrigger="<tab>"
let g:UltiSnipsJumpBackwardTrigger="<S-tab>"

" nerdtree
" Exit Vim if NERDTree is the only window remaining in the only tab.
autocmd BufEnter * if tabpagenr('$') == 1 && winnr('$') == 1 && exists('b:NERDTree') && b:NERDTree.isTabTree() | quit | endif
" Close the tab if NERDTree is the only window remaining in it.
autocmd BufEnter * if winnr('$') == 1 && exists('b:NERDTree') && b:NERDTree.isTabTree() | quit | endif
" Open the existing NERDTree on each new tab.
autocmd BufWinEnter * if getcmdwintype() == '' | silent NERDTreeMirror | endif
nnoremap <C-n> :NERDTreeToggle<CR>

" style mode
set cc=80
set title

set incsearch
set autoindent 

au BufNewFile,BufRead *.py 
	\ set tabstop=4 
	\ softtabstop=4 
	\ shiftwidth=4 
	\ expandtab 
	\ fileformat=unix 

au BufNewFile,BufRead *.md
	\ set textwidth=79
	\ fileformat=unix
	\ autoindent
au BufNewFile,BufRead *.html,*.css,*.scss,*.js
	\ set tabstop=2
	\ softtabstop=2
	\ shiftwidth=2
	\ expandtab


colorschem custom

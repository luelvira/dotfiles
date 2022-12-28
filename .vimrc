syntax on

filetype plugin indent on

set modelines=0


set number

set nohlsearch
set incsearch
set mouse=c
set number
set nowrap
set cc=80
set foldenable
set foldmethod=syntax
set foldlevel=99
set noshowmode

set expandtab
set copyindent
set preserveindent
set softtabstop=4
set tabstop=4
set shiftwidth=4
set autoindent 


set laststatus=0
set noshowcmd
"set cmdheight=1

"set termguicolors

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
nnoremap <F5> :set spell!<CR>:set spelllang=es<CR>


"set cursorline
"set cursorlineopt=number

call plug#begin()
Plug 'SirVer/ultisnips'
Plug 'honza/vim-snippets'
"Plug 'preservim/nerdtree'
"Plug 'catppuccin/vim', { 'as': 'catppuccin' }

"Plug 'neoclide/coc.nvim', {'branch': 'release'}
"Plug 'neoclide/coc-eslint'
call plug#end()

let g:UltiSnipsExpandTrigger="<tab>"
let g:UltiSnipsJumpForwardTrigger="<tab>"
let g:UltiSnipsJumpBackwardTrigger="<S-tab>"

" nerdtree
" Exit Vim if NERDTree is the only window remaining in the only tab.
" autocmd BufEnter * if tabpagenr('$') == 1 && winnr('$') == 1 && exists('b:NERDTree') && b:NERDTree.isTabTree() | quit | endif
" Close the tab if NERDTree is the only window remaining in it.
" autocmd BufEnter * if winnr('$') == 1 && exists('b:NERDTree') && b:NERDTree.isTabTree() | quit | endif
" Open the existing NERDTree on each new tab.
" autocmd BufWinEnter * if getcmdwintype() == '' | silent NERDTreeMirror | endif
" nnoremap <C-n> :NERDTreeToggle<CR>
"let g:lightline = {'colorscheme': 'catppuccin_mocha'}


" style mode
"au BufNewFile,BufRead *.py 
"	\ set tabstop=4 
"	\ softtabstop=4 
"	\ shiftwidth=4 
"	\ expandtab 
"	\ fileformat=unix 
"
au BufNewFile,BufRead *.md,*.tex
	\ set textwidth=79
	\ fileformat=unix
	\ autoindent
"au BufNewFile,BufRead *.html,*.css,*.scss,*.js
"	\ set tabstop=2
"	\ softtabstop=2
"	\ shiftwidth=2
"	\ expandtab
"colorscheme catppuccin_mocha

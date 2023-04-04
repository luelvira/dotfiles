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
vnoremap <Leader>y y:call system("xclip -i -selection c", getreg("\""))<CR>
nnoremap <F3> :set relativenumber!<CR>
nnoremap <F4> :setlocal list!<CR>
nnoremap <F5> :setlocal spell<CR>:set spelllang=es<CR>
nnoremap <F6> :setlocal spell<CR>:set spelllang=en<CR>

nnoremap <C-b> :bnext<CR>
nnoremap <C-S-b> :bprevious<CR>
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

" comments
Plug 'scrooloose/nerdcommenter'

" navigation
Plug 'preservim/nerdtree'

" lsp
Plug 'neoclide/coc.nvim', {'branch': 'release'}

" languages
Plug 'lervag/vimtex'
Plug 'sheerun/vim-polyglot'
"Plug 'craigemery/vim-autotag'
Plug 'davidhalter/jedi-vim'
Plug 'aklt/plantuml-syntax'

" themes
Plug 'dracula/vim', { 'as': 'dracula' }
Plug 'arcticicestudio/nord-vim'

" fluzzy finder
Plug 'junegunn/fzf', { 'do': { -> fzf#install() } }
Plug 'junegunn/fzf.vim'

" project management
Plug 'mhinz/vim-startify'
Plug 'tpope/vim-obsession'

" ctags
Plug 'ludovicchabant/vim-gutentags'


" custom pluging
Plug 'luelvira/vim-tangle'
call plug#end()
let g:UltiSnipsExpandTrigger="<tab>"
let g:UltiSnipsJumpForwardTrigger="<tab>"
let g:UltiSnipsJumpBackwardTrigger="<S-tab>"
let g:UltiSnipsListSnippets="<C-l>"
silent! call repeat#set("\<Plug>MyWonderfulMap", v:count)
autocmd BufEnter * if tabpagenr('$') == 1 && winnr('$') == 1 && exists('b:NERDTree') && b:NERDTree.isTabTree() | quit | endif
" Close the tab if NERDTree is the only window remaining in it.
autocmd BufEnter * if winnr('$') == 1 && exists('b:NERDTree') && b:NERDTree.isTabTree() | quit | endif
"" Open the existing NERDTree on each new tab.
autocmd BufWinEnter * if getcmdwintype() == '' | silent NERDTreeMirror | endif
nnoremap <C-n> :NERDTreeToggle<CR>
let g:vimtex_view_method = 'zathura'
let g:vimtex_quickfix_mode = 0
let g:tex_flavor = 'latex'
" hidden latex code when the pointer is over it
" set conceallevel=1
let g:javascript_plugin_jsdoc = 1
set encoding=utf-8
" Some servers have issues with backup files, see #649
set nobackup
set nowritebackup

set updatetime=300
set signcolumn=yes
inoremap <silent><expr> <CR> coc#pum#visible() ? coc#pum#confirm()
                              \: "\<C-g>u\<CR>\<c-r>=coc#on_enter()\<CR>"
function! CheckBackspace() abort
  let col = col('.') - 1
  return !col || getline('.')[col - 1]  =~# '\s'
endfunction

nmap <silent> gd <Plug>(coc-definition)
nmap <silent> gy <Plug>(coc-type-definition)
nmap <silent> gi <Plug>(coc-implementation)
nmap <silent> gr <Plug>(coc-references)

nnoremap <silent> K :call ShowDocumentation()<CR>

function! ShowDocumentation()
  if CocAction('hasProvider', 'hover')
    call CocActionAsync('doHover')
  else
    call feedkeys('K', 'in')
  endif
endfunction
" Colorscheme for coc
func! s:my_colors_setup() abort
  highlight CocFloating ctermbg=Black " For background color
endfunc

augroup colorscheme_coc_setup | au!
  au VimEnter * call s:my_colors_setup()
augroup END
" colors
" colorscheme dracula
colorscheme nord
" nord theme
set cursorline
let g:nord_cursor_line_number_background = 1
let g:nord_bold = 1
let g:nord_italic = 1
" let g:nord_italic_comments = 1
" let g:nord_underline = 1
let g:gutentangs_project_root = ['.git', '.hg', '.svn', '.root', '.project']
let g:gutentags_cache_dir = '~/.cache/gutentags'
let g:gutentags_ctags_tagfile = '.tags'
let g:gutentags_ctags_extra_args = ['--fields=+ailmnS', '--tag-relative=yes']
let g:gutentags_ctags_exclude = ['*.min.js', '*.min.css', '*.map', 'node_modules', 'test', 'cache', 'dist', 'build', 'vendor', '.*', '*/.*', '*.json', '*.yml', '*.html', '*.txt', '*.cpy', '*.css', 'bin']
" text mode
autocm BufNewFile,BufRead *.md,*.tex setlocal
	\ textwidth=80
	\ fileformat=unix
	\ autoindent
	\ cc=80
	\ spell
	\ spelllang=es
	\ wrap
au BufRead,BufNewFile *.{md,mdown,mkd,mkdn,markdown,mdwn,mdx} set filetype=markdown
autocmd BufNewFile *.md 0r ~/.vim/skeletons/headers.md
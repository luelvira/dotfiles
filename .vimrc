source $VIMRUNTIME/defaults.vim
syntax enable
filetype plugin indent on

" general rules
set modelines=0
set number
set nohlsearch
set incsearch
set mouse=c
set number
set nowrap
set cc=80
set lcs+=space:·

" indent rules
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
set ruler




" disable arrows keys in insert mode
inoremap <left> <nop>
inoremap <right> <nop>
inoremap <up> <nop>
inoremap <down> <nop>

" custom mapping
vnoremap <C-r> "hy:%s/<C-r>h//gc<Left><left><left>


vnoremap <Leader>y y:call system("xclip -i -selection c", getreg("\""))<CR>

nnoremap <F3> :set relativenumber!<CR>
nnoremap <F4> :set list!<CR>
nnoremap <F5> :set spell!<CR>:set spelllang=es<CR>
nnoremap <F6> :set spell!<CR>:set spelllang=en<CR>

call plug#begin()
" snippets
Plug 'SirVer/ultisnips'
Plug 'honza/vim-snippets'
"Plug 'mattn/emmet-vim'
" tpope
Plug 'tpope/vim-surround'
Plug 'tpope/vim-fugitive'
Plug 'tpope/vim-repeat'

" comments
Plug 'scrooloose/nerdcommenter'
" navigation
Plug 'preservim/nerdtree'

" lsp
"Plug 'dense-analysis/ale'
Plug 'neoclide/coc.nvim', {'branch': 'release'}
" languages
Plug 'lervag/vimtex'
Plug 'sheerun/vim-polyglot'
"Plug 'craigemery/vim-autotag'
Plug 'davidhalter/jedi-vim'

" git
" Plug 'airblade/vim-gitgutter'
"color
"Plug 'vim-airline/vim-airline'
"Plug 'vim-airline/vim-airline-themes'
Plug 'dracula/vim', { 'as': 'dracula' }

" local plugins
Plug '/home/lucas/Documents/git/vim-todo'
call plug#end()

let g:UltiSnipsExpandTrigger="<tab>"
let g:UltiSnipsJumpForwardTrigger="<tab>"
let g:UltiSnipsJumpBackwardTrigger="<S-tab>"
let g:UltiSnipsListSnippets="<C-l>"


silent! call repeat#set("\<Plug>MyWonderfulMap", v:count)


" nerdtree
autocmd BufEnter * if tabpagenr('$') == 1 && winnr('$') == 1 && exists('b:NERDTree') && b:NERDTree.isTabTree() | quit | endif
" Close the tab if NERDTree is the only window remaining in it.
autocmd BufEnter * if winnr('$') == 1 && exists('b:NERDTree') && b:NERDTree.isTabTree() | quit | endif
"" Open the existing NERDTree on each new tab.
autocmd BufWinEnter * if getcmdwintype() == '' | silent NERDTreeMirror | endif
nnoremap <C-n> :NERDTreeToggle<CR>


" vimtex
let g:vimtex_view_method = 'zathura'
let g:vimtex_quickfix_mode = 0
let g:tex_flavor = 'latex'
" hidden latex code when the pointer is over it
" set conceallevel=1
" let g:vimtex_conceal = 'abdmg'



let g:javascript_plugin_jsdoc = 1

" coc
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

" colors
colorscheme dracula


" text mode
autocm BufNewFile,BufRead *.md,*.tex set 
	\ textwidth=80
	\ fileformat=unix
	\ autoindent
	\ cc=80
	\ spell
	\ spelllang=es
	\ wrap


" TODO plugin (this should be in a separate file)

let g:todo_file = '~/Documents/Obsidian_vault/TODO.md'
function! TodoOpen()
  let l:todo = expand(g:todo_file)
  if filereadable(l:todo)
    execute 'edit' l:todo
  else
    execute 'edit' l:todo
    call append(0, ['# TODO'])
  endif
  normal! G
  startinsert!
endfunction

function! TodoAdd()
  let l:line = line('.')
  let l:todo = getline('.')
  " check if the line is empty
  if l:todo == ''
    call append(l:line, ['- [ ] '])
  else
    call append(l:line, ['- [ ] ' . l:todo])
  endif
  execute l:line .. 'delete'
  call cursor(l:line+1, 0)
  normal! $
  startinsert!
endfunction

function! TodoDone()
  let l:line = line('.')
  let l:todo = getline('.')
  " check if the line is empty
  if l:todo == ''
    return
  elseif l:todo =~ '\[x\]'
    call setline(l:line, substitute(l:todo, '\[x\]', '[ ]', ''))
  else
    call setline(l:line, substitute(l:todo, '\[ \]', '[x]', ''))
  endif

endfunction


command! TodoOpen call TodoOpen()
autocmd BufNewFile,BufRead TODO.md
  \ command! TodoAdd call TodoAdd() |
  \ command! TodoDone call TodoDone() |
  \ nnoremap <leader>ta :TodoAdd<CR> |
  \ nnoremap <leader>td :TodoDone<CR>

augroup vim-colors-xcode
    autocmd!
augroup END

autocmd vim-colors-xcode ColorScheme * hi Comment        cterm=italic gui=italic
autocmd vim-colors-xcode ColorScheme * hi SpecialComment cterm=italic gui=italic



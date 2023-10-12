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
" autocmd BufEnter * if tabpagenr('$') == 1 && winnr('$') == 1 && exists('b:NERDTree') && b:NERDTree.isTabTree() | quit | endif
" " Close the tab if NERDTree is the only window remaining in it.
" autocmd BufEnter * if winnr('$') == 1 && exists('b:NERDTree') && b:NERDTree.isTabTree() | quit | endif
" "" Open the existing NERDTree on each new tab.
" autocmd BufWinEnter * if getcmdwintype() == '' | silent NERDTreeMirror | endif
" nnoremap <C-n> :NERDTreeToggle<CR>
"let g:vimtex_view_method = 'zathura'
"let g:vimtex_quickfix_mode = 0
"let g:tex_flavor = 'latex'
" hidden latex code when the pointer is over it
" set conceallevel=1
let g:javascript_plugin_jsdoc = 1
" Colorscheme for coc
func! s:my_colors_setup() abort
  highlight CocFloating ctermbg=Black " For background color
endfunc

augroup colorscheme_coc_setup | au!
  au VimEnter * call s:my_colors_setup()
augroup END
set termguicolors
if $HOSTNAME == "fedora-pc"
" colors
" colorscheme gruvbox
    colorscheme nord
else
    colorscheme gruvbox
"    colorscheme nord
endif
if exists("colors_name")
    if colors_name == "nord"
        " nord theme
        set cursorline
        let g:nord_cursor_line_number_background = 1
        let g:nord_bold = 1
        let g:nord_italic = 1
        let g:nord_italic_comments = 1
        let g:nord_underline = 1
        set background=dark
        " reload the theme to apply settings
        colorscheme nord
    elseif colors_name == "gruvbox"
        set background=dark
        set cursorline
        let g:gruvbox_cursor_line_number_background = 1
        let g:gruvbox_bold = 1
        let g:gruvbox_italic = 1
        let g:gruvbox_italic_comments = 1
        let g:gruvbox_underline = 1
    endif
endif
"let g:gutentangs_project_root = ['.git', '.hg', '.svn', '.root', '.project']
"let g:gutentags_cache_dir = '~/.cache/gutentags'
"let g:gutentags_ctags_tagfile = '.tags'
"let g:gutentags_ctags_extra_args = ['--fields=+ailmnS', '--tag-relative=yes']
"let g:gutentags_ctags_exclude = ['*.min.js', '*.min.css', '*.map', 'node_modules', 'test', 'cache', 'dist', 'build', 'vendor', '.*', '*/.*', '*.json', '*.yml', '*.html', '*.txt', '*.cpy', '*.css', 'bin', '*.md', '*.org']
" for vimwiki
" let g:vimwiki_list = [{'path': '~/Documents/Obsidian_vault/',
"                      \ 'syntax': 'markdown', 'ext': '.md'}]
" let g:vimwiki_global_ext = 0
" let g:vimwiki_table_mappings=0
let g:wiki_root = '~/Documents/Obsidian_vault'
let g:wiki_journal = { 'name': '05_DAILY_NOTES', 'root': '', 'frequency': 'daily'}
let g:vimwiki_key_mappings = { 'table_mappings': 0, }
nnoremap <leader>ww :WikiIndex<CR>
nnoremap <leader>wj :WikiJournal<CR>
nnoremap <leader>ff :WikiPages<CR>
nnoremap <leader>fo :WikiOpen<CR>
" let g:wiki_map_create_page = 'AddDateAsPrefix'
" 
" function AddDateAsPrefix(name) abort
"     let l:name = wiki#get_root . '/' . a:name
" 
"     " If the tile is new, then append the current date
"     return filereadable(l:name) ? a:name : strftime(%Y%m%d%H%M%S') . '_' .  a:name
" endfunction
" text mode
autocm BufNewFile,BufRead *.md,*.tex,*.org setlocal
	\ textwidth=80
	\ fileformat=unix
	\ cc=80
	\ spell
	\ spelllang=es
	\ wrap

" au BufRead,BufNewFile *.{md,mdown,mkd,mkdn,markdown,mdwn,mdx} set filetype=markdown
" autocmd BufNewFile *.md 0r ~/.vim/skeletons/headers.md

" spell check for gitcommit
autocmd FileType gitcommit setlocal spell spelllang=en_us

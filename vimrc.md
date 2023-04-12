---
tangle: .vimrc
Author: Lucas Elvira Martín
date: 2023-04-02
Description: My vim configuration
---

# My vim configuration
This is my custom vim configuration writen in markdown and tangle to vimscript
using vim-tangle plugin.

## Set up common settings

### Default values
First we need to load the default vim because, if we don't do that, we will loss
some of the default settings

```vimscript
source $VIMRUNTIME/defaults.vim
```
Next there is some of the global variables and mode

```vimscript
syntax on
filetype on
filetype plugin indent on
```

### Global rules

This rules enable in this order:

1. Read the first of last line with vim setting for the current buffer
2. Display numbers
3. Disable highlight after search something
4. Allow incremental search
5. Disable mouse
6. Disable wrap lines
7. Dispaly a line in the column number 80
8. Add space to the hidden characters that can be displayed
9. Display line and column on the bottom


```vimscript
set modelines=1
set number
set nohlsearch
set incsearch
set mouse=c
set nowrap
set cc=80
set listchars+=space:·,tab:»\ ,trail:~,extends:>,precedes:<,nbsp:·
set ruler
```

#### Indent and folding rules

```vimscript
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
```


#### Disable status line 

```vimscript
" status bar
set noshowmode
set noshowcmd
set laststatus=0
```


compatibility mode has some problems with new plugins
```vimscript
set nocompatible
```

### Custom mapping

I don't like to use arrows keys in insert mode because it is a reference of the mode I am in. So I disable them.

```vimscript
" disable arrows keys in insert mode
inoremap <left> <nop>
inoremap <right> <nop>
inoremap <up> <nop>
inoremap <down> <nop>
```

```vimscript
vnoremap <C-r> "hy:%s/<C-r>h//gc<Left><left><left>
vnoremap <silent> <Leader>y :w !xclip -i -sel c<CR>
nnoremap <F3> :set relativenumber!<CR>
nnoremap <F4> :setlocal list!<CR>
nnoremap <F5> :setlocal spell<CR>:set spelllang=es<CR>
nnoremap <F6> :setlocal spell<CR>:set spelllang=en<CR>

nnoremap <C-b> :bnext<CR>
nnoremap <C-S-b> :bprevious<CR>
```

## Install plugins

For the plugin managment I use Pluged. To install it for vim:

```shell
curl -fLo ~/.vim/autoload/plug.vim --create-dirs \
    https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim
```

To install in neovim

```shell
sh -c 'curl -fLo "${XDG_DATA_HOME:-$HOME/.local/share}"/nvim/site/autoload/plug.vim --create-dirs \
       https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim'
```

### List of pluggins

```vimscript
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
Plug 'christoomey/vim-tmux-navigator'

" lsp
Plug 'neoclide/coc.nvim', {'branch': 'release'}

" languages
Plug 'lervag/vimtex'
Plug 'sheerun/vim-polyglot'
"Plug 'craigemery/vim-autotag'
Plug 'davidhalter/jedi-vim'
Plug 'aklt/plantuml-syntax'
Plug 'godlygeek/tabular'
Plug 'preservim/vim-markdown'

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


" vim-table-mode
Plug 'dhruvasagar/vim-table-mode'


" custom pluging
Plug 'luelvira/vim-tangle'
call plug#end()
```

### Configure UltiSnip
```vimscript
let g:UltiSnipsExpandTrigger="<tab>"
let g:UltiSnipsJumpForwardTrigger="<tab>"
let g:UltiSnipsJumpBackwardTrigger="<S-tab>"
let g:UltiSnipsListSnippets="<C-l>"
```

### Configure repeat
This plugin allow to repeat command similar to surround

```vimscript
silent! call repeat#set("\<Plug>MyWonderfulMap", v:count)
````

### nerdtree
Display a file manager on the left panel and allows navigation over it

```vimscript
autocmd BufEnter * if tabpagenr('$') == 1 && winnr('$') == 1 && exists('b:NERDTree') && b:NERDTree.isTabTree() | quit | endif
" Close the tab if NERDTree is the only window remaining in it.
autocmd BufEnter * if winnr('$') == 1 && exists('b:NERDTree') && b:NERDTree.isTabTree() | quit | endif
"" Open the existing NERDTree on each new tab.
autocmd BufWinEnter * if getcmdwintype() == '' | silent NERDTreeMirror | endif
nnoremap <C-n> :NERDTreeToggle<CR>
```

### vimtex

This plugin improves the experience of write in latex with vim

```vimscript
let g:vimtex_view_method = 'zathura'
let g:vimtex_quickfix_mode = 0
let g:tex_flavor = 'latex'
" hidden latex code when the pointer is over it
" set conceallevel=1
```

### jsdoc

Plugin to make easy write javascript documentation
```vimscript
let g:javascript_plugin_jsdoc = 1
```

### Coc
Coc is a lsp plugin. It uses nodejs as backend and has its own package manager.
To call it, you run `:CocInstall ` followed for the name of the package.

```vimscript
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
```
Coc has a problem with the color scheme if you don't use a theme

```vimscript
" Colorscheme for coc
func! s:my_colors_setup() abort
  highlight CocFloating ctermbg=Black " For background color
endfunc

augroup colorscheme_coc_setup | au!
  au VimEnter * call s:my_colors_setup()
augroup END
```

## Set a custom theme

The themes are installed with vim-plug. To set a theme, you need to add the
following line to your vimrc with the name of the theme you want to use. I use
nord theme.

```vimscript
if $HOSTNAME == "fedora-pc"
" colors
" colorscheme dracula
colorscheme nord
endif
```

### Configure the theme
```vimscript
if exists("colors_name") && colors_name == "nord"
    " nord theme
    set cursorline
    let g:nord_cursor_line_number_background = 1
    let g:nord_bold = 1
    let g:nord_italic = 1
    " let g:nord_italic_comments = 1
    " let g:nord_underline = 1
endif
```

### Gutentags
This plugin allows to generate tags for the current project. It uses ctags and cscope.

```vimscript
let g:gutentangs_project_root = ['.git', '.hg', '.svn', '.root', '.project']
let g:gutentags_cache_dir = '~/.cache/gutentags'
let g:gutentags_ctags_tagfile = '.tags'
let g:gutentags_ctags_extra_args = ['--fields=+ailmnS', '--tag-relative=yes']
let g:gutentags_ctags_exclude = ['*.min.js', '*.min.css', '*.map', 'node_modules', 'test', 'cache', 'dist', 'build', 'vendor', '.*', '*/.*', '*.json', '*.yml', '*.html', '*.txt', '*.cpy', '*.css', 'bin']
```
### vim table mode
To start the table mode `leader+tm` (leader + table mode).  Then you need to write the header delimite each item with `|` The plugin is in charge of give a space between the pipe and the words. 

Once you get the header and, without leaving Insert mode, enter `||` and a horizontal line will be displayed matched with the length of the table. Then you just need to write the content of your table

## Set local settings

With autcm you can enable or disable some settings for the current buffer.

```vimscript
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
```
### VIM ITALICS

There is a problem with italic letters in vim when using vim together tmux. The
espape character of *italics* are not well escaping. It is necessary to change it

```vimscript
" if exists('$TMUX')
" let &t_ZH="\e[3m"
" let &t_ZR="\e[23m"
" endif
```

Can not make it works


<!-- vim: set spelllang=en: filetype=markdown -->

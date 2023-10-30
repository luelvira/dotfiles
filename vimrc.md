---
tangle: .vimrc
Author: Lucas Elvira Martín
date: 2023-04-02
Description: My vim configuration
tags: CS/TOOLS/SW/TEXT_EDITOR
---

Contents
* [My vim configuration](#My vim configuration)
    * [Set up common settings](#My vim configuration#Set up common settings)
        * [Default values](#My vim configuration#Set up common settings#Default values)
        * [Global rules](#My vim configuration#Set up common settings#Global rules)
            * [Indent and folding rules](#My vim configuration#Set up common settings#Global rules#Indent and folding rules)
            * [Disable status line ](#My vim configuration#Set up common settings#Global rules#Disable status line )
        * [Custom mapping](#My vim configuration#Set up common settings#Custom mapping)
    * [Install plugins](#My vim configuration#Install plugins)
        * [List of pluggins](#My vim configuration#Install plugins#List of pluggins)
        * [Configure UltiSnip](#My vim configuration#Install plugins#Configure UltiSnip)
        * [Configure repeat](#My vim configuration#Install plugins#Configure repeat)
        * [nerdtree](#My vim configuration#Install plugins#nerdtree)
        * [vimtex](#My vim configuration#Install plugins#vimtex)
        * [jsdoc](#My vim configuration#Install plugins#jsdoc)
        * [Coc](#My vim configuration#Install plugins#Coc)
    * [Set a custom theme](#My vim configuration#Set a custom theme)
        * [Configure the theme](#My vim configuration#Set a custom theme#Configure the theme)
        * [Gutentags](#My vim configuration#Set a custom theme#Gutentags)
        * [vim table mode](#My vim configuration#Set a custom theme#vim table mode)
        * [vim wiki](#My vim configuration#Set a custom theme#vim wiki)
            * [Setup the wiki folder](#My vim configuration#Set a custom theme#vim wiki#Setup the wiki folder)
    * [Set local settings](#My vim configuration#Set local settings)

# My vim configuration
This is my custom vim configuration written in markdown and tangle to vimscript
using vim-tangle plugin.

## Set up common settings

### Default values
First we need to load the default vim because, if we don't do that, we will loss
some of the default settings

```vim
source $VIMRUNTIME/defaults.vim
```
Next there is some of the global variables and mode

```vim
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
7. Display a line in the column number 80
8. Add space to the hidden characters that can be displayed
9. Display line and column on the bottom


```vim
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

```vim
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

```vim
" status bar
set noshowmode
set noshowcmd
set laststatus=0
```

Compatibility mode has some problems with new plugins

```vim
set nocompatible
```

### Custom mapping

I don't like to use arrows keys in insert mode because it is a reference of the mode I am in. So I disable them.

```vim
" disable arrows keys in insert mode
inoremap <left> <nop>
inoremap <right> <nop>
inoremap <up> <nop>
inoremap <down> <nop>
```

```vim
vnoremap <C-r> "hy:%s/<C-r>h//gc<Left><left><left>
vnoremap <silent> <Leader>y :w !xclip -i -sel c<CR>
nnoremap <F3> :setlocal relativenumber!<CR>
nnoremap <F4> :setlocal list!<CR>
nnoremap <F5> :setlocal spell<CR>:set spelllang=es<CR>
nnoremap <F6> :setlocal spell<CR>:set spelllang=en<CR>
nnoremap <F7> :setlocal spell!<CR>
```

## Install plugins

For the plugin management I use Pluged. To install it for vim:

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

```vim
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
```

### Configure UltiSnip
```vim
let g:UltiSnipsExpandTrigger="<tab>"
let g:UltiSnipsJumpForwardTrigger="<tab>"
let g:UltiSnipsJumpBackwardTrigger="<S-tab>"
let g:UltiSnipsListSnippets="<C-l>"
```

### Configure repeat
This plugin allow to repeat command similar to surround

```vim
silent! call repeat#set("\<Plug>MyWonderfulMap", v:count)
````

### nerdtree
Display a file manager on the left panel and allows navigation over it

```vim
" autocmd BufEnter * if tabpagenr('$') == 1 && winnr('$') == 1 && exists('b:NERDTree') && b:NERDTree.isTabTree() | quit | endif
" " Close the tab if NERDTree is the only window remaining in it.
" autocmd BufEnter * if winnr('$') == 1 && exists('b:NERDTree') && b:NERDTree.isTabTree() | quit | endif
" "" Open the existing NERDTree on each new tab.
" autocmd BufWinEnter * if getcmdwintype() == '' | silent NERDTreeMirror | endif
" nnoremap <C-n> :NERDTreeToggle<CR>
```

### vimtex

This plugin improves the experience of write in latex with vim

```vim
"let g:vimtex_view_method = 'zathura'
"let g:vimtex_quickfix_mode = 0
"let g:tex_flavor = 'latex'
" hidden latex code when the pointer is over it
" set conceallevel=1
```

### jsdoc

Plugin to make easy write javascript documentation
```vim
let g:javascript_plugin_jsdoc = 1
```

### Coc

**Disclaimer:** This plugin make the computer a lot more slowly

Coc is a lsp plugin. It uses nodejs as backend and has its own package manager.
To call it, you run `:CocInstall ` followed for the name of the package.

```vim_no_tangle
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

```vim
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

` colorscheme themename`

**IMPORTANT**

Most of themes in terminal have some problems with some kind of fonts like
italic. To prevent it, *before* set the it is necessary to setup some vars

```vim
set termguicolors
if $HOSTNAME == "fedora-pc"
" colors
" colorscheme gruvbox
    colorscheme nord
else
"    colorscheme gruvbox
"    colorscheme nord
endif
```

### Configure the theme
```vim
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
```

### Gutentags

This plugin allows to generate tags for the current project. It uses ctags and
cscope.

```vim
"let g:gutentangs_project_root = ['.git', '.hg', '.svn', '.root', '.project']
"let g:gutentags_cache_dir = '~/.cache/gutentags'
"let g:gutentags_ctags_tagfile = '.tags'
"let g:gutentags_ctags_extra_args = ['--fields=+ailmnS', '--tag-relative=yes']
"let g:gutentags_ctags_exclude = ['*.min.js', '*.min.css', '*.map', 'node_modules', 'test', 'cache', 'dist', 'build', 'vendor', '.*', '*/.*', '*.json', '*.yml', '*.html', '*.txt', '*.cpy', '*.css', 'bin', '*.md', '*.org']
```
### vim table mode
To start the table mode `leader+tm` (leader + table mode).  Then you need to
write the header delimite each item with `|` The plugin is in charge of give a
space between the pipe and the words. 

Once you get the header and, without leaving Insert mode, enter `||` and a
horizontal line will be displayed matched with the length of the table. Then you
just need to write the content of your table

### vim wiki

### wikivm pluggin

#### Changing Wiki syntax

vimwiki currently ships with 3 syntaxes: VimWiki, Markdown and MediaWiki. I
would like to keep using markdown

Also I set the path of the wiki folder

```vim
" for vimwiki
" let g:vimwiki_list = [{'path': '~/Documents/Obsidian_vault/',
"                      \ 'syntax': 'markdown', 'ext': '.md'}]
```

To prevent vim treats all markdown as vimwiki
```vim
" let g:vimwiki_global_ext = 0
" let g:vimwiki_table_mappings=0
```

### vimwiki from lervag

The first is to customize the root folder and the journal folder

```vim
let g:wiki_root = '~/Documents/Obsidian_vault'
let g:wiki_journal = { 'name': '05_DAILY_NOTES', 'root': '', 'frequency': 'daily'}
```

Now we will setup some custom keybinding

```vim
let g:vimwiki_key_mappings = { 'table_mappings': 0, }
nnoremap <leader>ww :WikiIndex<CR>
nnoremap <leader>wj :WikiJournal<CR>
nnoremap <leader>ff :WikiPages<CR>
nnoremap <leader>fo :WikiOpen<CR>
```

Also could be interesting get a function which generate a unique ID as prefix
for the file. To do it, the file must be open with `WikiOpen`

```vim
" let g:wiki_map_create_page = 'AddDateAsPrefix'
" 
" function AddDateAsPrefix(name) abort
"     let l:name = wiki#get_root . '/' . a:name
" 
"     " If the tile is new, then append the current date
"     return filereadable(l:name) ? a:name : strftime(%Y%m%d%H%M%S') . '_' .  a:name
" endfunction
```

**Need to be completed this part**

### vim-taskwarrior

This plugin is an interface to the [[task warrior]] program. In order to view
the pending task: `:TW`. More info in [vim-taskwarrior](https://github.com/blindFS/vim-taskwarrior)

## Set local settings

With autocm you can enable or disable some settings for the current buffer.

```vim
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
```


### Templates
Vim file templates (known as skeleton) are file that can be read when a file
with some pattern is created and write the content of the template inside the
fil. It is useful for markdown headers or html common parts

To enable it you need to put in your config file:
```
autocmd BufNewFile <pattern> 0r <path of the file>
```
- `autocmd` is a command to run automatically on some events
- `BufNewFile`: Vim's new file events
- `0r` read into the buffer starting at line 0
source: [vim templates](https://vimtricks.com/p/vim-file-templates/)


## Custom function

Each time I want to write in vim in English, I need to change the spelllang
option. This command will automatically at it to the last line


<!-- vim: set spelllang=en: -->

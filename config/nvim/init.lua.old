require("plugins")
require("keymaps")
vim.cmd("source ~/.config/nvim/nerdtree.vim")


-- get or set general settings (vim.o)
-- vim.o.termguicolors = true
-- vim.o.background = 'dark'
vim.o.hlsearch = false -- disable highlight after search
vim.opt.incsearch = true
vim.o.mouse = 'c'
vim.o.updatetime = 1000

-- get or set window-scopet (vim.wo)
vim.wo.number = true
vim.wo.wrap = false
vim.wo.colorcolumn = "80"

-- get or set global, window and buffer setting (vim.opt)
-- vim.opt.lcs = "eol:$,space:·"
vim.opt.listchars = {eol = '$', trail = '^', extends = '>', precedes = '<', space= '·'}
vim.opt.foldenable = true
vim.opt.foldmethod = 'indent'
vim.opt.foldlevel=99
vim.opt.showmode = false

vim.opt.expandtab = true
vim.opt.tabstop=4
vim.opt.softtabstop=4
vim.opt.shiftwidth=4
vim.opt.copyindent = true
vim.opt.preserveindent = true
vim.opt.autoindent = true

vim.opt.laststatus = 0
vim.opt.showcmd = false
vim.opt.syntax = "on"
vim.opt.cursorline = true
vim.opt.cursorlineopt = "number"

vim.opt.wildmode= "longest,list"
vim.highlight.create("cursorlinenr", {cterm="bold", term="bold"})


-- keyboard mapping

vim.keymap.set('i','<left>', '<nop>')
vim.keymap.set('i','<right>', '<nop>')
vim.keymap.set('i','<up>', '<nop>')
vim.keymap.set('i','<down>', '<nop>')

vim.keymap.set('n', '<F2>', ':set invpaste paste?<CR>')
vim.keymap.set('i', '<F2>', ':set invpaste paste?<CR>')
vim.keymap.set('n', '<F3>', ':set relativenumber!<CR>')
vim.keymap.set('n', '<F4>', ':set list!<CR>')
vim.keymap.set('v', '<F4>', ':set list!<CR>')
vim.keymap.set('n', '<F5>', ':set spell!<CR>:set spelllang=es<CR>')

-- pluggins var

vim.g.UltiSnipsExpandTrigger="<tab>"
vim.g.UltiSnipsJumpForwardTrigger="<tab>"
vim.g.UltiSnipsJumpBackwardTrigger="<S-tab>"


vim.api.nvim_create_autocmd({"BufNewFile", "BufRead"}, {
	pattern = "*.py",
	callback= function()
			vim.opt.tabstop=4 
			vim.opt.softtabstop=4 
			vim.opt.shiftwidth=4 
			vim.opt.expandtab = true
			vim.opt.autoindent = true
			vim.opt.fileformat=unix 
	end
})

vim.api.nvim_create_autocmd({"BufNewFile", "BufRead"}, {
	pattern = "*tex",
	callback = function() 
		vim.opt.tabstop=2 
		vim.opt.softtabstop = 2
		vim.opt.shiftwidth=2 
		vim.opt.textwidth=80 
		vim.opt.expandtab = false
		vim.opt.autoindent = true
		vim.opt.fileformat=unix 
		vim.opt.spelllang=en, es
		vim.opt.spellsuggest=best,5
		vim.opt.spell = true
		vim.keymap.set('v', '<C-b>', ':s/.*/--&--/')
		vim.opt.fileformat=unix 
	end
})

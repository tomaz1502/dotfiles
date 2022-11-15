-- init.lua (Author: Tomaz Gomes Mascarenhas)
-- GitHub : https://github.com/tomaz1502/dotfiles/blob/master/.vimrc

-- THIS MUST COME FIRST!!!!
-- vim.cmd("colorscheme base16-gruvbox-light-medium")
-- vim.opt.laststatus = 0
-- vim.cmd("hi Normal guibg=NONE")
vim.opt.number = true
vim.opt.relativenumber = false
vim.opt.cursorline = true

-- Requires {{{
vim.opt.runtimepath = vim.opt.runtimepath + "/home/tomazgomes/.config/nvim/lua"
require('pack')
require('zen_mode')
require('color_catppuccin_mocha')
---require('color_base16_default_dark')
require('lsp_conf')
-- }}}

-- Options {{{
vim.cmd("filetype plugin on")
vim.cmd("filetype plugin indent on")
vim.cmd("syntax on")

vim.opt.numberwidth = 5
vim.opt.mouse = 'a'

vim.opt.shiftwidth = 4
vim.opt.tabstop = 4
vim.opt.expandtab = true

vim.opt.hidden = true
vim.opt.wildmenu = true

vim.opt.backup = false
vim.opt.writebackup = false
vim.opt.swapfile = false

vim.opt.ignorecase = true
vim.opt.smartcase = true
-- vim.opt.textwidth = 80

vim.opt.clipboard = vim.opt.clipboard + 'unnamedplus'
vim.opt.signcolumn = 'no'
vim.opt.termguicolors = true

vim.opt.foldmethod = 'marker'
vim.opt.foldenable = true
vim.opt_local.colorcolumn = '+' .. vim.fn.join(vim.fn.range(0, 254), ',+')
-- TODO vim.opt.inccomand = 'nosplit'
-- }}}

-- Maps {{{
local map = vim.api.nvim_set_keymap

vim.g.mapleader = ","

map("i", "(", "()<left>",    { noremap = true })
map("i", "[", "[]<left>",    { noremap = true })
map("i", "\"", "\"\"<left>", { noremap = true })
map("i", "{<CR>", "{<CR><ESC>o}<UP><ESC>a", { noremap = true })
map("i", "jk", "<Esc>", { noremap = true })
map("i", "kj", "<Esc>", { noremap = true })

map("n", "<Up>", "<Nop>", { noremap = true })
map("n", "<Down>", "<Nop>", { noremap = true })
map("n", "<Left>", "<Nop>", { noremap = true })
map("n", "<Right>", "<Nop>", { noremap = true })
map("n", "<C-h>", "<C-w>h", { noremap = true })
map("n", "<C-l>", "<C-w>l", { noremap = true })
map("n", "<C-j>", "<C-w>j", { noremap = true })
map("n", "<C-k>", "<C-w>k", { noremap = true })
map("n", "Y", "y$", { noremap = true })
map("n", "<Leader>f", "<C-^>", { noremap = true })
map("n", "<Leader>ev", ":e ~/.config/nvim/init.lua<CR>", { silent = true, noremap = true })
map("n", "<Leader>es", ":source ~/.config/nvim/init.lua<CR>", { noremap = true })
map("n", "<Tab>", "za", { noremap = true })
map("n", "/", "/\\c", { noremap = true })
map("n", "?", "?\\c", { noremap = true })
map("n", " ", ":nohlsearch<Bar>:echo<CR>", { silent = true, noremap = true })
map("n", "<Leader>t", ":Telescope find_files<CR>", { silent = true, noremap = true })
map("n", "<Leader>b", ":Telescope buffers<CR>", { silent = true, noremap = true })
map("n", "<Leader>h", ":Telescope oldfiles<CR>", { silent = true, noremap = true })
map("n", "<Leader>r", ":Telescope live_grep<CR>", { silent = true, noremap = true })
map("n", "<Leader>z", ":ZenMode<CR>", { silent = true, noremap = true })
map("n", "<Leader>w", ":w<CR>", { silent = true, noremap = true })
map("n", "<Leader>q", ":q<CR>", { silent = true, noremap = true })

map("x", "(", "xi()<Esc>P", { noremap = true })
map("x", "[", "xi[]<Esc>P", { noremap = true })
map("x", "\"", "xi\"\"<Esc>P", { noremap = true })

map("x", "ga", "<Plug>(EasyAlign)", { silent = true, noremap = true })
map("n", "ga", "<Plug>(EasyAlign)", { silent = true, noremap = true })

vim.g.maplocalleader = "m"

-- TODO vim.api.nvim_set_keymap("c", "<C-p>", "<C-r>\"", { noremap = true })
-- }}}

-- Autocmd {{{
vim.cmd("autocmd TextYankPost * silent! lua require'vim.highlight'.on_yank { higroup=\"Substitute\", timeout=200 }")
--}}}

-- vim.cmd("set spell")
-- vim.cmd("set spelllang=pt_br")

vim.cmd("let g:slime_target = \"tmux\"")

inspect = require("inspect")

vim.cmd("autocmd FileType sml setlocal commentstring=(*%s*)")

vim.cmd("set guicursor=n-v-c-i:block")

vim.cmd("let g:livepreview_previewer = 'zathura'")

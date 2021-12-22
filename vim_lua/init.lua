-- init.lua (Author: Tomaz Gomes Mascarenhas)
-- GitHub : https://github.com/tomaz1502/dotfiles/blob/master/.vimrc

-- THIS MUST COME FIRST!!!!
vim.cmd("colorscheme base16-default-dark")
vim.cmd("syntax on")

vim.opt.runtimepath = vim.opt.runtimepath + "/home/tomazgomes/.config/nvim/lua"
vim.opt.runtimepath = vim.opt.runtimepath + "/home/tomazgomes/.config/nvim/lua/plugins"
require('packer')
require('setup.zen_mode')
require('status_line')

-- Options {{{
vim.cmd("filetype plugin on")
vim.cmd("filetype plugin indent on")

vim.opt.number = true
vim.opt.mouse = 'a'
vim.opt.cursorline = true

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

vim.opt.laststatus = 2

vim.opt.clipboard = vim.opt.clipboard + 'unnamedplus'
vim.opt.signcolumn = 'no'
vim.opt.termguicolors = true

vim.opt.foldmethod = 'marker'
vim.opt.foldenable = false
-- TODO vim.opt.inccomand = 'nosplit'
-- }}}

-- Maps {{{
vim.g.mapleader = ","
vim.g.maplocalleader = ","

local map = vim.api.nvim_set_keymap

map("i", "(", "()<left>",    { noremap = true })
map("i", "[", "[]<left>",    { noremap = true })
map("i", "\"", "\"\"<left>", { noremap = true })
map("i", "{<CR>", "{<CR><ESC>o}<UP><ESC>a", { noremap = true })
map("i", "jk", "<Esc>", { noremap = true })

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
map("n", "<Leader>f", "<C-^>", { noremap = true })
map("n", "<Leader>ev", ":e ~/.config/nvim/init.lua<CR>", { noremap = true })
map("n", "<Leader>es", ":source ~/.config/nvim/init.lua<CR>", { noremap = true })
map("n", "<Tab>", "za", { noremap = true })
map("n", "/", "/\\v", { noremap = true })
map("n", "<Space>", ":nohlsearch<Bar>:echo<CR>", { silent = true, noremap = true })
map("n", "<Leader>t", ":Files<CR>", { silent = true, noremap = true })
map("n", "<Leader>b", ":Buffers<CR>", { silent = true, noremap = true })
map("n", "<Leader>w", ":w<CR>", { silent = true, noremap = true })
map("n", "<Leader>q", ":q<CR>", { silent = true, noremap = true })

map("x", "(", "xi()<Esc>P", { noremap = true })

-- TODO vim.api.nvim_set_keymap("c", "<C-p>", "<C-r>\"", { noremap = true })
-- }}}

-- Autocmd {{{

vim.cmd("autocmd TextYankPost * silent! lua require'vim.highlight'.on_yank { higroup=\"Substitute\", timeout=200 }")

--}

-- init.lua (Author: Tomaz Gomes Mascarenhas)
-- GitHub : https://github.com/tomaz1502/dotfiles/blob/master/.vimrc

-- THIS MUST COME FIRST!!!!
vim.cmd("colorscheme base16-default-dark")

-- Highlight Groups {{{
local highlights = {
    {'SL1',        { bg = '#282828', fg = '#b8b8b8', gui = 'Bold'}},
    {'SL2',        { bg = '#282828', fg = '#b8b8b8'              }},
    {'SL3',        { bg = 'Gray',    fg = '#282828'              }},
    {'ArSvd',      { bg = '#282828', fg = 'Red'                  }},
    {'ArMod',      { bg = '#282828', fg = 'Gold'                 }},
    {'ArRgt',      { bg = '#282828', fg = 'Gray'                 }},
    {'BgMod',      { bg = 'Gold'                                 }},
    {'BgSvd',      { bg = 'Red'                                  }}
}

local set_hl = function(group, options)
  local bg = options.bg == nil and '' or 'guibg=' .. options.bg
  local fg = options.fg == nil and '' or 'guifg=' .. options.fg
  local gui = options.gui == nil and '' or 'gui=' .. options.gui

  vim.cmd(string.format('hi %s %s %s %s', group, bg, fg, gui))
end

for _, highlight in ipairs(highlights) do
  set_hl(highlight[1], highlight[2])
end
--}}}

-- Requires {{{
vim.opt.runtimepath = vim.opt.runtimepath + "/home/tomazgomes/.config/nvim/lua"
require('pack')
require('zen_mode')
require('status_line')
require('lsp_conf')
-- }}}

-- Options {{{
vim.cmd("filetype plugin on")
vim.cmd("filetype plugin indent on")
vim.cmd("syntax on")

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
vim.opt.textwidth = 80

vim.opt.laststatus = 2

vim.opt.clipboard = vim.opt.clipboard + 'unnamedplus'
vim.opt.signcolumn = 'no'
vim.opt.termguicolors = true

vim.opt.foldmethod = 'marker'
vim.opt.foldenable = true
-- vim.opt_local.colorcolumn = '+' .. vim.fn.join(vim.fn.range(0, 254), ',+')
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


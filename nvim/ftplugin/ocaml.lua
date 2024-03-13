vim.opt.tabstop = 2
vim.opt.shiftwidth = 2

vim.api.nvim_set_keymap("n", "<Leader>c", ":!dune build<CR>", { noremap = true })
vim.api.nvim_set_keymap("n", "<Leader>C", ":!dune build && dune install<CR>", { noremap = true })

vim.opt.makeprg = "dune build"

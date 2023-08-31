local lazypath = vim.fn.stdpath("data") .. "/lazy/lazy.nvim"
if not vim.loop.fs_stat(lazypath) then
  vim.fn.system({
    "git",
    "clone",
    "--filter=blob:none",
    "https://github.com/folke/lazy.nvim.git",
    "--branch=stable", -- latest stable release
    lazypath,
  })
end
vim.opt.rtp:prepend(lazypath)

require("lazy").setup({
    "tpope/vim-commentary"
  -- , { "kylechui/nvim-surround", tag = "*", config = function() require("nvim-surround").setup({}) end }
  , "tpope/vim-surround"
  , "justinmk/vim-dirvish"
  , "nvim-telescope/telescope.nvim"
  , "folke/zen-mode.nvim"
  , "neovimhaskell/haskell-vim"
  , "neovim/nvim-lspconfig"
  , "hrsh7th/nvim-compe"
  , "Julian/lean.nvim"
  , "nvim-lua/plenary.nvim"
  , "andrewradev/switch.vim"
  , "junegunn/vim-easy-align"
  , { "catppuccin/nvim", name = "catppuccin", priority = 1000 }
})

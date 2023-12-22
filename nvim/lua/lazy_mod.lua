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
  , "tpope/vim-surround"
  , "justinmk/vim-dirvish"
  , "nvim-telescope/telescope.nvim"
  , "folke/zen-mode.nvim"
  , "neovimhaskell/haskell-vim"
  , "neovim/nvim-lspconfig"
  , "hrsh7th/nvim-cmp"
  , 'hrsh7th/cmp-nvim-lsp'
  , 'hrsh7th/cmp-buffer'
  , 'hrsh7th/cmp-path'
  , 'hrsh7th/cmp-cmdline'
  , "Julian/lean.nvim"
  , "nvim-lua/plenary.nvim"
  , "andrewradev/switch.vim"
  , "junegunn/vim-easy-align"
  , "preservim/vim-markdown"
  , { "catppuccin/nvim", name = "catppuccin", priority = 1000 }
  , {
      "epwalsh/obsidian.nvim",
      -- lazy = true,
      event = {
          -- "BufReadPre /home/tomazgomes/Vault/Vault/**.md",
          -- "BufRead    /home/tomazgomes/Vault/Vault/**.md",
          -- "BufNewFile /home/tomazgomes/Vault/Vault/**.md",
          "BufNew *" -- hack
      },
      dependencies = { "nvim-lua/plenary.nvim" },
      opts = { dir = "~/Vault/Vault" },
      open_app_foreground = true
  }
  , "jlapolla/vim-coq-plugin"
})

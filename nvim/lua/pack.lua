require('zen_mode')

vim.cmd [[packadd packer.nvim]]

return require('packer').startup(function()
    use 'wbthomason/packer.nvim'
    use 'nvim-telescope/telescope.nvim'
    use 'justinmk/vim-dirvish'
    use 'tpope/vim-commentary'
    use 'ggandor/lightspeed.nvim'
    use 'folke/zen-mode.nvim'
    use 'neovimhaskell/haskell-vim'
    use 'monkoose/fzf-hoogle.vim'
    use 'neovim/nvim-lspconfig'
    use 'hrsh7th/nvim-compe'
    use 'Julian/lean.nvim'
    use 'nvim-lua/plenary.nvim'
    use 'andrewradev/switch.vim'
    use 'xuhdev/vim-latex-live-preview'
    use 'mateusbraga/vim-spell-pt-br'
    use 'jpalardy/vim-slime'
    use 'junegunn/vim-easy-align'
    use 'bohlender/vim-smt2'
    use 'psiska/telescope-hoogle.nvim'
    use 'catppuccin/nvim'
end)



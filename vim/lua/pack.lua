require('zen_mode')

vim.cmd [[packadd packer.nvim]]

return require('packer').startup(function()
    use 'wbthomason/packer.nvim'
    use 'junegunn/fzf.vim'
    use { 'junegunn/fzf',
          run = function() vim.fn('fzf#install()') end
        }
    use 'justinmk/vim-dirvish'
    use 'tpope/vim-commentary'
    use 'jremmen/vim-ripgrep'
    use 'ggandor/lightspeed.nvim'
    use 'folke/zen-mode.nvim'
    use 'neovimhaskell/haskell-vim'
    use 'monkoose/fzf-hoogle.vim'
    use 'neovim/nvim-lspconfig'
    use 'hrsh7th/nvim-compe'
    use 'Julian/lean.nvim'
    use 'nvim-lua/plenary.nvim'
    use 'andrewradev/switch.vim'
end)



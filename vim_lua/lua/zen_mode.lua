return require("zen-mode").setup {
  plugins = {
    tmux = { enabled = true },
  },
  on_open = function(win)
    vim.opt.number = false;
    vim.opt.relativenumber = false;
  end,
  on_close = function(win)
    vim.opt.number = true;
    vim.opt.relativenumber = true;
  end
}

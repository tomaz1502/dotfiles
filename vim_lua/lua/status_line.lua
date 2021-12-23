local M = {}

M.info = function(self)
    local folder_name = vim.inspect(vim.fn.expand("%:p:h:t"))
    return table.concat(
      { "%#SL1# ", "%{", folder_name, "}/%t" -- left side
      , "%=%y ", "%#ArRgt#", "", "%#SL3#", " ℓ %l/%L  𝐜 %c/%{1 + strlen(getline(\".\"))} " -- right side
      }
    )
end

M.set_saved = function(self)
    local info = self:info()
    return table.concat({"%#BgSvd#", "    ", "%#ArSvd#", "", info })
end

M.set_modified = function(self)
    local info = self:info()
    return table.concat({"%#BgMod#", "    ", "%#ArMod#", "", info })
end

M.set_inactive = function(self)
    return "%#SL2#%=%f%="
end

Statusline = setmetatable(M, {
    __call = function(statusline, mode)
        if mode == 'saved' then return statusline:set_saved() end
        if mode == 'modified' then return statusline:set_modified() end
        if mode == 'inactive' then return statusline:set_inactive() end
    end
})

function blur_window()
  vim.opt_local.colorcolumn = vim.fn.join(vim.fn.range(1, 254), ',')
  vim.opt_local.statusline = Statusline('inactive')
end

function focus_window()
  vim.opt_local.colorcolumn = '+' .. vim.fn.join(vim.fn.range(0, 254), ',+')
  if vim.bo.modified then
      vim.opt_local.statusline = Statusline('modified')
  else
      vim.opt_local.statusline = Statusline('saved')
  end
end

vim.cmd([[
  autocmd VimEnter,WinNew,TabNew,BufNew,BufRead,BufWrite * setlocal statusline=%!v:lua.Statusline('saved')
  autocmd TextChanged,TextChangedI,TextChangedP,InsertChange * setlocal statusline=%!v:lua.Statusline('modified')
  autocmd BufEnter,FocusGained,VimEnter,WinEnter * lua focus_window()
  autocmd FocusLost,WinLeave * lua blur_window()
]])

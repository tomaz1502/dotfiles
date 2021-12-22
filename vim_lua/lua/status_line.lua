-- vim.cmd("hi IM gui=Bold guifg=White guibg=16")

local highlights = {
    {'SL1',        { bg = '#282828', fg = '#b8b8b8', gui = 'Bold'}},
    {'SL2',        { bg = '#282828', fg = '#b8b8b8'              }},
    {'SL3',        { bg = 'Gray',    fg = '#282828'              }},
    {'ArSvd', { bg = '#282828', fg = 'Red'                       }},
    {'ArMod',   { bg = '#282828', fg = 'Gold'                    }},
    {'ArRgt', { bg = '#282828', fg = 'Gray'                      }},
    {'BgMod',   { bg = 'Gold'                                 }},
    {'BgSaved',      { bg = 'Red'                                  }}
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

local M = {}

M.info = function(self)
    local folder_name = vim.inspect(vim.fn.expand("%:p:h:t"))
    return table.concat({ "%#SL1# ", "%{", folder_name, "}/%t" -- left side
                        , "%= ", "%#ArRgt#", "", "%#SL3#", " ℓ %l/%L  𝐜 %c/%{1 + strlen(getline(\".\"))} " -- right side
                        })
end

M.set_saved = function(self)
    local info = self:info()
    return table.concat({"%#BgSaved#", "    ", "%#ArSvd#", "", info })
end

M.set_modified = function(self)
    local info = self:info()
    return table.concat({"%#BgMod#", "    ", "%#ArMod#", "", info })
end

Statusline = setmetatable(M, {
    __call = function(statusline, mode)
        if mode == 'saved' then return statusline:set_saved() end
        if mode == 'modified' then return statusline:set_modified() end
    end
})

vim.cmd([[
  autocmd VimEnter,WinNew,TabNew,BufNew,BufRead,BufWrite * setlocal statusline=%!v:lua.Statusline('saved')
  autocmd TextChanged,TextChangedI,TextChangedP,InsertChange * setlocal statusline=%!v:lua.Statusline('modified')
]])

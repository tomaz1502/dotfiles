vim.cmd("colorscheme gruvbox")
vim.opt.laststatus = 2
vim.cmd(":set background=light")

local orange  = '#d65d0e'
local blue1   = '#458588'
local blue2   = '#83a598'
local aqua1   = '#689d6a'
local yellow1 = '#d79921'
local yellow2 = '#fabd2f'
local gray2   = '#928374'
local purple1 = '#b16286'
local purple2 = '#8f3f71'

local bg1     = '#ebdbb2'
local bg2     = '#d5c4a1'

local fg = '#3c3836'
local fg2 = '#504945'
local fg4 = '#7c6f64'

local highlights = {
    {'SL1',        { bg = bg1,     fg = fg2, gui = 'Bold' }},
    {'SL2',        { bg = bg1,     fg = fg2               }},
    {'SL3',        { bg = yellow1, fg = fg2               }},
    {'ArSvd',      { bg = bg1,     fg = yellow1           }},
    {'ArMod',      { bg = bg1,     fg = purple1           }},
    {'ArRgt',      { bg = bg1,     fg = yellow1           }},
    {'BgSvd',      { bg = yellow1                         }},
    {'BgMod',      { bg = purple1                         }}
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

M.get_lsp_diagnostic = function(self)
  local result = {errors = 0 , warnings = 0, info = 0, hints = 0}
  local diagTypes = {'errors', 'warnings', 'info', 'hints'}

  for _, value in pairs(vim.diagnostic.get()) do
    local tp = diagTypes[value["severity"]]
    result[tp] = result[tp] + 1
  end

  return string.format(
    ":%s :%s :%s",
    result['errors'] or 0, result['warnings'] or 0,
    (result['info'] or 0) + (result['hint'] or 0)
  )
end

GetPad1 = function()
    local line = vim.api.nvim_win_get_cursor(0)[1]
    local height = vim.api.nvim_buf_line_count(0)
    local padding = #tostring(height) - #tostring(line)
    local pad =  ""
    if padding > 0 then
      pad = pad .. (' '):rep(padding)
    end
    return pad
end

GetPad2 = function()
    local column = vim.fn.virtcol('.')
    local width = vim.fn.virtcol('$')
    local padding = ""
    if #tostring(column) < 2 then
      padding = padding .. ' '
    end
    if #tostring(width) < 2 then
      padding = padding .. ' '
    end
    return padding
end

M.info = function(self)
    local folder_name = vim.inspect(vim.fn.expand("%:p:h:t"))
    local lsp_info = self:get_lsp_diagnostic()
    return table.concat(
      { "%#SL1# ", "%{", folder_name, "}/%#SL2#%t" -- left side
      , "%= ", lsp_info
      , "%=%y ", "%#ArRgt#", "", "%#SL3# ", "%{v:lua.GetPad1()}", "ℓ %l/%L  𝐜 %{virtcol('.')}/%{virtcol('$')} ", "%{v:lua.GetPad2()}" -- right side
      }
    )
end

M.set_saved = function(self)
    local info = self:info()
    return table.concat({"%#BgSvd#", "     ", "%#ArSvd#", "", info })
end

M.set_modified = function(self)
    local info = self:info()
    return table.concat({"%#BgMod#", "     ", "%#ArMod#", "", info })
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
  -- vim.opt_local.colorcolumn = vim.fn.join(vim.fn.range(1, 254), ',')
  -- vim.opt_local.statusline = Statusline('inactive')
end

function focus_window()
  -- vim.opt_local.colorcolumn = '+' .. vim.fn.join(vim.fn.range(0, 254), ',+')
  vim.opt_local.colorcolumn = ""
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
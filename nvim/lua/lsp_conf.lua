vim.opt.pumheight = 6
vim.opt.completeopt = { "menuone", "noselect" }

vim.g.compe = { enabled = true
              , autocomplete = true
              , debug = false
              , min_length = 1
              , preselect = "enable"
              , documentation = true
              , source = { path = true, buffer = true, nvim_lsp = true }
              }

local t = function(str)
  return vim.api.nvim_replace_termcodes(str, true, true, true)
end

local check_back_space = function()
    local col = vim.fn.col('.') - 1
    return col == 0 or vim.fn.getline('.'):sub(col, col):match('%s') ~= nil
end

_G.tab_complete = function()
  if vim.fn.pumvisible() == 1 then
    return t "<C-n>"
  elseif check_back_space() then
    return t "<Tab>"
  else
    return vim.fn['compe#complete']()
  end
end

_G.s_tab_complete = function()
  if vim.fn.pumvisible() == 1 then
    return t "<C-p>"
  else
    return t "<S-Tab>"
  end
end

vim.api.nvim_set_keymap("i", "<Tab>", "v:lua.tab_complete()", {expr = true})
vim.api.nvim_set_keymap("s", "<Tab>", "v:lua.tab_complete()", {expr = true})
vim.api.nvim_set_keymap("i", "<S-Tab>", "v:lua.s_tab_complete()", {expr = true})
vim.api.nvim_set_keymap("s", "<S-Tab>", "v:lua.s_tab_complete()", {expr = true})

local on_attach = require('on_attach')

require'lspconfig'.gopls.setup{ on_attach = on_attach }
require'lspconfig'.clangd.setup{ on_attach = on_attach,
                                 init_options = { fallbackFlags = { "-std=c++2a" } }
                               }
require'lspconfig'.pyright.setup{ on_attach = on_attach }
require'lspconfig'.rust_analyzer.setup{ on_attach = on_attach
                                      , single_file_support = true
                                      }

require'lspconfig'.hls.setup{ on_attach = on_attach
                            , single_file_support = true
                            }

require'lspconfig'.ocamllsp.setup{ on_attach = on_attach
                                 , single_file_support = true
                                 }
-- require'lspconfig'.texlab.setup{ on_attach = on_attach
--                                , single_file_support = true
--                                }

-- local sumneko_root_path = "/home/tomazgomes/Tools/lua-language-server"
-- local sumneko_binary = "/home/tomazgomes/Tools/lua-language-server/bin/Linux/lua-language-server"
-- 
-- require'lspconfig'.sumneko_lua.setup {
--     cmd = {sumneko_binary, "-E", sumneko_root_path .. "/main.lua"},
--     settings = {
--         Lua = {
--             runtime = {
--                 version = 'LuaJIT',
--                 path = vim.split(package.path, ';')
--             },
--             diagnostics = {
--                 globals = {'vim', 'use'}
--             },
--             workspace = {
--                 library = {[vim.fn.expand('$VIMRUNTIME/lua')] = true, [vim.fn.expand('$VIMRUNTIME/lua/vim/lsp')] = true}
--             }
--         }
--     }
-- }

require('lean').setup({
    lsp = { on_attach = on_attach },
    lsp3 = { on_attach = on_attach },
    abbreviations = {
        builtin = true, -- built-in expander
        compe = true, -- nvim-compe source
        snippets = false, -- snippets.nvim source
        extra = {
            wknight = '♘',
        },
        leader ='\\',
    },
    mappings = true,
    infoview = {
        autoopen = true,
        width = 50,
    },
    progress_bars = {
        enable = true,
        priority = 10,
    }
})

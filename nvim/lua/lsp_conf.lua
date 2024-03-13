vim.opt.pumheight = 6

vim.opt.completeopt = { "menu", "menuone", "noselect" }

local has_words_before = function()
  unpack = unpack or table.unpack
  local line, col = unpack(vim.api.nvim_win_get_cursor(0))
  return col ~= 0 and vim.api.nvim_buf_get_lines(0, line - 1, line, true)[1]:sub(col, col):match('%s') == nil
end

local cmp = require('cmp')
cmp.setup({
    sources =
      { { name = 'nvim_lsp', keyword_length = 3 }, { name = 'buffer', keyword_length = 3 },  { name = 'path', keyword_length = 2 }}
    , mapping = cmp.mapping.preset.insert({
       ['<C-b>'] = cmp.mapping.scroll_docs(-4),
       ['<C-f>'] = cmp.mapping.scroll_docs(4),
       ['<C-Space>'] = cmp.mapping.complete(),
       ['<C-e>'] = cmp.mapping.abort(),
       ['<CR>'] = cmp.mapping.confirm({ select = false }), -- Accept currently selected item. Set `select` to `false` to only confirm explicitly selected items.
       ['<Tab>'] = function(fallback)
         if not cmp.select_next_item() then
           if vim.bo.buftype ~= 'prompt' and has_words_before() then
             cmp.complete()
           else
             fallback()
           end
         end
       end,
   
       ['<S-Tab>'] = function(fallback)
         if not cmp.select_prev_item() then
           if vim.bo.buftype ~= 'prompt' and has_words_before() then
             cmp.complete()
           else
             fallback()
           end
         end
       end,
      }),

  snippet = {
    -- We recommend using *actual* snippet engine.
    -- It's a simple implementation so it might not work in some of the cases.
    expand = function(args)
      unpack = unpack or table.unpack
      local line_num, col = unpack(vim.api.nvim_win_get_cursor(0))
      local line_text = vim.api.nvim_buf_get_lines(0, line_num - 1, line_num, true)[1]
      local indent = string.match(line_text, '^%s*')
      local replace = vim.split(args.body, '\n', true)
      local surround = string.match(line_text, '%S.*') or ''
      local surround_end = surround:sub(col)

      replace[1] = surround:sub(0, col - 1)..replace[1]
      replace[#replace] = replace[#replace]..(#surround_end > 1 and ' ' or '')..surround_end
      if indent ~= '' then
        for i, line in ipairs(replace) do
          replace[i] = indent..line
        end
      end

      vim.api.nvim_buf_set_lines(0, line_num - 1, line_num, true, replace)
    end,
  },
})

-- Set configuration for specific filetype.
cmp.setup.filetype('gitcommit', {
  sources = cmp.config.sources({
    { name = 'git' }, -- You can specify the `git` source if [you were installed it](https://github.com/petertriho/cmp-git).
  }, {
    { name = 'buffer', keyword_length = 3 },
  })
})

-- Use buffer source for `/` and `?` (if you enabled `native_menu`, this won't work anymore).
cmp.setup.cmdline({ '/', '?' }, {
  mapping = cmp.mapping.preset.cmdline(),
  sources = {
    { name = 'buffer' }
  }
})

-- Use cmdline & path source for ':' (if you enabled `native_menu`, this won't work anymore).
cmp.setup.cmdline(':', {
  mapping = cmp.mapping.preset.cmdline(),
  sources = cmp.config.sources({
    { name = 'path' }
  }, {
    { name = 'cmdline' }
  })
})

local capabilities = require('cmp_nvim_lsp').default_capabilities()

local on_attach = require('on_attach')

require'lspconfig'.gopls.setup{ on_attach = on_attach, capabilities = capabilities }
require'lspconfig'.clangd.setup{ on_attach = on_attach,
                                 init_options = { fallbackFlags = { "-std=c++17" } },
                                 capabilities = capabilities
                               }
require'lspconfig'.pyright.setup{ on_attach = on_attach, capabilities = capabilities }
require'lspconfig'.rust_analyzer.setup{ on_attach = on_attach
                                      , single_file_support = true
                                      , capabilities = capabilities
                                      }

require'lspconfig'.hls.setup{ on_attach = on_attach
                            , single_file_support = true
                            , capabilities = capabilities
                            }

require'lspconfig'.ocamllsp.setup{ on_attach = on_attach
                                 , single_file_support = true
                                 , capabilities = capabilities
                                 }
require'lspconfig'.tsserver.setup { on_attach = on_attach
                                  , single_file_support = true
                                  , capabilities = capabilities
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
    },
    capabilities = capabilities
})

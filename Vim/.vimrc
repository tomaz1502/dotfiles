" VIMRC FILE
" GitHub : https://github.com/tomaz1502/dotfiles/blob/master/.vimrc

"Plugged {{{
call plug#begin('~/.vim/autoload/plugged')
Plug 'junegunn/fzf', { 'do': { -> fzf#install() } }
Plug 'junegunn/fzf.vim'

Plug 'chriskempson/base16-vim'

Plug 'neovimhaskell/haskell-vim'
Plug 'monkoose/fzf-hoogle.vim'

Plug 'tpope/vim-commentary'

Plug 'jremmen/vim-ripgrep'

Plug 'xuhdev/vim-latex-live-preview', { 'for': 'tex' }
Plug 'justinmk/vim-dirvish'

Plug 'folke/zen-mode.nvim'

Plug 'Julian/lean.nvim'
Plug 'neovim/nvim-lspconfig'
Plug 'nvim-lua/plenary.nvim'

Plug 'hrsh7th/nvim-compe'  " For LSP completion
Plug 'hrsh7th/vim-vsnip'   " For snippets
Plug 'andrewradev/switch.vim'  " For Lean switch support
call plug#end()
"}}}

"Fundamentals {{{      
colorscheme base16-default-dark
filetype plugin on
filetype plugin indent on
set number
set mouse=a
set tabstop=4
set expandtab
set shiftwidth=4
set textwidth=80
set cursorline
set clipboard+=unnamedplus
set hidden
set wildmenu
set signcolumn=no
set foldmethod=marker
set termguicolors
set nobackup
set nowritebackup
set noswapfile
syntax on
let g:colorcolumn=join(range(80,256), ',')
let mapleader=","
let maplocalleader=","
highlight Comment gui=Italic

set inccommand=nosplit
"}}}

" All Maps {{{
map <F7> :<C-U>!./%:r
inoremap {<CR> {<CR><ESC>o}<UP><ESC>a
inoremap ( ()<left>
inoremap [ []<left>
inoremap " ""<left>
xnoremap ( xi()<Esc>P
nnoremap <Up> <Nop>
nnoremap <Left> <Nop>
nnoremap <Right> <Nop>
nnoremap <Down> <Nop>
nnoremap <C-h> <C-w>h
nnoremap <C-j> <C-w>j
nnoremap <C-k> <C-w>k
nnoremap <C-l> <C-w>l
nnoremap Y y$

nnoremap <silent> <Leader>ev :e ~/Desktop/dotfiles/Vim/.vimrc<CR>
nnoremap <Leader>es :source ~/Desktop/dotfiles/Vim/.vimrc<CR>

map <silent> <Leader>t :Files<CR>
map <silent> <Leader>b :Buffers<CR>

nnoremap <Tab> za

inoremap jk <Esc>
nnoremap <silent> <Leader>w :w<CR>
nnoremap <silent> <Leader>q :q<CR>
nnoremap <silent> <Leader>x :wq<CR>

" Press Space to turn off highlighting and clear any message already displayed.
nnoremap <silent> <Space> :nohlsearch<Bar>:echo<CR>

" very magic mode
nnoremap / /\v
"}}}

" Plugins Config {{{
" Haskell Indent {{{
let g:haskell_indent_if = 4
let g:haskell_indent_case = 4
let g:haskell_indent_let = 4
let g:haskell_indent_where = 4
let g:haskell_indent_before_where = 4
let g:haskell_indent_after_bare_where = 4
let g:haskell_indent_do = 4
let g:haskell_indent_in = 4
let g:haskell_indent_guard = 4
let g:haskell_indent_case_alternative = 4
" }}}

" ZenMode {{{
lua << EOF
  require("zen-mode").setup {
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
EOF
" }}}
" }}}

" Status Line {{{ 
highlight SL1 gui=Bold guifg=#b8b8b8 guibg=#282828
highlight SL2 guifg=#b8b8b8 guibg=#282828
highlight SL3 guifg=#282828 guibg=Gray

highlight IM gui=Bold guifg=White guibg=16
highlight SAVED guibg=Red
highlight MODIFIED guibg=Gold

highlight ARROWSAVED guibg=#282828 guifg=Red
highlight ARROWMOD guibg=#282828 guifg=Gold
highlight ARROWRIGHT guibg=#282828 guifg=Gray

function! s:status_info()
    setlocal statusline+=%#SL1#
    setlocal statusline+=\ \ %{expand('%:p:h:t')}/%t
    setlocal statusline+=%#SL2#
    " setlocal statusline+=\ \ [%{gitbranch#name()}]
    setlocal statusline+=%=%y

    setlocal statusline+=\ %#ARROWRIGHT#
    setlocal statusline+=
    setlocal statusline+=%#SL3#\ ℓ\ %l/%L\ \ 𝐜\ %c/%{&columns}\ 
endfunction

function! s:status_saved()
    setlocal statusline=%#SAVED#
    setlocal statusline+=\ \ \ \ 
    setlocal statusline+=%#ARROWSAVED#
    setlocal statusline+=
    call s:status_info()
endfunction

function! s:status_modified()
    if &modified
        setlocal statusline=%#MODIFIED#
        setlocal statusline+=\ \ \ \ 
        setlocal statusline+=%#ARROWMOD#
        setlocal statusline+=
        call s:status_info()
    endif
endfunction

function! s:focus_window() abort
  let &colorcolumn=join(range(80, 256), ',')
  if &modified
      call s:status_modified()
  else
      call s:status_saved()
  endif
endfunction

function! s:blur_window() abort
  let &colorcolumn=join(range(1,256), ',')
  setlocal statusline=%#SL2#
  setlocal statusline+=\ \ \ \ \ \ %f
endfunction
" }}} 

" Autocmds {{{ 
autocmd InsertEnter * echohl IM | echo "  -- Insert Mode --" | echohl None
autocmd InsertLeave * echo ""

autocmd TextChanged,TextChangedI,TextChangedP,InsertChange * call s:status_modified()
autocmd WinNew,TabNew,BufNew,BufRead,BufWrite * call s:status_saved()

autocmd BufEnter,FocusGained,VimEnter,WinEnter * call s:focus_window()
autocmd FocusLost,WinLeave * call s:blur_window()

autocmd Filetype tex setl updatetime=999999
autocmd TextYankPost * silent! lua require'vim.highlight'.on_yank { higroup="Substitute", timeout=200 }

autocmd Filetype cpp set tabstop=2 | set shiftwidth=2

" re-run commands from q:
autocmd CmdwinEnter * map <buffer> <F5> <CR>q:
" }}}

" LSP Config {{{
set pumheight=6
set completeopt=menuone,noselect
let g:compe = {}
let g:compe.enabled = v:true
let g:compe.autocomplete = v:true
let g:compe.debug = v:false
let g:compe.min_length = 1
let g:compe.preselect = 'enable'
let g:compe.documentation = v:true

let g:compe.source = {}
let g:compe.source.path = v:true
let g:compe.source.buffer = v:true
let g:compe.source.nvim_lsp = v:true

lua << EOF
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
  elseif vim.fn['vsnip#available'](1) == 1 then
    return t "<Plug>(vsnip-expand-or-jump)"
  elseif check_back_space() then
    return t "<Tab>"
  else
    return vim.fn['compe#complete']()
  end
end
_G.s_tab_complete = function()
  if vim.fn.pumvisible() == 1 then
    return t "<C-p>"
  elseif vim.fn['vsnip#jumpable'](-1) == 1 then
    return t "<Plug>(vsnip-jump-prev)"
  else
    return t "<S-Tab>"
  end
end

vim.api.nvim_set_keymap("i", "<Tab>", "v:lua.tab_complete()", {expr = true})
vim.api.nvim_set_keymap("s", "<Tab>", "v:lua.tab_complete()", {expr = true})
vim.api.nvim_set_keymap("i", "<S-Tab>", "v:lua.s_tab_complete()", {expr = true})
vim.api.nvim_set_keymap("s", "<S-Tab>", "v:lua.s_tab_complete()", {expr = true})

local on_attach = function(client, bufnr)
  local function buf_set_keymap(...) vim.api.nvim_buf_set_keymap(bufnr, ...) end
  local function buf_set_option(...) vim.api.nvim_buf_set_option(bufnr, ...) end

  buf_set_option('omnifunc', 'v:lua.vim.lsp.omnifunc')

  local opts = { noremap=true, silent=true }

  buf_set_keymap('n', 'gd',         '<cmd>lua vim.lsp.buf.definition()<CR>', opts)
  buf_set_keymap('n', 'K',          '<cmd>lua vim.lsp.buf.hover()<CR>', opts)
  buf_set_keymap('n', 'gi',         '<cmd>lua vim.lsp.buf.implementation()<CR>', opts)
  buf_set_keymap('n', '<C-k>',      '<cmd>lua vim.lsp.buf.signature_help()<CR>', opts)
  buf_set_keymap('n', '<leader>rn', '<cmd>lua vim.lsp.buf.rename()<CR>', opts)
  buf_set_keymap('n', '<leader>A',  '<cmd>lua vim.lsp.buf.code_action()<CR>', opts)
  buf_set_keymap('n', 'gr',         '<cmd>lua vim.lsp.buf.references()<CR>', opts)
  buf_set_keymap('n', '[d',         '<cmd>lua vim.lsp.diagnostic.goto_prev()<CR>', opts)
  buf_set_keymap('n', ']d',         '<cmd>lua vim.lsp.diagnostic.goto_next()<CR>', opts)
  buf_set_keymap('n', '<leader>d',  '<cmd>lua vim.lsp.diagnostic.set_loclist()<CR>', opts)
end

local nvim_lsp = require('lspconfig')

require'lspconfig'.clangd.setup{ on_attach = on_attach,
                                 init_options = { fallbackFlags = { "-std=c++17" } }
                               }
require'lspconfig'.hls.setup{ on_attach = on_attach }
require'lspconfig'.rust_analyzer.setup{ on_attach = on_attach }

require('lean').setup {
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
}
EOF
" }}}

au VimLeave * set guicursor=a:ver1-blinkoff0

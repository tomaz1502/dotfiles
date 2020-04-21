" VIMRC FILE
" GitHub : https://github.com/tomaz1502/dotfiles/blob/master/.vimrc

"Plugins {{{
call plug#begin()

Plug 'prabirshrestha/async.vim'
Plug 'prabirshrestha/vim-lsp'
Plug 'mattn/vim-lsp-settings'
Plug 'ncm2/ncm2'
Plug 'roxma/nvim-yarp'
Plug 'ncm2/ncm2-vim-lsp'
Plug 'scrooloose/nerdtree'

Plug 'wincent/command-t', {
 \    'do' : 'cd ruby/command-t/ext/command-t && ruby extconf.rb && make'
 \ }
Plug 'chriskempson/base16-vim'

call plug#end()
"}}}

"Fundamentals {{{
"colorscheme abstract
"colorscheme base16-dracula
"colorscheme base16-gruvbox-dark-hard
colorscheme base16-atelier-forest
filetype plugin on

set nu
set rnu
set mouse=a
set cindent
set tabstop=4
set expandtab
set shiftwidth=4
let mapleader=","
syntax on

set hidden
set wildmenu
set signcolumn=no
set foldmethod=marker
set termguicolors
""set wildignore+=**/node_modules/**
""set path+=**
"}}}

" All Maps {{{
map <F7> :<C-U>!./%:r
inoremap {<CR> {<CR><ESC>o}<UP><ESC>$a
inoremap ( ()<left>
inoremap [ []<left>
inoremap " ""<left>
inoremap ' ''<left>
nnoremap <C-S-Down> ddp
nnoremap <C-S-Up> <Up>ddp<Up>
xnoremap ( xi()<Esc>P
nnoremap <Up> <Nop>
nnoremap <Left> <Nop>
nnoremap <Right> <Nop>
nnoremap <Down> <Nop>
nnoremap <C-h> <C-w>h
nnoremap <C-j> <C-w>j
nnoremap <C-k> <C-w>k
nnoremap <C-l> <C-w>l

nnoremap <Leader>c :set cursorline!<CR>
nnoremap <Leader>ev :vsp ~/.vimrc<CR>
nnoremap <Leader>es :source ~/.vimrc<CR>

nnoremap <Leader>s :let g:lsp_diagnostics_enabled = 1<CR>
nnoremap <Leader>S :let g:lsp_diagnostics_enabled = 0<CR>

nnoremap <C-d> :LspDefinition<CR>
nnoremap <C-f> :LspHover<CR>
nnoremap <C-r> :LspReferences<CR>

map <C-t> :NERDTreeToggle<CR>
map <Leader>t :CommandT ~/Desktop/Tom/<CR>
map <Leader>T :CommandTHelp<CR>

nnoremap <Tab> za
"}}}

"LSP Stuff {{{
if executable('clangd-9')
    au User lsp_setup call lsp#register_server({
        \ 'name': 'clangd-9',
        \ 'cmd': {server_info->['clangd-9']},
        \ 'whitelist': ['cpp'],
        \ })
endif

if executable('pyls')
    au User lsp_setup call lsp#register_server({
        \ 'name': 'pyls',
        \ 'cmd': {server_info->['pyls']},
        \ 'whitelist': ['python'],
        \ })
endif

if executable('rls')
    au User lsp_setup call lsp#register_server({
        \ 'name' : 'rls',
        \ 'cmd' : {server_info->['rls']},
        \ 'whitelist' : ['rust'],
        \})
endif

augroup lsp_install
    au!
    " call s:on_lsp_buffer_enabled only for languages that has the server registered.
    autocmd User lsp_buffer_enabled call s:on_lsp_buffer_enabled()
augroup END
"}}}

"Auto Complete (NCM2) {{{
function! s:on_lsp_buffer_enabled() abort
    setlocal omnifunc=lsp#complete
endfunction
inoremap <expr> <CR> (pumvisible() ? "\<c-y>\<cr>" : "\<CR>")
let g:ncm2#auto_popup = 1

" Use <TAB> to select the popup menu:
inoremap <expr> <Tab> pumvisible() ? "\<C-n>" : "\<Tab>"
inoremap <expr> <S-Tab> pumvisible() ? "\<C-p>" : "\<S-Tab>"

autocmd BufEnter * call ncm2#enable_for_buffer()
set completeopt=noinsert,menuone,noselect
set shortmess+=c
let g:ncm2#complete_length = 3
""}}}

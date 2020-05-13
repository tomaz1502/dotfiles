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

Plug 'wincent/ferret'
Plug 'wincent/command-t', {
 \    'do' : 'cd ruby/command-t/ext/command-t && ruby extconf.rb && make'
 \ }
Plug 'chriskempson/base16-vim'

Plug 'xolox/vim-notes'
Plug 'xolox/vim-misc'

Plug 'itchyny/lightline.vim'
Plug 'itchyny/vim-gitbranch'
Plug 'dense-analysis/ale'

Plug 'junegunn/goyo.vim'

call plug#end()
"}}}

"Fundamentals {{{      
"colorscheme abstract
"colorscheme base16-dracula
"colorscheme base16-gruvbox-dark-hard
"colorscheme base16-atelier-forest
colorscheme base16-default-dark
filetype plugin on

set number
set relativenumber
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

highlight Search guibg='NONE' guifg='NONE'
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
nnoremap <Leader>ev :vsp ~/Tom/dotfiles/.vimrc<CR>
nnoremap <Leader>es :source ~/Tom/dotfiles/.vimrc<CR>

nnoremap gd :LspDefinition<CR>
nnoremap K :LspHover<CR>
nnoremap gr :LspReferences<CR>

map <C-t> :NERDTreeToggle<CR>
map <Leader>T :CommandTHelp<CR>

nnoremap <Tab> za
"}}}

"LSP Stuff {{{

"if executable('clangd-9')
"    au User lsp_setup call lsp#register_server({
"        \ 'name': 'clangd-9',
"        \ 'cmd': {server_info->['clangd-9']},
"        \ 'whitelist': ['cpp'],
"        \ })
"endif

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

let g:lsp_diagnostics_enabled=0
"}}}

"Auto Complete (NCM2) {{{
function! s:on_lsp_buffer_enabled() abort
    setlocal omnifunc=lsp#complete
endfunction
inoremap <expr> <CR> (pumvisible() ? "\<c-y>\<cr>" : "\<CR>")
let g:ncm2#auto_popup = 1
let g:ncm2#popup_limit = 5
"let g:ncm2#popup_delay = 1300 
let g:ncm2#complete_length = 3

" Use <TAB> to select the popup menu:
inoremap <expr> <Tab> pumvisible() ? "\<C-n>" : "\<Tab>"
inoremap <expr> <S-Tab> pumvisible() ? "\<C-p>" : "\<S-Tab>"

autocmd BufEnter * call ncm2#enable_for_buffer()
set completeopt=noinsert,menuone,noselect
set shortmess+=c
au TextChangedI * call ncm2#auto_trigger()
"}}}

"Light Line Options {{{

let g:lightline = {
      \ 'active': {
      \   'left': [ [ 'mode', 'paste' ],
      \             [ 'gitbranch', 'readonly', 'filename', 'modified' ] ]
      \ },
      \ 'component_function': {
      \   'gitbranch': 'gitbranch#name'
      \ },
      \ }

set noshowmode "because of lightline
"}}}

" ALE Options {{{

let g:ale_open_list=0
let g:ale_lint_on_text_changed=1
let g:ale_lint_on_insert_leave=0
let g:ale_lint_on_save=0
let g:ale_lint_delay=1000
let g:ale_virtualtext_cursor=1
let g:ale_virtualtext_prefix="      >>> "
highlight ALEVirtualTextError guifg=DarkRed

" }}} 

" Goyo (Hide tmux status bar) {{{

function! s:goyo_enter()
    if exists('$TMUX')
        silent !tmux set status off
    endif
endfunction

function! s:goyo_leave()
    if exists('$TMUX')
        silent !tmux set status on
    endif
endfunction

autocmd! User GoyoEnter nested call <SID>goyo_enter()
autocmd! User GoyoLeave nested call <SID>goyo_leave()

"}}}

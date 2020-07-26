" VIMRC FILE
" GitHub : https://github.com/tomaz1502/dotfiles/blob/master/.vimrc

"Plugins {{{
call plug#begin()

Plug 'junegunn/fzf', { 'do': { -> fzf#install() } }
Plug 'junegunn/fzf.vim'

Plug 'chriskempson/base16-vim'

Plug 'xolox/vim-notes'
Plug 'xolox/vim-misc'

Plug 'itchyny/lightline.vim'
Plug 'itchyny/vim-gitbranch'

Plug 'neovimhaskell/haskell-vim'
Plug 'junegunn/goyo.vim'
Plug 'tpope/vim-commentary'

Plug 'autozimu/LanguageClient-neovim', {
    \ 'branch': 'next',
    \ 'do': 'bash install.sh',
    \ }
Plug 'neoclide/coc.nvim', {'branch': 'release'}
Plug 'jremmen/vim-ripgrep'
call plug#end()
"}}}

"Fundamentals {{{      
"colorscheme base16-gruvbox-dark-hard
"colorscheme base16-atelier-forest
colorscheme base16-default-dark
filetype plugin on
filetype plugin indent on

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

set nobackup
set nowritebackup
set noswapfile

autocmd CmdwinEnter * map <buffer> <F5> <CR>q:
highlight Search guibg='NONE' guifg='NONE'

if exists('##TextYankPost')
    autocmd TextYankPost * silent! lua require'vim.highlight'.on_yank('Substitute', 200)
endif
"}}}

" All Maps {{{
map <F7> :<C-U>!./%:r
inoremap {<CR> {<CR><ESC>o}<UP><ESC>a
inoremap ( ()<left>
inoremap [ []<left>
inoremap " ""<left>
inoremap ' ''<left>
xnoremap ( xi()<Esc>P
nnoremap <Up> <Nop>
nnoremap <Left> <Nop>
nnoremap <Right> <Nop>
nnoremap <Down> <Nop>
nnoremap <C-h> <C-w>h
nnoremap <C-j> <C-w>j
nnoremap <C-k> <C-w>k
nnoremap <C-l> <C-w>l

nnoremap <silent> <Leader>ev :vsp ~/Desktop/Tom/Stuff/dotfiles/.vimrc<CR>
nnoremap <Leader>es :source ~/Desktop/Tom/Stuff/dotfiles/.vimrc<CR>

map <silent> <Leader>t :Files<CR>
map <silent> <Leader>b :Buffers<CR>

nnoremap <Tab> za

inoremap jk <Esc>
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

let g:goyo_width=130
"}}}

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

" COC {{{

"Tab auto complete
function! s:check_back_space() abort
  let col = col('.') - 1
  return !col || getline('.')[col - 1]  =~ '\s'
endfunction

inoremap <silent><expr> <TAB>
      \ pumvisible() ? "\<C-n>" :
      \ <SID>check_back_space() ? "\<TAB>" :
      \ coc#refresh()

"Maps

nmap <silent> gd <Plug>(coc-definition)
nmap <silent> gr <Plug>(coc-references)
nmap <silent> K :call <SID>show_documentation()<CR>
nmap <leader>rn <Plug>(coc-rename)

function! s:show_documentation()
  if (index(['vim','help'], &filetype) >= 0)
    execute 'h '.expand('<cword>')
  else
    call CocAction('doHover')
  endif
endfunction

nmap <silent> <Leader>cc :CocConfig<CR>
nmap <silent> <Leader>sc :CocList diagnostics<CR>
xmap <silent> <leader>f  <Plug>(coc-format-selected)
command! -nargs=0 Format :call CocAction('format')
"}}}

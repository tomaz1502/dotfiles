" VIMRC FILE
" GitHub : https://github.com/tomaz1502/dotfiles/blob/master/.vimrc

"Plugged {{{
call plug#begin()

Plug 'junegunn/fzf', { 'do': { -> fzf#install() } }
Plug 'junegunn/fzf.vim'

Plug 'chriskempson/base16-vim'

" Plug 'xolox/vim-notes'
" Plug 'xolox/vim-misc'

Plug 'itchyny/vim-gitbranch'

Plug 'neovimhaskell/haskell-vim'
Plug 'junegunn/goyo.vim'
Plug 'tpope/vim-commentary'

Plug 'neoclide/coc.nvim', {'branch': 'release'}
Plug 'jremmen/vim-ripgrep'

" Maybe?
" Plug 'bfrg/vim-cpp-modern'
" Plug 'honza/vim-snippets'

Plug 'walkie/twelf-vim'
Plug 'xuhdev/vim-latex-live-preview', { 'for': 'tex' }
Plug 'altercation/vim-colors-solarized'
call plug#end()
"}}}

"Fundamentals {{{      

" syntax enable
" set background=light
" colorscheme solarized
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

highlight Comment gui=Italic
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
nnoremap Y y$

nnoremap <silent> <Leader>ev :vsp ~/Tom/dotfiles/.vimrc<CR>
nnoremap <Leader>es :source ~/Tom/dotfiles/.vimrc<CR>

map <silent> <Leader>t :Files<CR>
map <silent> <Leader>b :Buffers<CR>

" re-run commands from q:
autocmd CmdwinEnter * map <buffer> <F5> <CR>q:
nnoremap <Tab> za

inoremap jk <Esc>
nnoremap <silent> <Leader>w :w<CR>

" Press Space to turn off highlighting and clear any message already displayed.
nnoremap <silent> <Space> :nohlsearch<Bar>:echo<CR>

" very magic mode
nnoremap / /\v
" vnoremap / /\v
"}}}

" Plugins Config {{{

"Light Line {{{
let g:lightline = {
      \ 'active': {
      \   'left': [ [ 'mode', 'paste' ],
      \             [ 'gitbranch', 'readonly', 'filename', 'modified' ] ]
      \ },
      \ 'component_function': {
      \   'gitbranch': 'gitbranch#name'
      \ },
      \ 'colorscheme': 'powerlineish'
      \ }

set noshowmode
"}}}

" Goyo {{{
" Hide tmux status bar
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


inoremap <silent><expr> <TAB>
  \ pumvisible() ? coc#_select_confirm() :
  \ coc#expandableOrJumpable() ? "\<C-r>=coc#rpc#request('doKeymap', ['snippets-expand-jump',''])\<CR>" :
  \ <SID>check_back_space() ? "\<TAB>" :
  \ coc#refresh()

function! s:check_back_space() abort
  let col = col('.') - 1
  return !col || getline('.')[col - 1]  =~# '\s'
endfunction

let g:coc_snippet_next = '<tab>'


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

highlight CocErrorVirtualText guifg=#ff422b gui=Italic
highlight CocErrorSign guifg=#ff422b
highlight CocWarningVirtualText guifg=#fab005 gui=Italic
highlight CocInfoSign guifg=#fab005
"}}}

" {{{ Tex 
autocmd Filetype tex setl updatetime=999999
" }}}

" }}}



"Status Line

highlight SL1 gui=Bold guifg=#b8b8b8 guibg=#282828
highlight SL2 gui=Italic guifg=#b8b8b8 guibg=#282828
highlight SL3 guifg=#b8b8b8 guibg=#282828

highlight IM gui=Bold guifg=Gold guibg=16
highlight MODIFIED guibg=#00FF00
highlight SAVED guibg=#FF0000

function! s:status_info()
    set statusline+=\ \ \ \ %#SL1#
    set statusline+=\ \ %f
    set statusline+=%#SL2#
    set statusline+=\ \ [%{gitbranch#name()}]
    set statusline+=%#SL3#
    set statusline+=%=%y\ \ ℓ\ %l/%L\ \ 𝐜\ %c/%{virtcol('$')}\ 
endfunction

function! s:status_saved()
    set statusline=%#SAVED#
    call s:status_info()
endfunction

function! s:status_modified()
    set statusline=%#MODIFIED#
    call s:status_info()
endfunction

autocmd InsertEnter * echohl IM | echo "  -- Insert Mode --" | echohl None
autocmd InsertLeave * echo ""
autocmd TextChanged,TextChangedI,TextChangedP,InsertChange * call s:status_modified()
autocmd BufWrite * call s:status_saved()

call s:status_saved()


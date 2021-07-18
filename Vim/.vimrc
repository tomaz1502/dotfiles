" VIMRC FILE
" GitHub : https://github.com/tomaz1502/dotfiles/blob/master/.vimrc

"Plugged {{{
call plug#begin('~/.vim/autoload/plugged')
Plug 'junegunn/fzf', { 'do': { -> fzf#install() } }
Plug 'junegunn/fzf.vim'

Plug 'chriskempson/base16-vim'

Plug 'itchyny/vim-gitbranch'

Plug 'neovimhaskell/haskell-vim'
Plug 'monkoose/fzf-hoogle.vim'

Plug 'tpope/vim-commentary'

Plug 'neoclide/coc.nvim', {'branch': 'master', 'do': 'yarn install --frozen-lockfile'}
Plug 'jremmen/vim-ripgrep'

Plug 'xuhdev/vim-latex-live-preview', { 'for': 'tex' }
Plug 'justinmk/vim-dirvish'

Plug 'honza/vim-snippets'
Plug 'folke/zen-mode.nvim'
call plug#end()
"}}}

"Fundamentals {{{      
" color column

" syntax enable
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
set noshowmode
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
highlight Comment gui=Italic

""set wildignore+=**/node_modules/**
""set path+=**

if has("nvim")
    set inccommand=nosplit
endif

if exists('##TextYankPost')
    autocmd TextYankPost * silent! lua require'vim.highlight'.on_yank { higroup="Substitute", timeout=200 }
endif
"}}}

" All Maps {{{
map <F7> :<C-U>!./%:r
inoremap {<CR> {<CR><ESC>o}<UP><ESC>a
" inoremap { {}<left>
inoremap ( ()<left>
inoremap [ []<left>
inoremap " ""<left>
" inoremap ' ''<left>
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

nnoremap <silent> <Leader>ev :vsp ~/Desktop/Tom/dotfiles/Vim/.vimrc<CR>
nnoremap <Leader>es :source ~/Desktop/Tom/dotfiles/Vim/.vimrc<CR>

map <silent> <Leader>t :Files<CR>
map <silent> <Leader>b :Buffers<CR>

" re-run commands from q:
autocmd CmdwinEnter * map <buffer> <F5> <CR>q:
nnoremap <Tab> za

inoremap jk <Esc>
nnoremap <silent> <Leader>w :w<CR>
nnoremap <silent> <Leader>q :q<CR>
nnoremap <silent> <Leader>x :wq<CR>

" Press Space to turn off highlighting and clear any message already displayed.
nnoremap <silent> <Space> :nohlsearch<Bar>:echo<CR>

" very magic mode
nnoremap / /\v
" vnoremap / /\v

" nnoremap <CR> *
" nnoremap <Backspace> #

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

" let g:coc_snippet_next = '<tab>'

inoremap <silent><expr> <TAB>
      \ pumvisible() ? "\<C-n>" :
      \ <SID>check_back_space() ? "\<TAB>" :
      \ coc#refresh()

" Use <C-l> for trigger snippet expand.
imap <C-l> <Plug>(coc-snippets-expand)

" " Use <C-j> for select text for visual placeholder of snippet.
" vmap <C-j> <Plug>(coc-snippets-select)

" " Use <C-j> for jump to next placeholder, it's default of coc.nvim
" let g:coc_snippet_next = '<c-j>'

" " Use <C-k> for jump to previous placeholder, it's default of coc.nvim
" let g:coc_snippet_prev = '<c-k>'

" " Use <C-j> for both expand and jump (make expand higher priority.)
" imap <C-j> <Plug>(coc-snippets-expand-jump)

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
nmap <silent> <Leader>A :CocAction<CR>
xmap <silent> <leader>f  <Plug>(coc-format-selected)
command! -nargs=0 Format :call CocAction('format')

highlight CocErrorVirtualText guifg=#ff422b gui=Italic
highlight CocErrorSign guifg=#ff422b
highlight CocWarningVirtualText guifg=#fab005 gui=Italic
highlight CocInfoSign guifg=#fab005
"}}}

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
highlight SL3 guifg=Black guibg=Gray

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
  " if exists('w:matches')
  "   for l:match in w:matches
  "     call matchdelete(l:match)
  "   endfor
  "   let w:matches=[]
  " endif
  let &colorcolumn=join(range(80, 256), ',')
  if &modified
      call s:status_modified()
  else
      call s:status_saved()
  endif
endfunction

function! s:blur_window() abort
  " if !exists('w:matches')
  "   " Instead of unconditionally resetting, append to existing array.
  "   " This allows us to gracefully handle duplicate autocmds.
  "   let w:matches=[]
  " endif
  " let l:start=max([1, line('w0') - 20])
  " let l:end=min([line('$'), line('w$') + 20])
  " while l:start <= l:end
  "   let l:next=l:start + 8
  "   let l:id=matchaddpos(
  "         \   'SL2',
  "         \   range(l:start, min([l:end, l:next])),
  "         \   1000
  "         \ )
  "   call add(w:matches, l:id)
  "   let l:start=l:next
  " endwhile
  let &colorcolumn=join(range(1,256), ',')
  setlocal statusline=%#SL2#
  setlocal statusline+=\ \ \ \ 
  setlocal statusline+=\ \ %f
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
" }}}

" let g:coc_start_at_startup = v:false
au VimLeave * set guicursor=a:ver1-blinkoff0

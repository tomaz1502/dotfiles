" VIMRC FILE
" GitHub : https://github.com/tomaz1502/dotfiles/blob/master/.vimrc

"Plugged {{{
call plug#begin('~/.vim/autoload/plugged')

Plug 'machakann/vim-highlightedyank'

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
" Plug 'honza/vim-snippets'

Plug 'xuhdev/vim-latex-live-preview', { 'for': 'tex' }
Plug 'justinmk/vim-dirvish'

call plug#end()
"}}}

"Fundamentals {{{      

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
set colorcolumn=80
set textwidth=80
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
" if exists('##TextYankPost')
"     autocmd TextYankPost * silent! lua require'vim.highlight'.on_yank('Substitute', 200)
" endif

if !exists('##TextYankPost')
  map y <Plug>(highlightedyank)
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

nnoremap <silent> <Leader>ev :vsp ~/Desktop/Tom/dotfiles/.vimrc<CR>
nnoremap <Leader>es :source ~/Desktop/Tom/dotfiles/.vimrc<CR>

map <silent> <Leader>t :Files<CR>
map <silent> <Leader>b :Buffers<CR>
map <silent> <Leader>r :RnvimrToggle<CR>

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

" Goyo {{{
" Hide tmux status bar
let g:in_goyo=0

function! s:goyo_enter()
    if exists('$TMUX')
        silent !tmux set status off
    endif
    let g:in_goyo=1
    set laststatus=0
endfunction

function! s:goyo_leave()
    if exists('$TMUX')
        silent !tmux set status on
    endif
    let g:in_goyo=0
    set laststatus=2
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
nmap <silent> <Leader>A :CocAction<CR>
xmap <silent> <leader>f  <Plug>(coc-format-selected)
command! -nargs=0 Format :call CocAction('format')

highlight CocErrorVirtualText guifg=#ff422b gui=Italic
highlight CocErrorSign guifg=#ff422b
highlight CocWarningVirtualText guifg=#fab005 gui=Italic
highlight CocInfoSign guifg=#fab005
"}}}

" Yank Highlight {{{ 

highlight HighlightedyankRegion ctermfg=10 ctermbg=3 guifg=#282828 guibg=#f7ca88
let g:highlightedyank_highlight_duration = 200

"}}}

" }}}

" Status Line {{{ 

highlight SL1 gui=Bold guifg=#b8b8b8 guibg=#282828
highlight SL2 guifg=#b8b8b8 guibg=#282828
highlight SL3 guifg=Black guibg=Gray

highlight IM gui=Bold guifg=White guibg=16
highlight SAVED guibg=Red
highlight MODIFIED guibg=Gold

function! s:status_info()
    setlocal statusline+=\ \ \ \ %#SL1#
    setlocal statusline+=\ \ %{expand('%:p:h:t')}/%t
    setlocal statusline+=%#SL2#
    setlocal statusline+=\ \ [%{gitbranch#name()}]
    setlocal statusline+=%=%y\ %#SL3#\ ℓ\ %l/%L\ \ 𝐜\ %c/%{strlen(join([getline('.'),'']))}\ 
endfunction

function! s:status_saved()
    if !g:in_goyo
        setlocal statusline=%#SAVED#
        call s:status_info()
    endif
endfunction

function! s:status_modified()
    if !g:in_goyo && &modified
        setlocal statusline=%#MODIFIED#
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
  let &colorcolumn="80"
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
  if !g:in_goyo
      setlocal statusline=%#SL2#
      setlocal statusline+=\ \ \ \ 
      setlocal statusline+=\ \ %f
  endif
endfunction
" }}} 

" Autocmds {{{ 

autocmd InsertEnter * echohl IM | echo "  -- Insert Mode --" | echohl None
autocmd InsertLeave * echo ""

autocmd TextChanged,TextChangedI,TextChangedP,InsertChange * call s:status_modified()
autocmd BufWrite * call s:status_saved()
autocmd WinNew,TabNew,BufNew,BufRead * call s:status_saved()

autocmd BufEnter,FocusGained,VimEnter,WinEnter * call s:focus_window()
autocmd FocusLost,WinLeave * call s:blur_window()

autocmd Filetype tex setl updatetime=999999
autocmd VimEnter {} :Files

" }}}

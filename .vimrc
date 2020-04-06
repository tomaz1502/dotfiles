"basic
colorscheme OceanicNext
filetype plugin on
set rnu
set nu
set mouse=a
set cindent
set tabstop=4
set expandtab
set shiftwidth=4
let mapleader=","
syntax on


set wildmenu
set path+=**
set hidden
set wildignore+=**/node_modules/**

nnoremap <Leader>ev :vsp $MYVIMRC<CR>

"status bar
":set laststatus=2

"Set cursor as a line
""if has("autocmd")
""  au VimEnter,InsertLeave * silent execute '!echo -ne "\e[2 q"' | redraw!
""  au InsertEnter,InsertChange *
""\ if v:insertmode == 'i' | 
""\   silent execute '!echo -ne "\e[6 q"' | redraw! |
""\ elseif v:insertmode == 'r' |
""\   silent execute '!echo -ne "\e[4 q"' | redraw! |
""\ endif
""au VimLeave * silent execute '!echo -ne "\e[ q"' | redraw!
""endif

"highlight current line
:nnoremap <Leader>c :set cursorline!<CR>

"One key make and run
autocmd FileType cpp map <F5> :<C-U>!make %:r && ./%:r
"autocmd FileType python map <F5> :<C-U>!python3 %:r.py
"autocmd FileType rust map <F5> :<C-U>!rustc %:r.rs && ./%:r
"autocmd FileType haskell map <F5> :<C-U>!ghc %:r.hs && ./%:r

autocmd FileType cpp map <F6> :<C-U>!make %:r
"autocmd FileType rust map <F6> :<C-U>!rustc %:r.rs
"autocmd FileType haskell map <F6> :<C-U>!ghc %:r.hs

map <F7> :<C-U>!./%:r

"nice maps
inoremap {<CR> {<CR><ESC>o}<UP><ESC>$a
inoremap ( ()<left>
inoremap [ []<left>
inoremap " ""<left>
inoremap ' ''<left>
nnoremap <C-Down> ddp
nnoremap <C-Up> <Up>ddp<Up>
xnoremap ( xi()<Esc>P

"Plugin Instalation
call plug#begin('~/.vim/plugged')

"Plug 'autozimu/LanguageClient-neovim', {
 "   \ 'branch': 'next',
  "  \ 'do': 'bash install.sh',
   " \ }

"Plug 'prabirshrestha/async.vim'
"Plug 'prabirshrestha/vim-lsp'
"Plug 'mattn/vim-lsp-settings'

call plug#end()

"set hidden

"let g:LanguageClient_serverCommands = {
 "   \ 'rust': ['~/.cargo/bin/rustup', 'run', 'stable', 'rls'],
  "  \ 'cpp' : ['clangd']
   " \ }

"nnoremap <F2> :call LanguageClient_contextMenu()<CR>

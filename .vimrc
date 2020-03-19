:colorscheme OceanicNext

:filetype plugin on
:set nu
:set mouse=a
:set cindent
:set tabstop=4
:set expandtab
:set shiftwidth=4
:syntax on

"autocmd FileType cpp map <F5> :<C-U>!make %:r && ./%:r
"autocmd FileType python map <F5> :<C-U>!python3 %:r.py
"autocmd FileType rust map <F5> :<C-U>!rustc %:r.rs && ./%:r
"autocmd FileType haskell map <F5> :<C-U>!ghc %:r.hs && ./%:r

"autocmd FileType cpp map <F6> :<C-U>!make %:r
"autocmd FileType rust map <F6> :<C-U>!rustc %:r.rs
"autocmd FileType haskell map <F6> :<C-U>!ghc %:r.hs

map <F7> :<C-U>!./%:r
inoremap {<CR> {<CR><ESC>o}<UP><ESC>$a
inoremap ( ()<left>
inoremap [ []<left>
inoremap " ""<left>
inoremap ' ''<left>
nnoremap <C-S-Down> ddp
nnoremap <C-S-Up> <Up>ddp<Up>
xnoremap ( xi()<Esc>P

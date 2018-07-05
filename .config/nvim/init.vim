
call plug#begin('~/.local/share/nvim/plugged')

Plug 'git@github.com:chriskempson/vim-tomorrow-theme.git'
Plug 'https://github.com/tpope/vim-fugitive.git'
Plug 'airblade/vim-gitgutter'
Plug 'git@github.com:pangloss/vim-javascript.git'
Plug 'git@github.com:mxw/vim-jsx.git'
Plug 'https://github.com/rafi/awesome-vim-colorschemes'
Plug 'https://github.com/tpope/vim-surround'
Plug 'https://github.com/mbbill/undotree'
Plug 'rust-lang/rust.vim'
Plug 'vim-syntastic/syntastic'
Plug 'ctrlpvim/ctrlp.vim'
Plug 'styled-components/vim-styled-components'
Plug 'leafgarland/typescript-vim'
Plug 'idris-hackers/idris-vim'

call plug#end()

set number
set tabstop=2 softtabstop=0 expandtab shiftwidth=2 smarttab

set mouse=a

" Folding by indentation
set fdm=indent

" GitGutter git commit to compare against with diff
let g:gitgutter_diff_base = 'HEAD~1'

" Enable conceals
set conceallevel=1
" Rust conceals
let g:rust_conceal = 1
let g:rust_conceal_mod_path = 1
" Idris conceals
let g:idris_conceal = 1

" Syntastic
set statusline+=%#warningmsg#
set statusline+=%{SyntasticStatuslineFlag()}
set statusline+=%*
let g:syntastic_always_populate_loc_list = 1
let g:syntastic_auto_loc_list = 1
let g:syntastic_check_on_open = 1
let g:syntastic_check_on_wq = 0
let g:syntastic_rs_checkers = ['rustc']

if exists("g:gui_oni")
  color Lurid

  " Change dir to the programming directory
  cd ~/Prog

  " Turn off statusbar, because it is externalized
  set noshowmode
  set noruler
  set laststatus=0
  set noshowcmd

  " Show [some] invisible characters
  set list
  set listchars=trail:·

  " Javascript syntax config
  let g:javascript_conceal_function             = "🔪"
  let g:javascript_conceal_null                 = "🍩"
  let g:javascript_conceal_this                 = "🤳"
  let g:javascript_conceal_return               = "🔫"
  let g:javascript_conceal_undefined            = "🔞"
  let g:javascript_conceal_prototype            = "🌃"
  let g:javascript_conceal_super                = "💪"
  let g:javascript_conceal_arrow_function       = "👉"
  let g:javascript_conceal_noarg_arrow_function = "🌀"
  let g:javascript_conceal_underscore_arrow_function = "🎇"

  syntax keyword jsBooleanTrue true conceal cchar=👌
  syntax keyword jsBooleanFalse false conceal cchar=👎
else
  color Lurid

  " Javascript syntax config
  let g:javascript_conceal_function             = "f"
  let g:javascript_conceal_null                 = "null"
  let g:javascript_conceal_this                 = "@"
  let g:javascript_conceal_return               = "r"
  let g:javascript_conceal_undefined            = "u"
  let g:javascript_conceal_prototype            = "p"
  let g:javascript_conceal_super                = "s"
  let g:javascript_conceal_arrow_function       = "=>"
  let g:javascript_conceal_noarg_arrow_function = "()"
  let g:javascript_conceal_underscore_arrow_function = "_"
endif

" Style: this has to be after theme setting
highlight Normal ctermfg=darkgray ctermbg=white
highlight TabLine ctermfg=003 ctermbg=white
highlight TabLineSel ctermfg=black ctermbg=011
highlight StatusLine ctermfg=003 ctermbg=white
highlight Conceal ctermbg=white ctermfg=black guibg=white guifg=black


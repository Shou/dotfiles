" :help nocp
" cp means you are forced to use OLD DEPRECATED CRAP; nocp disables that
set nocp

" Line numbering
set nu

" Tabs and spacing
set tabstop=4
set shiftwidth=4
set expandtab
set softtabstop=4

" Indentation
set autoindent

" Syntax highlighting
syntax on
set t_Co=256
color molokai

" highlight search results
set hlsearch

" ignore case when searching
" set ignorecase

set list
" What a tab looks like, what trailing spaces look like, etc
set listchars=tab:»·,trail:·,extends:>,precedes:·

" Line highlighting
set cursorline

" Command line height
set cmdheight=1

" Information
set ls=2

" Soft width column
set cc=80

" Rice
set fillchars=vert:\│
set fillchars=fold:\ 
set fillchars=diff:\ 

" Key map
" Open a new tab with Ctrl+t
nmap <C-t> :tabnew<CR>
imap <C-t> <Esc>:tabnew<CR>

" No freezing while scrolling -- only for vim, gvim should never freezzzzz
set nocursorcolumn
set nocursorline
syntax sync minlines=256

" Scroll initialization gap
set so=10

" Mouse scrolling
set mouse=a
map <ScrollWheelUp> <PageUp>
map <ScrollWheelDown> <PageDown>

" What to save when you save your session with :mksession
set sessionoptions=blank,buffers,curdir,folds,globals,help,localoptions,options,resize,tabpages,winsize,winpos

" Spell checking
set spell

if &term =~ '^screen'
    " tmux will send xterm-style keys when its xterm-keys option is on
    execute "set <xUp>=\e[1;*A"
    execute "set <xDown>=\e[1;*B"
    execute "set <xRight>=\e[1;*C"
    execute "set <xLeft>=\e[1;*D"
endif

" Haskell, GHC, etc
au Bufenter *.hs compiler ghc

let g:haddock_browser = "/usr/bin/xdg-open"
"let g:haddock_docdir = /home/hatate/.cabal/share/doc/

" Enable certain plugins depending on the file type
filetype plugin on

" Try to compile on save
"au BufWritePost * make

" Folding
set foldenable
set foldmethod=marker
set foldopen-=search
set foldopen-=undo
set foldopen-=undo

" Because SOMETHING is setting it to 2; FUCK THAT THING
au VimEnter * set cmdheight=1

" Rainbow parentheses
au VimEnter * RainbowParenthesesActivate
au Syntax * RainbowParenthesesLoadRound
au Syntax * RainbowParenthesesLoadSquare
let g:rbpt_max = 16
let g:rbpt_colorpairs = [
    \ ["green", "green"],
    \ ["yellow", "yellow"],
    \ ["darkyellow", "darkyellow"],
    \ ["darkred", "darkred"],
    \ ["darkmagenta", "darkmagenta"],
    \ ["darkcyan", "darkcyan"],
    \ ]

" Powerline

let g:Powerline_symbols = 'fancy'


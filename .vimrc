" :help nocp
" cp means you are forced to use OLD DEPRECATED CRAP; nocp disables that
set nocp
filetype off

set rtp+=~/.vim/bundle/Vundle.vim
call vundle#begin()

Plugin 'VundleVim/Vundle.vim'

Plugin 'kien/rainbow_parentheses.vim'

Plugin 'vim-airline/vim-airline'
Plugin 'vim-airline/vim-airline-themes'

Plugin 'tpope/vim-fugitive.git'

" Plugin 'ajh17/VimCompletesMe'
Plugin 'Shougo/neocomplete.vim'

Plugin 'pangloss/vim-javascript'
" FIXME
" Plugin 'bigfish/vim-js-context-coloring'
" Plugin 'othree/javascript-libraries-syntax.vim'
Plugin 'marijnh/tern_for_vim'

Plugin 'Raimondi/delimitMate'

Plugin 'Twinside/vim-haskellConceal'
" XXX requires: ghd-mod
Plugin 'eagletmt/neco-ghc'

call vundle#end()
filetype plugin indent on


" Line numbering
set nu

" Tabs and spacing
set tabstop=4
set shiftwidth=2
set expandtab
set softtabstop=4

" Indentation
set autoindent

" Wrapping
set wrap
set linebreak
set nolist

" Syntax highlighting
syntax on
set t_Co=256
color hemisu
set bg=light

" Syntax concealing
set conceallevel=2
set concealcursor=nc

hi clear Conceal
hi Conceal ctermfg=magenta

" Haskell-specific syntax options
let hs_highlight_debug=1

" Javascript specific syntax
let g:javascript_conceal_function       = "ƒ"
let g:javascript_conceal_null           = "∅"
let g:javascript_conceal_this           = "@"
let g:javascript_conceal_return         = "⇚"
let g:javascript_conceal_undefined      = "⊥"
let g:javascript_conceal_prototype      = "¶"
let g:javascript_conceal_arrow_function = "⇒"

" Context colors
let g:js_context_colors_enabled = 1

" Javascript libraries
let g:used_javascript_libs = 'ramda,underscore,jquery'

" Autocomplete

" Disable AutoComplPop.
let g:acp_enableAtStartup = 0
" " Use neocomplete.
let g:neocomplete#enable_at_startup = 1
" " Use smartcase.
let g:neocomplete#enable_smart_case = 1
" " Set minimum syntax keyword length.
let g:neocomplete#sources#syntax#min_keyword_length = 2

" Enable omni completion.
autocmd FileType css setlocal omnifunc=csscomplete#CompleteCSS
autocmd FileType html,markdown setlocal omnifunc=htmlcomplete#CompleteTags
autocmd FileType javascript setlocal omnifunc=javascriptcomplete#CompleteJS
autocmd FileType python setlocal omnifunc=pythoncomplete#Complete
autocmd FileType xml setlocal omnifunc=xmlcomplete#CompleteTags
" Disable haskell-vim omnifunc
let g:haskellmode_completion_ghc = 0
autocmd FileType haskell setlocal omnifunc=necoghc#omnifunc
" XXX can be slow; shows types
let g:necoghc_enable_detailed_browse = 1

" <TAB>: completion.
inoremap <expr><TAB>  pumvisible() ? "\<C-n>" : "\<TAB>"


" highlight search results
set hlsearch

" file encodings to recognize
set fileencodings=ucs-bom,utf-8,sjis,default,latin1

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
set nospell

if &term =~ '^screen'
    " tmux will send xterm-style keys when its xterm-keys option is on
    execute "set <xUp>=\e[1;*A"
    execute "set <xDown>=\e[1;*B"
    execute "set <xRight>=\e[1;*C"
    execute "set <xLeft>=\e[1;*D"
endif

" Haskell, GHC, etc
"au Bufenter *.hs compiler ghc

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

" let g:Powerline_symbols = 'fancy'

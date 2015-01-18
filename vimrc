" .vimrc
" Based on Steve Losh's vimrc :
" http://bitbucket.org/sjl/dotfiles/src/tip/vim/

" Plugins ----------------------------------------------------------------- {{{

call plug#begin('~/.vim/plugged')

Plug '5long/pytest-vim-compiler'
Plug 'AndrewRadev/linediff.vim', { 'on': ['Linediff', 'LinediffReset'] }
Plug 'airblade/vim-gitgutter'
Plug 'ap/vim-css-color'
Plug 'benmills/vimux'
Plug 'bling/vim-airline'
Plug 'christoomey/vim-tmux-navigator'
Plug 'ciaranm/securemodelines'
Plug 'dogrover/vim-pentadactyl', { 'for': 'pentadactyl' }
Plug 'edkolev/tmuxline.vim', { 'on': 'TmuxlineSnapshot' }
Plug 'exu/pgsql.vim', { 'for': 'pgsql' }
Plug 'godlygeek/tabular', { 'on': 'Tabularize' }
Plug 'gregsexton/MatchTag', { 'for': ['html', 'xml'] }
Plug 'groenewege/vim-less', { 'for': 'less' }
Plug 'honza/vim-snippets'
Plug 'ivanov/vim-ipython'
Plug 'jceb/vim-orgmode', { 'for': 'org' }
Plug 'junegunn/vim-github-dashboard', { 'on': ['GHDashboard', 'GHActivity'] }
Plug 'kien/ctrlp.vim'
" Plug 'kien/rainbow_parentheses.vim', { 'for': 'lisp' }
Plug 'klen/python-mode', { 'for': 'python' }
Plug 'majutsushi/tagbar', { 'on': 'TagbarToggle' }
Plug 'marijnh/tern_for_vim', { 'for': 'javascript', 'do': 'npm install' }
" Plug 'mhinz/vim-signify'
" Plug 'michaeljsmith/vim-indent-object'
Plug 'mileszs/ack.vim'
Plug 'mitsuhiko/vim-jinja', { 'for': ['htmljinja', 'sls'] }
Plug 'mitsuhiko/vim-sparkup', { 'on': ['SparkupExecute', 'SparkupNext'] }
Plug 'moll/vim-bbye', { 'on': 'Bdelete' }
Plug 'mtth/scratch.vim', { 'on': 'Scratch' }
Plug 'mxw/vim-jsx', { 'for': 'javascript' }
Plug 'othree/html5.vim'
Plug 'pangloss/vim-javascript', { 'for': 'javascript' }
Plug 'saltstack/salt-vim', { 'for': 'sls' }
Plug 'scrooloose/nerdtree', { 'on': 'NERDTreeToggle' }
Plug 'scrooloose/syntastic'
Plug 'SirVer/ultisnips'
" Plug 'sjl/AnsiEsc.vim'
Plug 'sjl/badwolf'
Plug 'sjl/clam.vim', { 'on': ['Clam', 'ClamVisual'] }
Plug 'sjl/gundo.vim', { 'on': 'GundoToggle' }
Plug 'sjl/splice.vim', { 'on': 'SpliceInit' }
" Plug 'smancill/conky-syntax.vim', { 'for': 'conkyrc' }
Plug 'tell-k/vim-autopep8', { 'for': 'python' }
Plug 'terryma/vim-multiple-cursors'
Plug 'tpope/vim-commentary'
Plug 'tpope/vim-dispatch', { 'on': 'Dispatch' }
Plug 'tpope/vim-fugitive'
Plug 'tpope/vim-markdown', { 'for': 'markdown' }
Plug 'tpope/vim-repeat'
Plug 'tpope/vim-surround'
Plug 'Valloric/YouCompleteMe', { 'do': './install.sh --clang-completer --system-libclang' }
Plug 'vim-scripts/Conflict2Diff', { 'on': 'Conflict2Diff' }
Plug 'vim-scripts/DirDiff.vim', { 'on': 'DirDiff' }
Plug 'vim-scripts/YankRing.vim', { 'on': 'YRShow' }
Plug 'wavded/vim-stylus', { 'for': 'stylus' }
Plug 'w0ng/vim-hybrid'

Plug '~/.vim/bundle/camptocamp', { 'for': 'camptocamp' }
Plug '~/.vim/bundle/closetags-custom'
Plug '~/.vim/bundle/django-custom', { 'for': ['django', 'htmldjango'] }
Plug '~/.vim/bundle/idlang', { 'for': 'idlang' }

call plug#end()

" }}}
" Preamble ---------------------------------------------------------------- {{{

set shell=/bin/bash

filetype off
filetype plugin indent on       " load file type plugins + indentation
set nocompatible                " choose no compatibility with legacy vi

" }}}
" Basic options ----------------------------------------------------------- {{{

set encoding=utf-8
set modelines=0
set autoindent                  " always set autoindenting on
set showmode                    " print current mode on the last line
set showcmd                     " display incomplete commands
set hidden                      " Allow backgrounding buffers without writing them
set visualbell
set ttyfast
set ruler
set backspace=indent,eol,start  " backspace through everything in insert mode
set nonumber
set norelativenumber
set laststatus=2                " show always statusline of last window
set history=1000
set undofile
set undoreload=10000
set list                        " show eol, tabs, spaces, trailing and non-breaking spaces
set listchars=tab:▸\ ,extends:❯,precedes:❮,trail:-,nbsp:_ " ,eol:¬
set lazyredraw
set matchtime=3
set showbreak=↪
set splitbelow
set splitright
set autowrite
set autoread
set shiftround
set title
set linebreak     " ne casse pas les mots en fin de ligne
set colorcolumn=+1
" set autochdir                   " change current dir
" set smartindent                 " clever autoindenting

" Spelling
"
" There are three dictionaries I use for spellchecking:
"
"   /usr/share/dict/words
"   Basic stuff.
"
"   ~/.vim/custom-dictionary.utf-8.add
"   Custom words (like my name).  This is in my (version-controlled) dotfiles.
"
"   ~/.vim-local-dictionary.utf-8.add
"   More custom words.  This is *not* version controlled, so I can stick
"   work stuff in here without leaking internal names and shit.
"
" I also remap zG to add to the local dict (vanilla zG is useless anyway).
set dictionary=/usr/share/dict/words
set spellfile=~/.vim/custom-dictionary.utf-8.add,~/.vim-local-dictionary.utf-8.add
nnoremap zG 2zg

" iTerm2 is currently slow as balls at rendering the nice unicode lines, so for
" now I'll just use ASCII pipes.  They're ugly but at least I won't want to kill
" myself when trying to move around a file.
set fillchars=diff:⣿,vert:│
" set fillchars=diff:⣿,vert:\|

" Don't try to highlight lines longer than 800 characters.
set synmaxcol=800

" Time out on key codes but not mappings.
" Basically this makes terminal Vim work sanely.
set notimeout
set ttimeout
set ttimeoutlen=10

" Make Vim able to edit crontab files again.
set backupskip=/tmp/*,/private/tmp/*"

" Better Completion
set complete=.,w,b,u,t
set completeopt=longest,menuone,preview

" Save when losing focus
au FocusLost * :silent! wall

" Resize splits when the window is resized
au VimResized * :wincmd =

" Leader
let mapleader = ","
let maplocalleader = "\\"

" Cursorline {{{

set cursorline

" Only show cursorline in the current window and in normal mode.
augroup cline
    au!
    au WinLeave,InsertEnter * set nocursorline
    au WinEnter,InsertLeave * set cursorline
augroup END

" }}}
" Trailing whitespace {{{
" Only shown when not in insert mode so I don't go insane.

augroup trailing
    au!

    " au InsertEnter * :set listchars-=trail:⌴

    " Remove trailing whitespace
    autocmd BufWritePre * :%s/\s\+$//e
augroup END

" }}}
" Wildmenu completion {{{

set wildmenu
" set wildmode=list:longest
set wildmode=longest:full,full

set wildignore+=.hg,.git,.svn                    " Version control
set wildignore+=*.aux,*.out,*.toc,.dvi,.bbl      " LaTeX intermediate files
set wildignore+=*.jpg,*.bmp,*.gif,*.png,*.jpeg   " binary images
set wildignore+=*.o,*.obj,*.exe,*.dll,*.manifest " compiled object files
set wildignore+=*.spl                            " compiled spelling word lists
set wildignore+=*.sw?                            " Vim swap files
set wildignore+=*.luac                           " Lua byte code
set wildignore+=*.pyc,*.pyo                      " Python byte code
set wildignore+=*.orig                           " Merge resolution files

" When doing tab completion, give the following files lower priority. You may
" wish to set 'wildignore' to completely ignore files, and 'wildmenu' to enable
" enhanced tab completion. These can be done in the user vimrc file.
set suffixes+=.info,.log,.lo

" }}}
" Line Return {{{

" Make sure Vim returns to the same line when you reopen a file.
" (but not for commit messages. see :help last-position-jump)
augroup line_return
    au!
    au BufReadPost *
        \ if &filetype !~ '^git\c' && line("'\"") > 0 && line("'\"") <= line("$") |
        \     execute 'normal! g`"zvzz' |
        \ endif
augroup END

" }}}
" Tabs, spaces, wrapping {{{

set tabstop=4
set shiftwidth=4
set softtabstop=4
set expandtab               " use spaces, not tabs
set wrap

" Autorise le passage d'une ligne à l'autre avec les flèches gauche et droite
set whichwrap=<,>,[,]

" Si activé, un <Tab> au début d'une ligne insère des blancs selon la valeur de
" 'shiftwidth'. 'tabstop' est utilisé dans les autres endroits. Un <RetArr> en
" début de ligne supprime un nombre d'espaces équivalant à la valeur de
" 'shiftwidth'.  Si désactivé, un <Tab> insère toujours des blancs selon la
" valeur de 'tabstop'. 'shiftwidth' n'est utilisé que lors du décalage de texte
" à gauche ou à droite.
set smarttab

set textwidth=80
set formatoptions=q         " Allow formatting of comments with "gq".
set formatoptions+=r        " Automatically insert the current comment leader
set formatoptions+=n        " recognize numbered lists
set formatoptions+=1        " Don't break a line after a one-letter word
"set formatoptions+=a       " Automatic formatting of paragraphs
set formatoptions+=t        " Auto-wrap text using textwidth
set formatoptions+=c        " Auto-wrap comments using textwidth
if v:version > 703 || v:version == 703
  set formatoptions+=j " Delete comment char when joining commented lines
endif

set colorcolumn=+1

" }}}
" Backups {{{

set backup                        " enable backups
set noswapfile                    " it's 2013, Vim.

set undodir=~/.vim/tmp/undo//     " undo files
set backupdir=~/.vim/tmp/backup// " backups
set directory=~/.vim/tmp/swap//   " swap files

" Make those folders automatically if they don't already exist.
if !isdirectory(expand(&undodir))
    call mkdir(expand(&undodir), "p")
endif
if !isdirectory(expand(&backupdir))
    call mkdir(expand(&backupdir), "p")
endif
if !isdirectory(expand(&directory))
    call mkdir(expand(&directory), "p")
endif

" }}}
" Color scheme {{{

syntax on
set background=dark
" colorscheme molokai

" Make the gutters darker than the background.
let g:badwolf_darkgutter = 1
" Make the tab line lighter than the background.
let g:badwolf_tabline = 2
" Turn off HTML link underlining
" let g:badwolf_html_link_underline = 0
" Turn on CSS properties highlighting
let g:badwolf_css_props_highlight = 1
" colorscheme badwolf

let g:hybrid_use_Xresources = 1
colorscheme hybrid

" Reload the colorscheme whenever we write the file.
" augroup color_badwolf_dev
"     au!
"     au BufWritePost badwolf.vim color badwolf
" augroup END

" Highlight VCS conflict markers
match ErrorMsg '^\(<\|=\|>\)\{7\}\([^=].\+\)\?$'

" Badwolf colors:
" plain          = #f8f6f2
" snow           = #ffffff
" coal           = #000000
" brightgravel   = #d9cec3
" lightgravel    = #998f84
" gravel         = #857f78
" mediumgravel   = #666462
" deepgravel     = #45413b
" deepergravel   = #35322d
" darkgravel     = #242321
" blackgravel    = #1c1b1a
" blackestgravel = #141413
" dalespale      = #fade3e
" dirtyblonde    = #f4cf86
" taffy          = #ff2c4b
" saltwatertaffy = #8cffba
" tardis         = #0a9dff
" orange         = #ffa724
" lime           = #aeee00
" dress          = #ff9eb8
" toffee         = #b88853
" coffee         = #c7915b
" darkroast      = #88633f

" Highlight Special
" hi Special guifg=#ffa724

" }}}

" }}}
" Abbreviations ----------------------------------------------------------- {{{

iabbrev qd quand
iabbrev qq quelque
iabbrev qqs quelques
iabbrev gh/ http://github.com/
iabbrev ssig -- <cr>Simon

" }}}
" Convenience mappings ---------------------------------------------------- {{{

" Don't make a # force column zero.
inoremap # X<BS>#

" Toggle line numbers
nnoremap <leader>n :setlocal number!<cr>

" Sort lines
nnoremap <leader>so vip:!sort<cr>
vnoremap <leader>s :!sort<cr>

" Tabs
nnoremap <leader>( :tabprev<cr>
nnoremap <leader>) :tabnext<cr>
nnoremap <leader>tt :tabs<cr>
nnoremap <leader>tn :tabnew<cr>
nnoremap <leader>tc :tabclose<cr>

" System clipboard interaction.  Mostly from:
" https://github.com/henrik/dotfiles/blob/master/vim/config/mappings.vim
" *: primary, +: clipboard
noremap <leader>y "*y
noremap <leader>Y "+y
vnoremap <leader>y "*ygv
vnoremap <leader>Y "+ygv
" noremap <leader>p :silent! set paste<CR>"*p<CR>:set nopaste<CR>
noremap <leader>p :set paste<CR>"*P<CR>:set nopaste<CR>
noremap <leader>P :set paste<CR>"+P<CR>:set nopaste<CR>

" Rebuild Ctags (mnemonic RC -> CR -> <cr>)
nnoremap <leader><cr> :silent !myctags<cr>:redraw!<cr>

" Clean trailing whitespace
nnoremap <leader>w mz:%s/\s\+$//<cr>:let @/=''<cr>`z
" Clean leading spaces
" nmap _S :%s/^\s\+//<CR>

" Send visual selection to paste.stevelosh.com
" vnoremap <c-p> :w !curl -sF 'sprunge=<-' 'http://paste.stevelosh.com' \| tr -d '\n ' \| pbcopy && open `pbpaste`<cr>

" Select entire buffer
nnoremap vaa ggvGg_
nnoremap Vaa ggVG

" "Uppercase word" mapping.
"
" This mapping allows you to press <c-u> in insert mode to convert the current
" word to uppercase.  It's handy when you're writing names of constants and
" don't want to use Capslock.
"
" To use it you type the name of the constant in lowercase.  While your
" cursor is at the end of the word, press <c-u> to uppercase it, and then
" continue happily on your way:
"
"                            cursor
"                            v
"     max_connections_allowed|
"     <c-u>
"     MAX_CONNECTIONS_ALLOWED|
"                            ^
"                            cursor
"
" It works by exiting out of insert mode, recording the current cursor location
" in the z mark, using gUiw to uppercase inside the current word, moving back to
" the z mark, and entering insert mode again.
"
" Note that this will overwrite the contents of the z mark.  I never use it, but
" if you do you'll probably want to use another mark.
inoremap <C-u> <esc>mzgUiw`za

" Panic Button
nnoremap <F12> mzggg?G`z

" Substitute
nnoremap <leader>: :%s//<left>

" Diffoff
nnoremap <leader>D :diffoff!<cr>

" Wrappe à 72 caractères avec la touche '#'
nnoremap <leader># gwap

" Wrappe et justifie à 72 caractères avec la touche '@'
" nnoremap <leader>@ {v}! par 72j

" Formatting, TextMate-style
nnoremap Q gqip
vnoremap Q gq

" Reformat line.
" I never use l as a macro register anyway.
nnoremap ql gqq

" Easier linewise reselection of what you just pasted.
nnoremap <leader>V V`]

" Vmap for maintain Visual Mode after shifting > and <
vmap < <gv
vmap > >gv

" Indent/dedent/autoindent what you just pasted.
nnoremap <lt>> V`]<
nnoremap ><lt> V`]>
nnoremap =- V`]=

" Keep the cursor in place while joining lines
" nnoremap J mzJ`z

" Join an entire paragraph.
"
" Useful for writing GitHub comments in actual Markdown and then translating it
" to their bastardized version of Markdown.
nnoremap <leader>J mzvipJ`z

" Split line (sister to [J]oin lines)
" The normal use of S is covered by cc, so don't worry about shadowing it.
nnoremap S i<cr><esc>^mwgk:silent! s/\v +$//<cr>:noh<cr>`w

" HTML tag closing
" inoremap <C-_> <space><bs><esc>:call InsertCloseTag()<cr>a
inoremap <C-_> </<C-X><C-O>

" Source
vnoremap <leader>S y:execute @@<cr>:echo 'Sourced selection.'<cr>
nnoremap <leader>S ^vg_y:execute @@<cr>:echo 'Sourced line.'<cr>

" Marks and Quotes
" noremap ' `
" noremap æ '
" noremap ` <C-^>

" Select (charwise) the contents of the current line, excluding indentation.
" Great for pasting Python lines into REPLs.
nnoremap vv ^vg_

" Faster Esc
inoremap jk <esc>

" Sudo to write
cnoremap w!! w !sudo tee % >/dev/null

" Quickreturn
inoremap <c-cr> <esc>A<cr>

" Toggle paste
" For some reason pastetoggle doesn't redraw the screen (thus the status bar
" doesn't change) while :set paste! does, so I use that instead.
" set pastetoggle=<F6>
nnoremap <F6> :set paste!<cr>

" Toggle [i]nvisible characters
nnoremap <leader>i :set list!<cr>

" Unfuck my screen
nnoremap <leader>u :syntax sync fromstart<cr>:redraw!<cr>

" switch between the currently open buffer and the previous one
nnoremap <c-Tab> <c-^>

" On fait tourner les tampons ...
nnoremap <c-n> :bnext!<CR>
nnoremap <c-p> :bprev!<CR>

" make ; do the same thing as :
nnoremap ; :
" visual command line
" nnoremap ; :<c-f>

" map CTRL+k S N (non-breaking space) to CTRL+space
imap <Nul> <C-k>NS
imap <C-Space> <C-k>NS

" change directory to the file being edited
nnoremap <leader>C :cd %:p:h<CR>:pwd<CR>

" disable cursor keys in normal mode
" map <Left> :echo "no!"<cr>
" map <Right> :echo "no!"<cr>
" map <Up> :echo "no!"<cr>
" map <Down> :echo "no!"<cr>

" expand %% to current directory in command-line mode
" http://vimcasts.org/e/14
cnoremap %% <C-R>=expand('%:h').'/'<cr>

nnoremap <leader>o :e <C-R>=expand('%:h').'/'<cr>

" Ctrl-+ pour augmenter la taille de la police
noremap <C-Up> :let &guifont=substitute(&guifont, '\d\+', '\=eval(submatch(0)+1)', '')<CR>
noremap <C-Down> :let &guifont=substitute(&guifont, '\d\+', '\=eval(submatch(0)-1)', '')<CR>

" Easy filetype switching {{{

nnoremap _md :set ft=markdown<CR>
nnoremap _hd :set ft=htmldjango<CR>
nnoremap _hj :set ft=htmljinja<CR>
nnoremap _js :set ft=javascript<CR>
nnoremap _pd :set ft=python.django<CR>
nnoremap _d  :set ft=diff<CR>

" Ranger
" nnoremap <leader>r :silent !ranger %:h<cr>:redraw!<cr>
" nnoremap <leader>R :silent !ranger<cr>:redraw!<cr>

" }}}
" Insert Mode Completion {{{

" filenames
inoremap <c-f> <c-x><c-f>
" tags
inoremap <c-]> <c-x><c-]>
" dictionary
inoremap <c-k> <c-x><c-k>

" }}}

" }}}
" Quick editing ----------------------------------------------------------- {{{

nnoremap <leader>ec :vsplit ~/.vim/doc/cheatsheet.txt<cr>
nnoremap <leader>ed :vsplit ~/.vim/custom-dictionary.utf-8.add<cr>
nnoremap <leader>eg :vsplit ~/.gitconfig<cr>
nnoremap <leader>eh :vsplit ~/.hgrc<cr>
nnoremap <leader>em :vsplit ~/.mutt/muttrc<cr>
nnoremap <leader>eo :vsplit ~/org<cr>4j
nnoremap <leader>ep :vsplit ~/.pentadactylrc<cr>
nnoremap <leader>er :vsplit ~/lib/dotfiles/README<cr>
nnoremap <leader>et :vsplit ~/.tmux.conf<cr>
nnoremap <leader>ev :vsplit ~/.vimrc<cr>
nnoremap <leader>ez :vsplit ~/lib/dotfiles/zsh<cr>4j

" }}}
" Searching and movement -------------------------------------------------- {{{

" Use sane regexes.
nnoremap / /\v
vnoremap / /\v

set ignorecase                  " searches are case insensitive...
set smartcase                   " ... unless they contain at least one capital letter
set incsearch                   " incremental searching
set showmatch                   " show matching brackets
set hlsearch                    " highlight matches
set gdefault                    " substitute all matches on the line

set scrolloff=3         " min nb of lines to keep above and below the cursor.
set sidescroll=1        " min nb of columns to scroll horizontally.(with nowrap)
set sidescrolloff=10

set virtualedit+=block  " allow cursor where there is no actual character.

noremap <silent> <leader><space> :noh<cr>:call clearmatches()<cr>

runtime macros/matchit.vim
map <tab> %
silent! unmap [%
silent! unmap ]%

" Made D behave
nnoremap D d$

" Don't move on *
nnoremap * *<c-o>

" Jumping to tags.
"
" Basically, <c-]> jumps to tags (like normal) and <c-\> opens the tag in a new
" split instead.
"
" Both of them will align the destination line to the upper middle part of the
" screen.  Both will pulse the cursor line so you can see where the hell you
" are.  <c-\> will also fold everything in the buffer and then unfold just
" enough for you to see the destination line.
function! JumpToTag()
    execute "normal! \<c-]>mzzvzz15\<c-e>"
    execute "keepjumps normal! `z"
    Pulse
endfunction

function! JumpToTagInSplit()
    execute "normal! \<c-w>v\<c-]>mzzMzvzz15\<c-e>"
    execute "keepjumps normal! `z"
    Pulse
endfunction

nnoremap <c-]> :silent! call JumpToTag()<cr>
nnoremap <c-\> :silent! call JumpToTagInSplit()<cr>

" nnoremap <c-\> <c-w>v<c-]>mzzMzvzz15<c-e>`z:Pulse<cr>

" Keep search matches in the middle of the window.
nnoremap n nzzzv:Pulse<cr>
nnoremap N Nzzzv:Pulse<cr>

" Same when jumping around
nnoremap g; g;zz
nnoremap g, g,zz
nnoremap <c-o> <c-o>zz

" Easier to type, and I never use the default behavior.
noremap H ^
noremap L $
vnoremap L g_

" Heresy
inoremap <c-a> <esc>I
inoremap <c-e> <esc>A
cnoremap <c-a> <home>
cnoremap <c-e> <end>

" gi already moves to "last place you exited insert mode", so we'll map gI to
" something similar: move to last change
nnoremap gI `.

" Fix linewise visual selection of various text objects
nnoremap VV V
nnoremap Vit vitVkoj
nnoremap Vat vatV
nnoremap Vab vabV
nnoremap VaB vaBV

" Open a Quickfix window for the last search.
nnoremap <silent> <leader>? :execute 'vimgrep /'.@/.'/g %'<CR>:copen<CR>

" Ack for the last search.
nnoremap <silent> <leader>/ :execute "Ack! '" . substitute(substitute(substitute(@/, "\\\\<", "\\\\b", ""), "\\\\>", "\\\\b", ""), "\\\\v", "", "") . "'"<CR>

" Directional Keys {{{

" It's 2013.
noremap j gj
noremap k gk
noremap gj j
noremap gk k

" Easy buffer navigation
noremap <C-h> <C-w>h
noremap <C-j> <C-w>j
noremap <C-k> <C-w>k
noremap <C-l> <C-w>l

noremap <leader>v <C-w>v

" }}}
" Visual Mode */# from Scrooloose {{{

function! s:VSetSearch()
  let temp = @@
  norm! gvy
  let @/ = '\V' . substitute(escape(@@, '\'), '\n', '\\n', 'g')
  let @@ = temp
endfunction

vnoremap * :<C-u>call <SID>VSetSearch()<CR>//<CR><c-o>
vnoremap # :<C-u>call <SID>VSetSearch()<CR>??<CR><c-o>

" }}}
" List navigation {{{
"             Location List     QuickFix Window
"            (e.g. Syntastic)     (e.g. Ack)
"            ----------------------------------
" Next      |     M-j               M-Down     |
" Previous  |     M-k                M-Up      |
"            ----------------------------------
"
" nnoremap ∆ :lnext<cr>zvzz
" nnoremap ˚ :lprevious<cr>zvzz
" inoremap ∆ <esc>:lnext<cr>zvzz
" inoremap ˚ <esc>:lprevious<cr>zvzz
nnoremap <m-j> :cnext<cr>zvzz
nnoremap <m-k> :cprevious<cr>zvzz

" nnoremap <left>  :cprev<cr>zvzz
" nnoremap <right> :cnext<cr>zvzz
" nnoremap <up>    :lprev<cr>zvzz
" nnoremap <down>  :lnext<cr>zvzz

" }}}

" }}}
" Folding ----------------------------------------------------------------- {{{

set foldlevelstart=0

" Space to toggle folds.
nnoremap <Space> za
vnoremap <Space> za

" "Refocus" the current line: close all folds and open just the folds containing
" the current line.
nnoremap <leader>z zMzvzz

" Make zO recursively open whatever fold we're in, even if it's partially open.
nnoremap zO zczO

" "Focus" the current line.  Basically:
"
" 1. Close all folds.
" 2. Open just the folds containing the current line.
" 3. Move the line to a little bit (15 lines) above the center of the screen.
" 4. Pulse the cursor line.  My eyes are bad.
"
" This mapping wipes out the z mark, which I never use.
"
" I use :sus for the rare times I want to actually background Vim.
" nnoremap <c-z> mzzMzvzz15<c-e>`z:Pulse<cr>

function! MyFoldText() " {{{
    let line = getline(v:foldstart)

    let nucolwidth = &fdc + &number * &numberwidth
    let windowwidth = winwidth(0) - nucolwidth - 3
    let foldedlinecount = v:foldend - v:foldstart

    " expand tabs into spaces
    let onetab = strpart('          ', 0, &tabstop)
    let line = substitute(line, '\t', onetab, 'g')

    let line = strpart(line, 0, windowwidth - 2 -len(foldedlinecount))
    let fillcharcount = windowwidth - len(line) - len(foldedlinecount)
    return line . '…' . repeat(" ",fillcharcount) . foldedlinecount . '…' . ' '
endfunction " }}}
set foldtext=MyFoldText()

" if has("gui_running")
"    " Ajoute une marge à gauche pour afficher les +/- des replis
"    set foldcolumn=2
"    " Le découpage des folders se base sur l'indentation
"    set foldmethod=marker "indent
" endif

" }}}
" Filetype-specific ------------------------------------------------------- {{{

" C {{{

augroup ft_c
    au!
    au FileType c,cpp setlocal foldmethod=syntax cindent
augroup END

" }}}
" Common Lisp {{{

" let g:lisp_rainbow = 1

" augroup ft_commonlisp
"     au!

"     au FileType lisp RainbowParenthesesActivate
"     au syntax lisp RainbowParenthesesLoadRound
"     au syntax lisp RainbowParenthesesLoadSquare
"     au syntax lisp RainbowParenthesesLoadBraces

"     au FileType lisp call TurnOnLispFolding()
" augroup END

" }}}
" CSS and LessCSS {{{

augroup ft_css
    au!

    au Filetype less,css,scss setlocal foldmethod=marker
    au Filetype less,css,scss setlocal foldmarker={,}
    au Filetype less,css,scss setlocal omnifunc=csscomplete#CompleteCSS
    au Filetype less,css,scss setlocal iskeyword+=-
    au FileType less,css,scss setlocal expandtab shiftwidth=2 tabstop=2 softtabstop=2

    " Use <leader>S to sort properties.  Turns this:
    "
    "     p {
    "         width: 200px;
    "         height: 100px;
    "         background: red;
    "
    "         ...
    "     }
    "
    " into this:

    "     p {
    "         background: red;
    "         height: 100px;
    "         width: 200px;
    "
    "         ...
    "     }
    au BufNewFile,BufRead *.less,*.css nnoremap <buffer> <localleader>S ?{<CR>jV/\v^\s*\}?$<CR>k:sort<CR>:noh<CR>

    " Make {<cr> insert a pair of brackets in such a way that the cursor is correctly
    " positioned inside of them AND the following code doesn't get unfolded.
    au BufNewFile,BufRead *.less,*.css,*.scss inoremap <buffer> {<cr> {}<left><cr><space><space><space><space>.<cr><esc>kA<bs>
augroup END

augroup ft_less
    au!

    au BufNewFile,BufRead *.less setlocal filetype=less
    " au FileWritePost,BufWritePost *.less :call LessCSSCompress()

    function! LessCSSCompress()
        let cwd = expand(':p:h')
        let name = expand(':t:r')
        if (executable('lessc'))
        cal system('lessc '.cwd.'/'.name.'.less > '.cwd.'/'.name.'.css &')
        endif
    endfunction
augroup END

" }}}
" Django {{{

" augroup ft_django
"     au!

"     au BufNewFile,BufRead urls.py           setlocal nowrap
"     au BufNewFile,BufRead urls.py           normal! zR
"     au BufNewFile,BufRead dashboard.py      normal! zR
"     au BufNewFile,BufRead local_settings.py normal! zR

"     au BufNewFile,BufRead admin.py     setlocal filetype=python.django
"     au BufNewFile,BufRead urls.py      setlocal filetype=python.django
"     au BufNewFile,BufRead models.py    setlocal filetype=python.django
"     au BufNewFile,BufRead views.py     setlocal filetype=python.django
"     au BufNewFile,BufRead settings.py  setlocal filetype=python.django
"     au BufNewFile,BufRead settings.py  setlocal foldmethod=marker
"     au BufNewFile,BufRead forms.py     setlocal filetype=python.django
"     au BufNewFile,BufRead common_settings.py  setlocal filetype=python.django
"     au BufNewFile,BufRead common_settings.py  setlocal foldmethod=marker
" augroup END

" }}}
" HTML, Django, Jinja, Dram {{{

fun! s:SelectHTML()
  let n = 1
  while n < 50 && n < line("$")
    " check for jinja
    if getline(n) =~ '{%\s*\(extends\|block\|macro\|set\|if\|for\|include\|trans\)\>'
      set ft=htmljinja
      return
    endif
    " check for django
    if getline(n) =~ '{%\s*\(extends\|block\|comment\|ssi\|if\|for\|blocktrans\)\>'
      set ft=htmldjango
      return
    endif
    " check for mako
    if getline(n) =~ '<%\(def\|inherit\)'
      set ft=mako
      return
    endif
    " check for genshi
    if getline(n) =~ 'xmlns:py\|py:\(match\|for\|if\|def\|strip\|xmlns\)'
      set ft=genshi
      return
    endif
    let n = n + 1
  endwhile
  " go with html
  set ft=html
endfun

augroup ft_html
    au!

    au BufNewFile,BufRead *.html,*.htm,*.tpl call s:SelectHTML()

    au FileType html,htmljinja,htmldjango setlocal foldmethod=manual
    au FileType html,htmljinja,htmldjango setlocal expandtab shiftwidth=2 tabstop=2 softtabstop=2

    " Use <localleader>f to fold the current tag.
    au FileType html,htmljinja,htmldjango nnoremap <buffer> <localleader>f Vatzf

    " Use <localleader>t to fold the current templatetag.
    au FileType html,htmljinja,htmldjango nmap <buffer> <localleader>t viikojozf

    " Use Shift-Return to turn this:
    "     <tag>|</tag>
    "
    " into this:
    "     <tag>
    "         |
    "     </tag>
    au FileType html,htmljinja,htmldjango nnoremap <buffer> <s-cr> vit<esc>a<cr><esc>vito<esc>i<cr><esc>

    " Smarter pasting
    au FileType html,htmljinja,htmldjango nnoremap <buffer> p :<C-U>YRPaste 'p'<CR>v`]=`]
    au FileType html,htmljinja,htmldjango nnoremap <buffer> P :<C-U>YRPaste 'P'<CR>v`]=`]
    " au FileType html,htmljinja,htmldjango nnoremap <buffer> π :<C-U>YRPaste 'p'<CR>
    " au FileType html,htmljinja,htmldjango nnoremap <buffer> ∏ :<C-U>YRPaste 'P'<CR>

    " Indent tag
    au FileType html,htmljinja,htmldjango nnoremap <buffer> <leader>= Vat=

    " Django tags
    au FileType htmljinja,htmldjango inoremap <buffer> <c-t> {%<space><space>%}<left><left><left>

    " Django variables
    au FileType htmljinja,htmldjango inoremap <buffer> <c-b> {{<space><space>}}<left><left><left>
augroup END

" }}}
" Java {{{

augroup ft_java
    au!

    au FileType java setlocal foldmethod=marker
    au FileType java setlocal foldmarker={,}
augroup END

" }}}
" Javascript {{{

" let g:tern_show_argument_hints = 'on_hold'
let g:tern_show_signature_in_pum = 1

augroup ft_javascript
    au!

    " Treat JSON files like JavaScript
    au BufNewFile,BufRead *.json set ft=javascript

    au FileType javascript setlocal expandtab shiftwidth=2 tabstop=2 softtabstop=2
    au FileType javascript setlocal foldmethod=marker
    au FileType javascript setlocal foldmarker={,}
    " au FileType javascript setlocal omnifunc=javascriptcomplete#Complete

    " Make {<cr> insert a pair of brackets in such a way that the cursor is correctly
    " positioned inside of them AND the following code doesn't get unfolded.
    au Filetype javascript inoremap <buffer> {<cr> {}<left><cr><space><space><space><space>.<cr><esc>kA<bs>
augroup END

" }}}
" Mail {{{

augroup ft_mail
    au!

    au BufRead,BufNewFile ~/.mutt/tmp/mutt* setfiletype mail

    au Filetype mail setlocal spell spelllang=fr,en
augroup END

" }}}
" Makefile {{{

augroup ft_make
    au!

    " In Makefiles, use real tabs, not tabs expanded to spaces
    au FileType make set noexpandtab
augroup END

" }}}
" Man {{{

runtime! ftplugin/man.vim

" }}}
" Markdown {{{

let g:markdown_folding = 1

augroup ft_markdown
    au!

    " au BufNewFile,BufRead *.m*down setlocal filetype=markdown
    " au BufNewFile,BufRead *.md setfiletype markdown
    " au BufNewFile,BufRead *.mkd setfiletype markdown
    au FileType markdown setlocal foldlevel=1

    " Use <localleader>1/2/3 to add headings.
    au Filetype markdown nnoremap <buffer> <localleader>1 yypVr=:redraw<cr>
    au Filetype markdown nnoremap <buffer> <localleader>2 yypVr-:redraw<cr>
    au Filetype markdown nnoremap <buffer> <localleader>3 mzI###<space><esc>`zllll
    au Filetype markdown nnoremap <buffer> <localleader>4 mzI####<space><esc>`zlllll

    au Filetype markdown nnoremap <buffer> <localleader>p VV:'<,'>!python -m json.tool<cr>
    au Filetype markdown vnoremap <buffer> <localleader>p :!python -m json.tool<cr>
augroup END

" }}}
" Mercurial {{{

augroup ft_mercurial
    au!

    au BufNewFile,BufRead *hg-editor-*.txt setlocal filetype=hgcommit
augroup END

" }}}
" Mutt {{{

augroup ft_muttrc
    au!

    au BufRead,BufNewFile *.muttrc set ft=muttrc

    au FileType muttrc setlocal foldmethod=marker foldmarker={{{,}}}
augroup END

" }}}
" Nginx {{{

augroup ft_nginx
    au!

    au BufRead,BufNewFile /etc/nginx/conf/*                      set ft=nginx
    au BufRead,BufNewFile /etc/nginx/sites-available/*           set ft=nginx
    au BufRead,BufNewFile /usr/local/etc/nginx/sites-available/* set ft=nginx
    au BufRead,BufNewFile vhost.nginx                            set ft=nginx

    au FileType nginx setlocal foldmethod=marker foldmarker={,}
augroup END

" }}}
" OrgMode {{{

augroup ft_org
    au!

    au Filetype org nmap <buffer> Q vahjgq
    au Filetype org setlocal nolist
augroup END

" }}}
" Pentadactyl {{{

augroup ft_pentadactyl
    au!
    au BufNewFile,BufRead *pentadactylrc set filetype=pentadactyl
    au BufNewFile,BufRead /tmp/**/pentadactyl.txt set nolist wrap linebreak columns=100 colorcolumn=0
augroup END

" }}}
" Php {{{

augroup ft_php
    au!
    autocmd BufRead,BufNewFile *.inc setfiletype php
augroup END

" }}}
" Postgresql {{{

augroup ft_postgres
    au!

    au BufNewFile,BufRead *.sql set filetype=pgsql
    au BufNewFile,BufRead *.pgsql set filetype=pgsql

    au FileType pgsql set foldmethod=indent
    au FileType pgsql set softtabstop=2 shiftwidth=2
    au FileType pgsql setlocal commentstring=--\ %s comments=:--
augroup END

" }}}
" Puppet {{{

" augroup ft_puppet
"     au!

"     au Filetype puppet setlocal foldmethod=marker
"     au Filetype puppet setlocal foldmarker={,}
" augroup END

" }}}
" Python {{{

augroup ft_python
    au!
    " add header for new files
    au BufNewFile *.py 0put=\"# -*- coding: utf-8 -*-\<nl>\<nl>\"|$
    " au BufNewFile *.py 0put=\"#!/usr/bin/env python2\"|1put=\"# -*- coding: utf-8 -*-\<nl>\<nl>\"|$

    " make Python follow PEP8 ( http://www.python.org/dev/peps/pep-0008/ )
    au FileType python set softtabstop=4 tabstop=4 shiftwidth=4 textwidth=79
    au FileType python setlocal smartindent cinwords=if,elif,else,for,while,try,except,finally,def,class,with

    " au FileType python setlocal omnifunc=pythoncomplete#Complete
    au FileType python setlocal define=^\s*\\(def\\\\|class\\)
    au FileType man nnoremap <buffer> <cr> :q<cr>

    au FileType python compiler pytest
    " au FileType python let b:dispatch = 'py.test'

    " Jesus tapdancing Christ, built-in Python syntax, you couldn't let me
    " override this in a normal way, could you?
    au FileType python if exists("python_space_error_highlight") | unlet python_space_error_highlight | endif

    " Jesus, Python.  Five characters of punctuation for a damn string?
    au FileType python inoremap <buffer> <c-g> _(u'')<left><left>

    au FileType python inoremap <buffer> <c-b> """"""<left><left><left>

    au FileType python iabbrev <buffer> afo assert False, "Okay"

    " Autopep8
    let g:autopep8_select="E1,E2,E3,E4,W2,W3"
    au FileType python map <buffer> <localleader>p :call Autopep8()<CR>
augroup END

" }}}
" QuickFix {{{

augroup ft_quickfix
    au!
    au Filetype qf setlocal colorcolumn=0 nolist nocursorline nowrap tw=0
augroup END

" }}}
" ReStructuredText {{{

augroup ft_rest
    au!

    au FileType rst setlocal foldlevel=1

    au Filetype rst nnoremap <buffer> <localleader>1 yypVr=:redraw<cr>
    au Filetype rst nnoremap <buffer> <localleader>2 yypVr-:redraw<cr>
    au Filetype rst nnoremap <buffer> <localleader>3 yypVr~:redraw<cr>
    au Filetype rst nnoremap <buffer> <localleader>4 yypVr`:redraw<cr>
augroup END

" }}}
" Ruby {{{

augroup ft_ruby
    au!
    au Filetype ruby setlocal foldmethod=syntax
    au BufRead,BufNewFile Capfile setlocal filetype=ruby
augroup END

" }}}
" Salt {{{

" Force using the Jinja template syntax file
let g:sls_use_jinja_syntax = 1

" }}}
" Shell {{{

augroup ft_sh
    au!
    " header
    au BufNewFile *.sh,*.bash 0put =\"#!/bin/bash\<nl># -*- coding: utf-8 -*-\<nl>\<nl>\"|$
augroup END

" }}}
" Standard In {{{

augroup ft_stdin
    au!

    " Treat buffers from stdin (e.g.: echo foo | vim -) as scratch.
    au StdinReadPost * :set buftype=nofile
augroup END

" }}}
" Text, tex, ... {{{

augroup ft_txt
    au!
    au BufRead,BufNewFile *.txt setfiletype text
    " au BufRead,BufNewFile *wikipedia.org* setfiletype Wikipedia
    " au BufRead,BufNewFile *camptocamp.org* setfiletype camptocamp

    " au FileType text setlocal textwidth=78 lbr wrap fo=l "spell spelllang=fr
    au FileType text setlocal textwidth=78 lbr complete+=k "fo+=a spell spelllang=fr
    au FileType tex setlocal textwidth=78 "spell spelllang=fr

    " Wiki pages should be soft-wrapped.
    au FileType Wikipedia setlocal wrap linebreak nolist spell spelllang=fr

    " spell check for commit messages
    au Filetype svn,*commit* setlocal spell spelllang=en
augroup END

" }}}
" Vagrant {{{

augroup ft_vagrant
    au!
    au BufRead,BufNewFile Vagrantfile set ft=ruby
augroup END

" }}}
" Vim {{{

augroup ft_vim
    au!

    au FileType vim setlocal foldmethod=marker keywordprg=:help
    au FileType help setlocal textwidth=78
    au BufWinEnter *.txt if &ft == 'help' | wincmd L | endif

    " Reload vimrc to apply changes
    autocmd! BufWritePost .vimrc source %
augroup END

" }}}
" YAML {{{

augroup ft_yaml
    au!

    au FileType yaml set shiftwidth=2
augroup END

" }}}
" XML {{{

augroup ft_xml
    au!

    au FileType xml setlocal expandtab shiftwidth=4 tabstop=4 softtabstop=4
    au FileType xml setlocal foldmethod=manual

    " Use <localleader>f to fold the current tag.
    au FileType xml nnoremap <buffer> <localleader>f Vatzf

    " Indent tag
    au FileType xml nnoremap <buffer> <leader>= Vat=
augroup END

" }}}
" }}}
" Plugin settings --------------------------------------------------------- {{{

" Ack {{{

nnoremap <leader>a :Ack!<space>
let g:ackprg = 'ag --smart-case --nogroup --nocolor --column'

" }}}
" Airline {{{

let g:airline_powerline_fonts = 1
" let g:airline_section_z = airline#section#create_right(['%2p%% %3l:%1c'])
let g:airline_theme='badwolf'

let g:airline#extensions#branch#enabled = 1
let g:airline#extensions#syntastic#enabled = 1
let g:airline#extensions#tabline#enabled = 1
let g:airline#extensions#tagbar#enabled = 0
let g:airline#extensions#tmuxline#enabled = 0
let g:airline#extensions#virtualenv#enabled = 1
let g:airline#extensions#whitespace#enabled = 0

" }}}
" Bbye {{{

nnoremap <Leader>q :Bdelete<CR>

" }}}
" Clam {{{

nnoremap ! :Clam<space>
vnoremap ! :ClamVisual<space>
let g:clam_autoreturn = 1
" let g:clam_debug = 1

" }}}
" Commentary {{{

nmap <leader>c <Plug>CommentaryLine
xmap <leader>c <Plug>Commentary

augroup plugin_commentary
    au!
    au FileType htmldjango setlocal commentstring={#\ %s\ #}
    au FileType htmljinja setlocal commentstring={#\ %s\ #}
    au FileType clojurescript setlocal commentstring=;\ %s
    au FileType lisp setlocal commentstring=;\ %s
    au FileType puppet setlocal commentstring=#\ %s
    au FileType fish setlocal commentstring=#\ %s
    au FileType cfg setlocal commentstring=#\ %s
    au FileType tmux setlocal commentstring=#\ %s
    au FileType pentadactyl setlocal commentstring=\"\ %s
augroup END

" }}}
" Ctrl-P {{{

let g:ctrlp_reuse_window = 'NERD_tree_2'
let g:ctrlp_working_path_mode = 'ra'
let g:ctrlp_max_height = 20
let g:ctrlp_extensions = ['tag']
let g:ctrlp_switch_buffer = 'et'

let g:ctrlp_map = '<leader>,'
nnoremap <leader>; :CtrlPTag<cr>
nnoremap <leader>b :CtrlPBuffer<cr>
nnoremap <leader>m :CtrlPMRU<cr>

let g:ctrlp_prompt_mappings = {
\ 'PrtSelectMove("j")':   ['<c-j>', '<down>', '<s-tab>'],
\ 'PrtSelectMove("k")':   ['<c-k>', '<up>', '<tab>'],
\ 'PrtHistory(-1)':       ['<c-n>'],
\ 'PrtHistory(1)':        ['<c-p>'],
\ 'ToggleFocus()':        ['<c-tab>'],
\ }

let ctrlp_filter_greps = "".
    \ "egrep -iv '\.(" .
    \ "jar|class|swp|swo|log|so|o|pyc|pyo|jpe?g|png|gif|mo|po|min\.js" .
    \ ")$' | " .
    \ "egrep -v '^(\.\/)?(" .
    \ "node_modules/|lib/|libs/|.git/|.hg/|.svn/|.tox/|dist/|build/|docs/build/|docs/_build/" .
    \ ")'"

let my_ctrlp_user_command = "" .
    \ "find %s '(' -type f -or -type l ')' -maxdepth 15 -not -path '*/\\.*/*' | " .
    \ ctrlp_filter_greps

let my_ctrlp_git_command = "" .
    \ "cd %s && git ls-files --exclude-standard -co | " .
    \ ctrlp_filter_greps

let my_ctrlp_ffind_command = "" .
    \ "ffind --semi-restricted --dir %s --type e -B -f | " .
    \ ctrlp_filter_greps

let g:ctrlp_user_command = ['.git/', my_ctrlp_ffind_command, my_ctrlp_user_command]

" }}}
" DirDiff {{{

" just
nnoremap <Leader>@g <Plug>DirDiffGet
nnoremap <Leader>@p <Plug>DirDiffPut
nnoremap <Leader>@j <Plug>DirDiffNext
nnoremap <Leader>@k <Plug>DirDiffPrev

" }}}
" Dispatch {{{

nnoremap <leader>d :Dispatch
" nnoremap <leader>m :Dispatch<cr>

" }}}
" Fugitive {{{

nnoremap <leader>gd :Gdiff<cr>
nnoremap <leader>gs :Gstatus<cr>
nnoremap <leader>gw :Gwrite<cr>
nnoremap <leader>ga :Git add %<cr>
nnoremap <leader>gb :Gblame<cr>
nnoremap <leader>gco :Gcheckout<cr>
nnoremap <leader>gci :Gcommit --verbose<cr>
nnoremap <leader>gca :Gcommit --all --verbose<cr>
nnoremap <leader>gm :Gmove<cr>
nnoremap <leader>gr :Gremove<cr>
nnoremap <leader>gl :Shell git hist<cr>:wincmd \|<cr>

" Hub
nnoremap <leader>gh :Gbrowse<cr>
vnoremap <leader>gh :Gbrowse<cr>

" }}}
" Git Gutter {{{

let g:gitgutter_escape_grep = 1
" let g:gitgutter_realtime = 0
" let g:gitgutter_max_signs = 500

highlight GitGutterAdd guifg=#d7ffaf ctermfg=193
highlight GitGutterDelete guifg=#cc6666 ctermfg=167
highlight GitGutterChange guifg=#d7d7ff ctermfg=189

" }}}
" Gundo {{{

nnoremap <F5> :GundoToggle<CR>

let g:gundo_debug = 1
let g:gundo_preview_bottom = 1
let g:gundo_tree_statusline = "Gundo"
let g:gundo_preview_statusline = "Gundo Preview"

" }}}
" HTML5 {{{

let g:event_handler_attributes_complete = 0
let g:rdfa_attributes_complete = 0
let g:microdata_attributes_complete = 0
let g:atia_attributes_complete = 0

" }}}
" Linediff {{{

vnoremap <leader>l :Linediff<cr>
nnoremap <leader>L :LinediffReset<cr>

" }}}
" NERD Tree {{{

noremap  <F2> :NERDTreeToggle<cr>
inoremap <F2> <esc>:NERDTreeToggle<cr>

augroup ps_nerdtree
    au!

    au Filetype nerdtree setlocal nolist
    au Filetype nerdtree nnoremap <buffer> H :vertical resize -10<cr>
    au Filetype nerdtree nnoremap <buffer> L :vertical resize +10<cr>
    " au Filetype nerdtree nnoremap <buffer> K :q<cr>
augroup END

let NERDTreeHighlightCursorline = 1
let NERDTreeIgnore = ['\~$', '.*\.pyo$', '.*\.pyc$', 'pip-log\.txt$',
                    \ 'xapian_index', '.*.pid', 'monitor.py', '.*-fixtures-.*.json',
                    \ '.*\.o$', 'db.db', 'tags.bak', '.*\.pdf$', '.*\.mid$',
                    \ '.*\.midi$']

let NERDTreeMinimalUI = 1
let NERDTreeDirArrows = 1
let NERDChristmasTree = 1
let NERDTreeChDirMode = 2
let NERDTreeMapJumpFirstChild = 'gK'
let NERDTreeShowBookmarks = 1

" }}}
" OrgMode {{{

let g:org_heading_shade_leading_stars = 1
" let g:org_plugins = ['ShowHide', '|', 'Navigator', 'EditStructure', '|', 'Todo', 'Date', 'Misc']

let g:org_todo_keywords = ['TODO', '|', 'DONE']
let g:org_agenda_files = ['~/org/home.org', '~/org/work.org']

" let g:org_debug = 1

" }}}
" Python-Mode {{{

let g:pymode_doc = 1
let g:pymode_doc_key = 'M'
let g:pydoc = 'pydoc'
let g:pymode_syntax = 1
let g:pymode_syntax_all = 1
let g:pymode_syntax_builtin_objs = 1
let g:pymode_syntax_print_as_function = 0
let g:pymode_syntax_space_errors = 0
let g:pymode_run = 1
let g:pymode_run_key = '<leader>r'
let g:pymode_lint = 0
let g:pymode_breakpoint = 0
let g:pymode_utils_whitespaces = 1
let g:pymode_virtualenv = 1
let g:pymode_folding = 0

let g:pymode_options_indent = 0
let g:pymode_options_fold = 0
let g:pymode_options_other = 0
let g:pymode_options = 0

let g:pymode_rope = 0
let g:pymode_rope_completion = 0
let g:pymode_rope_autoimport = 0

" let g:pymode_rope_global_prefix = "<localleader>R"
" let g:pymode_rope_local_prefix = "<localleader>r"
" let g:pymode_rope_auto_project = 1
" let g:pymode_rope_enable_autoimport = 1
" let g:pymode_rope_autoimport_generate = 1
" let g:pymode_rope_autoimport_underlineds = 0
" let g:pymode_rope_codeassist_maxfixes = 10
" let g:pymode_rope_sorted_completions = 1
" let g:pymode_rope_extended_complete = 1
" let g:pymode_rope_autoimport_modules = ["os", "shutil", "datetime"]
" let g:pymode_rope_confirm_saving = 1
" let g:pymode_rope_vim_completion = 1
" let g:pymode_rope_guess_project = 1
" let g:pymode_rope_goto_def_newwin = 0
" let g:pymode_rope_always_show_complete_menu = 0

" }}}
" Scratch {{{

command! ScratchToggle call ScratchToggle()

function! ScratchToggle()
    if exists("w:is_scratch_window")
        unlet w:is_scratch_window
        exec "q"
    else
        exec "normal! :Sscratch\<cr>\<C-W>L"
        let w:is_scratch_window = 1
    endif
endfunction

nnoremap <silent> <leader><tab> :ScratchToggle<cr>

" }}}
" Secure Modelines {{{

let g:secure_modelines_allowed_items = [
            \ "textwidth",     "tw",
            \ "foldmethod",    "fdm",
            \ "filetype",      "ft",
            \ "commentstring", "cms",
            \ ]

" }}}
" Signify {{{

" let g:signify_vcs_list = [ 'git', 'hg' ]
" let g:signify_disable_by_default = 0
" let g:signify_skip_filetype = { 'vim': 1, 'c': 1 }
" let g:signify_skip_filename = { '/home/user/.vimrc': 1 }

"}}}
" Sparkup {{{

augroup ft_sparkup
    au!
    au FileType xml,html,htmljinja,htmldjango imap <buffer> <c-e> <Plug>SparkupExecute
    au FileType xml,html,htmljinja,htmldjango imap <buffer> <c-l> <Plug>SparkupNext
augroup END

" let g:sparkupNextMapping = '<c-s>'

"}}}
" Syntastic {{{

let g:syntastic_aggregate_errors = 1
let g:syntastic_always_populate_loc_list = 1
let g:syntastic_check_on_open = 1

let g:syntastic_enable_signs = 1
let g:syntastic_error_symbol = '✗'
let g:syntastic_warning_symbol = '⚠'

" let g:syntastic_java_checker = 'javac'
let g:syntastic_mode_map = {
            \ "mode": "active",
            \ "active_filetypes": [],
            \ "passive_filetypes": ['java', 'html', 'rst', 'scala']
            \ }
let g:syntastic_stl_format = '[%E{%e Errors}%B{, }%W{%w Warnings}]'
let g:syntastic_jsl_conf = '$HOME/.vim/jsl.conf'

" let g:syntastic_python_flake8_quiet_messages
let g:syntastic_python_python_exec = 'python2'
let g:syntastic_python_flake8_exec = 'flake8-python2'

" nnoremap <leader>C :SyntasticCheck<cr>

" }}}
" Splice {{{

let g:splice_prefix = "-"

let g:splice_initial_mode = "grid"

let g:splice_initial_layout_grid = 0
let g:splice_initial_layout_loupe = 0
let g:splice_initial_layout_compare = 0
let g:splice_initial_layout_path = 0

let g:splice_initial_diff_grid = 1
let g:splice_initial_diff_loupe = 0
let g:splice_initial_diff_compare = 1
let g:splice_initial_diff_path = 0

let g:splice_initial_scrollbind_grid = 0
let g:splice_initial_scrollbind_loupe = 0
let g:splice_initial_scrollbind_compare = 0
let g:splice_initial_scrollbind_path = 0

let g:splice_wrap = "nowrap"

" }}}
" Tagbar {{{

noremap <F9> :TagbarToggle<CR>

augroup ps_tagbar
    au!

    " open Tagbar also if you open a supported file in an already running Vim
    " autocmd FileType c,cpp,python nested :call tagbar#autoopen(0)
augroup END

let g:tagbar_autofocus = 1
let g:tagbar_compact = 1
let g:tagbar_indent = 1
let g:tagbar_show_visibility = 0
let g:tagbar_zoomwidth = 0
" let g:tagbar_autoshowtag = 1
let g:tagbar_iconchars = ['▸', '▾']

"  }}}
" Tmuxline {{{

  let g:tmuxline_preset = {
      \'a'       : '#H',
      \'b'       : '#S',
      \'c'       : '#W',
      \'win'     : '#I #W',
      \'cwin'    : '#I #W',
      \'x'       : '#(uptime  | cut -d " " -f 1,2,3)',
      \'y'       : '%H:%M',
      \'z'       : '#T'}

" }}}
" Ultisnips {{{

let g:UltiSnipsEditSplit = 'context'
let g:UltiSnipsSnippetsDir = '~/.vim/mysnippets'
let g:UltiSnipsSnippetDirectories = ['UltiSnips', 'mysnippets']
let g:UltiSnipsExpandTrigger="<c-tab>"
let g:UltiSnipsListSnippets="<c-s-tab>"
" let g:UltiSnipsJumpForwardTrigger="<c-j>"
" let g:UltiSnipsJumpBackwardTrigger="<c-k>"

" }}}
" Viewdoc {{{

let g:viewdoc_open = "belowright vnew"
let g:viewdoc_pydoc_cmd ="/usr/bin/pydoc2"
" let g:viewdoc_man_cmd='LANG=en_US.UTF-8 /usr/bin/man'

" }}}
" Vimux {{{

nnoremap <Leader>rp :VimuxPromptCommand<CR>
nnoremap <Leader>rr :VimuxRunLastCommand<CR>
" map <Leader>vm :VimuxPromptCommand("make ")<CR>

" Run the current file with nose
nnoremap <Leader>rn :call VimuxRunCommand("clear; nosetests " . bufname("%"))<CR>
" Run command without sending a return
" map <Leader>rq :call VimuxRunCommand("clear; nosetests " . bufname("%"), 0)<CR>

" }}}
" YankRing {{{

function! YRRunAfterMaps()
    " Make Y yank to end of line.
    nnoremap Y :<C-U>YRYankCount 'y$'<CR>

    " Fix L and H in operator-pending mode, so yH and such works.
    omap <expr> L YRMapsExpression("", "$")
    omap <expr> H YRMapsExpression("", "^")

    " Don't clobber the yank register when pasting over text in visual mode.
    vnoremap p :<c-u>YRPaste 'p', 'v'<cr>gv:YRYankRange 'v'<cr>
endfunction

nnoremap <silent> <F11> :YRShow<CR>

" }}}
" YouCompleteMe {{{

let g:ycm_add_preview_to_completeopt = 1
let g:ycm_autoclose_preview_window_after_insertion = 1
let g:ycm_autoclose_preview_window_after_completion = 1

let g:ycm_collect_identifiers_from_tags_files = 1
let g:ycm_complete_in_comments = 1
let g:ycm_confirm_extra_conf = 1
" let g:ycm_min_num_of_chars_for_completion = 4
let g:ycm_seed_identifiers_with_syntax = 1

" remove '<S-TAB>' to avoid conflict with ultisnips
let g:ycm_key_list_previous_completion=['<Up>']

nnoremap <leader>] :YcmCompleter GoToDefinitionElseDeclaration<CR>mzzMzvzz15<c-e>`z:Pulse<cr>
" nnoremap <leader>] :YcmCompleter GoToDeclaration<CR>mzzMzvzz15<c-e>`z:Pulse<cr>

" }}}

" }}}
" Text objects ------------------------------------------------------------ {{{

" Shortcut for [] {{{

" onoremap ir i[
" onoremap ar a[
" vnoremap ir i[
" vnoremap ar a[

" }}}

" }}}
" Mini-plugins ------------------------------------------------------------ {{{
" Stuff that should probably be broken out into plugins, but hasn't proved to be
" worth the time to do so just yet.

" Synstack {{{

" Show the stack of syntax hilighting classes affecting whatever is under the
" cursor.
function! SynStack()
  echo join(map(synstack(line('.'), col('.')), 'synIDattr(v:val, "name")'), " > ")
endfunc

nnoremap <F7> :call SynStack()<CR>

" }}}
" Diffwhite Toggle {{{

set diffopt-=iwhite
let g:diffwhitespaceon = 0
function! ToggleDiffWhitespace()
    if g:diffwhitespaceon
        set diffopt-=iwhite
        let g:diffwhitespaceon = 0
    else
        set diffopt+=iwhite
        let g:diffwhitespaceon = 1
    endif
    diffupdate
endfunc

" TODO: Figure out the diffexpr shit necessary to make this buffer-local.
" nnoremap <leader>W :call ToggleDiffWhitespace()<CR>

" }}}
" Error Toggles {{{

command! ErrorsToggle call ErrorsToggle()
function! ErrorsToggle() " {{{
  if exists("w:is_error_window")
    unlet w:is_error_window
    lclose
  else
    exec "Errors"
    lopen
    let w:is_error_window = 1
  endif
endfunction " }}}

command! -bang -nargs=? QFixToggle call QFixToggle(<bang>0)
function! QFixToggle(forced) " {{{
  if exists("g:qfix_win") && a:forced == 0
    cclose
    unlet g:qfix_win
  else
    copen 10
    let g:qfix_win = bufnr("$")
  endif
endfunction " }}}

nmap <silent> <F3> :ErrorsToggle<cr>
nmap <silent> <F4> :QFixToggle<cr>

" }}}
" Hg {{{

" function! s:HgDiff() " {{{
"     diffthis

"     let fn = expand('%:p')
"     let ft = &ft

"     wincmd v
"     edit __hgdiff_orig__

"     setlocal buftype=nofile

"     normal ggdG
"     execute "silent r!hg cat --rev . " . fn
"     normal ggdd

"     execute "setlocal ft=" . ft

"     diffthis
"     diffupdate
" endfunction " }}}
" command! -nargs=0 HgDiff call s:HgDiff()
" " nnoremap <leader>hd :HgDiff<cr>

" function! s:HgBlame() " {{{
"     let fn = expand('%:p')

"     wincmd v
"     wincmd h
"     edit __hgblame__
"     vertical resize 28

"     setlocal scrollbind winfixwidth nolist nowrap nonumber buftype=nofile ft=none

"     normal ggdG
"     execute "silent r!hg blame -undq " . fn
"     normal ggdd
"     execute ':%s/\v:.*$//'

"     wincmd l
"     setlocal scrollbind
"     syncbind
" endfunction " }}}
" command! -nargs=0 HgBlame call s:HgBlame()
" " nnoremap <leader>hb :HgBlame<cr>

" }}}
" Ack motions {{{

" Motions to Ack for things.  Works with pretty much everything, including:
"
"   w, W, e, E, b, B, t*, f*, i*, a*, and custom text objects
"
" Awesome.
"
" Note: If the text covered by a motion contains a newline it won't work.  Ack
" searches line-by-line.

nnoremap <silent> <leader>A :set opfunc=<SID>AckMotion<CR>g@
xnoremap <silent> <leader>A :<C-U>call <SID>AckMotion(visualmode())<CR>

nnoremap <bs> :Ack! '\b<c-r><c-w>\b'<cr>
xnoremap <silent> <bs> :<C-U>call <SID>AckMotion(visualmode())<CR>

function! s:CopyMotionForType(type)
    if a:type ==# 'v'
        silent execute "normal! `<" . a:type . "`>y"
    elseif a:type ==# 'char'
        silent execute "normal! `[v`]y"
    endif
endfunction

function! s:AckMotion(type) abort
    let reg_save = @@

    call s:CopyMotionForType(a:type)

    execute "normal! :Ack! --literal " . shellescape(@@) . "\<cr>"

    let @@ = reg_save
endfunction

" }}}
" Indent Guides {{{

let g:indentguides_state = 0
function! IndentGuides() " {{{
    if g:indentguides_state
        let g:indentguides_state = 0
        2match None
    else
        let g:indentguides_state = 1
        execute '2match IndentGuides /\%(\_^\s*\)\@<=\%(\%'.(0*&sw+1).'v\|\%'.(1*&sw+1).'v\|\%'.(2*&sw+1).'v\|\%'.(3*&sw+1).'v\|\%'.(4*&sw+1).'v\|\%'.(5*&sw+1).'v\|\%'.(6*&sw+1).'v\|\%'.(7*&sw+1).'v\)\s/'
    endif
endfunction " }}}
hi def IndentGuides guibg=#303030 ctermbg=234
nnoremap <leader>I :call IndentGuides()<cr>

" }}}
" Pulse Line {{{

function! s:Pulse() " {{{
    redir => old_hi
        silent execute 'hi CursorLine'
    redir END
    let old_hi = split(old_hi, '\n')[0]
    let old_hi = substitute(old_hi, 'xxx', '', '')

    let steps = 8
    let width = 1
    let start = width
    let end = steps * width
    let color = 233

    for i in range(start, end, width)
        execute "hi CursorLine ctermbg=" . (color + i)
        redraw
        sleep 6m
    endfor
    for i in range(end, start, -1 * width)
        execute "hi CursorLine ctermbg=" . (color + i)
        redraw
        sleep 6m
    endfor

    execute 'hi ' . old_hi
endfunction " }}}
command! -nargs=0 Pulse call s:Pulse()

" }}}
" Highlight Word {{{
"
" This mini-plugin provides a few mappings for highlighting words temporarily.
"
" Sometimes you're looking at a hairy piece of code and would like a certain
" word or two to stand out temporarily.  You can search for it, but that only
" gives you one color of highlighting.  Now you can use <leader>N where N is
" a number from 1-6 to highlight the current word in a specific color.

function! HiInterestingWord(n) " {{{
    " Save our location.
    normal! mz

    " Yank the current word into the z register.
    normal! "zyiw

    " Calculate an arbitrary match ID.  Hopefully nothing else is using it.
    let mid = 86750 + a:n

    " Clear existing matches, but don't worry if they don't exist.
    silent! call matchdelete(mid)

    " Construct a literal pattern that has to match at boundaries.
    let pat = '\V\<' . escape(@z, '\') . '\>'

    " Actually match the words.
    call matchadd("InterestingWord" . a:n, pat, 1, mid)

    " Move back to our original location.
    normal! `z
endfunction " }}}

" Mappings {{{

nnoremap <silent> <leader>1 :call HiInterestingWord(1)<cr>
nnoremap <silent> <leader>2 :call HiInterestingWord(2)<cr>
nnoremap <silent> <leader>3 :call HiInterestingWord(3)<cr>
nnoremap <silent> <leader>4 :call HiInterestingWord(4)<cr>
nnoremap <silent> <leader>5 :call HiInterestingWord(5)<cr>
nnoremap <silent> <leader>6 :call HiInterestingWord(6)<cr>

" }}}
" Default Highlights {{{

hi def InterestingWord1 guifg=#000000 ctermfg=16 guibg=#ffa724 ctermbg=214
hi def InterestingWord2 guifg=#000000 ctermfg=16 guibg=#aeee00 ctermbg=154
hi def InterestingWord3 guifg=#000000 ctermfg=16 guibg=#8cffba ctermbg=121
hi def InterestingWord4 guifg=#000000 ctermfg=16 guibg=#b88853 ctermbg=137
hi def InterestingWord5 guifg=#000000 ctermfg=16 guibg=#ff9eb8 ctermbg=211
hi def InterestingWord6 guifg=#000000 ctermfg=16 guibg=#ff2c4b ctermbg=195

" }}}

" }}}
" DiffOrig {{{

" Convenient command to see the difference between the current buffer and the
" file it was loaded from, thus the changes you made.
" Only define it when not defined already.
if !exists(":DiffOrig")
  command DiffOrig vert new | set bt=nofile | r # | 0d_ | diffthis
                  \ | wincmd p | diffthis
endif

" }}}
" Spelling {{{

if has("spell")
    setlocal spell spelllang=
    nnoremap <leader>sp :setlocal spell spelllang=fr,en<cr>
    nnoremap <leader>sf :setlocal spell spelllang=fr<cr>
    nnoremap <leader>se :setlocal spell spelllang=en<cr>
    nnoremap <leader>sn :setlocal spell spelllang=<cr>
endif

"let loaded_vimspell = 1
"set spellsuggest=10
"let spell_executable = "aspell"
"let spell_auto_type = ''
"let spell_insert_mode = 0

" }}}
" Menubar Toggle {{{

set go-=m
let g:menubar_on = 0
function! ToggleMenuBar()
    if g:menubar_on
        set go-=m
        let g:menubar_on = 0
    else
        set go+=m
        let g:menubar_on = 1
    endif
    diffupdate
endfunc

nnoremap <F10> :call ToggleMenuBar()<CR>

" }}}

" }}}
" Environments (GUI/Console) ---------------------------------------------- {{{

if has('gui_running')
    " GUI Vim

    if has("win32")
        set guifont=Courier:h10:cANSI
    else
        if hostname() == "thunderball"
            set guifont=Inconsolata\ for\ Powerline\ 12
        elseif hostname() == "fireball"
            set guifont=Inconsolata\ for\ Powerline\ 10
        else
            set guifont=Inconsolata\ for\ Powerline\ 10
        endif
    endif

    " Remove all the UI cruft
    set go-=T   " No toolbar
    set go-=l   " No scrollbars
    set go-=L
    set go-=r
    set go-=R
    set go-=m   " No menubar
    set go-=e   " non-GUI tab pages line
    " set go+=a    " clipboard to autoselect
    " set go+=c    " Use console dialogs instead of popup

    set mousefocus                " Le focus suit la souris
    " set mousemodel=popup_setpos   " Le bouton droit affiche une popup

    highlight SpellBad term=underline gui=undercurl guisp=Orange

    " Different cursors for different modes.
    " set guicursor=n-c:block-Cursor-blinkon0
    " set guicursor+=v:block-vCursor-blinkon0
    " set guicursor+=i-ci:ver20-iCursor

    " Disable all blinking:
    set guicursor+=a:blinkon0

    " Make shift-insert work like in Xterm
    map <S-Insert> <MiddleMouse>
    map! <S-Insert> <MiddleMouse>
else
    " Console Vim

    " Mouse support
    set mouse=a

    "set term=xterm
    set t_Co=256
    " colorscheme molokai
    "set t_Co=8
    "set termencoding=utf-8
    "set ttymouse=xterm
endif

" UNIX Specials
" if has("unix")
"    " path: répertoires utilisés lors d'une recherche et autres commandes
"    set path=.,/usr/include,usr/local/include
" endif

" }}}
" Status line ------------------------------------------------------------- {{{

" augroup ft_statuslinecolor
"     au!

"     au InsertEnter * hi StatusLine ctermfg=196 guifg=#FF3145
"     au InsertLeave * hi StatusLine ctermfg=130 guifg=#CD5907
" augroup END

" }}}
" Local config ------------------------------------------------------------ {{{

if filereadable(expand("~/.vim/local.vim"))
    source ~/.vim/local.vim
endif

" }}}


" .vimrc
" Based on Steve Losh's vimrc :
" http://bitbucket.org/sjl/dotfiles/src/tip/vim/

" Plugins ----------------------------------------------------------------- {{{

call plug#begin('~/.vim/plugged')

" Plug 'AndrewRadev/linediff.vim', { 'on': ['Linediff', 'LinediffReset'] }
Plug 'airblade/vim-gitgutter'
Plug 'ap/vim-css-color'
Plug 'benmills/vimux'
Plug 'christoomey/vim-tmux-navigator'
Plug 'ciaranm/securemodelines'
" Plug 'ctrlpvim/ctrlp.vim'
Plug 'derekwyatt/vim-fswitch'
Plug 'edkolev/tmuxline.vim', { 'on': ['Tmuxline', 'TmuxlineSnapshot'] }
Plug 'exu/pgsql.vim', { 'for': 'pgsql' }
Plug 'fatih/vim-go', { 'do': ':GoUpdateBinaries' }
Plug 'freitass/todo.txt-vim'
Plug 'godlygeek/tabular', { 'on': 'Tabularize' }
Plug 'gregsexton/MatchTag', { 'for': ['html', 'xml'] }
Plug 'groenewege/vim-less', { 'for': 'less' }
Plug 'honza/vim-snippets'
" Plug 'jceb/vim-orgmode', { 'for': 'org' }
Plug 'jpalardy/vim-slime'
" Plug 'junegunn/vim-github-dashboard', { 'on': ['GHDashboard', 'GHActivity'] }
Plug 'junegunn/fzf', { 'dir': '~/.fzf', 'do': './install --all' }
Plug 'junegunn/fzf.vim'
Plug 'junegunn/goyo.vim'
Plug 'lervag/vimtex', { 'for': 'tex' }
Plug 'majutsushi/tagbar', { 'on': 'TagbarToggle' }
Plug 'maralla/completor.vim'
" Plug 'mhinz/vim-signify'
Plug 'mhinz/vim-startify'
" Plug 'michaeljsmith/vim-indent-object'
Plug 'mileszs/ack.vim'
Plug 'mitsuhiko/vim-jinja', { 'for': ['htmljinja', 'sls'] }
Plug 'mitsuhiko/vim-sparkup', { 'on': ['SparkupExecute', 'SparkupNext'] }
Plug 'moll/vim-bbye', { 'on': 'Bdelete' }
Plug 'mtth/scratch.vim', { 'on': 'Scratch' }
" Plug 'mxw/vim-jsx', { 'for': 'javascript' }
Plug 'othree/html5.vim'
Plug 'pangloss/vim-javascript', { 'for': 'javascript' }
Plug 'pearofducks/ansible-vim'
Plug 'plasticboy/vim-markdown', { 'for': 'markdown' }
Plug 'saltstack/salt-vim', { 'for': 'sls' }
Plug 'scrooloose/nerdtree', { 'on': 'NERDTreeToggle' }
" Plug 'scrooloose/syntastic'
Plug 'shime/vim-livedown', { 'for': 'markdown', 'do': 'npm install livedown' }
Plug 'SirVer/ultisnips'
" Plug 'sjl/AnsiEsc.vim'
Plug 'sjl/badwolf'
Plug 'sjl/clam.vim', { 'on': ['Clam', 'ClamVisual'] }
Plug 'sjl/gundo.vim', { 'on': 'GundoToggle' }
" Plug 'sjl/splice.vim', { 'on': 'SpliceInit' }
" Plug 'smancill/conky-syntax.vim', { 'for': 'conkyrc' }
" Plug 'stefandtw/quickfix-reflector.vim'
Plug 'ternjs/tern_for_vim', { 'for': 'javascript', 'do': 'npm install' }
Plug 'terryma/vim-multiple-cursors'
Plug 'tmux-plugins/vim-tmux-focus-events'
Plug 'tpope/vim-commentary'
Plug 'tpope/vim-dispatch'
Plug 'tpope/vim-fugitive'
Plug 'tpope/vim-repeat'
Plug 'tpope/vim-rhubarb'
" Plug 'tpope/vim-speeddating'
Plug 'tpope/vim-surround'
Plug 'tshirtman/vim-cython'
" Plug 'Valloric/YouCompleteMe', { 'do': '/usr/bin/python install.py --clang-completer --js-completer --system-libclang' }
Plug 'vim-airline/vim-airline'
Plug 'vim-airline/vim-airline-themes'
Plug 'vim-scripts/Conflict2Diff', { 'on': 'Conflict2Diff' }
Plug 'vim-scripts/DirDiff.vim', { 'on': 'DirDiff' }
Plug 'vim-scripts/YankRing.vim', { 'on': 'YRShow' }
" Plug 'wavded/vim-stylus', { 'for': 'stylus' }
Plug 'w0ng/vim-hybrid'

" Python
" Plug 'ivanov/vim-ipython'
" Plug 'python-mode/python-mode', { 'for': 'python' }
Plug '5long/pytest-vim-compiler'
Plug 'alfredodeza/pytest.vim'
" Plug 'fs111/pydoc.vim'
Plug 'tell-k/vim-autopep8', { 'for': 'python' }
Plug 'vim-python/python-syntax'
Plug 'Vimjas/vim-python-pep8-indent'
Plug 'w0rp/ale'

" Plug '~/.vim/bundle/camptocamp', { 'for': 'camptocamp' }
Plug '~/.vim/bundle/closetags-custom'
" Plug '~/.vim/bundle/django-custom', { 'for': ['django', 'htmldjango'] }
" Plug '~/.vim/bundle/idlang', { 'for': 'idlang' }

call plug#end()

" }}}
" Preamble ---------------------------------------------------------------- {{{

set shell=/bin/zsh

filetype off
filetype plugin indent on       " load file type plugins + indentation

" }}}
" Basic options ----------------------------------------------------------- {{{

if !has('nvim')
    set nocompatible                " choose no compatibility with legacy vi
    set encoding=utf-8
    set autoindent                  " always set autoindenting on
    set autoread
    set backspace=indent,eol,start  " backspace through everything in insert mode
    set laststatus=2                " show always statusline of last window
    set ttyfast
    set history=10000
    set smarttab
    set wildmenu
else
    let g:python3_host_prog = '/usr/bin/python'
endif

set modelines=0
set showmode                    " print current mode on the last line
set showcmd                     " display incomplete commands
set hidden                      " Allow backgrounding buffers without writing them
set visualbell
set ruler
set nonumber
set norelativenumber
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
set complete=.,w,b,u,t,i
" set completeopt=longest,menuone,preview

" Save when losing focus
au FocusLost * :silent! wall

" Resize splits when the window is resized
au VimResized * :wincmd =

" Leader
let mapleader = ","
let maplocalleader = ";"

" 3: tree style
let g:netrw_liststyle=3

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

" augroup trailing
"     au!

"     " Only shown when not in insert mode so I don't go insane.
"     " au InsertEnter * :set listchars-=trail:⌴

"     " Remove trailing whitespace
"     autocmd BufWritePre * :%s/\s\+$//e
" augroup END

" }}}
" Wildmenu completion {{{

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

set textwidth=80
set formatoptions=qrn1tcj
" q : Allow formatting of comments with "gq".
" r : Automatically insert the current comment leader
" n : recognize numbered lists
" 1 : Don't break a line after a one-letter word
" a : Automatic formatting of paragraphs
" t : Auto-wrap text using textwidth
" c : Auto-wrap comments using textwidth
" j : Delete comment char when joining commented lines

nnoremap <leader>w :call AutoWrapToggle()<CR>
function! AutoWrapToggle()
  if &formatoptions =~ 't'
    set fo-=tc
  else
    set fo+=tc
  endif
endfunction

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

" let g:hybrid_custom_term_colors = 1
" let g:hybrid_reduced_contrast = 1
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
noremap <leader>p :set paste<CR>"*P<CR>:set nopaste<CR>
noremap <leader>P :set paste<CR>"+P<CR>:set nopaste<CR>

" make p in Visual mode replace the selected text with the yank register
vnoremap p <Esc>:let current_reg = @"<CR>gvdi<C-R>=current_reg<CR><Esc>

" Rebuild Ctags (mnemonic RC -> CR -> <cr>)
nnoremap <leader><cr> :silent !myctags<cr>:redraw!<cr>

" Clean trailing whitespace
" nnoremap <leader>w mz:%s/\s\+$//<cr>:let @/=''<cr>`z
" Clean leading spaces
" nmap _S :%s/^\s\+//<CR>

" Send visual selection to paste.stevelosh.com
" vnoremap <c-p> :w !curl -sF 'sprunge=<-' 'http://paste.stevelosh.com' \| tr -d '\n ' \| pbcopy && open `pbpaste`<cr>

" Select entire buffer
" nnoremap vaa ggvGg_
" nnoremap Vaa ggVG

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

" Quote words under cursor
nnoremap <leader>" viW<esc>a"<esc>gvo<esc>i"<esc>gvo<esc>3l
nnoremap <leader>' viW<esc>a'<esc>gvo<esc>i'<esc>gvo<esc>3l

" Quote current selection
" TODO: This only works for selections that are created "forwardly"
vnoremap <leader>" <esc>a"<esc>gvo<esc>i"<esc>gvo<esc>ll
vnoremap <leader>' <esc>a'<esc>gvo<esc>i'<esc>gvo<esc>ll

" Panic Button
nnoremap <F12> mzggg?G`z

" Substitute
nnoremap <leader>: :%s//<left>

" Diffoff
nnoremap <leader>D :diffoff!<cr>

" Formatting, TextMate-style
" nnoremap Q gqip
" vnoremap Q gq
nnoremap Q gwip
vnoremap Q gw

" Reformat line.
" I never use l as a macro register anyway.
nnoremap ql gqq

" Easier linewise reselection of what you just pasted.
" nnoremap <leader>V V`]
nnoremap <leader>V `[v`]

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
" nnoremap S i<cr><esc>^mwgk:silent! s/\v +$//<cr>:noh<cr>`w
nnoremap S i<cr><esc>^mwgk:silent! s/\v +$//<cr>`w

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
" nnoremap <c-n> :bnext!<CR>
" nnoremap <c-p> :bprev!<CR>

" make ; do the same thing as :
" nnoremap ; :
" visual command line
" nnoremap ; :<c-f>

" map CTRL+k S N (non-breaking space) to CTRL+space
" imap <Nul> <C-k>NS
" imap <C-Space> <C-k>NS

" change directory to the file being edited
" nnoremap <leader>C :cd %:p:h<CR>:pwd<CR>

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

nnoremap gz :!zeal "<cword>"&<CR><CR>

" Easy filetype switching {{{

nnoremap _rst :set ft=rst<CR>
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
nnoremap <leader>et :vsplit ~/ownCloud/todo/todo.txt<cr>
nnoremap <leader>ev :vsplit ~/.vimrc<cr>
nnoremap <leader>ez :vsplit ~/lib/dotfiles/zsh<cr>4j

nnoremap <leader>sv :source ~/.vimrc<cr>

" }}}
" Searching and movement -------------------------------------------------- {{{

" Use sane regexes.
nnoremap / /\v
vnoremap / /\v

set ignorecase           " searches are case insensitive...
set smartcase            " ... unless they contain at least one capital letter
set showmatch            " show matching brackets
set gdefault             " substitute all matches on the line

if !has('nvim')
    set hlsearch         " highlight matches
    set incsearch        " incremental searching
endif

set scrolloff=3          " min nb of lines to keep above and below the cursor.
set sidescroll=1         " min nb of columns to scroll horizontally.(with nowrap)
set sidescrolloff=10
set virtualedit+=block   " allow cursor where there is no actual character.

noremap <silent> <leader><space> :noh<cr>:call clearmatches()<cr>

runtime macros/matchit.vim
map <tab> %
" silent! unmap [%
" silent! unmap ]%
"
" Jump to matching pairs easily, with Tab
" nnoremap <Tab> %
" vnoremap <Tab> %

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
    " Pulse
endfunction

function! JumpToTagInSplit()
    execute "normal! \<c-w>v\<c-]>mzzMzvzz15\<c-e>"
    execute "keepjumps normal! `z"
    " Pulse
endfunction

nnoremap <c-]> :silent! call JumpToTag()<cr>
nnoremap <c-\> :silent! call JumpToTagInSplit()<cr>

" nnoremap <c-\> <c-w>v<c-]>mzzMzvzz15<c-e>`z:Pulse<cr>

" Keep search matches in the middle of the window.
" nnoremap n nzzzv:Pulse<cr>
" nnoremap N Nzzzv:Pulse<cr>
nnoremap n nzzzv
nnoremap N Nzzzv

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
" nnoremap <silent> <leader>/ :execute "Ack '" . substitute(substitute(substitute(@/, "\\\\<", "\\\\b", ""), "\\\\>", "\\\\b", ""), "\\\\v", "", "") . "'"<CR>

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
" Terminal ---------------------------------------------------------------- {{{

nnoremap <leader>tv :vertical terminal<cr>
nnoremap <leader>th :terminal<cr>

" }}}
" Filetype-specific ------------------------------------------------------- {{{

" C {{{

let c_no_comment_fold = 1

augroup ft_c
    au!
    au FileType c,cpp setlocal foldmethod=syntax cindent
    au FileType c,cpp setlocal expandtab shiftwidth=2 tabstop=2 softtabstop=2
    au BufRead,BufNewFile *.h,*.c set filetype=c.doxygen
    " au BufEnter *.cpp let b:fswitchdst = 'hpp,h' | let b:fswitchlocs = '../inc'

    au Filetype c nmap <buffer> <localleader>h :FSHere<cr>
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
" Go {{{

" let g:go_auto_sameids = 1
" let g:go_auto_type_info = 1
let g:go_fmt_command = "goimports"
" let g:go_doc_command = ["go doc"]

let g:go_highlight_extra_types = 1
let g:go_highlight_fields = 1
let g:go_highlight_function_calls = 1
let g:go_highlight_functions = 1
let g:go_highlight_operators = 1
let g:go_highlight_types = 1

augroup ft_go
    au!

    autocmd BufNewFile,BufRead *.go setlocal noexpandtab tabstop=4 shiftwidth=4
    autocmd FileType go nmap <buffer> <localleader>b  <Plug>(go-build)
    autocmd FileType go nmap <buffer> <localleader>i  <Plug>(go-install)
    autocmd FileType go nmap <buffer> <localleader>r  <Plug>(go-run)
    autocmd FileType go nmap <buffer> <localleader>t  <Plug>(go-test)
    autocmd FileType go nmap <buffer> <localleader>c  <Plug>(go-coverage-toggle)
augroup END

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
    " au FileType html,htmljinja,htmldjango nnoremap <buffer> p :<C-U>YRPaste 'p'<CR>v`]=`]
    " au FileType html,htmljinja,htmldjango nnoremap <buffer> P :<C-U>YRPaste 'P'<CR>v`]=`]
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

" augroup ft_java
"     au!

"     au FileType java setlocal foldmethod=marker
"     au FileType java setlocal foldmarker={,}
" augroup END

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

let g:vim_markdown_new_list_item_indent = 2
let g:vim_markdown_toc_autofit = 1

let g:livedown_browser = "chromium"

augroup ft_markdown
    au!

    au BufRead,BufNewFile ~/ownCloud/Notes/*.txt setlocal filetype=markdown
    " au BufNewFile,BufRead *.m*down setlocal filetype=markdown
    " au BufNewFile,BufRead *.md setfiletype markdown
    " au BufNewFile,BufRead *.mkd setfiletype markdown

    au FileType markdown setlocal foldlevel=1

    " Use <localleader>1/2/3 to add headings.
    au Filetype markdown nnoremap <buffer> <localleader>1 yypVr=:redraw<cr>
    au Filetype markdown nnoremap <buffer> <localleader>2 yypVr-:redraw<cr>
    au Filetype markdown nnoremap <buffer> <localleader>3 mzI###<space><esc>`zllll
    au Filetype markdown nnoremap <buffer> <localleader>4 mzI####<space><esc>`zlllll

    au Filetype markdown nnoremap <buffer> <localleader>v :LivedownToggle<CR>

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

" augroup ft_pentadactyl
"     au!
"     au BufNewFile,BufRead *pentadactylrc set filetype=pentadactyl
"     au BufNewFile,BufRead /tmp/**/pentadactyl.txt set nolist wrap linebreak columns=100 colorcolumn=0
" augroup END

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

" vim-python/python-syntax conf
let g:python_highlight_all = 1

" let g:pydoc_open_cmd = 'vsplit'
" let g:pydoc_use_drop=1

augroup ft_python
    au!
    " add header for new files
    " au BufNewFile *.py 0put=\"# -*- coding: utf-8 -*-\<nl>\<nl>\"|$
    " au BufNewFile *.py 0put=\"#!/usr/bin/env python2\"|1put=\"# -*- coding: utf-8 -*-\<nl>\<nl>\"|$

    " make Python follow PEP8 ( http://www.python.org/dev/peps/pep-0008/ )
    au FileType python set softtabstop=4 tabstop=4 shiftwidth=4 textwidth=79
    au FileType python setlocal smartindent cinwords=if,elif,else,for,while,try,except,finally,def,class,with

    " disable autowrapping
    " autocmd filetype python setlocal formatoptions-=t

    " au FileType python setlocal omnifunc=pythoncomplete#Complete
    au FileType python setlocal define=^\s*\\(def\\\\|class\\)
    au FileType man nnoremap <buffer> <cr> :q<cr>

    au FileType python compiler pytest
    " au FileType python let b:dispatch = 'py.test'

    " Jesus tapdancing Christ, built-in Python syntax, you couldn't let me
    " override this in a normal way, could you?
    au FileType python if exists("python_space_error_highlight") | unlet python_space_error_highlight | endif

    au FileType python inoremap <buffer> <c-b> """"""<left><left><left>
    " au FileType python iabbrev <buffer> afo assert False, "Okay"

    " Autopep8
    let g:autopep8_select="E1,E2,E3,E4,W2,W3"
    au FileType python map <buffer> <localleader>p :call Autopep8()<CR>

    " au FileType python map <buffer> <localleader>y :0,$!yapf<CR>
    " au FileType python nnoremap <leader>y :0,$!yapf<Cr>

    " Defer to isort for sorting Python imports (instead of using Unix sort)
    au filetype python nmap <buffer> <localleader>s :ALEFix isort<CR>
    au filetype python nmap <buffer> <localleader>y :ALEFix yapf<CR>
    " if executable('isort')
    "     autocmd filetype python nnoremap <leader>s mX:%! isort -<cr>`X
    " endif
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

    au Filetype rst nnoremap <buffer> <localleader>1 yyPVr=jyypVr=
    au Filetype rst nnoremap <buffer> <localleader>2 yyPVr*jyypVr*
    " au Filetype rst nnoremap <buffer> <localleader>1 yypVr=:redraw<cr>
    " au Filetype rst nnoremap <buffer> <localleader>2 yypVr-:redraw<cr>
    au Filetype rst nnoremap <buffer> <localleader>3 yypVr~:redraw<cr>
    au Filetype rst nnoremap <buffer> <localleader>4 yypVr`:redraw<cr>
augroup END

" }}}
" Ruby {{{

" augroup ft_ruby
"     au!
"     au Filetype ruby setlocal foldmethod=syntax
"     au BufRead,BufNewFile Capfile setlocal filetype=ruby
" augroup END

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

" augroup ft_vagrant
"     au!
"     au BufRead,BufNewFile Vagrantfile set ft=ruby
" augroup END

" }}}
" Vim {{{

augroup ft_vim
    au!

    au FileType vim setlocal foldmethod=marker keywordprg=:help
    au FileType help setlocal textwidth=78
    au BufWinEnter *.txt if &ft == 'help' | wincmd L | endif

    " Bind <F1> to show the keyword under cursor
    " general help can still be entered manually, with :h
    autocmd filetype vim noremap <buffer> <F1> <Esc>:help <C-r><C-w><CR>
    autocmd filetype vim noremap! <buffer> <F1> <Esc>:help <C-r><C-w><CR>

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

if executable('rg')
    let g:ackprg = 'rg -S --no-heading --vimgrep'
elseif executable('ag')
    let g:ackprg = 'ag --vimgrep'
endif

nnoremap <leader>a :Ack<space>
nnoremap <leader>A :Ack <C-r><C-w><CR>
vnoremap <leader>a y:grep! "\b<c-r>"\b"<cr>:cw<cr><cr>

" }}}
" Ale {{{

let g:ale_sign_warning = '▲'
let g:ale_sign_error = '✗'
let g:ale_linters = {
  \   'python': ['flake8'],
  \}
let g:ale_fixers = {
  \   '*': ['remove_trailing_lines', 'trim_whitespace'],
  \}

" \   'python': ['isort'],
let g:ale_fix_on_save = 1
nmap <leader>f <Plug>(ale_fix)

" Fixers:
" 'remove_trailing_lines' - Remove all blank lines at the end of a file.
" 'trim_whitespace' - Remove all trailing whitespace characters at the end of every line.

" highlight link ALEWarningSign String
" highlight link ALEErrorSign

" }}}
" Airline {{{

let g:airline_powerline_fonts = 0
" let g:airline_section_z = airline#section#create_right(['%2p%% %3l:%1c'])
let g:airline_theme='badwolf'

let g:airline#extensions#branch#enabled = 1
let g:airline#extensions#ale#enabled = 1
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
" Completor {{{

" Use Tab to select completion
inoremap <expr> <Tab> pumvisible() ? "\<C-n>" : "\<Tab>"
inoremap <expr> <S-Tab> pumvisible() ? "\<C-p>" : "\<S-Tab>"
inoremap <expr> <cr> pumvisible() ? "\<C-y>\<cr>" : "\<cr>"

" let g:completor_complete_options = 'menuone,noselect,preview'
" set completeopt=longest,menuone,preview

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

" let g:ctrlp_reuse_window = 'NERD_tree_2'
" let g:ctrlp_working_path_mode = 'ra'
" let g:ctrlp_max_height = 20
" let g:ctrlp_extensions = ['tag']
" let g:ctrlp_switch_buffer = 'et'

" let g:ctrlp_map = '<leader>,'
" nnoremap <leader>; :CtrlPTag<cr>
" nnoremap <leader>b :CtrlPBuffer<cr>
" nnoremap <leader>m :CtrlPMRU<cr>

" " let g:ctrlp_prompt_mappings = {
" " \ 'PrtSelectMove("j")':   ['<c-j>', '<down>', '<s-tab>'],
" " \ 'PrtSelectMove("k")':   ['<c-k>', '<up>', '<tab>'],
" " \ 'PrtHistory(-1)':       ['<c-n>'],
" " \ 'PrtHistory(1)':        ['<c-p>'],
" " \ 'ToggleFocus()':        ['<c-tab>'],
" " \ }

" " let ctrlp_filter_greps = "".
" "     \ "egrep -iv '\.(" .
" "     \ "jar|class|swp|swo|log|so|o|pyc|pyo|jpe?g|png|gif|mo|po|min\.js" .
" "     \ ")$' | " .
" "     \ "egrep -v '^(\.\/)?(" .
" "     \ "node_modules/|lib/|libs/|.git/|.hg/|.svn/|.tox/|dist/|build/|docs/build/|docs/_build/" .
" "     \ ")'"

" " let my_ctrlp_user_command = "" .
" "     \ "find %s '(' -type f -or -type l ')' -maxdepth 15 -not -path '*/\\.*/*' | " .
" "     \ ctrlp_filter_greps

" " let my_ctrlp_git_command = "" .
" "     \ "cd %s && git ls-files --exclude-standard -co | " .
" "     \ ctrlp_filter_greps

" " let g:ctrlp_user_command = ['.git/', my_ctrlp_git_command, my_ctrlp_user_command]

" " Use The Silver Searcher over grep, iff possible
" if executable('ag')
"    " Use ag over grep
"    " set grepprg=ag\ --nogroup\ --nocolor

"    " Use ag in CtrlP for listing files. Lightning fast and respects .gitignore
"    let g:ctrlp_user_command = 'ag %s -l --nocolor --hidden -g ""'

"    " ag is fast enough that CtrlP doesn't need to cache
"    let g:ctrlp_use_caching = 0
" endif

" }}}
" DirDiff {{{

" just
" nnoremap <Leader>@g <Plug>DirDiffGet
" nnoremap <Leader>@p <Plug>DirDiffPut
" nnoremap <Leader>@j <Plug>DirDiffNext
" nnoremap <Leader>@k <Plug>DirDiffPrev

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
nnoremap <leader>gl :Clam git hist<cr>:wincmd \|<cr>

" Hub
nnoremap <leader>gh :Gbrowse<cr>
vnoremap <leader>gh :Gbrowse<cr>

" }}}
" FZF {{{

nnoremap <leader>, :Files<CR>
nnoremap <leader>b :Buffers<CR>
nnoremap <leader>; :Tags<CR>
nnoremap <leader>m :History<CR>
nnoremap <leader>C :Commits<CR>

let g:fzf_commits_log_options = '--graph --color=always'
let g:fzf_tags_command = 'ctags -R'

" Customize fzf colors to match your color scheme
let g:fzf_colors =
\ { 'fg':      ['fg', 'Normal'],
  \ 'bg':      ['bg', 'Normal'],
  \ 'hl':      ['fg', 'Comment'],
  \ 'fg+':     ['fg', 'CursorLine', 'CursorColumn', 'Normal'],
  \ 'bg+':     ['bg', 'CursorLine', 'CursorColumn'],
  \ 'hl+':     ['fg', 'Statement'],
  \ 'info':    ['fg', 'PreProc'],
  \ 'border':  ['fg', 'Ignore'],
  \ 'prompt':  ['fg', 'Conditional'],
  \ 'pointer': ['fg', 'Exception'],
  \ 'marker':  ['fg', 'Keyword'],
  \ 'spinner': ['fg', 'Label'],
  \ 'header':  ['fg', 'Comment'] }

" }}}
" Git Gutter {{{

let g:gitgutter_escape_grep = 1
let g:gitgutter_realtime = 0
" let g:gitgutter_max_signs = 500

" GitGutter styling to use · instead of +/-
let g:gitgutter_sign_added = '∙'
let g:gitgutter_sign_modified = '∙'
let g:gitgutter_sign_removed = '∙'
let g:gitgutter_sign_modified_removed = '∙'

highlight GitGutterAdd guifg=#d7ffaf ctermfg=193
highlight GitGutterDelete guifg=#cc6666 ctermfg=167
highlight GitGutterChange guifg=#d7d7ff ctermfg=189

" }}}
" Gundo {{{

nnoremap <F5> :GundoToggle<CR>

" let g:gundo_debug = 1
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

let NERDTreeBookmarksFile = expand("$HOME/.vim/NERDTreeBookmarks")
let NERDTreeHighlightCursorline = 1
let NERDTreeIgnore = ['\~$', '.*\.pyo$', '.*\.pyc$', 'pip-log\.txt$',
                    \ '.*\.o$', 'db.db', 'tags.bak', '.*\.pdf$']

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
let g:org_agenda_files = ['~/org/*.org']

" let g:org_debug = 1

" }}}
" Python-Mode {{{

" let g:pymode_doc = 1
" let g:pymode_doc_key = 'M'
" let g:pydoc = 'pydoc'
" let g:pymode_syntax = 1
" let g:pymode_syntax_all = 1
" let g:pymode_syntax_builtin_objs = 1
" let g:pymode_syntax_print_as_function = 0
" let g:pymode_syntax_space_errors = 0
" let g:pymode_run = 1
" let g:pymode_run_key = '<leader>r'
" let g:pymode_lint = 0
" let g:pymode_breakpoint = 0
" let g:pymode_utils_whitespaces = 1
" let g:pymode_virtualenv = 1
" let g:pymode_folding = 0

" let g:pymode_options_indent = 0
" let g:pymode_options_fold = 0
" let g:pymode_options_other = 0
" let g:pymode_options = 0

" let g:pymode_rope = 0
" let g:pymode_rope_completion = 0
" let g:pymode_rope_autoimport = 0

" }}}
" Scratch {{{

command! ScratchToggle call ScratchToggle()

function! ScratchToggle()
    if exists("w:is_scratch_window")
        unlet w:is_scratch_window
        exec "q"
    else
        exec "normal! :Scratch\<cr>\<C-W>L"
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
" Sparkup {{{

augroup ft_sparkup
    au!
    au FileType xml,html,htmljinja,htmldjango imap <buffer> <c-e> <Plug>SparkupExecute
    au FileType xml,html,htmljinja,htmldjango imap <buffer> <c-l> <Plug>SparkupNext
augroup END

" let g:sparkupNextMapping = '<c-s>'

"}}}
" " Syntastic {{{

" let g:syntastic_aggregate_errors = 1
" let g:syntastic_always_populate_loc_list = 1
" let g:syntastic_check_on_open = 1

" let g:syntastic_enable_signs = 1
" let g:syntastic_error_symbol = '✗'
" let g:syntastic_warning_symbol = '⚠'

" " let g:syntastic_java_checker = 'javac'
" let g:syntastic_mode_map = {
"             \ "mode": "active",
"             \ "active_filetypes": [],
"             \ "passive_filetypes": ['java', 'html', 'rst', 'scala']
"             \ }
" let g:syntastic_stl_format = '[%E{%e Errors}%B{, }%W{%w Warnings}]'
" let g:syntastic_jsl_conf = '$HOME/.vim/jsl.conf'

" " let g:syntastic_python_flake8_quiet_messages
" " let g:syntastic_python_python_exec = 'python2'
" " let g:syntastic_python_flake8_exec = 'flake8-python2'

" " nnoremap <leader>C :SyntasticCheck<cr>

" " }}}
" Slime {{{

" let g:slime_target = "tmux"
let g:slime_target = "vimterminal"
let g:slime_vimterminal_config = {"vertical":1}
" use %cpaste magic for error-free pasting
let g:slime_python_ipython = 1

" }}}
" Splice {{{

" let g:splice_prefix = "-"

" let g:splice_initial_mode = "grid"

" let g:splice_initial_layout_grid = 0
" let g:splice_initial_layout_loupe = 0
" let g:splice_initial_layout_compare = 0
" let g:splice_initial_layout_path = 0

" let g:splice_initial_diff_grid = 1
" let g:splice_initial_diff_loupe = 0
" let g:splice_initial_diff_compare = 1
" let g:splice_initial_diff_path = 0

" let g:splice_initial_scrollbind_grid = 0
" let g:splice_initial_scrollbind_loupe = 0
" let g:splice_initial_scrollbind_compare = 0
" let g:splice_initial_scrollbind_path = 0

" let g:splice_wrap = "nowrap"

" }}}
" Startify {{{

let g:startify_fortune_use_unicode = 1
let g:startify_bookmarks = [ '~/.vimrc', '~/.zshrc' ]

" }}}
" Surround {{{

" defines a new replacement on d (for "double"; 100 = char2nr('d'))
" use with ysiwd
let g:surround_100 = "{{ \r }}"

" surround work winth {{ }}
" nmap <C-J> ysiw}lysiw{

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

let g:tmuxline_powerline_separators = 0
let g:tmuxline_preset = {
      \'a'       : '#H',
      \'b'       : '#S',
      \'c'       : '#W',
      \'win'     : '#I #W',
      \'cwin'    : '#I #W',
      \'x'       : '#(uptime  | cut -d " " -f 1,2,3)',
      \'y'       : '%H:%M',
      \'z'       : '#T'}

" if hostname() == "fireball"
"     let g:tmuxline_preset['x'] = ['SC: #{maildir_counter_1}', 'SA: #{maildir_counter_2}', 'CR: #{maildir_counter_3}', '#(uptime  | cut -d " " -f 1,2,3)']
" endif

" }}}
" Ultisnips {{{

let g:UltiSnipsUsePythonVersion = 3
let g:UltiSnipsEditSplit = 'context'
let g:UltiSnipsSnippetsDir = '~/.vim/mysnippets'
" let g:UltiSnipsSnippetDirectories = ['UltiSnips', 'mysnippets']
let g:UltiSnipsExpandTrigger="<c-j>"
" let g:UltiSnipsExpandTrigger="<c-tab>"
" let g:UltiSnipsListSnippets="<c-s-tab>"
" let g:UltiSnipsJumpForwardTrigger="<c-j>"
" let g:UltiSnipsJumpBackwardTrigger="<c-k>"

" }}}
" Viewdoc {{{

" let g:viewdoc_open = "belowright vnew"
" let g:viewdoc_pydoc_cmd ="/usr/bin/pydoc2"
" " let g:viewdoc_man_cmd='LANG=en_US.UTF-8 /usr/bin/man'

" }}}
" Vimtex {{{

let g:vimtex_view_method = 'zathura'
let g:vimtex_view_general_viewer = 'zathura'
let g:vimtex_fold_enabled = 1
let g:vimtex_fold_sections = ["part", "chapter", "section"]

if !exists('g:ycm_semantic_triggers')
    let g:ycm_semantic_triggers = {}
endif
let g:ycm_semantic_triggers.tex = [
            \ 're!\\[A-Za-z]*cite[A-Za-z]*(\[[^]]*\]){0,2}{[^}]*',
            \ 're!\\[A-Za-z]*ref({[^}]*|range{([^,{}]*(}{)?))',
            \ 're!\\hyperref\[[^]]*',
            \ 're!\\includegraphics\*?(\[[^]]*\]){0,2}{[^}]*',
            \ 're!\\(include(only)?|input){[^}]*',
            \ 're!\\\a*(gls|Gls|GLS)(pl)?\a*(\s*\[[^]]*\]){0,2}\s*\{[^}]*',
            \ 're!\\includepdf(\s*\[[^]]*\])?\s*\{[^}]*',
            \ 're!\\includestandalone(\s*\[[^]]*\])?\s*\{[^}]*',
            \ ]

" }}}
" Vimux {{{

nnoremap <Leader>rp :VimuxPromptCommand<CR>
nnoremap <Leader>rl :VimuxRunLastCommand<CR>
nnoremap <Leader>ri :VimuxInspectRunner<CR>
nnoremap <Leader>rz :VimuxZoomRunner<CR>
" map <Leader>rm :VimuxPromptCommand("make ")<CR>

function! VimuxSlime()
    call VimuxSendText(@v)
    call VimuxSendKeys("Enter")
endfunction

" If text is selected, save it in the v buffer and send that buffer it to tmux
vnoremap <Leader>rs "vy :call VimuxSlime()<CR>

" Select current paragraph and send it to tmux
nnoremap <Leader>rs vip<Leader>rs<CR>

" Run the current file with nose
" nnoremap <Leader>rn :call VimuxRunCommand("clear; nosetests " . bufname("%"))<CR>
" Run command without sending a return
" map <Leader>rq :call VimuxRunCommand("clear; nosetests " . bufname("%"), 0)<CR>

" }}}
" YankRing {{{

let g:yankring_history_dir = '$HOME/.vim/tmp'

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
" " YouCompleteMe {{{

" " let g:ycm_python_binary_path = 'python'

" let g:ycm_add_preview_to_completeopt = 1
" let g:ycm_autoclose_preview_window_after_insertion = 1
" let g:ycm_autoclose_preview_window_after_completion = 1

" let g:ycm_collect_identifiers_from_tags_files = 1
" let g:ycm_complete_in_comments = 1
" " let g:ycm_min_num_of_chars_for_completion = 4
" let g:ycm_seed_identifiers_with_syntax = 1

" " remove '<S-TAB>' to avoid conflict with ultisnips
" let g:ycm_key_list_previous_completion=['<Up>']

" let g:ycm_global_ycm_extra_conf = '~/lib/dotfiles/ycm_extra_conf.py'
" let g:ycm_confirm_extra_conf = 1

" nnoremap <leader>] :YcmCompleter GoTo<CR>mzzMzvzz15<c-e>`z
" nnoremap K :YcmCompleter GetDoc<CR>
" nnoremap <leader>jf :YcmCompleter FixIt<CR>
" nnoremap <leader>ji :YcmCompleter GoToInclude<CR>
" nnoremap <leader>jd :YcmCompleter GoToDeclaration<CR>

" " nnoremap <leader>] :YcmCompleter GoToDefinitionElseDeclaration<CR>mzzMzvzz15<c-e>`z:Pulse<cr>
" " nnoremap <leader>] :YcmCompleter GoToDeclaration<CR>mzzMzvzz15<c-e>`z:Pulse<cr>

" " }}}

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
    " exec "Errors"
    lopen
    let w:is_error_window = 1
  endif
endfunction " }}}

command! -bang -nargs=? QuickfixToggle call QuickfixToggle(<bang>0)
function! QuickfixToggle(forced) " {{{
  if exists("g:qfix_win") && a:forced == 0
    cclose
    unlet g:qfix_win
  else
    copen 10
    let g:qfix_win = bufnr("$")
  endif
endfunction " }}}

nmap <silent> <F3> :ErrorsToggle<cr>
nmap <silent> <F4> :QuickfixToggle<cr>

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

" nnoremap <silent> <leader>A :set opfunc=<SID>AckMotion<CR>g@
" xnoremap <silent> <leader>A :<C-U>call <SID>AckMotion(visualmode())<CR>

" nnoremap <bs> :Ack! '\b<c-r><c-w>\b'<cr>
" xnoremap <silent> <bs> :<C-U>call <SID>AckMotion(visualmode())<CR>

" function! s:CopyMotionForType(type)
"     if a:type ==# 'v'
"         silent execute "normal! `<" . a:type . "`>y"
"     elseif a:type ==# 'char'
"         silent execute "normal! `[v`]y"
"     endif
" endfunction

" function! s:AckMotion(type) abort
"     let reg_save = @@

"     call s:CopyMotionForType(a:type)

"     execute "normal! :Ack! --literal " . shellescape(@@) . "\<cr>"

"     let @@ = reg_save
" endfunction

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

" function! s:Pulse() " {{{
"     redir => old_hi
"         silent execute 'hi CursorLine'
"     redir END
"     let old_hi = split(old_hi, '\n')[0]
"     let old_hi = substitute(old_hi, 'xxx', '', '')

"     let steps = 8
"     let width = 1
"     let start = width
"     let end = steps * width
"     let color = 233

"     for i in range(start, end, width)
"         execute "hi CursorLine ctermbg=" . (color + i)
"         redraw
"         sleep 6m
"     endfor
"     for i in range(end, start, -1 * width)
"         execute "hi CursorLine ctermbg=" . (color + i)
"         redraw
"         sleep 6m
"     endfor

"     execute 'hi ' . old_hi
" endfunction " }}}
" command! -nargs=0 Pulse call s:Pulse()

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
    " setlocal spell spelllang=
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

highlight Comment cterm=italic gui=italic

if has('gui_running')
    " GUI Vim

    if has("win32")
        " set guifont=Courier:h10:cANSI
    else
        if hostname() == "thunderball"
            set guifont=Inconsolata\ for\ Powerline\ 12
        elseif hostname() == "fireball"
            set guifont=Inconsolata\ for\ Powerline\ 14
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
" elseif has('nvim')
"     colorscheme badwolf
else
    " Console Vim
    " Mouse support
    set mouse=a

    "set term=xterm
    set t_Co=256
    " colorscheme molokai
    "set t_Co=8
    "set termencoding=utf-8
    if !has('nvim')
        set ttymouse=sgr
    endif
endif

" }}}
" Local config ------------------------------------------------------------ {{{

if filereadable(expand("~/.vim/local.vim"))
    source ~/.vim/local.vim
endif

" }}}

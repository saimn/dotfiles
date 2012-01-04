" Preamble ---------------------------------------------------------------- {{{

filetype off
call pathogen#infect()
filetype plugin indent on       " load file type plugins + indentation
set nocompatible                " choose no compatibility with legacy vi

" }}}
" Basic options ----------------------------------------------------------- {{{

set encoding=utf-8
set modelines=0                 " disable modelines
set autoindent                  " always set autoindenting on
set smartindent                 " clever autoindenting
set showcmd                     " display incomplete commands
set showmode
set hidden                      " Allow backgrounding buffers without writing them
set visualbell
set cursorline
set ttyfast
set ruler
set backspace=indent,eol,start  " backspace through everything in insert mode
set nonumber
set norelativenumber
set laststatus=2                " show always statusline of last window
set history=1000
set undofile
set undoreload=10000
set cpoptions+=J
set list                        " show eol, tabs, spaces, ...
set listchars=tab:▸\ ,eol:¬,extends:❯,precedes:❮
set shell=/bin/bash
set lazyredraw
set matchtime=3
set showbreak=↪
set splitbelow
set splitright
set fillchars=diff:⣿
set ttimeout
set notimeout
set nottimeout
set autowrite
set shiftround
set autoread
set title
set dictionary=/usr/share/dict/words
" set autochdir           " change current dir

" Wildmenu completion {{{

set wildmenu
" set wildmode=list:longest
set wildmode=longest:full,full

set wildignore+=.hg,.git,.svn                    " Version control
set wildignore+=*.aux,*.out,*.toc                " LaTeX intermediate files
set wildignore+=*.jpg,*.bmp,*.gif,*.png,*.jpeg   " binary images
set wildignore+=*.o,*.obj,*.exe,*.dll,*.manifest " compiled object files
set wildignore+=*.pyc                            " Python byte code

" When doing tab completion, give the following files lower priority. You may
" wish to set 'wildignore' to completely ignore files, and 'wildmenu' to enable
" enhanced tab completion. These can be done in the user vimrc file.
set suffixes+=.info,.aux,.log,.dvi,.bbl,.out,.o,.lo

" }}}

" Make Vim able to edit crontab files again.
set backupskip=/tmp/*,/private/tmp/*" 

" Save when losing focus
"au FocusLost * :wa

" Resize splits when the window is resized
au VimResized * exe "normal! \<c-w>="

" Tabs, spaces, wrapping {{{

" a tab is two spaces (or set this to 4)
set tabstop=4
set shiftwidth=4
set softtabstop=4
set expandtab               " use spaces, not tabs (optional)
set wrap
" set linebreak     " ne casse pas les mots en fin de ligne

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
"set formatoptions+=a        " Automatic formatting of paragraphs
set formatoptions+=t        " Auto-wrap text using textwidth
set formatoptions+=c        " Auto-wrap comments using textwidth

set colorcolumn=+1

" }}}
" Backups {{{

set undodir=~/.vim/tmp/undo//     " undo files
set backupdir=~/.vim/tmp/backup// " backups
set directory=~/.vim/tmp/swap//   " swap files
set backup                        " enable backups

" }}}
" Leader {{{

let mapleader = ","
let maplocalleader = "\\"

" }}}
" Color scheme {{{

syntax on
set background=dark
colorscheme molokai

" Highlight VCS conflict markers
match ErrorMsg '^\(<\|=\|>\)\{7\}\([^=].\+\)\?$'

" }}}

" }}}
" Status line ------------------------------------------------------------- {{{

augroup ft_statuslinecolor
    au!

    au InsertEnter * hi StatusLine ctermfg=196 guifg=#FF3145
    au InsertLeave * hi StatusLine ctermfg=130 guifg=#CD5907
augroup END

set statusline=%f    " Path.
set statusline+=%m   " Modified flag.
set statusline+=%r   " Readonly flag.
set statusline+=%w   " Preview window flag.

set statusline+=\    " Space.

set statusline+=%#redbar#                " Highlight the following as a warning.
set statusline+=%{SyntasticStatuslineFlag()} " Syntastic errors.
set statusline+=%*                           " Reset highlighting.

set statusline+=%=   " Right align.

" File format, encoding and type.  Ex: "(unix/utf-8/python)"
set statusline+=(
set statusline+=%{&ff}                        " Format (unix/DOS).
set statusline+=/
set statusline+=%{strlen(&fenc)?&fenc:&enc}   " Encoding (utf-8).
set statusline+=/
set statusline+=%{&ft}                        " Type (python).
set statusline+=)

" Line and column position and counts.
set statusline+=\ (line\ %l\/%L,\ col\ %03c)

" }}}
" Searching and movement -------------------------------------------------- {{{

" Use sane regexes.
nnoremap / /\v
vnoremap / /\v

set hlsearch                    " highlight matches
set incsearch                   " incremental searching
set ignorecase                  " searches are case insensitive...
set smartcase                   " ... unless they contain at least one capital letter
set showmatch                   " show matching brackets
set gdefault                    " substitute all matches on the line

set scrolloff=3         " min nb of lines to keep above and below the cursor.
set sidescroll=1        " min nb of columns to scroll horizontally.(with nowrap)
set sidescrolloff=10

set virtualedit+=block  " allow cursor where there is no actual character.

noremap <leader><space> :noh<cr>:call clearmatches()<cr>

" Ack for the last search.
nnoremap <silent> <leader>? :execute "Ack! '" . substitute(substitute(substitute(@/, "\\\\<", "\\\\b", ""), "\\\\>", "\\\\b", ""), "\\\\v", "", "") . "'"<CR>

" Error navigation {{{
"
"             Location List     QuickFix Window
"            (e.g. Syntastic)     (e.g. Ack)
"            ----------------------------------
" Next      |     M-j               M-Down     |
" Previous  |     M-k                M-Up      |
"            ----------------------------------
"
nnoremap ∆ :lnext<cr>zvzz
nnoremap ˚ :lprevious<cr>zvzz
inoremap ∆ <esc>:lnext<cr>zvzz
inoremap ˚ <esc>:lprevious<cr>zvzz
nnoremap <m-Down> :cnext<cr>zvzz
nnoremap <m-Up> :cprevious<cr>zvzz
" }}}

" }}}
" Mapping ----------------------------------------------------------------- {{{

" Toggle paste
set pastetoggle=<F8>

" Keep search matches in the middle of the window and pulse the line when moving
" to them.
nnoremap n nzzzv:call PulseCursorLine()<cr>
nnoremap N Nzzzv:call PulseCursorLine()<cr>

" switch between the currently open buffer and the previous one
nnoremap <c-Tab> <c-^>

" don't use Ex mode, use Q for formatting
"map Q gq
" Formatting, TextMate-style
nnoremap Q gqip

" make ; do the same thing as :
nnoremap ; :

" easier navigation between split windows
nnoremap <c-j> <c-w>j
nnoremap <c-k> <c-w>k
nnoremap <c-h> <c-w>h
nnoremap <c-l> <c-w>l
noremap <leader>v <C-w>v

" Heresy
inoremap <c-a> <esc>I
inoremap <c-e> <esc>A

" disable cursor keys in normal mode
map <Left> :echo "no!"<cr>
map <Right> :echo "no!"<cr>
map <Up> :echo "no!"<cr>
map <Down> :echo "no!"<cr>

" Directional Keys {{{

" move by screen line
noremap j gj
noremap k gk

" }}}
" }}}
" Environments (GUI/Console) ---------------------------------------------- {{{

if has('gui_running')
    if has("win32")
        set guifont=Courier:h10:cANSI
    else
        if $HOSTNAME == "goudes"
            set guifont=Inconsolata\ 11
        elseif $HOSTNAME == "fireball"
            set guifont=Inconsolata\ 10
        endif
    endif

    set go-=T   " No toolbar
    set go-=l   " No scrollbars
    set go-=L
    set go-=r
    set go-=R
    " set guioptions+=a    " clipboard to autoselect
    " set guioptions+=c    " Use console dialogs instead of popup

    set mousefocus                " Le focus suit la souris
    set mousemodel=popup_setpos   " Le bouton droit affiche une popup

    highlight SpellBad term=underline gui=undercurl guisp=Orange

    " Use a line-drawing char for pretty vertical splits.
    set fillchars+=vert:│

    " Different cursors for different modes.
    set guicursor=n-c:block-Cursor-blinkon0
    set guicursor+=v:block-vCursor-blinkon0
    set guicursor+=i-ci:ver20-iCursor
else
    " Console Vim
    "set term=xterm
    set t_Co=256
    colorscheme molokai
    "set t_Co=8
    "set termencoding=utf-8
    "set ttymouse=xterm
endif

" }}}
" Folding ----------------------------------------------------------------- {{{

set foldlevelstart=0

" Space to toggle folds.
nnoremap <Space> za
vnoremap <Space> za

" Make zO recursively open whatever top level fold we're in, no matter where the
" cursor happens to be.
nnoremap zO zCzO

" Use ,z to "focus" the current fold.
nnoremap <leader>z zMzvzz

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
" Various filetype-specific stuff ----------------------------------------- {{{

augroup filetypedetect
   autocmd BufRead,BufNewFile *.txt setfiletype text
   autocmd BufRead,BufNewFile *.tpl setfiletype html
   autocmd BufRead,BufNewFile *.pro setfiletype idlang
   autocmd BufRead,BufNewFile *.mkd setfiletype mkd
   autocmd BufRead,BufNewFile *.wiki setfiletype Wikipedia
   autocmd BufRead,BufNewFile *wikipedia.org* setfiletype Wikipedia
   autocmd BufRead,BufNewFile *camptocamp.org* setfiletype camptocamp
   autocmd BufRead,BufNewFile vimperator* setfiletype bbcode
   autocmd BufRead,BufNewFile *conky* setfiletype conkyrc
   autocmd BufRead,BufNewFile *.inc setfiletype php
   autocmd BufRead,BufNewFile *.muttrc setfiletype muttrc
   autocmd BufRead,BufNewFile ~/.mutt/tmp/mutt* setfiletype mail
augroup END

if has("autocmd")
    " Remember last location in file, but not for commit messages.
    " see :help last-position-jump
    au BufReadPost * if &filetype !~ '^git\c' && line("'\"") > 0 && line("'\"") <= line("$")
                \| exe "normal! g`\"" | endif
endif

" C {{{

augroup ft_c
    au!
    au FileType c setlocal foldmethod=syntax
augroup END

" }}}
" Confluence {{{

augroup ft_c
    au!

    au BufRead,BufNewFile *.confluencewiki setlocal filetype=confluencewiki

    " Wiki pages should be soft-wrapped.
    au FileType confluencewiki setlocal wrap linebreak nolist
augroup END

" }}}
" Cram {{{

let cram_fold=1

augroup ft_cram
    au!

    au BufNewFile,BufRead *.t set filetype=cram
    au Syntax cram setlocal foldlevel=1
augroup END


" }}}
" CSS and LessCSS {{{

augroup ft_css
    au!

    au BufNewFile,BufRead *.less setlocal filetype=less

    au Filetype less,css setlocal foldmethod=marker
    au Filetype less,css setlocal foldmarker={,}
    au Filetype less,css setlocal omnifunc=csscomplete#CompleteCSS
    au Filetype less,css setlocal iskeyword+=-

    " Use <leader>S to sort properties.  Turns this:
    au BufNewFile,BufRead *.less,*.css nnoremap <buffer> <localleader>S ?{<CR>jV/\v^\s*\}?$<CR>k:sort<CR>:noh<CR>

    " Make {<cr> insert a pair of brackets in such a way that the cursor is correctly
    " positioned inside of them AND the following code doesn't get unfolded.
    au BufNewFile,BufRead *.less,*.css inoremap <buffer> {<cr> {}<left><cr><space><space><space><space>.<cr><esc>kA<bs>
augroup END

" }}}
" Django {{{

augroup ft_django
    au!

    au BufNewFile,BufRead urls.py           setlocal nowrap
    au BufNewFile,BufRead urls.py           normal! zR
    au BufNewFile,BufRead dashboard.py      normal! zR
    au BufNewFile,BufRead local_settings.py normal! zR

    au BufNewFile,BufRead admin.py     setlocal filetype=python.django
    au BufNewFile,BufRead urls.py      setlocal filetype=python.django
    au BufNewFile,BufRead models.py    setlocal filetype=python.django
    au BufNewFile,BufRead views.py     setlocal filetype=python.django
    au BufNewFile,BufRead settings.py  setlocal filetype=python.django
    au BufNewFile,BufRead settings.py  setlocal foldmethod=marker
    au BufNewFile,BufRead forms.py     setlocal filetype=python.django
    au BufNewFile,BufRead common_settings.py  setlocal filetype=python.django
    au BufNewFile,BufRead common_settings.py  setlocal foldmethod=marker
augroup END

" }}}
" HTML and HTMLDjango {{{

augroup ft_html
    au!

    au BufNewFile,BufRead *.html setlocal filetype=htmldjango
    au FileType html,jinja,htmldjango setlocal foldmethod=manual

    " Use <localleader>f to fold the current tag.
    au FileType html,jinja,htmldjango nnoremap <buffer> <localleader>f Vatzf

    " Use Shift-Return to turn this:
    "     <tag>|</tag>
    "
    " into this:
    "     <tag>
    "         |
    "     </tag>
    au FileType html,jinja,htmldjango nnoremap <buffer> <s-cr> vit<esc>a<cr><esc>vito<esc>i<cr><esc>

    " Smarter pasting
    au FileType html,jinja,htmldjango nnoremap <buffer> p :<C-U>YRPaste 'p'<CR>v`]=`]
    au FileType html,jinja,htmldjango nnoremap <buffer> P :<C-U>YRPaste 'P'<CR>v`]=`]
    au FileType html,jinja,htmldjango nnoremap <buffer> π :<C-U>YRPaste 'p'<CR>
    au FileType html,jinja,htmldjango nnoremap <buffer> ∏ :<C-U>YRPaste 'P'<CR>

    " Django tags
    au FileType jinja,htmldjango inoremap <buffer> <c-t> {%<space><space>%}<left><left><left>

    " Django variables
    au FileType jinja,htmldjango inoremap <buffer> <c-f> {{<space><space>}}<left><left><left>
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

augroup ft_javascript
    au!

    " Treat JSON files like JavaScript
    au BufNewFile,BufRead *.json set ft=javascript

    au FileType javascript setlocal foldmethod=marker
    au FileType javascript setlocal foldmarker={,}
augroup END

" }}}
" Lisp {{{

augroup ft_lisp
    au!
    au FileType lisp call TurnOnLispFolding()
augroup END

" }}}
" Makefile {{{

augroup ft_make
    au!

    " In Makefiles, use real tabs, not tabs expanded to spaces
    au FileType make set noexpandtab
augroup END

" }}}
" Markdown {{{

augroup ft_markdown
    au!

    au BufRead,BufNewFile *.{md,markdown,mdown,mkd,mkdn,txt} setlocal filetype=markdown
    "au BufRead,BufNewFile *.{md,markdown,mdown,mkd,mkdn,txt} setf markdown | call s:setupWrapping()

    " Use <localleader>1/2/3 to add headings.
    au Filetype markdown nnoremap <buffer> <localleader>1 yypVr=
    au Filetype markdown nnoremap <buffer> <localleader>2 yypVr-
    au Filetype markdown nnoremap <buffer> <localleader>3 I### <ESC>
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
augroup END

" }}}
" Pentadactyl {{{

augroup ft_pentadactyl
    au!
    au BufNewFile,BufRead .pentadactylrc set filetype=pentadactyl
    au BufNewFile,BufRead ~/Library/Caches/TemporaryItems/pentadactyl-*.tmp set nolist wrap linebreak columns=100 colorcolumn=0
augroup END

" }}}
" Puppet {{{

augroup ft_puppet
    au!

    au Filetype puppet setlocal foldmethod=marker
    au Filetype puppet setlocal foldmarker={,}
augroup END

" }}}
" Python {{{

augroup ft_python
    au!

    " make Python follow PEP8 ( http://www.python.org/dev/peps/pep-0008/ )
    au FileType python set softtabstop=4 tabstop=4 shiftwidth=4 textwidth=79

    " au FileType python setlocal omnifunc=pythoncomplete#Complete
    au FileType python setlocal define=^\s*\\(def\\\\|class\\)
    au FileType python compiler nose
    au FileType man nnoremap <buffer> <cr> :q<cr>
    
    " Jesus tapdancing Christ, built-in Python syntax, you couldn't let me
    " override this in a normal way, could you?
    au FileType python if exists("python_space_error_highlight") | unlet python_space_error_highlight | endif
augroup END

" }}}
" QuickFix {{{

augroup ft_quickfix
    au!
    au Filetype qf setlocal colorcolumn=0 nolist nocursorline nowrap
augroup END

" }}}
" ReStructuredText {{{

augroup ft_rest
    au!

    au Filetype rst nnoremap <buffer> <localleader>1 yypVr=
    au Filetype rst nnoremap <buffer> <localleader>2 yypVr-
    au Filetype rst nnoremap <buffer> <localleader>3 yypVr~
    au Filetype rst nnoremap <buffer> <localleader>4 yypVr`
augroup END

" }}}
" Ruby {{{

augroup ft_ruby
    au!
    au Filetype ruby setlocal foldmethod=syntax
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

    au FileType vim setlocal foldmethod=marker
    au FileType help setlocal textwidth=78
    au BufWinEnter *.txt if &ft == 'help' | wincmd L | endif
augroup END

" }}}

" }}}
" Plugin settings --------------------------------------------------------- {{{

" Ack {{{

map <leader>a :Ack! 

" }}}
" Commentary {{{

nmap <leader>c <Plug>CommentaryLine
xmap <leader>c <Plug>Commentary
au FileType htmldjango setlocal commentstring={#\ %s\ #}

" }}}
" Ctrl-P {{{

let g:ctrlp_map = '<leader>,'           " mapping to invoke CtrlP
let g:ctrlp_working_path_mode = 1       " working directory
let g:ctrlp_match_window_reversed = 1
let g:ctrlp_split_window = 0
let g:ctrlp_prompt_mappings = {
\ 'PrtSelectMove("j")':   ['<c-j>', '<down>', '<s-tab>'],
\ 'PrtSelectMove("k")':   ['<c-k>', '<up>', '<tab>'],
\ 'PrtHistory(-1)':       ['<c-n>'],
\ 'PrtHistory(1)':        ['<c-p>'],
\ 'ToggleFocus()':        ['<c-tab>'],
\ }

" }}}
" Easymotion {{{

let g:EasyMotion_do_mapping = 0

nnoremap <silent> <Leader>f      :call EasyMotionF(0, 0)<CR>
onoremap <silent> <Leader>f      :call EasyMotionF(0, 0)<CR>
vnoremap <silent> <Leader>f :<C-U>call EasyMotionF(1, 0)<CR>

nnoremap <silent> <Leader>F      :call EasyMotionF(0, 1)<CR>
onoremap <silent> <Leader>F      :call EasyMotionF(0, 1)<CR>
vnoremap <silent> <Leader>F :<C-U>call EasyMotionF(1, 1)<CR>

onoremap <silent> <Leader>t      :call EasyMotionT(0, 0)<CR>
onoremap <silent> <Leader>T      :call EasyMotionT(0, 1)<CR>

" }}}
" Fugitive {{{

nnoremap <leader>gd :Gdiff<cr>
nnoremap <leader>gs :Gstatus<cr>
nnoremap <leader>gw :Gwrite<cr>
nnoremap <leader>ga :Gadd<cr>
nnoremap <leader>gb :Gblame<cr>
nnoremap <leader>gco :Gcheckout<cr>
nnoremap <leader>gci :Gcommit<cr>
nnoremap <leader>gm :Gmove<cr>
nnoremap <leader>gr :Gremove<cr>
nnoremap <leader>gl :Shell git gl -18<cr>:wincmd \|<cr>

augroup ft_fugitive
    au!

    au BufNewFile,BufRead .git/index setlocal nolist
augroup END

" }}}
" Gundo {{{

nnoremap <F5> :GundoToggle<CR>
let g:gundo_debug = 1
let g:gundo_preview_bottom = 1

" }}}
" HTML5 {{{

let g:event_handler_attributes_complete = 0
let g:rdfa_attributes_complete = 0
let g:microdata_attributes_complete = 0
let g:atia_attributes_complete = 0

" }}}
" Makegreen {{{

nnoremap \| :call MakeGreen('')<cr>

" }}}
" NERD Tree {{{

noremap  <F2> :NERDTreeToggle<cr>
inoremap <F2> <esc>:NERDTreeToggle<cr>

au Filetype nerdtree setlocal nolist

let NERDTreeHighlightCursorline=1
let NERDTreeIgnore=['.vim$', '\~$', '.*\.pyc$', 'pip-log\.txt$', 'whoosh_index', 'xapian_index', '.*.pid', 'monitor.py', '.*-fixtures-.*.json', '.*\.o$', 'db.db']

let NERDTreeMinimalUI = 1
let NERDTreeDirArrows = 1

" }}}
" OrgMode {{{

let g:org_plugins = ['ShowHide', '|', 'Navigator', 'EditStructure', '|', 'Todo', 'Date', 'Misc']

let g:org_todo_keywords = ['TODO', '|', 'DONE']

let g:org_debug = 1

" }}}
" Python-Mode {{{

let g:pymode_doc = 1
let g:pymode_doc_key = '<localleader>ds'
let g:pydoc = 'pydoc'
let g:pymode_syntax = 0
let g:pymode_run = 0
let g:pymode_lint = 0
let g:pymode_breakpoint = 0
let g:pymode_utils_whitespaces = 0
let g:pymode_virtualenv = 1

let g:pymode_options_indent = 0
let g:pymode_options_fold = 0
let g:pymode_options_other = 0

let g:pymode_rope = 1
let g:pymode_rope_global_prefix = "<localleader>R"
let g:pymode_rope_local_prefix = "<localleader>r"
let g:pymode_rope_auto_project = 1
let g:pymode_rope_enable_autoimport = 0
let g:pymode_rope_autoimport_generate = 1
let g:pymode_rope_autoimport_underlineds = 0
let g:pymode_rope_codeassist_maxfixes = 10
let g:pymode_rope_sorted_completions = 1
let g:pymode_rope_extended_complete = 1
let g:pymode_rope_autoimport_modules = ["os", "shutil", "datetime"]
let g:pymode_rope_confirm_saving = 1
let g:pymode_rope_vim_completion = 1
let g:pymode_rope_guess_project = 1
let g:pymode_rope_goto_def_newwin = 0
let g:pymode_rope_always_show_complete_menu = 0

" }}}
" Scratch {{{

command! ScratchToggle call ScratchToggle()

function! ScratchToggle() " {{{
  if exists("w:is_scratch_window")
    unlet w:is_scratch_window
    exec "q"
  else
    exec "normal! :Sscratch\<cr>\<C-W>J:resize 13\<cr>"
    let w:is_scratch_window = 1
  endif
endfunction " }}}

nnoremap <silent> <leader><tab> :ScratchToggle<cr>

" }}}
" Sparkup {{{

let g:sparkupNextMapping = '<c-s>'

"}}}
" Supertab {{{

let g:SuperTabDefaultCompletionType = "<c-n>"
let g:SuperTabLongestHighlight = 1

"}}}
" Syntastic {{{

let g:syntastic_enable_signs = 1
let g:syntastic_disabled_filetypes = ['html']
let g:syntastic_stl_format = '[%E{Error 1/%e: line %fe}%B{, }%W{Warning 1/%w: line %fw}]'
let g:syntastic_jsl_conf = '$HOME/.vim/jsl.conf'

" }}}
" YankRing {{{

function! YRRunAfterMaps()
    nnoremap Y :<C-U>YRYankCount 'y$'<CR>
    omap <expr> L YRMapsExpression("", "$")
    omap <expr> H YRMapsExpression("", "^")
endfunction

" Yankring
nnoremap <silent> <F6> :YRShow<cr>

" }}}

" }}}
" Shell ------------------------------------------------------------------- {{{

function! s:ExecuteInShell(command) " {{{
    let command = join(map(split(a:command), 'expand(v:val)'))
    let winnr = bufwinnr('^' . command . '$')
    silent! execute  winnr < 0 ? 'botright vnew ' . fnameescape(command) : winnr . 'wincmd w'
    setlocal buftype=nowrite bufhidden=wipe nobuflisted noswapfile nowrap nonumber
    echo 'Execute ' . command . '...'
    silent! execute 'silent %!'. command
    silent! redraw
    silent! execute 'au BufUnload <buffer> execute bufwinnr(' . bufnr('#') . ') . ''wincmd w'''
    silent! execute 'nnoremap <silent> <buffer> <LocalLeader>r :call <SID>ExecuteInShell(''' . command . ''')<CR>:AnsiEsc<CR>'
    silent! execute 'nnoremap <silent> <buffer> q :q<CR>'
    silent! execute 'AnsiEsc'
    echo 'Shell command ' . command . ' executed.'
endfunction " }}}
command! -complete=shellcmd -nargs=+ Shell call s:ExecuteInShell(<q-args>)
nnoremap <leader>! :Shell 

" }}}
" Convenience mappings ---------------------------------------------------- {{{

" Clean whitespace
map <leader>W  :%s/\s\+$//<cr>:let @/=''<CR>

" Change case
nnoremap <C-u> gUiw
inoremap <C-u> <esc>gUiwea

" Substitute
nnoremap <leader>s :%s//<left>

" Emacs bindings in command line mode
cnoremap <c-a> <home>
cnoremap <c-e> <end>

" Easier linewise reselection
nnoremap <leader>V V`]

" Align text
nnoremap <leader>Al :left<cr>
nnoremap <leader>Ac :center<cr>
nnoremap <leader>Ar :right<cr>
vnoremap <leader>Al :left<cr>
vnoremap <leader>Ac :center<cr>
vnoremap <leader>Ar :right<cr>

" Faster Esc
inoremap jk <esc>

" Quickreturn
inoremap <c-cr> <esc>A<cr>

" }}}
" Pulse ------------------------------------------------------------------- {{{

function! PulseCursorLine()
    let current_window = winnr()

    windo set nocursorline
    execute current_window . 'wincmd w'

    setlocal cursorline

    redir => old_hi
        silent execute 'hi CursorLine'
    redir END
    let old_hi = split(old_hi, '\n')[0]
    let old_hi = substitute(old_hi, 'xxx', '', '')

    hi CursorLine guibg=#2a2a2a ctermbg=233
    redraw
    sleep 20m

    hi CursorLine guibg=#333333 ctermbg=235
    redraw
    sleep 20m

    hi CursorLine guibg=#3a3a3a ctermbg=237
    redraw
    sleep 20m

    hi CursorLine guibg=#444444 ctermbg=239
    redraw
    sleep 20m

    hi CursorLine guibg=#3a3a3a ctermbg=237
    redraw
    sleep 20m

    hi CursorLine guibg=#333333 ctermbg=235
    redraw
    sleep 20m

    hi CursorLine guibg=#2a2a2a ctermbg=233
    redraw
    sleep 20m

    execute 'hi ' . old_hi

    windo set cursorline
    execute current_window . 'wincmd w'
endfunction

" }}}


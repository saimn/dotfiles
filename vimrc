" Description: Vim configuration file
" Author: Simon <contact at saimon dot org>

" -----------------------------------------------------------
" General setup
" -----------------------------------------------------------

set nocompatible        " Use Vim defaults (much better!)
set viminfo='20,\"50    " .viminfo: don't store more than 50 lines of registers
set history=100         " nb of command line history
set undolevels=150

set noerrorbells        " ne fait pas un bip lors d'une erreur
set visualbell          " Avertissement par flash

set showmatch           " Affiche la paire de parenthèses
set nostartofline       " curseur dans la même colonne quand on change de ligne
set scrolloff=2         " Nb de lignes visible autour du curseur
"set number             " afficher les numéros de ligne
set autowrite
set previewheight=5
set paste

" allow backspacing over everything in insert mode
set backspace=indent,eol,start

" In many terminal emulators the mouse works just fine, thus enable it.
if has('mouse')
  set mouse=a
endif

" Tags file search path
set tags=./tags,tags,../tags

" Don't wake up system with blinking cursor:
set guicursor=a:blinkon0

" use full featured format-options. see "help fo-table for help
if v:version >= 600
   set formatoptions=tcrqn2
endif
" Autoflow paragraphs you edit as you type, no more gq!
"set fo=aw2t

" define what are comments
"set com& " reset to default
"set com^=sr:*\ -,mb:*\ \ ,el:*/ com^=sr://\ -,mb://\ \ ,el:///

" encoding ?
"if v:lang =~ "utf8$" || v:lang =~ "UTF-8$"
"               set fileencodings=ucs-bom,utf-8,latin1
"endif

" Encodage par défaut des buffers et des fichiers
"set encoding=utf-8
"set fileencoding=utf-8

" Always check for UTF-8 when trying to determine encodings.
"if &amp;fileencodings !~? "utf-8"
  "set fileencodings+=utf-8
"endif

" Make sure we have a sane fallback for encoding detection
set fileencodings+=default

" -----------------------------------------------------------
" Indentation
" -----------------------------------------------------------

" Remplace les tabulations par des espaces. Les espaces sont utilisés dans les
" indentations, avec lescommandes > et < et quand 'autoindent' est activé.
set expandtab

" Si activé, un <Tab> au début d'une ligne insère des blancs selon la valeur de
" 'shiftwidth'. 'tabstop' est utilisé dans les autres endroits. Un <RetArr> en
" début de ligne supprime un nombre d'espaces équivalant à la valeur de
" 'shiftwidth'.  Si désactivé, un <Tab> insère toujours des blancs selon la
" valeur de 'tabstop'. 'shiftwidth' n'est utilisé que lors du décalage de texte
" à gauche ou à droite.
set smarttab

set preserveindent   " préserve la structure de l'indentation autant que possible
set autoindent       " always set autoindenting on
set smartindent      " clever autoindenting
set shiftwidth=3     " Nombre d'espace pour l'(auto-)indentation
set softtabstop=3    " if non-zero, number of spaces to insert for a <tab>
set tabstop=3        " number of spaces the tab stands for >> softtabstop is better

" Montre les caractères de fin de lignes, tabs et espaces en trop
set list
"set listchars=tab:>-,eol:$,trail:-
set listchars=tab:»·,trail:·,extends:~,nbsp:.

"  go with smartindent if there is no plugin indent file.
"  but don't outdent hashes
"set smartindent
"inoremap # X#

" -----------------------------------------------------------
" Folding
" -----------------------------------------------------------
if has("gui_running")
   " Ajoute une marge à gauche pour afficher les +/- des replis
   set foldcolumn=2
   " Le découpage des folders se base sur l'indentation
   set foldmethod=marker "indent
   " 12 niveaux d'indentation par défaut pour les folders
   set foldlevel=12
endif

" -----------------------------------------------------------
" Wrap
" -----------------------------------------------------------

set wrap          " les lignes plus longues que la largeur de l'écran sont enroulées
set linebreak     " ne casse pas les mots en fin de ligne

" Largeur maxi du texte inséré
" '72' permet de wrapper automatiquement à 72 caractères
" '0' désactive la fonction
set textwidth=0

" Wrappe à 72 caractères avec la touche '#'
map # gwap
" Wrappe et justifie à 72 caractères avec la touche '@'
map @ {v}! par 72j

" Autorise le passage d'une ligne à l'autre avec les flèches gauche et droite
set whichwrap=<,>,[,]

" -----------------------------------------------------------
" Searching, Substituting
" -----------------------------------------------------------

set ignorecase  " ignore la casse des caractères dans les recherches
set smartcase   " No ignorecase if Uppercase chars in search
set magic       " change the way backslashes are used in search patterns
set wrapscan    " begin search at top when EOF reached
set sm          " jump to matches during entering the pattern
set incsearch   " ...and also during entering the pattern
set hlsearch    " Mettre en surbrillance le mot cherché

" !!! use 'g'-flag when substituting (subst. all matches in that line, not
" only first) to turn off, use g (why is there no -g ?) set gdefault

" !!! turn off the fucking :s///gc toggling
"set noedcompatible

" -----------------------------------------------------------
" Highlighting, Colors, Fonts
" -----------------------------------------------------------

" Switch syntax highlighting on, when the terminal has colors
" Also switch on highlighting the last used search pattern.
if &t_Co > 2 || has("gui_running")
   syntax on
endif

if has("gui_running")
   "set co=98                    "Nombre de colonnes à afficher
   "set lines=41                 "Nombre de lignes à afficher

   if has("win32")
      set guifont=Fixedsys:h9:cANSI
      "set guifont=Courier:h10:cANSI
   else
      "set gfn=-adobe-courier-medium-r-normal-*-*-140-*-*-m-*-iso8859-15
      if $HOSTNAME == "goudes"
         set guifont=Inconsolata\ 11
      elseif $HOSTNAME == "fireball"
         set guifont=Inconsolata\ 10
      endif

      "set guifont=Monaco\ 10
      "set guifont=Monaco:h12
   endif

   " alt jumps to menu
   set winaltkeys=menu

   " GUI options, default GTK: agimrLtT
   set guioptions+=a    " clipboard to autoselect
   set guioptions+=c    " Use console dialogs instead of popup
   set guioptions-=r    " ascenseur à droite
   set guioptions-=T    " toolbar
   set cursorline

   set mousef                    " Le focus suit la souris
   set mousemodel=popup_setpos   " Le bouton droit affiche une popup

   colorscheme wombat
   "let g:zenburn_high_Contrast = 1
   "colorscheme zenburn
elseif $HOSTNAME == "goudes"
   set t_Co=256
   colorscheme 256-grayvim
else
   set t_Co=8
   set termencoding=utf-8
   "set ttymouse=xterm
endif

" ??? how many lines to sync backwards
syn sync minlines=10000 maxlines=10000

" -----------------------------------------------------------
" Completion
" -----------------------------------------------------------

" Complétion en mode insertion (<C-n>)
set completeopt+=longest

" omnicompletion: CTRL-X CTRL-O
set ofu=syntaxcomplete#Complete

"set complete=.,t,i,b,w,k

" -----------------------------------------------------------
" Statusline, Menu
" -----------------------------------------------------------

set ruler         " show cursor position below each window
set showmode      " shows the current status (insert, visual, ...) in statusline
set shortmess=a   " use shortest messages
set showcmd       " Affiche les commandes dans la barre de status
set wc=<TAB>      " use tab for auto-expansion in menus
set wmnu          " show a list of all matches when tabbing a command

set laststatus=2  " show always statusline of last window
set statusline=%<%t\(%n\)\ %y%h%m%r%=[%l,%c]\ %P
"set statusline=%<%f%m\ %r\ %h\ %w%=%l,%c\ %p%%
"set statusline=[%l,%c\ %P%M]\ %f\ %r%h%w

" When doing tab completion, give the following files lower priority. You may
" wish to set 'wildignore' to completely ignore files, and 'wildmenu' to enable
" enhanced tab completion. These can be done in the user vimrc file.
set suffixes+=.info,.aux,.log,.dvi,.bbl,.out,.o,.lo

" how command line completion works
"set wildmode=list:longest

" ignore some files for filename completion
set wildignore=*.o,*.r,*.so,*.sl,*.tar,*.tgz,*.pyc,*~

set wildchar=<tab>
set wildmenu
set wildmode=longest:full,full

" -----------------------------------------------------------
" window handling
" -----------------------------------------------------------

set wh=1            " minimal number of lines used for the current window
set wmh=0           " minimal number of lines used for any window
set noequalalways   " make all windows the same size when adding/removing windows
set splitbelow      " a new window is put below the current one

" Raccourcis claviers pour les Tabs
map <C-Down>  :tabnew <CR>
nmap <C-Right> :tabnext <CR>
nmap <C-Left>  :tabprevious <CR>

" -----------------------------------------------------------
" Sauvegarde
" -----------------------------------------------------------

set backup
set backupdir=~/.vim/backup/
set dir=~/.vim/backup   " pour le fichier d'échange

" -----------------------------------------------------------
" UNIX Specials
" -----------------------------------------------------------
if has("unix")
   " path: répertoires utilisés lors d'une recherche et autres commandes
   set path=.,/usr/include,usr/local/include
   "set clipboard=autoselect
   set shell=/bin/bash
endif

" -----------------------------------------------------------
" Mapping
" -----------------------------------------------------------

" sets leader to ',' and localleader to "\"
let mapleader=","
let maplocalleader="\\"

" Don't use Ex mode, use Q for formatting
map Q gq

" space bar un-highligts search
:noremap <silent> <Space> :silent noh<Bar>echo<CR>

" Allows writing to files with root priviledges
cmap w!! %!sudo tee > /dev/null %

" CTRL-U in insert mode deletes a lot.  Use CTRL-G u to first break undo,
" so that you can undo CTRL-U after inserting a line break.
inoremap <C-U> <C-G>u<C-U>

" Supprime tout les blancs en debut de ligne
nmap _S :%s/^\s\+//<CR>

" Deplace la ligne courante vers le bas
nmap _j :move .+1<CR>
" Deplace la ligne courante vers le haut
nmap _k :move .-2<CR>

" Converts file format to/from unix
"command Unixformat :set ff=unix
"command Dosformat :set ff=dos

" On fait tourner les tampons ...
nnoremap <C-N> :bn!<CR>
nnoremap <C-P> :bp!<CR>

" Annuler aka Undo (window$'s style)
inoremap <C-Z> <C-O>u
noremap <C-Z> u

" Refaire aka Redo (window$'s style)
" Supprimer car on confit avec le scroll montant
"noremap <C-Y> <C-R>
"inoremap <C-Y> <C-O><C-R>

" Scroll vers le bas sans bouger le curseur (window$'s style)
"map <C-DOWN> <C-E>
" Scroll vers le haut sans bouger le curseur (window$'s style)
"map <C-UP> <C-Y>

" Tout séléctionner (window$'s style)
noremap <C-A> gggH<C-O>G
"inoremap <C-A> <C-O>gg<C-O>gH<C-O>G
cnoremap <C-A> <C-C>gggH<C-O>G

" Indentation automatique (Emacs's style)
vnoremap <C-F>   =$
vnoremap <tab>   =
nnoremap <tab>   =$
nnoremap <C-tab> mzvip=`z

" Fermer fichier (tampon) (window$'s style)
map <C-F4> :bd<cr>
imap <C-F4> <C-O>:bd<cr>
cmap <c-F4> <c-c>:bd<cr>

" Sauvegarder fichier (touv's style)
noremap <c-x><c-s> w<cr>
inoremap <c-x><c-s> <C-O>:w<cr>
inoremap <C-s> <esc>:w<cr>a
nnoremap <C-s> :w<cr>

" Ouvrir un fichier (Emacs's style)
"noremap <c-x><c-f> :e!
"inoremap <c-x><c-f> <c-O>:e!

" pour vimdiff
map <A-Left> :diffget<CR>
map <A-Right> :diffput<CR>

" have <F1> prompt for a help topic, rather than displaying the introduction
" page, and have it do this from any mode:
nnoremap <F1> :help<Space>
vmap <F1> <C-C><F1>
omap <F1> <C-C><F1>
map! <F1> <C-C><F1>

" Force la fermeture d'un tampon
map <F4> :bd!<cr>
imap <F4> <C-O>:bd!<cr>
cmap <F4> <c-c>:bd!<cr>

" Fermer un tag (cf. closetag.vim)
"inoremap <F5> <C-R>=GetCloseTag()<CR>
"map <F5> a<C-_><ESC>

" Toggle le mode collage
set pastetoggle=<F5>

" Supprime tout les blancs en fin de ligne
map <F6> :%s/\s\+$//<CR>

" Mets en commentaire
map <F7> <Leader>c
imap <F7> <esc><F7>

" active/désactive la navigation par tags
nnoremap <silent> <F8> :Tlist<CR>

" noremap <F7> :set hlsearch!<cr>:set hlsearch?<cr>
" noremap <F5> :set expandtab!<cr>:set expandtab?<cr>
" noremap <F2> :set paste!<cr>:set paste?<cr>
" noremap <F3> :set nu!<cr>:set nu?<cr>

if has("gui_running")
   " Shift-Fleche pour selectionner un bloc
   map <S-Up> vk
   vmap <S-Up> k
   map <S-Down> vj
   vmap <S-Down> j
   map <S-Right> v
   vmap <S-Right> l
   map <S-Left> v
   vmap <S-Left> h

   if has("win32") || has("win16")
      " ...
   else
      " Couper aka Cut (generic's style)
      vmap <S-Del> "*x

      " Copier aka Copy (generic's style)
      "vmap <C-Insert> "*y
      "vmap <Return> "*y"               "Return realise la copie du bloc selectionner
      "vmap <S-Return> "*y" "Shift Return aussi

      " Coller aka Insert (generic's style)
      function! NormalPaste()
              if @* != ""
                 normal "*gP
              endif
      endfunction
      function! SelectPaste()
              if @* != ""
                 if col(".") < col("'<")
                    normal "*gp
                 else
                    normal "*gP
                 endif
              endif
      endfunction

      "map <S-Insert>            :call NormalPaste()<CR>
      "imap <S-Insert>           x<Esc>:call NormalPaste()<CR>s
      "cmap <S-Insert> <C-R>*
      "vmap <S-Insert>           "-x:call SelectPaste()<CR>
   endif
endif

" -----------------------------------------------------------
" Vimspell - Correction orthographique
" -----------------------------------------------------------

" Dictionnaire français
" Liste des propositions par CTRL-X_CTRL-K
"set dictionary+=/usr/share/dict/french

map <silent> <S-F10> "<Esc>:silent setlocal spell! spelllang=en<CR>"
map <silent> <F10> "<Esc>:silent setlocal spell! spelllang=fr<CR>"
map <silent> <C-F10> "<Esc>:silent setlocal nospell<CR>"
"if has("spell")
"    setlocal spell spelllang=
"    map ,lf :setlocal spell spelllang=fr<cr>
"    map ,le :setlocal spell spelllang=en<cr>
"    map ,ln :setlocal spell spelllang=<cr>
"endif

"let loaded_vimspell = 1
"set spellsuggest=10
"let spell_executable = "aspell"
"let spell_auto_type = ''
"let spell_insert_mode = 0

" -----------------------------------------------------------
" Plugins
" -----------------------------------------------------------

" MiniBuf plugin
let g:miniBufExplMapWindowNavVim = 1      " Control + [hjkl]
"let g:miniBufExplMapWindowNavArrows = 1   " Control + Arrow Keys
let g:miniBufExplMapCTabSwitchBufs = 1    " <C-TAB> and <C-S-TAB>
let g:miniBufExplModSelTarget = 1
let g:miniBufExplUseSingleClick = 1

map <Leader>b :MiniBufExplorer<cr>
map <Leader>c :CMiniBufExplorer<cr>
map <Leader>u :UMiniBufExplorer<cr>
map <Leader>t :TMiniBufExplorer<cr>

" Matchit - Permet notament la navigation de TAG en TAG en xml
" source ~/.vim/scripts/matchit.vim

" Set taglist plugin options
"let Tlist_Use_Right_Window = 1
"let Tlist_Exit_OnlyWindow = 1
"let Tlist_Compact_Format = 1
"let Tlist_Show_Menu = 1

" -----------------------------------------------------------
" Commande Automatique
" -----------------------------------------------------------
if has("autocmd")
   ":autocmd!   " Supprime TOUTES les autocmd pour le groupe <courant.

   " Enable file type detection.
   " Use the default filetype settings, so that mail gets 'tw' set to 72,
   " 'cindent' is on in C files, etc.
   " Also load indent files, to automatically do language-dependent indenting.
   filetype plugin indent on
   filetype plugin on

   " Put these in an autocmd group, so that we can delete them easily.
   augroup vimrcEx
      au!

      " Reload vimrc to apply changes
      autocmd! BufWritePost .vimrc source %

      " When editing a file, always jump to the last known cursor position.
      " Don't do it when the position is invalid or when inside an event handler
      " (happens when dropping a file on gvim).
      " Also don't do it when the mark is in the first line, that is the default
      " position when opening a file.
      autocmd BufReadPost *
            \ if line("'\"") > 1 && line("'\"") <= line("$") |
            \   exe "normal! g`\"" |
            \ endif

   augroup END

   " don't write swapfile on most commonly used directories for NFS mounts or USB sticks
   autocmd BufNewFile,BufReadPre /media/*,/mnt/* set directory=~/tmp,/var/tmp,/tmp

   " 11.1 Template
   " au BufNewFile *.xsl 0r~/.vim/templates/xsl.xsl
   " au BufNewFile *.xml 0r~/.vim/templates/xml.xml
   " au BufNewFile *.html 0r~/.vim/templates/html.html
   " au BufNewFile *.c 0r~/.vim/templates/c.c
   " au BufNewFile *.php 0r~/.vim/templates/php.php
   " shebang automatique lors de l'ouverture nouveau
   " d'un fichier *.py, *.sh (bash), modifier l'entête selon les besoins :
   autocmd BufNewFile *.sh,*.bash 0put =\"#!/bin/bash\<nl># -*- coding: UTF8 -*-\<nl>\<nl>\"|$
   autocmd BufNewFile *.py 0put=\"#!/usr/bin/env python\"|1put=\"# -*- coding: UTF8 -*-\<nl>\<nl>\"|$


   " 11.2 En fonction du type de fichier
   autocmd FileType text setlocal textwidth=78 lbr "fo+=a "spell spelllang=fr     " Text
   autocmd FileType tex setlocal textwidth=78 "spell spelllang=fr                " Tex
   autocmd FileType camptocamp setlocal spell spelllang=fr                                " C2C
   autocmd FileType css setlocal expandtab shiftwidth=2 tabstop=2 softtabstop=2 " CSS
   autocmd FileType c,cpp,slang setlocal cindent                                                " C, C++
   autocmd FileType ruby setlocal expandtab shiftwidth=2 tabstop=2 softtabstop=2 " Ruby
   autocmd FileType php setlocal shiftwidth=8 tabstop=8 softtabstop=8            " PHP
   autocmd FileType html,xml setlocal expandtab shiftwidth=2 tabstop=2 softtabstop=2   " HTML
   autocmd FileType rst setlocal expandtab shiftwidth=4 tabstop=4 softtabstop=4        " ReST
   autocmd FileType javascript setlocal expandtab shiftwidth=2 tabstop=2 softtabstop=2 " JS

   "autocmd Filetype html,xml,xsl source ~/.vim/plugin/closetag.vim              " HTML ...
   "autocmd Filetype idlang :source ~/.vim/syntax/idlang.vim "IDL mode"          " IDL
   "autocmd FileType py source ~/.vim/scripts/python.vim
   "autocmd FileType php set dictionary=~/.vim/dictionaries/PHP.dict keywordprg=~/.vim/external/phpmanual.sh

   " python support
   " --------------
   "  don't highlight exceptions and builtins. I love to override them in local
   "  scopes and it sucks ass if it's highlighted then. And for exceptions I
   "  don't really want to have different colors for my own exceptions ;-)
   autocmd FileType python setlocal expandtab shiftwidth=4 tabstop=8 softtabstop=4 smartindent cinwords=if,elif,else,for,while,try,except,finally,def,class,with
   let python_highlight_all=1
   let python_highlight_exceptions=0
   let python_highlight_builtins=0

endif

" -----------------------------------------------------------
" Some tricks for mutt
" -----------------------------------------------------------

" F1 through F3 re-wraps paragraphs
augroup MUTT
   au BufRead ~/.mutt/temp/mutt* set tw=72 spell spelllang=fr
   au BufRead ~/.mutt/temp/mutt* nmap  <F1>  gqap
   au BufRead ~/.mutt/temp/mutt* nmap  <F2>  gqqj
   au BufRead ~/.mutt/temp/mutt* nmap  <F3>  kgqj
   au BufRead ~/.mutt/temp/mutt* map!  <F1>  <ESC>gqapi
   au BufRead ~/.mutt/temp/mutt* map!  <F2>  <ESC>gqqji
   au BufRead ~/.mutt/temp/mutt* map!  <F3>  <ESC>kgqji
augroup END

" -----------------------------------------------------------
" Spécificité pour chaque language
" -----------------------------------------------------------

" Langage C
"let c_minlines = 200
let c_comment_strings = 1
"set cinoptions=(0                       " Options d'indentation pour un fichier C
" some nice options for cindenting      by FOLKE
set cinoptions={.5s,+.5s,t0,n-2,p2s,(03s,=.5s,>1s,=1s,:1s
"g:C_CCompiler             "gcc"
"g:C_CplusCompiler         "g++"
"g:C_CFlags                 "-Wall -g -O0 -c"
"g:C_LFlags                 "-Wall -g -O0"

" Langage PHP
let php_sql_query = 1
let php_noShortTags = 1
let php_parent_error_close = 1
"let php_parent_error_open = 1
"let php_minlines=300
"let php_htmlInStrings=1
"let php_folding = 1
let php_sql_query = 1            "Coloration des requetes SQL
let php_htmlInStrings = 1        "Coloration des balises html

" Javascript
let javascript_enable_domhtmlcss=1

" Le plugin 2html utilise le CSS
let html_use_css = 1

" For syntaxt/2html.vim
let use_xhtml=1
let html_ignore_folding=1

" -----------------------------------------------------------
" Functions
" -----------------------------------------------------------

" Aller dans le répertoire du fichier édité.
function! ChangeToFileDirectory()
        if bufname("") !~ "^ftp://" " C'est impératif d'avoir un fichier local !
                lcd %:p:h
        endif
endfunction

map ,fd :call ChangeToFileDirectory()<CR>

" -----------------------------------------------------------
" A voir
" -----------------------------------------------------------

" Génération des tags pour les fichiers à la C++
"map <F11> :!ctags -R --c++-kinds=+p --fields=+iaS --extra=+q .<CR>

" Cscope est un outil similaire à ctags, mais voyez-le plutôt comme un ctags
" sous amphétamines, car il est capable de bien plus.
" if has("cscope") && filereadable("/usr/bin/cscope")
"    set csprg=/usr/bin/cscope
"    set csto=0
"    set cst
"    set nocsverb
"    " add any database in current directory
"    if filereadable("cscope.out")
"       cs add cscope.out
"       " else add database pointed to by environment
"    elseif $CSCOPE_DB != ""
"       cs add $CSCOPE_DB
"    endif
"    set csverb
" endif

" Convenient command to see the difference between the current buffer and the
" file it was loaded from, thus the changes you made.
" Only define it when not defined already.
if !exists(":DiffOrig")
  command DiffOrig vert new | set bt=nofile | r # | 0d_ | diffthis
                  \ | wincmd p | diffthis
endif

" -----------------------------------------------------------
" Include
" -----------------------------------------------------------

" Inclusion d'un autre fichier avec des options
"if filereadable(expand("~/.vimrc_local.vim"))
"    source ~/.vimrc_local.vim
"endif

" vim:fdm=marker

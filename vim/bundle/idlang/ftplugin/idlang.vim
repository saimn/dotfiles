" Marshall Perrin's IDL vimrc file
" Note that this IDL is RSI's Interactive Data Language, 
" *NOT* the Interface Definition Language
" 
" Permission to copy, use, and modify is freely given.
" 
" This file loosely based on vimrc files originally by Ken Shan.

" source updated/hacked language syntax file
":source ~/.vim/idl-syntax.vim


" enable smart matching of begin/end pairs
:source $VIMRUNTIME/macros/matchit.vim

" Basic settings
set tabstop=4 textwidth=80
set formatoptions=croq2
"set comments=sr:/*,mb:*,el:*/,b://
set comments=b:;,b:;;
set smartindent autoindent nocindent
set cinwords=if,else,while,do,for,switch,pro,function,begin,PRO,FUNCTION
set ignorecase smarttab noexpandtab nolinebreak
set nowrap

" enable tags checking
set tags=./tags,tags,../tags,~mperrin/idl/tags

" Basic abbreviations:
imap `w while () do begin <CR><CR><C-H>endwhile<ESC>2k11hi
imap `for for =0L do begin<CR><CR><C-H>endfor <ESC>2k$11hi
imap `func FUNCTION <CR><CR><C-H>end<ESC>2k$a
imap `pro PRO <CR><CR><C-H>end<ESC>2k$a
imap `if if  then <ESC>5hi
imap `stop if keyword_set(stop) then stop
imap `IF if  then begin<CR><CR><C-H>endif else begin<CR><CR>endelse<ESC>4k$10hi
imap `beg begin <CR><C-H>end<ESC>O<Tab>
imap `ks keyword_set()<LEFT>
imap `nks ~(keyword_set())<LEFT><LEFT>
imap `ne n_elements()<LEFT>
imap `pn ptr_new()<LEFT>
imap `sz sz = size()<LEFT>
imap `fl fltarr()<LEFT>
imap `fn for =0L,n_elements()-1 do begin<CR><CR><C-H>endfor <ESC>2k$11hi
imap `fc fsc_color('')<LEFT><LEFT>
imap `cfc color=fsc_color('')<LEFT><LEFT>
imap `; ;*******************************************************************
"imap `: ;**************************** ***************************
"imap `PT ;**************************** ***************************<CR>;*<CR>;*ARGS:<CR>;*<CR>;*******************************************************************<CR><CR>PRO <CR><CR><C-H>end<ESC>2k$a
imap `doc ;+<CR>; NAME: <CR><CR>INPUTS:<CR>KEYWORDS:<CR>OUTPUTS:<CR><CR>HISTORY:<CR><Tab>Began <ESC>_DJ3x$a by Marshall Perrin <CR><C-H><C-H>-<CR><C-H><ESC>8<UP>$a
imap `" "++"<LEFT><LEFT>
imap `ei endif else begin
imap `ee endelse


nnoremap ;; ^i;<ESC>


" `- in visual, insert or normal mode: Insert dash rows for documentation block
imap `- ;----------------------------------------------------------------------<CR>;----------------------------------------------------------------------<Esc><CR>O// 
nmap `- O`-
vmap `- y`-<C-R>"<CR><Tab>

" `/ in insert or normal mode: Move over to // comment at column 41
imap `/ <ESC>`/
nmap `/ :set textwidth=80 nohlsearch<CR>:s@[ \\t]*\\(// \\=\\(.*\\)\\)\\=$@                                            // \\2@<CR>41\\|dt/A

" `/ in visual mode: Make visual selection into // comment block
"vmap `/ :s/^\\(    \\\|\\t\\)*/&\\/\\/ /<CR>

" `\ in visual mode: Uncomment visually selected /***/ or // comment block
"vmap `\\ :s/^\\(\\(    \\\|\\t\\)*\\)\\(\\/\\/ \\\| \\*  \\)/\\1<CR>:'\<,'\>g/^\\(    \\\|\\t\\)*\\(\\/\\*\\\| \\*\\/\\)\\( \\\|\\t\\)*$/d<CR>

" `v in normal mode: Select this identifier in visual mode
nmap `v :set nohlsearch<CR>l?[A-Za-z0-9_:~]\\+<CR>/[^A-Za-z0-9_:~]<CR>hv``

" Clean up when leaving buffer
cnoremap _~DoBufLeave~ <C-U>iunmap `a<CR>:iunmap `d<CR>:iunmap `n<CR>:iunmap `w<CR>:iunmap `f<CR>:iunmap `i<CR>:iunmap `\<CR><CR>:vunmap `0<CR>:iunmap `-<CR>:nunmap `-<CR>:vunmap `-<CR>:iunmap `/<CR>:nunmap `/<CR>:vunmap `/<CR>:vunmap `\\\\<CR>:nunmap `v<CR>:cunmap <C-V>_~DoBufLeave~

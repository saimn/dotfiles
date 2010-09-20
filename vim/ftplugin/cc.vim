" Ken Shan's C/C++ mode vimrc file <ken@digitas.harvard.edu>
" $Id: cc.vim,v 1.3 2003/01/21 03:52:41 mperrin Exp $

" Basic settings
set tabstop=4 textwidth=80
set formatoptions=croq2
set comments=sr:/*,mb:*,el:*/,b://
set nosmartindent noautoindent cindent
set cinwords=if,else,while,do,for,switch
set noignorecase smarttab noexpandtab nolinebreak

" for use in compiling:
set autowrite 


cnoremap make<CR> :make<CR>:source ~/.vim/makemode.vim<CR>
" Basic abbreviations:
"	`a: brace pair	`d: #define	`n: #include
"	`w: while 	`f: for 	`i: if
"	`<CR>: open brace pair below current line
imap `a {<CR>}<Esc>O
imap `d <ESC>a#define 
imap `n <ESC>a#include 
imap `w while ()<ESC>i
imap `f for ()<ESC>i
imap `i if ()<ESC>i
imap `<CR> <ESC>o`a

" `0 in visual mode: Comment out visual selection with #if 0 ... #endif
vmap `0 <Esc>'<O#if 0<Esc>'>o#endif<Esc><C-O><C-O>

" `- in visual, insert or normal mode: Insert dash rows for documentation block
imap `- //----------------------------------------------------------------------<CR>//----------------------------------------------------------------------<Esc>:set textwidth=72<CR>O// 
imap `doc /**********************************************************************<CR><TAB>Function: <CR><CR>Description: <CR><CR>Arguments: <CR><CR><C-H><C-H>**********************************************************************/<Esc>:set textwidth=72<CR>6<up>$a
nmap `- O`-
vmap `- y`-<C-R>"<CR><Tab>

" `* in insert or normal mode: Insert /***/ comment block
" (works only with the comment recognition setting above)
nmap `* _2O/*<CR><CR><BS>/<Up><Space>
imap `* <ESC>`*

" `* in visual mode: Make visual selection into /***/ comment block
vmap `* :s/^\\(    \\\|\\t\\)*/& *  /<CR>'<O<BS><BS><BS><BS>/*<Esc>'>o<BS><BS>/<Esc><C-O><C-O>

" `/ in insert or normal mode: Move over to // comment at column 41
imap `/ <ESC>`/
nmap `/ :set textwidth=80 nohlsearch<CR>:s@[ \\t]*\\(// \\=\\(.*\\)\\)\\=$@                                            // \\2@<CR>41\\|dt/A

" `/ in visual mode: Make visual selection into // comment block
vmap `/ :s/^\\(    \\\|\\t\\)*/&\\/\\/ /<CR>

" `\ in visual mode: Uncomment visually selected /***/ or // comment block
vmap `\\ :s/^\\(\\(    \\\|\\t\\)*\\)\\(\\/\\/ \\\| \\*  \\)/\\1<CR>:'\<,'\>g/^\\(    \\\|\\t\\)*\\(\\/\\*\\\| \\*\\/\\)\\( \\\|\\t\\)*$/d<CR>

" `v in normal mode: Select this identifier in visual mode
nmap `v :set nohlsearch<CR>l?[A-Za-z0-9_:~]\\+<CR>/[^A-Za-z0-9_:~]<CR>hv``

" Syntax highlighting
if has("syntax")
  syntax clear
  "if bufname =~ "\\.\\(l\\|lex\\)$"
  "  source $VIMRUNTIME/syntax/lex.vim
  "elseif bufname =~ "\\.\\(y\\|yacc\\)$"
  "  source $VIMRUNTIME/syntax/yacc.vim
  "else
    source $VIMRUNTIME/syntax/cpp.vim
  "endif
  syntax keyword cNumber NULL
  syntax keyword cppClass class
  syntax keyword cTodo contained TODO REVIEW
  syntax match   cTodo contained "@@@.*"
  highlight! link cppClass Structure
endif

" Clean up when leaving buffer
cnoremap _~DoBufLeave~ <C-U>iunmap `a<CR>:iunmap `d<CR>:iunmap `n<CR>:iunmap `w<CR>:iunmap `f<CR>:iunmap `i<CR>:iunmap `\<CR><CR>:vunmap `0<CR>:iunmap `-<CR>:nunmap `-<CR>:vunmap `-<CR>:iunmap `/<CR>:nunmap `/<CR>:vunmap `/<CR>:vunmap `\\\\<CR>:nunmap `v<CR>:cunmap <C-V>_~DoBufLeave~

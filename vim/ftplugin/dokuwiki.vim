"Mappings {{{1
"
"This should allow you to press ",,h1" in INSERT mode, then
"type your header, then press <C-j> to jump to the next
"line.  
"
"(If <C-j> doesn't jump for you, let me know; this
"may require latex-suite (http://vim-latex.sf.net), but that
"would be a hefty thing to download just for this.)
"
"Darn huge DokuWiki headings:
imap <buffer> ,,h1 ====== <++> ======<++><Esc>k0<C-J>
imap <buffer> ,,h2 ===== <++> =====<++><Esc>k0<C-J>
imap <buffer> ,,h3 ==== <++> ====<++><Esc>k0<C-J>
imap <buffer> ,,h4 === <++> ===<++><Esc>k0<C-J>
imap <buffer> ,,h5 == <++> ==<++><Esc>k0<C-J>
imap <buffer> ,,h6 = <++> =<++><Esc>k0<C-J>

"promote and demote
imap <buffer> ,,hd <Esc>:s#=\(.*\)=#\1#<CR>:let @/ = ""<CR>
nmap <buffer> ,,hd :s#=\(.*\)=#\1#<CR>:let @/ = ""<CR>
imap <buffer> ,,hp <Esc>:s#\(.*\)#=\1=#<CR>:let @/ = ""<CR>
nmap <buffer> ,,hp :s#\(.*\)#=\1=#<CR>:let @/ = ""<CR>

"}}}1



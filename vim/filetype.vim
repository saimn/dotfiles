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

"au BufNewFile,BufRead *.rb,*.rbw,*.gem,*.gemspec	set filetype=ruby " Ruby
"au BufNewFile,BufRead *.builder,*.rxml,*.rjs		set filetype=ruby " ROR
"au BufNewFile,BufRead [rR]akefile,*.rake		set filetype=ruby       " Rakefile
"au BufNewFile,BufRead [rR]antfile,*.rant		set filetype=ruby       " Rantfile
"au BufNewFile,BufRead *.erb,*.rhtml			set filetype=eruby      " eRuby

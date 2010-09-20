" filetypes

augroup filetypedetect
   autocmd BufRead,BufNewFile *.txt setfiletype text
   autocmd BufRead,BufNewFile *.tpl setfiletype html
   autocmd BufRead,BufNewFile *.pro setfiletype idlang
   autocmd BufRead,BufNewFile *.mkd setfiletype mkd
   autocmd BufRead,BufNewFile *.wiki setfiletype Wikipedia
   autocmd BufRead,BufNewFile *wikipedia.org* setfiletype Wikipedia
   autocmd BufRead,BufNewFile *climbr.fr* setfiletype dokuwiki
   autocmd BufRead,BufNewFile *camptocamp.org* setfiletype camptocamp
   autocmd BufNewFile,BufRead vimperator* setfiletype bbcode
   autocmd BufRead,BufNewFile *conky* setfiletype conkyrc
   autocmd BufNewFile,BufRead *.inc setfiletype php
   autocmd BufNewFile,BufRead *.sys setfiletype php
   autocmd BufNewFile,BufRead *.muttrc setfiletype muttrc
augroup END

"au BufNewFile,BufRead *.rb,*.rbw,*.gem,*.gemspec	set filetype=ruby " Ruby
"au BufNewFile,BufRead *.builder,*.rxml,*.rjs		set filetype=ruby " ROR
"au BufNewFile,BufRead [rR]akefile,*.rake		set filetype=ruby       " Rakefile
"au BufNewFile,BufRead [rR]antfile,*.rant		set filetype=ruby       " Rantfile
"au BufNewFile,BufRead *.erb,*.rhtml			set filetype=eruby      " eRuby

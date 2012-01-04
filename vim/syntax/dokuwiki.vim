" DokuWiki Vim Syntaxfile
" Language:      DokuWiki Pages (http://wiki.splitbrain.org/wiki:dokuwiki)
" Maintainer:    Michael Klier <chi@chimeric.de>
" URL:           http://www.chimeric.de/projects/dokuwiki/vimsyntax
" Last Change:   May 20, 2008 21:29:12

if !exists("main_syntax")
  if version < 600
    syntax clear
  elseif exists("b:current_syntax")
    finish
  endif
  let main_syntax = "dokuwiki"
endif

syn case ignore
syn spell toplevel
syn sync linebreaks=1

syn match DokuHeadline          #^ \=\(=\{2,6}\)\(=\)\@!.\+\1 *$#
syn match DokuRule              #^ \=-\{4,} *$#
syn match DokuMonospaced        #''[^'[\]]\+''#
syn match DokuNewLine           #\\\\\( \{1,}\|$\)#
syn match DokuSmileys           #8-)\|8-O\|:-(\|:-)\|=)\|:-/\|:-\\\|:-?\|:-D\|:-P\|:-O\|:-X\|:-|\|;-)\|:?:\|:!:\|\^_\^\|LOL\|FIXME\|DELETEME#
syn match DokuList              #^\( \{2}\| \{4}\| \{6}\| \{8}\| \{10}\| \{12}\| \{14}\| \{16}\| \{18}\| \{20}\| \{22}\| \{24}\)\(\-\|\*\)# 
syn match DokuNoWiki            #%%[^%]\+%%#
syn match DokuQuote             #^>\+#
syn match DokuLinKInterwiki     #[a-z]\+># contained
syn match DokuLinkExternal      #\(http\|https\)\=\(://\)\=\(www\.\)\(\a\|-\)\+\(\.\{1}\l\{2,3}\)\=[^| \]]*#
syn match DokuLinkMail          #<[^@]\+@[^>]\+>#
syn match DokuLinkTitle         #|\zs[^|\]{}]\+# contained
syn match DokuImageMode         #?[^}]\+# contained
syn match DokuTableTH           #\^# contained
syn match DokuTableTD           #|# contained

syn region DokuLink             start=#\[\[# end=#\]\]# contains=DokuLinkInterwiki,DokuLinkExternal,DokuLinkTitle,DokuMedia oneline
syn region DokuMedia            start=#{{#   end=#}}# contains=DokuLinkExternal,DokuImageMode,DokuLinkTitle oneline

syn region DokuSub              start=#<sub># end=#</sub># keepend
syn region DokuSup              start=#<sup># end=#</sup># keepend 
syn region DokuDel              start=#<del># end=#</del># keepend 
syn region DokuFootnote         start=#((#    end=#))# keepend

syn region DokuFileGeneric matchgroup=DokuFileMatch start=#<file># end=#</file># keepend
syn region DokuCodeGeneric matchgroup=DokuCodeMatch start=#<code># end=#</code># keepend

syn region DokuBold             start=#\*\*# end=#\*\*# contains=DokuItalic,DokuUnderlined,DokuLink oneline
syn region DokuItalic           start=#//# end=#//# skip=#://# contains=DokuBold,DokuUnderlined,DokuLink oneline
syn region DokuUnderlined       start=#__# end=#__# contains=DokuBold,DokuItalic,DokuLink oneline
syn region DokuTableTH          start=#\^# end=#\^\|# contains=DokuTableTH,DokuLink,DokuMedia,DokuBold,DokuItalic,DokuUnderlined,DokuMonospaced,DokuSmileys,DokuNewLine oneline
syn region DokuTableTD          start=#|#  end=#|\|# contains=DokuTableTD,DokuLink,DokuMedia,DokuBold,DokuItalic,DokuUnderlined,DokuMonospaced,DokuSmileys,DokuNewLine oneline

" TODO make other syntax work inside codeblocks
" syn include @PHP syntax/php.vim
" syn region DokuPHP matchgroup=DokuCodeMatch start=#<code php># end=#</code># contains=@PHP
" syn include @PERL syntax/perl.vim
" syn region DokuPERL matchgroup=DokuCodeMatch start=#<code perl># end=#</code># contains=@PERL
" syn include @HTML syntax/html.vim
" syn region DokuHTML matchgroup=DokuCodeMatch start=#<html># end=#</html># contains=@HTML
" syn include @JAVA syntax/java.vim
" syn region DokuJAVA matchgroup=DokuCodeMatch start=#<code java># end=#</code># contains=@JAVA

" costum color groups
hi DokuHeadline           term=bold,underline cterm=bold,underline gui=bold,underline
hi DokuNewLine            term=bold cterm=bold ctermfg=Red gui=bold guifg=Red
hi DokuRule               term=bold cterm=bold ctermfg=Red gui=bold guifg=Red
hi DokuQuote              term=bold cterm=bold ctermfg=Red gui=bold guifg=Red
hi DokuLink               term=bold cterm=bold ctermfg=Green gui=bold guifg=Green
hi DokuMedia              term=bold cterm=bold ctermfg=Yellow gui=bold guifg=Darkyellow
hi DokuLinkInterwiki      term=bold cterm=bold ctermfg=Red gui=bold guifg=Red
hi DokuImageMode          term=bold cterm=bold ctermfg=Red gui=bold guifg=Red
hi DokuLinkExternal       term=bold cterm=bold ctermfg=Magenta gui=bold guifg=Magenta
hi DokuLinkMail           term=bold cterm=bold ctermfg=Magenta gui=bold guifg=Magenta
hi DokuLinkTitle          term=bold cterm=bold ctermfg=Blue gui=bold guifg=Blue
hi DokuSmileys            term=bold ctermbg=Yellow ctermfg=Black guibg=Yellow guifg=Black
hi DokuItalic             term=italic cterm=italic gui=italic
hi DokuBold               term=bold cterm=bold gui=bold
hi DokuUnderlined         term=underline cterm=underline gui=underline
hi DokuSub                term=bold ctermbg=LightBlue ctermfg=Black guibg=LightBlue guifg=Black
hi DokuSup                term=bold ctermbg=LightBlue ctermfg=Black guibg=LightBlue guifg=Black
hi DokuDel                term=bold ctermbg=LightRed ctermfg=Black guibg=LightRed  guifg=Black
hi DokuFootnote           term=bold ctermfg=LightGreen guifg=LightGreen
hi DokuList               term=bold cterm=bold ctermfg=Green gui=bold guifg=Green
hi DokuTableTH            term=bold cterm=bold ctermfg=Blue gui=bold guifg=Blue
hi DokuTableTD            term=bold cterm=bold ctermfg=Blue gui=bold guifg=Blue
hi DokuNoWiki             ctermbg=Gray ctermfg=Black guibg=Gray guifg=Black

" color groups
hi DokuBlockColor         ctermbg=Gray ctermfg=Black guibg=Gray guifg=Black
hi DokuFormatColor        cterm=bold ctermfg=Gray gui=bold guifg=Gray

" link to groups 
hi link DokuMonospaced DokuFormatColor
hi link DokuFile       DokuBlockColor
hi link DokuCode       DokuBlockColor
hi link DokuCodeMatch  DokuBlockColor
hi link DokuFileMatch  DokuBlockColor
hi link DokuNoWiki     DokuBlockColor

" FIXME is this right, or could it be omited?
if main_syntax == "dokuwiki"
  syn sync minlines=2
endif

let b:current_syntax = "dokuwiki"

if main_syntax == 'dokuwiki'
  unlet main_syntax
endif

" vim:ts=2:sw=2:et:enc=utf-8:

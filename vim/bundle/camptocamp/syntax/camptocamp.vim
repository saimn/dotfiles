" Camptocamp.org syntax file
" Language: Mix of BBCode & Markdown
" Maintainer: Saïmon <contact at saimon dot org>
" URL: http://github.com/saimn/c2c-tools/
" Version: 1
" Last Change: 2013-12-24
" Remark: Uses HTML syntax file. Based on Markdown & BBCode syntax files.

if exists("b:current_syntax")
  finish
endif

" Read the HTML syntax to start with
runtime! syntax/html.vim
unlet b:current_syntax

syn case ignore
syn spell toplevel

" bbcode formatting
syn match bbcodeItem contained "\[\s*[-a-zA-Z0-9]\+"hs=s+1 contains=@NoSpell
syn match bbcodeItem contained "\[/\s*[-a-zA-Z0-9]\+"hs=s+2 contains=@NoSpell
syn match bbcodeItem contained "\[\s*\*\s*\]"hs=s+1,he=e-1 contains=@NoSpell
syn match bbcodeArgument contained "\s[-a-zA-Z0-9]\+\s*="ms=s+1,me=e-1 contains=@NoSpell
syn region bbcodeValue contained start="\"" end="\"" contains=@NoSpell
syn region bbcodeValue contained start="'" end="'" contains=@NoSpell
syn match bbcodeValue contained "=[\t ]*[^'" \t\]][^ \t\]]*"hs=s+1 contains=@NoSpell
syn region bbcodeTag start="\[/\{0,1}" end="\]" contains=@NoSpell,bbcodeItem,bbcodeArgument,bbcodeValue

syn region bbcodeBold start="\[b\]" end="\[/b\]"me=e-4 contains=bbcodeTag,bbcodeBoldItalic,bbcodeBoldUnderline
syn region bbcodeBoldItalic contained start="\[i\]" end="\[/i\]"me=e-4 contains=bbcodeTag,bbcodeBoldItalicUnderline
syn region bbcodeBoldItalicUnderline contained start="\[u\]" end="\[/u\]"me=e-4 contains=bbcodeTag
syn region bbcodeBoldUnderline contained start="\[u\]" end="\[/u\]"me=e-4 contains=bbcodeTag,bbcodeBoldUnderlineItalic
syn region bbcodeBoldUnderlineItalic contained start="\[i\]" end="\[/i\]"me=e-4 contains=bbcodeTag

syn region bbcodeItalic start="\[i\]" end="\[/i\]"me=e-4 contains=bbcodeTag,bbcodeItalicBold,bbcodeItalicUnderline
syn region bbcodeItalicBold contained start="\[b\]" end="\[/b\]"me=e-4 contains=bbcodeTag,bbcodeItalicBoldUnderline
syn region bbcodeItalicBoldUnderline contained start="\[u\]" end="\[/u\]"me=e-4 contains=bbcodeTag
syn region bbcodeItalicUnderline contained start="\[u\]" end="\[/u\]"me=e-4 contains=bbcodeTag,bbcodeItalicUnderlineBold
syn region bbcodeItalicUnderlineBold contained start="\[b\]" end="\[/b\]"me=e-4 contains=bbcodeTag

syn region bbcodeUnderline start="\[u\]" end="\[/u\]"me=e-4 contains=bbcodeTag,bbcodeUnderlineBold,bbcodeUnderlineItalic
syn region bbcodeUnderlineBold contained start="\[b\]" end="\[/b\]"me=e-4 contains=bbcodeTag,bbcodeUnderlineBoldItalic
syn region bbcodeUnderlineBoldItalic contained start="\[i\]" end="\[/i\]"me=e-4 contains=bbcodeTag
syn region bbcodeUnderlineItalic contained start="\[i\]" end="\[/i\]"me=e-4 contains=bbcodeTag,bbcodeUnderlineItalicBold
syn region bbcodeUnderlineItalicBold contained start="\[b\]" end="\[/b\]"me=e-4 contains=bbcodeTag

syn region bbcodeStrike start="\[s\]" end="\[/s\]"me=e-4 contains=@NoSpell,bbcodeTag
syn region bbcodeCite start="\[q\]" end="\[/q\]"me=e-4 contains=@NoSpell,bbcodeTag
syn region bbcodeMonospace start="\[c\]" end="\[/c\]"me=e-4 contains=@NoSpell,bbcodeTag

syn region bbcodeQuote start="\[quote\]" end="\[/quote\]"me=e-8 contains=@NoSpell,bbcodeTag
syn region bbcodeUrl start="\[url\s*[=\]]" end="\[/url\]"me=e-6 contains=@NoSpell,bbcodeTag
syn region bbcodeUrl start="\[email\]" end="\[/email\]"me=e-8 contains=@NoSpell,bbcodeTag
syn region bbcodeColor start="\[color\s*[=\]]" end="\[/color\]"me=e-8 contains=@NoSpell,bbcodeTag
syn region bbcodeAcronym start="\[acronym\s*[=\]]" end="\[/acronym\]"me=e-10 contains=@NoSpell,bbcodeTag

syn region bbcodeImage start="\[img\]" end="\[/img\]"me=e-6 contains=@NoSpell,bbcodeTag
syn region bbcodeImage start="\[img\s*[=\]]" end="\[/img\]"me=e-6 contains=@NoSpell,bbcodeTag


" wiki formatting
syn region htmlH1       start="^\s*##"                   end="\($\|#\+\)" contains=@Spell
syn region htmlH2       start="^\s*###"                  end="\($\|#\+\)" contains=@Spell
syn region htmlH3       start="^\s*####"                 end="\($\|#\+\)" contains=@Spell
syn region htmlH4       start="^\s*#####"                end="\($\|#\+\)" contains=@Spell
syn region htmlH5       start="^\s*######"               end="\($\|#\+\)" contains=@Spell
syn match  htmlH1       /^.\+\n=\+$/ contains=@Spell
syn match  htmlH2       /^.\+\n-\+$/ contains=@Spell

syn region wikiLink start="\[\[" end="\]\]" oneline contains=wikiLink

syn match  mkdListItem  "^\s*[-*+]\s\+"
syn match  mkdListItem  "^\s*\d\+\.\s\+"

" HTML highlighting

hi def link bbcodeTag Identifier
hi def link bbcodeItem Statement
hi def link bbcodeArgument Type
hi def link bbcodeValue Constant
hi def link bbcodeUrl Underlined
hi def link bbcodeImage Underlined

hi def link bbcodeStrike Comment
hi def link bbcodeCite String
hi def link bbcodeQuote String
hi def link bbcodeMonospace PreProc
hi def link bbcodeColor Special
hi def link bbcodeAcronym Special

hi def link mkdListItem Identifier

hi def link bbcodeBoldUnderlineItalic bbcodeBoldItalicUnderline
hi def link bbcodeItalicBold bbcodeBoldItalic
hi def link bbcodeItalicBoldUnderline bbcodeBoldItalicUnderline
hi def link bbcodeItalicUnderlineBold bbcodeBoldItalicUnderline
hi def link bbcodeUnderlineBold bbcodeBoldUnderline
hi def link bbcodeUnderlineBoldItalic bbcodeBoldItalicUnderline
hi def link bbcodeUnderlineItalic bbcodeItalicUnderline
hi def link bbcodeUnderlineItalicBold bbcodeBoldItalicUnderline

hi def link bbcodeBold htmlBold
hi def link bbcodeBoldItalic htmlBoldItalic
hi def link bbcodeBoldItalicUnderline htmlBoldUnderlineItalic
hi def link bbcodeBoldUnderline htmlBoldUnderline
hi def link bbcodeItalic htmlItalic
hi def link bbcodeItalicUnderline htmlUnderlineItalic
hi def link bbcodeUnderline htmlUnderline

hi def link wikiLink	Underlined

if !exists("html_my_rendering")
   hi def htmlBold                term=bold cterm=bold gui=bold
   hi def htmlBoldUnderline       term=bold,underline cterm=bold,underline gui=bold,underline
   hi def htmlBoldItalic          term=bold,italic cterm=bold,italic gui=bold,italic
   hi def htmlBoldUnderlineItalic term=bold,italic,underline cterm=bold,italic,underline gui=bold,italic,underline
   hi def htmlUnderline           term=underline cterm=underline gui=underline
   hi def htmlUnderlineItalic     term=italic,underline cterm=italic,underline gui=italic,underline
   hi def htmlItalic              term=italic cterm=italic gui=italic
endif

let b:current_syntax = "camptocamp"

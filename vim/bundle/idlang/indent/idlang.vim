" IDL (Interactive Data Language) indent file.
" Language: IDL (ft=idlang)
" Last change:	2009 Feb  9
" Maintainer: Alfred de Wijn <dwijn AT iluvatar.org>
" based on indent file by Aleksandar Jelenak <ajelenak AT yahoo.com>

" Only load this indent file when no other was loaded.
"if exists("b:did_indent")
"	finish
"endif
"let b:did_indent = 1

setlocal indentkeys=o,O,=~end,;
setlocal indentexpr=GetIdlangIndent(v:lnum)

" Only define the function once.
"if exists("*GetIdlangIndent")
"	finish
"endif

function! GetIdlangIndent(lnum)
	" First non-empty non-comment line above the current line.
	let pnum = prevnonblank(v:lnum-1)
	" v:lnum is the first non-empty line -- zero indent.
	while getline(pnum) =~ '^\s*;'
		let pnum = prevnonblank(pnum-1)
	endwhile
	if pnum == 0
		return 0
	endif

	" Current indent.
	let curind = indent(pnum)

	" Don't indent comments
"	if getline(v:lnum) =~ '^\s*;'
"		return curind
"	endif

	" Second non-empty non-comment line above the current line.
	let pnum2 = prevnonblank(pnum-1)
	while getline(pnum2) =~ '^\s*;'
		let pnum2 = prevnonblank(pnum2-1)
	endwhile

	" Indenting of continued lines.
	if getline(pnum) =~ '\$\s*\(;.*\)\=$'
		if getline(pnum2) !~ '\$\s*\(;.*\)\=$'
			let curind = curind+&sw
		endif
	else
		if getline(pnum2) =~ '\$\s*\(;.*\)\=$'
			let curind = curind-&sw
		endif
	endif

	" Indenting blocks of statements.
	let line = substitute(substitute(getline(pnum), '".{-}"', '', 'g'), "'.{-}'", '', 'g')
	if line =~? '^[^;]*\<\(\(\S\+:\|else\|do\|then\|repeat\)\s\+begin\|\(pro\|function\)\s\+\w\+\(\s*,\s*[a-zA-Z0-9_=]\+\)*\|\(case\|switch\)\s\+.*\s\+of\)\s*\(&\|;\|$\)'
		let curind = curind+&sw
	endif
	let line = substitute(substitute(getline(v:lnum), '".{-}"', '', 'g'), "'.{-}'", '', 'g')
	if line =~? '\(^\|&\)\s*end\(if\|else\|while\|for\|rep\|case\|switch\|\)\>'
		if curind >= &sw
			let curind = curind-&sw
		else
			let curind = 0
		endif
	endif
	return curind
endfunction


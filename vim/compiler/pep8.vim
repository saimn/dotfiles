" Installation:
"   Drop pep8.vim in ~/.vim/compiler directory. Ensure that your PATH
"   environment variable includes the path to 'pep8' executable.
"
"   Add the following line to the autocmd section of .vimrc
"
"      autocmd FileType python compiler pep8
"
" Usage:
"   pep8 is called after a buffer with Python code is saved. QuickFix
"   window is opened to show errors, warnings and hints provided by pep8.
"
"   Above is realized with :pep8 command. To disable calling pep8 every
"   time a buffer is saved put into .vimrc file
"
"       let g:pep8_onwrite = 0
"
"   Displaying code rate calculated by pep8 can be avoided by setting
"
"       let g:pep8_show_rate = 0
"
"   Openning of QuickFix window can be disabled with
"
"       let g:pep8_cwindow = 0
"
"   Of course, standard :make command can be used as in case of every
"   other compiler.
"


if exists('current_compiler')
  finish
endif
let current_compiler = 'pep8'

if !exists('g:pep8_onwrite')
    let g:pep8_onwrite = 1
endif

if !exists('g:pep8_show_rate')
    let g:pep8_show_rate = 1
endif

if !exists('g:pep8_cwindow')
    let g:pep8_cwindow = 1
endif

if exists(':Pep8') != 2
    command Pep8 :call Pep8(0)
endif

if exists(":CompilerSet") != 2          " older Vim always used :setlocal
  command -nargs=* CompilerSet setlocal <args>
endif

CompilerSet makeprg=(echo\ '[%]';\ pep8\ %)

" We could omit end of file-entry, there is only one file
" %+I... - include code rating information
" %-G... - remove all remaining report lines from quickfix buffer
CompilerSet efm=%f:%l:%c:\ %m

if g:pep8_onwrite
    augroup python
        au!
        au BufWritePost * call Pep8(1)
    augroup end
endif

function! Pep8(writing)
    if !a:writing && &modified
        " Save before running
        write
    endif

    if has('win32') || has('win16') || has('win95') || has('win64')
        setlocal sp=>%s
    else
        setlocal sp=>%s\ 2>&1
    endif

    " If check is executed by buffer write - do not jump to first error
    if !a:writing
        silent make
    else
        silent make!
    endif

    if g:pep8_cwindow
        cwindow
    endif

    call Pep8Evaluation()

    if g:pep8_show_rate
        echon 'code rate: ' b:pep8_rate ', prev: ' b:pep8_prev_rate
    endif
endfunction

function! Pep8Evaluation()
    let l:list = getqflist()
    let b:pep8_rate = '0.00'
    let b:pep8_prev_rate = '0.00'
    for l:item in l:list
        if l:item.type == 'I' && l:item.text =~ 'Your code has been rated'
            let l:re_rate = '\(-\?[0-9]\{1,2\}\.[0-9]\{2\}\)/'
            let b:pep8_rate = substitute(l:item.text, '.*rated at '.l:re_rate.'.*', '\1', 'g')
            " Only if there is information about previous run
            if l:item.text =~ 'previous run: '
                let b:pep8_prev_rate = substitute(l:item.text, '.*previous run: '.l:re_rate.'.*', '\1', 'g')
            endif
        endif
    endfor
endfunction

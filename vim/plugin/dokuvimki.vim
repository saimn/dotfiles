"-----------------------------------------------------------------------------
" Copyright (C) 2008 Michael Klier <chi@chimeric.de>
"
" This program is free software; you can redistribute it and/or modify
" it under the terms of the GNU General Public License as published by
" the Free Software Foundation; either version 2, or (at your option)
" any later version.
"
" This program is distributed in the hope that it will be useful,
" but WITHOUT ANY WARRANTY; without even the implied warranty of
" MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
" GNU General Public License for more details.
"
" You should have received a copy of the GNU General Public License
" along with this program; if not, write to the Free Software Foundation,
" Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.  
"
" Maintainer:   Michael Klier <chi@chimeric.de>
" Contributors: Julian Knauer <jpk@goatpr0n.de>
" URL:		    http://www.chimeric.de/projects/dokuwiki/dokuvim
"-----------------------------------------------------------------------------

" Command definitions
command! -nargs=1 DWEdit exec('py dwEdit(<f-args>)')
command! -nargs=? DWSend exec('py dwSend(<f-args>)')
command! -nargs=? DWList exec('py dwList(<f-args>)')
command! -nargs=* DWRevs exec('py dwRevs(<f-args>)')
command! -nargs=? DWBackLinks exec('py dwBacklinks(<f-args>)')
command! -nargs=0 -bar DWPageTree exec('py dwPageTreeWindowOpen()')
command! -nargs=0 DWAuth exec('py dwAuth()')

python <<EOF
# -*- coding: utf-8 -*-

__version__ = '2008-05-20';
__author__  = 'Michael Klier <chi@chimeric.de>'

import sys, re, datetime, vim
from urllib import urlencode


def dwEdit(wp, rev=''):
    """
    Edit a Wiki page, tries to fetch the page content beforehand if the page
    exists. The function requires a valid page name ie.: a:valid:pagename
    """

    if not dwXMLRPCCheck(): return
    
    global editorBuffer
    global editorWindowID

    global pageTreeBuffer
    global pageTreeBufferName

    # save page id for later use
    global svwp
    svwp = wp

    if pageTreeBuffer is vim.current.buffer:
        buf = editorBuffer
    else:
        buf = vim.current.buffer

    del buf[:]

    print >>sys.stdout, "Opening", wp, "for editing ..."

    winnum = vim.eval('bufwinnr("'+pageTreeBufferName+'")')

    if winnum != '-1':
        if vim.eval('winnr()') == winnum:
            id = int(editorWindowID) + 1
            wcmd = str(id) + 'wincmd w'
            vim.command(wcmd)
    
    # prepare environment
    vim.command('set encoding=utf-8')
    vim.command('setlocal textwidth=0')
    vim.command('setlocal wrap')
    vim.command('setlocal linebreak')
    vim.command('setlocal filetype=dokuwiki')
    vim.command('setlocal syntax=dokuwiki')
    vim.command('setlocal buftype=nofile')

    # we mimic DWs toolbar shortcuts
    vim.command('imap <M-1> ======  ======<ESC>bhi')
    vim.command('imap <M-2> =====  =====<ESC>bhi')
    vim.command('imap <M-3> ====  ====<ESC>bhi')
    vim.command('imap <M-4> ===  ===<ESC>bhi')
    vim.command('imap <M-5> ==  ==<ESC>bhi')
    vim.command('imap <M-B> ****<ESC>2ha')
    vim.command('imap <M-I> ////<ESC>2ha')
    vim.command('imap <M-U> ____<ESC>2ha')
    vim.command("imap <M-C> ''''<ESC>2ha")
    vim.command('imap <M-D> <del\></del><ESC>7hxa')
    vim.command('imap <M-L> [[]]<ESC>2ha')

    vim.command('map <silent> <M-E> :py dwIDLookup()<cr>')

    try:
        if rev:
            text = xmlrpc.wiki.getPageVersion(wp, int(rev))
        else:
            text = xmlrpc.wiki.getPage(wp)

        if text:
            lines = text.split("\n")
            buf[:] = map(lambda x: x.encode('utf-8'), lines)
        else:
            print >>sys.stdout, 'Creating new page: %s' % wp

    except xmlrpclib.Fault, err:
        print >>sys.stderr, 'DokuVimKi XML-RPC Error: %s' % err


def dwSend(sum=''):
    """
    Sends the current buffer to the wiki.
    """

    global editorBuffer
    global svwp

    params = {}
    if sum:
        params['sum'] = sum
    else:
        params['minor'] = 1

    text = "\n".join(editorBuffer)

    try:
        xmlrpc.wiki.putPage(svwp,text,params)

        if text:
            print >>sys.stdout, 'Page %s written!' % svwp
        else:
            print >>sys.stdout, 'Page %s removed!' % svwp

        dwPageTreeWindowRefresh()

    except xmlrpclib.Fault, err:
        print >>sys.stderr, 'DokuVimKi XML-RPC Error: %s' % err


def dwList(pattern=''):
    """
    Lists all pages of the Wiki. If you hit <Enter> the wiki page under the
    cursor is loaded into the current buffer.
    """

    if not dwXMLRPCCheck(): return

    global editorBuffer 

    list = []
    try:
        del editorBuffer[:]
        pages = xmlrpc.wiki.getAllPages()
        pages = map(lambda x: x.encode('utf-8'), pages)

        if pattern:
            p = re.compile(pattern)
            list = filter(p.search, pages)

        else:
            list = pages

        if len(list) > 0:
            editorBuffer[:] = list
            vim.command('map <enter> :py dwListEdit()<cr>')
        else:
            print >>sys.stderr, 'DokuVimKi Error: No matching pages found!'

    except xmlrpclib.Fault, err:
        print >>sys.stderr, 'DokuVimKi XML-RPC Error: %s' % err


def dwRevs(wp, first=0):
    """ 
    Get a list of available revisions of a wiki page. Pressing <Enter> will
    open the revision under the cursor in the a new buffer.
    """

    if not dwXMLRPCCheck(): return

    global svwp
    svwp = wp

    try:
        del vim.current.buffer[:]

        revs = xmlrpc.wiki.getPageVersions(wp, int(first))

        lines = []
        if len(revs) > 0:
            for rev in revs:
                line = "\t".join(map(lambda x: str(rev[x]), ['modified', 'version', 'ip', 'type', 'user', 'sum']))
                lines.append(line)
            
            vim.current.buffer[:] = lines
            vim.command('map <enter> :py dwRevEdit()<cr>')

        else:
            print >>sys.stderr, 'DokuVimKi Error: No revisions found for page: %s' % wp

    except xmlrpclib.Fault, err:
        print >>sys.stderr, 'DokuVimKi XML-RPC Error: %s' % err


def dwBacklinks(wp=''):
    """
    Fetches a list of Backlinks of a given wiki page.
    """

    if not dwXMLRPCCheck(): return

    if not wp:
        global svwp

        if svwp:
            wp = svwp

        else:
            print >>sys.stderr, 'DokuVimKi Error: You have to supply a pagename!'
            return

    try:
        del vim.current.buffer[:]

        blinks = xmlrpc.wiki.getBackLinks(wp)

        if len(blinks) > 0:

            for link in blinks:
                vim.current.buffer[:] = map(str, blinks)
           
            vim.command('map <enter> :py dwListEdit()<cr>')

        else:
            print >>sys.stderr, 'DokuVimKi Error: No backlinks found for page: %s' % wp

    except xmlrpc.Fault, err:
        print >>sys.stderr, 'DokuVimKi XML-RPC Error: %s' % err


def dwIDLookup():
    """ 
    Checks for a page link under the current cursor position and opens it for
    editing ... serious magic 8)
    """

    global svwp

    line = vim.current.line
    row, col = vim.current.window.cursor

    # look for link syntax on the left and right from the current curser position
    reL = re.compile('\[{2}[^]]*$') # opening link syntax
    reR = re.compile('^[^\[]*]{2}') # closing link syntax

    L = reL.search(line[:col])
    R = reR.search(line[col:])

    # if both matched we probably have a link
    if L and R:

        # sanitize match remove anchors and everything after '|'
        id = (L.group() + R.group()).strip('[]').split('|')[0].split('#')[0]

        # get namespace from current page
        ns = svwp.rsplit(':', 1)[0]

        # check if it's not and external/interwiki/share link
        if id.find('>') == -1 and id.find('://') == -1 and id.find('\\') == -1:

            # check if useshlash is used
            if id.find('/'):
                id = id.replace('/', ':')

            # this is _almost_ a rip off of DokuWikis resolve_id() function
            if id[0] == '.':
                re_sanitize = re.compile('(\.(?=[^:\.]))')
                id = re_sanitize.sub('.:', id)
                id = ns + ':' + id
                path = id.split(':')

                result = []
                for dir in path:
                    if dir == '..':
                        try:
                            if result[-1] == '..':
                                result.append('..')
                            elif not result.pop():
                                result.append('..')
                        except IndexError:
                            pass
                    elif dir and dir != '.' and not len(dir.split('.')) > 2:
                        result.append(dir)

                id = ':'.join(result)

            elif id[0] != ':':
                id = ns + ':' + id

            # we're done, open the page for editing
            dwEdit(id)


def dwListEdit():
    """
    Helper function for dwList().
    """

    row, col = vim.current.window.cursor
    wp = vim.current.buffer[row-1]

    dwEdit(wp)


def dwTreeEdit():
    """
    Helper function for dwPageTree().
    """

    global pageList

    row, col = vim.current.window.cursor
    wp = pageList[row-1]

    vim.command("unmap <enter>")
    dwEdit(wp)


def dwRevEdit():
    """
    Helper function for dwRevs().
    """

    global svwp

    row, col = vim.current.window.cursor
    rev = vim.current.buffer[row-1].split("\t")[1]

    vim.command("unmap <enter>")
    dwEdit(svwp, rev)


def dwPageTreeWindowCreate():
    """
    Creates a seperate Window for the PageTree.
    """

    winnum = vim.eval('bufwinnr("'+pageTreeBufferName+'")')

    if winnum != '-1':
        if vim.eval('winnr()') != winnum:
            vim.command(winnum+'wincmd w')
        return

    win_dir  = 'topleft vertical'

    try:
        win_width = vim.eval('g:DokuVimKi_TREEWIDTH')
    except vim.error:
        win_width = '35'

    try:
        foldcol_width = vim.eval('g:DokuVimKi_FOLDCOLWIDTH')
    except vim.error:
        foldcol_width = '3'

    bufnum = vim.eval('bufnr("'+pageTreeBufferName+'")')

    if bufnum == '-1':
        wcmd = pageTreeBufferName
    else:
        wcmd = '+buffer ' + bufnum
    
    vim.command('silent! '+win_dir+' '+win_width+' split '+wcmd)

    vim.command('setlocal foldenable')
    vim.command('setlocal foldminlines=0')
    vim.command('setlocal foldmethod=manual')
    vim.command('setlocal foldcolumn='+foldcol_width)
    vim.command('setlocal foldtext=v:folddashes.getline(v:foldstart)')

    # setup buffer environment
    vim.command('setlocal noreadonly')
    vim.command('setlocal filetype=taglist')
    vim.command('silent! setlocal buftype=nofile')
    vim.command('silent! setlocal bufhidden=delete')
    vim.command('silent! setlocal noswapfile')
    vim.command('silent! setlocal nobuflisted')
    vim.command('silent! setlocal nowrap')
    vim.command('silent! setlocal nonumber')
    vim.command('map <enter> :py dwTreeEdit()<cr>')


def dwPageTreeWindowOpen():
    """
    Opens the page tree window.
    """

    if not dwXMLRPCCheck(): return

    winnum = vim.eval('bufwinnr("'+pageTreeBufferName+'")')

    if winnum != '-1':
        if vim.eval('winnr()') != winnum:
            vim.command(winnum+'wincmd w')
        return

    curbuf_name  = vim.eval('fnamemodify(bufname("%"), ":p")')
    curbuf_ftype = vim.eval('getbufvar("%", "%filetype")')
    cur_lnum     = vim.eval('line(".")')

    dwPageTreeWindowCreate()
    dwPageTreeWindowRefresh()


def dwBuildFoldingTree(index=0, level=0, fold_start=0):
    """
    Mark tree levels and fold them
    """

    buffer = vim.current.buffer
    fold_end = 0

    while index < len(buffer):
        item = buffer[index]
        if item.count(' ') > level:
            index = dwBuildFoldingTree(index, item.count(' '), index)
            if index == None:
                break
        elif item.count(' ') < level:
            fold_end = index
            vim.command(str(fold_start)+','+str(fold_end)+'fold')
            return fold_end
        elif item.count(' ') == level:
            index += 1
    if index != None:
        vim.command(str(fold_start)+','+str(index)+'fold')


def dwPageTreeWindowRefresh():
    """
    Refreshes the PageTree.
    """

    global pageList
    global pageTreeBuffer
    global xmlrpc
    
    winnum = vim.eval('bufwinnr("'+pageTreeBufferName+'")')

    if winnum != '-1':
        if vim.eval('winnr()') != winnum:
            vim.command(winnum+'wincmd w')

        vim.command('setlocal modifiable')
        vim.command('silent! %delete _')

        print >>sys.stdout, 'Loading PageTree ...'

        pageList = xmlrpc.wiki.getAllPages()
        pageList = map(lambda x: x.encode('utf-8'), pageList)

        pageTreeBuffer    = vim.current.buffer
        pageTreeBuffer[:] = dwPageTree()

        # Build folding tree
        dwBuildFoldingTree()

        # Folding keys
        vim.command('nnoremap <buffer> <silent> + :silent! foldopen<CR>')
        vim.command('nnoremap <buffer> <silent> * :%foldopen<CR>')
        vim.command('nnoremap <buffer> <silent> - :silent! foldclose<CR>')
        vim.command('nnoremap <buffer> <silent> _ :%foldclose<CR>')

        vim.command('setlocal nomodifiable')
        vim.command('setlocal foldlevel=1')


def dwPageTree():
    """
    Generates the pageTree from the page list.
    """

    global pageList
    treeList = []
    tree = []

    # build the tree list
    for page in pageList:
        try:
            while page.index(':'):
                treeList.append(page)
                page = page.rsplit(':', 1)[0]
        except ValueError:
            treeList.append(page)

    # remove double list entries and sort the list
    tmp = {}
    for page in treeList:
        tmp[page] = 0

    treeList = tmp.keys()
    treeList.sort()

    # generate the list stack
    treeStack = {}
    for page in treeList:
        try:
            pageList.index(page)
            treeStack[page] = 1
        except ValueError:
            treeStack[page] = 0

    pageList = treeList

    # generate the output
    for page in pageList:
        line = page.count(':') * '  '
        try:
            if treeStack[page]:
                line += page.rsplit(':', 1)[1]
            else:
                line += '+ ' + page.rsplit(':', 1)[1]
            
        except IndexError:
            if treeStack[page]:
                line += page
            else:
                line += '+ ' + page

        tree.append(line)
    return tree


def dwAuth():
    """
    Simple re-authentication wrapper (might be stupid to do it that way).
    """
    global xmlrpc
    try:
        xmlrpc = dwXMLRPCInit()
        print >>sys.stdout, 'Connection to ' + vim.eval('g:DokuVimKi_URL') + ' established!'
    except:
        pass


def dwXMLRPCInit():
    """
    Simple Check to see if the XML-RPC interface is available or not.
    """

    try:
        import xmlrpclib
    except ImportError:
        print >>sys.stderr, 'DokuVimKi Error: The xmlrpclib python module is missing! Disabling all DokuVimKi commands!'
        dwDisable()

    dwUser = vim.eval('g:DokuVimKi_USER')
    dwPass = vim.eval('g:DokuVimKi_PASS')
    dwURL  = vim.eval('g:DokuVimKi_URL')

    URL = dwURL + '/lib/exe/xmlrpc.php?' + urlencode({'u':dwUser,'p':dwPass})

    try:
        xmlrpc = xmlrpclib.ServerProxy(URL)
        xmlrpclib.Transport.user_agent = UserAgent
        xmlrpclib.SafeTransport.user_agent = UserAgent

        try:
            version = xmlrpc.dokuwiki.getVersion()
            return xmlrpc

        except:
            print >>sys.stderr, 'DokuVimKi Error: Something went wrong! Maybe the XML-RPC interface is not enabled on your wiki? Disabling all DokuVimKi commands!'
            dwDisable()

    except IOError:
        print >>sys.stderr, 'DokuVimKi XML-RPC Error: The provided wiki URL is not valid! Disabling all DokuVimKi commands!'
        dwDisable()


def dwXMLRPCCheck():
    """
    Checks if the xmlrpc object is initialized already.
    """

    try:
        version = xmlrpc.dokuwiki.getVersion()
        return 1

    except NameError:
        print >>sys.stdout, 'You are not authenticated to a remote wiki yet. Use :dwAuth to establish a connection!'
        return 0


def dwDisable():
    """
    Disables the dw commands in case some libs are missing or the login
    credentials are wrong.
    """
    vim.command('delcommand DWEdit')
    vim.command('delcommand DWSend')
    vim.command('delcommand DWList')
    vim.command('delcommand DWRevs')
    vim.command('delcommand DWBackLinks')
    vim.command('delcommand DWPageTree')


# ---------------------------------------------------------------------------
# MAIN SCRIPT
# ---------------------------------------------------------------------------
if __name__ == '__main__':

    UserAgent = 'DokuVimKi/' + __version__ + ' (by www.chimeric.de)'

    editorBuffer   = vim.buffers[0]
    editorBufferID = vim.eval('bufnr("%")')
    editorWindowID = vim.eval('winnr()')

    pageTreeBuffer      = None
    pageTreeBufferID    = None
    pageTreeBufferName  = '__DokuVimKi_pageTree__'

    pageList = []
    svwp     = None

    if vim.eval('g:DokuVimKi_AUTOCONNECT') == 'yes':
        xmlrpc = dwXMLRPCInit()

# vim:ts=4:sw=4:et:enc=utf-8:

" Rename.vim  -  Rename a buffer within Vim and on the disk
"
" Copyright June 2007-2011 by Christian J. Robinson <heptite@gmail.com>
" Copyright June 2012 by xaimus
"
" Distributed under the terms of the Vim license.  See ":help license".
"
" Usage:
"
" :Rename[!] {newname}

command! -nargs=+ -complete=file -bang Rename call Rename(<q-args>, '<bang>')

function! Rename(name, bang)
    let l:name    = a:name
    let l:oldfile = expand('%:p')
    let l:oldfilepath = expand('%:h')
    let l:newfile = l:oldfilepath . '/' . l:name
    let l:status = 1

    if isdirectory(l:newfile)
        echohl ErrorMsg
        echomsg 'Target name "' . l:newfile . '" is a directory'
        echohl None
        return 0
    endif

    if filewritable(l:newfile) && a:bang !=# '!'
        echohl ErrorMsg
        echomsg 'Target file "' . l:newfile . '" already exists (use ! to override)'
        echohl None
        let l:status = 0
    endif

    if bufexists(fnamemodify(l:name, ':p'))
        if (a:bang ==# '!')
            silent exe bufnr(fnamemodify(l:name, ':p')) . 'bwipe!'
        else
            echohl ErrorMsg
            echomsg 'A buffer with the name "' . l:name . '" already exists (use ! to override)'
            echohl None
            let l:status = 0
        endif
    endif

    if l:status == 0
        return l:status
    endif

    let v:errmsg = ''
    silent! exe 'saveas' . a:bang . ' ' . l:newfile

    if v:errmsg =~# '^$\|^E329'
        let l:lastbufnr = bufnr('$')

        if expand('%:p') !=# l:oldfile && filewritable(expand('%:p'))
            if fnamemodify(bufname(l:lastbufnr), ':p') ==# l:oldfile
                silent exe l:lastbufnr . 'bwipe!'
            else
                echohl ErrorMsg
                echomsg 'Could not wipe out the old buffer for some reason'
                echohl None
                let l:status = 0
            endif

            if delete(l:oldfile) != 0
                echohl ErrorMsg
                echomsg 'Could not delete the old file: ' . l:oldfile
                echohl None
                let l:status = 0
            endif
        else
            echohl ErrorMsg
            echomsg 'Rename failed for some reason'
            echohl None
            let l:status = 0
        endif
    else
        echoerr v:errmsg
        let l:status = 0
    endif

    return l:status
endfunction

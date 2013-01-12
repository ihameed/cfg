function! vim2hs#haskell#editing#indentexpr(lnum) " {{{
  let l:line    = getline(a:lnum - 1)
  let l:curline = getline(a:lnum)

  let at_new_line_p = (col('.') - 1) == matchend(l:curline, '^\s*')
  "if l:line =~# '^\s*$'
  "  return -1
  "endif

  let l:indent = -1 "indent(a:lnum - 1)
  if at_new_line_p
    if l:line =~# '^data\>.*=.\+'
      let l:indent = match(l:line, '=')

    elseif l:line =~# '^data\>[^=]\+\|^class\>\|^instance\>'
      let l:indent = &shiftwidth * 2

    elseif l:line =~# '^newtype\>.*=.\+'
      let l:indent = match(l:line, '=') + 2

    elseif l:line =~# '^\k\+.*=\s*\%(do\)\?$'
      let l:indent = &shiftwidth

    elseif l:line =~# '\[[^\]]*$'
      let l:indent = match(l:line, '\[')

    elseif l:line =~# '{[^}]*$'
      let l:indent = match(l:line, '{')

    elseif l:line =~# '([^)]*$'
      let l:indent = match(l:line, '(')

    elseif l:line =~# '\<case\>.*\<of$'
      let l:indent = match(l:line, '\<case\>') + &shiftwidth

    elseif l:line =~# '\<case\>.*\<of\>'
      let l:indent = match(l:line, '\<of\>') + 3

    elseif l:line =~# '\<if\>.*\<then\>.*\%(\<else\>\)\@!'
      let l:indent = match(l:line, '\<then\>')

    elseif l:line =~# '\<if\>'
      let l:indent = match(l:line, '\<if\>') + &shiftwidth

    elseif l:line =~# '\<\%(do\|let\|in\|then\|else\)$'
      let l:indent = indent(a:lnum - 1) + &shiftwidth

    elseif l:line =~# '\<where$'
      let l:indent = indent(a:lnum - 1) + &shiftwidth - (&shiftwidth / 2)

    elseif l:line =~# '\<do\>'
      let l:indent = match(l:line, '\<do\>') + 3

    elseif l:line =~# '\<let\>.*\s=$'
      let l:indent = match(l:line, '\<let\>') + 4 + &shiftwidth

    elseif l:line =~# '\<let\>'
      let l:indent = match(l:line, '\<let\>') + 4

    elseif l:line =~# '\<where\>'
      let l:indent = match(l:line, '\<where\>') + 6

    elseif l:line =~# '\s=$'
      let l:indent = indent(a:lnum - 1) + &shiftwidth

    endif

    if synIDattr(synIDtrans(synID(a:lnum - 1, l:indent, 1)), 'name')
      \ =~# '\%(Comment\|String\)$'
      return indent(a:lnum - 1)
    endif
  else
    if l:curline =~# '\v^\s*<where>'
      let l:indent = indent(prevnonblank(a:lnum)) + (&l:shiftwidth / 2)
    endif
  endif

  return l:indent
endfunction " }}}

function! vim2hs#haskell#editing#indenting() " {{{
  setlocal indentexpr=vim2hs#haskell#editing#indentexpr(v:lnum)
  setlocal indentkeys=!^F,o,O,=where
endfunction " }}}

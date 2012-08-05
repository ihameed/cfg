setlocal expandtab
setlocal shiftwidth=2
setlocal softtabstop=2
setlocal tabstop=8

function! OCamlType()
    let col  = col('.')
    let line = line('.')
    let file = expand("%:p:r")
    echo system("annot -n -type ".line." ".col." ".file.".annot")
endfunction
map <buffer> <LocalLeader>t :call OCamlType()<cr>
command! -buffer Type :call OCamlType()

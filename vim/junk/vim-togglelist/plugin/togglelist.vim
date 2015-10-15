" orig: https://github.com/milkypostman/vim-togglelist
" also: http://vim.wikia.com/wiki/Toggle_to_open_or_close_the_quickfix_window
" also: http://www.vim.org/scripts/script.php?script_id=1375
"
" INSTRUCTIONS
" At the bottom of the file you will find commands that you can bind to keys.
" For example:
" nnoremap <silent> <leader>a :Ltoggle<cr>
" nnoremap <silent> <leader>q :Ctoggle<cr>
" is kinda sweet.


function! s:GetBufferList()
  redir =>buflist
  silent! ls
  redir END
  return buflist
endfunction

function! s:FilterBuffers(match)
  let rawbufs  = split(s:GetBufferList(), '\n')
  let filtered = filter(rawbufs, 'v:val =~ "' . a:match . '"')
  let bufnums  = map(filtered, 'str2nr(matchstr(v:val, "\\d\\+"))')
  return bufnums
endfunction

function! s:GetWindowState()
  let prev = winnr('#')
  let cur  = winnr()
  return {'cur': winnr(), 'prev': prev ? prev : cur}
endfunction

function! s:RestoreWindowState(window_state)
  execute a:window_state.prev . 'wincmd w'
  execute a:window_state.cur  . 'wincmd w'
endfunction



function! ToggleLocationList()
  if IsLocationOpen()
    lclose
  else
    call OpenLocationList()
  endif
endfunction

function! IsLocationOpen()
  let locbefore = len(s:FilterBuffers('Location List'))
  call OpenLocationList()
  let locafter = len(s:FilterBuffers('Location List'))
  if locbefore != locafter
    lclose
    return 0
  else
    return 1
  endif
endfunction

function! OpenLocationList()
  let window_state = s:GetWindowState()
  try
    lopen
  catch /E776/
    echohl ErrorMsg
    echo "Empty location list"
    echohl None
    return
  endtry

  " assumption: lopen always opens immediately after cur
  if window_state.cur < window_state.prev
    let window_state.prev += 1
  endif
  call s:RestoreWindowState(window_state)
endfunction



function! ToggleQuickfixList(force)
  if IsQuickfixOpen()
    cclose
  else
    call OpenQuickfixList(a:force)
  endif
endfunction

function! IsQuickfixOpen()
  return len(s:FilterBuffers('Quickfix List')) > 0
endfunction

function! OpenQuickfixList(force)
  let window_state = s:GetWindowState()
  if a:force
    botright copen
  else
    botright cwindow
  endif
  call s:RestoreWindowState(window_state)
endfunction



command! -bang -nargs=? Ctoggle :call ToggleQuickfixList('<bang>' ==# '!')
command! -bang -nargs=? Copen   :call OpenQuickfixList('<bang>' ==# '!')
command!       -nargs=0 Cclose  :cclose

command! -nargs=0 Ltoggle :call ToggleLocationList()
command! -nargs=0 Lopen   :call OpenLocationList()
command! -nargs=0 Lclose  :lclose

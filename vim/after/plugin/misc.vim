AddTabularPipeline! ws /\s/
  \ map(a:lines, "substitute(v:val, '\\s\\s*', ' ', 'g')") |
  \ tabular#TabularizeStrings(a:lines, ' ', 'l0')

if executable('sk') && exists('g:loaded_skim') && g:loaded_skim
  map <f4> <esc>:SK<cr>
  command! -bang -nargs=* SKrg call fzf#vim#rg_interactive(<q-args>, fzf#vim#with_preview('right:50%:hidden', 'alt-h'))
  map <f5> <esc>:SKrg<cr>
endif

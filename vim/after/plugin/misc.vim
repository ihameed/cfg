AddTabularPipeline! ws /\s/
  \ map(a:lines, "substitute(v:val, '\\s\\s*', ' ', 'g')") |
  \ tabular#TabularizeStrings(a:lines, ' ', 'l0')

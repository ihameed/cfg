setx HOME "%USERPROFILE%"
setx DISPLAY ":0"

set FN="%TEMP%\ssh-agent-init.cmd"
C:\cygwin\bin\charade.exe -c | C:\cygwin\bin\sed.exe 's/setenv/setx/' | C:\cygwin\bin\sed 's/;$//' > %FN%
call %FN%
del %FN%

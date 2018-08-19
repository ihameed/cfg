setx HOME "%USERPROFILE%"
setx DISPLAY ":0"

set FN="%TEMP%\ssh-agent-init.cmd"
C:\cygwin\bin\ssh-pageant.exe -c | C:\cygwin\bin\sed.exe 's/setenv/setx/' | C:\cygwin\bin\sed "s/'//g" | C:\cygwin\bin\sed 's/;$//' > %FN%
call %FN%
del %FN%

C:\cygwin\bin\xrdb.exe -merge ~/.Xresources

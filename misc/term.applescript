tell application "Terminal" to launch
tell application "System Events"
    set loaded to false
    set attempts to 0
    set limit to 100
    repeat until loaded or (attempts >= limit)
        if menu "Shell" of menu bar 1 of application process "Terminal" exists then
            set loaded to true
        else
            delay 0.05
            set attempts to attempts + 1
        end if
    end repeat
    tell process "Terminal"
        click menu item "imran" of menu 1 of menu item "New Window" of menu "Shell" of menu bar 1
    end tell
end tell

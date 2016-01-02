# minkzdesktop

Note: Please try Sparta docking editor first if you are looking for a form editor for Lazarus, as that one is the defacto and stable one.

Fork of KZDesktop.

Converts the form editor of Lazarus to a window that is compatible with docking solutions (like AnchorDocking). This is made to be used for a 2 window layout (code and design) via the usual 2 separate windows or via manage desktop.

I only tested this at Linux 64 bit (lazarus 1.6+) so use this at your own risk. This is designed only for my personal use since for some reason i do not prefer the current workflow provided by Sparta.

Known Bugs:
* If you moved the window while on docking mode, you might need to restart afterwards to make the form editor work properly
* Might have some popup errors after setting up docking position and closing ide (This one is also part AnchorDocking problem, since it has issues on closed combined dock windows)
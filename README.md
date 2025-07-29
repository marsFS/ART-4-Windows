# ART-4-Windows & TelePaint
ART 4 Windows is a retro drawing utility, original written in BBC Basic for Windows and latest version is written in Pure Basic compiled to EXE
TelePaint is BBC Mirco Mode 7 Teletext paint and animation program written in BBC Basic for SDL

Both programs are works in progress with likely bugs, save projects often!

Current Executable Versions:
* ART4EVA_PB_010_x86.exe - Current Release - Windows x86 executable 
* ART4EVA_PB_007_x86.exe - Pre menu change build - Windows x86 executable
* ART4WIN_024.exe - Last Release of BBC Basic for Windows version - Windows x86 executable
* ART_EXA_PB_015_x86.exe - Exa Gear interface layout version - Windows x86 executable
* TELEPAINT_BBCSDL.zip - Windows compatible executable and library files for BBC Basic for SDL version of Telepaint

Current Source Versions:
* TELEPAINT_BBCSDL.bbc - BBC SDL BBC Micro Mode 7 Art program, animated frames, sprites, import / export images
* ART4WIN_024.bbc - Current BBC Basic for Windows version of Art, no longer actively developed but used as a reference point for other bbc basic versions

Current ART features (needs updating for each version):
* Configured for BBC Micro Mode 2 (160x256 16 colors)
* Save and load files in PNG format for PB version, BMP for BB4W version
* Save and load files in BBC RAW format, creates INF file for easy import to SSD
* New menu system to allow for more tools
* Added Gradient Editor
* Added Selection Transformation tools
* ART_PB_xxx supports flashing colours and colour cycling animation for colours 1-15, can configure which colours are used for flashing in animation menu
* ART_EXA_PB_xxx version has smaller screen footprint to suit mobile devices (EXA Gear)

Current TelePaint features:
* Requires BBC Basic for SDL 2.0
* Configured for BBC Micro Mode 7 (40x25 chars, 78x72 sixels)
* Save and load files in BIN and/or BMP format, project name can now be edited
* Canvas Mode - Configure up to 100 animation frames and 48 sprites
* Movie Mode - Up to 9999 frames, animated sprites
* Movie Mode - Up to 99999 insertable objects, including sprites, animated sprites, text
* Import sprites and BMP files (specific format required 640x480 24bpp BMP)
* Import sprites from GIF files, step through GIF frames to grab specific sequences of sprites
* Import GIF includes a special mode for 12 extra large sprites
* Added Fonts, requires download of M7_FONTS folder
* Added Transformation tools
* Implemented gradients correctly
* Reworked sub menus system
* Added 5 new brushes
* Added hotkeys:
* * CTRL + C  : Open Selection Tool
* * SHIFT + Cursors   : Move selected region around in pixel increments
* * CRTL + Left / Right  : Copy current frame to prev / next frame
* * Movie mode added many hotkeys, access the help screen for details

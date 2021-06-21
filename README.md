# ART-4-Windows
ART 4 Windows is a retro drawing utility, original written in BBC Basic for Windows and latest version is written in Pure Basic compiled to EXE

Current Executable Versions:
* ART4EVA_PB_007_x86.exe - Current Release - Windows executable
* ART4EVA_PB_008_x86.exe - Experimental Release - Windows executable
* ART4WIN_024.exe - Last Release of BBC Basic for Windows version - Windows executable
* ART_EXA_PB_015_x86.exe - Exa Gear interface layout version - Windows executable
* TELEPAINT_BBCSDL.zip - Windows compatible executable and library files for BBC Basic for SDL version of Telepaint

Current Source Versions:
* TELEPAINT_BBCSDL.bbc - BBC SDL Mode 7 Art program, animated frames, sprites, import / export
* ART4WIN_024.bbc - Current BBC Basic for Windows version of Art, no longer actively developed but used as a reference point for other bbc basic versions

Current ART features (needs updating for each version):
* Configured for BBC Micro Mode 2 (160x256 16 colors)
* Save and load files in PNG format for PB version, BMP for BB4W version
* Save and load files in BBC RAW format, creates INF file for easy import to SSD
* New menu system to allow for more tools
* Added Gradient Editor
* Added Selection Transformation tools
* BB4W Version creates temporary undo files in BMP format
* ART_PB_xxx version now supports flashing colours and colour cycling animation for colours 8-15
* ART_EXA_PB_xxx version has smaller screen footprint to suit mobile devices (EXA Gear)

Current TelePaint features:
* Requires BBC Basic for SDL 2.0
* Configured for BBC Micro Mode 7 (40x25 chars, 78x72 sixels)
* Save and load files in BIN and/or BMP format
* Configure up to 100 animation frames and 48 sprites
* Import sprites and BMP files (specific format required)
* Added Fonts, requires download of M7_FONTS folder
* Added Transformation tools
* Implemented gradients correctly
* Reworked sub menus system
* Added 5 new brushes
* Added hotkeys:
* * CTRL + C  : Open Selection Tool
* * SHIFT + Cursors   : Move selected region around in pixel increments
* * CRTL + Left / Right  : Copy current frame to prev / next frame

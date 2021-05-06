# ART-4-Windows
ART 4 Windows is a retro drawing utility, original written in BBC Basic for Windows and latest version is written in Pure Basic compiled to EXE

Current Executable Versions:
* ART4EVA_PB_007_x86.exe - Windows compatible executable
* TELEPAINT_BBCSDL.zip - Windows compatible executable and library files

Current Source Versions:
* ART4EVA_PB_007_x86.pb - Current version of Art program developed in Pure Basic, Mode 2, animated colours, import / export
* TELEPAINT_BBCSDL.bbc - BBC SDL Mode 7 Art program, animated frames, sprites, import / export
* ART4SDL_PORTRAIT_001.bbc - BBC SDL version of Art modified to work in portrait mode specifically for iPad (based on ART4WIN_024.bbc)
* ART4WIN_024.bbc - Current BBC Basic for Windows version of Art, no longer actively developed but used as a reference point for other bbc basic versions

Current ART features (needs updating for each version):
* Configured for BBC Micro Mode 2 (160x256 16 colors)
* Save and load files in PNG format for PB version, BMP for BB4W version
* Save and load files in BBC RAW format, creates INF file for easy import to SSD
* BB4W Version creates temporary undo files in BMP format
* ART_PB_xxx version now supports flashing colours and colour cycling animation for colours 8-15
* ART_EXA_PB_xxx version has smaller screen footprint to suit mobile devices (EXA Gear)

Current TelePaint features:
* Requires BBC Basic for SDL 2.0
* Configured for BBC Micro Mode 7 (40x25 chars, 78x72 sixels)
* Save and load files in BIN and/or BMP format
* Configure up to 100 animation frames and 48 sprites
* Gradient fills
* Import sprites and BMP files (specific format required)
* Added Fonts, ensure to download M7_FONTS folder

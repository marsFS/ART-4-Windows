      REM *** TODO LIST ***

      REM *** INKEY(-256) MATRIX BRANYDY &4D (INCLUDE OTHERS?)

      REM *** CROSS BASIC COMPATIBILITY (SORUK FOR ADVICE AND TESTING)

      REM *** LOAD SCREEN SORT BY NEWEST, NEEDS WORK!

      REM *** LOAD SCREEN USABILITY (IMPROVED A LITTLE)

      REM *** IMPLEMENT ANIMATED CIRCLE (REDO CIRCLE ROUTINE)

      REM *** IMAGE CONVERTER FOR IMPORTING BMP FILE, ADD MOVE FRAME OPTION (needs work)

      REM *** SPRITES - ADD MASKING AND TRANSPARENCY FOR EACH SPRITE SET

      REM *** TRANSITION EFFECTS, SCROLL FRAME IN / OUT - LEFT, RIGHT, UP, DOWN  , FRAME DISOLVE... OTHERS?

      REM Things still to work on:
      REM * Moving frames and better sprite movement options
      REM ** Moving & fixed sprites, animations, frames, text relative to frame
      REM ** Moving & fixed sprites, animations, frames, text relative to background (existing configuration needs work)
      REM ** Animated objects fixed to background and incremented each frame based on skip / repeat values
      REM ** Object and Frame editable lists
      REM ** Sprite and animation name labels
      REM * Integrating fonts as special sprites
      REM * Text labels and bubbles
      REM * Improve colour and special control code logic
      REM * Improve frame management tools
      REM ** panning and base frame animations - if animation is on base frame then increment ani index e.g. frame% mod anicount
      REM ** Improve panning options - accelerate, decelerate, coast
      REM * ..... ???
      REM      * Onion Skin - show previous / next frame sprites shaded
      REM      * Playback loop mode
      REM Frame tools - * Add Frame Button * Delete frame * Move frame * insert frame * Timeline

      REM *** END TODO LIST ***

      REM *** Added internal mode changes between Mode 6 and Mode 7 to eliminate screen repos

      REM *** Internal Mode 6 uses Mode 7 font and needs colour index adjusted

      REM *** MODE 6 : TEXT: 40x25 (32x40 per char) PIXELS: 640x500 GU: 1280x1000 COLOURS: 16

      REM *** HELP SCREEN: MODE 6

      REM *** SPECIAL SUBMENUS: MODE 6

      REM *** MOVIE MODE SPRITE ROUTINES: MODE 6

      REM *** https://edit.tf/#0:<1167 BYTES FOR 25 ROWS>

      REM *** https://zxnet.co.uk/teletext/editor/#0:<1167 BYTES FOR 25 ROWS>

      REM animated GIF code... ???
      REM https://www.bbcbasic.net/wiki/doku.php?id=displaying_20animated_20gifs

      REM capture screen to bmp - read more...
      REM https://www.bbcbasic.net/wiki/doku.php?id=capturing_20the_20contents_20of_20a_20window

      BB4W = INKEY$(-256) == "W"

      REM ALLOCATE 50MB FOR BUFFERS
      HIMEM = PAGE+50000000

      INSTALL @lib$+"sortlib"
      INSTALL @lib$+"imglib"

      version$="v0.35"

      DEBUG%=0 : REM for displaying mouse and other debug details, F12 toggles debug mode while running Telepaint

      MODE 7

      REM breaks application if not set on 64bit OS
      *HEX 64

      REM ESC OFF FOR BACK ARROW ON SOME DEVICES
      *ESC OFF

      MOUSE ON 3

      VDU 23,1,0;0;0;0; : REM Disable cursor

      REM menu constants
      M_canvasmode%=0
      M_keyboard%=1
      M_sprites%=2
      M_help%=3
      M_codes%=4
      M_createfont%=5
      M_loadscreen%=6
      M_moviemode%=7

      REM canvas mode sub menus
      M_paint%=10
      M_dither%=11
      M_copypaste%=12
      M_fill%=13
      M_special%=14

      REM movie mode sub menus
      M_sprProperty%=15
      M_sprSelect%=16
      M_frmProperty%=17
      M_movieMenu%=18

      REM tool constants
      T_paint&=0
      T_line&=1
      T_box&=2
      T_circle&=3
      T_symmetry&=4
      T_dither1&=5
      T_dither2&=6
      T_dither3&=7
      T_dither4&=8
      T_dither5&=9
      T_brush1&=10
      T_brush2&=11
      T_brush3&=12
      T_brush4&=13
      T_brush5&=14
      T_brush6&=15
      T_brush7&=16
      T_fill&=17
      T_gradl&=18
      T_gradr&=19
      T_gradt&=20
      T_gradb&=21
      T_gradtl&=22
      T_gradtr&=23
      T_gradbr&=24
      T_gradbl&=25
      REM cursor at tool for the tools below
      T_copy&=26
      T_paste&=27
      T_text&=28
      T_flash&=29
      T_double&=30
      T_separate&=31
      T_hold&=32
      T_backg&=33
      T_foreg&=34

      REM movie mode - WX, WY world x,y sixel position, 0,0 = bottom left corner of canvas map
      mmWX%=0
      mmWY%=0

      REM canvas mode - WX, WY world x,y sixel position used when plotting sprites to scroll off screen
      cmWX%=0
      cmWY%=0

      REM drawing bounds used for < and >
      xMin%=1
      xMax%=80
      yMin%=2
      yMax%=75

      REM fill bounds & buffer used with < and >
      fxMin%=2
      fxMax%=80
      fyMin%=3
      fyMax%=74
      fillmax%=255
      bCnt%=0
      DIM fill{(fillmax%) x%,y%}

      REM old pixel, mouse, text coords, code display
      OLD_PX%=0
      OLD_PY%=0
      OLD_MX%=0
      OLD_MY%=0
      OLD_TX%=0
      OLD_TY%=0
      newcode%=0
      oldcode%=0

      REM mouse coords
      MX%=0
      MY%=0
      MB%=0

      REM text & pixel coords for current mouse location
      TX%=0
      TY%=0
      PX%=0
      PY%=0

      REM text input coords
      TEXTX%=0
      FONTX%=0
      OTX%=0
      OTY%=0

      REM sub menu pos data
      sub_count%=8
      sub_cur%=-1
      DIM subm{(sub_count%) x%,y%,w%,h%}

      FOR I%=0 TO sub_count%
        READ subm{(I%)}.x%
        READ MY%
        READ subm{(I%)}.w%
        READ subm{(I%)}.h%
        subm{(I%)}.y%=MY%-subm{(I%)}.h%
      NEXT

      REM drawing region for point command, changes for sprite menu

      REM tool and animation vars
      curcol%=7
      oldcol%=7
      bakcol%=0
      textfore%=0
      toolsel%=T_paint&
      toolcursor%=15
      copypaste%=0
      copysize%=0
      copyx%=0
      copyy%=0
      copylockx%=-1
      copylocky%=-1
      copylockxt%=0
      copylockyt%=0
      copymovepx%=0
      copymovepy%=0
      copymovepw%=0
      copymoveph%=0
      copymovef%=0
      copy_trns%=1
      colmode%=0        : REM column mode for fore and back tools
      shapetype%=0      : REM enclosed shape type 0=outline, 1=filled, 2=empty
      gradtype%=0
      animation%=0
      animateshape%=0
      animategap%=0
      animategapcount%=0
      animatelen%=1
      animatelencount%=0
      framedupe%=1
      scrollh%=0
      scrollv%=0
      erase%=0
      dither%=0
      frame%=1             : REM displays current canvas frame
      movieframe%=-1       : REM displays current movie frame, -1 for background mode
      movieframetotal%=-1  : REM total frames saved
      movieframemax%=9999  : REM maximum frames
      movieframeadd%=0     : REM add control for new frames
      movieframeaddh%=0    : REM h increment for new frames
      movieframeaddv%=0    : REM v increment for new frames
      insertmode%=0        : REM movie mode insert mode 0=sprite, 2=animation, 2=frame, 3=text
      insertrepeat%=0      : REM flag for inserting animation or sprite on multiple frames 1=animation, 2=sprite, 3=text, frames ???
      insertani%=0         : REM animation counter for inserting sprites
      insertset%=0         : REM current animation set
      insertlarge%=0       : REM insert large sprites flag
      sprlrg_max%=180      : REM large sprite buffer size
      sprani_max%=12       : REM animation set sprite count max
      sprlrg_count%=0      : REM large sprite buffer counter
      lrgx%=450            : REM large sprite width
      lrgy%=60             : REM large sprite height
      insertphase%=0       : REM current animation click phase for selecting first and last animation frames
      inserttextcol%=6     : REM text insert colour -1
      insertsave%=0        : REM insert options save flag
      insertrepflag%=0     : REM multiple frames flag
      insertrelflag%=1     : REM sprite relative position to frame
      insertskipcount%=0   : REM multiple frames skip count
      insertfrmrep%=0      : REM animation frame repeat count
      insertfrmindex%=1    : REM animation frame start index
      spritemoving%=-1     : REM flag to check if selected sprite is moving
      spritedupe%=-1       : REM flag to check if selected sprite is being duped in movie mode
      spritestartframe%=0
      insertstartx%=0      : REM start location for sprite and screen selections
      insertstarty%=0      : REM start location for sprite and screen selections
      spriteold%=0
      spriterelocate%=0
      spriteselect%=-1
      spriteselectold%=-1
      spritechange%=-1
      objsprscroll%=0
      objaniscroll%=0
      objfrmscroll%=0
      objtxtscroll%=0
      menumode%=M_canvasmode%  : REM tracks if in canvas or movie modes for returning from multiple sub menus
      menuext%=M_canvasmode%   : REM current extended menu
      menufrom%=M_canvasmode%  : REM menu to return to
      menuYadd%=0          : REM general var for control layout
      menuXadd%=0          : REM general var for control layout
      session%=0
      curdir$=@dir$
      cursave$=@dir$
      saverootdir$=@dir$
      cursavedir$=@dir$
      bmpload$=""
      save_bin%=1        : REM save bin flag
      save_bmp%=0        : REM save bmp flag
      save_spr%=1        : REM save spr flag - saves sprites and animation information
      save_dat%=0        : REM save dat flag - saves sprites as DATA statements
      mov_frm%=1        : REM save movie frame flag - saves movie frame and sprite position data as MOV
      mov_bin%=1        : REM save movie bin flag - saves canvas mode frames as BIN
      mov_bmp%=0        : REM save movie bmp flag - saves movie frames as BMP
      mov_spr%=1        : REM save movie spr flag - saves sprites as SPR
      mov_txt%=1        : REM save movie txt flag - saves text strings as TXT
      mov_dat%=1        : REM save movie dat flag - saves movie frames as BIN
      text$=""
      caps%=1
      showcodes%=0
      esccodes%=0
      gridshow%=0
      pc%=0                : REM dither random

      REM colour string constants
      tr$=CHR$(129) : REM alphanumeric red
      tg$=CHR$(130) : REM alphanumeric green
      ty$=CHR$(131) : REM alphanumeric yellow
      tb$=CHR$(132) : REM alphanumeric blue
      tm$=CHR$(133) : REM alphanumeric magenta
      tc$=CHR$(134) : REM alphanumeric cyan
      tw$=CHR$(135) : REM alphanumeric white

      gr$=CHR$(145) : REM graphics red
      gg$=CHR$(146) : REM graphics green
      gy$=CHR$(147) : REM graphics yellow
      gb$=CHR$(148) : REM graphics blue
      gm$=CHR$(149) : REM graphics magenta
      gc$=CHR$(150) : REM graphics cyan
      gw$=CHR$(151) : REM graphics white

      REM shape codes
      DIM scode&(7)
      scode&(0)=136 : REM flash
      scode&(1)=137 : REM steady
      scode&(2)=141 : REM double height text
      scode&(3)=140 : REM normal height text
      scode&(4)=154 : REM separated
      scode&(5)=153 : REM contiguous
      scode&(6)=158 : REM hold graphics
      scode&(7)=159 : REM release

      REM plot mode for sprite sets
      DIM plotmode$(2)
      plotmode$(0)="NORMAL"
      plotmode$(1)="TRANSP"
      plotmode$(2)="MASKED"

      REM dither pattern data
      DIM pat%(17,15)
      FOR I%=0 TO 17
        FOR H%=0 TO 15
          READ pat%(I%,H%)
        NEXT
      NEXT

      DIM grad%(3,17)
      FOR I%=0 TO 3
        FOR H%=0 TO 17
          READ grad%(I%,H%)
        NEXT
      NEXT

      REM custom icons sprite data
      customspr%=7
      customsprx%=14
      customspry%=14
      customsize%=customspr%*customsprx%*customspry%-1
      DIM customspr&(customsize%)

      FOR I%=0 TO customsize%
        READ customspr&(I%)
      NEXT

      REM loading screen
      menuext%=M_loadscreen%
      frame_limit%=100
      frame_max%=8
      launch_action%=0

      REM sprite variables
      sprite_max%=100
      sprite_old%=0
      sprite_cur%=0
      spr_lstcount%=0
      spr_trns%=1
      spr_scroll%=1

      REM object list
      obj_lstmax%=99999
      obj_lstcount%=-1
      obj_lstcur%=0
      obj_txtcur%=-1

      REM bmp image variables for importing to memory
      bmp_imgwid%=0
      bmp_imghgt%=0
      bmp_imgbpp%=0
      bmp_imgofs%=0

      PROCloadscreen

      REM current menu or screen showing
      menuext%=M_canvasmode%

      REM frame buffer 24x40 chars
      DIM frame_buffer&(frame_max%-1,959)

      REM move pixel buffer 80x72 pixels
      DIM move_buffer&(5759)

      REM sixel pixel height and ypos lookup table for mode 6 overlays
      DIM sixel{(74) h%,y%,sdly%,sdlh%}
      Y%=1000
      Z%=0
      FOR I%=0 TO 74
        C%=((I%+1) MOD 3=0)*2
        sixel{(I%)}.h%=12+C%
        Y%=Y%-(14+C%)
        sixel{(I%)}.y%=Y%
        C%=(I%+1) MOD 3=0
        sixel{(I%)}.sdlh%=6+C%
        sixel{(I%)}.sdly%=Z%
        Z%=Z%+(7+C%)
      NEXT

      REM FOR I%=74 TO 71 STEP-1:RECTANGLE FILL (74-I%)*16,sixel{(I%)}.y%,14,sixel{(I%)}.h%:NEXT

      REM 20x16 chars @320 bytes : 40x48 pixels @1920 bytes
      REM sprlist Width, Height, Mode - 0 for pixel (single colour or code, 1 for char (multiple colours and codes)
      REM sprlrg - 8 large sprites for importing and displaying large images
      DIM sprbuf&(sprite_max%-1,319)
      DIM sprname$(sprite_max%-1)
      DIM sprlrg&(sprlrg_max%-1,30600)
      DIM sprlist{(sprite_max%-1) w%,h%,m%}
      DIM spr_tmp&(2000)


      REM lists for movie mode
      REM obj% = sprite or frame number, -1 = inactive
      REM type% = type, 0 = sprite, 1 = frame, 3 = text
      REM f% = movie start frame
      REM x%,y% = world x,y position
      REM rel% = plot sprite relative to frame coords
      REM hop% = frame repeat skip amount...to be deprecated and replaced with dialog options
      REM parent% = reference object for displaying relative world map position  -1 for absolute
      REM u% = toggle undo repeat action, will remove any objs with matching parent=obj number
      DIM objlist{(obj_lstmax%) obj%,type%,f%,rel%,hop%,parent%,x%,y%,h%,v%,m%,c%,u%}
      DIM frmlist{(9999) x%,y%,b%,f%}
      DIM sprani{(99) s%(11),f%,r%,x%,y%,h%,v%,m%,d%,c%}
      DIM txtlist$(999)

      REM animation and menu controls - redfinable depending on current screen / menu
      controls%=47
      DIM controlrange{(controls%) x1%,y1%,x2%,y2%}

      REM general idea for supporting font files
      REM each font is stored in an external file, height is static but width is variable per font char
      REM as height is static, make height part of font header data and remove from each font
      REM each font contains standard ascii lookup code so only fonts found in file are set to active in the font array
      REM ascii code is decremented when loading to suit 0 index array i.e  ASC(32) = 0 ASC(65) = 33 ASC(126) = 94
      REM only one font file loaded at a time
      REM standard font array, fixed font count TBD (100?), max data size for each font i.e. w * h -1 20x20 = 400 bytes
      REM Data:  ascii code, width
      REM        data rows - 0= black pixel, 1=solid pixel, 2=transparent i.e. no plot

      REM import detail for fonts supplied by pixelblip
      REM pixel sizes appears to be 6x5 TBD
      REM develop import utility to create data files (in progress)

      REM arrays:
      REM  fontcur% - 0=normal text
      REM  fontname$ - scan font folder for file names -need to be standard max 10 chars
      REM  fonts.d% - font pixel data
      REM  fonts.a% - font active i.e. loaded from font file
      REM  fonts.w% - font width
      fontcur%=0
      fontmax%=99
      fonthgt%=0
      fontcount%=0
      fontfound%=0

      DIM fontname$(fontmax%)
      DIM fonts{(fontmax%) d%(399),a%,w%}

      fontname$(0)="TEXT"
      PROCloadfontnames

      REM undo buffer
      undo_max%=99
      DIM undo_buffer&(frame_max%-1,undo_max%,959)
      DIM undo_index%(frame_max%-1)
      DIM undo_count&(frame_max%-1)

      REM redo buffer
      DIM redo_buffer&(frame_max%-1,undo_max%,959)
      DIM redo_index%(frame_max%-1)
      DIM redo_count&(frame_max%-1)

      REM sprite undo buffer
      DIM spr_undo_buffer&(sprite_max%-1,undo_max%,319)
      DIM spr_undo_index%(sprite_max%-1)
      DIM spr_undo_count&(sprite_max%-1)

      REM sprite redo buffer
      DIM spr_redo_buffer&(sprite_max%-1,undo_max%,319)
      DIM spr_redo_index%(sprite_max%-1)
      DIM spr_redo_count&(sprite_max%-1)

      REM menu buffer
      DIM menu_buffer&(959)

      REM movie mode buffer
      DIM movie_buffer&(959)
      DIM movie_colbuf&(959)

      REM copypaste buffer
      DIM copy_buffer&(959)

      DIM import_buffer%% 1000000

      REM init sprites
      CLS
      FOR S%=0 TO sprite_max%-1
        FOR U%=0 TO 319
          sprbuf&(S%,U%)=32
        NEXT

        sprlist{(S%)}.w%=1
        sprlist{(S%)}.h%=1
        sprlist{(S%)}.m%=0

        sprname$(S%)="Sprite "+RIGHT$("00"+STR$(S%+1),3)
      NEXT

      FOR S%=0 TO 99
        sprani{(S%)}.f%=1
        FOR ss%=0 TO sprani_max%-1
          sprani{(S%)}.s%(ss%)=-1
          sprani{(S%)}.d%=1
          sprani{(S%)}.c%=0
        NEXT
      NEXT

      PROCresetmovie


      REM init frames
      PROCGR(curcol%,bakcol%,1)
      FOR frame%=1 TO frame_max%
        PROCframesave(frame%)
      NEXT frame%
      frame%=1
      PROCmenudraw

      IF launch_action%=-2 THEN PROCshowhelp

      IF launch_action%=-3 THEN PROCimportimage

      VDU 23,1,1;0;0;0;  : REM Enable cursor
      REM VDU 23,0,10;0;0;0; : REM block cursor

      REM ##########################################################
      REM main loop starts here

      REPEAT

        PROCREADMOUSE

        IF MX%<>OLD_MX% OR MY%<>OLD_MY% OR MB% THEN

          REM handle mouse move and buttone clicks
          CASE MB% OF

            WHEN 0 : REM no mouse botton
              IF OLD_PX%<>PX% OR OLD_PY%<>PY% THEN
                IF spritemoving%>-1 AND spritemoving%<>9999 THEN
                  PROCspritemove(1)
                ENDIF
                OLD_PX%=PX% : OLD_PY%=PY%
              ENDIF

            WHEN 1 : REM right mouse click

            WHEN 2 : REM middle mouse click

            WHEN 4 : REM left mouse click or touch screen

              IF TY%=0 AND spritemoving%=-1 THEN
                REM click menu area
                PROCmenuhandler

              ELSE

                CASE menuext% OF
                  WHEN M_keyboard% : REM handle keyboard and options screen
                    PROCkybrfontdhandler

                  WHEN M_sprites% : REM sprite editor
                    PROCspritehandler

                  WHEN M_paint%,M_dither%,M_special%,M_fill%,M_copypaste%,M_sprProperty%,M_sprSelect%,M_frmProperty%,M_movieMenu% : REM sub menu
                    PROCsubhandler

                  WHEN M_moviemode% : REM movie mode
                    PROCmoviemode

                  WHEN M_canvasmode% : REM canvas mode
                    PROCcanvasmode

                  OTHERWISE : REM main drawin canvas
                    PRINTTAB(0,1)"UNKNOWN MENU: ";STR$(menuext%);
                    A=GET

                ENDCASE
              ENDIF
          ENDCASE : REM mb%

          TEXTX%=TX%
          FONTX%=PX%

        ELSE : REM no mouse move or clicks detected

          PROCkeyboardhandler

          WAIT 2

        ENDIF

        REM save old mouse position
        OLD_MX%=MX%
        OLD_MY%=MY%
        OLD_TX%=TX%
        OLD_TY%=TY%

      UNTIL 0
      END

      REM ##########################################################
      REM READ MOUSE AND CALCULATE TEXT AND SIXEL LOCATIONS
      DEF PROCREADMOUSE

      MOUSE MX%,MY%,MB%

      REM TEXT LOCATION RELEATIVE TO MOUSE
      TX%=MX% DIV 32
      TY%=(999-MY%) DIV 40

      REM SIXEL LOCATION RELEATIVE TO MOUSE
      PX%=MX% DIV 16
      PY%=(999-MY%)/13.3333333

      IF menuext%=M_canvasmode% OR menuext%=M_sprites% THEN
        IF showcodes%=1 THEN
          newcode%=GET(TX%,TY%)
          IF oldcode%<>newcode% THEN
            PRINTTAB(36,24)tc$+RIGHT$("00"+STR$(newcode%),3);
            oldcode%=newcode%
          ENDIF
        ENDIF
        REM show cursor at MOUSE or on toolbar
        IF toolsel%>T_gradbl& THEN
          IF TX%>-1 AND TX%<40 AND TY%>0 AND TY%<25 THEN
            VDU 31,TX%,TY%
          ELSE
            VDU 31,toolcursor%,0
          ENDIF
        ELSE
          VDU 31,toolcursor%,0
        ENDIF

        IF MX%<>OLD_MX% OR MY%<>OLD_MY% THEN
          IF gridshow%=1 THEN PROCdrawgrid
        ENDIF
      ELSE
        CASE menuext% OF
          WHEN M_keyboard%
            VDU 31,LEN(text$)+6,20

          WHEN M_createfont%
            VDU 31,LEN(text$)+4,9

        ENDCASE
      ENDIF

      PROCshowdebug(7)

      ENDPROC

      REM ##########################################################
      REM wait for mouse to be a specific button clicked
      DEF PROCWAITMOUSE(M%)
      REPEAT
        PROCREADMOUSE
        WAIT 2
      UNTIL MB%=M%
      ENDPROC

      REM ##########################################################
      REM wait for no key input
      DEF PROCWAITNOKEY(K%,R%)
      REPEAT UNTIL INKEY(K%)=R%
      ENDPROC

      REM ##########################################################
      REM move mouse relative to current pos
      DEF PROCMOVEMOUSE(X%,Y%)
      MX%+=X%
      MY%+=Y%
      MOUSE TO MX%,MY%
      ENDPROC

      REM ##########################################################
      REM perform soft mode change to preserve window location
      DEF PROCchangemode(M%,C%)

      REM set internal mode and clearscreen
      @vdu.m.a& = M%
      IF C%=1 THEN CLS

      CASE M% OF
        WHEN 6
          REM icrease colour index and set palette
          @vdu.m.b& = 15
          COLOUR 8,63,63,63
          COLOUR 9,255,0,0
          COLOUR 10,0,255,0
          COLOUR 11,255,255,0
          COLOUR 12,0,0,255
          COLOUR 13,255,0,255
          COLOUR 14,0,255,255
          COLOUR 15,255,255,255

          VDU 23,1,0;0;0;0; : REM Disable cursor

        WHEN 7
          @vdu.m.b& = 7
          VDU 23,1,1;0;0;0; : REM Enable cursor
          VDU 4

      ENDCASE

      *REFRESH

      ENDPROC

      REM ##########################################################
      REM display debug info
      DEF PROCshowdebug(c%)
      IF DEBUG% THEN

        COLOUR c%
        REM PRINTTAB(0,20)SPC(20)
        REM PRINTTAB(0,21)SPC(20)
        REM PRINTTAB(0,22)SPC(20)
        REM PRINTTAB(0,23)SPC(20)
        REM PRINTTAB(0,20)"A:";STR$(GET(TX%,TY%));"  ";"SM:";STR$(spritemoving%);gw$;
        PRINTTAB(0,20)"SM:";STR$(spritemoving%);" ME:";STR$(menuext%);"  ";
        PRINTTAB(0,21)"MX:";STR$(MX%);" MY:";STR$(MY%);gw$;
        PRINTTAB(0,22)"SC";STR$(spritechange%);" ";
        REM PRINTTAB(0,21)"TX:";STR$(TX%);" TY:";STR$(TY%);gw$;
        REM PRINTTAB(0,23)"PX:";STR$(PX%);" PY:";STR$(PY%);gw$;
        REM PRINTTAB(0,23)"IP:";STR$(insertphase%);" IR:";STR$(insertrepeat%);gw$;

      ENDIF
      ENDPROC

      REM ##########################################################
      REM Read the point at the specified coordinates (1=set, 0=cleared)
      DEF FNpoint(x%,y%)
      LOCAL cx%,cy%,chr%,C%
      REM Get character cell
      cx% = x% DIV 2
      cy% = y% DIV 3
      chr%=GET(cx%,cy%) AND &5F
      C%=(x% AND 1)+(y% MOD 3)*2
      C%=2^C% - (C%=5)*32
      =SGN(chr% AND C%)

      REM ##########################################################
      REM Plot a Teletext sixel point
      REM SIXEL COORDINATES WITH 0,0 BEING TOP LEFT THE SAME AS THE TEXT SCREEN
      REM cmd% 0: Clear the point
      REM cmd% 1: Set the point
      REM cmd% 2: Toggle the point XOR
      DEF PROCpoint(x%, y%, cmd%)
      IF menuext%=M_sprites% THEN
        PROCpoint_spr(x%, y%, cmd%)
      ELSE
        IF x%>xMin% AND x%<xMax% AND y%>yMin% AND y%<yMax% THEN

          LOCAL cx%,cy%,chr%,C%
          REM Get character cell
          cx% = x% DIV 2
          cy% = y% DIV 3
          chr%=GET(cx%,cy%) AND &5F
          C%=(x% AND 1)+(y% MOD 3)*2
          C%=2^C% - (C%=5)*32
          CASE cmd% OF
            WHEN 0:chr% AND=(&5F - C%)
            WHEN 1:chr% OR=C%
            WHEN 2:chr% EOR=C%
          ENDCASE

          VDU 31,cx%, cy%,(chr%+160)
        ENDIF
      ENDIF
      ENDPROC

      REM ##########################################################
      REM Read the point at the specified coordinates from specified buffer (1=set, 0=cleared)
      DEF FNpoint_buf(x%,y%,f%)
      LOCAL cx%,cy%,chr%,C%
      REM Get character cell
      cx% = x% DIV 2
      cy% = (y% DIV 3)-1
      chr%=frame_buffer&(f%-1,cx%+cy%*40) AND &5F
      C%=(x% AND 1)+(y% MOD 3)*2
      C%=2^C% - (C%=5)*32
      =SGN(chr% AND C%)

      REM ##########################################################
      REM Plot a Teletext sixel point from specified buffer
      REM SIXEL COORDINATES WITH 0,0 BEING TOP LEFT THE SAME AS THE TEXT SCREEN
      REM cmd% 0: Clear the point
      REM cmd% 1: Set the point
      REM cmd% 2: Toggle the point
      DEF PROCpoint_buf(x%, y%, cmd%,f%)

      IF x%>xMin% AND x%<xMax% AND y%>yMin% AND y%<yMax% THEN
        LOCAL cx%,cy%,chr%,C%
        REM Get character cell
        cx% = x% DIV 2
        cy% = (y% DIV 3)-1

        chr%=frame_buffer&(f%-1,cx%+cy%*40) AND &5F

        C%=(x% AND 1)+(y% MOD 3)*2
        C%=2^C% - (C%=5)*32
        CASE cmd% OF
          WHEN 0:chr% AND=(&5F - C%)
          WHEN 1:chr% OR=C%
          WHEN 2:chr% EOR=C%
        ENDCASE

        frame_buffer&(f%-1,cx%+cy%*40)=chr%+160

      ENDIF
      ENDPROC

      REM ##########################################################
      REM Plot a Teletext sixel point
      REM SIXEL COORDINATES WITH 0,0 BEING TOP LEFT THE SAME AS THE TEXT SCREEN
      REM cmd% 0: Clear the point
      REM cmd% 1: Set the point
      REM cmd% 2: Toggle the point XOR
      DEF PROCpoint_spr(x%, y%, cmd%)

      IF x%>19 AND x%<60 AND y%>8 AND y%<57 THEN

        LOCAL cx%,cy%,chr%,C%
        REM Get character cell
        cx% = x% DIV 2
        cy% = y% DIV 3
        chr%=GET(cx%,cy%) AND &5F
        C%=(x% AND 1)+(y% MOD 3)*2
        C%=2^C% - (C%=5)*32
        CASE cmd% OF
          WHEN 0:chr% AND=(&5F - C%)
          WHEN 1:chr% OR=C%
          WHEN 2:chr% EOR=C%
        ENDCASE

        VDU 31,cx%, cy%,(chr%+160)
      ENDIF

      ENDPROC

      REM ##########################################################
      REM Plot a Teletext sixel point from specified buffer
      REM SIXEL COORDINATES WITH 0,0 BEING TOP LEFT THE SAME AS THE TEXT SCREEN
      REM cmd% 0: Clear the point
      REM cmd% 1: Set the point
      REM cmd% 2: Toggle the point
      DEF PROCpoint_sprbuf(x%, y%, cmd%,s%)

      IF x%>-1 AND x%<40 AND y%>-1 AND y%<48 THEN

        LOCAL cx%,cy%,chr%,C%
        REM Get character cell
        cx% = x% DIV 2
        cy% = y% DIV 3
        chr%=sprbuf&(s%,cx%+cy%*20) AND &5F
        C%=(x% AND 1)+(y% MOD 3)*2
        C%=2^C% - (C%=5)*32
        CASE cmd% OF
          WHEN 0:chr% AND=(&5F - C%)
          WHEN 1:chr% OR=C%
          WHEN 2:chr% EOR=C%
        ENDCASE

        sprbuf&(s%,cx%+cy%*20)=chr%+160
      ENDIF
      ENDPROC

      REM ##########################################################
      REM Read the point at the specified coordinates from specified sprite buffer (1=set, 0=cleared)
      DEF FNpoint_sprbuf(x%,y%,s%)
      LOCAL cx%,cy%,chr%,C%
      REM Get character cell
      cx% = x% DIV 2
      cy% = (y% DIV 3)
      chr%=sprbuf&(s%,cx%+cy%*20)
      IF chr%>160 THEN
        chr%=chr% AND &5F
        C%=(x% AND 1)+(y% MOD 3)*2
        C%=2^C% - (C%=5)*32
        =SGN(chr% AND C%)
      ELSE
        =0
      ENDIF

      REM ##########################################################
      REM Plot a Teletext sixel point to specified buffer
      REM SIXEL COORDINATES WITH 0,0 BEING TOP LEFT THE SAME AS THE TEXT SCREEN
      REM cmd% 0: Clear the point
      REM cmd% 1: Set the point
      REM cmd% 2: Toggle the point
      DEF PROCpoint_movbuf(x%, y%, cmd%)

      IF x%>xMin% AND x%<xMax% AND y%>yMin% AND y%<yMax% THEN
        LOCAL cx%,cy%,chr%,C%
        REM Get character cell
        cx% = x% DIV 2
        cy% = (y% DIV 3)-1

        chr%=movie_buffer&(cx%+cy%*40) AND &5F

        C%=(x% AND 1)+(y% MOD 3)*2
        C%=2^C% - (C%=5)*32
        CASE cmd% OF
          WHEN 0:chr% AND=(&5F - C%)
          WHEN 1:chr% OR=C%
          WHEN 2:chr% EOR=C%
        ENDCASE

        movie_buffer&(cx%+cy%*40)=chr%+160

      ENDIF
      ENDPROC



      REM ##########################################################
      REM update grid
      DEF PROCdrawgrid
      LOCAL X%

      CASE menuext% OF
        WHEN M_canvasmode%,M_moviemode%,78
          SYS "SDL_SetRenderDrawColor", @memhdc%, 63, 63, 63, 0
          FOR X%=0 TO 39
            SYS "SDL_RenderDrawLine", @memhdc%, X%*16, 499, X%*16, 20
            IF X%<25 THEN SYS "SDL_RenderDrawLine", @memhdc%, 0, X%*20, 639, X%*20
          NEXT
          REM quarter screen
          SYS "SDL_SetRenderDrawColor", @memhdc%, 127, 127, 127, 0
          SYS "SDL_RenderDrawLine", @memhdc%, 41*8, 499, 41*8, 20
          SYS "SDL_RenderDrawLine", @memhdc%, 0, 13*20, 639, 13*20
        WHEN M_sprites%
          SYS "SDL_SetRenderDrawColor", @memhdc%, 63, 63, 63, 0

          FOR X%=0 TO 20
            IF X%<>10 THEN
              SYS "SDL_SetRenderDrawColor", @memhdc%, 63, 63, 63, 0
            ELSE
              SYS "SDL_SetRenderDrawColor", @memhdc%, 16, 127, 16, 0
            ENDIF
            SYS "SDL_RenderDrawLine", @memhdc%, X%*16+160, 60, X%*16+160, 379
            IF X%<17 THEN
              IF X%<>8 THEN
                SYS "SDL_SetRenderDrawColor", @memhdc%, 63, 63, 63, 0
              ELSE
                SYS "SDL_SetRenderDrawColor", @memhdc%, 16, 127, 16, 0
              ENDIF
              SYS "SDL_RenderDrawLine", @memhdc%, 160, X%*20+59, 480, X%*20+59
            ENDIF
          NEXT

      ENDCASE
      *REFRESH

      ENDPROC

      REM ##########################################################
      REM LINE ROUTINE USE m% TO PERFORM 0=ERASE / 1=DRAW / 2=EOR
      DEF PROCbresenham(x1%,y1%,x2%,y2%,m%)
      LOCAL dx%, dy%, sx%, sy%, e
      dx% = ABS(x2% - x1%) : sx% = SGN(x2% - x1%)
      dy% = ABS(y2% - y1%) : sy% = SGN(y2% - y1%)
      IF dx% > dy% e = dx% / 2 ELSE e = dy% / 2
      REPEAT
        PROCpoint(x1%,y1%,m%)
        IF x1% = x2% IF y1% = y2% EXIT REPEAT
        IF dx% > dy% THEN
          x1% += sx% : e -= dy% : IF e < 0 e += dx% : y1% += sy%
        ELSE
          y1% += sy% : e -= dx% : IF e < 0 e += dy% : x1% += sx%
        ENDIF
      UNTIL FALSE
      ENDPROC

      REM ##########################################################
      REM line routine using pattern template
      DEF PROCbresenham_p(x1%,y1%,x2%,y2%,m%,p%)
      LOCAL dx%, dy%, sx%, sy%, e
      dx% = ABS(x2% - x1%) : sx% = SGN(x2% - x1%)
      dy% = ABS(y2% - y1%) : sy% = SGN(y2% - y1%)
      IF dx% > dy% e = dx% / 2 ELSE e = dy% / 2
      REPEAT
        PROCpoint(x1%,y1%,pat%(p%,pc%))
        pc%=(pc%+1-(RND(10)=1)) MOD 16
        IF x1% = x2% IF y1% = y2% EXIT REPEAT
        IF dx% > dy% THEN
          x1% += sx% : e -= dy% : IF e < 0 e += dx% : y1% += sy%
        ELSE
          y1% += sy% : e -= dx% : IF e < 0 e += dy% : x1% += sx%
        ENDIF
      UNTIL FALSE
      ENDPROC


      REM ##########################################################
      REM LINE ROUTINE FOR BUFFER USE m% TO PERFORM 0=ERASE / 1=DRAW / 2=EOR
      DEF PROCbresenham_buf(x1%,y1%,x2%,y2%,m%,z%)
      LOCAL dx%, dy%, sx%, sy%, e, rx%, ty%
      dx% = ABS(x2% - x1%) : sx% = SGN(x2% - x1%)
      dy% = ABS(y2% - y1%) : sy% = SGN(y2% - y1%)
      IF dx% > dy% e = dx% / 2 ELSE e = dy% / 2

      rx%=x1% : ty%=y1%
      REPEAT
        IF animategapcount%=0 THEN

          PROCpoint_buf(x1%,y1%,m%,frame%)

          REM change frame if animate length reaches 0
          animatelencount%-=1
          IF animatelencount%=0 THEN
            animatelencount%=animatelen%
            animategapcount%=animategap%
            frame%=frame%+1
            REM            IF frame%>frame_max% THEN frame%=1
            IF frame%>frame_max% THEN
              IF z%=1 THEN
                EXIT REPEAT
              ELSE
                frame%=1
              ENDIF
            ENDIF
          ENDIF
        ELSE
          animategapcount%-=1
        ENDIF
        REM        IF x1% = x2% IF y1% = y2% EXIT REPEAT
        IF x1% = x2% IF y1% = y2% THEN
          IF z%=1 THEN
            x1%=rx% : y1%=ty%
          ELSE
            EXIT REPEAT
          ENDIF
        ENDIF
        IF dx% > dy% THEN
          x1% += sx% : e -= dy% : IF e < 0 e += dx% : y1% += sy%
        ELSE
          y1% += sy% : e -= dx% : IF e < 0 e += dy% : x1% += sx%
        ENDIF
      UNTIL FALSE
      ENDPROC

      REM ##########################################################
      REM RECTANGLE ROUTINE
      DEF PROCrectangle(x1%,y1%,x2%,y2%,m%,f%)

      REM CHECK FOR SPECIAL CASES TO PRESERVE EOR OPERATIONS
      IF x1%=x2% AND y1%=y2% THEN
        PROCpoint(x1%,y1%,m%)
      ELSE
        IF x1%=x2% OR y1%=y2% THEN
          PROCbresenham(x1%,y1%,x2%,y2%,m%)
        ELSE
          IF x1%>x2% THEN SWAP x1%,x2%
          PROCbresenham(x1%,y1%,x2%,y1%,m%)
          PROCbresenham(x1%,y2%,x2%,y2%,m%)
          IF ABS(y2%-y1%)>1 THEN
            IF y1%>y2% THEN SWAP y1%,y2%
            FOR Y%=y1%+1 TO y2%-1
              PROCpoint(x1%,Y%,m%)
              PROCpoint(x2%,Y%,m%)
              IF f%=1 THEN
                CASE shapetype% OF
                  WHEN 1 : REM filled
                    PROCbresenham(x1%,Y%,x2%,Y%,m%)
                  WHEN 2 : REM outline / empty
                    PROCbresenham(x1%+1,Y%,x2%-1,Y%,1-m%)
                ENDCASE
              ENDIF
            NEXT
          ENDIF
        ENDIF
      ENDIF
      ENDPROC

      REM ##########################################################
      REM RECTANGLE ROUTINE FOR BUFFER
      DEF PROCrectangle_buf(x1%,y1%,x2%,y2%,m%)

      REM CHECK FOR SPECIAL CASES TO PRESERVE EOR OPERATIONS
      IF x1%=x2% AND y1%=y2% THEN
        PROCpoint(x1%,y1%,m%)
      ELSE
        IF x1%=x2% OR y1%=y2% THEN
          PROCbresenham_buf(x1%,y1%,x2%,y2%,m%,0)
        ELSE
          IF x1%>x2% THEN SWAP x1%,x2%
          IF y1%>y2% THEN SWAP y1%,y2%
          PROCbresenham_buf(x1%,y1%,x2%,y1%,m%,0)
          PROCbresenham_buf(x2%,y1%+1,x2%,y2%-1,m%,0)
          PROCbresenham_buf(x2%,y2%,x1%,y2%,m%,0)
          PROCbresenham_buf(x1%,y2%-1,x1%,y1%+1,m%,0)
        ENDIF
      ENDIF
      ENDPROC

      REM ##########################################################
      REM rectangle with gradient
      DEF PROCrectangle_g(x1%,y1%,x2%,y2%,d%,dv%)

      LOCAL lx%,ly%,dc%,gR,gRd,gAdd,p%

      IF x1%=x2% OR y1%=y2% THEN ENDPROC

      REM Calculate direction vector, gradient value And gradient Default values
      gR=0 : gRd=0
      IF x1%>x2% THEN
        SWAP x1%,x2%
        REM   IF d%=0 THEN dv%=-1
      ENDIF
      IF y1%>y2% THEN
        SWAP y1%,y2%
        REM IF d%=1 THEN dv%=-1
      ENDIF
      IF dv%=-1 THEN
        gR=17.9: gRd=17.9
      ENDIF

      IF d%=1 THEN
        gAdd=18/(y2%-y1%)*dv%
      ELSE
        gAdd=18/(x2%-x1%)*dv%
      ENDIF

      FOR lx%=x1% TO x2%
        IF d% THEN gR=gRd

        FOR ly%=y1% TO y2%
          REM range check, set pattern colour and plot
          IF lx%>xMin% AND lx%<xMax% AND ly%>yMin% AND ly%<yMax% THEN
            p%=lx% MOD 4+(ly% MOD 4)*4
            dc%=pat%(grad%(gradtype%,INT(gR)),p%)
          ENDIF

          PROCpoint(lx%,ly%,dc%)

          IF d%=1 THEN
            gR+=gAdd
            IF gR>17.9 THEN gR=17.9
            IF gR<0 THEN gR=0
          ENDIF

        NEXT
        IF d%=0 THEN
          gR+=gAdd
          IF gR>17.9 THEN gR=17.9
          IF gR<0 THEN gR=0

        ENDIF
      NEXT

      ENDPROC

      REM ##########################################################
      REM diabonal gradient
      DEF PROCdiagonal_g(x1%,y1%,x2%,y2%,p%,d%)
      LOCAL h%,v%,l%,gR,gAdd,min%,max%,L%

      IF x1%=x2% OR y1%=y2% THEN ENDPROC

      IF x1%>x2% THEN SWAP x1%,x2%
      IF y1%>y2% THEN SWAP y1%,y2%

      xmin%=x1%
      xmax%=x2%
      ymin%=y1%
      ymax%=y2%

      CASE d% OF
        WHEN 0 : REM top left
          x2%=x1%
          y2%=y1%
          h%=1
          v%=1
        WHEN 1 : REM top right
          x1%=x2%
          y2%=y1%
          h%=-1
          v%=1
        WHEN 2 : REM bottom right
          x1%=x2%
          y1%=y2%
          h%=-1
          v%=-1
        WHEN 3 : REM bottom left
          x2%=x1%
          y1%=y2%
          h%=1
          v%=-1

      ENDCASE

      L%=(xmax%-xmin%)+(ymax%-ymin%)

      REM preset gradient value
      min%=0
      max%=18
      gR=0
      gAdd=18/L%
      IF min%>max% THEN gAdd=-gAdd
      pc%=0

      REM main loop
      FOR l%=0 TO L%-1
        CASE d% OF
          WHEN 0 : REM top left
            IF y1%<ymax% THEN
              y1%+=v%
            ELSE
              x1%+=h%
            ENDIF

            IF x2%<xmax% THEN
              x2%+=h%
            ELSE
              y2%+=v%
            ENDIF

          WHEN 1 : REM top right
            IF y2%<ymax% THEN
              y2%+=v%
            ELSE
              x2%+=h%
            ENDIF

            IF x1%>xmin% THEN
              x1%+=h%
            ELSE
              y1%+=v%
            ENDIF


          WHEN 2 : REM bottom right
            IF y2%>ymin% THEN
              y2%+=v%
            ELSE
              x2%+=h%
            ENDIF

            IF x1%>xmin% THEN
              x1%+=h%
            ELSE
              y1%+=v%
            ENDIF

          WHEN 3 : REM bottom left
            IF y1%>ymin% THEN
              y1%+=v%
            ELSE
              x1%+=h%
            ENDIF

            IF x2%<xmax% THEN
              x2%+=h%
            ELSE
              y2%+=v%
            ENDIF

        ENDCASE
        gR+=gAdd
        IF gR>max% OR gR<min% THEN gAdd=-gAdd
        IF gR>17.9 THEN gR=17.9
        IF gR<0 THEN gR=0
        PROCbresenham_p(x1%,y1%,x2%,y2%,1-erase%,grad%(gradtype%,INT(gR)))

      NEXT

      ENDPROC

      REM ##########################################################
      REM CIRCLE ROUTINE
      DEF PROCcircle(x1%,y1%,r%,m%,f%)
      LOCAL p,x%,y%

      r%=ABS(r%)
      p=(5-r%*4)/4
      x%=0
      y%=r%

      PROCcirclepoints(x1%,y1%,x%,y%,m%,f%)

      WHILE x%<y%
        x%+=1
        IF p<0 THEN
          p+=2*x%+1
        ELSE
          y%-=1
          p+=2*(x%-y%)+1
        ENDIF

        PROCcirclepoints(x1%,y1%,x%,y%,m%,f%)

      ENDWHILE

      ENDPROC

      REM ##########################################################
      REM THIS PLOTS THE POINTS FOR CIRCLE ROUTINE
      DEF PROCcirclepoints(cx%,cy%,x%,y%,m%,f%)
      IF x%=0 THEN
        PROCpoint(cx%,cy%+y%,m%)
        PROCpoint(cx%,cy%-y%,m%)
        PROCpoint(cx%+y%,cy%,m%)
        PROCpoint(cx%-y%,cy%,m%)
        IF f%=1 THEN
          CASE shapetype% OF
            WHEN 1 : REM filled
              PROCbresenham(cx%,cy%+y%,cx%,cy%-y%,m%)
              PROCbresenham(cx%-y%,cy%,cx%+y%,cy%,m%)
            WHEN 2 : REM outline / empty
              PROCbresenham(cx%,cy%+y%-1,cx%,cy%-y%+1,1-m%)
              PROCbresenham(cx%-y%+1,cy%,cx%+y%-1,cy%,1-m%)
          ENDCASE
        ENDIF

      ELSE
        IF x%<=y% THEN
          PROCpoint(cx%+x%,cy%+y%,m%)
          PROCpoint(cx%-x%,cy%+y%,m%)
          PROCpoint(cx%+x%,cy%-y%,m%)
          PROCpoint(cx%-x%,cy%-y%,m%)
          IF f%=1 THEN
            CASE shapetype% OF
              WHEN 1 : REM filled
                PROCbresenham(cx%+x%,cy%+y%,cx%+x%,cy%-y%,m%)
                PROCbresenham(cx%-x%,cy%+y%,cx%-x%,cy%-y%,m%)
              WHEN 2 : REM outline / empty
                PROCbresenham(cx%+x%,cy%+y%-1,cx%+x%,cy%-y%+1,1-m%)
                PROCbresenham(cx%-x%,cy%+y%-1,cx%-x%,cy%-y%+1,1-m%)
            ENDCASE
          ENDIF

          IF x%<y% THEN
            PROCpoint(cx%+y%,cy%+x%,m%)
            PROCpoint(cx%-y%,cy%+x%,m%)
            PROCpoint(cx%+y%,cy%-x%,m%)
            PROCpoint(cx%-y%,cy%-x%,m%)
            IF f%=1 THEN
              CASE shapetype% OF
                WHEN 1 : REM filled
                  PROCbresenham(cx%-y%,cy%+x%,cx%+y%,cy%+x%,m%)
                  PROCbresenham(cx%-y%,cy%-x%,cx%+y%,cy%-x%,m%)
                WHEN 2 : REM outline / empty
                  PROCbresenham(cx%-y%+1,cy%+x%,cx%+y%-1,cy%+x%,1-m%)
                  PROCbresenham(cx%-y%+1,cy%-x%,cx%+y%-1,cy%-x%,1-m%)
              ENDCASE
            ENDIF

          ENDIF
        ENDIF
      ENDIF
      ENDPROC

      REM ##########################################################
      REM ### flood fill from ART4BBW
      DEF PROCfloodfill(sx%,sy%)

      IF sx%>xMin% AND sx%<xMax% AND sy%>yMin% AND sy%<yMax% THEN

        LOCAL uf,df,c%,x%,y%
        uf=0
        df=0

        REM fill with mask colour first
        bCnt%=0
        PROCaddFill(sx%,sy%)

        REPEAT
          REM get next fill point
          bCnt%-=1
          x%=fill{(bCnt%)}.x%
          y%=fill{(bCnt%)}.y%
          IF FNpoint(x%,y%) = 0 THEN

            uf=1 : df=1

            REM scan left
            WHILE x%>fxMin% AND FNpoint(x%-1,y%) =0
              x%-=1
            ENDWHILE

            REM scan right
            WHILE x%<fxMax% AND FNpoint(x%,y%) = 0
              PROCpoint(x%,y%,1)

              REM detect colour changes above and add to list
              IF y%<fyMax% THEN
                c%=FNpoint(x%,y%+1)
                IF uf AND c%=0 THEN PROCaddFill(x%,y%+1) : uf=0
                IF c%=1 THEN uf=1
              ENDIF

              REM detect colour changes below and add to list
              IF y%>fyMin% THEN
                c%=FNpoint(x%,y%-1)
                IF df AND c%=0 THEN PROCaddFill(x%,y%-1) : df=0
                IF c%=1 THEN df=1
              ENDIF
              x%+=1
            ENDWHILE
          ENDIF

        UNTIL bCnt%=0
      ENDIF

      ENDPROC

      REM ##########################################################
      REM ### fill quasi stack
      DEF PROCaddFill(x%,y%)
      fill{(bCnt%)}.x%=x%
      fill{(bCnt%)}.y%=y%
      IF bCnt%<fillmax% THEN bCnt%+=1
      ENDPROC

      REM ##########################################################
      REM main canvas click handler
      DEF PROCcanvasmode
      LOCAL X%,Y%,A%,D%,DA%,oldframe%,char%,startx%,starty%,A$
      LOCAL F%,XF%,YF%,SM%,XC,YC,deltax,deltay,sm%

      REM IF menuext%>0 THEN PROCmenurestore

      IF spritemoving%>-1 THEN
        PROCWAITMOUSE(0)

        CASE insertmode% OF
          WHEN 0,1 : REM sprite,ani
            IF insertlarge%=0 THEN
              X%=PX%-cmWX%-sprlist{(spritemoving%)}.w%
              Y%=PY%+cmWY%-(sprlist{(spritemoving%)}.h%*3 DIV 2)
            ELSE
              X%=PX%-cmWX%
              Y%=PY%+cmWY%
            ENDIF
          WHEN 3 : REM text
            X%=((PX%-cmWX%-(LEN(txtlist$(spritemoving%))+1)) DIV 2)*2
            Y%=((PY%+cmWY%) DIV 3)*3
        ENDCASE

        IF insertlarge%=0 AND sprlist{(spritemoving%)}.m%=1 THEN
          X%=(X% DIV 2)*2
          Y%=(Y% DIV 3)*3
          sm%=1
        ENDIF

        REM PROCobjdraw(PX%,PY%,3,13)
        PROCmenurestore

        IF insertphase%=0 THEN
          CASE insertmode% OF
            WHEN 0,1 : REM sprite,ani
              IF insertlarge%=0 THEN
                PROCundosave
                PROCspritetocanvas(spritemoving%,X%,Y%)
                PROCframesave(frame%)
                spritestartframe%=frame%
                insertstartx%=X%
                insertstarty%=Y%
              ELSE

              ENDIF
            WHEN 3 : REM text

          ENDCASE
          spriteold%=spritemoving%

        ENDIF

        spriteold%=spritemoving%

        IF spritedupe%=-1 AND insertrepeat%=0 spritemoving%=-1

        IF insertrepeat%>0 THEN
          insertphase%+=1
          IF insertphase%=2 THEN
            REM add remaining animation sprites to selected frames
            REM calc difference between start frame and end frame and create a delta step variable for x and y to move animation

            insertphase%=0
            frameskip%=0
            C%=frame%-spritestartframe%
            IF C%>0 THEN
              SM%=spritemoving%
              spritemoving%=-1
              XF%=X%
              YF%=Y%

              deltax=(XF%-insertstartx%) / C%
              deltay=(YF%-insertstarty%) / C%

              XC=insertstartx%+deltax
              YC=insertstarty%+deltay

              insertani%=0
              insertani%=insertfrmindex%

              frame%=spritestartframe%
              PROCloadnextframe(1,0)

              FOR F%=1 TO C%
                REM *** FIX THIS AND ALSO IN SPR INI - ADD DIALOG TO CHOSE STARTING SPRITE
                IF insertrepeat%=1 THEN
                  IF insertset%>-1 THEN
                    insertani%=(insertani%+1) MOD sprani{(insertset%)}.c%
                    SM%=sprani{(insertset%)}.s%(insertani%)
                  ELSE
                    insertani%=(insertani%+1) MOD sprlrg_count%
                    SM%=insertani%
                  ENDIF
                ENDIF

                REM normalise end point
                IF F%=C% THEN
                  XC=XF%
                  YC=YF%
                ENDIF

                IF frameskip%=0 THEN
                  PROCundosave
                  IF sm% THEN
                    X%=(INT(XC+0.5) DIV 2)*2
                    Y%=(INT(YC+0.5) DIV 3)*3
                  ELSE
                    X%=INT(XC+0.5)
                    Y%=INT(YC+0.5
                  ENDIF

                  PROCspritetocanvas(SM%,X%,Y%)
                  REM PROCspritetocanvas(SM%,INT(XC+0.5),INT(YC+0.5))
                  PROCloadnextframe(1,1)
                ELSE
                  frameskip%-=1
                ENDIF
                XC+=deltax
                YC+=deltay
              NEXT
            ENDIF
            spritemoving%=-1
            insertrepeat%=0
            spriteselect%=-1
            spriteselectold%=-1
            PROCmenurestore
          ENDIF
        ENDIF

        IF spritemoving%>-1 PROCspritemoveinit(1)

        IF animation%=1 AND spritemoving%=-1 THEN
          PROCloadnextframe(1,0)
        ENDIF

        REM        insertrepeat%=0
        REM spriteselect%=-1
        REM spriteselectold%=-1

      ELSE

        CASE toolsel% OF
          WHEN T_paint& : REM paint tool
            PROCundosave
            PROCpoint(PX%,PY%,1-erase%)
            REPEAT
              PROCREADMOUSE
              IF PX%<>OLD_PX% OR PY%<>OLD_PY% THEN PROCpoint(PX%,PY%,1-erase%)
              OLD_PX%=PX%
              OLD_PY%=PY%
            UNTIL MB%=0
            PROCframesave(frame%)
            IF animation% THEN PROCloadnextframe(1,0)

          WHEN T_dither1&,T_dither2&,T_dither3&,T_dither4&,T_dither5& : REM dither tools
            PROCundosave

            CASE dither% OF
              WHEN 0,1,2,3 : REM patterns
                D%=2^(dither%)
                DA%=2
                IF dither%=2 THEN DA%=4
                IF dither%=3 THEN DA%=8

              WHEN 4 : REM solid
                char%=255-erase%*95 : REM SOLID BLOCK #255

            ENDCASE
            OLD_PX%=-1
            REPEAT
              PROCREADMOUSE
              IF PX%<>OLD_PX% OR PY%<>OLD_PY% THEN
                CASE dither% OF
                  WHEN 0,1,2,3 : REM patterns
                    X%=(PX% DIV DA%)*DA%
                    Y%=(PY% DIV DA%)*DA%

                    PROCpoint(X%,Y%,1-erase%)
                    PROCpoint(X%+D%,Y%+D%,1-erase%)
                  WHEN 4
                    IF TX%>0 AND TX%<40 AND TY%>0 AND TY%<25 THEN
                      char%=255-erase%*95 : REM SOLID BLOCK #255
                      VDU 31,TX%,TY%,char%
                    ENDIF
                ENDCASE
              ENDIF
              OLD_PX%=PX%
              OLD_PY%=PY%
            UNTIL MB%=0
            PROCframesave(frame%)
            IF animation% THEN PROCloadnextframe(1,0)

          WHEN T_brush1&,T_brush2&,T_brush3&,T_brush4&,T_brush5&,T_brush6&,T_brush7& : REM brush tools
            PROCundosave

            OLD_PX%=-1
            REPEAT
              PROCREADMOUSE
              IF PX%<>OLD_PX% OR PY%<>OLD_PY% THEN
                CASE toolsel% OF
                  WHEN T_brush1& : REM brush 2   \
                    FOR X%=-2 TO 2
                      PROCpoint(PX%+X%,PY%+X%,1-erase%)
                    NEXT
                  WHEN T_brush2& : REM brush 2   /
                    FOR X%=-2 TO 2
                      PROCpoint(PX%+X%,PY%-X%,1-erase%)
                    NEXT

                  WHEN T_brush3& : REM brush 3  -
                    FOR X%=-2 TO 2
                      PROCpoint(PX%+X%,PY%,1-erase%)
                    NEXT

                  WHEN T_brush4& : REM brush 4 |
                    FOR X%=-2 TO 2
                      PROCpoint(PX%,PY%+X%,1-erase%)
                    NEXT

                  WHEN T_brush5& : REM brush 5 * 1
                    FOR X%=-1 TO 1
                      PROCbresenham(PX%-1,PY%+X%,PX%+1,PY%+X%,1-erase%)
                    NEXT

                  WHEN T_brush6& : REM brush 6 * 2
                    FOR X%=-2 TO 2
                      PROCbresenham(PX%-2-(ABS(X%)>1),PY%+X%,PX%+2+(ABS(X%)>1),PY%+X%,1-erase%)
                    NEXT

                  WHEN T_brush7& : REM brush 7 * 3
                    FOR X%=-3 TO 3
                      PROCbresenham(PX%-3-(ABS(X%)>2),PY%+X%,PX%+3+(ABS(X%)>2),PY%+X%,1-erase%)
                    NEXT

                ENDCASE
              ENDIF
              OLD_PX%=PX%
              OLD_PY%=PY%
            UNTIL MB%=0
            PROCframesave(frame%)
            IF animation% THEN PROCloadnextframe(1,0)

          WHEN T_paste&: REM paste tool

            PROCWAITMOUSE(0)
            PROCpasteregion(TX%,TY%)
            PROCframesave(frame%)
            IF animation% THEN PROCloadnextframe(1,0)

          WHEN T_fill&: REM fill tools
            PROCundosave
            PROCfloodfill(PX%,PY%)
            PROCWAITMOUSE(0)
            PROCframesave(frame%)
            IF animation% THEN PROCloadnextframe(1,0)

          WHEN T_line& : REM line tool
            IF animateshape% THEN
              PROCundosaveall
              PROCmenudraw
            ELSE
              PROCundosave
            ENDIF
            startx%=PX%: starty%=PY%
            OLD_PX%=PX% : OLD_PY%=PY%
            PROCpoint(startx%,starty%,2)

            REPEAT
              PROCREADMOUSE
              IF PX%<>OLD_PX% OR PY%<>OLD_PY% THEN
                PROCbresenham(startx%,starty%,OLD_PX%,OLD_PY%,2)
                PROCbresenham(startx%,starty%,PX%,PY%,2)
                OLD_PX%=PX%
                OLD_PY%=PY%
              ENDIF
            UNTIL MB%=0

            REM PROCbresenham(startx%,starty%,PX%,PY%,2)
            PROCmenurestore

            IF animateshape% THEN
              oldframe%=frame%
              PROCframesave(frame%)
              animatelencount%=animatelen%
              animategapcount%=0
              PROCbresenham_buf(startx%,starty%,PX%,PY%,1-erase%,1)
              frame%=oldframe%-1
              PROCloadnextframe(1,0)
            ELSE
              PROCbresenham(startx%,starty%,PX%,PY%,1-erase%)
              PROCframesave(frame%)
              IF animation% THEN PROCloadnextframe(1,0)
            ENDIF

          WHEN T_box&,T_gradl&,T_gradr&,T_gradt&,T_gradb&,T_gradtl&,T_gradtr&,T_gradbr&,T_gradbl& : REM rectangle / grad tools
            IF animateshape% AND toolsel%=T_box& THEN
              PROCundosaveall
              PROCmenudraw
            ELSE
              PROCundosave
            ENDIF

            startx%=PX%: starty%=PY%
            OLD_PX%=PX% : OLD_PY%=PY%
            PROCpoint(startx%,starty%,2)

            REPEAT
              PROCREADMOUSE
              IF PX%<>OLD_PX% OR PY%<>OLD_PY% THEN
                PROCrectangle(startx%,starty%,OLD_PX%,OLD_PY%,2,0)
                PROCrectangle(startx%,starty%,PX%,PY%,2,0)
                OLD_PX%=PX%
                OLD_PY%=PY%
              ENDIF
            UNTIL MB%=0
            REM PROCrectangle(startx%,starty%,PX%,PY%,2)
            PROCmenurestore
            IF animateshape%=1 AND toolsel%=T_box& THEN
              oldframe%=frame%
              PROCframesave(frame%)
              animatelencount%=animatelen%
              animategapcount%=0
              PROCrectangle_buf(startx%,starty%,PX%,PY%,1-erase%)
              frame%=oldframe%-1
              PROCloadnextframe(1,0)
            ELSE

              CASE toolsel% OF
                WHEN T_box& : REM rectangle
                  PROCrectangle(startx%,starty%,PX%,PY%,1-erase%,1)
                WHEN T_gradl& : REM gradient left to right
                  PROCrectangle_g(startx%,starty%,PX%,PY%,0,1)
                WHEN T_gradr& : REM gradient right to left
                  PROCrectangle_g(startx%,starty%,PX%,PY%,0,-1)
                WHEN T_gradt& : REM gradient top to bottom
                  PROCrectangle_g(startx%,starty%,PX%,PY%,1,1)
                WHEN T_gradb& : REM gradient bottom to top
                  PROCrectangle_g(startx%,starty%,PX%,PY%,1,-1)
                WHEN T_gradtl& : REM gradient top left to bottom right
                  PROCdiagonal_g(startx%,starty%,PX%,PY%,0,0)
                WHEN T_gradtr& : REM gradient top right to bottom left
                  PROCdiagonal_g(startx%,starty%,PX%,PY%,0,1)
                WHEN T_gradbr& : REM gradient bottom right to top left
                  PROCdiagonal_g(startx%,starty%,PX%,PY%,0,2)
                WHEN T_gradbl& : REM gradient bottom left to top right
                  PROCdiagonal_g(startx%,starty%,PX%,PY%,0,3)

              ENDCASE

              PROCframesave(frame%)
              IF animation% THEN PROCloadnextframe(1,0)
            ENDIF

          WHEN T_circle&: REM circle
            PROCundosave
            startx%=PX%: starty%=PY%
            OLD_PX%=PX% : OLD_PY%=PY%
            REM PROCpoint(startx%,starty%,2)

            REPEAT
              PROCREADMOUSE
              IF PX%<>OLD_PX% OR PY%<>OLD_PY% THEN
                PROCcircle(startx%,starty%,startx%-OLD_PX%,2,0)
                PROCcircle(startx%,starty%,startx%-PX%,2,0)
                OLD_PX%=PX%
                OLD_PY%=PY%
              ENDIF
            UNTIL MB%=0
            PROCmenurestore
            PROCcircle(startx%,starty%,startx%-PX%,1-erase%,1)
            PROCframesave(frame%)
            IF animation% THEN PROCloadnextframe(1,0)

          WHEN T_symmetry& : REM symmetry tool
            PROCundosave
            PROCpoint(PX%,PY%,1-erase%)
            PROCpoint(79-(PX%-2),PY%,1-erase%)
            PROCpoint(79-(PX%-2),74-(PY%-3),1-erase%)
            PROCpoint(PX%,74-(PY%-3),1-erase%)

            REPEAT
              PROCREADMOUSE
              IF PX%<>OLD_PX% OR PY%<>OLD_PY% THEN
                PROCpoint(PX%,PY%,1-erase%)
                PROCpoint(79-(PX%-2),PY%,1-erase%)
                PROCpoint(79-(PX%-2),74-(PY%-3),1-erase%)
                PROCpoint(PX%,74-(PY%-3),1-erase%)
                OLD_PX%=PX%
                OLD_PY%=PY%
              ENDIF
            UNTIL MB%=0
            PROCframesave(frame%)
            IF animation% THEN PROCloadnextframe(1,0)

          WHEN T_flash&,T_double&,T_separate&,T_hold& : REM special control codes
            PROCundosave
            X%=toolsel%-T_flash&
            IF TX%<40 AND TX%>-1 AND TY%>0 AND TY%<25 THEN VDU 31,TX%,TY%,scode&(X%*2+erase%)
            REPEAT
              PROCREADMOUSE
              IF TX%<>OLD_TX% OR TY%<>OLD_TY% THEN
                IF TX%<40 AND TX%>-1 AND TY%>0 AND TY%<25 THEN VDU 31,TX%,TY%,scode&(X%*2+erase%)
              ENDIF
              OLD_TX%=TX%
              OLD_TY%=TY%
            UNTIL MB%=0
            PROCframesave(frame%)
            IF animation% THEN PROCloadnextframe(1,0)

          WHEN T_text&: REM text print tool
            PROCundosave
            PROCWAITMOUSE(0)
            IF fontcur%=0 THEN
              A$=LEFT$(text$,40-TX%)
              FOR X%=0 TO LEN(A$)-1
                A%=ASC(MID$(A$,X%+1,1))
                CASE A% OF
                  WHEN 35 : A%=96
                  WHEN 95 : A%=35
                  WHEN 96 : A%=95
                ENDCASE
                VDU 31,TX%+X%,TY%,A% : REM ASC(MID$(A$,X%+1,1))+128
              NEXT
              REM                PRINTTAB(TX%,TY%)A$;
            ELSE
              PROCdrawfont(PX%,PY%,text$)
            ENDIF
            PROCframesave(frame%)
            IF animation% THEN PROCloadnextframe(1,0)

          WHEN T_backg& : REM background colour
            PROCundosave

            IF colmode%=1 THEN
              PROCWAITMOUSE(4)
              PROCWAITMOUSE(0)

              IF TX%<39 AND TX%>-1 AND TY%>0 AND TY%<25 THEN
                FOR Y%=1 TO 24
                  IF erase% THEN
                    VDU 31,TX%,Y%,156
                  ELSE
                    VDU 31,TX%,Y%,(curcol%+144),157
                  ENDIF
                NEXT
              ENDIF
            ELSE
              IF TX%<39 AND TX%>-1 AND TY%>0 AND TY%<25 THEN
                IF erase% THEN
                  VDU 31,TX%,TY%,156
                ELSE
                  VDU 31,TX%,TY%,(curcol%+144),157
                ENDIF
              ENDIF
              REPEAT
                PROCREADMOUSE
                IF TX%<>OLD_TX% OR TY%<>OLD_TY% THEN
                  IF TX%<39 AND TX%>-1 AND TY%>0 AND TY%<25 THEN
                    IF erase% THEN
                      VDU 31,TX%,TY%,156
                    ELSE
                      VDU 31,TX%,TY%,(curcol%+144),157
                    ENDIF
                  ENDIF
                ENDIF
                OLD_TX%=TX%
                OLD_TY%=TY%
              UNTIL MB%=0
            ENDIF

            PROCframesave(frame%)
            IF animation% THEN PROCloadnextframe(1,0)

          WHEN T_foreg& : REM foreground colour
            PROCundosave

            IF colmode%=1 THEN
              PROCWAITMOUSE(4)
              PROCWAITMOUSE(0)

              FOR Y%=1 TO 24
                IF TX%<40 AND TX%>-1 AND TY%>0 AND TY%<25 THEN
                  VDU 31,TX%,Y%,(curcol%+144-textfore%*16)
                ELSE
                  EXIT FOR
                ENDIF
              NEXT
            ELSE
              IF TX%<40 AND TX%>-1 AND TY%>0 AND TY%<25 THEN VDU 31,TX%,TY%,(curcol%+144-textfore%*16)
              REPEAT
                PROCREADMOUSE
                IF TX%<>OLD_TX% OR TY%<>OLD_TY% THEN
                  IF TX%<40 AND TX%>-1 AND TY%>0 AND TY%<25 THEN VDU 31,TX%,TY%,(curcol%+144-textfore%*16)
                  OLD_TX%=TX%
                  OLD_TY%=TY%
                ENDIF
              UNTIL MB%=0
            ENDIF

            PROCframesave(frame%)
            IF animation% THEN PROCloadnextframe(1,0)

        ENDCASE  : REM toolsel%
      ENDIF
      ENDPROC

      REM ##########################################################
      REM MOVIE MODE click handler
      DEF PROCmoviemode
      LOCAL shift%,skip%,X%,Y%,framestart%,frameskip%,SP%

      shift%=INKEY(-1)
      PROCWAITMOUSE(0)

      REM skip inserting object on background frame if animated or relative objects selected
      IF insertrepeat%>0 AND insertrepeat%<4 AND movieframe%=-1 skip%=1
      IF insertrelflag%=1 AND movieframe%=-1 skip%=1

      IF skip%=0 THEN

        REM determine coordinates based on object type
        IF spritemoving%>-1 AND spritemoving%<>9999 THEN
          CASE insertmode% OF
            WHEN 0,1 : REM sprite,ani
              IF insertlarge%=0 THEN
                X%=mmWX%+PX%-sprlist{(spritemoving%)}.w%
                Y%=mmWY%-PY%+(sprlist{(spritemoving%)}.h%*3 DIV 2)
                IF insertrelflag%=1 THEN
                  X%=X%-frmlist{(movieframe%)}.x%
                  Y%=Y%-frmlist{(movieframe%)}.y%
                  REM PRINTTAB(0,20)"X,Y: ";STR$(X%);",";STR$(Y%);"   ";
                  REM PRINTTAB(0,21)"WX,WY: ";STR$(WX%);",";STR$(WY%);"   ";
                  REM A=GET
                ENDIF
              ELSE
                X%=mmWX%+PX%
                Y%=mmWY%-PY%
              ENDIF
            WHEN 3 : REM text
              X%=((mmWX%+PX%-(LEN(txtlist$(spritemoving%))+1)) DIV 2)*2
              Y%=((mmWY%-PY%) DIV 3)*3
          ENDCASE
        ENDIF

        IF insertrepeat%<4 THEN
          IF spritemoving%>-1 THEN
            IF insertlarge%=0 THEN
              IF sprlist{(spritemoving%)}.m%=1 THEN
                X%=(X% DIV 2)*2
                Y%=(Y% DIV 3)*3
              ENDIF
            ENDIF
            PROCobjdraw(PX%,PY%,3,13)

            IF spriterelocate%=1 THEN
              REM move sprite
              objlist{(obj_lstcur%)}.obj%=spritemoving%
              objlist{(obj_lstcur%)}.x%=X%
              objlist{(obj_lstcur%)}.y%=Y%

              spriterelocate%=0
            ELSE

              IF insertphase%=0 THEN
                IF obj_lstcount%<obj_lstmax% obj_lstcount%+=1
                obj_lstcur%=obj_lstcount%

                REM add sprite to spritelist
                IF spritedupe%=-1 THEN
                  spriteselect%=-1
                  spriteselectold%=-1
                ELSE
                  IF objlist{(spritedupe%)}.f%>-1 shift%=0
                ENDIF

                CASE insertmode% OF
                  WHEN 0,1 : REM sprite,ani
                    IF insertlarge%=0 THEN
                      objlist{(obj_lstcount%)}.type%=1
                    ELSE
                      objlist{(obj_lstcount%)}.type%=4
                    ENDIF
                  WHEN 3 : REM text
                    objlist{(obj_lstcount%)}.type%=3
                    objlist{(obj_lstcount%)}.c%=inserttextcol%
                ENDCASE
                IF insertrelflag%=1 THEN

                ENDIF

                objlist{(obj_lstcount%)}.obj%=spritemoving%
                objlist{(obj_lstcount%)}.x%=X%
                objlist{(obj_lstcount%)}.y%=Y%
                objlist{(obj_lstcount%)}.parent%=-1
                objlist{(obj_lstcount%)}.rel%=insertrelflag%
                objlist{(obj_lstcount%)}.f%=movieframe%
                spriteold%=spritemoving%

              ENDIF
            ENDIF

            IF spritedupe%=-1 AND insertrepeat%=0 spritemoving%=-1

            IF insertrepeat%>0 THEN
              insertphase%+=1
              IF insertphase%=2 THEN
                REM add remaining animation sprites to selected frames
                REM calc difference between start frame and end frame and create a delta step variable for x and y to move animation
                REM
                insertphase%=0
                framestart%=objlist{(obj_lstcur%)}.f%
                frameskip%=insertskipcount% : REM objlist{(obj_lstcur%)}.hop%
                C%=movieframe%-framestart%
                IF C%>0 THEN
                  XS%=objlist{(obj_lstcur%)}.x%
                  YS%=objlist{(obj_lstcur%)}.y%
                  XF%=X%
                  YF%=Y%

                  deltax=(XF%-XS%) / C%
                  deltay=(YF%-YS%) / C%

                  XC=XS%+deltax
                  YC=YS%+deltay

                  REM select starting sprite
                  IF insertrepeat%=1 THEN
                    IF insertset%>-1 THEN
                      SP%=insertfrmrep%
                      insertani%=insertfrmindex%-1
                      spritemoving%=sprani{(insertset%)}.s%(insertani%)
                    ELSE
                      insertani%=0
                    ENDIF
                  ENDIF

                  REM loop through frames and insert sprites
                  FOR F%=1 TO C%
                    IF insertrepeat%=1 THEN
                      IF insertset%>-1 THEN
                        IF SP%=0 THEN
                          insertani%=(insertani%+1) MOD sprani{(insertset%)}.c%
                          spritemoving%=sprani{(insertset%)}.s%(insertani%)
                          SP%=insertfrmrep%
                        ELSE
                          SP%-=1
                        ENDIF
                      ELSE
                        insertani%=(insertani%+1) MOD sprlrg_count%
                        spritemoving%=insertani%
                      ENDIF
                    ENDIF

                    REM normalise end point
                    IF F%=C% THEN
                      XC=XF%
                      YC=YF%
                    ENDIF

                    IF frameskip%=0 THEN
                      IF obj_lstcount%<obj_lstmax% obj_lstcount%+=1
                      obj_lstcur%=obj_lstcount%
                      objlist{(obj_lstcount%)}.parent%=-1
                      objlist{(obj_lstcount%)}.obj%=spritemoving%
                      IF insertrepeat%=3 THEN
                        objlist{(obj_lstcount%)}.type%=3
                        objlist{(obj_lstcount%)}.c%=inserttextcol%
                      ELSE
                        IF insertlarge%=0 THEN
                          objlist{(obj_lstcount%)}.type%=1
                        ELSE
                          objlist{(obj_lstcount%)}.type%=4
                        ENDIF
                      ENDIF
                      objlist{(obj_lstcount%)}.x%=INT(XC+0.5)
                      objlist{(obj_lstcount%)}.y%=INT(YC+0.5)
                      objlist{(obj_lstcount%)}.f%=framestart%+F%
                      objlist{(obj_lstcount%)}.rel%=insertrelflag%
                      frameskip%=insertskipcount% : REM objlist{(SP%)}.hop%
                    ELSE
                      frameskip%-=1
                    ENDIF
                    XC+=deltax
                    YC+=deltay

                  NEXT
                ENDIF
                spritemoving%=-1
                insertrepeat%=0
                spriteselect%=-1
                spriteselectold%=-1
              ENDIF
            ENDIF

          ENDIF
        ELSE
          REM handle panning position
          insertphase%+=1
          IF insertphase%=1 THEN
            insertstartx%=mmWX%
            insertstarty%=mmWY%
          ELSE

            insertendx%=mmWX%
            insertendy%=mmWY%

            xdir%=SGN(insertendx%-insertstartx%)
            ydir%=SGN(insertendy%-insertstarty%)

            accelerate = 0.1
            decelearterad = 1
            topspeed = 5
            deltax = accelerate
            deltay = 0

            XC=insertstartx%
            YC=insertstarty%

            REPEAT
              IF movieframetotal%<movieframemax% THEN
                movieframetotal%+=1
                frmlist{(movieframetotal%)}.x%=INT(XC+0.5)
                frmlist{(movieframetotal%)}.y%=INT(YC+0.5)
                frmlist{(movieframetotal%)}.b%=0
                frmlist{(movieframetotal%)}.f%=7

                XC+=deltax*xdir%
                YC+=deltay*ydir%

                distance=(XC-insertendx%)^2+(YC-insertendy%)^2
                inrange=distance+(topspeed/2)<=(decelearterad^2)

                REM IF XC>=(insertendx%-decelearterad) THEN
                IF inrange THEN
                  IF DEBUG% PRINTTAB(0,6)"DECELERATE!!";
                  IF deltax>accelerate deltax -= accelerate
                  IF deltax<accelerate deltax = accelerate
                  IF distance<1 THEN
                    XC = insertendx%
                    YC = insertendx%
                  ENDIF
                ELSE
                  IF deltax<topspeed THEN
                    deltax += accelerate
                    IF deltax>topspeed deltax=topspeed
                    decelearterad=ABS(INT(XC+0.5)-insertstartx%)
                  ENDIF
                ENDIF

              ENDIF
              IF DEBUG% THEN
                PRINTTAB(0,1)"XS: ";STR$(insertstartx%);"  XE: ";STR$(insertendx%)
                PRINTTAB(0,2)"XC: ";STR$(XC);"   ";
                PRINTTAB(0,3)"DX: ";STR$(deltax);"   ";
                PRINTTAB(0,4)"DC: ";STR$(decelearterad);"   ";
                PRINTTAB(0,5)"IN: ";STR$(inrange);"  ";
                A=GET
              ENDIF
            UNTIL XC = insertendx%
            movieframe%=movieframetotal%

            insertphase%=0
            spritemoving%=-1
            insertrepeat%=0
            spriteselect%=-1
            spriteselectold%=-1

          ENDIF
        ENDIF
        PROCmenurestore

        IF spritemoving%>-1 PROCspritemoveinit(1)

      ENDIF

      ENDPROC

      REM ##########################################################
      REM MENU HANDLER
      DEF PROCmenuhandler
      PROCWAITMOUSE(0)

      REM first pass to reset sub menu or determine calling screen
      CASE menuext% OF
        WHEN M_moviemode%,M_canvasmode%
          menufrom%=menuext%
        WHEN M_sprites%
          IF TX%<>39 menufrom%=menuext%
        WHEN M_paint%
          IF TX%<>15 PROCmenurestore
        WHEN M_dither%
          IF TX%<>16 PROCmenurestore
        WHEN M_copypaste%
          IF TX%<>17 PROCmenurestore
        WHEN M_fill%
          IF TX%<>18 PROCmenurestore
        WHEN M_special%
          IF TX%<>19 PROCmenurestore
        WHEN M_sprProperty%
          IF TX%<>31 PROCmenurestore
        WHEN M_sprSelect%
          IF menufrom%=M_moviemode% AND TX%<>33 PROCmenurestore
          IF (menufrom%=M_canvasmode% OR menufrom%=M_sprites%) AND TX%<>30 PROCmenurestore
        WHEN M_frmProperty%
          IF TX%<1 OR TX%>6 PROCmenurestore
        WHEN M_movieMenu%
          IF TX%<>29 PROCmenurestore
      ENDCASE

      REM second pass to determine new sub menu
      CASE menuext% OF
        WHEN M_paint%,M_dither%,M_copypaste%,M_fill%,M_special%,M_sprSelect%,M_sprProperty%,M_frmProperty%,M_movieMenu% : REM close sub menu if already open
          PROCmenurestore

        WHEN M_moviemode%
          CASE TX% OF
            WHEN 1,2,3,4,5,6 : REM frame object sub menu
              menuext%=M_frmProperty%
              PROCsubinit(7)

            WHEN 29 : REM movie sub menu
              menuext%=M_movieMenu%
              PROCsubinit(8)

            WHEN 31 : REM edit object sub menu
              IF spriteselect%>-1 obj_lstcur%=spriteselect%
              menuext%=M_sprProperty%
              PROCsubinit(6)

            WHEN 33 : REM insert object sub menu
              menuext%=M_sprSelect%
              PROCsubinit(5)

            WHEN 35 : REM sprites edit screen
              menuext%=M_sprites%
              PROCspritescreen(1)

            WHEN 37 : REM text edit screen
              menuext%=M_keyboard%
              PROCkeyboardmenu(1)

            WHEN 39 : REM exit movie mode
              menuext%=M_canvasmode%
              menufrom%=M_canvasmode%
              PROCmenurestore

            OTHERWISE

          ENDCASE

        OTHERWISE
          CASE TX% OF
            WHEN 0 : REM display control codes
              PROCmenucheck
              PROCcontrolcodes(1,7)
              frame%-=1
              PROCloadnextframe(1,0)

            WHEN 1,2,3,4,5,6,7,8,9,10,11,12,13,14 : REM colour selector
              oldcol%=curcol%
              curcol%=(TX%+1) DIV 2

              IF curcol%=oldcol% THEN
                textfore%=(textfore%+1) AND 1
              ENDIF

            WHEN 15 : REM paint sub menu
              IF menuext%=M_canvasmode% THEN PROCframesave(frame%)
              menuext%=M_paint%
              PROCsubinit(0)

            WHEN 16 : REM dither & scale merged
              IF menuext%=M_canvasmode% THEN PROCframesave(frame%)
              menuext%=M_dither%
              PROCsubinit(1)

            WHEN 17 : REM copy sub menu
              IF menuext%=M_canvasmode% THEN PROCframesave(frame%)
              menuext%=M_copypaste%
              PROCsubinit(2)

            WHEN 18 : REM fill menu
              IF menuext%=M_canvasmode% THEN PROCframesave(frame%)
              menuext%=M_fill%
              PROCsubinit(3)

            WHEN 19 : REM shape / special menu
              IF menuext%=M_canvasmode% THEN PROCframesave(frame%)
              menuext%=M_special%
              PROCsubinit(4)

            WHEN 21 : erase%=(erase%+1) AND 1 : REM toggle erase tool

            WHEN 23 : REM undo PROCmenurestore
              IF menuext%=M_canvasmode% OR menuext%=M_sprites% THEN
                PROCundorestore
              ELSE
                PROCmenucheck
              ENDIF

            WHEN 25 : REM redo PROCmenurestore
              IF menuext%=M_canvasmode% OR menuext%=M_sprites% THEN
                PROCredorestore
              ELSE
                PROCmenucheck
              ENDIF

            WHEN 27 : PROCmenucheck : PROCclearscreen:toolsel%=T_paint&:toolcursor%=15 : REM clearscreen PROCmenurestore:
            WHEN 28 : toolsel%=T_backg&:toolcursor%=TX% : REM background colour
            WHEN 29 : toolsel%=T_foreg&:toolcursor%=TX% : REM foreground colour

            WHEN 30 : REM insert object
              IF menuext%=M_canvasmode% THEN PROCframesave(frame%)
              menuext%=M_sprSelect%
              PROCsubinit(5)

            WHEN 31 : PROCmenucheck : PROCloadfile(0) : REM load file dialog - 0 load bin file PROCmenurestore:
            WHEN 32 : PROCmenucheck : PROCsavefile : REM save frames to file PROCmenurestore:

            WHEN 34 : animation%=(animation%+1) AND 1 : REM toggle frame animation advance tool

              REM                  WHEN 36 : REM frame%
            WHEN 36,37 : REM save current frame and display previous frame in sequence PROCmenurestore
              IF menuext%=M_canvasmode% PROCmenucheck : PROCloadnextframe(-1,1)
            WHEN 38 : REM save current frame and display next frame in sequence  PROCmenurestore:
              IF menuext%=M_canvasmode% PROCmenucheck : PROCloadnextframe(1,1)
            WHEN 39 :  REM save current frame and play all frames in a loop  PROCmenurestore:
              IF menuext%=M_canvasmode% PROCmenucheck : PROCplay
              IF menuext%=M_sprites% OR menuext%=M_keyboard% THEN
                menufrom%=menumode%
                PROCmenurestore
              ENDIF

          ENDCASE

      ENDCASE
      REM hide shape menu if another menu item was clicked
      CASE menuext% OF
        WHEN M_keyboard% : REM keyboard and options
          REM IF TX%<>19 THEN PROCmenurestore

        WHEN M_paint% : REM paint sub menu

        WHEN M_dither% : REM dither sub menu

        WHEN M_special% : REM special sub menu

        WHEN M_fill% : REM fill sub menu

        WHEN M_copypaste% : REM copy paste sub menu

        WHEN M_sprSelect% : REM sprite select sub menu

        WHEN M_sprProperty% : REM object properties sub menu

        WHEN M_frmProperty% : REM frame properties sub menu

        WHEN M_movieMenu% : REM movie mode sub menu

        OTHERWISE
          PROCmenudraw
      ENDCASE

      IF gridshow%=1 THEN PROCdrawgrid

      ENDPROC


      REM ##########################################################
      REM check keyboard
      DEF PROCkeyboardhandler
      LOCAL nf%,ctrl%,shift%,OWX%,OWY%,OMF%,SX%,SY%,first%,selectnew%,oldselect%
      LOCAL newWX%,newWY%

      shift%=INKEY(-1)
      ctrl%=INKEY(-2)

      K%=INKEY(0)

      REM F12 debug mode
      IF K%=156 DEBUG%=1-DEBUG%

      REM escape key cancels menus, moving or selected sprites
      IF K%=27 THEN
        nf%=0
        IF menuext%=M_canvasmode% esccodes%=1

        IF spritemoving%>-1 THEN
          spritemoving%=-1
          spriterelocate%=0
          spritedupe%=-1
          insertrepeat%=0
          insertphase%=0
          insertlarge%=0
          nf%=1
        ENDIF
        IF spriteselect%>-1 THEN
          spriteselect%=-1
          spriteselectold%=-1
          nf%=1
        ENDIF
        IF menuext%<>M_canvasmode% AND menuext%<>M_moviemode% AND menuext%<>M_keyboard% AND menuext%<>M_sprites% THEN
          nf%=1
        ENDIF
        IF nf%=1 PROCmenurestore
      ENDIF

      CASE menuext% OF
        WHEN M_canvasmode% : REM keyboard handler

          REM TEXT AT CURSOR HANDLER, IF MOUSE IS MOVED, NEW TEXT POS IS SET

          REM sprite moving screen reference
          OWX%=cmWX%
          OWY%=cmWY%
          newWX%=cmWX%
          newWY%=cmWY%
          OMF%=frame%

          IF K%>1 THEN

            REM PRINTTAB(0,1)STR$(K%);"  ";STR$(shift%);"  ";

            REM SAVE UNDO ONLY FOR CURRENT 'LINE' OF TEXT
            IF TX%<>OTX% OR TY%<>OTY% AND TY%>0 THEN
              OTX%=TX%
              OTY%=TY%
            ENDIF

            REM handle specific keypresses
            CASE K% OF
              WHEN 3 : REM ctrl + c
                IF ctrl% THEN PROCselectregion

              WHEN 8 : REM backspace
                IF TEXTX%>TX% AND TY%>0 THEN
                  PROCundosave
                  REM IF AT END OF LINE CHECK LAST CHAR IF SPACE ALREADY
                  C%=GET(TEXTX%,TY%)
                  IF TEXTX%=39 AND C%<>32 AND C%<>160 THEN
                    VDU 31,TEXTX%,TY%,32
                  ELSE
                    TEXTX%-=1
                    VDU 31,TEXTX%,TY%,32
                  ENDIF
                  PROCframesave(frame%)
                ENDIF

              WHEN 128 : REM left + ctrl

                REM copy current frame to prev frame
                nf%=frame%-1
                IF nf%<1 THEN nf%=frame_max%

                PROCcopyframe(frame%,nf%,0,0,0)

                PROCWAITNOKEY(0,-1)

              WHEN 129 : REM left + ctrl
                REM copy current frame to next frame
                nf%=frame%+1
                IF nf%>frame_max% THEN nf%=1
                PROCcopyframe(frame%,nf%,0,0,0)

              WHEN 134 : REM insert
                REM open object insert menu
                menuext%=M_sprSelect%
                PROCsubinit(5)

              WHEN 135 : REM del
                REM insert last selected object directly to world map
                IF spritemoving%=-1 THEN
                  spritemoving%=spriteold%
                  CASE insertmode% OF
                    WHEN 0 : REM spr
                    WHEN 1 : REM ani
                    WHEN 2 : REM frm
                    WHEN 3 : REM text
                  ENDCASE
                  PROCspriteinserthandler(1)
                  PROCspritemoveinit(1)
                ENDIF

              WHEN 136 : REM left cursor
                IF shift% THEN
                  REM shift selection left in pixel increments
                  PROCmoveregion(-1,0)
                ELSE
                  IF spritemoving%=-1 THEN
                    REM save current frame and load previous frame
                    PROCloadnextframe(-1,1)
                  ELSE
                    REM pan window for sprites
                    newWX%+=1
                  ENDIF
                ENDIF

              WHEN 137 : REM right cursor
                IF shift% THEN
                  REM shift selection right in pixel increments
                  PROCmoveregion(1,0)
                ELSE
                  IF spritemoving%=-1 THEN
                    REM save current frame and load next frame
                    PROCloadnextframe(1,1)
                  ELSE
                    REM pan window for sprites
                    newWX%-=1
                  ENDIF
                ENDIF

              WHEN 138 : REM down cursor
                IF shift% THEN
                  REM shift selection down in pixel increments
                  PROCmoveregion(0,1)
                ELSE
                  REM pan window for sprites
                  IF spritemoving%<>-1 THEN
                    newWY%+=1
                  ENDIF
                ENDIF

              WHEN 139 : REM up cursor
                IF shift% THEN
                  REM shift selection up in pixel increments
                  PROCmoveregion(0,-1)
                ELSE
                  REM pan window for sprites
                  IF spritemoving%<>-1 THEN
                    newWY%-=1
                  ENDIF
                ENDIF

              WHEN 132,158 : REM pgup
                PROCloadnextframe(1,1)
                OWX%=cmWX%
                OWY%=cmWY%
                newWX%=cmWX%
                newWY%=cmWY%
                OMF%=frame%

              WHEN 133,159 : REM pgdn
                PROCloadnextframe(-1,1)
                OWX%=cmWX%
                OWY%=cmWY%
                newWX%=cmWX%
                newWY%=cmWY%
                OMF%=frame%

              OTHERWISE

                IF K%>31 AND K%<127 THEN
                  IF esccodes%=0 THEN
                    REM ADD VALID CHARS AND INCREASE TEXT POS
                    IF TY%>0 THEN
                      IF fontcur%=0 THEN
                        PROCundosave
                        VDU 31,TEXTX%,TY%,K%+128
                        IF TEXTX%<39 THEN TEXTX%+=1
                      ELSE
                        IF fonts{(K%-32)}.a%<>0 THEN
                          PROCundosave
                          REM A$=CHR$(K%)
                          PROCdrawfont(FONTX%,PY%,CHR$(K%))
                          FONTX%+=fonts{(K%-32)}.w%
                        ENDIF
                      ENDIF
                    ENDIF
                  ELSE
                    REM handle escape code hotkeys
                    CASE K% OF
                      WHEN 68,100 : REM D & d - double height text
                        first%=(K%=100)
                        PROCWAITNOKEY(-51,0)
                        IF TY%>0 THEN
                          PROCundosave
                          IF TX%<40 AND TX%>-1 AND TY%>0 AND TY%<25 THEN VDU 31,TX%,TY%,scode&(2-first%)
                          PROCframesave(frame%)
                          IF animation% THEN PROCloadnextframe(1,0)
                          esccodes%=0
                        ENDIF

                      WHEN 70,102 : REM F & f - flashing text
                        first%=(K%=102)
                        PROCWAITNOKEY(-68,0)
                        IF TY%>0 THEN
                          PROCundosave
                          IF TX%<40 AND TX%>-1 AND TY%>0 AND TY%<25 THEN VDU 31,TX%,TY%,scode&(0-first%)
                          PROCframesave(frame%)
                          IF animation% THEN PROCloadnextframe(1,0)
                          esccodes%=0
                        ENDIF

                      WHEN 72,104 : REM H & h - hold graphics
                        first%=(K%=104)
                        PROCWAITNOKEY(-85,0)
                        IF TY%>0 THEN
                          PROCundosave
                          IF TX%<40 AND TX%>-1 AND TY%>0 AND TY%<25 THEN VDU 31,TX%,TY%,scode&(6-first%)
                          PROCframesave(frame%)
                          IF animation% THEN PROCloadnextframe(1,0)
                          esccodes%=0
                        ENDIF

                      WHEN 81,113 : REM Q & q - toggle control codes
                        PROCcontrolcodes(1,7)
                        frame%-=1
                        PROCloadnextframe(1,0)

                      WHEN 83,115 : REM S & s - separated graphics
                        first%=(K%=115)
                        PROCWAITNOKEY(-82,0)
                        IF TY%>0 THEN
                          PROCundosave
                          IF TX%<40 AND TX%>-1 AND TY%>0 AND TY%<25 THEN VDU 31,TX%,TY%,scode&(4-first%)
                          PROCframesave(frame%)
                          IF animation% THEN PROCloadnextframe(1,0)
                          esccodes%=0
                        ENDIF

                    ENDCASE

                  ENDIF
                  PROCframesave(frame%)
                ENDIF
            ENDCASE
          ENDIF

          REM if canvas screen position has moved then update screen and moving sprite if active
          IF spritemoving%>-1 THEN
            IF OWX%<>newWX% OR OWY%<>newWY% OR OMF%<>frame% THEN
              cmWX%=newWX%
              cmWY%=newWY%
              PROCspritemoveinit(0)
            ENDIF
          ENDIF

        WHEN M_keyboard% : REM keyboard screen
          REM type text on keyboard screen
          IF K%>1 THEN

            REM handle specific keypresses
            CASE K% OF
              WHEN 8 : REM backspace
                IF text$<>"" THEN
                  text$=LEFT$(text$,LEN(text$)-1)
                  PROCkeyboardmenu(0)
                ENDIF

              WHEN 13 : REM enter
                IF obj_txtcur%<999 obj_txtcur%+=1
                txtlist$(obj_txtcur%)=text$

              OTHERWISE
                REM ADD VALID CHARS AND INCREASE TEXT POS  AND TY%>0
                IF K%>31 AND K%<127 THEN
                  IF LEN(text$)<30 THEN
                    text$+=CHR$(K%)
                    PROCkeyboardmenu(0)
                  ENDIF
                ENDIF
            ENDCASE
          ENDIF

        WHEN M_sprites% : REM sprite menu

          REM left cursor key
          IF INKEY(-26) THEN
            sprite_cur%-=1
            IF sprite_cur%<0 THEN sprite_cur%=sprite_max%-1
            PROCspritescreen(0)
            sprite_old%=sprite_cur%
            PROCWAITNOKEY(-26,0)
            FONTX%=PX%
          ENDIF

          REM right cursor key
          IF INKEY(-122) THEN
            sprite_cur%+=1
            IF sprite_cur%>sprite_max%-1 THEN sprite_cur%=0
            PROCspritescreen(0)
            sprite_old%=sprite_cur%
            PROCWAITNOKEY(-122,0)
            FONTX%=PX%
          ENDIF

          REM ADD VALID CHARS AND INCREASE TEXT POS
          IF K%>31 AND K%<127 AND TY%>0 THEN
            PROCundosave
            IF fontcur%=0 THEN
              REM VDU 31,TEXTX%,TY%,K%+128
              REM IF TEXTX%<39 THEN TEXTX%+=1
            ELSE
              IF fonts{(K%-32)}.a%<>0 THEN
                REM A$=CHR$(K%)
                PROCdrawfont(FONTX%,PY%,CHR$(K%))
                FONTX%+=fonts{(K%-32)}.w%
              ENDIF
            ENDIF
            PROCspritecommit(sprite_cur%)
          ENDIF

          REM Q & q - toggle control codes
          IF INKEY(-17) PROCspritescreen(1)


        WHEN M_moviemode% : REM movie mode keyboard controls
          IF K%>1 THEN

            OWX%=mmWX%
            OWY%=mmWY%
            newWX%=mmWX%
            newWY%=mmWY%
            OMF%=movieframe%
            oldselect%=spriteselect%

            REM handle specific keypresses
            CASE K% OF
              WHEN 2 : REM escape?

              WHEN 3 : REM ctrl + c

              WHEN 8 : REM backspace

              WHEN 9 : REM tab
                REM select spr object if on screen
                first%=-1
                selectnew%=spriteselect%
                IF obj_lstcount%>-1 THEN
                  FOR L%=0 TO obj_lstcount%
                    IF objlist{(L%)}.type%=1 OR objlist{(L%)}.type%=3 THEN
                      IF objlist{(L%)}.f%=movieframe% OR objlist{(L%)}.f%=-1 THEN
                        X%=objlist{(L%)}.x%
                        Y%=objlist{(L%)}.y%
                        IF objlist{(L%)}.type%=1 THEN
                          SX%=sprlist{(objlist{(L%)}.obj%)}.w%*2
                          SY%=sprlist{(objlist{(L%)}.obj%)}.h%*3
                        ELSE
                          X%=(X% DIV 2)*2
                          Y%=(Y% DIV 3)*3
                          SX%=LEN(txtlist$(objlist{(L%)}.obj%))*2+2
                          SY%=3
                        ENDIF

                        IF X%>mmWX%-SX% AND X%<mmWX%+80 AND Y%<mmWY%+SY%-3 AND Y%>mmWY%-75 THEN
                          IF first%=-1 first%=L%
                          selectnew%=L%
                          IF spriteselect%<L% THEN
                            EXIT FOR
                          ENDIF
                        ENDIF
                      ENDIF
                    ENDIF
                  NEXT
                  IF first%>-1 THEN
                    IF spriteselect%>=selectnew% selectnew%=first%
                    IF spriteselect%<>selectnew% spriteselect%=selectnew%
                    REM PROCmenurestore
                  ELSE
                    spriteselect%=-1
                    spriteselectold%=-1
                  ENDIF
                ENDIF

              WHEN 13 : REM enter
                REM add current world map position to frame list
                IF shift% THEN
                  IF movieframetotal%>-1 THEN
                    movieframetotal%-=1
                    movieframe%=movieframetotal%
                  ENDIF
                ELSE
                  IF movieframetotal%<movieframemax% THEN
                    movieframetotal%+=1
                    movieframe%=movieframetotal%
                    frmlist{(movieframetotal%)}.x%=mmWX%
                    frmlist{(movieframetotal%)}.y%=mmWY%
                    frmlist{(movieframetotal%)}.b%=0
                    frmlist{(movieframetotal%)}.f%=7
                  ENDIF
                ENDIF
                PROCmenudraw
                PROCWAITNOKEY(-74,0)

              WHEN 32 : REM space bar to highlight all visible objects?

              WHEN 68,100 : REM D duplicate current sprite
                IF (spriteselect%>-1 OR obj_lstcount%>-1) AND spritemoving%=-1 THEN
                  IF spriteselect%>-1 obj_lstcur%=spriteselect%
                  spritedupe%=obj_lstcur%
                  spritemoving%=objlist{(obj_lstcur%)}.obj%
                  PROCspritemoveinit(1)
                  PROCWAITNOKEY(-51,0)
                ENDIF

              WHEN 69,101 : REM E current sprite in sprite editor, SHIFT E to clone a new copy and edit
                IF spriteselect%>-1 AND spritemoving%=-1 THEN
                  IF objlist{(obj_lstcur%)}.type%=1 THEN
                    obj_lstcur%=spriteselect%
                    sprite_cur%=objlist{(obj_lstcur%)}.obj%
                    done%=0
                    IF shift% THEN
                      REM find available sprite and copy current sprite to the new sprite to edit
                      FOR S%=0 TO sprite_max%-1
                        F%=0
                        FOR U%=0 TO 319
                          IF sprbuf&(S%,U%)<>32 AND sprbuf&(S%,U%)<>160 F%+=1
                          IF F%>0 EXIT FOR
                        NEXT
                        IF F%=0 EXIT FOR
                      NEXT
                      IF F%=0 THEN
                        FOR U%=0 TO 319
                          sprbuf&(S%,U%)=sprbuf&(sprite_cur%,U%)
                        NEXT
                        sprite_cur%=S%
                        objlist{(obj_lstcur%)}.obj%=S%
                        done%=1
                      ELSE
                        FOR SY%=6 TO 11
                          PROCprint40(SY%,"")
                        NEXT
                        PRINTTAB(2,8)"NO AVAILABLE SPRITES FOUND!"
                        PRINTTAB(2,9)"CANNOT CLONE THIS SPRITE ";tg$;"OK";
                        REPEAT
                          PROCREADMOUSE

                          IF MB%=4 THEN
                            PROCWAITMOUSE(0)
                            IF TY%=9 AND TX%>28 AND TX%<31 THEN done%=2
                          ELSE
                            WAIT 2
                          ENDIF
                        UNTIL done%<>0
                        PROCmenurestore
                      ENDIF
                    ELSE
                      done%=1
                    ENDIF
                    PROCWAITNOKEY(-35,0)
                    IF done%=1 THEN
                      menuext%=M_sprites%
                      PROCspritescreen(1)
                    ENDIF
                  ENDIF
                ENDIF

              WHEN 77,109 : REM M move selected sprite
                IF (spriteselect%>-1 OR obj_lstcount%>-1) AND spritemoving%=-1 THEN
                  IF spriteselect%>-1 obj_lstcur%=spriteselect%
                  spritemoving%=objlist{(obj_lstcur%)}.obj%
                  spriterelocate%=1
                  PROCspritemoveinit(1)
                ENDIF
                PROCWAITNOKEY(-102,0)

              WHEN 80,112 : REM P show selected sprite properties
                IF spritemoving%=-1 THEN
                  REM PRINTTAB(0,21);STR$(spriteselect%);"  ";
                  IF spriteselect%>-1 obj_lstcur%=spriteselect%
                  menuext%=M_sprProperty%
                  PROCsubinit(6)
                ENDIF
                PROCWAITNOKEY(-56,0)

              WHEN 81,113 : REM Q & q - toggle control codes
                IF spritemoving%=-1 THEN
                  PROCcontrolcodes(1,0)
                  PROCmenurestore
                ENDIF

              WHEN 82,114 : REM R repeat current sprite to multiple frames
                IF spritemoving%=-1 THEN
                  IF spriteselect%>-1 obj_lstcur%=spriteselect%
                  IF objlist{(obj_lstcur%)}.type%=3 THEN
                    insertrepeat%=3
                  ELSE
                    insertrepeat%=2
                  ENDIF
                  insertphase%=1
                  spritemoving%=objlist{(obj_lstcur%)}.obj%
                  PROCspriteinsertupdate(1)

                  PROCspritemoveinit(1)
                  PROCWAITNOKEY(-52,0)
                ENDIF


              WHEN 128 : REM left + ctrl

              WHEN 129 : REM left + ctrl

              WHEN  130 : REM home
                REM jump to first frame in frame list
                IF movieframetotal%>-1 THEN
                  movieframe%=0
                  newWX%=frmlist{(movieframe%)}.x%
                  newWY%=frmlist{(movieframe%)}.y%
                  spriteselect%=-1
                ENDIF

              WHEN  131 : REM end
                REM jump to last frame in frame list
                IF movieframetotal%>-1 THEN
                  movieframe%=movieframetotal%
                  newWX%=frmlist{(movieframe%)}.x%
                  newWY%=frmlist{(movieframe%)}.y%
                  spriteselect%=-1
                ENDIF

              WHEN 132,158 : REM pgup
                REM advance to next frame in frame list
                IF movieframetotal%>-1 THEN
                  nf%=1
                  IF ctrl% nf%=10
                  IF shift% nf%=50
                  IF movieframe%+nf%<=movieframetotal% THEN
                    movieframe%+=nf%
                    newWX%=frmlist{(movieframe%)}.x%
                    newWY%=frmlist{(movieframe%)}.y%
                    spriteselect%=-1
                  ENDIF
                ENDIF

              WHEN 133,159 : REM pgdn
                REM move to previous frame in frame list
                IF movieframetotal%>-1 THEN
                  nf%=-1
                  IF ctrl% nf%=-10
                  IF shift% nf%=-50
                  IF movieframe%+nf%>=-1 THEN
                    movieframe%+=nf%
                    IF movieframe%>-1 THEN
                      newWX%=frmlist{(movieframe%)}.x%
                      newWY%=frmlist{(movieframe%)}.y%
                    ENDIF
                    spriteselect%=-1
                  ENDIF
                ENDIF

              WHEN 134 : REM insert
                REM open object insert menu
                IF spritemoving%=-1 THEN
                  menuext%=M_sprSelect%
                  PROCsubinit(5)
                ENDIF
                PROCWAITNOKEY(-62,0)

              WHEN 135 : REM del
                REM insert last selected object directly to world map
                IF spritemoving%=-1 THEN
                  spritemoving%=spriteold%
                  PROCspriteinserthandler(1)
                  PROCspritemoveinit(1)
                ENDIF
                PROCWAITNOKEY(-90,0)

              WHEN 136 : REM left cursor
                IF shift% THEN nf%=80 ELSE nf%=1
                IF mmWX%-nf%>-10000 newWX%-=nf%
              WHEN 137 : REM right cursor
                IF shift% THEN nf%=80 ELSE nf%=1
                IF mmWX%+nf%<10000 newWX%+=nf%
              WHEN 138 : REM down cursor
                IF shift% THEN nf%=72 ELSE nf%=1
                IF mmWY%-nf%>-10000 newWY%-=nf%
              WHEN 139 : REM up cursor
                IF shift% THEN nf%=72 ELSE nf%=1
                IF mmWY%+nf%<10000 newWY%+=nf%
              OTHERWISE

            ENDCASE

            REM if movie map position has moved then update screen and moving sprite if active
            IF OWX%<>newWX% OR OWY%<>newWY% OR OMF%<>movieframe% OR oldselect%<>spriteselect% THEN
              IF spriteselectold%>-1 THEN
                PROCupdateselection(spriteselectold%,0)
                spriteselectold%=-1
              ENDIF
              mmWX%=newWX%
              mmWY%=newWY%

              IF spritemoving%>-1 THEN
                REM PROCobjdraw(PX%,PY%,3,13)
                PROCmenurestore
                PROCspritemoveinit(1)

              ELSE
                REM PROCmenurestore
                PROCobjtoworldmap
              ENDIF
            ENDIF
            REM PRINTTAB(0,20);STR$(movieframe%);"  ("STR$(movieframetotal%);")  ";
          ENDIF

        WHEN M_sprProperty% : REM close properties menu
          IF K%=80 THEN
            PROCmenurestore
            PROCWAITNOKEY(-56,0)
          ENDIF

        WHEN M_sprSelect% : REM close sprite select menu
          IF K%=134 THEN
            PROCmenurestore
            PROCWAITNOKEY(-62,0)
          ENDIF

        OTHERWISE

      ENDCASE

      PROCWAITNOKEY(0,-1)

      ENDPROC

      REM ##########################################################
      REM SPECIAL DIALOG
      DEF PROCkybrfontdhandler
      LOCAL of%

      PROCWAITMOUSE(0)
      CASE TY% OF

        WHEN 3
          of%=fontcur%
          IF TX%=6 AND fontcur%>0 THEN fontcur%-=1

          IF TX%=8 AND fontcur%<fontcount% THEN fontcur%+=1

          REM load specific font file
          IF TX%>20 AND TX%<29 THEN
            PROCloadfile(5)
            menuext%=M_keyboard%
            PROCkeyboardmenu(1)
          ENDIF

          REM reset fonts loaded fonts
          IF TX%>30 AND TX%<39 THEN
            IF fontfound%=0 THEN
              PROCloadfontnames
              fontcur%=0
              PROCkeyboardmenu(1)
            ENDIF
          ENDIF

          IF fontcur%<>of% AND fontcur%>0 AND fontfound%=1 THEN PROCloadfont(fontname$(fontcur%))

        WHEN 12,13,14,15,16,17,18,19 : REM alphabet selector
          IF TY%=18 AND TX%>33 AND TX%<37 THEN
            caps%=(caps%+1) AND 1
          ELSE
            IF TY%=18 AND TX%>28 AND TX%<33 THEN
              text$+=CHR$(32)
            ELSE
              C%=GET(TX%,TY%)
              IF C%<>32 AND C%<127 THEN text$+=CHR$(C%)
            ENDIF
          ENDIF
          text$=LEFT$(text$,30)
          REM toolsel%=T_text&

        WHEN 20 : REM text controls
          CASE TX% OF
            WHEN 37 : REM backspace
              IF text$<>"" THEN text$=LEFT$(text$,LEN(text$)-1)
            WHEN 39 : REM clear text
              text$=""
          ENDCASE

          REM showcodes%=(showcodes%+1) AND 1

        WHEN 22 : REM buttons
          CASE TX% OF
            WHEN 2,3,4,5,6,7,8,9,10,11,12,13 : REM add text to text list for insert menu
              IF obj_txtcur%<999 obj_txtcur%+=1
              txtlist$(obj_txtcur%)=text$

            WHEN 17,18,19,20,21,22,23,24 : REM help screen

              REM PROCchangemode(7,1)
              CLS
              menuext%=M_canvasmode%
              PROCmenudraw
              PROCshowhelp
              menuext%=M_keyboard%
              PROCkeyboardmenu(1)

            WHEN 29,30,31,32,33,34,35,36,37 : REM close screen
              PROCmenurestore

          ENDCASE
        WHEN 24 : REM hidden import control
          IF TX%=0 THEN
            menuext%=M_sprites%
            PROCfontcreate
            menuext%=M_keyboard%
            PROCkeyboardmenu(1)
          ENDIF
      ENDCASE

      IF menuext%=M_keyboard% THEN PROCkeyboardmenu(0)

      ENDPROC

      REM ##########################################################
      REM SPRITE EDITOR DIALOG
      DEF PROCspritehandler
      LOCAL textx%,D$

      REM sprite drawing region
      IF PX%>19 AND PX%<60 AND PY%>8 AND PY%<57 THEN
        CASE toolsel% OF
          WHEN T_paint&: REM PAINT TOOL
            PROCundosave
            PROCpoint(PX%,PY%,1-erase%)
            REPEAT
              PROCREADMOUSE
              IF PX%>19 AND PX%<60 AND PY%>8 AND PY%<57 THEN
                IF PX%<>OLD_PX% OR PY%<>OLD_PY% THEN
                  PROCpoint(PX%,PY%,1-erase%)
                  OLD_PX%=PX%
                  OLD_PY%=PY%
                ENDIF
              ENDIF
            UNTIL MB%=0
            PROCspritecommit(sprite_cur%)
            IF animation% THEN
              sprite_cur%+=1
              IF sprite_cur%>sprite_max%-1 THEN sprite_cur%=0
            ENDIF

          WHEN T_dither1&,T_dither2&,T_dither3&,T_dither4&,T_dither5& : REM dither tools
            PROCundosave
            CASE dither% OF
              WHEN 0,1,2,3
                D%=2^(dither%)
                DA%=2
                IF dither%=2 THEN DA%=4
                IF dither%=3 THEN DA%=8

                X%=(PX% DIV DA%)*DA%
                Y%=(PY% DIV DA%)*DA%

                IF PX%>19 AND PX%<60 AND PY%>8 AND PY%<57 THEN PROCpoint(X%,Y%,1-erase%)
                IF PX%+D%>19 AND PX%+D%<60 AND PY%+D%>8 AND PY%+D%<57 THEN PROCpoint(X%+D%,Y%+D%,1-erase%)
              WHEN 4
                IF TX%>9 AND TX%<30 AND TY%>2 AND TY%<19 THEN
                  char%=255-erase%*95 : REM SOLID BLOCK #255 / #160
                  VDU 31,TX%,TY%,char%
                ENDIF
            ENDCASE
            REPEAT
              PROCREADMOUSE
              IF PX%<>OLD_PX% OR PY%<>OLD_PY% THEN
                CASE dither% OF
                  WHEN 0,1,2,3
                    X%=(PX% DIV DA%)*DA%
                    Y%=(PY% DIV DA%)*DA%

                    IF X%>19 AND X%<60 AND Y%>8 AND Y%<57 THEN PROCpoint(X%,Y%,1-erase%)
                    IF X%+D%>19 AND X%+D%<60 AND Y%+D%>8 AND Y%+D%<57 THEN PROCpoint(X%+D%,Y%+D%,1-erase%)
                  WHEN 4
                    IF TX%>9 AND TX%<30 AND TY%>2 AND TY%<19 THEN
                      char%=255-erase%*95 : REM SOLID BLOCK #255
                      VDU 31,TX%,TY%,char%
                    ENDIF
                ENDCASE
              ENDIF
              OLD_PX%=PX%
              OLD_PY%=PY%
            UNTIL MB%=0

            PROCspritecommit(sprite_cur%)
            IF animation% THEN
              sprite_cur%+=1
              IF sprite_cur%>sprite_max%-1 THEN sprite_cur%=0
            ENDIF


          WHEN T_brush1&,T_brush2&,T_brush3&,T_brush4&,T_brush5&,T_brush6&,T_brush7&
            PROCundosave
            OLD_PX%=-1
            REPEAT
              PROCREADMOUSE
              IF PX%<>OLD_PX% OR PY%<>OLD_PY% THEN
                CASE toolsel% OF
                  WHEN T_brush1& : REM brush 1  \
                    FOR X%=-2 TO 2
                      PROCpoint(PX%+X%,PY%+X%,1-erase%)
                    NEXT
                  WHEN T_brush2& : REM brush 2   /
                    FOR X%=-2 TO 2
                      PROCpoint(PX%+X%,PY%-X%,1-erase%)
                    NEXT

                  WHEN T_brush3& : REM brush 3  -
                    FOR X%=-2 TO 2
                      PROCpoint(PX%+X%,PY%,1-erase%)
                    NEXT

                  WHEN T_brush4& : REM brush 4 |
                    FOR X%=-2 TO 2
                      PROCpoint(PX%,PY%+X%,1-erase%)
                    NEXT

                  WHEN T_brush5& : REM brush 5 * 1
                    FOR X%=-1 TO 1
                      PROCbresenham(PX%-1,PY%+X%,PX%+1,PY%+X%,1-erase%)
                    NEXT

                  WHEN T_brush6& : REM brush 6 * 2
                    FOR X%=-2 TO 2
                      PROCbresenham(PX%-2-(ABS(X%)>1),PY%+X%,PX%+2+(ABS(X%)>1),PY%+X%,1-erase%)
                    NEXT

                  WHEN T_brush7& : REM brush 7 * 3
                    FOR X%=-3 TO 3
                      PROCbresenham(PX%-3-(ABS(X%)>2),PY%+X%,PX%+3+(ABS(X%)>2),PY%+X%,1-erase%)
                    NEXT

                ENDCASE
              ENDIF
              OLD_PX%=PX%
              OLD_PY%=PY%
            UNTIL MB%=0
            PROCspritecommit(sprite_cur%)
            IF animation% THEN
              sprite_cur%+=1
              IF sprite_cur%>sprite_max%-1 THEN sprite_cur%=0
            ENDIF


          WHEN T_line&: REM line tool
            PROCundosave

            startx%=PX%: starty%=PY%
            OLD_PX%=PX% : OLD_PY%=PY%
            PROCpoint(startx%,starty%,2)

            REPEAT
              PROCREADMOUSE
              IF PX%<>OLD_PX% OR PY%<>OLD_PY% THEN
                IF PX%<20 THEN PX%=20
                IF PX%>59 THEN PX%=59
                IF PY%<9 THEN PY%=9
                IF PY%>56 THEN PY%=56
                PROCbresenham(startx%,starty%,OLD_PX%,OLD_PY%,2)
                PROCbresenham(startx%,starty%,PX%,PY%,2)
                OLD_PX%=PX%
                OLD_PY%=PY%
              ENDIF
            UNTIL MB%=0

            PROCspriteredraw
            PROCbresenham(startx%,starty%,PX%,PY%,1-erase%)
            PROCspritecommit(sprite_cur%)
            IF animation% THEN
              sprite_cur%+=1
              IF sprite_cur%>sprite_max%-1 THEN sprite_cur%=0
            ENDIF

          WHEN T_box&: REM rectangle tool
            PROCundosave

            startx%=PX%: starty%=PY%
            OLD_PX%=PX% : OLD_PY%=PY%
            PROCpoint(startx%,starty%,2)

            REPEAT
              PROCREADMOUSE
              IF PX%<20 THEN PX%=20
              IF PX%>59 THEN PX%=59
              IF PY%<9 THEN PY%=9
              IF PY%>56 THEN PY%=56
              IF PX%<>OLD_PX% OR PY%<>OLD_PY% THEN
                PROCrectangle(startx%,starty%,OLD_PX%,OLD_PY%,2,0)
                PROCrectangle(startx%,starty%,PX%,PY%,2,0)
                OLD_PX%=PX%
                OLD_PY%=PY%
              ENDIF
            UNTIL MB%=0

            PROCspriteredraw
            PROCrectangle(startx%,starty%,PX%,PY%,1-erase%,1)
            PROCspritecommit(sprite_cur%)
            IF animation% THEN
              sprite_cur%+=1
              IF sprite_cur%>sprite_max%-1 THEN sprite_cur%=0
            ENDIF

          WHEN T_circle&: REM circle
            PROCundosave

            startx%=PX%: starty%=PY%
            OLD_PX%=PX% : OLD_PY%=PY%

            REPEAT
              PROCREADMOUSE
              IF PX%<>OLD_PX% OR PY%<>OLD_PY% THEN
                PROCcircle(startx%,starty%,startx%-OLD_PX%,2,0)
                PROCcircle(startx%,starty%,startx%-PX%,2,0)
                OLD_PX%=PX%
                OLD_PY%=PY%
              ENDIF
            UNTIL MB%=0

            PROCspriteredraw
            PROCcircle(startx%,starty%,startx%-PX%,1-erase%,1)
            PROCspritecommit(sprite_cur%)
            IF animation% THEN
              sprite_cur%+=1
              IF sprite_cur%>sprite_max%-1 THEN sprite_cur%=0
            ENDIF

          WHEN T_symmetry&: REM symmetry tool
            IF PX%>19 AND PX%<60 AND PY%>8 AND PY%<57 THEN
              PROCundosave
              PROCpoint(PX%,PY%,1-erase%)

              REPEAT
                PROCREADMOUSE
                IF PX%>19 AND PX%<60 AND PY%>8 AND PY%<57 THEN
                  IF PX%<>OLD_PX% OR PY%<>OLD_PY% THEN
                    PROCpoint(PX%,PY%,1-erase%)
                    OLD_PX%=PX%
                    OLD_PY%=PY%
                  ENDIF
                ENDIF
              UNTIL MB%=0
              PROCspritecommit(sprite_cur%)
              IF animation% THEN
                sprite_cur%+=1
                IF sprite_cur%>sprite_max%-1 THEN sprite_cur%=0
              ENDIF

            ENDIF

          WHEN T_flash&,T_double&,T_separate&,T_hold& : REM special control codes
            PROCundosave

            X%=toolsel%-T_flash&
            IF TX%<30 AND TX%>9 AND TY%>2 AND TY%<19 THEN VDU 31,TX%,TY%,scode&(X%*2+erase%)
            REPEAT
              PROCREADMOUSE
              IF TX%<>OLD_TX% OR TY%<>OLD_TY% THEN
                IF TX%<30 AND TX%>9 AND TY%>2 AND TY%<19 THEN VDU 31,TX%,TY%,scode&(X%*2+erase%)
              ENDIF
              OLD_TX%=TX%
              OLD_TY%=TY%
            UNTIL MB%=0
            PROCspritecommit(sprite_cur%)

          WHEN T_text&: REM text print tool
            PROCWAITMOUSE(0)
            IF LENtext$>0 THEN
              PROCundosave

              FOR textx%=0 TO LENtext$-1
                IF TX%+textx%<30 AND TX%+textx%>9 AND TY%>2 AND TY%<19 THEN
                  PRINTTAB(TX%+textx%,TY%)MID$(text$,textx%+1,1);
                ENDIF
              NEXT
              PROCspritecommit(sprite_cur%)
            ENDIF


          WHEN T_backg& : REM background colour - char mode only

            IF TX%>9 AND TX%<30 AND TY%>2 AND TY%<19 AND sprlist{(sprite_cur%)}.m%=1 THEN
              PROCundosave

              IF colmode%=1 THEN
                PROCWAITMOUSE(4)
                PROCWAITMOUSE(0)

                FOR Y%=3 TO 18
                  IF erase% THEN
                    VDU 31,TX%,Y%,156
                  ELSE
                    VDU 31,TX%,Y%,(curcol%+144),157
                  ENDIF
                NEXT
              ELSE
                IF erase% THEN
                  VDU 31,TX%,TY%,156
                ELSE
                  IF TX%<29 THEN VDU 31,TX%,TY%,(curcol%+144),157
                ENDIF

                REPEAT
                  PROCREADMOUSE
                  IF TX%<>OLD_TX% OR TY%<>OLD_TY% THEN
                    IF TX%>9 AND TX%<30 AND TY%>2 AND TY%<19 THEN
                      IF erase% THEN
                        VDU 31,TX%,TY%,156
                      ELSE
                        IF TX%<29 THEN VDU 31,TX%,TY%,(curcol%+144),157
                      ENDIF
                    ENDIF
                  ENDIF
                  OLD_TX%=TX%
                  OLD_TY%=TY%
                UNTIL MB%=0
              ENDIF
              PROCspritecommit(sprite_cur%)
            ENDIF
          WHEN T_foreg&: REM foreground colour
            IF TX%>9 AND TX%<30 AND TY%>2 AND TY%<19 THEN
              PROCundosave

              IF colmode%=1 AND sprlist{(sprite_cur%)}.m%=1 THEN
                PROCWAITMOUSE(4)
                PROCWAITMOUSE(0)

                FOR Y%=3 TO 18
                  VDU 31,TX%,Y%,(curcol%+144-textfore%*16)
                NEXT
              ELSE
                REM pix mode sets same colour for entire sprite, char mode can have multicolour
                IF sprlist{(sprite_cur%)}.m%=0 THEN
                  FOR Y%=3 TO sprlist{(sprite_cur%)}.h%+3
                    VDU 31,10,Y%,(curcol%+144-textfore%*16)
                  NEXT
                  PROCWAITMOUSE(0)
                ELSE
                  VDU 31,TX%,TY%,(curcol%+144-textfore%*16)
                  REPEAT
                    PROCREADMOUSE
                    IF TX%<>OLD_TX% OR TY%<>OLD_TY% THEN
                      IF TX%>9 AND TX%<30 AND TY%>2 AND TY%<19 THEN VDU 31,TX%,TY%,(curcol%+144-textfore%*16)
                      OLD_TX%=TX%
                      OLD_TY%=TY%
                    ENDIF
                  UNTIL MB%=0
                ENDIF
              ENDIF
              PROCspritecommit(sprite_cur%)
            ENDIF
        ENDCASE
      ELSE
        REM 20x16 chars @320 bytes : 40x48 pixels @1920 bytes
        PROCWAITMOUSE(0)

        REM process sprite buttons
        CASE TY% OF
          WHEN 1 : REM change sprite name
            F$=sprname$(sprite_cur%)
            NF$=F$
            REPEAT
              K%=INKEY(0)
              IF K%=-1 THEN
                WAIT 10
                VDU 31,11+LEN(NF$),1
              ELSE
                REM valid chars
                IF K%>31 AND K%<127 AND LEN(NF$)<18 THEN
                  NF$=NF$+CHR$(K%)
                ENDIF
                REM check if delete or backspace is pressed
                IF K%=135 OR K%=8 THEN
                  IF LEN(NF$)>1 THEN
                    NF$=LEFT$(NF$,LEN(NF$)-1)
                  ELSE
                    NF$=""
                  ENDIF
                ENDIF
                PRINTTAB(11,1)NF$;SPC(18-LEN(NF$))
              ENDIF
            UNTIL K%=13 OR K%=27
            IF K%=13 sprname$(sprite_cur%)=NF$
            PRINTTAB(11,1)sprname$(sprite_cur%);SPC(18-LEN(sprname$(sprite_cur%)))

          WHEN 2
            CASE TX% OF
              WHEN 1,2,3,4,5 : REM load sprite
                PROCloadfile(3)
                menuext%=M_sprites%
                PROCspritescreen(1)

              WHEN 33,34,35 : REM cls
                PROCundosave

                FOR S%=0 TO 319
                  sprbuf&(sprite_cur%,S%)=32
                NEXT
                PROCspriteredraw
                PROCupdatespritesize(sprite_cur%,1)

              WHEN 37,38,39 : REM scroll mode
                spr_scroll%=(spr_scroll%+1) AND 1
                PROCspritescreen(0)

            ENDCASE
          WHEN 4
            CASE TX% OF
              WHEN 1,2,3,4,5 : REM save sprite
                IF session%=0 THEN
                  D$=FNgetdate
                  cursavedir$= "M7_"+LEFT$(D$,LEN(D$)-2)
                  session%=1
                ENDIF
                PROCcreatefolder(saverootdir$+cursavedir$)
                cursave$=saverootdir$+cursavedir$+"/"
                OSCLI "CD """+saverootdir$+cursavedir$+""""

                PROCsavespritefile(cursave$+"SPRITEDATA",1,0)

              WHEN 33,34,35,36,37 : REM scroll left
                PROCundosave

                IF spr_scroll%=1 THEN
                  REM char mode
                  FOR S%=0 TO 15
                    spr_tmp&(S%)=sprbuf&(sprite_cur%,S%*20)
                  NEXT
                  FOR S%=0 TO 318
                    sprbuf&(sprite_cur%,S%)=sprbuf&(sprite_cur%,S%+1)
                  NEXT
                  FOR S%=0 TO 15
                    sprbuf&(sprite_cur%,S%*20+19)=spr_tmp&(S%)
                  NEXT
                  PROCspriteredraw
                  PROCupdatespritesize(sprite_cur%,1)
                ELSE
                  REM pixel mode

                  FOR Y%=0 TO 47
                    FOR X%=0 TO 39
                      CASE X% OF
                        WHEN 0 : REM SAVE FIRST COL
                          spr_tmp&(0)=FNpoint(20,(47-Y%)+9)
                          PROCpoint(20+X%,(47-Y%)+9,FNpoint(21+X%,(47-Y%)+9))
                        WHEN 39 : REM PLOT LAST COL
                          PROCpoint(20+X%,(47-Y%)+9,spr_tmp&(0))
                        OTHERWISE : REM SHIFT PIXELS LEFT
                          PROCpoint(20+X%,(47-Y%)+9,FNpoint(21+X%,(47-Y%)+9))
                      ENDCASE
                    NEXT
                  NEXT
                  PROCspritecommit(sprite_cur%)
                ENDIF

            ENDCASE

          WHEN 6
            CASE TX% OF
              WHEN 1,2,3,4,5 : REM import sprite from bmp
                PROCimportsprite(2,0)
                menuext%=M_sprites%
                PROCspritescreen(1)

              WHEN 33,34,35,36,37 : REM SCROLL RIGHT
                PROCundosave

                IF spr_scroll%=1 THEN
                  REM CHAR MODE
                  FOR S%=0 TO 15
                    spr_tmp&(S%)=sprbuf&(sprite_cur%,S%*20+19)
                  NEXT
                  FOR S%=319 TO 1 STEP -1
                    sprbuf&(sprite_cur%,S%)=sprbuf&(sprite_cur%,S%-1)
                  NEXT
                  FOR S%=0 TO 15
                    sprbuf&(sprite_cur%,S%*20)=spr_tmp&(S%)
                  NEXT
                  PROCspriteredraw
                  PROCupdatespritesize(sprite_cur%,1)

                ELSE
                  REM PIX MODE

                  FOR Y%=0 TO 47
                    FOR X%=39 TO 0 STEP -1
                      CASE X% OF
                        WHEN 0 : REM PLOT FIRST COL
                          PROCpoint(20,(47-Y%)+9,spr_tmp&(0))
                        WHEN 39 : REM SAVE LAST COL
                          spr_tmp&(0)=FNpoint(20+X%,(47-Y%)+9)
                          PROCpoint(20+X%,(47-Y%)+9,FNpoint(19+X%,(47-Y%)+9))

                        OTHERWISE : REM SHIFT PIXELS RIGHT
                          PROCpoint(20+X%,(47-Y%)+9,FNpoint(19+X%,(47-Y%)+9))
                      ENDCASE
                    NEXT
                  NEXT
                  PROCspritecommit(sprite_cur%)

                ENDIF
            ENDCASE

          WHEN 8
            CASE TX% OF
              WHEN 1,2,3,4,5 : REM animate screen
                PROCanimscreen
                PROCmenudraw
                IF menuext%=77 THEN
                  PROCmenurestore
                ELSE
                  PROCspritescreen(1)
                  PROCspriteredraw
                  PROCupdatespritesize(sprite_cur%,1)
                ENDIF

              WHEN 33,34,35,36,37 : REM SCROLL UP
                PROCundosave

                IF spr_scroll%=1 THEN
                  REM CHAR MODE

                  FOR S%=0 TO 19
                    spr_tmp&(S%)=sprbuf&(sprite_cur%,S%)
                  NEXT
                  FOR S%=20 TO 319
                    sprbuf&(sprite_cur%,S%-20)=sprbuf&(sprite_cur%,S%)
                  NEXT
                  FOR S%=0 TO 19
                    sprbuf&(sprite_cur%,300+S%)=spr_tmp&(S%)
                  NEXT
                  PROCspriteredraw
                  PROCupdatespritesize(sprite_cur%,1)
                ELSE
                  REM PIX MODE

                  FOR X%=0 TO 39
                    FOR Y%=47 TO 0 STEP -1
                      CASE Y% OF
                        WHEN 0 : REM PLOT FIRST ROW
                          PROCpoint(20+X%,(47-Y%)+9,spr_tmp&(0))

                        WHEN 47 : REM SAVE LAST ROW
                          spr_tmp&(0)=FNpoint(20+X%,(47-Y%)+9)
                          PROCpoint(20+X%,(47-Y%)+9,FNpoint(20+X%,(47-Y%)+10))

                        OTHERWISE : REM SHIFT PIXELS UP
                          PROCpoint(20+X%,(47-Y%)+9,FNpoint(20+X%,(47-Y%)+10))
                      ENDCASE
                    NEXT
                  NEXT
                  PROCspritecommit(sprite_cur%)

                ENDIF
            ENDCASE

          WHEN 10
            CASE TX% OF
              WHEN 1,2,3,4,5 : REM copy sprite to next
                dst%=sprite_cur%+1
                IF dst%>sprite_max%-1 THEN dst%=0
                FOR S%=0 TO 319
                  sprbuf&(dst%,S%)=sprbuf&(sprite_cur%,S%)
                NEXT

              WHEN 33,34,35,36,37 : REM scroll down
                PROCundosave

                IF spr_scroll%=1 THEN
                  FOR S%=0 TO 19
                    spr_tmp&(S%)=sprbuf&(sprite_cur%,300+S%)
                  NEXT
                  FOR S%=319 TO 20 STEP -1
                    sprbuf&(sprite_cur%,S%)=sprbuf&(sprite_cur%,S%-20)
                  NEXT
                  FOR S%=0 TO 19
                    sprbuf&(sprite_cur%,S%)=spr_tmp&(S%)
                  NEXT
                  PROCspriteredraw
                  PROCupdatespritesize(sprite_cur%,1)
                ELSE
                  REM PIX MODE

                  FOR X%=0 TO 39
                    FOR Y%=0 TO 47
                      CASE Y% OF
                        WHEN 0 : REM SAVE LAST ROW
                          spr_tmp&(0)=FNpoint(20+X%,(47-Y%)+9)
                          PROCpoint(20+X%,(47-Y%)+9,FNpoint(20+X%,(47-Y%)+8))

                        WHEN 47: REM PLOT FIRST ROW
                          PROCpoint(20+X%,(47-Y%)+9,spr_tmp&(0))

                        OTHERWISE : REM SHIFT PIXELS DOWN
                          PROCpoint(20+X%,(47-Y%)+9,FNpoint(20+X%,(47-Y%)+8))
                      ENDCASE
                    NEXT
                  NEXT
                  PROCspritecommit(sprite_cur%)

                ENDIF

            ENDCASE

          WHEN 12
            CASE TX% OF
              WHEN 1,2,3,4,5 : REM copy sprite prev
                dst%=sprite_cur%-1
                IF dst%<0 THEN dst%=sprite_max%-1
                FOR S%=0 TO 319
                  sprbuf&(dst%,S%)=sprbuf&(sprite_cur%,S%)
                NEXT

              WHEN 33,34,35,36,37 : REM flip horizontal
                PROCundosave

                FOR X%=0 TO 39
                  FOR Y%=0 TO 47
                    spr_tmp&(Y%*40+X%)=FNpoint((39-X%)+20,Y%+9)
                  NEXT
                NEXT
                FOR S%=0 TO 1919
                  PROCpoint(S% MOD 40+20,S% DIV 40+9,spr_tmp&(S%))
                NEXT
                PROCspritecommit(sprite_cur%)

            ENDCASE

          WHEN 14
            CASE TX% OF
              WHEN 1,2,3,4,5 : REM copy sprite to clipboad
                FOR S%=0 TO 319
                  copy_buffer&(S%)=sprbuf&(sprite_cur%,(S% MOD 16)*20+(S% DIV 16))
                NEXT
                copyx%=19
                copyy%=15
                copysize%=S%
                REM toolsel%=T_paste&:toolcursor%=17:copypaste%=1
                REM PROCmenurestore

              WHEN 33,34,35,36,37 : REM flip vertical
                PROCundosave

                FOR X%=0 TO 39
                  FOR Y%=0 TO 47
                    spr_tmp&(Y%*40+X%)=FNpoint(X%+20,(47-Y%)+9)
                  NEXT
                NEXT
                FOR S%=0 TO 1919
                  PROCpoint(S% MOD 40+20,S% DIV 40+9,spr_tmp&(S%))
                NEXT
                PROCspritecommit(sprite_cur%)

            ENDCASE

          WHEN 16
            CASE TX% OF
              WHEN 1,2,3,4,5 : REM paste clip board to sprite
                PROCundosave

                FOR S%=0 TO 319
                  sprbuf&(sprite_cur%,(S% MOD 16)*20+(S% DIV 16))=copy_buffer&(S%)
                NEXT
                PROCspriteredraw
                PROCupdatespritesize(sprite_cur%,1)

              WHEN 33,34,35,36,37 : REM rotate sprite right
                REM *** FIX ***

            ENDCASE

          WHEN 18
            CASE TX% OF
              WHEN 1,2,3,4,5 : REM sprite transparency toggle
                spr_trns%=(spr_trns%+1) MOD 2
                PROCspritescreen(0)

              WHEN 33,34,35,36,37 : REM rotate sprite left
                REM *** FIX ***
            ENDCASE

          WHEN 20 : REM prev / next sprite
            CASE TX% OF
              WHEN 1,2,3,4,5 : REM import small sprite from gif
                PROCimportsprite(7,0)
                menuext%=M_sprites%
                PROCspritescreen(1)

              WHEN 11,12,13,14
                sprite_cur%-=1
                IF sprite_cur%<0 THEN sprite_cur%=sprite_max%-1
              WHEN 17,18,19,20
                sprite_cur%+=1
                IF sprite_cur%>sprite_max%-1 THEN sprite_cur%=0

            ENDCASE
            PROCmenudraw

          WHEN 21 : REM sprite size
            CASE TX% OF
              WHEN 1,2,3,4,5,6,7,8,9,10,11,12 : REM ???

              WHEN 15,16,17,18,19,20,21,22,23,24,25 : REM ???

            ENDCASE
          WHEN 22 : REM sprite mode
            CASE TX% OF
              WHEN 1,2,3,4,5 : REM import large sprite from gif
                PROCimportsprite(7,1)
                menuext%=M_sprites%
                PROCspritescreen(1)

              WHEN 23,24,25,26,27,28,29,30,31
                IF sprlist{(sprite_cur%)}.m%=0 THEN
                  sprlist{(sprite_cur%)}.m%=1
                  IF sprbuf&(sprite_cur%,0)>144 AND sprbuf&(sprite_cur%,0)<152 THEN
                    FOR Y%=0 TO 15
                      sprbuf&(sprite_cur%,Y%*20)=32
                    NEXT
                  ENDIF

                ELSE
                  sprlist{(sprite_cur%)}.m%=0
                ENDIF
                PROCspritescreen(0)
            ENDCASE
          WHEN 24 : REM close button
            CASE TX% OF
              WHEN 2,3,4,5,6,7,8,9,10 : REM movie mode screen
                menuext%=M_moviemode%
                menufrom%=M_moviemode%
                PROCmenurestore

              WHEN 15,16,17,18,19,20,21,22,23,24 : REM canvas mode screen
                menuext%=M_canvasmode%
                menufrom%=M_canvasmode%
                PROCmenurestore

              WHEN 29,30,31,32,33,34,35,36,37 : REM close sprites screen
                menufrom%=menumode%
                PROCmenurestore

            ENDCASE

        ENDCASE
      ENDIF

      REM keyboard change
      IF sprite_cur%<>sprite_old% THEN
        PROCspritescreen(0)
        sprite_old%=sprite_cur%
      ENDIF

      ENDPROC

      REM ##########################################################
      REM initialise menu and button controls
      DEF PROCresetcontrols
      LOCAL L%
      FOR L%=0 TO controls%
        controlrange{(L%)}.x1%=-1
      NEXT
      ENDPROC

      REM ##########################################################
      REM find control at current mouse position
      DEF FNgetcontrol
      LOCAL L%,C%
      C%=-1
      FOR L%=0 TO controls%
        IF controlrange{(L%)}.x1%>-1 THEN
          IF MX%>controlrange{(L%)}.x1% AND MX%<controlrange{(L%)}.x2% AND MY%>controlrange{(L%)}.y1% AND MY%<controlrange{(L%)}.y2% THEN
            C%=L%
            EXIT FOR
          ENDIF
        ENDIF
      NEXT
      =C%


      REM ##########################################################
      REM initialise shape and special sub menu
      DEF PROCsubinit(S%)

      sub_cur%=S%
      OSCLI "SCREENSAVE """+@tmp$+"M7_TMP.BMP"" 0,0,1280,1000"
      PROCchangemode(6,0)

      GCOL 0,0
      RECTANGLE FILL subm{(sub_cur%)}.x%,subm{(sub_cur%)}.y%,subm{(sub_cur%)}.w%,subm{(sub_cur%)}.h%

      GCOL 0,15
      RECTANGLE subm{(sub_cur%)}.x%+8,subm{(sub_cur%)}.y%+8,subm{(sub_cur%)}.w%-16,subm{(sub_cur%)}.h%-16
      RECTANGLE subm{(sub_cur%)}.x%+10,subm{(sub_cur%)}.y%+10,subm{(sub_cur%)}.w%-20,subm{(sub_cur%)}.h%-20

      spritechange%=-1
      PROCresetcontrols
      PROCsubupdate(-1)
      REM PROCsubupdate(0)

      ENDPROC

      REM ##########################################################
      REM shape and special sub menu updater
      DEF PROCsubupdate(redraw%)
      LOCAL SX%,SY%,SW%,SH%,MY%,I%,P%,L%,COL%,F$,T$

      SX%=subm{(sub_cur%)}.x%
      SY%=subm{(sub_cur%)}.y%
      SW%=subm{(sub_cur%)}.w%
      SH%=subm{(sub_cur%)}.h%

      menuYadd%=996

      REM draw static menu items if redraw%=-1
      CASE sub_cur% OF
        WHEN 0 : REM paint
          IF redraw%=-1 THEN
            PROCgtext("P",SX%+32,menuYadd%,10,0,-64)
            PROCmenutext(0,"PAINT        ",SX%+20,menuYadd%,14,(toolsel%=T_paint&)*-4,-48)
            PROCmenutext(1,"LINE         ",SX%+20,menuYadd%,14,(toolsel%=T_line&)*-4,-48)
            PROCmenutext(2,"BOX          ",SX%+20,menuYadd%,14,(toolsel%=T_box&)*-4,-48)
            PROCmenutext(3,"CIRCLE       ",SX%+20,menuYadd%,14,(toolsel%=T_circle&)*-4,-48)
            PROCmenutext(4,"SYMMETRY     ",SX%+20,menuYadd%,14,(toolsel%=T_symmetry&)*-4,-48)
            PROCmenutext(5,"TEXT         ",SX%+20,menuYadd%,14,(toolsel%=T_text&)*-4,-48)
            PROCmenuline(SX%+20,menuYadd%,SW%-40,8,-24)
            PROCmenutext(6,"ANIM8 LINES  ",SX%+20,menuYadd%,11,0,-48)
            PROCgtext("GAP",SX%+20,menuYadd%,14,0,0)
            PROCmenutext(7," - ",SX%+128,menuYadd%,14,4,0)
            PROCmenutext(8," + ",SX%+332,menuYadd%,14,4,-48)
            PROCgtext("LEN",SX%+20,menuYadd%,14,0,0)
            PROCmenutext(9," - ",SX%+128,menuYadd%,14,4,0)
            PROCmenutext(10," + ",SX%+332,menuYadd%,14,4,-48)
            PROCmenuline(SX%+20,menuYadd%,SW%-40,8,-24)
            PROCgtext("SHAPE OPTIONS",SX%+20,menuYadd%,8,0,-48)
            PROCmenutext(11,"OUTLINE      ",SX%+20,menuYadd%,11,0,-48)
            PROCmenutext(12,"FILLED       ",SX%+20,menuYadd%,11,0,-48)
            PROCmenutext(13,"EMPTY        ",SX%+20,menuYadd%,11,0,-48)
            PROCmenuline(SX%+20,menuYadd%,SW%-40,8,-24)
            PROCgtext("FONT:",SX%+20,menuYadd%,14,0,0)
            PROCmenutext(14," < ",SX%+192,menuYadd%,14,4,0)
            PROCmenutext(15," > ",SX%+332,menuYadd%,14,4,-96)
            PROCmenuline(SX%+20,menuYadd%,SW%-40,8,-24)
            PROCmenutext(16,"KYBRD FONTS  ",SX%+20,menuYadd%,10,0,-48)

          ENDIF

          REM redraw dynamic values
          menuYadd%=620
          PROCgtext(CHR$(78+11*animateshape%),SX%+404,menuYadd%,9+animateshape%,0,-48)
          PROCgtext(RIGHT$("0"+STR$(animategap%),2),SX%+244,menuYadd%,11,8,-48)
          PROCgtext(RIGHT$("0"+STR$(animatelen%),2),SX%+244,menuYadd%,11,8,-120)
          PROCgtext(CHR$(78-11*(shapetype%=0)),SX%+404,menuYadd%,9-(shapetype%=0),0,-48)
          PROCgtext(CHR$(78-11*(shapetype%=1)),SX%+404,menuYadd%,9-(shapetype%=1),0,-48)
          PROCgtext(CHR$(78-11*(shapetype%=2)),SX%+404,menuYadd%,9-(shapetype%=2),0,-120)
          PROCgtext(LEFT$(fontname$(fontcur%)+"         ",10),SX%+20,menuYadd%,13,0,432)

        WHEN 1 : REM dither
          IF redraw%=-1 THEN
            PROCgtext("B",SX%+32,menuYadd%,10,0,-64)
            PROCmenutext(0,"DITHER 1     ",SX%+20,menuYadd%,14,(toolsel%=T_dither1&)*-4,-48)
            PROCmenutext(1,"DITHER 2     ",SX%+20,menuYadd%,14,(toolsel%=T_dither2&)*-4,-48)
            PROCmenutext(2,"DITHER 3     ",SX%+20,menuYadd%,14,(toolsel%=T_dither3&)*-4,-48)
            PROCmenutext(3,"DITHER 4     ",SX%+20,menuYadd%,14,(toolsel%=T_dither4&)*-4,-48)
            PROCmenutext(4,"SOLID BLOCK  ",SX%+20,menuYadd%,14,(toolsel%=T_dither5&)*-4,-48)
            PROCmenuline(SX%+20,menuYadd%,SW%-40,8,-24)
            PROCmenutext(5,"BRUSH       ",SX%+20,menuYadd%,14,(toolsel%=T_brush1&)*-4,-48)
            GCOL 0,14
            LINE SX%+310,menuYadd%+42,SX%+330,menuYadd%+22
            LINE SX%+310,menuYadd%+44,SX%+332,menuYadd%+22
            LINE SX%+312,menuYadd%+44,SX%+332,menuYadd%+24
            PROCmenutext(6,"BRUSH    /  ",SX%+20,menuYadd%,14,(toolsel%=T_brush2&)*-4,-48)
            PROCmenutext(7,"BRUSH    -  ",SX%+20,menuYadd%,14,(toolsel%=T_brush3&)*-4,-48)
            PROCmenutext(8,"BRUSH       ",SX%+20,menuYadd%,14,(toolsel%=T_brush4&)*-4,-48)
            GCOL 0,14
            RECTANGLE FILL SX%+328,menuYadd%+22,2,24
            PROCmenutext(9,"BRUSH   * 1 ",SX%+20,menuYadd%,14,(toolsel%=T_brush5&)*-4,-48)
            PROCmenutext(10,"BRUSH   * 2 ",SX%+20,menuYadd%,14,(toolsel%=T_brush6&)*-4,-48)
            PROCmenutext(11,"BRUSH   * 3 ",SX%+20,menuYadd%,14,(toolsel%=T_brush7&)*-4,-48)
          ENDIF

        WHEN 2 : REM copy paste
          IF redraw%=-1 THEN
            PROCgtext("C",SX%+32,menuYadd%,10,0,-64)
            PROCmenutext(0,"COPY/SELECT  ",SX%+20,menuYadd%,14,(toolsel%=T_copy&)*-4,-48)
            PROCmenutext(1,"PASTE        ",SX%+20,menuYadd%,14,(toolsel%=T_paste&)*-4,-48)
            PROCmenuline(SX%+20,menuYadd%,SW%-40,8,-24)
            PROCmenutext(2,"PASTE TO ALL ",SX%+20,menuYadd%,10,0,-48)
            PROCmenutext(3,"CPY CODES ALL",SX%+20,menuYadd%,10,0,-48)
            PROCmenutext(4,"COPY FRAME > ",SX%+20,menuYadd%,10,0,-48)
            PROCmenutext(5,"COPY FRAME < ",SX%+20,menuYadd%,10,0,-48)
            PROCmenutext(6,"MIRROR SEL   ",SX%+20,menuYadd%,10,0,-48)
            PROCmenutext(7,"MIRROR LEFT  ",SX%+20,menuYadd%,13,0,-48)
            PROCmenutext(8,"REFLECT SEL  ",SX%+20,menuYadd%,10,0,-48)
            PROCmenutext(9,"REFLECT TOP  ",SX%+20,menuYadd%,13,0,-48)
            PROCmenutext(10,"FLIP HORZ    ",SX%+20,menuYadd%,10,0,-48)
            PROCmenutext(11,"FLIP VERT    ",SX%+20,menuYadd%,10,0,-48)
            PROCmenutext(12,"NEGATIVE     ",SX%+20,menuYadd%,10,0,-48)
            PROCmenutext(13,"ERASE        ",SX%+20,menuYadd%,10,0,-48)
            PROCmenuline(SX%+20,menuYadd%,SW%-40,8,-24)
            PROCmenutext(14,"PASTE FIX X  ",SX%+20,menuYadd%,11,0,-48)
            PROCmenutext(15,"PASTE FIX Y  ",SX%+20,menuYadd%,11,0,-48)
            PROCmenutext(16,"TRANSPARENT  ",SX%+20,menuYadd%,11,0,-48)
          ENDIF

          REM redraw dynamic values
          menuYadd%=212
          PROCgtext(CHR$(78+11*copylockxt%),SX%+404,menuYadd%,9+copylockxt%,0,-48)
          PROCgtext(CHR$(78+11*copylockyt%),SX%+404,menuYadd%,9+copylockyt%,0,-48)
          PROCgtext(CHR$(78+11*copy_trns%),SX%+404,menuYadd%,9+copy_trns%,0,0)


        WHEN 3 : REM fill
          IF redraw%=-1 THEN
            PROCgtext("F",SX%+32,menuYadd%,10,0,-64)
            PROCmenutext(0,"FILL SOLID   ",SX%+20,menuYadd%,14,(toolsel%=T_fill&)*-4,-48)
            PROCmenutext(1,"GRAD LEFT    ",SX%+20,menuYadd%,14,(toolsel%=T_gradl&)*-4,-48)
            PROCmenutext(2,"GRAD RIGHT   ",SX%+20,menuYadd%,14,(toolsel%=T_gradr&)*-4,-48)
            PROCmenutext(3,"GRAD TOP     ",SX%+20,menuYadd%,14,(toolsel%=T_gradt&)*-4,-48)
            PROCmenutext(4,"GRAD BOTTOM  ",SX%+20,menuYadd%,14,(toolsel%=T_gradb&)*-4,-48)
            PROCmenutext(5,"GRAD TOP-L   ",SX%+20,menuYadd%,14,(toolsel%=T_gradtl&)*-4,-48)
            PROCmenutext(6,"GRAD TOP-R   ",SX%+20,menuYadd%,14,(toolsel%=T_gradtr&)*-4,-48)
            PROCmenutext(7,"GRAD BOT-R   ",SX%+20,menuYadd%,14,(toolsel%=T_gradbr&)*-4,-48)
            PROCmenutext(8,"GRAD BOT-L   ",SX%+20,menuYadd%,14,(toolsel%=T_gradbl&)*-4,-48)
            PROCmenuline(SX%+20,menuYadd%,SW%-40,8,-24)

            REM gradient fill options
            PROCaddcontrange(9,SX%+40,menuYadd%-50,SX%+440,menuYadd%)
            GCOL 0,9
            RECTANGLE FILL SX%+40,menuYadd%,284,-50
            GCOL 0,11
            menuYadd%-=60

            FOR P%=0 TO 35
              FOR L%=0 TO 12
                IF pat%(P% DIV 2,P% MOD 4+(L% MOD 4)*4)=1 THEN
                  RECTANGLE FILL SX%+40+P%*8,menuYadd%-L%*4+58,6,2
                ENDIF
              NEXT
            NEXT

            PROCaddcontrange(10,SX%+40,menuYadd%-50,SX%+440,menuYadd%)
            GCOL 0,11
            RECTANGLE FILL SX%+40,menuYadd%,284,-50
            GCOL 0,12

            menuYadd%-=60
            I%=0
            FOR P%=0 TO 35
              CASE P% OF
                WHEN 10,12,14,16,18
                  I%+=3
                  IF I%>17 THEN I%=17
                WHEN 20,22,24,26,28
                  I%-=3
                  IF I%<0 THEN I%=0
              ENDCASE

              FOR L%=0 TO 12
                IF pat%(I%,P% MOD 4+(L% MOD 4)*4)=1 THEN
                  RECTANGLE FILL SX%+40+P%*8,menuYadd%-L%*4+58,6,2
                ENDIF
              NEXT
            NEXT

            PROCaddcontrange(11,SX%+40,menuYadd%-50,SX%+440,menuYadd%)
            GCOL 0,12
            RECTANGLE FILL SX%+40,menuYadd%,284,-50
            GCOL 0,14
            menuYadd%-=60

            I%=0
            FOR P%=0 TO 35
              IF P%<18 AND P%>0 THEN
                I%+=1
                IF I%>17 THEN I%=17
              ELSE
                I%-=1
                IF I%<0 THEN I%=0

              ENDIF

              FOR L%=0 TO 12
                IF pat%(I%,P% MOD 4+(L% MOD 4)*4) THEN
                  RECTANGLE FILL SX%+40+P%*8,menuYadd%-L%*4+58,6,2
                ENDIF
              NEXT
            NEXT

            PROCaddcontrange(12,SX%+40,menuYadd%-50,SX%+440,menuYadd%)
            GCOL 0,13
            RECTANGLE FILL SX%+40,menuYadd%,284,-50
            GCOL 0,11

            menuYadd%-=60
            I%=0
            FOR P%=0 TO 35
              CASE P% OF
                WHEN 20,21,22,23,24,25,26
                  I%+=2
                  IF I%>17 THEN I%=17
                WHEN 28,29,30,31,32,33,34
                  I%-=2
                  IF I%<0 THEN I%=0
              ENDCASE

              FOR L%=0 TO 12
                IF pat%(I%,P% MOD 4+(L% MOD 4)*4)=1 THEN
                  RECTANGLE FILL SX%+40+P%*8,menuYadd%-L%*4+58,6,2
                ENDIF
              NEXT
            NEXT
          ENDIF

          REM redraw dynamic values
          menuYadd%=468
          FOR I%=0 TO 3
            PROCgtext("O",SX%+404,menuYadd%,12+(gradtype%=I%)*2,0,-60)
          NEXT

        WHEN 4 : REM special
          IF redraw%=-1 THEN
            PROCgtext("S",SX%+32,menuYadd%,10,0,-64)
            PROCmenutext(0,"FLSH (136)   ",SX%+20,menuYadd%,14,(toolsel%=T_flash&)*-4,-48)
            PROCmenutext(1,"DBLH (141)   ",SX%+20,menuYadd%,14,(toolsel%=T_double&)*-4,-48)
            PROCmenutext(2,"SEPR (154)   ",SX%+20,menuYadd%,14,(toolsel%=T_separate&)*-4,-48)
            PROCmenutext(3,"HOLD (158)   ",SX%+20,menuYadd%,14,(toolsel%=T_hold&)*-4,-48)
            PROCmenuline(SX%+20,menuYadd%,SW%-40,8,-24)
            PROCmenutext(4,"SHOW GRID    ",SX%+20,menuYadd%,11,0,-48)
            PROCmenutext(5,"COLUMN MODE  ",SX%+20,menuYadd%,11,0,-48)
            PROCmenuline(SX%+20,menuYadd%,SW%-40,8,-24)
            PROCmenutext(6,"SPRITES      ",SX%+20,menuYadd%,10,0,-48)
            PROCmenutext(7,"MOVIE MODE   ",SX%+20,menuYadd%,10,0,-48)
            PROCmenutext(8,"EDIT.TF      ",SX%+20,menuYadd%,10,0,-48)
            PROCmenutext(9,"ZXNET        ",SX%+20,menuYadd%,10,0,-48)
            PROCmenutext(10,"SAVE LINK    ",SX%+20,menuYadd%,10,0,-48)
            PROCmenutext(11,"KYBRD FONTS  ",SX%+20,menuYadd%,10,0,-48)
            PROCmenutext(12,"GIF TO BMP   ",SX%+20,menuYadd%,10,0,-48)
            PROCmenutext(13,"HELP         ",SX%+20,menuYadd%,10,0,-48)
          ENDIF

          REM redraw dynamic values
          menuYadd%=716
          PROCgtext(CHR$(78+11*gridshow%),SX%+404,menuYadd%,9+gridshow%,0,-48)
          PROCgtext(CHR$(78+11*colmode%),SX%+404,menuYadd%,9+colmode%,0,0)


        WHEN 5 : REM insert sprite, ani, frame, text layout
          IF redraw%=-1 THEN
            IF menufrom%=M_moviemode% THEN
              PROCgtext("I",SX%+256,menuYadd%,10,0,-64)
            ELSE
              PROCgtext("I",SX%+160,menuYadd%,10,0,-64)
            ENDIF
          ENDIF

          REM insert menu has standard controls 0,1,2,3 = tab selector, 4 = main panel selector, 5 = scrollbar
          menuXadd%=SX%+16
          menuYadd%=908
          PROCmenucontrol(0,"SPR",menuXadd%,menuYadd%,12,(insertmode%=0)*2+8,4)
          PROCmenucontrol(1,"ANI",SX%+128,menuYadd%,12,(insertmode%=1)*2+8,4)
          PROCmenucontrol(2,"FRM",SX%+240,menuYadd%,12,(insertmode%=2)*2+8,4)
          PROCmenucontrol(3,"TXT",SX%+352,menuYadd%,12,(insertmode%=3)*2+8,4)

          GCOL 0,0
          RECTANGLE FILL SX%+16,menuYadd%-824,450,784

          REM full redraw section
          IF redraw%=-1 THEN
            CASE insertmode% OF
              WHEN 0,1 : REM spr / ani insert
                REM sprite selector
                PROCaddcontrange(4,SX%+18,menuYadd%-712,SX%+400,menuYadd%-46)
                IF insertmode%=1 THEN
                  PROCmenutext(6,"LRGSPR "+STR$(lrgx%)+"x"+STR$(lrgy%),SX%+18,menuYadd%-778,10,0,0)
                ENDIF

                REM scrolLbar
                PROCdrawscrollbar(5,SX%+412,menuYadd%-712,50,668,7)

                REM draw sprites
                menuYadd%-=160
                FOR Y%=0 TO 5
                  FOR X%=0 TO 3
                    CASE insertmode% OF
                      WHEN 0
                        S%=objsprscroll%*4+X%+Y%*4
                      WHEN 1
                        S%=sprani{(objaniscroll%*4+X%+Y%*4)}.s%(0)
                    ENDCASE

                    DX%=SX%+X%*96+12
                    DY%=menuYadd%-Y%*112
                    PROCdrawsprbitmap(S%,DX%+14,DY%+14)
                    COL%=8
                    IF S%>-1 COL%=8-(sprlist{(S%)}.m%*4)
                    GCOL 0,COL%
                    RECTANGLE DX%+8,DY%+8,90,106
                  NEXT
                NEXT

              WHEN 2 : REM frame insert
                REM frame selector
                PROCaddcontrange(4,SX%+18,menuYadd%-726,SX%+394,menuYadd%-46)

                REM scrolLbar
                PROCdrawscrollbar(5,SX%+412,menuYadd%-726,50,682,7)

                REM draw frames
                menuYadd%-=218
                FOR Y%=0 TO 3
                  FOR X%=0 TO 1
                    S%=objfrmscroll%*2+X%+Y%*2
                    DX%=SX%+X%*192+12
                    DY%=menuYadd%-Y%*172
                    PROCdrawframetomenu(S%,DX%+16,DY%+12)
                    GCOL 0,8
                    RECTANGLE DX%+8,DY%+8,180,164
                  NEXT
                NEXT

              WHEN 3 : REM text insert
                REM text selector
                PROCaddcontrange(4,SX%+18,menuYadd%-726,SX%+394,menuYadd%-46)

                REM scrolLbar
                PROCdrawscrollbar(5,SX%+412,menuYadd%-726,50,682,7)

                REM col selector
                PROCaddcontrange(6,SX%+16,menuYadd%-792,SX%+352,menuYadd%-740)
                FOR I%=0 TO 6
                  GCOL 0,I%+9
                  RECTANGLE FILL SX%+24+I%*48,menuYadd%-786,32,40
                NEXT
                RECTANGLE SX%+20+inserttextcol%*48,menuYadd%-790,40,48

                REM text strings
                menuYadd%-=52
                IF obj_txtcur%>-1 THEN
                  FOR Y%=0 TO 16
                    IF Y%+objtxtscroll%<=obj_txtcur% PROCgtext(LEFT$(txtlist$(Y%+objtxtscroll%),12),SX%+18,menuYadd%,10,0,-40)
                  NEXT
                ENDIF

            ENDCASE
          ENDIF

        WHEN 6 : REM object properties
          PROCgtext("O",SX%+192,menuYadd%,10,0,-64)
          IF redraw%=-1 THEN
            GCOL 0,0
            RECTANGLE FILL SX%+16,64,450,878
          ENDIF

          IF obj_lstcount%>-1 THEN
            COL%=10
            CASE objlist{(obj_lstcur%)}.type% OF
              WHEN 1 : REM sprite
                T$="SPR:"
              WHEN 2 : REM frame
                T$="FRM:"
                COL%=8
              WHEN 3 : REM text
                T$="TXT:"
              WHEN 4 : REM large sprite
                T$="LRG:"

            ENDCASE

            F$="ALL"
            IF objlist{(obj_lstcur%)}.f%>-1 F$=STR$(objlist{(obj_lstcur%)}.f%+1)

            PROCgtext(" "+RIGHT$("0000"+STR$(obj_lstcur%+1),5)+" ",SX%+20,menuYadd%,10,8,-56)
            PROCgtext(T$,SX%+20,menuYadd%,14,0,0)
            PROCgtext(RIGHT$("    "+STR$(objlist{(obj_lstcur%)}.obj%+1),5),SX%+162,menuYadd%,11,8,-48)
            IF spritechange%=-1 THEN
              PROCgtext(RIGHT$("     "+STR$(objlist{(obj_lstcur%)}.x%),6),SX%+130,menuYadd%,11,8,-48)
              PROCgtext(RIGHT$("     "+STR$(objlist{(obj_lstcur%)}.y%),6),SX%+130,menuYadd%,11,8,-48)
              PROCgtext(RIGHT$("     "+F$,6),SX%+130,menuYadd%,11,8,-48)
              PROCgtext(RIGHT$("     "+STR$(objlist{(obj_lstcur%)}.parent%),6),SX%+130,menuYadd%,11,8,-144)
              IF objlist{(obj_lstcur%)}.f%>-1 THEN
                PROCgtext(RIGHT$("  "+STR$(objlist{(obj_lstcur%)}.hop%),3),SX%+148,menuYadd%,11,4,-96)
              ELSE
                GCOL 0,1
                REM RECTANGLE FILL SX%+20,menuYadd%-286,SW%-40,340
              ENDIF
            ENDIF

            IF redraw%=-1 THEN
              menuYadd%=932

              REM              PROCgtext("@",SX%+20,menuYadd%,14,0)
              PROCmenutext(0," < ",SX%+256,menuYadd%,14,4,0)
              PROCmenutext(1," > ",SX%+354,menuYadd%,14,4,-104)
              PROCaddcontrange(2,SX%+162,menuYadd%+12,SX%+320,menuYadd%+48)

              IF spritechange%=-1 THEN
                PROCmenutext(3,"REMOVE",SX%+232,116,14,4,0)

                PROCgtext("WX:",SX%+20,menuYadd%,14,0,-48)
                PROCdrawcustomspr(-1,0,SX%+374,menuYadd%-22,COL%)
                PROCdrawcustomspr(-1,1,SX%+374,menuYadd%+22,COL%)
                PROCdrawcustomspr(-1,2,SX%+398,menuYadd%,COL%)
                PROCdrawcustomspr(-1,3,SX%+350,menuYadd%,COL%)
                PROCaddcontrange(4,SX%+346,menuYadd%-32,SX%+432,menuYadd%+48)
                PROCgtext("WY:",SX%+20,menuYadd%,14,0,-48)
                PROCaddcontrange(5,SX%+130,menuYadd%-36,SX%+320,menuYadd%+4)
                PROCgtext("FS:",SX%+20,menuYadd%,14,0,-48)
                PROCgtext("PA:",SX%+20,menuYadd%,14,0,-96)

                IF objlist{(obj_lstcur%)}.f%>-1 THEN
                  F$="REPEAT"
                  COL%=14
                  IF objlist{(obj_lstcur%)}.u%=1 THEN
                    F$=" UNDO "
                    COL%=11
                  ENDIF
                  PROCmenutext(6,F$,SX%+20,menuYadd%,COL%,4,-48)
                  PROCgtext("HOP:",SX%+20,menuYadd%,14,0,0)
                  PROCmenutext(7,"-",SX%+274,menuYadd%,14,4,0)
                  PROCmenutext(8,"+",SX%+338,menuYadd%,14,4,-48)

                ELSE
                  REM PROCmenutext(4,"      ",SX%+20,menuYadd%-288,7,8,0)
                ENDIF

                CASE objlist{(obj_lstcur%)}.type% OF
                  WHEN 1 : REM sprite

                    PROCdrawsprbitmap(objlist{(obj_lstcur%)}.obj%,SX%+30,86)
                    GCOL 0,8
                    RECTANGLE SX%+24,80,90,106
                    REM SY%-204

                  WHEN 2 : REM frame
                    PROCdrawframetomenu(objlist{(obj_lstcur%)}.obj%,SX%+32,84)
                    GCOL 0,8
                    RECTANGLE SX%+24,80,180,164

                  WHEN 3 : REM text

                  WHEN 4 : REM large sprite

                ENDCASE

              ELSE
                REM sprites select layout
                menuYadd%+=48
                GCOL 0,0
                RECTANGLE FILL SX%+16,menuYadd%-728,450,688

                menuYadd%-=160
                PROCaddcontrange(4,SX%+18,menuYadd%-552,SX%+400,menuYadd%+116)


                PROCdrawscrollbar(5,SX%+412,menuYadd%-552,50,666,7)

                FOR Y%=0 TO 5
                  FOR X%=0 TO 3
                    S%=objsprscroll%*4+X%+Y%*4

                    DX%=SX%+X%*96+12
                    DY%=menuYadd%-Y%*112
                    PROCdrawsprbitmap(S%,DX%+14,DY%+14)

                    GCOL 0,8
                    RECTANGLE DX%+8,DY%+8,90,106
                  NEXT
                NEXT

              ENDIF

            ENDIF
          ELSE
            PROCgtext("No Objects",SX%+20,menuYadd%,9,0,0)
          ENDIF

        WHEN 7 : REM frame properties
          F$=RIGHT$("000"+STR$(movieframe%+1),4)
          IF movieframe%=-1 F$="----"
          F$="F:"+F$
          PROCgtext(F$,32,menuYadd%,10,0,0)

          REM colour selector
          IF movieframe%>-1 THEN
            COL%=frmlist{(movieframe%)}.f%
            GCOL 0,0
            RECTANGLE FILL SX%+872,SY%+136,384,40
            RECTANGLE FILL SX%+872,SY%+80,348,40
            PROCgtext("FORE",SX%+704,SY%+168,15,0,0)
            F$=CHR$(83+(COL%>0)*13)
            PROCgtext(F$,SX%+874+ABS(COL%)*48,SY%+168,ABS(COL%)+8,0,0)
            PROCgtext("BACK",SX%+704,SY%+112,15,0,0)
            PROCgtext("B",SX%+874+frmlist{(movieframe%)}.b%*48,SY%+112,frmlist{(movieframe%)}.b%+8,0,0)
            FOR I%=0 TO 7
              GCOL 0,I%-8*(I%<>0)
              IF I%<>ABS(COL%) RECTANGLE FILL SX%+872+I%*48,SY%+136,32,40
              IF I%<>frmlist{(movieframe%)}.b% RECTANGLE FILL SX%+872+I%*48,SY%+80,32,40
            NEXT
            PROCanimcontrol(15,"CopyAll",704,56,8,7,14,4)
            PROCanimcontrol(16,"RsetAll",1000,56,8,7,14,4)
          ENDIF
          PROCaddcontrange(0,SX%+872,SY%+136,SX%+1240,SY%+176)
          PROCaddcontrange(1,SX%+872,SY%+80,SX%+1240,SY%+120)

          REM frame selector
          F%=0
          GCOL 0,15
          RECTANGLE SX%+114+F%*180,SY%+214,172,160

          GCOL 0,8
          FOR I%=0 TO 5
            RECTANGLE FILL SX%+120+I%*180,SY%+220,160,148
          NEXT

          REM add frames controls
          menuadd%=32
          PROCanimcontrol(2,"FIXED",menuadd%,100,0,7,14,4)
          PROCanimcontrol(3,"PANNED",menuadd%,100,0,7,14,4)
          menuadd%=32
          PROCgtext(RIGHT$("000"+STR$(movieframeadd%),3),menuadd%,160,15,4,0)
          menuadd%+=128
          PROCanimcontrol(4,"<<",menuadd%,160,0,8,14,0)
          PROCanimcontrol(5,"<",menuadd%,160,0,8,14,0)
          PROCanimcontrol(6,">",menuadd%,160,0,8,14,0)
          PROCanimcontrol(7,">>",menuadd%,160,0,8,14,0)


        WHEN 8 : REM movie mode menu
          PROCgtext("M",SX%+128,menuYadd%,10,0,-64)

          PROCmenutext(0,"LOAD MOVIE   ",SX%+20,menuYadd%,14,0,-48)
          PROCmenutext(1,"SAVE MOVIE   ",SX%+20,menuYadd%,14,0,-96)
          PROCmenutext(2,"RESET OBJECTS",SX%+20,menuYadd%,11,0,-48)
          PROCmenutext(3,"RESET FRAMES ",SX%+20,menuYadd%,11,0,-48)
          PROCmenutext(4,"RESET MOVIE  ",SX%+20,menuYadd%,11,0,-96)
          PROCmenutext(6,"CUSTOM PROC 1",SX%+20,menuYadd%,10,0,-48)
          PROCmenutext(7,"CUSTOM PROC 2",SX%+20,menuYadd%,10,0,-48)

          PROCmenutext(5,"HELP SCREEN  ",SX%+20,116,11,0,0)

      ENDCASE

      ENDPROC

      REM ##########################################################
      REM shape and special sub menu
      DEF PROCsubhandler
      LOCAL done%,shift%,L%,C%,SP%,NS%,OS%,SX%,SY%,F%,F$

      PROCWAITMOUSE(0)
      C%=-1
      IF MX%>subm{(sub_cur%)}.x% AND MX%<subm{(sub_cur%)}.x%+subm{(sub_cur%)}.w% AND MY%>subm{(sub_cur%)}.y% AND MY%<subm{(sub_cur%)}.y%+subm{(sub_cur%)}.h% THEN
        C%=FNgetcontrol

        CASE sub_cur% OF
          WHEN 0 : REM paint menu
            CASE C% OF
              WHEN 0 : REM paint
                toolsel%=T_paint&
              WHEN 1 : REM line
                toolsel%=T_line&
              WHEN 2 : REM box
                toolsel%=T_box&
              WHEN 3 : REM circle
                toolsel%=T_circle&
              WHEN 4 : REM symmetry
                toolsel%=T_symmetry&
              WHEN 5 : REM text
                toolsel%=T_text&
              WHEN 6 : REM animate lines toggle
                animateshape%=(animateshape%+1) AND 1
              WHEN 7 : REM dec line gap
                IF animategap%>0 THEN animategap%-=1
              WHEN 8 : REM inc line gap
                IF animategap%<10 THEN animategap%+=1
              WHEN 9 : REM dec line len
                IF animatelen%>1 THEN animatelen%-=1
              WHEN 10 : REM inc line len
                IF animatelen%<10 THEN animatelen%+=1
              WHEN 11 : REM shape - outline
                shapetype%=0
              WHEN 12 : REM shape - filled
                shapetype%=1
              WHEN 13 : REM shape - empty
                shapetype%=2
              WHEN 14 : REM dec font
                F%=fontcur%
                IF fontcur%>0 fontcur%-=1
                IF fontcur%<>F% AND fontcur%>0 AND fontfound%=1 THEN PROCloadfont(fontname$(fontcur%))
              WHEN 15 : REM inc font
                F%=fontcur%
                IF fontcur%<fontcount% fontcur%+=1
                IF fontcur%<>F% AND fontcur%>0 AND fontfound%=1 THEN PROCloadfont(fontname$(fontcur%))

              WHEN 16 : REM keyboard font screen
                done%=1
            ENDCASE

            PROCsubupdate(0)

            IF C%>-1 AND C%<6 THEN
              done%=1
              toolcursor%=15
            ENDIF

          WHEN 1 : REM dither
            CASE C% OF
              WHEN 0 : REM dither 1
                toolsel%=T_dither1&
              WHEN 1 : REM dither 2
                toolsel%=T_dither2&
              WHEN 2 : REM dither 3
                toolsel%=T_dither3&
              WHEN 3 : REM dither 4
                toolsel%=T_dither4&
              WHEN 4 : REM solid block
                toolsel%=T_dither5&
              WHEN 5 : REM brush 1
                toolsel%=T_brush1&
              WHEN 6 : REM brush 2
                toolsel%=T_brush2&
              WHEN 7 : REM brush 3
                toolsel%=T_brush3&
              WHEN 8 : REM brush 4
                toolsel%=T_brush4&
              WHEN 9 : REM brush 5
                toolsel%=T_brush5&
              WHEN 10 : REM brush 6
                toolsel%=T_brush6&
              WHEN 11 : REM brush 7
                toolsel%=T_brush7&

            ENDCASE

            IF C%>-1 AND C%<12 THEN
              done%=1
              toolcursor%=16
              IF C%<5 THEN dither%=toolsel%-T_dither1&
            ENDIF

          WHEN 2 : REM copy paste
            CASE C% OF
              WHEN 0,1 : REM copy menu tools
                done%=1
                copypaste%=C%
                toolsel%=T_paste&
                toolcursor%=17

              WHEN 14 : REM fix x paste pos
                copylockxt%=(copylockxt%+1) AND 1 : REM lock horizontal paste pos

              WHEN 15 : REM fix y paste pos
                copylockyt%=(copylockyt%+1) AND 1 :REM lock vertical paste pos

              WHEN 16 : REM paste transparent
                copy_trns%=(copy_trns%+1) AND 1

              OTHERWISE
                done%=1

            ENDCASE

            PROCsubupdate(0)

          WHEN 3 : REM fill
            CASE C% OF

              WHEN 0,1,2,3,4,5,6,7,8 : REM flood fill / gradients
                toolsel%=C%+T_fill&
                toolcursor%=18

              WHEN 9,10,11,12 : REM gradient type
                gradtype%=C%-9

            ENDCASE
            PROCsubupdate(C%)

            IF C%>-1 AND C%<9 THEN  done%=1

          WHEN 4 : REM special

            CASE C% OF
              WHEN 0 : REM flash
                toolsel%=T_flash&
              WHEN 1 : REM double
                toolsel%=T_double&
              WHEN 2 : REM separated
                toolsel%=T_separate&
              WHEN 3 : REM hold
                toolsel%=T_hold&
              WHEN 4 : REM show grid
                gridshow%=(gridshow%+1) AND 1
              WHEN 5 : REM column mode
                colmode%=(colmode%+1) AND 1

            ENDCASE

            IF C%>=0 AND C%<14THEN
              PROCsubupdate(C%)

              REM tool selected
              IF C%>-1 AND C%<4 THEN
                toolcursor%=19
                done%=1
              ENDIF

              REM other menus
              IF C%>5 THEN
                done%=1
              ENDIF

            ENDIF

          WHEN 5 : REM sprite select
            CASE C% OF
              WHEN 0,1,2,3 : REM sprite, ani, frame, text selector
                IF insertmode%<>C% THEN
                  insertmode%=C%
                  PROCsubupdate(-1)
                ENDIF

              WHEN 4 : REM insert sprite, animation, frame or text into world map
                CASE insertmode% OF
                  WHEN 0 : REM spr insert
                    SP%=objsprscroll%*4+(MX%-controlrange{(C%)}.x1%) DIV 96+((controlrange{(C%)}.y2%-MY%) DIV 112)*4
                    spritemoving%=SP%
                    PROCspriteinserthandler(0)
                    done%=1

                  WHEN 1 : REM spr set insert
                    SP%=objaniscroll%*4+(MX%-controlrange{(C%)}.x1%) DIV 96+((controlrange{(C%)}.y2%-MY%) DIV 112)*4
                    IF sprani{(SP%)}.s%(0)>-1 THEN
                      insertrepeat%=1
                      insertset%=SP%
                      insertani%=0
                      insertphase%=0
                      spritemoving%=sprani{(SP%)}.s%(0)
                      PROCspriteinserthandler(0)

                      done%=1
                    ENDIF

                  WHEN 2 : REM frm insert
                    IF menufrom%=M_moviemode% THEN
                      REM insert frame to movie mode
                      SP%=objfrmscroll%*2+(MX%-controlrange{(C%)}.x1%) DIV 192+((controlrange{(C%)}.y2%-MY%) DIV 172)*2
                      IF obj_lstcount%<obj_lstmax% obj_lstcount%+=1
                      obj_lstcur%=obj_lstcount%
                      PROCspriteinserthandler(0)
                      spriteselect%=-1
                      spriteselectold%=-1
                      objlist{(obj_lstcount%)}.obj%=SP%
                      objlist{(obj_lstcount%)}.type%=2
                      objlist{(obj_lstcount%)}.x%=(mmWX% DIV 2)*2
                      objlist{(obj_lstcount%)}.y%=((mmWY%-3) DIV 3)*3
                      IF movieframe%>-1 THEN
                        objlist{(obj_lstcount%)}.f%=movieframe%
                      ELSE
                        objlist{(obj_lstcount%)}.f%=-1
                      ENDIF
                      objlist{(obj_lstcount%)}.parent%=-1

                      done%=1
                    ELSE
                      REM insert to main canvas
                    ENDIF

                  WHEN 3 : REM txt insert
                    SP%=objtxtscroll%+((controlrange{(C%)}.y2%-MY%) DIV 40)
                    IF SP%>-1 AND SP%<=obj_txtcur% THEN
                      spritemoving%=SP%
                      PROCspriteinserthandler(0)
                      done%=1
                    ENDIF

                ENDCASE

              WHEN 5 : REM scroll bar
                SP%=controlrange{(C%)}.y2%-MY%
                SY%=controlrange{(C%)}.y2%-controlrange{(C%)}.y1%
                L%=SY% / 2
                CASE insertmode% OF
                  WHEN 0 : REM spr select
                    IF SP%<64 THEN NS%=-1
                    IF SP%>62 AND SP%<L% THEN NS%=-6
                    IF SP%>L%-2 AND SP%<SY%-64 THEN NS%=6
                    IF SP%>SY%-62 THEN NS%=1

                    OS%=objsprscroll%
                    objsprscroll%+=NS%
                    IF objsprscroll%<0 THEN objsprscroll%=0
                    IF objsprscroll%>19 THEN objsprscroll%=19
                    IF OS%<>objsprscroll% PROCsubupdate(-1)

                  WHEN 1 : REM ani select
                    IF SP%<64 THEN NS%=-1
                    IF SP%>62 AND SP%<L% THEN NS%=-6
                    IF SP%>L%-2 AND SP%<SY%-64 THEN NS%=6
                    IF SP%>SY%-62 THEN NS%=1

                    OS%=objaniscroll%
                    objaniscroll%+=NS%
                    IF objaniscroll%<0 THEN objaniscroll%=0
                    IF objaniscroll%>19 THEN objaniscroll%=19
                    IF OS%<>objaniscroll% PROCsubupdate(-1)

                  WHEN 2 : REM frm select
                    IF SP%<64 THEN NS%=-1
                    IF SP%>62 AND SP%<L% THEN NS%=-4
                    IF SP%>L%-2 AND SP%<SY%-64 THEN NS%=4
                    IF SP%>SY%-62 THEN NS%=1

                    OS%=objfrmscroll%
                    objfrmscroll%+=NS%
                    IF objfrmscroll%<0 THEN objfrmscroll%=0
                    IF objfrmscroll%>(frame_max%-8) DIV 2 THEN objfrmscroll%=(frame_max%-8) DIV 2
                    IF OS%<>objfrmscroll% PROCsubupdate(-1)

                  WHEN 3 : REM txt select
                    IF SP%<64 THEN NS%=-1
                    IF SP%>62 AND SP%<L% THEN NS%=-17
                    IF SP%>L%-2 AND SP%<SY%-64 THEN NS%=17
                    IF SP%>SY%-62 THEN NS%=1

                    OS%=objtxtscroll%
                    objtxtscroll%+=NS%
                    IF objtxtscroll%>(obj_txtcur%-16) objtxtscroll%=obj_txtcur%-16
                    IF objtxtscroll%<0 THEN objtxtscroll%=0
                    IF OS%<>objtxtscroll% PROCsubupdate(-1)

                ENDCASE

              WHEN 6 : REM insert menu controls
                CASE insertmode% OF
                  WHEN 1 : REM large animation insert
                    insertrepeat%=1
                    insertset%=-1
                    insertani%=0
                    insertphase%=0
                    spritemoving%=0
                    insertlarge%=1
                    done%=1

                  WHEN 3 : REM text colour selector
                    SP%=(MX%-controlrange{(C%)}.x1%) DIV 48
                    IF SP%<0 SP%=0
                    IF SP%>6 SP%=6
                    IF SP%<>inserttextcol% THEN
                      inserttextcol%=SP%
                      PROCsubupdate(-1)
                    ENDIF
                ENDCASE

              OTHERWISE
                REM done%=1

            ENDCASE

          WHEN 6 : REM object properties
            CASE C% OF
              WHEN 0 : REM dec current object
                IF obj_lstcur%>0 THEN
                  obj_lstcur%-=1
                ELSE
                  obj_lstcur%=obj_lstcount%
                ENDIF
              WHEN 1 : REM inc current object
                IF obj_lstcur%<obj_lstcount% THEN
                  obj_lstcur%+=1
                ELSE
                  obj_lstcur%=0
                ENDIF
              WHEN 2 : REM change sprite
                spritechange%=1

              WHEN 3 : REM remove object from list
                PROCremoveobj(obj_lstcur%)
                IF obj_lstcur%>0 obj_lstcur%-=1
                spriteselect%=-1
                spriteselectold%=-1
                done%=1

              WHEN 4 : REM move sprite
                IF spritechange%=-1 THEN
                  IF objlist{(obj_lstcur%)}.type%=1 THEN
                    spritemoving%=objlist{(obj_lstcur%)}.obj%
                    spriterelocate%=1
                    done%=1
                  ENDIF
                ELSE
                  REM select new sprite
                  SP%=objsprscroll%*4+(MX%-controlrange{(C%)}.x1%) DIV 96+((controlrange{(C%)}.y2%-MY%) DIV 112)*4
                  objlist{(obj_lstcur%)}.obj%=SP%
                  spritechange%=-1
                  PROCsubupdate(-1)
                ENDIF

              WHEN 5 : REM change frame index
                IF spritechange%=-1 THEN
                  SY%=932
                  SX%=controlrange{(C%)}.x1%
                  F%=0
                  F$="ALL"
                  IF objlist{(obj_lstcur%)}.f%>-1 F$=STR$(objlist{(obj_lstcur%)}.f%+1)
                  PROCgtext(RIGHT$("     "+F$,6),SX%,SY%-200,15,0,0)
                  OS%=LEN(F$)*32
                  REPEAT
                    K%=INKEY(0)
                    IF K%=-1 THEN
                      WAIT 10
                      F%=1-F%
                      IF F%=1 OS%=LEN(F$)*32
                      GCOL 0,F%*15
                      LINE SX%+160-OS%,SY%-228,SX%+192-OS%,SY%-228
                    ELSE
                      REM check for numbers 0 to 9 and update string value ALL=0 all other values are numeric
                      IF K%>47 AND K%<58 THEN
                        IF F$="ALL" THEN
                          IF K%<>48 F$=CHR$(K%)
                        ELSE
                          IF LEN(F$)<4 F$=F$+CHR$(K%)
                        ENDIF
                      ENDIF
                      REM check if delete or backspace is pressed
                      IF K%=135 OR K%=8 THEN
                        IF VAL(F$)>0 F$=LEFT$(F$,LEN(F$)-1)
                        IF VAL(F$)=0 F$="ALL"
                      ENDIF
                      PROCgtext(RIGHT$("     "+F$,6),SX%,SY%-200,15,0,0)
                    ENDIF
                  UNTIL K%=13 OR K%=27
                  IF K%=13 objlist{(obj_lstcur%)}.f%=VAL(F$)-1
                  GCOL 0,0
                  LINE SX%+160-OS%,SY%-228,SX%+192-OS%,SY%-228
                ELSE
                  REM select sprite scrolling
                  SP%=controlrange{(C%)}.y2%-MY%
                  SY%=controlrange{(C%)}.y2%-controlrange{(C%)}.y1%
                  L%=SY% / 2

                  IF SP%<64 THEN NS%=-1
                  IF SP%>62 AND SP%<L% THEN NS%=-6
                  IF SP%>L%-2 AND SP%<SY%-64 THEN NS%=6
                  IF SP%>SY%-62 THEN NS%=1

                  OS%=objsprscroll%
                  objsprscroll%+=NS%
                  IF objsprscroll%<0 THEN objsprscroll%=0
                  IF objsprscroll%>19 THEN objsprscroll%=19
                  REM IF OS%<>objsprscroll% PROCsubupdate(-1)
                ENDIF

              WHEN 6 : REM repeat button
                REM undo repeat for this object
                IF objlist{(obj_lstcur%)}.u%=1 THEN
                  objlist{(obj_lstcur%)}.u%=0
                  SP%=obj_lstcount%
                  FOR L%=SP% TO obj_lstcur%+1 STEP -1
                    IF objlist{(L%)}.parent%=obj_lstcur% PROCremoveobj(L%)
                  NEXT

                ELSE
                  REM init sprite repeat
                  IF objlist{(obj_lstcur%)}.type%=3 THEN
                    insertrepeat%=3
                  ELSE
                    insertrepeat%=2
                  ENDIF
                  insertphase%=1
                  spritemoving%=objlist{(obj_lstcur%)}.obj%
                  done%=1
                ENDIF

              WHEN 7 : REM hop dec
                IF objlist{(obj_lstcur%)}.hop%>0 THEN objlist{(obj_lstcur%)}.hop%-=1

              WHEN 8 : REM hop inc
                IF objlist{(obj_lstcur%)}.hop%<100 THEN objlist{(obj_lstcur%)}.hop%+=1

            ENDCASE
            IF (C%>-1 AND C%<3) OR C%>4 THEN PROCsubupdate(-1)

          WHEN 7 : REM frm properties
            shift%=INKEY(-1)
            CASE C% OF
              WHEN 0 : REM foreground colour
                IF movieframe%>-1 THEN
                  SP%=(MX%-controlrange{(C%)}.x1%) DIV 48
                  IF SP%>0 AND SP%<8 THEN
                    IF frmlist{(movieframe%)}.f%=SP% SP%=-SP%
                    frmlist{(movieframe%)}.f%=SP%
                  ENDIF
                ENDIF
              WHEN 1 : REM background colour
                IF movieframe%>-1 THEN
                  SP%=(MX%-controlrange{(C%)}.x1%) DIV 48
                  IF SP%>-1 AND SP%<8 frmlist{(movieframe%)}.b%=SP%
                ENDIF
              WHEN 2 : REM add fixed frames button
                IF movieframeadd%>0 THEN
                  FOR X%=1 TO movieframeadd%
                    IF movieframetotal%<movieframemax% THEN
                      movieframetotal%+=1
                      frmlist{(movieframetotal%)}.x%=mmWX%
                      frmlist{(movieframetotal%)}.y%=mmWY%
                      frmlist{(movieframetotal%)}.b%=0
                      frmlist{(movieframetotal%)}.f%=7
                      mmWX%+=movieframeaddh%
                      mmWY%+=movieframeaddv%
                    ENDIF
                  NEXT
                  movieframe%=movieframetotal%
                ENDIF
              WHEN 3 : REM add panned frames button
                spritemoving%=9999
                insertrepeat%=4
                insertphase%=0
                done%=1

              WHEN 4 : REM frame dec ++
                IF shift% THEN
                  movieframeadd%=0
                ELSE
                  movieframeadd%-=10
                  IF movieframeadd%<0 movieframeadd%=0
                ENDIF
              WHEN 5 : REM frame dec
                movieframeadd%-=1
                IF movieframeadd%<0 movieframeadd%=0
              WHEN 6 : REM frame inc
                movieframeadd%+=1
                IF movieframeadd%>200 movieframeadd%=200
              WHEN 7 : REM frame inc ++
                IF shift% THEN
                  movieframeadd%=200
                ELSE
                  movieframeadd%+=10
                  IF movieframeadd%>200 movieframeadd%=200
                ENDIF
              WHEN 15,16 : REM copy / reset colours to all frames
                IF movieframe%>-1 THEN
                  IF movieframetotal%>-1 THEN
                    IF C%=16 THEN
                      F%=7
                      B%=0
                    ELSE
                      F%=frmlist{(movieframe%)}.f%
                      B%=frmlist{(movieframe%)}.b%
                    ENDIF
                    FOR X%=0 TO movieframetotal%
                      frmlist{(X%)}.b%=B%
                      frmlist{(X%)}.f%=F%
                    NEXT
                    done%=1
                  ENDIF
                ENDIF

            ENDCASE
            IF C%>-1 AND C%<15 THEN PROCsubupdate(-1)

          WHEN 8 : REM movie mode menu
            CASE C% OF
              WHEN 0 : REM load
                done%=1
              WHEN 1 : REM save
                done%=1
              WHEN 2 : REM reset obj

              WHEN 3 : REM reset frm

              WHEN 4,5 : REM reset all, help screen
                done%=1

              WHEN 6,7 : REM custom procs
                done%=1

            ENDCASE
        ENDCASE

      ELSE
        done%=1
      ENDIF
      REM IF SP%>sprite_max%-1 THEN SP%=-1
      REM PRINTTAB(0,1)STR$(C%);" "; STR$(done%); " ";
      REM PROCWAITMOUSE(4)
      REM PROCWAITMOUSE(0)

      IF done%>0 THEN
        PROCWAITMOUSE(0)

        PROCchangemode(7,1)

        CASE sub_cur% OF
          WHEN 0 : REM paint menu
            CASE C% OF

              WHEN 16 : REM keyboard and options screen
                menuext%=M_keyboard%
                PROCkeyboardmenu(1)

              OTHERWISE

                CASE menufrom% OF
                  WHEN M_keyboard%
                    menuext%=M_keyboard%
                    PROCkeyboardmenu(1)

                  WHEN M_sprites%
                    menuext%=M_sprites%
                    PROCspritescreen(1)

                  WHEN M_moviemode%
                    menuext%=M_moviemode%
                    PROCmenurestore

                  OTHERWISE
                    PROCmenurestore
                    REM PROCdrawgrid

                ENDCASE

            ENDCASE

          WHEN 1 : REM dither menu
            CASE menufrom% OF
              WHEN 1
                menuext%=M_keyboard%
                PROCkeyboardmenu(1)

              WHEN 2
                menuext%=M_sprites%
                PROCspritescreen(1)

              WHEN M_moviemode%
                menuext%=M_moviemode%
                PROCmenurestore

              OTHERWISE
                PROCmenurestore
                REM PROCdrawgrid

            ENDCASE

          WHEN 2 : REM copy paste menu
            CASE C% OF
              WHEN 0,1
                PROCmenurestore

                REM show control codes while copying
                IF C%=0 THEN
                  PROCselectregion
                ENDIF

              WHEN 2 : REM paste all frames
                PROCmenurestore
                PROCundosaveall

                FOR X%=1 TO frame_max%
                  PROCpasteregion_buf(X%,copylockx%,copylocky%)
                NEXT

              WHEN 3 : REM dupe all codes to all frames
                PROCmenurestore
                PROCundosaveall

                PROCcopycodes_buf(frame%)

              WHEN 4 : REM copy frame to next frame
                PROCmenurestore

                X%=frame%+1
                IF X%>frame_max% THEN X%=1
                PROCcopyframe(frame%,X%,0,0,0)

              WHEN 5 : REM copy frame to previous frame
                PROCmenurestore

                X%=frame%-1
                IF X%<1 THEN X%=frame_max%
                PROCcopyframe(frame%,X%,0,0,0)

              WHEN 6 : REM mirror selection
                PROCmenurestore
                PROCmirrorregion(0)

              WHEN 7 : REM mirror left screen
                PROCmenurestore
                PROCmirrorregion(1)

              WHEN 8 : REM refelect selection
                PROCmenurestore
                PROCreflectregion(0)

              WHEN 9 : REM refelect top screen
                PROCmenurestore
                PROCreflectregion(1)

              WHEN 10 : REM flip horizontal
                PROCmenurestore
                PROCfliphregion

              WHEN 11 : REM flip vertical
                PROCmenurestore
                PROCflipvregion

              WHEN 12 : REM negative
                PROCmenurestore
                PROCnegativeregion

              WHEN 13 : REM erase
                PROCmenurestore
                IF copymovef%=frame% THEN
                  PROCundosave
                  PROCeraseregion
                  PROCframesave(frame%)
                ENDIF


              OTHERWISE
                CASE menufrom% OF
                  WHEN 1
                    menuext%=M_keyboard%
                    PROCkeyboardmenu(1)

                  WHEN 2
                    menuext%=M_sprites%
                    PROCspritescreen(1)

                  WHEN M_moviemode%
                    menuext%=M_moviemode%
                    PROCmenurestore

                  OTHERWISE
                    PROCmenurestore
                    REM PROCdrawgrid

                ENDCASE
            ENDCASE

          WHEN 3 : REM fill
            CASE C% OF
              WHEN 1,2,3,4,5,6,7,8 : REM gradients
                PROCmenurestore

              OTHERWISE
                CASE menufrom% OF
                  WHEN 1
                    menuext%=M_keyboard%
                    PROCkeyboardmenu(1)

                  WHEN 2
                    menuext%=M_sprites%
                    PROCspritescreen(1)

                  WHEN M_moviemode%
                    menuext%=M_moviemode%
                    PROCmenurestore

                  OTHERWISE
                    PROCmenurestore
                    REM PROCdrawgrid

                ENDCASE
            ENDCASE

          WHEN 4 : REM special
            CASE C% OF

              WHEN 6 : REM sprites screen
                menuext%=M_sprites%
                menufrom%=M_sprites%
                PROCspritescreen(1)

              WHEN 7 : REM movie mode
                menuext%=M_moviemode%
                menufrom%=M_moviemode%
                PROCmenurestore

              WHEN 8: REM edit.tf export
                frame%-=1
                PROCloadnextframe(1,0)
                PROCmenurestore
                REM PROCdrawgrid
                PROCexport_toURL(0)

              WHEN 9: REM zxnet export
                frame%-=1
                PROCloadnextframe(1,0)
                PROCmenurestore
                REM PROCdrawgrid
                PROCexport_toURL(1)

              WHEN 10 REM save link to file
                frame%-=1
                PROCloadnextframe(1,0)
                PROCmenurestore
                REM PROCdrawgrid
                PROCexport_toURL(2)

              WHEN 11: REM keyboard and options screen
                menuext%=M_keyboard%
                PROCkeyboardmenu(1)

              WHEN 12: REM gif to bmp
                PROCmenudraw
                PROCexportgif

              WHEN 13: REM help screen
                PROCmenudraw
                PROCshowhelp

              OTHERWISE
                CASE menufrom% OF
                  WHEN 1
                    menuext%=M_keyboard%
                    PROCkeyboardmenu(1)

                  WHEN 2
                    menuext%=M_sprites%
                    PROCspritescreen(1)

                  WHEN M_moviemode%
                    menuext%=M_moviemode%
                    PROCmenurestore

                  OTHERWISE
                    PROCmenurestore
                    REM PROCdrawgrid

                ENDCASE
            ENDCASE

          WHEN 5 : REM object select sub menu
            CASE C% OF
              WHEN 4 : REM select sprite to drag or frame to insert
                IF menumode%=M_moviemode% THEN
                  menuext%=M_moviemode%
                ELSE
                  menuext%=M_canvasmode%
                  menufrom%=M_canvasmode%
                ENDIF
                PROCmenurestore

              OTHERWISE
                IF menufrom%=M_moviemode% THEN
                  menuext%=M_moviemode%
                  menufrom%=M_moviemode%
                ELSE
                  menuext%=M_canvasmode%
                  menufrom%=M_canvasmode%
                ENDIF
                PROCmenurestore

            ENDCASE

            IF spritemoving%>-1 THEN
              IF menuext%=M_canvasmode% PROCmenudraw
              PROCspritemoveinit(1)
            ENDIF

          WHEN 6 : REM object property
            CASE C% OF
              WHEN 0 : REM

              WHEN 2 : REM move sprite
                menuext%=M_moviemode%
                PROCmenurestore

              OTHERWISE
                menuext%=M_moviemode%
                PROCmenurestore

            ENDCASE
            IF spritemoving%>-1 PROCspritemoveinit(1)

          WHEN 7 : REM frm properties
            CASE C% OF
              WHEN 0 : REM

              OTHERWISE
                menuext%=M_moviemode%
                PROCmenurestore
                IF spritemoving%>-1 PROCspritemoveinit(1)

            ENDCASE

          WHEN 8 : REM movie mode menu
            CASE C% OF
              WHEN 0 : REM load movie
                PROCloadfile(6)
                menuext%=M_moviemode%
                PROCmenurestore

              WHEN 1 : REM save movie
                PROCmenurestore
                PROCsavemovie
                menuext%=M_moviemode%
                PROCmenurestore

              WHEN 4 : REM reset movie
                done%=0
                menuext%=M_moviemode%
                PROCmenudraw

                FOR SY%=6 TO 11
                  PROCprint40(SY%,"")
                NEXT
                PRINTTAB(2,8)"MOVIE DATA WILL BE RESET!"
                PRINTTAB(2,9)"RESET MOVIE DATA? ";tg$;"YES  ";tr$;"NO"
                REPEAT
                  PROCREADMOUSE

                  IF MB%=4 THEN
                    PROCWAITMOUSE(0)
                    IF TY%=9 AND TX%>20 AND TX%<24 THEN done%=1
                    IF TY%=9 AND TX%>26 AND TX%<29 THEN done%=2
                  ELSE
                    WAIT 2
                  ENDIF
                UNTIL done%<>0

                IF done%=1 THEN PROCresetmovie

                PROCmenurestore

              WHEN 5 : REM help screen
                PROCmenudraw
                PROCshowhelp

              WHEN 6 : REM custom proc 1
                menuext%=M_moviemode%
                PROCCUSTOMPROC1
                PROCmenurestore

              WHEN 7 : REM custom proc 2
                menuext%=M_moviemode%
                PROCCUSTOMPROC2
                PROCmenurestore

            ENDCASE
        ENDCASE
      ENDIF

      ENDPROC

      REM ##########################################################
      REM remove object from object list
      DEF PROCremoveobj(O%)
      LOCAL L%
      IF obj_lstcount%=0 OR obj_lstcount%=O% THEN
        objlist{(O%)}.obj%=-1
      ELSE
        FOR L%=O% TO obj_lstcount%-1
          objlist{(L%)}.obj%=objlist{(L%+1)}.obj%
          objlist{(L%)}.type%=objlist{(L%+1)}.type%
          objlist{(L%)}.x%=objlist{(L%+1)}.x%
          objlist{(L%)}.y%=objlist{(L%+1)}.y%
          objlist{(L%)}.f%=objlist{(L%+1)}.f%
          objlist{(L%)}.parent%=objlist{(L%+1)}.parent%
          objlist{(L%)}.rel%=objlist{(L%+1)}.rel%
          objlist{(L%)}.hop%=objlist{(L%+1)}.hop%
          objlist{(L%)}.h%=objlist{(L%+1)}.h%
          objlist{(L%)}.v%=objlist{(L%+1)}.v%
        NEXT
      ENDIF
      obj_lstcount%-=1

      ENDPROC

      REM ##########################################################
      REM display moving objects
      DEF PROCobjdraw(px%,py%,m%,c%)
      LOCAL C%,X%,Y%,SX%,SY%,SW%,SH%,YC%,XC%,S%,A$

      GCOL m%,c%

      CASE insertmode% OF
        WHEN 0,1 : REM sprite, animation

          IF insertlarge%=0 THEN
            SX%=px%-sprlist{(spritemoving%)}.w%
            SY%=py%-(sprlist{(spritemoving%)}.h%*3 DIV 2)
            IF sprlist{(spritemoving%)}.m%=1 THEN
              SX%=(SX% DIV 2)*2
              SY%=(SY% DIV 3)*3
            ENDIF
            REM scan over and plot sprite data within sprite width and height
            SH%=sprlist{(spritemoving%)}.h%*3-1
            SW%=sprlist{(spritemoving%)}.w%*2-1
            FOR Y%=0 TO SH%
              YC%=SY%+Y%
              IF YC%>=0 AND YC%<75 THEN
                FOR X%=0 TO SW%
                  XC%=(SX%+X%)*16
                  IF XC%>=0 AND XC%<1280 THEN
                    C%=FNpoint_sprbuf(X%,Y%,spritemoving%)
                    IF C% RECTANGLE FILL XC%,sixel{(YC%)}.y%,14,sixel{(YC%)}.h%
                  ENDIF
                NEXT
              ENDIF
            NEXT
          ELSE
            SX%=px%
            SY%=py%
            S%=0
            REM scan over and plot sprite data within sprite width and height
            FOR X%=0 TO lrgx%
              XC%=(SX%+X%)*16
              FOR Y%=lrgy% TO 0 STEP -1
                YC%=SY%+Y%
                IF YC%>=0 AND YC%<75 AND XC%>=0 AND XC%<1280 THEN
                  C%=sprlrg&(spritemoving%,S%)
                  IF C% RECTANGLE FILL XC%,sixel{(YC%)}.y%,14,sixel{(YC%)}.h%
                ENDIF
                S%+=1
              NEXT
            NEXT

          ENDIF
        WHEN 3 : REM text
          A$="*"+txtlist$(spritemoving%)
          SX%=((px%-LEN(A$)) DIV 2)*32
          SY%=(py% DIV 3)*3
          VDU 5
          FOR X%=0 TO LEN(A$)-1
            IF (SX%+X%*32)<1278 THEN
              MOVE SX%+X%*32,sixel{(SY%)}.y%+12
              PRINT MID$(A$,X%+1,1)
            ENDIF
          NEXT
          VDU 4

      ENDCASE
      ENDPROC

      REM ##########################################################
      REM display and update sprite details dialog
      DEF PROCspriteinserthandler(c%)
      LOCAL done%,C%,SP%,NS%,OS%,SX%,SY%,F%,F$

      IF c%=1 PROCchangemode(6,0)

      REM reset / adjust frame controls
      IF insertsave%=0 THEN
        insertrepflag%=0   : REM multiple frames flag
        insertskipcount%=0 : REM multiple frames skip count
        insertfrmrep%=0    : REM animation frame repeat count
        insertfrmindex%=1  : REM animation frame start index
        IF menufrom%=M_moviemode% insertrelflag%=1 ELSE insertrelflag%=0  : REM sprite relative position flag
      ELSE
        IF insertfrmindex%>sprani{(insertset%)}.c% insertfrmindex%=1
      ENDIF

      PROCspriteinsertupdate(1)

      REPEAT
        PROCWAITMOUSE(4)
        PROCWAITMOUSE(0)
        C%=-1
        C%=FNgetcontrol

        CASE C% OF
          WHEN 0 : REM go button
            done%=1
          WHEN 1 : REM save settings
            insertsave%=1-insertsave%
          WHEN 2 : REM toggle multiple frames flag
            insertrepflag%=1-insertrepflag%
          WHEN 3 : REM toggle relative flag
            insertrelflag%=1-insertrelflag%
          WHEN 4 : REM decrement frame skip count
            IF insertskipcount%>0 insertskipcount%-=1
          WHEN 5 : REM increment frame skip count
            IF insertskipcount%<12 insertskipcount%+=1
          WHEN 6 : REM decrement repeat frames
            IF insertfrmrep%>0 insertfrmrep%-=1
          WHEN 7 : REM increment repeat frames
            IF insertfrmrep%<20 insertfrmrep%+=1
          WHEN 8 : REM decrement start animation frame
            IF insertfrmindex%>1 insertfrmindex%-=1
          WHEN 9 : REM increment start animation frame
            IF insertfrmindex%<sprani{(insertset%)}.c% insertfrmindex%+=1
        ENDCASE

        IF C%>0 PROCspriteinsertupdate(0)

      UNTIL done%=1
      PROCWAITMOUSE(0)

      IF insertrepflag%=1 AND insertmode%<>1 THEN
        insertrepeat%=2
        insertphase%=0
      ENDIF

      IF c%=1 THEN
        PROCmenurestore
        PROCmenudraw
      ENDIF

      ENDPROC

      REM ##########################################################
      REM update sprite details dialog to select insert options
      DEF PROCspriteinsertupdate(r%)
      LOCAL done%,BX%,t$,dx%,dy%,dw%,dh%,dt%

      dx%=240
      dy%=300
      dw%=800
      dh%=480
      dt%=dy%+dh%

      IF r%=1 THEN
        GCOL 0,0
        REM RECTANGLE FILL 240,300,800,400
        RECTANGLE FILL dx%,dy%,dw%,dh%

        GCOL 0,15
        RECTANGLE dx%+8,dy%+8,dw%-16,dh%-16
        RECTANGLE dx%+10,dy%+10,dw%-20,dh%-20

        CASE insertmode% OF
          WHEN 0 : REM spr
            t$="Insert Sprite Options"
          WHEN 1 : REM ani
            t$="Insert Animation Options"
          WHEN 2 : REM frm
            t$="Insert Frame Options"
          WHEN 3 : REM text
            t$="Insert Text Options"
        ENDCASE

        REM title
        GCOL 0,8
        RECTANGLE FILL dx%+12,dt%-54,dw%-24,44
        PROCgtext(t$,dx%+16,dt%-20,10,8,0)
        PROCanimcontrol(0,"  GO!  ",dx%+(dw%/2)-120,dy%+72,0,7,14,4)
      ENDIF

      CASE insertmode% OF
        WHEN 0,3 : REM sprite or text
          IF r%=1 THEN
            PROCgtext("Multiple Frames?",dx%+84,dt%-100,7,0,0)
            IF menufrom%=M_moviemode% PROCgtext("Relative To Frame?",dx%+84,dt%-160,7,0,0)
            PROCgtext("Skip Frames:",dx%+36,dt%-220,7,0,0)
            PROCanimcontrol(4,"<",dx%+564,dt%-220,0,7,10,8)
            PROCanimcontrol(5,">",dx%+624,dt%-220,0,7,10,8)
          ENDIF
          PROCdrawcustomspr(2,4+insertrepflag%,dx%+36,dt%-126,10)
          IF menufrom%=M_moviemode% PROCdrawcustomspr(3,4+insertrelflag%,dx%+36,dt%-186,10)
          PROCgtext(RIGHT$("0"+STR$(insertskipcount%),2),dx%+484,dt%-220,11,8,0)

        WHEN 1 : REM ani
          IF r%=1 THEN
            IF menufrom%=M_moviemode% PROCgtext("Relative To Frame?",dx%+84,dt%-100,7,0,0)
            PROCgtext("Skip Frames:",dx%+36,dt%-160,7,0,0)
            PROCanimcontrol(4,"<",dx%+564,dt%-160,0,7,10,8)
            PROCanimcontrol(5,">",dx%+624,dt%-160,0,7,10,8)
            PROCgtext("Frame Repeat:",dx%+36,dt%-220,7,0,0)
            PROCanimcontrol(6,"<",dx%+564,dt%-220,0,7,10,8)
            PROCanimcontrol(7,">",dx%+624,dt%-220,0,7,10,8)
            PROCgtext("First Sprite:",dx%+36,dt%-280,7,0,0)
            PROCanimcontrol(8,"<",dx%+564,dt%-280,0,7,10,8)
            PROCanimcontrol(9,">",dx%+624,dt%-280,0,7,10,8)
          ENDIF
          IF menufrom%=M_moviemode% PROCdrawcustomspr(3,4+insertrelflag%,dx%+36,dt%-126,10)
          PROCgtext(RIGHT$("0"+STR$(insertskipcount%),2),dx%+484,dt%-160,11,8,0)
          PROCgtext(RIGHT$("0"+STR$(insertfrmrep%),2),dx%+484,dt%-220,11,8,0)
          PROCgtext(RIGHT$("0"+STR$(insertfrmindex%),2),dx%+484,dt%-280,11,8,0)

          IF insertani%<>insertfrmindex%-1 OR r%=1 THEN
            insertani%=insertfrmindex%-1
            spritemoving%=sprani{(insertset%)}.s%(insertani%)
            GCOL 0,0
            RECTANGLE FILL dx%+32,dy%+32,86,102
            PROCdrawanimspr(sprani{(insertset%)}.s%(insertani%),dx%+36,dy%+36)
          ENDIF

        WHEN 2 : REM frame
          IF r%=1 THEN
            PROCgtext("Left Column?",dx%+84,dt%-100,7,0,0)
            PROCgtext("Pixel Mode?",dx%+84,dt%-160,7,0,0)
          ENDIF
          PROCdrawcustomspr(2,4+insertrepflag%,dx%+36,dt%-126,10)
          IF menufrom%=M_moviemode% PROCdrawcustomspr(3,4+insertrelflag%,dx%+36,dt%-186,10)

      ENDCASE

      GCOL 0,15
      IF r%=1 THEN
        CASE insertmode% OF
          WHEN 0 : REM spr
            RECTANGLE dx%+30,dy%+30,90,106
            PROCdrawanimspr(spritemoving%,dx%+36,dy%+36)

          WHEN 1 : REM ani
            RECTANGLE dx%+30,dy%+30,90,106

          WHEN 2 : REM frm

          WHEN 3 : REM text

        ENDCASE
      ENDIF


      REM save settings icon
      PROCdrawcustomspr(1,6,dx%+dw%-60,dy%+48,9+insertsave%)

      ENDPROC

      REM ##########################################################
      REM display initial moving sprite
      DEF PROCspritemoveinit(r%)
      LOCAL X%,Y%,YC%,C%,T$

      IF r%=1 THEN
        REM save current mode 7 screen as underlay for moving sprites
        OSCLI "SCREENSAVE """+@tmp$+"M7_TMP.BMP"" 0,0,1280,1000"
        PROCloadbmptobuf(@tmp$+"M7_TMP.BMP",0)
        PROCchangemode(6,0)
        OSCLI "MDISPLAY "+STR$~import_buffer%%
      ELSE
        YC%=ABS(cmWY% MOD 75)
        CLS
        IF cmWX%>-80 AND cmWX%<80 AND cmWY%>-75 AND cmWY%<75 THEN
          OSCLI "MDISPLAY "+STR$~import_buffer%%+" "+STR$(cmWX%*16)+","+STR$(SGN(cmWY%)*sixel{(YC%)}.sdly%*2)
        ENDIF
      ENDIF

      REM restore previous display
      REM OSCLI "DISPLAY """+@tmp$+"M7_TMP.BMP"" 0,0"
      IF menuext%=M_canvasmode% AND r%=1 THEN
        cmWX%=0
        cmWY%=0
      ENDIF

      REM calculate colour and animation frame to display when selecting destination frame
      IF insertrepeat%>0 THEN
        C%=2

        IF insertrepeat%<4 THEN
          IF menuext%=M_moviemode% AND movieframe%=-1 C%=1

          IF insertphase%=1 THEN
            IF menuext%=M_canvasmode% THEN
              IF frame%=spritestartframe% C%=1
              X%=ABS(frame%-spritestartframe%)
            ELSE
              IF movieframe%=objlist{(obj_lstcur%)}.f% C%=1
              X%=ABS(objlist{(obj_lstcur%)}.f%-movieframe%)
            ENDIF

          ENDIF
        ENDIF

        CASE insertrepeat% OF
          WHEN 1 : REM sprite animation
            T$="animation"
            IF insertrepeat%=1 THEN
              IF insertset%>-1 THEN
                insertani%=insertfrmindex%-1
                IF X%>0 THEN
                  REM needs to be fixed... again!!
                  insertani%=(X% DIV (insertfrmrep%+1)+(insertfrmindex%-1)) MOD sprani{(insertset%)}.c%
                ENDIF
                spritemoving%=sprani{(insertset%)}.s%(insertani%)
              ELSE
                insertani%=0
                IF X%>0 THEN
                  insertani%=X% MOD sprlrg_count%
                ENDIF
                spritemoving%=insertani%
              ENDIF
            ENDIF

          WHEN 2 T$="sprite"
          WHEN 3 T$="text"
          WHEN 4 T$="frame"
        ENDCASE

        CASE insertphase% OF
          WHEN 0
            PROCgprint("Select start location of "+T$,120,40,C%)
          WHEN 1
            PROCgprint("Select final location of "+T$,128,40,C%)
        ENDCASE

      ENDIF

      REM Draw initial xor dragged sprite if mouse has moved while holding button
      IF insertrepeat%<4 THEN
        PROCspritemove(0)
        OLD_PX%=PX%
        OLD_PY%=PY%

        REM draw movie frame boundary
        IF menuext%=M_moviemode% AND movieframe%>-1 THEN
          X%=frmlist{(movieframe%)}.x%-mmWX%
          Y%=frmlist{(movieframe%)}.y%-mmWY%

          IF X%>-80 AND X%<80 AND Y%>-75 AND Y%<75 THEN
            YC%=ABS(Y% MOD 75)

            GCOL 0,14
            RECTANGLE X%*16,SGN(Y%)*sixel{(YC%)}.sdly%*2,1278,998
          ENDIF

          REM overlay existing sprite if relocating
          IF spriterelocate%=1 THEN
            PROCobjdraw(objlist{(obj_lstcur%)}.x%-mmWX%+sprlist{(spritemoving%)}.w%,mmWY%-objlist{(obj_lstcur%)}.y%+(sprlist{(spritemoving%)}.h%*3 DIV 2),0,8)
          ENDIF
        ENDIF

        REM draw canvas frame boundary
        IF menuext%=M_canvasmode% THEN
          IF cmWX%>-80 AND cmWX%<80 AND cmWY%>-75 AND cmWY%<75 THEN
            GCOL 0,14
            RECTANGLE cmWX%*16,SGN(cmWY%)*sixel{(YC%)}.sdly%*2,1278,998
          ENDIF

        ENDIF
      ELSE
        PROCspriteposition
        REM insert frames boundary at 0,0
        X%=0-mmWX%
        Y%=0-mmWY%

        IF X%>-80 AND X%<80 AND Y%>-75 AND Y%<75 THEN
          YC%=ABS(Y% MOD 75)

          GCOL 0,14
          RECTANGLE X%*16,SGN(Y%)*sixel{(YC%)}.sdly%*2,1278,998
        ENDIF

      ENDIF

      ENDPROC

      REM ##########################################################
      REM update moving sprite at new location
      DEF PROCspritemove(e%)
      REM erase old sprite and redraw in new mouse pos
      IF e% PROCobjdraw(OLD_PX%,OLD_PY%,3,13)
      PROCspriteposition
      PROCobjdraw(PX%,PY%,3,13)

      ENDPROC

      REM ##########################################################
      REM update sprite plot position
      DEF PROCspriteposition
      LOCAL C%,X%,Y%,ty%,A$,F$

      ty%=994
      C%=10

      IF insertrepeat%<>4 THEN
        CASE insertmode% OF
          WHEN 1,0 : REM sprite, animation
            IF insertlarge%=0 THEN
              X%=PX%-sprlist{(spritemoving%)}.w%
              Y%=PY%-(sprlist{(spritemoving%)}.h%*3 DIV 2)
            ELSE
              X%=PX%
              Y%=PY%
            ENDIF
          WHEN 3 : REM text
            X%=((PX%-(LEN(txtlist$(spritemoving%))+1)) DIV 2)*2
            Y%=(PY% DIV 3)*3
        ENDCASE

        IF menuext%=M_moviemode% THEN
          X%=mmWX%+X%
          Y%=mmWY%-Y%

          IF insertlarge%=0 THEN
            IF sprlist{(spritemoving%)}.m%=1 THEN
              X%=(X% DIV 2)*2
              Y%=(Y% DIV 3)*3
            ENDIF
          ENDIF

          F$=RIGHT$("000"+STR$(movieframe%+1),4)
          IF movieframe%=-1 F$="----"
          A$="F:"+F$+" "
        ELSE
          X%=X%-cmWX%
          Y%=Y%+cmWY%
          F$=RIGHT$("00"+STR$(frame%),3)
          A$="F:"+F$+" "
        ENDIF
      ELSE
        X%=mmWX%
        Y%=mmWY%
      ENDIF

      IF insertrepeat%<>4 THEN
        IF insertrepeat%>0 AND insertphase%=1 THEN
          IF menuext%=M_moviemode% THEN
            FS%=objlist{(obj_lstcount%)}.f%
            REM C%=movieframe%-FS%

            A$=A$+"SX:"+STR$(objlist{(obj_lstcount%)}.x%)+" SY:"+STR$(objlist{(obj_lstcount%)}.y%)
            A$=A$+"  EX:"+STR$(X%)+" EY:"+STR$(Y%)
          ELSE
            FS%=spritestartframe%
            REM C%=frame%-FS%

            A$=A$+"SX:"+STR$(insertstartx%)+" SY:"+STR$(insertstarty%)
            A$=A$+"  EX:"+STR$(X%)+" EY:"+STR$(Y%)
          ENDIF
        ELSE
          A$=A$+"PX:"+STR$(X%)+" PY:"+STR$(Y%)
        ENDIF
      ELSE
        IF insertphase%=1 THEN
          A$=A$+"SX:"+STR$(insertstartx%)+" SY:"+STR$(insertstarty%)
          A$=A$+"  WX:"+STR$(X%)+" WY:"+STR$(Y%)
        ELSE
          A$=A$+"WX:"+STR$(X%)+" WY:"+STR$(Y%)
        ENDIF

      ENDIF

      GCOL 0,8
      RECTANGLE FILL 0,ty%-34,1278,40

      IF menuext%=M_moviemode% AND insertrelflag%=1 AND movieframe%=-1 C%=1

      A%=LEN(A$)*32 DIV 2
      PROCgprint(A$,638-A%,ty%,C%)

      ENDPROC

      REM ##########################################################
      REM copy sprite buffer to frame
      DEF PROCobjtoworldmap
      LOCAL L%,T%,BC%,OC%,X%,Y%,WX%,WY%,WL%,SP%,B%,A$
      IF movieframe%=-1 THEN
        PROCGR_BUF(1,7,0,1)
      ELSE
        PROCGR_BUF(1,frmlist{(movieframe%)}.f%,frmlist{(movieframe%)}.b%,1)
      ENDIF

      REM draw any sprite and frame scenes in current movie mode view
      IF obj_lstcount%>-1 THEN
        FOR L%=0 TO obj_lstcount%
          IF objlist{(L%)}.obj%>-1 THEN
            X%=objlist{(L%)}.x%
            Y%=objlist{(L%)}.y%
            SP%=objlist{(L%)}.obj%
            IF objlist{(L%)}.f%=-1 OR objlist{(L%)}.f%=movieframe% THEN
              CASE objlist{(L%)}.type% OF
                WHEN 1 : REM sprite
                  REM *** adjust repeated sprites world x,y relative to frame wx,wy
                  IF objlist{(L%)}.parent%>-1 THEN
                    WX%=frmlist{(movieframe%)}.x%-frmlist{(objlist{(objlist{(L%)}.parent%)}.f%)}.x%
                    WY%=frmlist{(movieframe%)}.y%-frmlist{(objlist{(objlist{(L%)}.parent%)}.f%)}.y%
                    REM PRINTTAB(0,20)STR$(X%);",";STR$(Y%);"   ";
                    REM PRINTTAB(0,20)STR$(WX%);",";STR$(WY%);"   ";
                    X%+=WX%
                    Y%+=WY%
                  ENDIF

                  IF objlist{(L%)}.rel%=1 THEN
                    WX%=frmlist{(movieframe%)}.x%
                    WY%=frmlist{(movieframe%)}.y%
                    X%=X%+WX%
                    Y%=Y%+WY%
                    REM PRINTTAB(0,20)STR$(X%);",";STR$(Y%);"   ";
                    REM PRINTTAB(0,21)STR$(WX%);",";STR$(WY%);"   ";
                    REM PRINTTAB(0,22)STR$(mmWX%);",";STR$(mmWY%);"   ";
                  ENDIF

                  REM *** separated graphics could use b%=-1
                  IF movieframe%>-1 B%=(frmlist{(movieframe%)}.b%<>0)*-2-(frmlist{(movieframe%)}.f%<0)

                  IF X%>mmWX%-sprlist{(SP%)}.w%*2 AND X%<mmWX%+80 AND Y%<mmWY%+sprlist{(SP%)}.h%*3 AND Y%>mmWY%-75 THEN
                    REM IF objlist{(L%)}.rel%=0 THEN
                    X%=X%-mmWX%
                    Y%=mmWY%-Y%
                    REM ELSE
                    REM X%=X%-frmlist{(movieframe%)}.x%
                    REM Y%=frmlist{(movieframe%)}.y%-Y%
                    REM ENDIF
                    PROCspritetomovbuf(SP%,X%,Y%,B%)

                  ENDIF

                WHEN 2 : REM frame
                  WX%=X%-mmWX%
                  WX%=(WX%+(SGN(WX%)=-1)) DIV 2
                  WY%=mmWY%-Y%
                  WY%=(WY%+(SGN(WY%)=-1)*2) DIV 3

                  IF WX%>-81 AND WX%<80 AND WY%<75 AND WY%>-75 THEN
                    PROCbuffertoframe(SP%,WX%,WY%)
                  ENDIF

                WHEN 3 : REM text
                  A$=CHR$(129+objlist{(L%)}.c%)+txtlist$(SP%)
                  WL%=LEN(A$)-1
                  WY%=mmWY%-Y%
                  REM char mode - accounts for negative DIV causing too many location 0's
                  WY%=(WY%+(SGN(WY%)=-1)*2) DIV 3
                  WX%=X%-mmWX%
                  WX%=(WX%+(SGN(WX%)=-1)) DIV 2

                  IF WY%>0 AND WY%<25 AND WX%+WL%>0 AND WX%<39 THEN
                    FOR T%=0 TO WL%
                      WX%=X%-mmWX%
                      WX%=(WX%+(SGN(WX%)=-1)) DIV 2+T%
                      IF T%=0 AND WX%<1 movie_buffer&((WY%-1)*40)=ASC(MID$(A$,1,1))
                      IF WX%>0 AND WX%<40 THEN
                        movie_buffer&(WX%+(WY%-1)*40)=ASC(MID$(A$,T%+1,1))
                        REM VDU 31,WX%,WY%,ASC(MID$(A$,WX%+1,1))
                      ENDIF
                    NEXT
                  ENDIF

                WHEN 4 : REM large sprite
                  REM *** adjust repeated sprites world x,y relative to frame wx,wy
                  WX%=lrgx%+80
                  WY%=lrgy%
                  IF movieframe%>-1 B%=(frmlist{(movieframe%)}.b%<>0)*-6-(frmlist{(movieframe%)}.f%<0)
                  IF X%>mmWX%-WX% AND X%<mmWX%+80 AND Y%<mmWY%+WY% AND Y%>mmWY%-75 THEN
                    PROClrgsprtomovbuf(SP%,X%-mmWX%,mmWY%-Y%,B%)
                  ENDIF

              ENDCASE
            ENDIF
          ENDIF
        NEXT

        REM add colour codes
        FOR L%=0 TO obj_lstcount%
          IF objlist{(L%)}.obj%>-1 THEN
            X%=objlist{(L%)}.x%
            Y%=objlist{(L%)}.y%
            SP%=objlist{(L%)}.obj%
            IF objlist{(L%)}.f%=-1 OR objlist{(L%)}.f%=movieframe% THEN

              CASE objlist{(L%)}.type% OF
                WHEN 1 : REM sprite
                  REM *** adjust repeated sprites world x,y relative to frame wx,wy
                  IF objlist{(L%)}.parent%>-1 THEN
                    WX%=frmlist{(movieframe%)}.x%-frmlist{(objlist{(objlist{(L%)}.parent%)}.f%)}.x%
                    WY%=frmlist{(movieframe%)}.y%-frmlist{(objlist{(objlist{(L%)}.parent%)}.f%)}.y%
                    REM PRINTTAB(0,20)STR$(X%);",";STR$(Y%);"   ";
                    REM PRINTTAB(0,20)STR$(WX%);",";STR$(WY%);"   ";
                    X%+=WX%
                    Y%+=WY%
                  ENDIF

                  IF objlist{(L%)}.rel%=1 THEN
                    WX%=frmlist{(movieframe%)}.x%
                    WY%=frmlist{(movieframe%)}.y%
                    X%=X%+WX%
                    Y%=Y%+WY%
                    REM PRINTTAB(0,20)STR$(X%);",";STR$(Y%);"   ";
                    REM PRINTTAB(0,21)STR$(WX%);",";STR$(WY%);"   ";
                    REM PRINTTAB(0,22)STR$(mmWX%);",";STR$(mmWY%);"   ";
                  ENDIF

                  IF X%>mmWX%-sprlist{(SP%)}.w%*2 AND X%<mmWX%+80 AND Y%<mmWY%+sprlist{(SP%)}.h%*3 AND Y%>mmWY%-75 THEN
                    B%=0
                    IF movieframe%>-1 B%=ABS(frmlist{(movieframe%)}.b%<>0)*2-(frmlist{(movieframe%)}.f%<0)
                    REM IF objlist{(L%)}.rel%=0 THEN
                    X%=X%-mmWX%
                    Y%=mmWY%-Y%
                    REM ELSE
                    REM X%=X%-frmlist{(movieframe%)}.x%
                    REM Y%=frmlist{(movieframe%)}.y%-Y%
                    REM ENDIF

                    PROCspritecoltomovbuf(SP%,X%,Y%,B%,0)
                  ENDIF

                WHEN 2 : REM frame

                WHEN 3 : REM text
                  A$=CHR$(129+objlist{(L%)}.c%)+txtlist$(SP%)
                  WL%=LEN(A$)-1
                  WY%=mmWY%-Y%
                  REM char mode - accounts for negative DIV causing too many location 0's
                  WY%=(WY%+(SGN(WY%)=-1)*2) DIV 3
                  WX%=X%-mmWX%
                  WX%=(WX%+(SGN(WX%)=-1)) DIV 2

                  IF WY%>0 AND WY%<25 AND WX%<39 AND (WX%+WL%)>0 AND (WX%+WL%)<39 THEN
                    OC%=151
                    IF movieframe%>-1 OC%=frmlist{(movieframe%)}.f%+144
                    FOR T%=0 TO WX%+WL%
                      IF T%<38 THEN
                        BC%=movie_buffer&(T%+(WY%-1)*40)
                        IF ((BC%>128 AND BC%<136) OR (BC%>144 AND BC%<152)) AND BC%<>objlist{(L%)}.c%+129 THEN
                          OC%=BC%
                        ENDIF
                      ENDIF
                    NEXT
                    movie_buffer&(WX%+WL%+1+(WY%-1)*40)=OC%
                  ENDIF
              ENDCASE
            ENDIF
          ENDIF
        NEXT

        REM colour clean up, remove dupes and apply
        WX%=0
        IF movieframe%>-1 WX%=ABS(frmlist{(movieframe%)}.b%<>0)*2-(frmlist{(movieframe%)}.f%<0)
        FOR Y%=0 TO 23
          LC%=movie_buffer&(WX%+Y%*40)
          FOR X%=WX% TO 38
            C%=movie_colbuf&(X%+Y%*40)
            IF C%>32 THEN
              IF C%=LC% THEN
                movie_colbuf&(X%+Y%*40)=32
              ELSE
                LC%=C%
              ENDIF
            ENDIF
          NEXT
        NEXT
        FOR U%=0 TO 959
          IF movie_colbuf&(U%)>32 movie_buffer&(U%)=movie_colbuf&(U%)
        NEXT
      ENDIF

      PROCframerestore(1)
      IF spritemoving%>-1 THEN

      ELSE
        PROCmenudraw
      ENDIF

      IF spriteselect%>-1 THEN
        IF spriteselectold%>-1 PROCupdateselection(spriteselectold%,0)
        spriteselectold%=spriteselect%
        PROCupdateselection(spriteselect%,1)
      ENDIF

      ENDPROC

      REM ##########################################################
      REM update sprite selection box
      DEF PROCupdateselection(sp%,c%)
      LOCAL x1%,x2%,y1%,y2%

      x1%=objlist{(sp%)}.x%-mmWX%
      y1%=mmWY%-objlist{(sp%)}.y%
      IF objlist{(sp%)}.type%=1 THEN
        x2%=x1%+sprlist{(objlist{(sp%)}.obj%)}.w%*2
        y2%=y1%+sprlist{(objlist{(sp%)}.obj%)}.h%*3
      ELSE
        x1%=(x1% DIV 2)*2
        y1%=(y1% DIV 3)*3
        x2%=x1%+LEN(txtlist$(objlist{(sp%)}.obj%))*2+2
        y2%=y1%+3
      ENDIF

      IF x1%<0 x1%=-1
      IF x1%>80 x1%=81
      IF x2%<0 x2%=-1
      IF x2%>80 x2%=81

      REM PRINTTAB(0,20);STR$(y1%);"  ";
      REM PRINTTAB(0,21);STR$(y2%);"  ";
      IF y1%<0 y1%=-1
      IF y1%>74 y1%=500
      IF y2%<0 y2%=-1
      IF y2%>74 y2%=500

      IF y1%>-1 AND y1%<75 y1%=sixel{(y1%)}.sdly% - 2
      IF y2%>-1 AND y2%<75 y2%=sixel{(y2%)}.sdly% + 2

      REM PRINTTAB(0,22);STR$(y1%);"  ";
      REM PRINTTAB(0,23);STR$(y2%);"  ";


      SYS "SDL_SetRenderDrawColor", @memhdc%, 32*c%, 255*c%, 32*c%, 0
      SYS "SDL_RenderDrawLine", @memhdc%, x1%*8-2, y1%, x2%*8+1, y1%
      SYS "SDL_RenderDrawLine", @memhdc%, x1%*8-2, y2%, x2%*8+1, y2%
      SYS "SDL_RenderDrawLine", @memhdc%, x1%*8-2, y1%, x1%*8-2, y2%
      SYS "SDL_RenderDrawLine", @memhdc%, x2%*8+1, y1%, x2%*8+1, y2%

      ENDPROC

      REM ##########################################################
      REM draw font
      DEF PROCdrawfont(x%,y%,text$)
      LOCAL c%
      IF text$<>"" THEN
        LOCAL F%,I%,X%,Y%
        FOR F%=0 TO LEN(text$)-1
          I%=ASC(MID$(text$,F%+1,1))-32
          IF fonts{(I%)}.a%<>0 THEN
            FOR Y%=0 TO fonthgt%-1
              FOR X%=0 TO fonts{(I%)}.w%-1
                c%=fonts{(I%)}.d%(X%+Y%*fonts{(I%)}.w%)()
                IF spr_trns%=1 AND c%=0 THEN
                  REM nothing
                ELSE
                  PROCpoint(X%+x%,y%+fonthgt%-Y%,c%-erase%)
                ENDIF
              NEXT
            NEXT
            x%+=fonts{(I%)}.w%
          ENDIF
        NEXT
      ENDIF
      ENDPROC

      REM ##########################################################
      REM update font variables
      DEF PROCupdatefont(i%,v%,a$)
      COLOUR 11
      CASE i% OF
        WHEN 0 : REM name
          PRINTTAB(6,0)a$
        WHEN 1 : REM font width
          PRINTTAB(11,1)RIGHT$("0"+STR$(v%),2)
        WHEN 2 : REM font height
          PRINTTAB(22,1)RIGHT$("0"+STR$(v%),2)
        WHEN 3 : REM cell x
          PRINTTAB(11,2)RIGHT$("0"+STR$(v%),2)
        WHEN 4 : REM cell y
          PRINTTAB(22,2)RIGHT$("0"+STR$(v%),2)
        WHEN 5 : REM asc val
          PRINTTAB(7,3)RIGHT$("00"+STR$(v%),3)
          COLOUR 10
          PRINTTAB(13,3)CHR$(v%)
      ENDCASE
      ENDPROC

      REM ##########################################################
      REM create font data file from bitmap
      DEF PROCfontcreate
      LOCAL X%,Y%,GX%,GY%,K%,find%,f%,h%,done%
      LOCAL startx%,starty%,gridsx%,gridsy%,line_wid%,box%,px%,py%
      LOCAL fw%,fh%,cx%,cy%,nw%,asc%,bmpx%,bmpy%
      LOCAL ofw%,ofh%,ocx%,ocy%,oasc%,ofs%,col%
      LOCAL name$,a$,asc$
      REM MODE 6 : CHAR 40x25 PIXELS: 640x500 GRAPHICS UNITS: 1280x1000 COLOURS: 16  CHARS: 32X40 GU

      menuext%=M_createfont%
      done%=-1
      text$=""

      REPEAT
        IF done%=-1 THEN
          FOR Y%=6 TO 11
            PROCprint40(Y%,"")
          NEXT

          REM get font name and confirm if already exists
          PRINTTAB(2,8)"ENTER FONT NAME:"
          PRINTTAB(1,9)tb$;CHR$(157);tc$;SPC(14);CHR$(156);
          PRINTTAB(20,9)tb$;CHR$(157);tg$;"CREATE  ";CHR$(156)
          VDU31,3,9
          done%=0
          name$=""
        ENDIF

        PROCREADMOUSE
        IF MB%=4 THEN
          PROCWAITMOUSE(0)
          CASE TY% OF

            WHEN 9 : REM create button
              IF TX%>20 AND TX%<31 AND name$<>"" THEN done%=1

            WHEN 12,13,14,15,16,17,18,19 : REM alphabet selector
              C%=GET(TX%,TY%)
              IF (C%>47 AND C%<58) OR (C%>64 AND C%<91) AND LEN(text$)<10 THEN
                text$=text$+CHR$(C%)
              ENDIF

            WHEN 20 : REM text controls
              CASE TX% OF
                WHEN 37 : REM backspace
                  IF text$<>"" THEN text$=LEFT$(text$,LEN(text$)-1)
                WHEN 39 : REM clear text
                  text$=""
              ENDCASE

            WHEN 22 : REM cancel button
              IF TX%>28 AND TX%<38 THEN done%=2

          ENDCASE
        ELSE

          REM check keyboard for text input
          K%=INKEY(0)
          IF K%>1 THEN

            REM handle specific keypresses
            CASE K% OF
              WHEN 8 : REM backspace
                IF text$<>"" THEN text$=LEFT$(text$,LEN(text$)-1)

              WHEN 13 : REM enter
                IF text$<>"" THEN done%=1

              OTHERWISE
                REM ADD VALID CHARS AND INCREASE TEXT POS
                IF (K%>47 AND K%<58) OR (K%>64 AND K%<91) AND LEN(text$)<10 THEN
                  text$=text$+CHR$(K%)
                ENDIF
            ENDCASE
          ENDIF

          WAIT 2
        ENDIF
        IF text$<>name$ THEN
          name$=text$
          PRINTTAB(4,9)SPC(10);
          PRINTTAB(4,9)name$;
        ENDIF

        REM check if name already exists
        IF done%=1 THEN
          FOR X%=0 TO 31
            IF fontname$(X%)<>"" THEN
              IF name$=fontname$(X%) THEN
                done%=0
                FOR Y%=6 TO 11
                  PROCprint40(Y%,"")
                NEXT
                PRINTTAB(2,8)"FONT NAME ALREADY EXISTS!"
                PRINTTAB(2,9)"OVERWRITE FONT DATA? ";tg$;"Y  ";tr$;"N"
                REPEAT
                  PROCREADMOUSE
                  IF MB%=4 THEN
                    PROCWAITMOUSE(0)
                    IF TY%=9 AND TX%=24 THEN done%=1
                    IF TY%=9 AND TX%=28 THEN done%=-1
                  ELSE
                    WAIT 2
                  ENDIF
                UNTIL done%<>0
                EXIT FOR
              ENDIF
            ELSE
              EXIT FOR
            ENDIF
          NEXT
        ENDIF

      UNTIL done%>0

      text$=""

      REM begin capture if valid name entered
      IF done%=1 THEN
        done%=0

        PROCloadfile(4)

        IF menuext%=95 THEN

          REM adjust for correct byte width multiple of 4
          line_wid%=bmp_imgwid%*3
          WHILE line_wid% MOD 4<>0
            line_wid%+=1
          ENDWHILE

          fw%=10
          fh%=11
          cx%=6
          cy%=5

          asc%=65

          COLOUR 15
          PRINTTAB(0,0)"NAME:"
          PRINTTAB(0,1)"FONT  W: -    +  H: -    +"
          PRINTTAB(0,2)"CELL  X: -    +  Y: -    +"
          PRINTTAB(0,3)"ASC: -     +"

          COLOUR 5
          PRINTTAB(0,4)"SPC: SAVE FONT   S: SKIP ASC"
          PRINTTAB(0,5)"TAB: AUTO WIDTH  F: FIND CELL"

          PROCgtext(" CLOSE ",768,998,11,9,0)

          PROCupdatefont(0,0,name$)
          PROCupdatefont(1,fw%,"")
          PROCupdatefont(2,fh%,"")
          PROCupdatefont(3,cx%,"")
          PROCupdatefont(4,cy%,"")
          PROCupdatefont(5,asc%,"")

          f%=OPENOUT(@dir$+"M7_FONTS/"+name$+".M7F")
          PRINT#f%,"TELEPAINT_FONT"
          CLOSE#f%


          gridsx%=fw%*cx%*2-2
          gridsy%=fh%*cy%*2-2

          REM PROCprint40(0,"Select Sprite: "+RIGHT$("0"+STR$(sprite_cur%+1),2))
          GCOL 0,2
          RECTANGLE FILL 1270-fw%*12,990-fh%*12,fw%*12+8,fh%*12+8

          MX%=(MX% DIV 2)*2
          MY%=(MY% DIV 2)*2
          OLDMX%=MX%
          OLDMY%=MY%

          IF MY%<880 THEN
            GCOL 3,15
            RECTANGLE MX%,MY%,gridsx%,gridsy%
            box%=1
          ENDIF

          REPEAT
            PROCREADMOUSE

            MX%=(MX% DIV 2)*2
            MY%=(MY% DIV 2)*2
            REM start a new selection

            ofw%=fw%
            ofh%=fh%
            ocx%=cx%
            ocy%=cy%
            oasc%=asc%

            IF MY%<880-gridsy% THEN
              REM CTRL & cursor keys update mouse and font capture box
              IF INKEY(-5) THEN
                GX%=2
                GY%=2
              ELSE
                GX%=cx%*2
                GY%=cy%*2
              ENDIF
              REM left arrow
              IF INKEY(-26) AND MX%>0 THEN
                PROCWAITNOKEY(-26,0)
                IF INKEY(-4) THEN
                  IF fw%>1 THEN fw%-=1
                ELSE
                  PROCMOVEMOUSE(-GX%,0)
                ENDIF
              ENDIF
              REM right arrow
              IF INKEY(-122) AND MX%<1278 THEN
                PROCWAITNOKEY(-122,0)
                IF INKEY(-4) THEN
                  IF fw%<20 THEN fw%+=1
                ELSE
                  PROCMOVEMOUSE(GX%,0)
                ENDIF
              ENDIF
              REM up arrow
              IF INKEY(-58) AND MY%<998 THEN
                PROCWAITNOKEY(-58,0)
                IF INKEY(-4) THEN
                  IF fh%<20 THEN fh%+=1
                ELSE
                  PROCMOVEMOUSE(0,GX%)
                ENDIF
              ENDIF
              REM down arrow
              IF INKEY(-42) AND MY%>0 THEN
                PROCWAITNOKEY(-42,0)
                IF INKEY(-4) THEN
                  IF fh%>1 THEN fh%-=1
                ELSE
                  PROCMOVEMOUSE(0,-GX%)
                ENDIF
              ENDIF

              REM F - find next cell
              IF INKEY(-68) THEN
                PROCWAITNOKEY(-68,0)

                REM start new find or next line
                IF find%=0 THEN
                  starty%=bmp_imghgt%-1
                  find%=1
                ELSE
                  starty%=MY% DIV 2
                ENDIF

                bmpy%=starty%
                bmpx%=0
                GX%=0

                REM scan bitmap left to right until pixels found
                REPEAT
                  ofs%=bmp_imgofs%+bmpx%*3+bmpy%*line_wid%
                  col%=import_buffer%%?ofs%+import_buffer%%?(ofs%+1)+import_buffer%%?(ofs%+2)
                  IF col%>0 THEN

                    REM scan a block cx% * cy% to determine if full cell found
                    GY%=0
                    FOR X%=0 TO cx%*cy%-1
                      ofs%=bmp_imgofs%+(bmpx%+X% MOD cx%)*3+(bmpy%-X% DIV cx%)*line_wid%
                      col%=import_buffer%%?ofs%+import_buffer%%?(ofs%+1)+import_buffer%%?(ofs%+2)
                      IF col%=0 THEN
                        bmpx%+=X% MOD cx%
                        bmpy%-=X% DIV cx%
                        EXIT FOR
                      ELSE
                        GY%+=1
                      ENDIF
                    NEXT

                    REM if full cell found, scan down until bottom left of char is found
                    IF GY%=cx%*cy% THEN
                      REPEAT
                        ofs%=bmp_imgofs%+(bmpx%+X% MOD cx%)*3+(bmpy%-X% DIV cx%)*line_wid%
                        col%=import_buffer%%?ofs%+import_buffer%%?(ofs%+1)+import_buffer%%?(ofs%+2)
                        bmpy%-=cy%
                      UNTIL col%=0 OR bmpy%<cy%
                      IF bmpy%>cy% THEN GX%=1 ELSE GX%=2

                    ENDIF
                  ELSE
                    bmpy%-=1
                    IF bmpy%<cy% THEN
                      bmpx%+=1
                      bmpy%=starty%
                      IF bmpx%>bmp_imgwid%-cx% THEN GX%=2
                    ENDIF
                  ENDIF
                UNTIL GX%>0

                REM move mouse to found char
                IF GX%=1 THEN PROCMOVEMOUSE(bmpx%*2-MX%,bmpy%*2-MY%-cy%)

              ENDIF

              REM check for SPACE and save font to data file OR tab for auto width
              K%=0
              IF INKEY(-99) THEN K%=1
              IF INKEY(-97) THEN K%=2
              IF K%<>0 THEN
                PROCWAITNOKEY(-99,0)
                PROCWAITNOKEY(-97,0)

                bmpx%=MX% DIV 2
                bmpy%=MY% DIV 2
                nw%=0

                IF K%=1 THEN
                  REM save current font and step to next one
                  f%=OPENUP(@dir$+"M7_FONTS/"+name$+".M7F")
                  PTR#f%=EXT#f%
                  IF h%=0 THEN
                    PRINT#f%,STR$(fh%)
                    h%=1
                  ENDIF
                  CASE asc% OF
                    WHEN 35 : X%=96
                    WHEN 95 : X%=35
                    WHEN 96 : X%=95

                    OTHERWISE
                      X%=asc%
                  ENDCASE
                  PRINT#f%,STR$(X%)
                  PRINT#f%,STR$(fw%)
                  a$=""
                  FOR Y%=bmpy% TO bmpy%+fh%*cy%-cy% STEP cy%
                    FOR X%=bmpx% TO bmpx%+fw%*cx%-cx% STEP cx%
                      col%=0
                      IF X%>-1 AND X%<bmp_imgwid% AND Y%>-1 AND Y%<bmp_imghgt% THEN
                        ofs%=bmp_imgofs%+X%*3+Y%*line_wid%
                        col%=import_buffer%%?ofs%+import_buffer%%?(ofs%+1)+import_buffer%%?(ofs%+2)
                      ENDIF
                      IF col%>0 THEN
                        a$+="1"
                      ELSE
                        a$+="0"
                      ENDIF
                    NEXT
                  NEXT
                  PRINT#f%,a$

                  CLOSE#f%

                  PROCMOVEMOUSE(fw%*cx%*2,0)
                  X%=bmpx%+fw%*cx%
                ELSE
                  X%=bmpx%
                ENDIF

                REM scan next font and determine font width
                REPEAT
                  GX%=0
                  FOR Y%=bmpy% TO bmpy%+fh%*cy%-cy% STEP cy%
                    col%=0
                    IF X%>-1 AND X%<bmp_imgwid% AND Y%>-1 AND Y%<bmp_imghgt% THEN
                      ofs%=bmp_imgofs%+X%*3+Y%*line_wid%
                      col%=import_buffer%%?ofs%+import_buffer%%?(ofs%+1)+import_buffer%%?(ofs%+2)
                    ENDIF
                    GX%+=col%
                  NEXT
                  X%+=cx%
                  nw%+=1
                UNTIL GX%=0

                fw%=nw%

                IF K%=1 AND asc%<126 THEN
                  asc%+=1
                  PROCupdatefont(5,asc%,"")
                ENDIF
              ENDIF

              REM check for S and skip this char
              IF INKEY(-82) AND asc%<126 THEN
                asc%+=1
                PROCWAITNOKEY(-82,0)
                PROCupdatefont(5,asc%,"")
              ENDIF

            ELSE
              REM menu area
              REM PRINTTAB(0,4)STR$(TX%);"   ";STR$(TY%);"   ";
              IF MB%=4 THEN

                CASE TY% OF
                  WHEN 0 : REM close window
                    done%=1
                  WHEN 1 : REM font width height
                    PROCWAITMOUSE(0)
                    CASE TX% OF
                      WHEN 9
                        IF fw%>1 THEN fw%-=1

                      WHEN 14
                        IF fw%<20 THEN fw%+=1

                      WHEN 20
                        IF fh%>1 THEN fh%-=1

                      WHEN 25
                        IF fh%<20 THEN fh%+=1

                    ENDCASE

                  WHEN 2 : REM cell x,y
                    PROCWAITMOUSE(0)
                    CASE TX% OF
                      WHEN 9
                        IF cx%>1 THEN cx%-=1

                      WHEN 14
                        IF cx%<10 THEN cx%+=1

                      WHEN 20
                        IF cy%>1 THEN cy%-=1

                      WHEN 25
                        IF cy%<10 THEN cy%+=1

                    ENDCASE

                  WHEN 3 : REM asc
                    IF TX%=5 AND asc%>32 THEN
                      asc%-=1
                      WAIT 12
                    ENDIF

                    IF TX%=11 AND asc%<126 THEN
                      asc%+=1
                      WAIT 12
                    ENDIF

                ENDCASE

              ENDIF

            ENDIF

            REM read numberpad to update ascii code
            K%=INKEY(0)
            REM handle specific keypresses
            IF K%>47 AND K%<58 THEN
              COLOUR 12
              IF asc$="" THEN
                IF K%<>50 AND K%<>48 THEN
                  asc$=CHR$(K%)
                  PRINTTAB(15,3)asc$;"  "
                ENDIF
              ELSE
                IF LEN(asc$)=1 THEN
                  CASE asc$ OF
                    WHEN "1" : REM >100
                      IF K%<51 THEN
                        asc$+=CHR$(K%)
                        PRINTTAB(15,3)asc$;"  "
                      ENDIF

                    WHEN "3" : REM 30 something
                      IF K%>49 THEN
                        asc$+=CHR$(K%)
                        asc%=VAL(asc$)
                        PRINTTAB(15,3)asc$;"  "
                        asc$=""
                      ENDIF

                    OTHERWISE : REM 32-99
                      asc$+=CHR$(K%)
                      asc%=VAL(asc$)
                      PRINTTAB(15,3)asc$;"  "
                      asc$=""
                  ENDCASE
                ELSE
                  IF LEFT$(asc$,1)="1" THEN
                    IF K%<55 THEN
                      asc$+=CHR$(K%)
                      asc%=VAL(asc$)
                      PRINTTAB(15,3)asc$;"  "

                      asc$=""
                    ENDIF
                  ENDIF
                ENDIF
              ENDIF
            ENDIF

            REM update any changes
            IF ofw%<>fw% THEN PROCupdatefont(1,fw%,"")
            IF ofh%<>fh% THEN PROCupdatefont(2,fh%,"")
            IF ocx%<>cx% THEN PROCupdatefont(3,cx%,"")
            IF ocy%<>cy% THEN PROCupdatefont(4,cy%,"")
            IF oasc%<>asc% THEN PROCupdatefont(5,asc%,"")

            REM update output window
            IF ofw%<>fw% OR ofh%<>fh% OR ocx%<>cx% OR ocy%<>cy% THEN
              GCOL 0,0
              RECTANGLE FILL 1270-ofw%*12,990-ofh%*12,ofw%*12+8,ofh%*12+8

              GCOL 0,2
              RECTANGLE FILL 1270-fw%*12,990-fh%*12,fw%*12+8,fh%*12+8

            ENDIF

            REM mouse moved or font width changed?
            IF OLDMX%<>MX% OR OLDMY%<>MY% OR ofw%<>fw% OR ofh%<>fh% THEN
              IF box%=1 THEN
                GCOL 3,15
                RECTANGLE OLDMX%,OLDMY%,gridsx%,gridsy%
              ENDIF

              bmpx%=MX% DIV 2
              bmpy%=MY% DIV 2

              REM PRINTTAB(0,1)STR$(x%);"  ";STR$(y%);"   ";

              IF MY%<880-gridsy% THEN
                px%=0
                FOR X%=bmpx% TO bmpx%+fw%*cx%-cx% STEP cx%
                  py%=994-fh%*12
                  FOR Y%=bmpy% TO bmpy%+fh%*cy%-cy% STEP cy%
                    col%=0
                    IF X%>-1 AND X%<bmp_imgwid% AND Y%>-1 AND Y%<bmp_imghgt% THEN
                      ofs%=bmp_imgofs%+X%*3+Y%*line_wid%
                      col%=import_buffer%%?ofs%+import_buffer%%?(ofs%+1)+import_buffer%%?(ofs%+2)
                    ENDIF
                    IF col%>0 THEN
                      GCOL 0,15
                    ELSE
                      GCOL 0,0
                    ENDIF
                    RECTANGLE FILL 1274-fw%*12+px%,py%,12

                    py%+=12
                  NEXT
                  px%+=12
                NEXT

                gridsx%=fw%*cx%*2-2
                gridsy%=fh%*cy%*2-2

                GCOL 3,15
                RECTANGLE MX%,MY%,gridsx%,gridsy%
                box%=1
              ELSE
                REM mouse in menu area
                box%=0
              ENDIF


              OLDMX%=MX%
              OLDMY%=MY%
            ELSE
              WAIT 2
            ENDIF

          UNTIL done%<>0

        ENDIF
        PROCWAITMOUSE(0)
        PROCchangemode(7,1)

      ENDIF
      menuext%=M_keyboard%

      ENDPROC

      REM ##########################################################
      REM UPDATE CLEARSCREEN OPTIONS
      DEF PROCupdateCS
      LOCAL I%

      PRINTTAB(5,5)"FORE  ";CHR$(234);SPC(17);CHR$(181);
      PRINTTAB(5,7)"BACK  ";CHR$(234);SPC(17);CHR$(181);

      IF bakcol%=0 THEN PRINTTAB(13,7)"B"

      FOR I%=1 TO 7
        PRINTTAB(12+I%*2,5)CHR$(144+I%);CHR$(255+(I%=curcol%)*185);
        PRINTTAB(12+I%*2,7)CHR$(144+I%);CHR$(255+(I%=bakcol%)*189);
      NEXT

      PRINTTAB(5,9)"OUTPUT";
      IF bakcol%>0 THEN
        VDU 144+bakcol%,157,144+curcol%
      ELSE
        VDU 32,32,144+curcol%
      ENDIF
      PRINTTAB(14,9)"abcdefghijklmno";CHR$(156);gw$

      ENDPROC

      REM ##########################################################
      REM CLEARSCREEN DIALOG
      DEF PROCclearscreen
      LOCAL I%,L%,A$,B$,C$,cls%,fix%,done%,col_old%,bak_old%,h_old%,v_old%,hindex%,vindex%,skip%,skip_old%,framedupe_old%

      VDU 23,1,0;0;0;0; : REM Disable cursor

      menuext%=99
      cls%=0
      fix%=1
      skip%=3
      skip_old%=3

      PROCWAITMOUSE(0)

      FOR L%=3 TO 22
        PRINTTAB(0,L%)SPC(40);
      NEXT

      PRINTTAB(1,3)tb$;STRING$(11,"-");tg$;"CLEARSCREEN";tb$;STRING$(11,"-");
      FOR L%=4 TO 21
        PRINTTAB(1,L%)gw$ : REM ;CHR$(234);STRING$(32," ");gw$;CHR$(181);
      NEXT


      PRINTTAB(4,11)tb$;"OPTION:  ";tc$;"CLS:";tr$;"N";tc$;" FIX:";tg$;"Y";
      PRINTTAB(4,13)gg$;CHR$(157);tb$;"ALL FRAME  ";CHR$(156);" ";gg$;CHR$(157);tb$;"CUR FRAME  ";CHR$(156)

      PRINTTAB(4,14)tr$;STRING$(29,"-")

      PRINTTAB(4,15)tb$;"SCROLL:  ";tc$;"SKIP : HORZ : VERT"
      A$=STR$(skip%)+" "
      B$=LEFT$(STR$(scrollh%)+" ",2)
      C$=LEFT$(STR$(scrollv%)+" ",2)
      PRINTTAB(13,16)tw$;"-";ty$;A$;tw$;"+ -";ty$;B$;tw$;"+ -";ty$;C$;tw$+"+"

      A$=LEFT$(STR$(framedupe%)+" ",2)
      B$=RIGHT$(" "+STR$(frame_max% DIV framedupe%),2)+"/"+STR$(frame_max%)
      PRINTTAB(4,18)tc$;"FRAMES: ";tw$;"-";ty$;A$;tw$+"+";tb$;"COPIES:";ty$;B$

      REM      PRINTTAB(13,19)tw$+"-"+ty$+A$+tw$+"+"

      PRINTTAB(4,20)gg$;CHR$(157);tb$;"DUPE FRAME  ";CHR$(156);gr$;CHR$(157);ty$;" CANCEL    ";CHR$(156);

      REM      PRINTTAB(1,22)gw$;CHR$(170);STRING$(33,CHR$(172));CHR$(165);
      PRINTTAB(4,21)tr$;STRING$(29,"-")
      PRINTTAB(4,22)gg$;CHR$(157);tb$;" CUSTOM 1   ";CHR$(156);gg$;CHR$(157);tb$;" CUSTOM 2  ";CHR$(156)

      PROCupdateCS

      done%=0
      col_old%=curcol%
      bak_old%=bakcol%
      h_old%=scrollh%
      v_old%=scrollv%
      skip_old%=skip%
      framedupe_old%=framedupe%
      REPEAT
        PROCREADMOUSE
        IF MB%=4 THEN
          PROCWAITMOUSE(0)
          CASE TY% OF
            WHEN 0,1,2 : done%=-1 : REM CANCEL DIALOG
            WHEN 5  : REM FORGROUND COLOUR SELECTOR
              IF TX%>13 AND TX%<28 THEN curcol%=(TX%-12) DIV 2

            WHEN 7 : REM BACKGROUND COLOUR SELECTOR
              IF TX%>11 AND TX%<28 THEN bakcol%=(TX%-12) DIV 2

            WHEN 11  : REM TOGGLE CLS AND FIX
              IF TX%=20 THEN
                cls%=(cls%+1) AND 1
                PRINTTAB(19,11);CHR$(129+cls%);CHR$(78+cls%*11); : REM TOGGLE CLS
              ENDIF
              IF TX%=28 THEN
                fix%=(fix%+1) AND 1
                PRINTTAB(27,11);CHR$(129+fix%);CHR$(78+fix%*11); : REM TOGGLE FIX
              ENDIF

            WHEN 13
              IF TX%>5 AND TX%<19 THEN done%=1 : REM select all frame clearscreen
              IF TX%>22 AND TX%<34 THEN done%=2 : REM select cur frame clearscreen

            WHEN 16
              CASE TX% OF
                WHEN 14 : REM HORIZONTAL DECREMENT
                  skip%-=1
                  IF skip%<1 THEN skip%=1
                WHEN 19 : REM HORIZONTAL INCREMENT
                  skip%+=1
                  IF skip%>5 THEN skip%=5
                WHEN 21 : REM HORIZONTAL DECREMENT
                  scrollh%-=1
                  IF scrollh%<-5 THEN scrollh%=-5
                WHEN 26 : REM HORIZONTAL INCREMENT
                  scrollh%+=1
                  IF scrollh%>5 THEN scrollh%=5
                WHEN 28 : REM VERTICAL DECREMENT
                  scrollv%-=1
                  IF scrollv%<-3 THEN scrollv%=-3
                WHEN 33 : REM VERTICAL INCREMENT
                  scrollv%+=1
                  IF scrollv%>3 THEN scrollv%=3
              ENDCASE

            WHEN 18
              CASE TX% OF
                WHEN 14 : REM frame count decrement
                  IF framedupe%>1 THEN framedupe%-=1
                WHEN 19 : REM frame count increment
                  IF framedupe%<(frame_max% DIV 2) THEN framedupe%+=1
              ENDCASE
              IF framedupe_old%<>framedupe% THEN
                A$=LEFT$(STR$(framedupe%)+" ",2)
                B$=RIGHT$(" "+STR$(frame_max% DIV framedupe%),2)
                PRINTTAB(16,18)A$;
                PRINTTAB(29,18)B$;
                framedupe_old%=framedupe%
              ENDIF

            WHEN 20
              IF TX%>5 AND TX%<20 THEN done%=3 : REM SELECT DUPE SCREEN AND FINISH
              IF TX%>23 AND TX%<34 THEN done%=-1 : REM CANCEL SCLEARSCREEN DIALOG
            WHEN 22
              IF TX%>5 AND TX%<20 THEN done%=4 : REM CUSTOM PROCEDURE 1
              IF TX%>23 AND TX%<34 THEN done%=5 : REM CUSTOM PROCEDURE 2

          ENDCASE
          IF col_old%<>curcol% OR bak_old%<>bakcol% THEN
            PROCupdateCS
            col_old%=curcol%
            bak_old%=bakcol%

          ENDIF
          IF skip_old%<>skip% THEN
            A$=LEFT$(STR$(skip%)+" ",2)
            PRINTTAB(16,16)A$;
            skip_old%=skip%
          ENDIF
          IF h_old%<>scrollh% THEN
            A$=LEFT$(STR$(scrollh%)+" ",2)
            PRINTTAB(23,16)A$;
            h_old%=scrollh%
          ENDIF
          IF v_old%<>scrollv% THEN
            A$=LEFT$(STR$(scrollv%)+" ",2)
            PRINTTAB(30,16)A$;
            v_old%=scrollv%
          ENDIF


        ENDIF
      UNTIL done%

      PROCWAITMOUSE(0)

      PROCmenurestore
      REM *** DRAWFRAME??

      CASE done% OF
        WHEN 1: REM new background / foreground all frames
          PROCundosaveall
          PROCGR(curcol%,bakcol%,cls%)
          IF cls% THEN
            FOR frame%=1 TO frame_max%
              PROCframesave(frame%)
            NEXT frame%
            frame%=1
          ELSE
            PROCframesave(1)
            frame%=1

            FOR I%=2 TO frame_max%
              PROCGR_BUF(I%,curcol%,bakcol%,0)
            NEXT

          ENDIF
          IF fix%=1 AND bakcol%>0 THEN
            xMin%=5
            fxMin%=6
          ELSE
            xMin%=1
            fxMin%=2
          ENDIF

        WHEN 2: REM new background / foreground cur frame
          PROCundosave
          PROCGR(curcol%,bakcol%,cls%)
          PROCframesave(frame%)

        WHEN 3: REM DUPLICATE FRAME 1
          PROCframesave(1)
          frame%=0
          PROCloadnextframe(1,0)
          hindex%=scrollh%
          vindex%=scrollv%
          IF framedupe%=1 THEN
            FOR frame%=2 TO frame_max%
              IF scrollh%<>0 OR scrollv%<>0 THEN
                PROCcopyframe(1,frame%,hindex%,vindex%,skip%)

                hindex%+=scrollh%
                IF hindex%>39 THEN hindex%=hindex%-40
                IF hindex%<0 THEN hindex%=40+hindex%

                vindex%+=scrollv%
                IF vindex%>23 THEN vindex%=vindex%-24
                IF vindex%<0 THEN vindex%=24+vindex%

              ELSE
                PROCframesave(frame%)
              ENDIF
            NEXT frame%
            frame%=1
          ELSE
            fix%=frame_max% DIV framedupe%
            FOR I%=1 TO framedupe%
              FOR L%=1 TO fix%-1
                PROCcopyframe(I%,I%+framedupe%*L%,0,0,skip%)
              NEXT
            NEXT
          ENDIF
        WHEN 4: REM CALL CUSTOM PROCEDURE 1
          PROCundosave
          PROCCUSTOMPROC1
          PROCframesave(frame%)

        WHEN 5: REM CALL CUSTOM PROCEDURE 2
          PROCundosave
          PROCCUSTOMPROC2
          PROCframesave(frame%)

      ENDCASE

      VDU 23,1,1;0;0;0;  : REM Enable cursor

      ENDPROC

      REM ##########################################################
      REM SAVE FRAME BUFFER
      DEF PROCframesave(f%)
      LOCAL U%

      FOR U%=0 TO 959
        frame_buffer&(f%-1,U%)=GET(U% MOD 40,U% DIV 40+1)
      NEXT

      IF gridshow%=1 THEN PROCdrawgrid

      ENDPROC

      REM ##########################################################
      REM RESTORE FRAME BUFFER
      DEF PROCframerestore(f%)
      LOCAL U%
      IF menuext%<>M_moviemode% THEN
        FOR U%=0 TO 959
          VDU 31,(U% MOD 40),(U% DIV 40+1),frame_buffer&(f%-1,U%)
        NEXT
      ELSE
        FOR U%=0 TO 959
          VDU 31,(U% MOD 40),(U% DIV 40+1),movie_buffer&(U%)
        NEXT
      ENDIF
      ENDPROC

      REM ##########################################################
      REM COPY A FRAME STARTING FROM OFFSET
      DEF PROCcopyframe(S%,D%,H%,V%,skip%)
      LOCAL X%,Y%,xofs%,yofs%

      FOR X%=skip% TO 39
        REMIF X%>skip% THEN
        xofs%=X%+H%
        IF xofs%<0 THEN xofs%=40+xofs%
        IF xofs%>39 THEN xofs%=xofs%-40
        IF xofs%>skip%-1 THEN
          FOR Y%=0 TO 23
            yofs%=Y%+V%
            IF yofs%<0 THEN yofs%=24+yofs%
            IF yofs%>23 THEN yofs%=yofs%-24

            frame_buffer&(D%-1,X%+Y%*40)=frame_buffer&(S%-1,xofs%+yofs%*40)
          NEXT
        ENDIF
      NEXT

      ENDPROC

      REM ##########################################################
      REM load next frame from buffer and display it
      DEF PROCloadnextframe(F%,S%)

      IF spritemoving%>-1 PROCchangemode(7,1)

      IF S%<>0 AND spritemoving%=-1 THEN
        PROCWAITMOUSE(0)
        PROCframesave(frame%)
      ENDIF

      frame%+=F%
      IF frame%>frame_max% THEN frame%=1
      IF frame%<1 THEN frame%=frame_max%
      PROCframerestore(frame%)
      PROCmenudraw

      IF spritemoving%>-1 PROCspritemoveinit(1)

      REM reset text cursor
      TEXTX%=TX%
      FONTX%=PX%

      ENDPROC

      REM ##########################################################
      REM copy sprite buffer to frame
      DEF PROCspritetoframe(f%,s%,sx%,sy%)
      LOCAL S%,U%,X%,Y%
      FOR U%=0 TO 319
        X%=sx%+U% MOD 20
        Y%=sy%+U% DIV 20
        IF X%>0 AND X%<40 AND Y%>0 AND Y%<25 THEN
          S%=sprbuf&(s%,U%)
          IF spr_trns%=1 THEN
            IF S%<>32 AND S%<>160 THEN frame_buffer&(f%-1,X%+(Y%-1)*40)=S%
          ELSE
            frame_buffer&(f%-1,X%+(Y%-1)*40)=S%
          ENDIF
        ENDIF
      NEXT
      ENDPROC

      REM ##########################################################
      REM copy sprite buffer to frame
      DEF PROCbuffertoframe(f%,sx%,sy%)
      LOCAL D%,S%,U%,X%,Y%,C%,CS%
      FOR U%=0 TO 959
        X%=sx%+U% MOD 40
        Y%=sy%+U% DIV 40
        IF U% MOD 40=0 THEN
          C%=-1
          CS%=0
        ENDIF

        S%=frame_buffer&(f%,U%)
        IF S%>144 AND S%<152 AND CS%=0 THEN
          C%=S%
        ENDIF

        IF X%>0 AND X%<40 AND Y%>0 AND Y%<25 THEN
          IF CS%=0 AND C%<>-1 THEN
            movie_buffer&((Y%-1)*40)=C%
            CS%=1
          ENDIF
          IF spr_trns%=1 THEN
            REM IF S%<>32 AND S%<>160 THEN movie_buffer&(X%+(Y%-1)*40)=S%
            IF S%<>32 THEN
              D%=0
              IF S%>159 D%=movie_buffer&(X%+(Y%-1)*40)

              movie_buffer&(X%+(Y%-1)*40)=S% OR D%
            ENDIF
          ELSE
            movie_buffer&(X%+(Y%-1)*40)=S%
          ENDIF
        ENDIF
      NEXT
      ENDPROC

      REM ##########################################################
      REM copy sprite buffer to movie frame
      DEF PROCspritetomovbuf(s%,sx%,sy%,b%)
      LOCAL M%,X%,Y%,YC%,XC%,SW%,SH%,S%,U%,SX%,SY%

      IF sprlist{(s%)}.m%=0 THEN
        REM pixel mode
        SH%=sprlist{(s%)}.h%*3-1
        SW%=sprlist{(s%)}.w%*2-1

        FOR Y%=0 TO SH%
          YC%=sy%+Y%
          IF YC%>=3 AND YC%<75 THEN
            FOR X%=0 TO SW%
              XC%=sx%+X%
              IF XC%>=(b%+1)*2 AND XC%<80 THEN
                C%=FNpoint_sprbuf(X%,Y%,s%)
                IF C% PROCpoint_movbuf(XC%, YC%, 1)
              ENDIF
            NEXT
          ENDIF
        NEXT
      ELSE
        REM char mode - accounts for negative DIV causing too many location 0's
        SX%=(sx%+(SGN(sx%)=-1)) DIV 2
        SY%=(sy%+(SGN(sy%)=-1)*2) DIV 3

        FOR U%=0 TO 319
          XC%=U% MOD 20
          YC%=U% DIV 20
          IF XC%<sprlist{(s%)}.w% AND YC%<sprlist{(s%)}.h% THEN
            X%=SX%+XC%
            Y%=SY%+YC%
            IF Y%>0 AND Y%<25 THEN
              S%=sprbuf&(s%,U%)
              IF S%>144 AND S%<152 AND X%<=b% THEN movie_buffer&(b%+(Y%-1)*40)=S%
              IF X%>b% AND X%<40 THEN
                M%=movie_buffer&(X%+(Y%-1)*40)
                IF spr_trns%=1 THEN
                  IF S%<>32 AND S%<>160 THEN
                    IF M%>144 AND M%<152 THEN
                    ELSE
                      movie_buffer&(X%+(Y%-1)*40)=S%
                    ENDIF
                  ENDIF
                ELSE
                  IF M%>144 AND M%<152 THEN
                  ELSE
                    movie_buffer&(X%+(Y%-1)*40)=S%
                  ENDIF

                ENDIF
              ENDIF
            ENDIF
          ENDIF
        NEXT

      ENDIF

      ENDPROC

      REM ##########################################################
      REM copy sprite buffer to movie frame
      DEF PROClrgsprtomovbuf(s%,sx%,sy%,b%)
      LOCAL X%,Y%,YC%,XC%,S%,U%,SX%,SY%

      S%=0

      FOR X%=0 TO lrgx%
        XC%=sx%+X%
        FOR Y%=lrgy% TO 0 STEP -1
          YC%=sy%+Y%
          IF XC%>=b% AND XC%<80 AND YC%>=3 AND YC%<75 THEN
            C%=sprlrg&(s%,S%)
            IF C% PROCpoint_movbuf(XC%, YC%, 1)
          ENDIF
          S%+=1
        NEXT
      NEXT

      ENDPROC


      REM ##########################################################
      REM copy sprite colours to movie frame
      DEF PROCspritecoltomovbuf(s%,sx%,sy%,b%,f%)
      LOCAL SC%,FC%,Y%,CX1%,CX2%,CY1%,CY2%,SH%,SW%

      IF sprlist{(s%)}.m%=0 THEN
        SC%=sprbuf&(s%,0)
        IF SC%>144 AND SC%<152 THEN
          CX1%=sx% DIV 2
          CX2%=CX1%+sprlist{(s%)}.w%+(sx% MOD 2)
          REM CY1%=sy% DIV 3-1
          REM CY2%=CY1%+sprlist{(s%)}.h%+(sy% MOD 3=0)
          CY1%=-1
          CY2%=-1

          SH%=sprlist{(s%)}.h%*3-1
          SW%=sprlist{(s%)}.w%*2-1

          REM define sprite plot boundary
          FOR Y%=0 TO SH%
            YC%=sy%+Y%
            IF YC%>=3 AND YC%<75 THEN
              FOR X%=0 TO SW%
                XC%=sx%+X%
                IF XC%>=b%*2 AND XC%<80 THEN
                  IF FNpoint_sprbuf(X%,Y%,s%) THEN
                    IF CY1%=-1 CY1%=YC%
                    CY2%=YC%
                    CX2%=XC%
                  ENDIF
                ENDIF
              NEXT
            ENDIF
          NEXT

          REM clean up start and end locations and apply colour
          IF CY1%<>-1 OR CY2%<>-1 THEN
            CY1%=CY1% DIV 3-1
            CY2%=CY2% DIV 3-1
            CX2%=CX2% DIV 2

            IF CX1%<b% CX1%=b%
            IF CX2%<b% CX2%=b%
            REM IF CX1%>39 CX1%=39
            REM IF CX2%>40 CX2%=40
            IF CY1%<0 CY1%=0
            IF CY2%<0 CY2%=0
            IF CY1%>23 CY1%=23
            IF CY2%>23 CY2%=23
            FC%=151
            IF movieframe%>-1 FC%=frmlist{(movieframe%)}.f%+144

            IF CX1%<39 AND CX2%>0 THEN
              FOR Y%=CY1% TO CY2%
                IF f%=0 THEN
                  movie_colbuf&(CX1%+Y%*40)=SC%
                ELSE
                  REM frame_buffer&(f%-1,CX1%+Y%*40)=SC%
                  VDU 31,CX1%,Y%+1,SC%
                ENDIF

                REM IF CX2%<40 movie_buffer&(CX2%+Y%*40)=FC%

                REM IF CX1%<39 movie_buffer&(CX1%+Y%*40)=67
                REM IF CX2%<40 movie_buffer&(CX2%+Y%*40)=66

              NEXT
            ENDIF
          ENDIF
        ENDIF
      ENDIF
      ENDPROC

      REM ##########################################################
      REM copy sprite buffer to movie frame
      DEF PROCspritetocanvas(s%,sx%,sy%)
      LOCAL D%,S%,U%,X%,Y%,SW%,SH%,LC%,FC%,SC%,XC%,YC%,CX1%,CX2%

      b%=0
      SH%=sprlist{(s%)}.h%*3-1
      SW%=sprlist{(s%)}.w%*2-1

      REM pixel mode
      IF sprlist{(s%)}.m%=0 THEN
        REM define and apply colour and end colour
        SC%=sprbuf&(s%,0)
        IF SC%>144 AND SC%<152 THEN

          REM define sprite colour boundary
          CX1%=40
          CX2%=-1

          REM define sprite plot boundary
          MY%=-1
          FOR Y%=0 TO SH%
            YC%=sy%+Y%
            IF YC%>2 AND YC%<75 THEN

              REM scan sprite line to define left and right boundary
              FOR X%=0 TO SW%
                XC%=(sx%+X%) DIV 2
                IF FNpoint_sprbuf(X%,Y%,s%) THEN
                  IF XC%<CX1% CX1%=XC%
                  IF XC%>CX2% CX2%=XC%
                ENDIF
              NEXT

              REM apply colours for current char row once all pixel rows scanned
              IF (YC%+1) MOD 3=0 OR Y%=SH% THEN
                MY%=YC% DIV 3
                REM apply colour if mask is in range
                IF CX2%>b% AND CX1%<40 THEN
                  IF CX1%<=b% THEN CX1%=b% ELSE CX1%-=1
                  CX2%+=1

                  REM apply end colour, scan backwards until another colour is found
                  IF CX2%<40 THEN
                    FC%=FNgetleftcol(CX2%,MY%)
                    IF FC%<>SC% VDU 31,CX2%,MY%,FC%
                  ENDIF

                  REM apply start colour, scan backwards until another colour is found
                  IF CX1%>-1 THEN
                    FC%=FNgetleftcol(CX1%,MY%)
                    IF FC%<>SC% VDU 31,CX1%,MY%,SC%
                  ENDIF

                  REM blank out spaces between colours
                  XC%=CX2%-1
                  IF XC%>39 XC%=39
                  XC%=XC%-CX1%
                  PRINTTAB(CX1%+1,MY%)SPC(XC%);
                ENDIF
                CX1%=40
                CX2%=-1

              ENDIF
            ENDIF
          NEXT
        ENDIF

        REM plot pixels
        FOR Y%=0 TO SH%
          YC%=sy%+Y%
          IF YC%>2 AND YC%<75 THEN
            FOR X%=0 TO SW%
              XC%=sx%+X%
              IF XC%>=0 AND XC%<80 THEN
                C%=FNpoint_sprbuf(X%,Y%,s%)
                IF C% PROCpoint(XC%, YC%, 1)
              ENDIF
            NEXT
          ENDIF
        NEXT

      ELSE
        REM define sprite colour boundary
        CX1%=40
        CX2%=-1

        REM define sprite plot boundary
        MY%=-1
        FOR Y%=0 TO SH%
          YC%=sy%+Y%
          IF YC%>2 AND YC%<75 THEN

            REM scan sprite line to define left and right boundary
            FOR X%=0 TO SW%
              XC%=(sx%+X%) DIV 2
              IF FNpoint_sprbuf(X%,Y%,s%) THEN
                IF XC%<CX1% CX1%=XC%
                IF XC%>CX2% CX2%=XC%
              ENDIF
            NEXT

            REM apply colours for current char row once all pixel rows scanned
            IF (YC%+1) MOD 3=0 OR Y%=SH% THEN
              MY%=YC% DIV 3
              REM apply colour if mask is in range
              IF CX2%>b% AND CX1%<40 THEN
                IF CX1%<=b% THEN CX1%=b%
                CX2%+=1

                REM apply end colour, scan backwards until another colour is found
                IF CX2%<40 THEN
                  FC%=FNgetleftcol(CX2%,MY%)
                  IF FC%<>SC% VDU 31,CX2%,MY%,FC%
                ENDIF

                REM apply start colour, scan backwards until another colour is found
                IF CX1%>-1 THEN
                  REM FC%=FNgetleftcol(CX1%,MY%)
                  REM IF FC%<>SC% VDU 31,CX1%,MY%,SC%
                ENDIF

                REM blank out spaces between colours
                XC%=CX2%
                IF XC%>39 XC%=39
                XC%=XC%-CX1%
                PRINTTAB(CX1%,MY%)SPC(XC%);
              ENDIF
              CX1%=40
              CX2%=-1

            ENDIF
          ENDIF
        NEXT

        REM plot pixels
        FOR Y%=0 TO SH%
          YC%=sy%+Y%
          IF YC%>2 AND YC%<75 THEN
            FOR X%=0 TO SW%
              XC%=sx%+X%
              IF XC%>=0 AND XC%<80 THEN
                C%=FNpoint_sprbuf(X%,Y%,s%)
                IF C% PROCpoint(XC%, YC%, 1)
              ENDIF
            NEXT
          ENDIF
        NEXT

        REM char mode
        SX%=(sx%+(SGN(sx%)=-1)) DIV 2
        SY%=(sy%+(SGN(sy%)=-1)*2) DIV 3

        REM plot codes
        FOR U%=0 TO 319
          XC%=U% MOD 20
          YC%=U% DIV 20
          IF XC%<sprlist{(s%)}.w% AND YC%<sprlist{(s%)}.h% THEN
            X%=SX%+XC%
            Y%=SY%+YC%
            S%=sprbuf&(s%,U%)
            IF S%>144 AND S%<152 THEN
              IF X%<b% THEN X%=b%
              IF X%>-1 AND X%<40 AND Y%>0 AND Y%<25 THEN
                VDU 31,X%,Y%,S%
              ENDIF
            ENDIF
          ENDIF
        NEXT
      ENDIF
      ENDPROC

      REM ##########################################################
      REM scan a row to the left until a colour code is found
      DEFFNgetleftcol(X%,Y%)
      LOCAL FC%,LC%

      FC%=0
      REPEAT
        LC%=GET(X%,Y%)
        IF LC%>144 AND LC%<152 FC%=LC%
        X%-=1
      UNTIL X%<0 OR FC%<>0
      IF FC%=0 FC%=151
      =FC%

      REM ##########################################################
      REM draw sprite
      DEF PROCupdatespritesize(S%,D%)
      LOCAL U%,W%,H%,C%

      W%=0
      H%=0

      REM reset colour of pix mode sprites
      IF sprlist{(S%)}.m%=0 AND sprbuf&(S%,0)>144 AND sprbuf&(S%,0)<152 THEN
        C%=sprbuf&(S%,0)
        FOR Y%=0 TO 15
          sprbuf&(S%,Y%*20)=32
        NEXT
      ENDIF

      REM 20x16 chars @320 bytes : 40x48 pixels @1920 bytes
      FOR U%=0 TO 319
        IF sprbuf&(S%,U%)<>32 AND sprbuf&(S%,U%)<>160 THEN
          IF U% MOD 20>W% W%=U% MOD 20
          IF U% DIV 20>H% H%=U% DIV 20
        ENDIF
      NEXT

      sprlist{(S%)}.w%=W%+1
      sprlist{(S%)}.h%=H%+1

      IF sprlist{(S%)}.m%=0 AND C%>144 AND C%<152 THEN
        FOR Y%=0 TO sprlist{(S%)}.h%-1
          sprbuf&(S%,Y%*20)=C%
        NEXT
      ENDIF

      IF D% THEN
        PRINTTAB(14,22)RIGHT$("0"+STR$(sprlist{(S%)}.w%),2);
        PRINTTAB(20,22)RIGHT$("0"+STR$(sprlist{(S%)}.h%),2);
      ENDIF

      ENDPROC

      REM ##########################################################
      REM save sprite
      DEF PROCspritecommit(S%)
      LOCAL U%
      REM 20x16 chars @320 bytes : 40x48 pixels @1920 bytes

      FOR U%=0 TO 319
        sprbuf&(S%,U%)=GET(U% MOD 20+10,U% DIV 20+3)
      NEXT

      PROCupdatespritesize(S%,1)

      PROCdrawgrid

      ENDPROC

      REM ##########################################################
      REM draw sprite
      DEF PROCspriteredraw
      LOCAL U%

      REM 20x16 chars @320 bytes : 40x48 pixels @1920 bytes
      FOR U%=0 TO 319
        VDU 31,U% MOD 20+10,U% DIV 20+3,sprbuf&(sprite_cur%,U%)
      NEXT

      PROCdrawgrid

      ENDPROC

      REM ##########################################################
      REM save current screen / sprite to undo buffer
      DEF PROCundosave
      LOCAL U%
      CASE menuext% OF
        WHEN M_canvasmode% : REM main canvas
          IF undo_count&(frame%-1)<undo_max% THEN undo_count&(frame%-1)+=1

          FOR U%=0 TO 959
            undo_buffer&(frame%-1,undo_index%(frame%-1),U%)=GET(U% MOD 40,U% DIV 40+1)
          NEXT

          undo_index%(frame%-1)+=1
          IF undo_index%(frame%-1)>undo_max% THEN undo_index%(frame%-1)=0

          redo_count&(frame%-1)=0

        WHEN M_sprites% : REM sprites screen
          IF spr_undo_count&(sprite_cur%)<undo_max% THEN spr_undo_count&(sprite_cur%)+=1

          FOR U%=0 TO 319
            spr_undo_buffer&(sprite_cur%,spr_undo_index%(sprite_cur%),U%)=GET(U% MOD 20+10,U% DIV 20+3)
          NEXT

          spr_undo_index%(sprite_cur%)+=1
          IF spr_undo_index%(sprite_cur%)>undo_max% THEN spr_undo_index%(sprite_cur%)=0

          spr_redo_count&(sprite_cur%)=0

      ENDCASE
      PROCmenudraw

      ENDPROC

      REM ##########################################################
      REM save all screens to undo buffer
      DEF PROCundosaveall
      LOCAL U%,F%

      FOR F%=0 TO frame_max%-1

        IF undo_count&(F%)<undo_max% THEN undo_count&(F%)+=1

        FOR U%=0 TO 959
          undo_buffer&(F%,undo_index%(F%),U%)=frame_buffer&(F%,U%)
        NEXT

        undo_index%(F%)+=1
        IF undo_index%(F%)>undo_max% THEN undo_index%(F%)=0

        redo_count&(F%)=0
      NEXT

      ENDPROC

      REM ##########################################################
      REM restore all screens from undo buffer
      DEF PROCundorestoreall
      LOCAL U%,F%

      FOR F%=0 TO frame_max%-1
        IF undo_count&(F%)>0 THEN
          undo_count&(F%)-=1

          undo_index%(F%)-=1
          IF undo_index%(F%)<0 THEN undo_index%(F%)=undo_max%

          FOR U%=0 TO 959
            frame_buffer&(F%,U%)=undo_buffer&(F%,undo_index%(F%),U%)
          NEXT
        ENDIF

      NEXT

      ENDPROC


      REM ##########################################################
      REM restore current undo buffer to screen / sprite
      DEF PROCundorestore
      LOCAL U%
      CASE menuext% OF
        WHEN M_canvasmode% : REM main canvas
          IF undo_count&(frame%-1)>0 THEN
            undo_count&(frame%-1)-=1

            undo_index%(frame%-1)-=1
            IF undo_index%(frame%-1)<0 THEN undo_index%(frame%-1)=undo_max%

            PROCredosave
            FOR U%=0 TO 959
              VDU 31,(U% MOD 40),(U% DIV 40+1),undo_buffer&(frame%-1,undo_index%(frame%-1),U%)
            NEXT
            PROCframesave(frame%)
          ENDIF
        WHEN M_sprites% : REM sprite screen
          IF spr_undo_count&(sprite_cur%)>0 THEN
            spr_undo_count&(sprite_cur%)-=1

            spr_undo_index%(sprite_cur%)-=1
            IF spr_undo_index%(sprite_cur%)<0 THEN spr_undo_index%(sprite_cur%)=undo_max%

            PROCredosave
            FOR U%=0 TO 319
              VDU 31,U% MOD 20+10,U% DIV 20+3,spr_undo_buffer&(sprite_cur%,spr_undo_index%(sprite_cur%),U%)
            NEXT
            PROCspritecommit(sprite_cur%)
          ENDIF

      ENDCASE

      ENDPROC

      REM ##########################################################
      REM save redo screen / sprite
      DEF PROCredosave
      LOCAL U%

      CASE menuext% OF
        WHEN M_canvasmode% : REM main canvas
          IF redo_count&(frame%-1)<undo_max% THEN redo_count&(frame%-1)+=1

          FOR U%=0 TO 959
            redo_buffer&(frame%-1,redo_index%(frame%-1),U%)=GET(U% MOD 40,U% DIV 40+1)
          NEXT

          redo_index%(frame%-1)+=1
          IF redo_index%(frame%-1)>undo_max% THEN redo_index%(frame%-1)=0

        WHEN M_sprites% : REM sprite screen
          IF spr_redo_count&(sprite_cur%)<undo_max% THEN spr_redo_count&(sprite_cur%)+=1

          FOR U%=0 TO 319
            spr_redo_buffer&(sprite_cur%,spr_redo_index%(sprite_cur%),U%)=GET(U% MOD 20+10,U% DIV 20+3)
          NEXT

          spr_redo_index%(sprite_cur%)+=1
          IF spr_redo_index%(sprite_cur%)>undo_max% THEN spr_redo_index%(sprite_cur%)=0

      ENDCASE

      ENDPROC

      REM ##########################################################
      REM restore redo screen
      DEF PROCredorestore
      LOCAL U%

      CASE menuext% OF
        WHEN M_canvasmode% : REM main canvas
          IF redo_count&(frame%-1)>0 THEN
            redo_count&(frame%-1)-=1

            redo_index%(frame%-1)-=1
            IF redo_index%(frame%-1)<0 THEN redo_index%(frame%-1)=undo_max%

            FOR U%=0 TO 959
              VDU 31,(U% MOD 40),(U% DIV 40+1),redo_buffer&(frame%-1,redo_index%(frame%-1),U%)
            NEXT
            PROCframesave(frame%)

            undo_index%(frame%-1)+=1
            IF undo_index%(frame%-1)>undo_max% THEN undo_index%(frame%-1)=0
            IF undo_count&(frame%-1)<undo_max% THEN undo_count&(frame%-1)+=1

          ENDIF
        WHEN M_sprites% : REM sprite screen
          IF spr_redo_count&(sprite_cur%)>0 THEN
            spr_redo_count&(sprite_cur%)-=1

            spr_redo_index%(sprite_cur%)-=1
            IF spr_redo_index%(sprite_cur%)<0 THEN spr_redo_index%(sprite_cur%)=undo_max%

            FOR U%=0 TO 319
              VDU 31,U% MOD 20+10,U% DIV 20+3,spr_redo_buffer&(sprite_cur%,spr_redo_index%(sprite_cur%),U%)
            NEXT
            PROCspritecommit(sprite_cur%)

            spr_undo_index%(sprite_cur%)+=1
            IF spr_undo_index%(sprite_cur%)>undo_max% THEN spr_undo_index%(sprite_cur%)=0
            IF spr_undo_count&(sprite_cur%)<undo_max% THEN spr_undo_count&(sprite_cur%)+=1

          ENDIF

      ENDCASE

      ENDPROC

      REM ##########################################################
      REM selct region of current frame and call copyregion
      DEF PROCselectregion
      LOCAL X%,Y%,SX%,SY%,OX%,OY%,boxx%,boxy%,C$

      PROCcontrolcodes(0,0)
      menuext%=78

      REM get start char location
      SX%=TX%
      SY%=TY%

      boxx%=SX%*32
      boxy%=(25-SY%)*40
      X%=32
      Y%=-40
      VDU 4
      GCOL 3,15
      RECTANGLE boxx%,boxy%,X%,Y%

      C$="S:"+RIGHT$("0"+STR$(SX%),2)+","+RIGHT$("0"+STR$(SY%),2) + " E:00,00 "
      PROCgtext(C$,0,996,14,4,0)


      REPEAT
        PROCREADMOUSE
        IF (TX%<>SX% OR TY%<>SY%) AND TY%>0 THEN
          GCOL 3,15
          RECTANGLE boxx%,boxy%,X%,Y%
          SX%=TX%
          SY%=TY%
          boxx%=SX%*32
          boxy%=(25-SY%)*40

          RECTANGLE boxx%,boxy%,X%,Y%

          C$="S:"+RIGHT$("0"+STR$(SX%),2)+","+RIGHT$("0"+STR$(SY%),2) + " E:00,00 " REM +" E:"+RIGHT$("0"+STR$(TX%),2)+","+RIGHT$("0"+STR$(TY%),2)
          PROCgtext(C$,0,996,14,4,0)

        ELSE
          WAIT 2
        ENDIF
      UNTIL MB%=4

      OX%=TX%
      OY%=TY%

      REPEAT
        PROCREADMOUSE
        IF TX%<0 TX%=0
        IF TX%>39 TX%=39
        IF TY%<1 TY%=1
        IF TY%>24 TY%=24

        IF OX%<>TX% OR OY%<>TY% THEN
          GCOL 3,15
          RECTANGLE boxx%,boxy%,X%,Y%

          IF TX%<SX% THEN
            boxx%=SX%*32+32
            X%=-((SX%-TX%)*32+32)
          ELSE
            boxx%=SX%*32
            X%=(TX%-SX%)*32+32
          ENDIF

          IF TY%<SY% THEN
            boxy%=(25-SY%)*40-40
            Y%=(SY%-TY%)*40+40
            IF boxy%+Y%>960 THEN Y%-=40
          ELSE
            boxy%=(25-SY%)*40
            Y%=-((TY%-SY%)*40+40)
          ENDIF

          RECTANGLE boxx%,boxy%,X%,Y%

          OX%=TX%
          OY%=TY%

          C$="S:"+RIGHT$("0"+STR$(SX%),2)+","+RIGHT$("0"+STR$(SY%),2)+" E:"+RIGHT$("0"+STR$(TX%),2)+","+RIGHT$("0"+STR$(TY%),2)+" "
          PROCgtext(C$,0,996,14,4,0)

        ELSE
          WAIT 2

        ENDIF
      UNTIL MB%=0
      RECTANGLE boxx%,boxy%,X%,Y%

      PROCchangemode(7,1)
      frame%-=1
      PROCloadnextframe(1,0)

      PROCcopyregion(SX%,SY%,TX%,TY%)
      copypaste%=1
      menuext%=M_canvasmode%
      toolsel%=T_paste&
      toolcursor%=17

      PROCmenudraw

      ENDPROC

      REM ##########################################################
      REM copy region of current frame to copypaste buffer
      DEF PROCcopyregion(x1%,y1%,x2%,y2%)
      LOCAL C%,X%,Y%,PX%,PY%
      IF x1%>x2% THEN SWAP x1%,x2%
      IF y1%>y2% THEN SWAP y1%,y2%

      IF x1%<0 THEN x1%=0
      IF x1%>39 THEN x1%=39
      IF y1%<1 THEN y1%=1
      IF y1%>24 THEN y1%=24

      IF x2%<0 THEN x2%=0
      IF x2%>39 THEN x2%=39
      IF y2%<1 THEN y2%=1
      IF y2%>24 THEN y2%=24

      C%=0

      FOR X%=x1% TO x2%
        FOR Y%=y1% TO y2%
          copy_buffer&(C%)=GET(X%,Y%)
          C%+=1
        NEXT
      NEXT
      copyx%=x2%-x1%
      copyy%=y2%-y1%
      copysize%=C%

      copylockx%=x1%
      copylocky%=y1%

      REM create a pixel buffer of the selected region for moving around
      copymovepx%=x1%*2
      copymovepy%=y1%*3
      copymovepw%=copyx%*2+1
      copymoveph%=copyy%*3+2
      copymovef%=frame%

      REM copy pixels to buffer
      FOR Y%=0 TO copymoveph%
        PY%=copymovepy%+Y%
        FOR X%=0 TO copymovepw%
          PX%=copymovepx%+X%
          IF PX%>1 AND PX%<80 AND PY%>2 AND PY%<75 THEN
            C%=FNpoint_buf(PX%,PY%,frame%)
          ELSE
            C%=0
          ENDIF
          move_buffer&(X%+Y%*(copymovepw%+1))=C%
        NEXT
      NEXT

      ENDPROC

      REM ##########################################################
      REM paste copypaste buffer to current frame
      DEF PROCpasteregion(x1%,y1%)
      LOCAL C%,X%,Y%,C%

      s%=0

      IF copylockxt% THEN x1%=copylockx%
      IF copylockyt% THEN y1%=copylocky%

      IF copysize%>0 THEN
        PROCundosave
        FOR X%=x1% TO x1%+copyx%
          FOR Y%=y1% TO y1%+copyy%
            IF X%<40 AND X%>-1 AND Y%<25 AND Y%>0 THEN
              C%=copy_buffer&(s%)
              IF copy_trns%=0 THEN
                VDU 31,X%,Y%,C%
              ELSE
                IF C%<>32 AND C%<>160 THEN VDU 31,X%,Y%,C% : REM copy_buffer&(s%)
              ENDIF
            ENDIF
            s%+=1
          NEXT
        NEXT

      ENDIF
      ENDPROC

      REM ##########################################################
      REM paste copypaste buffer to buffer
      DEF PROCpasteregion_buf(f%,x1%,y1%)
      LOCAL s%,X%,Y%,C%

      s%=0

      IF copysize%>0 THEN
        REM PROCundosave
        FOR X%=x1% TO x1%+copyx%
          FOR Y%=y1%-1 TO y1%+copyy%-1

            IF X%<40 AND X%>-1 AND Y%<24 AND Y%>-1 THEN
              C%=copy_buffer&(s%)
              IF copy_trns%=0 THEN
                frame_buffer&(f%-1,X%+Y%*40)=C%
              ELSE
                IF C%<>32 AND C%<>160 THEN frame_buffer&(f%-1,X%+Y%*40)=C%
              ENDIF
            ENDIF
            s%+=1
          NEXT
        NEXT
      ENDIF

      ENDPROC

      REM ##########################################################
      REM copy control codes to all frames
      DEF PROCcopycodes_buf(s%)
      LOCAL X%,Y%,C%,F%

      FOR F%=1 TO frame_max%
        IF s%<>F% THEN
          REM PROCundosave
          FOR X%=0 TO 39
            FOR Y%=0 TO 23
              C%=frame_buffer&(s%-1,X%+Y%*40)
              IF C%>128 AND C%<160 THEN
                frame_buffer&(F%-1,X%+Y%*40)=C%
              ENDIF
            NEXT
          NEXT
        ENDIF
      NEXT
      ENDPROC

      REM ##########################################################
      REM move selected region to a new location
      DEF PROCmoveregion(h%,v%)
      LOCAL X%,Y%,PX%,PY%

      IF copymovef%=frame% THEN
        PROCundosave

        REM erase old pixels
        PROCeraseregion

        REM change position
        copymovepx%+=h%
        copymovepy%+=v%

        REM plot pixels in new location
        FOR Y%=0 TO copymoveph%
          PY%=copymovepy%+Y%
          FOR X%=0 TO copymovepw%
            PX%=copymovepx%+X%
            IF PX%>1 AND PX%<80 AND PY%>2 AND PY%<75 THEN
              IF move_buffer&(X%+Y%*(copymovepw%+1))=1 THEN PROCpoint(PX%,PY%,1)
            ENDIF
          NEXT
        NEXT

        PROCframesave(frame%)
      ENDIF

      ENDPROC

      REM ##########################################################
      REM mirror selected region horizontally
      DEF PROCmirrorregion(M%)
      LOCAL X%,Y%,PX%,PY%,CW%,CH%,CX%,CY%,C%

      IF copymovef%=frame% OR M%=1 THEN
        PROCundosave

        IF M%=0 THEN
          CW%=copymovepw%
          CH%=copymoveph%
          CX%=copymovepx%
          CY%=copymovepy%
        ELSE
          CW%=38
          CH%=72
          CX%=2
          CY%=3
        ENDIF

        REM plot pixels in mirrored location
        FOR Y%=0 TO CH%
          PY%=CY%+Y%
          FOR X%=0 TO CW%
            PX%=CX%+CW%*2-X%-1
            IF PX%>1 AND PX%<80 AND PY%>2 AND PY%<75 THEN
              IF M%=0 THEN
                C%=move_buffer&(X%+Y%*(CW%+1))
                IF C%=1 OR copy_trns%=0 THEN PROCpoint(PX%,PY%,C%)
              ELSE
                C%=FNpoint(X%+CX%,Y%+CY%)
                IF C%=1 OR copy_trns%=0 THEN PROCpoint(79-X%,Y%+CY%,C%)
              ENDIF
            ENDIF
          NEXT
        NEXT

        PROCframesave(frame%)
      ENDIF

      ENDPROC

      REM ##########################################################
      REM reflect selected region vertically
      DEF PROCreflectregion(M%)
      LOCAL X%,Y%,PX%,PY%,CW%,CH%,CX%,CY%,C%

      IF copymovef%=frame% OR M%=1 THEN
        PROCundosave

        IF M%=0 THEN
          CW%=copymovepw%
          CH%=copymoveph%
          CX%=copymovepx%
          CY%=copymovepy%
        ELSE
          CW%=78
          CH%=35
          CX%=2
          CY%=3
        ENDIF

        REM plot pixels in reflected location
        FOR Y%=0 TO CH%
          PY%=CY%+CH%*2-Y%-1
          FOR X%=0 TO CW%
            PX%=CX%+X%
            IF PX%>1 AND PX%<80 AND PY%>2 AND PY%<75 THEN
              REM               IF move_buffer&(X%+Y%*(CW%+1))=1 THEN PROCpoint(PX%,PY%,1)
              IF M%=0 THEN
                C%=move_buffer&(X%+Y%*(CW%+1))
                IF C%=1 OR copy_trns%=0 THEN PROCpoint(PX%,PY%,C%)
              ELSE
                C%=FNpoint(X%+CX%,Y%+CY%)
                IF C%=1 OR copy_trns%=0 THEN PROCpoint(X%+CX%,74-Y%,C%)
              ENDIF

            ENDIF
          NEXT
        NEXT

        PROCframesave(frame%)
      ENDIF

      ENDPROC

      REM ##########################################################
      REM flip selected region horizontally
      DEF PROCfliphregion
      LOCAL X%,Y%,PX%,PY%

      IF copymovef%=frame% THEN
        PROCundosave

        REM erase old pixels
        PROCeraseregion

        REM plot pixels in new location
        FOR Y%=0 TO copymoveph%
          PY%=copymovepy%+Y%
          FOR X%=0 TO copymovepw%
            PX%=copymovepx%+copymovepw%-X%
            IF PX%>1 AND PX%<80 AND PY%>2 AND PY%<75 THEN
              IF move_buffer&(X%+Y%*(copymovepw%+1))=1 THEN PROCpoint(PX%,PY%,1)
            ENDIF
          NEXT
        NEXT

        PROCframesave(frame%)
      ENDIF

      ENDPROC

      REM ##########################################################
      REM flip selected region vertically
      DEF PROCflipvregion
      LOCAL X%,Y%,PX%,PY%

      IF copymovef%=frame% THEN
        PROCundosave

        REM erase old pixels
        PROCeraseregion

        REM plot pixels in new location
        FOR Y%=0 TO copymoveph%
          PY%=copymovepy%+copymoveph%-Y%
          FOR X%=0 TO copymovepw%
            PX%=copymovepx%+X%
            IF PX%>1 AND PX%<80 AND PY%>2 AND PY%<75 THEN
              IF move_buffer&(X%+Y%*(copymovepw%+1))=1 THEN PROCpoint(PX%,PY%,1)
            ENDIF
          NEXT
        NEXT

        PROCframesave(frame%)
      ENDIF

      ENDPROC

      REM ##########################################################
      REM apply negative to selected region vertically
      DEF PROCnegativeregion
      LOCAL X%,Y%,PX%,PY%

      IF copymovef%=frame% THEN
        PROCundosave

        REM erase old pixels
        PROCeraseregion

        REM plot pixels in new location
        FOR Y%=0 TO copymoveph%
          PY%=copymovepy%+Y%
          FOR X%=0 TO copymovepw%
            PX%=copymovepx%+X%
            IF PX%>1 AND PX%<80 AND PY%>2 AND PY%<75 THEN
              IF move_buffer&(X%+Y%*(copymovepw%+1))=0 THEN PROCpoint(PX%,PY%,1)
            ENDIF
          NEXT
        NEXT

        PROCframesave(frame%)
      ENDIF

      ENDPROC

      REM ##########################################################
      REM erase region
      DEF PROCeraseregion
      LOCAL X%,Y%,PX%,PY%

      IF copymovef%=frame% THEN

        REM erase old pixels
        FOR Y%=0 TO copymoveph%
          PY%=copymovepy%+Y%
          FOR X%=0 TO copymovepw%
            PX%=copymovepx%+X%
            IF move_buffer&(X%+Y%*(copymovepw%+1))=1 IF PX%>1 AND PX%<80 AND PY%>2 AND PY%<75 THEN PROCpoint(PX%,PY%,0)
          NEXT
        NEXT

      ENDIF

      ENDPROC

      REM ##########################################################
      REM save sprite file
      DEF PROCsavespritefile(F$,S%,D%)
      LOCAL f%,u%,c%,O$

      REM sprite scroll mode
      O$=STR$(sprlist{(0)}.m%)
      FOR c%=1 TO sprite_max%-1
        O$=O$+","+STR$(sprlist{(c%)}.m%)
      NEXT

      IF S%=1 THEN
        f%=OPENOUT(F$+".SPR")

        PRINT#f%,"TELEPAINT_SPR"
        PRINT#f%,O$

        REM sprite data
        FOR c%=0 TO sprite_max%-1
          FOR u%=0 TO 319
            BPUT#f%,sprbuf&(c%,u%)
          NEXT
        NEXT

        REM sprite animation data
        FOR c%=0 TO 99
          FOR u%=0 TO sprani_max%-1
            PRINT#f%,sprani{(c%)}.s%(u%)
          NEXT
          PRINT#f%,sprani{(c%)}.f%
          PRINT#f%,sprani{(c%)}.r%
          PRINT#f%,sprani{(c%)}.x%
          PRINT#f%,sprani{(c%)}.y%
          PRINT#f%,sprani{(c%)}.h%
          PRINT#f%,sprani{(c%)}.v%
          PRINT#f%,sprani{(c%)}.m%
          PRINT#f%,sprani{(c%)}.d%
          PRINT#f%,sprani{(c%)}.c%
        NEXT

        CLOSE#f%
      ENDIF

      IF D%=1 THEN
        A$=""
        f%=OPENOUT(F$+".TXT")
        FOR c%=0 TO sprite_max%-1
          PRINT#f%,"REM SPR: "+STR$(c%)+"  W: 40  H: 48"
          BPUT#f%,10
          FOR Y%=0 TO 47
            A$=""
            FOR X%=0 TO 39
              A$+=STR$(FNpoint_sprbuf(X%,Y%,c%))
              IF X%<39 THEN A$+=","
            NEXT
            PRINT#f%,"DATA "+A$
            BPUT#f%,10
          NEXT
          PRINT#f%,""
          BPUT#f%,10
        NEXT
        CLOSE#f%
      ENDIF

      ENDPROC

      REM ##########################################################
      REM load sprite file
      DEF PROCloadspritefile(F$)
      LOCAL f%,u%,char%,c%,I$

      f%=OPENIN(F$)

      IF f% THEN
        REM read header for new spr file format or reset ptr if old format
        INPUT#f%,I$
        IF I$="TELEPAINT_SPR" THEN
          INPUT#f%,I$
          c% = FN_split(I$, ",", a$())
          FOR u%=0 TO c%-1
            sprlist{(u%)}.m%=VAL(a$(u%))
          NEXT
        ELSE
          PTR#f%=0
        ENDIF

        c%=0
        REPEAT
          IF c%<sprite_max% THEN
            REM read sprite data, try to read old format if possible
            FOR u%=0 TO 319
              char%=BGET#f%
              sprbuf&(c%,u%)=char%
            NEXT
            PROCupdatespritesize(c%,0)
            c%+=1
          ELSE
            REM read animation data
            FOR c%=0 TO 99
              FOR u%=0 TO sprani_max%-1
                INPUT#f%,sprani{(c%)}.s%(u%)
              NEXT
              INPUT#f%,sprani{(c%)}.f%
              INPUT#f%,sprani{(c%)}.r%
              INPUT#f%,sprani{(c%)}.x%
              INPUT#f%,sprani{(c%)}.y%
              INPUT#f%,sprani{(c%)}.h%
              INPUT#f%,sprani{(c%)}.v%
              INPUT#f%,sprani{(c%)}.m%
              INPUT#f%,sprani{(c%)}.d%
              INPUT#f%,sprani{(c%)}.c%
            NEXT
          ENDIF
        UNTIL EOF#f%
        CLOSE#f%
      ENDIF

      ENDPROC

      REM ##########################################################
      REM load text file
      DEF PROCloadtextfile(F$)
      LOCAL f%,c%,line$
      f%=OPENIN(F$)

      IF f% THEN
        INPUT#f%,line$
        obj_txtcur%=VAL(line$)
        IF obj_txtcur%>-1 THEN
          FOR c%=0 TO obj_txtcur%
            INPUT#f%,line$
            txtlist$(c%)=line$
          NEXT
        ENDIF
        CLOSE#f%
      ENDIF

      ENDPROC

      REM ##########################################################
      REM load mov file
      DEF PROCloadmovfile(F$)
      LOCAL f%,line$,c%,l%

      f%=OPENIN(F$)

      IF f% THEN
        INPUT#f%,line$
        IF line$="*FRAME DATA*" THEN
          INPUT#f%,line$
          movieframetotal%=VAL(line$)
          IF movieframetotal%>-1 THEN
            FOR l%=0 TO movieframetotal%
              INPUT#f%,line$
              c% = FN_split(line$, ",", a$())
              IF c%=4 THEN
                frmlist{(l%)}.x%=VAL(a$(0))
                frmlist{(l%)}.y%=VAL(a$(1))
                frmlist{(l%)}.b%=VAL(a$(2))
                frmlist{(l%)}.f%=VAL(a$(3))
              ENDIF
            NEXT
          ENDIF
          INPUT#f%,line$
          IF line$="*OBJECT DATA*" THEN
            INPUT#f%,line$
            obj_lstcount%=VAL(line$)
            IF obj_lstcount%>-1 THEN
              FOR l%=0 TO obj_lstcount%
                INPUT#f%,line$
                c% = FN_split(line$, ",", a$())
                IF c%=13 THEN
                  objlist{(l%)}.obj%=VAL(a$(0))
                  objlist{(l%)}.type%=VAL(a$(1))
                  objlist{(l%)}.f%=VAL(a$(2))
                  objlist{(l%)}.rel%=VAL(a$(3))
                  objlist{(l%)}.hop%=VAL(a$(4))
                  objlist{(l%)}.parent%=VAL(a$(5))
                  objlist{(l%)}.x%=VAL(a$(6))
                  objlist{(l%)}.y%=VAL(a$(7))
                  objlist{(l%)}.h%=VAL(a$(8))
                  objlist{(l%)}.v%=VAL(a$(9))
                  objlist{(l%)}.m%=VAL(a$(10))
                  objlist{(l%)}.c%=VAL(a$(11))
                  objlist{(l%)}.u%=VAL(a$(12))
                ENDIF
              NEXT
            ENDIF
          ENDIF
        ENDIF
        CLOSE#f%
      ENDIF

      ENDPROC


      REM ##########################################################
      REM save binary file
      DEF PROCsavebinaryfile(F$)
      LOCAL f%,u%
      f%=OPENOUT(F$)
      FOR u%=0 TO 999
        BPUT#f%,GET(u% MOD 40,u% DIV 40)
      NEXT
      CLOSE#f%
      ENDPROC

      REM ##########################################################
      REM load binary file
      DEF PROCloadbinaryfile(F$)
      LOCAL f%,u%,char%
      f%=OPENIN(F$)

      IF f% THEN
        FOR u%=0 TO 999
          char%=BGET#f%
          IF char% OR &80>&9F THEN char%=char% OR &80
          VDU 31,u% MOD 40,u% DIV 40,char%
        NEXT
        CLOSE#f%
      ENDIF
      ENDPROC

      REM ##########################################################
      REM load tti file
      DEF PROCloadttifile(F$)
      LOCAL f%,u%,char%,line$
      f%=OPENIN(F$)

      IF f% THEN
        WHILE NOT EOF#(f%)
          INPUT#f%,line$
          PRINT TAB(0,1) line$
          A$=GET$
        ENDWHILE

        CLOSE#f%
      ENDIF
      ENDPROC

      REM ##########################################################
      REM load bmp file and store in import buffer
      DEF PROCloadbmptobuf(F$,chk%)
      LOCAL C%,T$
      REM max load size is ~1MB to match import buffer
      OSCLI "LOAD """+F$+""" "+STR$~import_buffer%%+" +"+STR$~1000000


      REM bmp filetype (2 bytes)
      T$=CHR$(import_buffer%%?0)+CHR$(import_buffer%%?1)
      REM bmp filesize (4 bytes) ?2 ?3 ?4 ?5
      REM bmp reserved (2 bytes) ?6 ?7
      REM bmp reserved (2 bytes) ?8 ?9
      REM bmp pixel data offset (4 bytes)
      bmp_imgofs%=import_buffer%%!10
      REM bmp header size (4 bytes)
      REM bmp image width (4 bytes)
      bmp_imgwid%=import_buffer%%!18
      REM bmp image height (4 bytes)
      bmp_imghgt%=import_buffer%%!22
      REM bmp planes (2 bytes) ?26 ?27
      REM bmp but per pixel (2 bytes)
      bmp_imgbpp%=import_buffer%%?28+(import_buffer%%?29*256)
      REM bmp compression (4 bytes) ?30 ?31 ?32 ?33
      REM bmp image size (4 bytes) ?34 ?35 ?36 ?37
      REM bmp x pixels per meter (4 bytes) ?38 ?39 ?40 ?41
      REM bmp y pixels per meter (4 bytes) ?42 ?43 ?44 ?45
      REM bmp total colours (4 bytes) ?46 ?47 ?48 ?49
      REM bmp important colours (4 bytes) ?50 ?51 ?52 ?53
      IF chk%=1 THEN
        IF T$<>"BM" OR bmp_imgofs%<>54 OR bmp_imgbpp%<>24 THEN
          PRINTTAB(0,0)"Image format not supported, must be BMP 24bpp"
          C%=0
          REPEAT
            PROCREADMOUSE
            WAIT 5
            C%+=1
          UNTIL C%>100 OR MB%<>0
          PROCWAITMOUSE(0)
          menuext%=94
        ELSE
          menuext%=95
        ENDIF
      ENDIF

      ENDPROC

      REM ##########################################################
      REM load font names
      DEF PROCloadfontnames
      LOCAL N%,I%,C%
      LOCAL n$,t&

      DIM n$(10000)
      DIM t&(10000)
      fontcount%=0

      OSCLI "CD """+@dir$+""""

      ON ERROR LOCAL IF FALSE THEN
        REM OSCLI "CD """+@dir$+"M7_FONTS"""
        N% = FN_dirscan2(n$(), t&(), "DIR M7_FONTS/*.M7F", "",1)
        IF N%>2 THEN
          C%=1
          FOR I%=3 TO N%
            IF t&(I%)=2 THEN
              fontname$(C%)=LEFT$(n$(I%),LEN(n$(I%))-4)
              C%+=1
              fontcount%+=1
            ENDIF
          NEXT
        ENDIF
        REM OSCLI "CD """+@dir$+""""
        IF fontcount%>0 fontfound%=1

      ELSE
        OSCLI "CD """+@dir$+""""
        PRINTTAB(0,1);"ERROR! COULD NOT FIND FONTS FOLDER"
        PRINTTAB(0,2);"CD """+@dir$+"M7_FONTS"""
        A=GET

      ENDIF : RESTORE ERROR

      ENDPROC

      REM ##########################################################
      REM loadfile - modified dirscan to include type array so files list can be displayed for mode 7
      REM loadtype determines the type of load / import function
      DEF PROCloadfile(loadtype%)
      LOCAL I%,N%,L%,M%,F%,SEL%,SELOLD%,SELY%,INDEX%,INDEXOLD%,fh%,MACT%,maxy%,opt1%,opt2%,GT%,OGT%,reload%
      LOCAL title$,filetype$
      REM n$ holds file and dir list of current folder
      REM t& holds type list for current folder, 0=special, 1=dir, 2=file

      PRIVATE n$,t&,c%
      DIM n$(10000)
      DIM t&(10000)
      DIM c%(2)

      REM folder and file colour codes
      c%(0)=131
      c%(1)=134
      c%(2)=135

      REM top of load window
      M%=3

      menuext%=99
      maxy%=M%+15
      CASE loadtype% OF
        WHEN 0 : maxy%=M%+19

      ENDCASE

      VDU 23,1,0;0;0;0;  : REM disable cursor

      PROCWAITMOUSE(0)

      FOR L%=M% TO maxy%
        PROCprint40(L%,"")
      NEXT

      FOR L%=M%+1 TO maxy%-1
        PRINTTAB(2,L%)gw$;CHR$(234);STRING$(30," ");gw$;CHR$(181);
      NEXT
      PRINTTAB(34,M%+3)tg$;"-";
      PRINTTAB(34,M%+4)tg$;"#";
      PRINTTAB(34,M%+11)tg$;"#";
      PRINTTAB(34,M%+12)tg$;"+";
      PRINTTAB(2,maxy%)gw$;CHR$(170);STRING$(31,CHR$(172));CHR$(165);
      PRINTTAB(5,M%+14)tb$;CHR$(157);tc$;"LOAD  ";CHR$(156);"       ";tr$;CHR$(157);ty$;"CLOSE  ";CHR$(156);

      CASE loadtype% OF
        WHEN 0 : REM bin files
          filetype$=".bin"
          title$="LOAD FILE"
          PRINTTAB(15,M%+14)tr$;CHR$(157);ty$;"LOAD LAST SAVE ";gw$;CHR$(156);
          PRINTTAB(4,M%+16)tg$;"(*)";tw$;"ALL FRMS ";tg$;"( )";tw$;"SINGLE FRM";
          PRINTTAB(4,M%+17)tg$;"(*)";tw$;"CLS ";tg$;"( )";tw$;"BACK ";tg$;"( )";tw$;"FORE";
          PRINTTAB(4,M%+18)tg$;"( )";tw$;"SERIES 78x72 : F0001.BMP"

        WHEN 1 : REM import bmp
          filetype$=".bmp"
          title$="IMPORT FILE"
          opt1%=1
          PRINTTAB(1,M%+17)tg$;"(*)";tw$;"SINGLE BOX CAPTURE"
          PRINTTAB(1,M%+18)tg$;"( )";tw$;"GRID";tc$;"HOR";tw$;"-";ty$;"10";tw$;"+ ";tc$;"VER";tw$;"-";ty$;"02";tw$"+"
          PRINTTAB(1,M%+19)tg$;"( )";tw$;"SERIES 78x72 FORMAT: F0001.BMP"
          GX%=10
          GY%=2

        WHEN 2 : REM import bmp to sprite
          filetype$=".bmp"
          title$="SPRITE IMPORT"
          opt1%=1

        WHEN 3 : REM load spr sprite
          filetype$=".spr"
          title$="LOAD SPR FILE"
          opt1%=1

        WHEN 4 : REM import bmp to font
          filetype$=".bmp"
          title$="FONT IMPORT"
          opt1%=1

        WHEN 5 : REM import M7F to font
          filetype$=".m7f"
          title$="FONT LOAD"
          opt1%=1

        WHEN 6 : REM load mov file and spr file if exists
          filetype$=".mov"
          title$="LOAD MOV FILE"
          opt1%=1

        WHEN 7 : REM convert gig to bmp files
          filetype$=".gif"
          title$="GIF CONVERSION TO BMP"
          opt1%=1


      ENDCASE
      L%=(29-LEN(title$)) DIV 2
      I%=L% + (LEN(title$)-1) MOD 2
      PRINTTAB(2,M%)gw$;CHR$(232);STRING$(L%,CHR$(172));tg$;title$;gw$;STRING$(I%,CHR$(172));CHR$(180);

      REM scan initial folder and set menu defaults
      N% = FN_dirscan2(n$(), t&(), "dir *.*", filetype$,opt1%)
      F%=0
      S%=0
      SEL%=0
      SELOLD%=0
      SELY%=-1
      INDEX%=1
      INDEXOLD%=1
      MACT%=-1

      FOR I%=INDEX% TO INDEX%+11
        IF I%<N%+1 THEN PRINTTAB(6,M%+I%)CHR$(c%(t&(I%)));LEFT$(n$(I%),24);
      NEXT

      REPEAT
        PROCREADMOUSE

        REM detect first touch and movement
        IF MB%=4 THEN
          IF MACT%=-1 THEN MACT%=TY%

          REM drag action scrolls file list
          IF MACT%>M% AND MACT%<M%+13 THEN
            IF TX%>4 AND TX%<34 THEN
              IF TY%<>OLD_TY% THEN
                INDEX%-=SGN(TY%-OLD_TY%)
                IF INDEX%>N%-11 THEN INDEX%=N%-11
                IF INDEX%<1 THEN INDEX%=1
                SELY%=-2
              ELSE
                IF SELY%=-1 THEN SELY%=TY%
              ENDIF
            ENDIF
          ELSE
            IF TY%=M%+18 AND loadtype%=1 THEN
              CASE TX% OF
                WHEN 15 : IF GX%>1 THEN GX%-=1

                WHEN 20 : IF GX%<20 THEN GX%+=1

                WHEN 27 : IF GY%>1 THEN GY%-=1

                WHEN 32 : IF GY%<20 THEN GY%+=1
              ENDCASE
              PRINTTAB(17,TY%)RIGHT$("0"+STR$(GX%),2)
              PRINTTAB(29,TY%)RIGHT$("0"+STR$(GY%),2)
              WAIT 20
            ENDIF
          ENDIF

        ENDIF

        REM detect touch release
        IF MB%=0 AND MACT%<>-1 THEN

          REM scroll buttons
          IF TX%=35 THEN
            IF MACT%=M%+3 THEN INDEX%-=1
            IF MACT%=M%+12 THEN INDEX%+=1
            IF MACT%=M%+4 THEN INDEX%-=10
            IF MACT%=M%+11 THEN INDEX%+=10
            IF INDEX%>N%-11 THEN INDEX%=N%-11
            IF INDEX%<1 THEN INDEX%=1
          ENDIF

          REM select file / folder
          IF SELY%=TY% AND MACT%>M% AND MACT%<M%+13 AND TX%>3 AND TX%<35 THEN
            S%=TY%-M%-1
            IF S%>-1 AND S%<12 THEN
              SEL%=S%+INDEX%
              IF SEL%<1 THEN SEL%=1
              IF SEL%>N% THEN SEL%=N%

              IF t&(SEL%)=2 THEN
                REM F%=SEL%
              ELSE
                REM change folder
                ON ERROR LOCAL IF FALSE THEN
                  S% = 0
                  a$=n$(SEL%)
                  IF ASC(a$)=&40 a$=EVAL(a$)
                  OSCLI "cd """ + a$ + """"
                ELSE
                  S% = 3
                ENDIF : RESTORE ERROR
                IF S%=0 THEN reload%=1

              ENDIF
            ENDIF

          ENDIF


          REM check for button and control clicks
          IF TY%=0 AND MACT%=0 THEN F%=-1

          REM load and cancel buttons
          IF TY%=M%+14 AND MACT%=TY% THEN
            IF TX%>5 AND TX%<14 THEN F%=SEL%
            IF loadtype%=0 THEN
              IF TX%>15 AND TX%<34 THEN F%=-2
            ELSE
              IF TX%>22 AND TX%<32 THEN F%=-1
            ENDIF
          ENDIF

          CASE loadtype% OF
            WHEN 0 :
              REM load frame options - all frames or single frame
              IF TY%=M%+16 AND MACT%=TY% THEN
                CASE TX% OF
                  WHEN 5,6,7
                    opt1%=0
                    reload%=1
                  WHEN 19,20,21
                    IF GT%=0 THEN
                      opt1%=1
                      reload%=1
                    ENDIF
                ENDCASE
                PRINTTAB(6,TY%)CHR$(42-opt1%*10)
                PRINTTAB(20,TY%)CHR$(32+opt1%*10)
              ENDIF

              REM load merge options
              IF TY%=M%+17 AND MACT%=TY% THEN
                CASE TX% OF
                  WHEN 5,6,7 : opt2%=0
                  WHEN 14,15,16 : opt2%=1
                  WHEN 24,25,26 : opt2%=2
                ENDCASE
                PRINTTAB(6,TY%)CHR$(32-(opt2%=0)*10)
                PRINTTAB(15,TY%)CHR$(32-(opt2%=1)*10)
                PRINTTAB(25,TY%)CHR$(32-(opt2%=2)*10)
              ENDIF

              REM import series
              IF TY%=M%+18 AND MACT%=TY% THEN
                CASE TX% OF
                  WHEN 5,6,7
                    GT%=(GT%+1) AND 1
                    IF GT%=1 AND opt1%=1 THEN
                      opt1%=0
                      PRINTTAB(6,TY%-1)CHR$(42-opt1%*10)
                      PRINTTAB(20,TY%-1)CHR$(32+opt1%*10)
                    ENDIF
                ENDCASE
                IF GT%=0 THEN
                  filetype$=".bin"
                ELSE
                  filetype$=".bmp"
                ENDIF
                reload%=1
                PRINTTAB(6,M%+18)CHR$(32+GT%*10)
              ENDIF

            WHEN 1 :

              REM grid size controls
              IF MACT%>M%+16 THEN
                OGT%=GT%
                IF TY%=M%+17 AND TX%>1 AND TX%<5 THEN GT%=0
                IF TY%=M%+18 AND TX%>1 AND TX%<5 THEN GT%=1
                IF TY%=M%+19 AND TX%>1 AND TX%<5 THEN GT%=2
                IF OGT%<>GT% THEN
                  PRINTTAB(3,M%+17+GT%)"*"
                  PRINTTAB(3,M%+17+OGT%)" "
                ENDIF
              ENDIF

          ENDCASE
          SELY%=-1
          MACT%=-1
        ENDIF

        IF reload%=1 THEN
          N% = FN_dirscan2(n$(), t&(), "dir *.*",filetype$,opt1%)
          SEL%=0
          SELOLD%=0
          SELY%=-1
          INDEX%=1
          INDEXOLD%=-1
          FOR I%=INDEX% TO INDEX%+11
            PRINTTAB(4,M%+I%)SPC(28);
          NEXT
          reload%=0
        ENDIF

        REM IF SCROLLING DETECTED UPDATE FILE LIST AND SELECTED FILE INDEX
        IF INDEX%<>INDEXOLD% OR SELOLD%<>SEL% THEN
          FOR I%=0 TO 11
            K%=I%+INDEX%
            IF K%<N%+1 THEN
              PRINTTAB(4,I%+M%+1)SPC(28)
              VDU 31,4,I%+M%+1
              IF SEL%=K% AND K%>2 THEN
                VDU 132,157
              ELSE
                VDU 32,32
              ENDIF
              PRINTCHR$(c%(t&(K%)));LEFT$(n$(K%),24);
              IF SEL%=K% AND K%>2 THEN VDU 32,32,156
            ENDIF
          NEXT
          SELOLD%=SEL%
          INDEXOLD%=INDEX%
        ENDIF

        REM PRINTTAB(0,1)STR$(SEL%)

        OLD_TY%=TY%

        WAIT 2
      UNTIL F%<>0

      VDU 23,1,1;0;0;0;  : REM Enable cursor

      CASE loadtype% OF
        WHEN 0 : REM load bin
          PROCmenurestore

          IF F%=-2 THEN
            REM read last session file
            fh%=OPENIN(@dir$+"telepaint_pref.ini")
            IF fh% THEN
              INPUT#fh%,F$
              CLOSE#fh%
              F$=F$+"_"
            ELSE
              F%=-1
            ENDIF
          ENDIF

          IF F%<>-1 THEN
            IF GT%=0 THEN
              IF INSTR(n$(SEL%),"M7_") OR F%=-2 THEN

                IF F%>0 THEN F$=curdir$+LEFT$(n$(SEL%),LEN(n$(SEL%))-5)
                FOR frame%=1 TO frame_max%
                  PROCloadbinaryfile(F$ + STR$(frame%)+".BIN")
                  PROCframesave(frame%)
                  REM WAIT 10
                NEXT
                PROCloadnextframe(1,0)
              ELSE
                IF RIGHT$(FNUPPER(n$(SEL%)),3)="BIN" THEN
                  PROCloadbinaryfile(curdir$+n$(SEL%))
                  PROCframesave(frame%)
                ENDIF
              ENDIF

            ELSE
              REM import series
              IF LEN(n$(SEL%))=9 THEN
                F%=VAL(MID$(n$(SEL%),2,4))
                F$=LEFT$(n$(SEL%),1)
                CASE opt1% OF
                  WHEN 0 : REM load full series
                    PROCimportseries(F$,F%,1,frame_max%)
                    frame%=0
                  WHEN 1 :  REM load single frame
                    PROCimportseries(F$,F%,frame%,frame%)
                    frame%-=1
                ENDCASE
                PROCloadnextframe(1,0)
                menuext%=M_canvasmode%

              ELSE
                COLOUR 9
                PRINTTAB(0,0)"FILE NAME NOT CORRECT: E.G. F0001.BMP"
                menuext%=94
              ENDIF

            ENDIF
          ENDIF

        WHEN 1 : REM import bmp
          PROCchangemode(6,1)

          IF F%=-1 THEN
            COLOUR 9
            PRINTTAB(0,0)"NO FILE LOADED"
            menuext%=94
          ELSE
            IF GT%<2 THEN
              OSCLI "DISPLAY """+curdir$+n$(SEL%)+""" 0,0,1280,1000"
              menuext%=95+GT%
              OLD_TX%=GX%
              OLD_TY%=GY%
            ELSE
              REM import series
              IF LEN(n$(SEL%))=9 THEN
                F%=VAL(MID$(n$(SEL%),2,4))
                F$=LEFT$(n$(SEL%),1)
                PROCimportseries(F$,F%,1,frame_max%)
              ELSE
                COLOUR 9
                PRINTTAB(0,0)"FILE NAME NOT CORRECT: E.G. F0001.BMP"
                menuext%=94
              ENDIF
            ENDIF
          ENDIF

        WHEN 2,7 : REM import sprite from bmp or gif
          IF F%<>-1 THEN
            bmpload$=curdir$+n$(SEL%)
            menuext%=95
          ELSE
            menuext%=94
          ENDIF

        WHEN 3 : REM load spr
          PROCloadspritefile(curdir$+n$(SEL%))

        WHEN 4 : REM font import

          PROCchangemode(6,1)

          IF F%=-1 THEN
            COLOUR 9
            PRINTTAB(0,0)"NO FILE LOADED"
            PROCWAITMOUSE(4)
            PROCWAITMOUSE(0)
          ELSE

            PROCloadbmptobuf(curdir$+n$(SEL%),1)
            IF menuext%=95 THEN
              OSCLI "MDISPLAY "+STR$~import_buffer%%
            ENDIF

          ENDIF

        WHEN 5 : REM load specific font
          IF F%<>-1 THEN
            PROCloadfont(curdir$+n$(SEL%))
            fontcur%=1
            fontcount%=1
            fontfound%=0
            fontname$(fontcur%)=LEFT$(n$(SEL%),LEN(n$(SEL%))-4)
          ENDIF

        WHEN 6 : REM load mov and spr files
          PROCloadmovfile(curdir$+n$(SEL%))
          PROCloadspritefile(curdir$+"SPRITEDATA.SPR")
          PROCloadtextfile(curdir$+"TEXTDATA.TXT")

          F$=curdir$+LEFT$(n$(SEL%),LEN(n$(SEL%))-4)+ "_FRM_"
          CLS
          FOR frame%=1 TO frame_max%
            N$=RIGHT$("00"+STR$(frame%),3)
            PROCloadbinaryfile(F$ + N$ +".BIN")
            PROCframesave(frame%)
            REM WAIT 10
          NEXT
          PROCloadnextframe(1,0)

      ENDCASE

      ENDPROC

      REM ##########################################################
      REM save all frames to file, create session folder if not already exists
      DEF PROCsavefile
      LOCAL D$,done%,OG%,L%,C%,F%

      PROCWAITMOUSE(0)

      PROCframesave(frame%)

      REM turn off grid and save state
      OG%=gridshow%
      gridshow%=0

      PROCchangemode(6,0)

      D$=FNgetdate
      IF session%=0 cursavedir$= "M7_"+LEFT$(D$,LEN(D$)-2)

      PROCsaveupdate(1)

      REPEAT
        PROCREADMOUSE

        IF MB%=4 THEN

          PROCWAITMOUSE(0)
          IF TY%=0 THEN done%=2
          C%=FNgetcontrol

          CASE C% OF
            WHEN 2 : REM bin
              save_bin%=(save_bin%+1) AND 1
              PROCsaveupdate(0)

            WHEN 3 : REM bmp
              save_bmp%=(save_bmp%+1) AND 1
              PROCsaveupdate(0)

            WHEN 4 : REM spr
              save_spr%=(save_spr%+1) AND 1
              PROCsaveupdate(0)

            WHEN 5 : REM dat
              save_dat%=(save_dat%+1) AND 1
              PROCsaveupdate(0)

            WHEN 6 : REM reset folder name
              cursavedir$= "M7_"+LEFT$(D$,LEN(D$)-2)
              PROCsaveupdate(1)

            WHEN 0 : REM save - if folder already exists then set it as the current save folder, otherwise set flag to create new folder
              IF cursavedir$<>"" THEN
                PROCcreatefolder(saverootdir$+cursavedir$)
                OSCLI "CD """+saverootdir$+cursavedir$+""""
                cursave$=saverootdir$+cursavedir$+"/"
                session%=1
                done%=1
              ENDIF

            WHEN 1 : REM cancel
              done%=2

          ENDCASE
          IF save_bin%+save_bmp%+save_spr%+save_dat%=0 THEN
            save_bin%=1
            PROCsaveupdate(0)
          ENDIF

        ELSE
          PROCupdatefilename
        ENDIF

      UNTIL done%>0

      PROCchangemode(7,1)

      IF done%=1 THEN
        REM update last session file
        f%=OPENOUT(@dir$+"telepaint_pref.ini")
        IF f% THEN
          PRINT#f%,cursave$+"M7_" + D$
          CLOSE#f%
        ENDIF

        CLS

        REM save frames
        frame%=frame_max%
        FOR I%=1 TO frame_max%
          PROCloadnextframe(1,0)
          IF save_bin%=1 THEN PROCsavebinaryfile(cursave$+"M7_" + D$ + "_" + STR$(frame%)+".BIN")
          IF save_bmp%=1 OSCLI "SCREENSAVE """+cursave$+"M7_" + D$ + "_" + STR$(frame%)+".BMP"" 0,0,1280,1000"
          WAIT 10
        NEXT

        PROCloadnextframe(1,0)

        IF save_spr%=1 OR save_dat%=1 THEN PROCsavespritefile(cursave$+"SPRITEDATA",save_spr%,save_dat%)

        menuext%=99
        PRINTTAB(9,10)gw$;CHR$(232);STRING$(18,CHR$(172));CHR$(180);CHR$(144+curcol%);
        FOR L%=11 TO 13
          PRINTTAB(9,L%)gw$;CHR$(234);STRING$(17," ");gw$;CHR$(181);CHR$(144+curcol%);
        NEXT
        PRINTTAB(9,14)gw$;CHR$(170);STRING$(18,CHR$(172));CHR$(165);CHR$(144+curcol%);

        REM READ FILES
        PRINTTAB(13,12)tg$;"FILES SAVED!";

        PROCWAITMOUSE(4)

        PROCWAITMOUSE(0)
      ENDIF

      gridshow%=OG%

      PROCmenurestore


      ENDPROC

      REM ##########################################################
      REM create folder ignore any errors
      DEF PROCcreatefolder(F$)
      LOCAL F%
      ON ERROR LOCAL F%=1
      IF F%=0 OSCLI "MD """+F$+""""
      ENDPROC

      REM ##########################################################
      REM update save screen options
      DEF PROCsaveupdate(M%)
      LOCAL SX%,SY%,SW%,SH%,D$

      SX%=100: SY%=100
      SW%=1078 : SH%=800
      menuYadd%=860

      IF M%=1 THEN
        PROCresetcontrols
        D$=LEFT$(cursavedir$+"                        ",24)

        GCOL 0,0
        RECTANGLE FILL SX%,SY%,SW%,SH%
        GCOL 0,15
        RECTANGLE SX%+8,SY%+8,SW%-16,SH%-16
        RECTANGLE SX%+10,SY%+10,SW%-20,SH%-20
        PROCgtext(" SAVE FILE OPTIONS ",SX%+236,menuYadd%,10,0,-60)
        PROCgtext("PROJECT NAME: ",SX%+40,menuYadd%,11,0,-48)
        PROCmenucontrol(6,"X",SX%+820,menuYadd%,0,9,0)
        PROCgtext(D$,SX%+40,menuYadd%,11,4,-80)
        PROCgtext("FILES TO SAVE:",SX%+40,menuYadd%,11,0,-48)
        PROCgtext("BIN:",SX%+40,menuYadd%,15,0,0)
        PROCgtext("(BIN FORMAT 1 KB)",SX%+340,menuYadd%,4,0,-48)
        PROCgtext("BMP:",SX%+40,menuYadd%,15,0,0)
        PROCgtext("(BMP FORMAT 1 MB)",SX%+340,menuYadd%,4,0,-48)
        PROCgtext("SPR:",SX%+40,menuYadd%,15,0,0)
        PROCgtext("(ALL SPRITE DATA)",SX%+340,menuYadd%,4,0,-48)
        PROCgtext("TXT:",SX%+40,menuYadd%,15,0,0)
        PROCgtext("(SPR DATA STATEMENTS)",SX%+340,menuYadd%,4,0,0)
        PROCmenucontrol(0,"  SAVE  ",SX%+100,SY%+100,12,10,4)
        PROCmenucontrol(1," CANCEL ",SX%+700,SY%+100,9,11,1)
      ENDIF

      menuYadd%=624
      PROCmenucontrol(2," "+CHR$(78+save_bin%*11)+" ",SX%+180,menuYadd%,8,11+4*save_bin%,1+save_bin%)
      PROCmenucontrol(3," "+CHR$(78+save_bmp%*11)+" ",SX%+180,menuYadd%-48,8,11+4*save_bmp%,1+save_bmp%)
      PROCmenucontrol(4," "+CHR$(78+save_spr%*11)+" ",SX%+180,menuYadd%-96,8,11+4*save_spr%,1+save_spr%)
      PROCmenucontrol(5," "+CHR$(78+save_dat%*11)+" ",SX%+180,menuYadd%-144,8,11+4*save_dat%,1+save_dat%)

      ENDPROC

      REM ##########################################################
      REM update save screen filename
      DEF PROCupdatefilename
      LOCAL SX%,SY%,OS%,K%,F$
      PRIVATE F%
      SY%=752
      SX%=140
      F$=cursavedir$
      REM PROCgtext(F$,SX%,SY%,11,4,0)
      OS%=LEN(F$)*32
      K%=INKEY(0)
      IF K%=-1 THEN
        F%=1-F%
        GCOL 0,F%*15
        IF LEN(F$)<24 LINE SX%+OS%,SY%-32,SX%+30+OS%,SY%-32

        WAIT 10
      ELSE
        REM check for numbers, letters, _ -
        IF (K%>47 AND K%<58) OR (K%>64 AND K%<91) OR (K%>96 AND K%<123) OR K%=95 OR K%=45 THEN
          IF LEN(F$)<24 F$=F$+CHR$(K%)
        ENDIF
        REM check if delete or backspace is pressed
        IF K%=135 OR K%=8 THEN
          IF LEN(F$)>0 F$=LEFT$(F$,LEN(F$)-1)
        ENDIF

        REM update dir name if changed
        IF F$<>cursavedir$ THEN
          PROCgtext(LEFT$(F$+"                        ",24),SX%,SY%,11,4,0)
          cursavedir$=F$
        ENDIF
      ENDIF
      ENDPROC

      REM ##########################################################
      REM save all frames to file, create session folder if not already exists
      DEF PROCsavemovie
      LOCAL D$,N$,done%,OG%,L%,C%,I%,F%

      PROCWAITMOUSE(0)

      REM PROCframesave(frame%)

      REM turn off grid and save state
      OG%=gridshow%
      gridshow%=0

      PROCchangemode(6,0)

      D$=FNgetdate
      IF session%=0 cursavedir$= "M7_"+LEFT$(D$,LEN(D$)-2)

      PROCsavemovieupdate(1)

      REPEAT
        PROCREADMOUSE

        IF MB%=4 THEN

          PROCWAITMOUSE(0)
          IF TY%=0 THEN done%=2
          C%=FNgetcontrol

          CASE C% OF
            WHEN 2 : REM movie frame data .MOV
              mov_frm%=1-mov_frm%
              PROCsavemovieupdate(0)

            WHEN 3 : REM movie bin data .BIN
              mov_dat%=1-mov_dat%
              PROCsavemovieupdate(0)

            WHEN 4 : REM frame bin data .BIN
              mov_bin%=1-mov_bin%
              PROCsavemovieupdate(0)

            WHEN 5 : REM movie frames .BMP
              mov_bmp%=1-mov_bmp%
              PROCsavemovieupdate(0)

            WHEN 6 : REM sprite data .SPR
              mov_spr%=1-mov_spr%
              PROCsavemovieupdate(0)

            WHEN 7 : REM text data .TXT
              mov_txt%=1-mov_txt%
              PROCsavemovieupdate(0)

            WHEN 8 : REM reset folder name
              cursavedir$= "M7_"+LEFT$(D$,LEN(D$)-2)
              PROCsavemovieupdate(1)

            WHEN 0 : REM save
              IF cursavedir$<>"" THEN
                PROCcreatefolder(saverootdir$+cursavedir$)
                PROCcreatefolder(saverootdir$+cursavedir$+"/MOVIE_BMP")
                OSCLI "CD """+saverootdir$+cursavedir$+""""
                session%=1
                cursave$=saverootdir$+cursavedir$+"/"
                done%=1
              ENDIF

            WHEN 1 : REM cancel
              done%=2

          ENDCASE
          IF mov_frm%+mov_bin%+mov_bmp%+mov_spr%+mov_txt%+mov_dat%=0 THEN
            mov_frm%=1
            PROCsavemovieupdate(0)
          ENDIF

        ELSE
          PROCupdatefilename
        ENDIF

      UNTIL done%>0

      PROCchangemode(7,1)

      IF done%=1 THEN

        REM update last session file
        f%=OPENOUT(@dir$+"telepaint_pref.ini")
        IF f% THEN
          PRINT#f%,cursave$+"M7_" + D$
          CLOSE#f%
        ENDIF

        REM movie frame and object position data
        IF mov_frm%=1 THEN
          f%=OPENOUT(cursave$+"M7_" + D$ + ".MOV")
          PRINT#f%,"*FRAME DATA*"
          PRINT#f%,STR$(movieframetotal%)
          IF movieframetotal%>-1 THEN
            FOR I%=0 TO movieframetotal%
              PRINT#f%,STR$(frmlist{(I%)}.x%)+","+STR$(frmlist{(I%)}.y%)+","+STR$(frmlist{(I%)}.b%)+","+STR$(frmlist{(I%)}.f%)
            NEXT
          ENDIF

          PRINT#f%,"*OBJECT DATA*"
          PRINT#f%,STR$(obj_lstcount%)
          IF obj_lstcount%>-1 THEN
            FOR I%=0 TO obj_lstcount%
              N$=STR$(objlist{(I%)}.obj%)+","
              N$+=STR$(objlist{(I%)}.type%)+","
              N$+=STR$(objlist{(I%)}.f%)+","
              N$+=STR$(objlist{(I%)}.rel%)+","
              N$+=STR$(objlist{(I%)}.hop%)+","
              N$+=STR$(objlist{(I%)}.parent%)+","
              N$+=STR$(objlist{(I%)}.x%)+","
              N$+=STR$(objlist{(I%)}.y%)+","
              N$+=STR$(objlist{(I%)}.h%)+","
              N$+=STR$(objlist{(I%)}.v%)+","
              N$+=STR$(objlist{(I%)}.m%)+","
              N$+=STR$(objlist{(I%)}.c%)+","
              N$+=STR$(objlist{(I%)}.u%)
              PRINT#f%,N$
            NEXT
          ENDIF
          CLOSE#f%

        ENDIF

        REM movie bin data and bmp export
        IF mov_dat%=1 OR mov_bmp%=1 THEN
          IF movieframetotal%>-1 THEN
            CLS
            FOR I%=0 TO movieframetotal%
              movieframe%=I%
              mmWX%=frmlist{(I%)}.x%
              mmWY%=frmlist{(I%)}.y%
              PROCobjtoworldmap
              N$=RIGHT$("0000"+STR$(I%),5)
              IF mov_dat%=1 PROCsavebinaryfile(cursave$+"M7_" + D$ + "_MOV_" + N$ +".BIN")
              IF mov_bmp%=1 OSCLI "SCREENSAVE """+cursave$+"MOVIE_BMP/M7_" + D$ + "_" + N$ +".BMP"" 0,0,1280,1000"
              REM SYS "SDL_SavePNG", @memhdc%, "C:\DATA\Retro\BeebEm\BB4WProjects\test.PNG"
              WAIT 10
            NEXT
          ELSE
            IF mov_dat%=1 PROCsavebinaryfile(cursave$+"M7_" + D$ + "_MOV.BIN")
            IF mov_bmp%=1 OSCLI "SCREENSAVE """+cursave$+"MOVIE_BMP/M7_" + D$ + ".BMP"" 0,0,1280,1000"
          ENDIF

        ENDIF

        REM save frame bin data
        IF mov_bin%=1 THEN
          menuext%=M_canvasmode%
          CLS
          frame%=frame_max%
          FOR I%=1 TO frame_max%
            PROCloadnextframe(1,0)
            N$=RIGHT$("00"+STR$(I%),3)
            PROCsavebinaryfile(cursave$+"M7_" + D$ + "_FRM_" + N$ +".BIN")
            WAIT 10
          NEXT
        ENDIF


        REM sprite data
        IF mov_spr%=1 PROCsavespritefile(cursave$+"SPRITEDATA",mov_spr%,0)

        REM text data
        IF obj_txtcur%>-1 THEN
          f%=OPENOUT(cursave$+"TEXTDATA.TXT")
          PRINT#f%,STR$(obj_txtcur%)
          FOR I%=0 TO obj_txtcur%
            PRINT#f%,txtlist$(I%)
          NEXT
          CLOSE#f%

        ENDIF

        menuext%=99
        PRINTTAB(9,10)gw$;CHR$(232);STRING$(18,CHR$(172));CHR$(180);CHR$(144+curcol%);
        FOR L%=11 TO 13
          PRINTTAB(9,L%)gw$;CHR$(234);STRING$(17," ");gw$;CHR$(181);CHR$(144+curcol%);
        NEXT
        PRINTTAB(9,14)gw$;CHR$(170);STRING$(18,CHR$(172));CHR$(165);CHR$(144+curcol%);

        REM READ FILES
        PRINTTAB(13,12)tg$;"FILES SAVED!";

        PROCWAITMOUSE(4)

        PROCWAITMOUSE(0)
        menuext%=M_moviemode%
      ENDIF

      gridshow%=OG%

      PROCmenurestore


      ENDPROC


      REM ##########################################################
      REM update save movie screen options
      DEF PROCsavemovieupdate(M%)
      LOCAL SX%,SY%,SW%,SH%,D$

      SX%=100: SY%=100
      SW%=1078 : SH%=800
      menuYadd%=860

      IF M%=1 THEN
        PROCresetcontrols
        D$=LEFT$(cursavedir$+"                        ",24)

        GCOL 0,0
        RECTANGLE FILL SX%,SY%,SW%,SH%
        GCOL 0,15
        RECTANGLE SX%+8,SY%+8,SW%-16,SH%-16
        RECTANGLE SX%+10,SY%+10,SW%-20,SH%-20
        REM t$,x%,y%,tc%,bc%
        PROCgtext(" SAVE MOVIE OPTIONS ",SX%+236,menuYadd%,10,0,-60)
        PROCgtext("MOVIE NAME: ",SX%+40,menuYadd%,15,0,-48)
        PROCmenucontrol(8,"X",SX%+820,menuYadd%,0,9,0)
        PROCgtext(D$,SX%+40,menuYadd%,11,4,-80)
        PROCgtext("FILES TO SAVE:",SX%+40,menuYadd%,15,0,-48)
        PROCgtext("MOV:",SX%+40,menuYadd%,15,0,0)
        PROCgtext("(MOVIE FRAME DATA)",SX%+340,menuYadd%,4,0,-48)
        PROCgtext("BIN:",SX%+40,menuYadd%,15,0,0)
        PROCgtext("(MOVIE BIN DATA 1 KB)",SX%+340,menuYadd%,4,0,-48)
        PROCgtext("BIN:",SX%+40,menuYadd%,15,0,0)
        PROCgtext("(FRAME BIN DATA 1 KB)",SX%+340,menuYadd%,4,0,-48)
        PROCgtext("BMP:",SX%+40,menuYadd%,15,0,0)
        PROCgtext("(MOVIE BMP FILES 1 MB)",SX%+340,menuYadd%,4,0,-48)
        PROCgtext("SPR:",SX%+40,menuYadd%,15,0,0)
        PROCgtext("(SPRITE DATA FILE)",SX%+340,menuYadd%,4,0,-48)
        PROCgtext("TXT:",SX%+40,menuYadd%,15,0,0)
        PROCgtext("(TEXT DATA FILE)",SX%+340,menuYadd%,4,0,-48)

        PROCmenucontrol(0,"  SAVE  ",SX%+100,SY%+100,12,10,4)
        PROCmenucontrol(1," CANCEL ",SX%+700,SY%+100,9,11,1)


      ENDIF
      menuYadd%=624
      PROCmenucontrol(2," "+CHR$(78+mov_frm%*11)+" ",SX%+180,menuYadd%,8,11+4*mov_frm%,1+mov_frm%)
      PROCmenucontrol(3," "+CHR$(78+mov_dat%*11)+" ",SX%+180,menuYadd%-48,8,11+4*mov_dat%,1+mov_dat%)
      PROCmenucontrol(4," "+CHR$(78+mov_bin%*11)+" ",SX%+180,menuYadd%-96,8,11+4*mov_bin%,1+mov_bin%)
      PROCmenucontrol(5," "+CHR$(78+mov_bmp%*11)+" ",SX%+180,menuYadd%-144,8,11+4*mov_bmp%,1+mov_bmp%)
      PROCmenucontrol(6," "+CHR$(78+mov_spr%*11)+" ",SX%+180,menuYadd%-192,8,11+4*mov_spr%,1+mov_spr%)
      PROCmenucontrol(7," "+CHR$(78+mov_txt%*11)+" ",SX%+180,menuYadd%-236,8,11+4*mov_txt%,1+mov_txt%)


      ENDPROC


      REM ##########################################################
      REM import picture to one or more frames
      DEF PROCimportimage

      LOCAL GX%,GY%

      REM menuext%=96
      done%=0

      PROCloadfile(1)

      REM after loadfile menuext% returns: No file: 94, single image: 95, grid: 96, series: 97

      PROCWAITMOUSE(0)

      CASE menuext% OF
        WHEN 94 : REM no file
          GX%=0
          REPEAT
            PROCREADMOUSE
            WAIT 5
            GX%+=1
          UNTIL GX%>200 OR MB%<>0

        WHEN 95,96 : REM single / grid

          GX%=1
          GY%=1

          IF menuext%=96 THEN
            GX%=OLD_TX%
            GY%=OLD_TY%
          ENDIF

          startx%=-1
          starty%=-1
          OLDMX%=MX%
          OLDMY%=MY%

          PROCprint40(0,"Select Frame: "+RIGHT$("0"+STR$(frame%),2))

          REPEAT
            PROCREADMOUSE

            MX%=(MX% DIV 2)*2
            MY%=(MY% DIV 2)*2
            REM start a new selection
            IF MB%=4 THEN
              startx%=MX%
              starty%=MY%
              OLDMX%=MX%
              OLDMY%=MY%
              gridsx%=(MX%-startx%)/GX%
              gridsy%=(MY%-starty%)/GY%

              PROCupdategrid(startx%,starty%,gridsx%,gridsy%,GX%,GY%,3,15)
              REM GCOL 3,15
              REM FOR X%=0 TO GX%
              REM LINE startx%+X%*gridsx%,starty%,startx%+X%*gridsx%,starty%+gridsy%*GY%
              REM NEXT
              REM FOR Y%=0 TO GY%
              REM LINE startx%,starty%+Y%*gridsy%,startx%+gridsx%*GX%,starty%+Y%*gridsy%
              REM NEXT

              REPEAT
                PROCREADMOUSE
                IF OLDMX%<>MX% OR OLDMY%<>MY% THEN
                  PROCupdategrid(startx%,starty%,gridsx%,gridsy%,GX%,GY%,3,15)

                  gridsx%=(MX%-startx%)/GX%
                  gridsy%=(MY%-starty%)/GY%

                  PROCupdategrid(startx%,starty%,gridsx%,gridsy%,GX%,GY%,3,15)

                  OLDMX%=MX%
                  OLDMY%=MY%
                ELSE
                  WAIT 4
                ENDIF

              UNTIL MB%=0

              IF gridsx%<>0 AND gridsy%<>0 THEN
                x1%=MX%-startx%
                y1%=MY%-starty%

                REM option to move frame layout
                PROCprint40(0,"Move frame?  Y   N")
                GCOL 3,10
                RECTANGLE FILL 376,960,108,40

                GCOL 3,9
                RECTANGLE FILL 504,960,108,40

                PROCWAITMOUSE(4)
                PROCWAITMOUSE(0)

                IF TY%=0 AND TX%>11 AND TX%<15 THEN

                  REM move frame
                  REPEAT
                    PROCREADMOUSE
                    IF OLDMX%<>MX% OR OLDMY%<>MY% THEN
                      PROCupdategrid(startx%,starty%,gridsx%,gridsy%,GX%,GY%,3,15)

                      startx%=MX%
                      starty%=MY%

                      PROCupdategrid(startx%,starty%,gridsx%,gridsy%,GX%,GY%,3,15)
                    ELSE
                      WAIT 4

                    ENDIF
                  UNTIL MB%=4
                  PROCWAITMOUSE(0)
                ENDIF

                PROCupdategrid(startx%,starty%,gridsx%,gridsy%,GX%,GY%,3,15)

                REM process selection(s)
                x2%=startx%+x1%
                y2%=starty%+y1%
                x1%=startx%
                y1%=starty%

                IF x1%>x2% THEN SWAP x1%,x2%
                IF y1%<y2% THEN SWAP y1%,y2%

                gridsx%=ABS(gridsx%)
                gridsy%=ABS(gridsy%)

                startx%=x1%

                IF gridsx%>gridsy% THEN
                  stepx=gridsx%/78
                  stepr=gridsx%/gridsy%
                  stepy=gridsy%/72*stepr
                ELSE
                  stepy=gridsy%/72
                  stepr=gridsy%/gridsx%
                  stepx=gridsx%/78*stepr
                ENDIF
                threshold=stepx*stepy/4


                FOR Y%=1 TO GY%
                  FOR X%=0 TO GX%-1

                    XS%=x1%+gridsx%*X%
                    XE%=XS%+gridsx%

                    YS%=y1%-gridsy%*Y%
                    YE%=YS%+gridsy%
                    REMPRINTTAB(0,0)"stepx: ";RIGHT$("000000000"+STR$(stepx),10)
                    REMPRINTTAB(0,1)"stepy: ";RIGHT$("000000000"+STR$(stepy),10)
                    REMPRINTTAB(0,2)"stepr: ";RIGHT$("000000000"+STR$(stepr),10)
                    REMPRINTTAB(0,3)"xs: ";RIGHT$("000"+STR$(XS%),4);" xe: ";RIGHT$("000"+STR$(XE%),4)
                    REMPRINTTAB(0,4)"ys: ";RIGHT$("000"+STR$(YS%),4);" ye: ";RIGHT$("000"+STR$(YE%),4)

                    PROCprint40(0,"Processing selection, Frame: "+STR$(frame%))
                    REM A=GET

                    px%=0
                    FOR X=XS% TO XE% STEP stepx
                      py%=74
                      FOR Y=YS% TO YE% STEP stepy

                        REM average colour for each region
                        avg%=0
                        FOR PX=X TO X+stepx STEP 2
                          FOR PY=Y TO Y+stepy STEP 2
                            IF POINT(PX,PY)<>0 THEN avg%+=1
                            IF avg%>=threshold THEN EXIT FOR
                          NEXT
                          IF avg%>=threshold THEN EXIT FOR
                        NEXT

                        IF avg%>=threshold THEN
                          PROCpoint_buf(px%+2, py%, 1,frame%)
                          REM PLOT 69,px%+2, py%+3
                        ENDIF

                        py%-=1
                        REM PRINTTAB(0,0)"px: "+RIGHT$("000"+STR$(px%),4)
                        REM PRINTTAB(0,1)"py: "+RIGHT$("000"+STR$(py%),4)

                        REM A=GET

                      NEXT
                      px%+=1
                    NEXT

                    IF menuext%=96 THEN
                      IF frame%<frame_max% THEN
                        frame%+=1
                      ELSE
                        done%=1
                        EXIT FOR
                      ENDIF
                    ENDIF
                  NEXT
                  IF menuext%=96 THEN
                    x1%=startx%
                    IF done%=1 THEN EXIT FOR
                  ENDIF
                NEXT
                REM next image
                IF menuext%=95 THEN
                  PROCprint40(0,"Complete! Process next frame?  Y   N")
                  GCOL 3,10
                  RECTANGLE FILL 948,960,108,40

                  GCOL 3,9
                  RECTANGLE FILL 1076,960,108,40

                  PROCWAITMOUSE(4)
                  PROCWAITMOUSE(0)

                  IF TY%=0 AND TX%>29 AND TX%<33 THEN

                    REM reset selection
                    startx%=-1
                    IF frame%<frame_max% THEN
                      frame%+=1
                    ELSE
                      done%=1
                    ENDIF

                  ELSE
                    done%=1

                  ENDIF
                ENDIF
              ELSE
                PROCprint40(0,"ERROR! Capture box size too small.")
                PROCWAITMOUSE(4)
                PROCWAITMOUSE(0)

              ENDIF
              IF done%=0 PROCprint40(0,"Select Frame: "+RIGHT$("0"+STR$(frame%),2))

            ELSE
              WAIT 2
            ENDIF

          UNTIL done%=1

      ENDCASE
      PROCWAITMOUSE(0)
      PROCchangemode(7,1)
      frame%=0
      PROCloadnextframe(1,0)
      menuext%=M_canvasmode%

      ENDPROC

      REM ##########################################################
      REM import a series of 78x72 BMP images
      DEF PROCimportseries(F$,S%,frmStart%,frmEnd%)
      LOCAL F%,NAME$
      REM OSCLI "DISPLAY """+curdir$+n$(SEL%)+""" 0,0"

      FOR F%=frmStart% TO frmEnd%

        NAME$=F$+RIGHT$("000"+STR$(S%),4)+".BMP"

        fnum=OPENIN(curdir$+NAME$)
        IF fnum<>0 THEN
          CLOSE#fnum

          PROCloadbmptobuf(curdir$+NAME$,1)

          IF menuext%=95 THEN
            REM adjust for correct line byte width multiple of 4
            line_wid%=bmp_imgwid%*3
            WHILE line_wid% MOD 4<>0
              line_wid%+=1
            ENDWHILE

            FOR X%=0 TO 77
              FOR Y%=0 TO 71
                col%=0
                IF X%<bmp_imgwid% AND Y%<bmp_imghgt% THEN
                  ofs%=bmp_imgofs%+X%*3+Y%*line_wid%
                  col%=import_buffer%%?ofs%+import_buffer%%?(ofs%+1)+import_buffer%%?(ofs%+2)
                ENDIF
                IF col%>0 THEN
                  PROCpoint_buf(X%+2, 74-Y%, 1,F%)
                ELSE
                  PROCpoint_buf(X%+2, 74-Y%, 0,F%)
                ENDIF
              NEXT
            NEXT
          ENDIF
          S%+=1
        ELSE
          EXIT FOR
        ENDIF
      NEXT

      ENDPROC

      REM ##########################################################
      REM update grid
      DEF PROCupdategrid(startx%,starty%,gridsx%,gridsy%,GX%,GY%,GC1%,GC2%)
      GCOL GC1%,GC2%
      FOR X%=0 TO GX%
        LINE startx%+X%*gridsx%,starty%,startx%+X%*gridsx%,starty%+gridsy%*GY%
      NEXT
      FOR Y%=0 TO GY%
        LINE startx%,starty%+Y%*gridsy%,startx%+gridsx%*GX%,starty%+Y%*gridsy%
      NEXT
      ENDPROC

      REM ##########################################################
      REM import picture to one or more frames
      DEF PROCimportsprite(mode%,lrg%)

      LOCAL X%,Y%,x%,y%,px%,py%,GX%,GY%,ST%,startx%,starty%,gridsx%,gridsy%,done%,shift%,ctrl%,autocap%
      LOCAL gf%,nf%,fmax%,delay%,gifobject,initimage%,coltop%,oldtop%,xsize%,ysize%,oldsprc%,updatetext%

      menuext%=96
      done%=0
      gf%=0
      coltop%=253
      oldtop%=1
      autocap%=0

      REM after loadfile menuext% returns: No file: 94, single image: 95, grid: 96
      PROCloadfile(mode%)

      IF menuext%=94 THEN
        CLS
        PRINTTAB(0,0)tr$;" *** NO FILE LOADED *** "

      ELSE

        PROC_imgInit

        IF mode%=7 THEN
          CLS
          VDU 23,1,0;0;0;0; : REM Disable cursor

          PRINTTAB(0,0)tg$;"LOADING GIF, PLEASE WAIT";CHR$(scode&(0));"...";CHR$(scode&(1));"  ";

          gifobject=FN_imgLoadAnimatedGIF(bmpload$)

          REM get gif frame count and initialise first frame
          IF gifobject<>0 THEN
            sprlrg_count%=0
            delay%=FN_imgFrame(gifobject, gf%)
            IF delay%=0 THEN
              menuext%=94
            ELSE
              REPEAT
                fmax%=gf%
                gf%+=1
                delay%=FN_imgFrame(gifobject, gf%)
              UNTIL delay%=0
              gf%=0
              delay%=FN_imgFrame(gifobject, gf%)
            ENDIF
          ELSE
            menuext%=94
          ENDIF
          VDU 23,1,1;0;0;0;  : REM Enable cursor

        ELSE
          PROCloadbmptobuf(bmpload$,1)
        ENDIF

        REM if bmp or gif loaded then display initial image
        IF menuext%=95 THEN
          PROCchangemode(6,1)
          initimage%=1

          startx%=-1
          starty%=-1
          IF lrg%=0 THEN
            gridsx%=78
            gridsy%=94
            xsize%=39
            ysize%=47
          ELSE
            xsize%=lrgx%
            ysize%=lrgy%
            gridsx%=lrgx%*2
            gridsy%=lrgy%*2
            oldsprc%=sprite_cur%
            sprite_cur%=0
          ENDIF

          REPEAT
            IF initimage%=1 THEN
              CLS
              REM load current gif frame
              IF mode%=7 THEN
                PROC_imgPlot(gifobject, 638, 498, 1, 1, 0)
                OSCLI "SCREENSAVE """+@tmp$+"M7_TMP.BMP"" 0,0,1280,1000"
                PROCloadbmptobuf(@tmp$+"M7_TMP.BMP",0)
              ELSE
                OSCLI "MDISPLAY "+STR$~import_buffer%%
              ENDIF

              REM adjust for correct byte width multiple of 4
              line_wid%=bmp_imgwid%*3
              WHILE line_wid% MOD 4<>0
                line_wid%+=1
              ENDWHILE

              initimage%=0
              updatetext%=1
              IF autocap%=0 THEN
                MX%=(MX% DIV 2)*2
                MY%=(MY% DIV 2)*2
                OLDMX%=-1
                OLDMY%=MY%
                GX%=0
                GY%=0
              ENDIF
            ENDIF

            REM update image & sprite details
            PROCprint40(0,"F:"+STR$(gf%+1)+" Select Sprite: "+RIGHT$("00"+STR$(sprite_cur%+1),3))
            GCOL 0,2
            RECTANGLE FILL 946,602,332,396

            REPEAT
              REM update details
              IF updatetext% THEN
                IF lrg%=0 THEN
                  PROCprint40(24,"CA: "+RIGHT$("00"+STR$(coltop%),3))
                ELSE
                  PROCprint40(24,"CA: "+RIGHT$("00"+STR$(coltop%),3)+" SPR W: "+STR$(lrgx%)+" H:"+STR$(lrgy%))
                ENDIF
                updatetext%=0
              ENDIF

              PROCREADMOUSE
              MX%=(MX% DIV 2)*2
              MY%=(MY% DIV 2)*2
              REM start a new selection

              IF OLDMX%<>MX% OR OLDMY%<>MY% OR oldtop%<>coltop% OR startx%=-1 THEN
                IF OLDMX%<>-1 PROCupdategrid(OLDMX%,OLDMY%,gridsx%,gridsy%,1,1,3,15)
                x%=MX% DIV 2
                y%=MY% DIV 2

                ST%=0
                px%=0
                startx%=x%
                FOR X%=x% TO x%+xsize%
                  py%=384
                  FOR Y%=y% TO y%+ysize%

                    REM IF POINT(X%,Y%)<>0 THEN
                    col%=0
                    IF X%>-1 AND X%<bmp_imgwid% AND Y%>-1 AND Y%<bmp_imghgt% THEN
                      ofs%=bmp_imgofs%+X%*3+Y%*line_wid%
                      col%=import_buffer%%?ofs%+import_buffer%%?(ofs%+1)+import_buffer%%?(ofs%+2)
                      IF col%<coltop% col%=0
                    ENDIF
                    IF px%<320 AND py%>0 THEN
                      GCOL 0,SGN(col%)*15
                      RECTANGLE FILL 952+px%,992-py%,8
                    ENDIF
                    IF lrg%=0 THEN
                      spr_tmp&(ST%)=SGN(col%)
                    ELSE
                      IF ST%<30001 sprlrg&(sprite_cur%,ST%)=SGN(col%)
                    ENDIF
                    ST%+=1

                    py%-=8
                  NEXT
                  px%+=8
                NEXT

                PROCupdategrid(MX%,MY%,gridsx%,gridsy%,1,1,3,15)
                OLDMX%=MX%
                OLDMY%=MY%
                oldtop%=coltop%

              ELSE
                WAIT 2
                shift%=INKEY(-1)
                ctrl%=INKEY(-2)
                GX%=0
                GY%=0

                REM check for cursor keys and enter to capture sprite, esc cancels
                IF INKEY(-26) THEN
                  IF (shift%+ctrl%=0) AND MX%>0 MX%-=2
                  IF lrg%=1 THEN
                    IF shift% GX%=-1
                    IF ctrl% GX%=-10
                  ENDIF
                ENDIF
                IF INKEY(-122) THEN
                  IF (shift%+ctrl%=0) AND MX%<1278 MX%+=2
                  IF lrg%=1 THEN
                    IF shift% GX%=1
                    IF ctrl% GX%=10
                  ENDIF
                ENDIF
                IF INKEY(-58) THEN
                  IF (shift%+ctrl%=0) AND MY%<998 MY%+=2
                  IF lrg%=1 THEN
                    IF shift% GY%=1
                    IF ctrl% GY%=10
                  ENDIF
                ENDIF
                IF INKEY(-42) THEN
                  IF (shift%+ctrl%=0) AND MY%>0 MY%-=2
                  IF lrg%=1 THEN
                    IF shift% GY%=-1
                    IF ctrl% GY%=-10
                  ENDIF
                ENDIF

                IF OLDMX%<>MX% OR OLDMY%<>MY% MOUSE TO MX%,MY%

                REM home / end to change color check threshold to fine tune pixel capture
                IF INKEY(-63) AND coltop%<757 THEN
                  coltop%+=63
                  updatetext%=1
                  PROCWAITNOKEY(-63,0)
                ENDIF
                IF INKEY(-106) AND coltop%>1 THEN
                  coltop%-=63
                  updatetext%=1
                  PROCWAITNOKEY(-106,0)
                ENDIF

                REM enter to capture sprite, esc to cancel
                IF INKEY(-74) THEN
                  MB%=4
                  IF shift% AND mode%=7 autocap%=1
                ENDIF
                IF INKEY(-113) initimage%=2

                REM pgup / pgdn to select next image if gif loaded
                IF mode%=7 THEN
                  IF INKEY(-64) nf%=gf%+1
                  IF INKEY(-79) nf%=gf%-1
                  IF nf%<0 THEN nf%=fmax%
                  IF nf%>fmax% THEN nf%=0
                  IF gf%<>nf% THEN
                    gf%=nf%
                    delay%=FN_imgFrame(gifobject, gf%)
                    initimage%=1
                    WAIT 10
                  ENDIF
                ENDIF

                REM calulate large sprite box size change if any and update box
                IF lrg%=1 THEN
                  IF GX%<>0 THEN
                    xsize%+=GX%
                    IF xsize%<10 xsize%=10
                    IF xsize%*ysize%>30000 xsize%=30000/ysize%
                    IF (MX% DIV 2)+xsize%<0 xsize%=-(MX% DIV 2)
                    IF (MX% DIV 2)+xsize%>639 xsize%=639-(MX% DIV 2)
                  ENDIF
                  IF GY%<>0 THEN
                    ysize%+=GY%
                    IF ysize%<10 ysize%=10
                    IF xsize%*ysize%>30000 ysize%=30000/xsize%
                    IF (MY% DIV 2)+ysize%<0 ysize%=-(MY% DIV 2)
                    IF (MY% DIV 2)+ysize%>499 ysize%=499-(MY% DIV 2)
                  ENDIF

                  IF ysize%<>lrgy% OR xsize%<>lrgx% THEN
                    lrgx%=xsize%
                    lrgy%=ysize%
                    gridsx%=lrgx%*2
                    gridsy%=lrgy%*2
                    initimage%=1
                    WAIT 5
                  ENDIF
                ENDIF
              ENDIF
              IF autocap%=1 THEN
                MB%=4
              ENDIF
            UNTIL MB%=4 OR initimage%
            PROCWAITNOKEY(-74,0)
            PROCWAITNOKEY(-113,0)

            REM PROCupdategrid(MX%,MY%,gridsx%,gridsy%,GX%,GY%,3,15)

            IF initimage%=0 THEN
              PROCWAITMOUSE(0)

              REM process selection(s)
              IF lrg%=0 THEN
                ST%=0
                FOR x%=0 TO xsize%
                  FOR y%=ysize% TO 0 STEP -1
                    PROCpoint_sprbuf(x%,y%,spr_tmp&(ST%),sprite_cur%)
                    ST%+=1
                  NEXT
                NEXT
                PROCupdatespritesize(sprite_cur%,0)
              ENDIF

              REM reset selection
              startx%=-1
              IF lrg%=0 THEN
                IF sprite_cur%<sprite_max%-1 THEN
                  sprite_cur%+=1
                  initimage%=1
                ELSE
                  done%=1
                ENDIF
              ELSE
                IF sprite_cur%<sprlrg_max%-1 THEN
                  sprite_cur%+=1
                  initimage%=1
                ELSE
                  done%=1
                ENDIF
              ENDIF

              REM advance gif frame
              IF mode%=7 AND initimage%=1 THEN
                sprlrg_count%+=1
                gf%+=1
                IF gf%>fmax% THEN gf%=0
                nf%=gf%
                delay%=FN_imgFrame(gifobject, gf%)
                IF autocap%=1 AND gf%=0 done%=1
              ENDIF

            ENDIF
            IF initimage%=2 THEN
              done%=1
            ENDIF

          UNTIL done%=1

          PROCchangemode(7,1)

        ELSE
          IF mode%=7 THEN
            PRINTTAB(0,0)tr$;" *** ERROR LOADING GIF *** "
          ELSE
            PRINTTAB(0,0)tr$;" *** ERROR LOADING BMP *** "
          ENDIF
        ENDIF

        PROC_imgExit

      ENDIF

      IF lrg%<>0 THEN
        sprite_cur%=oldsprc%
      ELSE
        IF sprite_cur%>0 sprite_cur%-=1
      ENDIF

      REM delay exit to display info
      IF menuext%=94 THEN
        gf%=0
        REPEAT
          PROCREADMOUSE
          WAIT 5
          gf%+=1
        UNTIL gf%>100 OR MB%<>0
      ENDIF
      PROCWAITMOUSE(0)

      menuext%=M_sprites%

      ENDPROC

      REM ##########################################################
      REM load gif and save each frame as bmp
      DEF PROCexportgif
      LOCAL f%,delay%,N$,gifobject
      bmpload$=""

      PROCloadfile(7)
      CLS

      IF bmpload$<>"" THEN
        VDU 23,1,0;0;0;0; : REM Disable cursor

        PRINTTAB(0,0)tg$;"LOADING GIF, PLEASE WAIT";CHR$(scode&(0));"...";CHR$(scode&(1));"  ";

        PROC_imgInit
        gifobject=FN_imgLoadAnimatedGIF(bmpload$)

        IF gifobject<>0 THEN
          CLS
          D$=FNgetdate
          IF session%=0 THEN
            cursavedir$= "M7_"+LEFT$(D$,LEN(D$)-2)
            session%=1
          ENDIF

          PROCcreatefolder(saverootdir$+cursavedir$)
          PROCcreatefolder(saverootdir$+cursavedir$+"/GIF_BMP")
          cursave$=saverootdir$+cursavedir$+"/"

          OSCLI "CD """+saverootdir$+cursavedir$+""""
          f%=0
          REPEAT
            delay% = FN_imgFrame(gifobject, f%)

            IF delay%>0 THEN
              PROC_imgPlot(gifobject, 638, 498, 1, 1, 0)
              WAIT 5

              N$=RIGHT$("0000"+STR$(f%),5)
              OSCLI "SCREENSAVE """+cursave$+"/GIF_BMP/M7_" + D$ + "_" + N$ +".BMP"" 0,0,1280,1000"

              f%+=1
              WAIT 5
              PROCREADMOUSE
            ENDIF
          UNTIL delay%=0 OR MB%<>0

          PRINTTAB(0,0)tg$;" *** GIF EXPORT COMPLETE *** "

        ELSE
          PRINTTAB(0,0)tr$;" *** ERROR LOADING GIF *** "
        ENDIF

        PROC_imgExit
        VDU 23,1,1;0;0;0;  : REM Enable cursor

      ELSE
        PRINTTAB(0,0)tr$;" *** NO FILE LOADED *** "
      ENDIF

      f%=0
      REPEAT
        PROCREADMOUSE
        WAIT 5
        f%+=1
      UNTIL f%>100 OR MB%<>0
      PROCWAITMOUSE(0)

      ENDPROC

      REM ##########################################################
      REM load font file into font array
      DEF PROCloadfont(name$)

      REM clear existing font data
      FOR I%=0 TO fontmax%
        fonts{(I%)}.a%=0
      NEXT

      REM read font file
      IF INSTR(name$,".M7F")=0 AND INSTR(name$,".m7f")=0 THEN
        name$=@dir$+"M7_FONTS/"+name$+".M7F"
      ENDIF
      f%=OPENIN(name$)
      IF f% THEN
        INPUT#f%,i$
        IF i$="TELEPAINT_FONT" THEN
          INPUT#f%,i$
          fonthgt%=VAL(i$)
          IF fonthgt%>0 THEN
            REPEAT
              INPUT#f%,i$
              a%=VAL(i$)-32
              fonts{(a%)}.a%=1
              INPUT#f%,i$
              fonts{(a%)}.w%=VAL(i$)

              INPUT#f%,i$
              FOR I%=1 TO fonts{(a%)}.w%*fonthgt%
                fonts{(a%)}.d%(I%-1)=VAL(MID$(i$,I%,1))
              NEXT
            UNTIL EOF#f%
          ENDIF
        ENDIF
        CLOSE#f%

        REM add default space char if not exists in font file
        IF fonts{(0)}.a%=0 THEN
          fonts{(0)}.a%=1
          fonts{(0)}.w%=2
        ENDIF
      ELSE
        CLS
        PRINT"Unable to load file:"
        PRINT name$
        PRINT
        PRINT "Press any key"
        a%=GET
      ENDIF
      ENDPROC

      REM ##########################################################
      REM animate all frames in sequence from 1 to frame_max%
      DEF PROCplay

      PROCframesave(frame%)
      PROCWAITMOUSE(0)

      VDU 23,1,0;0;0;0; : REM Disable cursor

      D%=0

      frame%=frame_max%
      REPEAT
        PROCloadnextframe(1,0)
        FOR I%=0 TO 9
          PROCREADMOUSE
          IF MB%<>0 THEN D%=1
          WAIT 2
        NEXT
      UNTIL D%
      PROCWAITMOUSE(0)

      VDU 23,1,1;0;0;0; : REM Enable cursor

      ENDPROC

      REM ##########################################################
      REM display a help screen
      DEF PROCshowhelp
      LOCAL H$,C%,Y%

      PROCprint40(24,ty$+"TelePaint"+tm$+version$+tc$+"by 4thStone & Pixelblip")

      OSCLI "SCREENSAVE """+@tmp$+"M7_TMP.BMP"" 0,0,1280,1000"
      PROCchangemode(6,1)
      OSCLI "DISPLAY """+@tmp$+"M7_TMP.BMP"" 0,0"

      REM control code help
      GCOL 0,10
      PROChelpbox(0,0,1,10)
      PROChelpdot(0,23,10)
      PROChelpvline(0,23,10)
      PRINTTAB(1,23)"Show control codes";

      REM colour selector help
      GCOL 0,11
      PROChelpbox(1,0,13,11)
      PROChelpdot(1,21,11)
      PROChelpvline(1,21,11)
      PRINTTAB(2,21)"Colour Select/Toggle 'T'ext 'G'raphic";

      REM paint tool
      GCOL 0,10
      PROChelpbox(15,0,1,10)
      PROChelpdot(12,1,10)
      PROChelpvline(15,1,10)
      PROChelphline(12,1,3,10)
      REM LINE 494,944,400,944
      PRINTTAB(2,1)"Paint Tool";

      REM dither tool
      GCOL 0,11
      PROChelpbox(16,0,1,11)
      PROChelpdot(13,3,11)
      PROChelpvline(16,3,11)
      PROChelphline(13,3,3,11)
      PRINTTAB(2,3)"Dither Tool";
      PRINTTAB(2,4)"Toggle 1..5";

      REM copy / paste tool
      GCOL 0,10
      PROChelpbox(17,0,1,10)
      PROChelpdot(12,6,10)
      PROChelpvline(17,6,10)
      PROChelphline(12,6,5,10)
      PRINTTAB(2,6)"Copy/Paste";
      PRINTTAB(2,7)"C - Copy Region";
      PRINTTAB(2,8)"P - Paste";

      REM copy / paste tool
      GCOL 0,11
      PROChelpbox(18,0,1,11)
      PROChelpdot(11,10,11)
      PROChelpvline(18,10,11)
      PROChelphline(11,10,7,11)
      PRINTTAB(2,10)"Fill Tool";

      REM special sub menu
      GCOL 0,10
      PROChelpbox(19,0,1,10)
      PROChelpdot(18,12,10)
      PROChelpvline(19,12,10)
      PROChelphline(18,12,1,10)
      PRINTTAB(2,12)"Special Sub Menu";

      REM erase toggle
      PROChelpbox(21,0,1,11)
      PROChelpdot(19,14,11)
      PROChelpvline(21,14,11)
      PROChelphline(19,14,2,11)
      PRINTTAB(2,14)"Draw Erase Toggle";

      REM play animate
      PROChelpbox(39,0,1,10)
      PROChelpdot(39,19,10)
      PROChelpvline(39,19,10)
      PRINTTAB(25,19)"Play Animation";

      REM current frame
      PROChelpbox(36,0,3,11)
      PROChelpdot(38,17,11)
      PROChelpvline(38,17,11)
      PRINTTAB(25,17)"Current Frame";

      REM animate frame advance
      PROChelpbox(34,0,1,10)
      PROChelpdot(37,14,10)
      PROChelpvline(34,1,10)
      PROChelpvline2(37,1,13,10)
      PROChelphline(34,1,3,10)
      PRINTTAB(23,14)"Animate Toggle";
      PRINTTAB(23,15)"Advance On Draw";

      REM save
      PROChelpbox(32,0,1,11)
      PROChelpdot(32,2,11)
      PROChelpvline(32,2,11)
      PRINTTAB(33,2)"Save";

      REM load
      PROChelpbox(31,0,1,10)
      PROChelpdot(31,3,10)
      PROChelpvline(31,3,10)
      PRINTTAB(32,3)"Load";

      REM foreground col
      PROChelpbox(29,0,1,11)
      PROChelpdot(29,5,11)
      PROChelpvline(29,5,11)
      PRINTTAB(30,5)"Txt/Gph";
      PRINTTAB(30,6)"Codes";

      REM new background
      PROChelpbox(28,0,1,10)
      PROChelpdot(28,8,10)
      PROChelpvline(28,8,10)
      PRINTTAB(29,8)"Gph+New";
      PRINTTAB(29,9)"Bckgrnd";

      REM clearscreen / dupscreen
      PROChelpbox(27,0,1,11)
      PROChelpdot(27,11,11)
      PROChelpvline(27,11,11)
      PRINTTAB(28,11)"Clr Scrn";
      PRINTTAB(28,12)"Dupe Scrn";

      REM undo / redo
      PROChelpbox(23,0,3,10)
      PROChelpdot(22,2,10)
      PROChelpvline(23,1,10)
      PROChelpvline2(22,1,1,10)
      PROChelphline(22,1,1,10)
      PRINTTAB(23,2)"Undo";
      PRINTTAB(23,3)"Redo";

      PROCWAITMOUSE(4)
      PROCWAITMOUSE(0)

      REM canvas mode hotkeys
      CLS
      menuYadd%=980
      PROCgtext("Canvas Mode Hot Keys",8,menuYadd%,11,0,-48)
      PROCgtext("====================",8,menuYadd%,11,0,-48)
      PROCgtext("ESC+Qq : Show Control Codes",8,menuYadd%,14,0,-48)
      PROCgtext("ESC+Ff : Flash/Steady",8,menuYadd%,14,0,-48)
      PROCgtext("ESC+Hh : Hold/Releas Graphics",8,menuYadd%,14,0,-48)
      PROCgtext("ESC+Dd : Double/Normal Height",8,menuYadd%,14,0,-48)
      PROCgtext("ESC+Ss : Separated/Normal Graphics",8,menuYadd%,14,0,-48)
      PROCgtext("Ins    : Show Object Insert Menu",8,menuYadd%,14,0,-48)
      PROCgtext("End    : Insert Last Selected Object",8,menuYadd%,14,0,-48)
      PROCgtext("Esc    : Cancel Insert Object",8,menuYadd%,14,0,-48)
      PROCgtext("Left   : Navigate To Previous Frame",8,menuYadd%,14,0,-48)
      PROCgtext("Right  : Navigate To Next Frame",8,menuYadd%,14,0,-48)
      PROCgtext("CTRL+C : Begin Region Selection",8,menuYadd%,14,0,-48)
      PROCgtext("CTRL+Left  : Copy Frame Left",8,menuYadd%,14,0,-48)
      PROCgtext("CTRL+Right : Copy Frame Right",8,menuYadd%,14,0,-48)
      PROCgtext("SHFT+Cursors : Pixel Shift Selection",8,menuYadd%,14,0,-48)

      PROCWAITMOUSE(4)
      PROCWAITMOUSE(0)

      REM movie mode hotkeys
      CLS
      menuYadd%=980
      PROCgtext("Movie Mode Hot Keys",8,menuYadd%,11,0,-48)
      PROCgtext("===================",8,menuYadd%,11,0,-48)
      PROCgtext("ENTER : Add Frame At Current Coords",8,menuYadd%,14,0,-48)
      PROCgtext("TAB   : Select On Screen Object",8,menuYadd%,14,0,-48)
      PROCgtext("Q     : Show Control Codes",8,menuYadd%,14,0,-48)
      PROCgtext("D     : Duplicate Selected Sprite",8,menuYadd%,14,0,-48)
      PROCgtext("R     : Repeat Sprite Across Frames",8,menuYadd%,14,0,-48)
      PROCgtext("P     : Selected Sprite Properties",8,menuYadd%,14,0,-48)
      PROCgtext("M     : Move Selected Sprite",8,menuYadd%,14,0,-48)
      PROCgtext("E     : Edit Selected Sprite",8,menuYadd%,14,0,-48)
      PROCgtext("SHFT E: Clone/Replace Selected Sprite",8,menuYadd%,14,0,-48)
      PROCgtext("Ins   : Show Object Insert Menu",8,menuYadd%,14,0,-48)
      PROCgtext("End   : Insert Last Selected Object",8,menuYadd%,14,0,-48)
      PROCgtext("Esc   : Cancel Insert Object",8,menuYadd%,14,0,-48)
      PROCgtext("PGUP PGDN : Next/Prev Frame (SHFT +50)",8,menuYadd%,14,0,-48)
      PROCgtext("HOME END  : First/Last Frame",8,menuYadd%,14,0,-48)
      PROCgtext("LMB       : Place Sprite In Frame",8,menuYadd%,14,0,-48)
      PROCgtext("SHIFT LMB : Add Sprite To Background",8,menuYadd%,14,0,-48)
      PROCgtext("Cursors   : Move World Map",8,menuYadd%,14,0,-48)
      PROCgtext("SHFT+Cursors : Move World Map 1 Page",8,menuYadd%,14,0,-48)

      PROCWAITMOUSE(4)
      PROCWAITMOUSE(0)

      REM import BMP / GIF hotkeys
      CLS
      menuYadd%=980
      PROCgtext("Import BMP / GIF Hot Keys",8,menuYadd%,11,0,-48)
      PROCgtext("=========================",8,menuYadd%,11,0,-48)
      PROCgtext("ENTER  : Capture Sprite",8,menuYadd%,14,0,-48)
      PROCgtext("LMB    : Capture Sprite",8,menuYadd%,14,0,-48)
      PROCgtext("Arrows : Move Sprite Box By Pixel",8,menuYadd%,14,0,-48)
      PROCgtext("Escape : Cancel Import",8,menuYadd%,14,0,-48)
      PROCgtext("HOME END : Colour Adjust Limit",8,menuYadd%,14,0,-80)
      PROCgtext("GIF Import Only",8,menuYadd%,13,0,-48)
      PROCgtext("ENTER +SHFT : Auto Capture All Frames",8,menuYadd%,14,0,-48)
      PROCgtext("PGUP PGDN   : Next/Prev GIF Frame",8,menuYadd%,14,0,-80)
      PROCgtext("GIF Import Large Sprite Only",8,menuYadd%,13,0,-48)
      PROCgtext("SHFT+Arrows : Adjust Sprite Size",8,menuYadd%,14,0,-48)
      PROCgtext("CTRL+Arrows : Adjust Sprite Size +10",8,menuYadd%,14,0,-80)
      PROCgtext("180 large sprites are available and",8,menuYadd%,7,0,-48)
      PROCgtext("have a maximum size of 30000 pixels",8,menuYadd%,7,0,-48)
      PROCgtext("e.g. 500x60 pixels, 100x300 pixels",8,menuYadd%,7,0,-48)


      PROCWAITMOUSE(4)
      PROCWAITMOUSE(0)

      REM      PROCchangemode(7,1)
      REM frame%-=1
      REM PROCloadnextframe(1,0)
      PROCmenurestore

      ENDPROC

      REM ##########################################################
      REM help screen connectors
      DEF PROChelpbox(cx%,cy%,cw%,col%)
      GCOL 0,col%
      RECTANGLE cx%*32,(24-cy%)*40+4,cw%*32-2,34

      ENDPROC

      REM ##########################################################
      DEF PROChelpdot(cx%,cy%,col%)
      GCOL 0,col%
      RECTANGLE FILL cx%*32+8,(24-cy%)*40+16,16
      ENDPROC

      REM ##########################################################
      DEF PROChelpvline(cx%,cy%,col%)
      GCOL 0,col%
      LINE cx%*32+16,964,cx%*32+16,(24-cy%)*40+24
      ENDPROC

      REM ##########################################################
      DEF PROChelpvline2(cx%,cy%,cl%,col%)
      GCOL 0,col%
      LINE cx%*32+16,(24-cy%)*40+24,cx%*32+16,(24-(cy%+cl%))*40+24
      ENDPROC

      REM ##########################################################
      DEF PROChelphline(cx%,cy%,cw%,col%)
      GCOL 0,col%
      LINE cx%*32+16,(24-cy%)*40+24,(cx%+cw%)*32+16,(24-cy%)*40+24
      ENDPROC

      REM ##########################################################
      REM resets all movie objects
      DEF PROCresetmovie
      LOCAL S%
      FOR S%=0 TO obj_lstmax%
        objlist{(S%)}.obj%=-1
        objlist{(S%)}.type%=0
        objlist{(S%)}.f%=-1
        objlist{(S%)}.rel%=0
        objlist{(S%)}.hop%=0
        objlist{(S%)}.parent%=-1
        objlist{(S%)}.x%=0
        objlist{(S%)}.y%=0
        objlist{(S%)}.h%=0
        objlist{(S%)}.v%=0
        objlist{(S%)}.m%=0
        objlist{(S%)}.c%=0
        objlist{(S%)}.u%=0
        IF S%<10000 THEN
          frmlist{(S%)}.x%=0
          frmlist{(S%)}.y%=0
          frmlist{(S%)}.b%=0
          frmlist{(S%)}.f%=7
        ENDIF

      NEXT

      REM reset world map and object list vars
      mmWX%=0
      mmWY%=0

      obj_lstcount%=-1
      obj_lstcur%=0

      movieframe%=-1       : REM displays current movie frame
      movieframetotal%=-1  : REM total frames saved

      spriteselect%=-1
      spriteselectold%=-1


      ENDPROC

      REM ##########################################################
      REM change to mode 6 and overlay control codes on current screen
      DEF PROCcontrolcodes(w%,m%)

      LOCAL C%,col%,p%,x%,y%,xs%,xe%,ys%,ye%

      showcodes%=0
      esccodes%=0
      CASE menuext% OF
        WHEN M_canvasmode%,M_moviemode%
          xs%=0
          xe%=39
          ys%=0
          ye%=23
        WHEN M_sprites%
          xs%=10
          xe%=29
          ys%=2
          ye%=17
      ENDCASE

      IF menuext%=M_canvasmode% PROCframesave(frame%)
      PROCchangemode(6,0)
      PROCdrawgrid

      REM control code special chars

      REM 129 alphanumeric red
      REM 130 alphanumeric green
      REM 131 alphanumeric yellow
      REM 132 alphanumeric blue
      REM 133 alphanumeric magenta
      REM 134 alphanumeric cyan
      REM 135 alphanumeric white
      VDU 23,224,224,64,64,64,64,0,0,0
      REM 136 flash
      VDU 23,225,224,128,192,128,128,0,0,0
      REM 137 steady
      VDU 23,226,224,128,192,128,128,0,0,0
      REM 140 normal height
      VDU 23,227,192,160,160,160,192,0,0,0
      REM 141 double height
      VDU 23,228,192,160,160,160,192,0,0,0
      REM 145 graphics red
      REM 146 graphics green
      REM 147 graphics yellow
      REM 148 graphics blue
      REM 149 graphics magenta
      REM 150 graphics cyan
      REM 151 graphics white
      VDU 23,229,224,128,160,160,224,0,0,0
      REM 152 conceal
      REM 153 contiguous graphics
      VDU 23,230,224,128,224,32,224,0,0,0
      REM 154 separated graphics
      VDU 23,231,224,128,224,32,224,0,0,0
      REM 156 black background
      VDU 23,232,224,144,224,144,224,0,0,0
      REM 157 new background
      VDU 23,233,224,144,224,144,224,0,0,0
      REM 158 hold graphics
      VDU 23,234,160,160,224,160,160,0,0,0
      REM 159 release graphics
      VDU 23,235,160,160,224,160,160,0,0,0

      VDU 5

      REM show codes
      FOR x%=xs% TO xe%
        FOR y%=ys% TO ye%
          C%=frame_buffer&(frame%-1,x%+y%*40)
          p%=0
          CASE C% OF
            WHEN 129,130,131,132,133,134,135 : REM text codes
              col%=15-(135-C%)
              p%=224
            WHEN 136 : REM flashing
              col%=15
              p%=225
            WHEN 137 : REM non flashing
              col%=9
              p%=226
            WHEN 140 : REM normal height
              col%=9
              p%=227
            WHEN 141 : REM double height
              col%=15
              p%=228
            WHEN 145,146,147,148,149,150,151 : REM graphic codes
              col%=15-(151-C%)
              p%=229
            WHEN 153 : REM contiguous
              col%=9
              p%=230
            WHEN 154 : REM separated
              col%=15
              p%=231
            WHEN 156 : REM black background
              col%=9
              p%=232
            WHEN 157 : REM new background
              col%=15
              p%=233
            WHEN 158 : REM hold graphic
              col%=15
              p%=234
            WHEN 159 : REM release
              col%=9
              p%=235
          ENDCASE

          IF p% THEN
            GCOL 0,0
            MOVE x%*32+2,957-(y%*40)
            PLOT 101,x%*32+18,933-(y%*40)

            MOVE x%*32+4,953-(y%*40)
            GCOL 0,col%
            VDU p%
          ENDIF
        NEXT
      NEXT

      VDU 4

      REM wait for click or key press to hide codes
      IF w% THEN
        PROCWAITNOKEY(-17,0)
        PROCWAITNOKEY(-113,0)
        REPEAT
          PROCREADMOUSE
        UNTIL MB%=4 OR INKEY(-113)
        PROCWAITMOUSE(0)
        REM PROCWAITNOKEY(-113,0)
        REPEAT UNTIL INKEY(0)=-1
        IF m% PROCchangemode(7,1)
      ENDIF
      ENDPROC

      REM ##########################################################
      REM DRAW SPRITE EDITOR SCREEN
      DEF PROCspritescreen(R%)

      REM REDRAW EVERYTHING
      IF R%=1 THEN
        FOR Y%=1 TO 24
          PROCprint40(Y%,"")
        NEXT

        PROCGR(7,0,0)

        PRINTTAB(8,1)tb$;CHR$(157);ty$;"                    ";CHR$(156);

        FOR Y%=2 TO 19
          VDU 31,8,Y%,151
          VDU 31,31,Y%,156
        NEXT

        VDU 31,9,2,184
        VDU 31,9,19,169
        VDU 31,30,2,228
        VDU 31,30,19,166

        PRINTTAB(10,20)tb$;CHR$(157);tc$;"< ";CHR$(156);tb$;CHR$(157);tc$;"> ";CHR$(156);

        PRINTTAB(1,24)tb$;CHR$(157);ty$;"MOVIE  ";CHR$(156);
        PRINTTAB(14,24)tb$;CHR$(157);ty$;"CANVAS  ";CHR$(156);
        PRINTTAB(28,24)tr$;CHR$(157);ty$;"CLOSE  ";CHR$(156);

        PRINTTAB(10,22)tc$;"W:";ty$;"  ";tc$;"H:";ty$;"  ";

        PRINTTAB(0,2)ty$;"LOAD";
        PRINTTAB(0,4)ty$;"SAVE";
        PRINTTAB(0,6)ty$;"IMPRT";
        PRINTTAB(0,8)tw$;"ANIM8";
        PRINTTAB(0,10)tc$;"CPY >";
        PRINTTAB(0,12)tc$;"CPY <";
        PRINTTAB(0,14)tc$;"COPY";
        PRINTTAB(0,16)tc$;"PASTE";
        PRINTTAB(0,20)ty$;"GIF S";
        PRINTTAB(0,22)ty$;"GIF L";

        PRINTTAB(32,2)tc$;"CLS";
        PRINTTAB(32,4)tc$;"SCR-L";
        PRINTTAB(32,6)tc$;"SCR-R";
        PRINTTAB(32,8)tc$;"SCR-U";
        PRINTTAB(32,10)tc$;"SCR-D";
        PRINTTAB(32,12)tc$;"FLP-]";
        PRINTTAB(32,14)tc$;"FLP-^";
        PRINTTAB(32,16)tc$;"ROT >";
        PRINTTAB(32,18)tc$;"ROT <";
      ENDIF

      REM REFRESH DYNAMIC AREAS
      PRINTTAB(11,1)sprname$(sprite_cur%);SPC(18-LEN(sprname$(sprite_cur%)))
      PRINTTAB(22,20)ty$;CHR$(157);tr$;RIGHT$("00"+STR$(sprite_cur%+1),3)+" ";CHR$(156);CHR$(151)
      p$="CHR"
      IF spr_scroll%=0 p$="PIX"
      PRINTTAB(36,2)tg$;p$;
      p$="PIX"
      IF sprlist{(sprite_cur%)}.m%=1 p$="CHR"
      PRINTTAB(22,22)tc$;"MODE:";ty$;p$;

      PRINTTAB(0,18)CHR$(129+spr_trns%);"TRANS";

      PROCspriteredraw
      PROCupdatespritesize(sprite_cur%,1)
      PROCmenudraw

      REM testing idea to add sprite select overlay on sprite screen
      DIM pt{x%,y%,w%,h%}
      pt.x%=500
      pt.y%=40
      pt.w%=136
      pt.h%=456

      REM SYS "SDL_SetRenderDrawColor", @memhdc%, 0, 255, 0, 255
      REM SYS "SDL_RenderDrawLine", @memhdc%, 50, 50, 600, 480

      REM SYS "SDL_SetRenderDrawColor", @memhdc%, 0, 0, 255, 255
      REM SYS "SDL_RenderFillRect", @memhdc%, pt{}

      ENDPROC

      REM ##########################################################
      REM animation UI
      DEF PROCanimscreen
      LOCAL C%,X%,Y%,S%,SP%,DP%,done%
      REM MODE 6 : CHAR 40x25 PIXELS: 640x500 GRAPHICS UNITS: 1280x1000 COLOURS: 16  CHARS: 32X40 GU
      PROCchangemode(6,1) : REM MODE 3 : CHAR 80x25 PIXELS: 640x500 GRAPHICS UNITS: 1280x1000 COLOURS: 16

      PROCanimredraw
      PROCanimupdate(0)

      REPEAT
        PROCREADMOUSE
        IF MB%=4 THEN
          IF MX%>6 AND MX%<1156 AND MY%>6 AND MY%<450 THEN
            SP%=(MX%-8) DIV 96+((448-MY%) DIV 112)*12

            REM insert next sprite set obj
            PROCWAITMOUSE(0)
            IF MX%>6 AND MX%<1156 AND MY%>6 AND MY%<450 THEN
              IF SP%=(MX%-8) DIV 96+((448-MY%) DIV 112)*12 THEN
                DP%=sprani{(spr_lstcount%)}.c%
                IF DP%<sprani_max% THEN
                  sprani{(spr_lstcount%)}.c%+=1
                  sprani{(spr_lstcount%)}.s%(DP%)=SP%
                  PROCanimupdate(0)
                ENDIF
              ENDIF
            ENDIF
          ELSE
            PROCWAITMOUSE(0)
            C%=FNgetcontrol

            CASE C% OF
              WHEN 0 : REM set dec 10
                IF spr_lstcount%>0 THEN
                  spr_lstcount%-=10
                  IF spr_lstcount%<0 THEN spr_lstcount%=0
                  PROCanimupdate(X%)
                ENDIF

              WHEN 1 : REM set dec 1
                IF spr_lstcount%>0 THEN
                  spr_lstcount%-=1
                  PROCanimupdate(X%)
                ENDIF

              WHEN 2 : REM set inc
                IF spr_lstcount%<99 THEN
                  spr_lstcount%+=1
                  PROCanimupdate(X%)
                ENDIF

              WHEN 3 : REM set inc 10
                IF spr_lstcount%<99 THEN
                  spr_lstcount%+=10
                  IF spr_lstcount%>99 THEN spr_lstcount%=99
                  PROCanimupdate(X%)
                ENDIF

              WHEN 4 : REM frame dec 10
                IF sprani{(spr_lstcount%)}.f%>1 THEN
                  sprani{(spr_lstcount%)}.f%-=10
                  IF sprani{(spr_lstcount%)}.f%<1 THEN sprani{(spr_lstcount%)}.f%=1
                  PROCanimupdate(X%)
                ENDIF

              WHEN 5 : REM frame dec 1
                IF sprani{(spr_lstcount%)}.f%>1 THEN
                  sprani{(spr_lstcount%)}.f%-=1
                  PROCanimupdate(X%)
                ENDIF

              WHEN 6 : REM frame inc 1
                IF sprani{(spr_lstcount%)}.f%<frame_max% THEN
                  sprani{(spr_lstcount%)}.f%+=1
                  PROCanimupdate(X%)
                ENDIF

              WHEN 7 : REM frame inc 10
                IF sprani{(spr_lstcount%)}.f%<frame_max% THEN
                  sprani{(spr_lstcount%)}.f%+=10
                  IF sprani{(spr_lstcount%)}.f%>frame_max% THEN sprani{(spr_lstcount%)}.f%=frame_max%
                  PROCanimupdate(X%)
                ENDIF

              WHEN 8 : REM repeat dec 10
                IF sprani{(spr_lstcount%)}.r%>0 THEN
                  sprani{(spr_lstcount%)}.r%-=10
                  IF sprani{(spr_lstcount%)}.r%<0 THEN sprani{(spr_lstcount%)}.r%=0
                  PROCanimupdate(X%)
                ENDIF

              WHEN 9 : REM repeat dec 1
                IF sprani{(spr_lstcount%)}.r%>0 THEN
                  sprani{(spr_lstcount%)}.r%-=1
                  PROCanimupdate(X%)
                ENDIF

              WHEN 10 : REM repeat inc
                IF sprani{(spr_lstcount%)}.r%<20 THEN
                  sprani{(spr_lstcount%)}.r%+=1
                  PROCanimupdate(X%)
                ENDIF

              WHEN 11 : REM repeat inc 10
                IF sprani{(spr_lstcount%)}.r%<20 THEN
                  sprani{(spr_lstcount%)}.r%+=10
                  IF sprani{(spr_lstcount%)}.r%>20 THEN sprani{(spr_lstcount%)}.r%=20
                  PROCanimupdate(X%)
                ENDIF

              WHEN 12 : REM x dec 10
                IF sprani{(spr_lstcount%)}.x%>-20 THEN
                  sprani{(spr_lstcount%)}.x%-=10
                  IF sprani{(spr_lstcount%)}.x%<-20 THEN sprani{(spr_lstcount%)}.x%=-20
                  PROCanimupdate(X%)
                ENDIF

              WHEN 13 : REM x dec 1
                IF sprani{(spr_lstcount%)}.x%>-20 THEN
                  sprani{(spr_lstcount%)}.x%-=1
                  PROCanimupdate(X%)
                ENDIF

              WHEN 14 : REM x inc
                IF sprani{(spr_lstcount%)}.x%<40 THEN
                  sprani{(spr_lstcount%)}.x%+=1
                  PROCanimupdate(X%)
                ENDIF

              WHEN 15 : REM x inc 10
                IF sprani{(spr_lstcount%)}.x%<40 THEN
                  sprani{(spr_lstcount%)}.x%+=10
                  IF sprani{(spr_lstcount%)}.x%>40 THEN sprani{(spr_lstcount%)}.x%=40
                  PROCanimupdate(X%)
                ENDIF

              WHEN 16 : REM h dec 10
                IF sprani{(spr_lstcount%)}.h%>-10 THEN
                  sprani{(spr_lstcount%)}.h%-=10
                  IF sprani{(spr_lstcount%)}.h%<-10 THEN sprani{(spr_lstcount%)}.h%=-10
                  PROCanimupdate(X%)
                ENDIF

              WHEN 17 : REM h dec 1
                IF sprani{(spr_lstcount%)}.h%>-10 THEN
                  sprani{(spr_lstcount%)}.h%-=1
                  PROCanimupdate(X%)
                ENDIF

              WHEN 18 : REM h inc 1
                IF sprani{(spr_lstcount%)}.h%<10 THEN
                  sprani{(spr_lstcount%)}.h%+=1
                  PROCanimupdate(X%)
                ENDIF

              WHEN 19 : REM h inc 10
                IF sprani{(spr_lstcount%)}.h%<10 THEN
                  sprani{(spr_lstcount%)}.h%+=10
                  IF sprani{(spr_lstcount%)}.h%>10 THEN sprani{(spr_lstcount%)}.h%=10
                  PROCanimupdate(X%)
                ENDIF

              WHEN 20 : REM y dec 10
                IF sprani{(spr_lstcount%)}.y%>-16 THEN
                  sprani{(spr_lstcount%)}.y%-=10
                  IF sprani{(spr_lstcount%)}.y%<-16 THEN sprani{(spr_lstcount%)}.y%=-16
                  PROCanimupdate(X%)
                ENDIF

              WHEN 21 : REM y dec 1
                IF sprani{(spr_lstcount%)}.y%>-16 THEN
                  sprani{(spr_lstcount%)}.y%-=1
                  PROCanimupdate(X%)
                ENDIF

              WHEN 22 : REM y inc 1
                IF sprani{(spr_lstcount%)}.y%<41 THEN
                  sprani{(spr_lstcount%)}.y%+=1
                  PROCanimupdate(X%)
                ENDIF

              WHEN 23 : REM y inc 10
                IF sprani{(spr_lstcount%)}.y%<25 THEN
                  sprani{(spr_lstcount%)}.y%+=10
                  IF sprani{(spr_lstcount%)}.y%>25 THEN sprani{(spr_lstcount%)}.y%=25
                  PROCanimupdate(X%)
                ENDIF

              WHEN 24 : REM v dec 10
                IF sprani{(spr_lstcount%)}.v%>-10 THEN
                  sprani{(spr_lstcount%)}.v%-=10
                  IF sprani{(spr_lstcount%)}.v%<-10 THEN sprani{(spr_lstcount%)}.v%=-10
                  PROCanimupdate(X%)
                ENDIF

              WHEN 25 : REM v dec 1
                IF sprani{(spr_lstcount%)}.v%>-10 THEN
                  sprani{(spr_lstcount%)}.v%-=1
                  PROCanimupdate(X%)
                ENDIF

              WHEN 26 : REM v inc 1
                IF sprani{(spr_lstcount%)}.v%<10 THEN
                  sprani{(spr_lstcount%)}.v%+=1
                  PROCanimupdate(X%)
                ENDIF

              WHEN 27 : REM v inc 10
                IF sprani{(spr_lstcount%)}.v%<10 THEN
                  sprani{(spr_lstcount%)}.v%+=10
                  IF sprani{(spr_lstcount%)}.v%>10 THEN sprani{(spr_lstcount%)}.v%=10
                  PROCanimupdate(X%)
                ENDIF

              WHEN 28 : REM load

              WHEN 29 : REM save

              WHEN 30 : REM plot
                PROCundosaveall
                FOR L%=0 TO 99
                  IF sprani{(L%)}.s%(0)>-1 THEN
                    REM iniital starting location and frame
                    X%=sprani{(L%)}.x%
                    Y%=sprani{(L%)}.y%
                    F%=sprani{(L%)}.f%
                    D%=1

                    REM count sprites in this set
                    C%=sprani{(L%)}.c%

                    REM plot at least 1 set
                    FOR S%=0 TO C%-1
                      IF F%<frame_max%+1 THEN
                        PROCspritetoframe(F%,sprani{(L%)}.s%(S%),X%,Y%)
                        IF D% MOD sprani{(L%)}.d%=0 THEN
                          X%+=sprani{(L%)}.h%
                          Y%+=sprani{(L%)}.v%
                        ENDIF
                        F%+=1
                        D%+=1
                      ELSE
                        EXIT FOR
                      ENDIF
                    NEXT

                    REM repeat set if required
                    IF sprani{(L%)}.r%<>1 AND F%<frame_max% THEN
                      R%=1
                      REPEAT
                        FOR S%=0 TO C%-1
                          IF F%<frame_max%+1 THEN
                            PROCspritetoframe(F%,sprani{(L%)}.s%(S%),X%,Y%)
                            IF D% MOD sprani{(L%)}.d%=0 THEN
                              X%+=sprani{(L%)}.h%
                              Y%+=sprani{(L%)}.v%
                            ENDIF
                            F%+=1
                            D%+=1
                          ELSE
                            EXIT FOR
                          ENDIF
                        NEXT

                        R%+=1
                      UNTIL R%=sprani{(L%)}.r% OR F%>frame_max%
                    ENDIF

                  ENDIF
                NEXT
                REM done%=1
                REM menuext%=77

              WHEN 31 : REM undo
                PROCundorestoreall
                REM done%=1
                REM menuext%=77

              WHEN 32 : REM spare

              WHEN 33 : REM reset all sets
                IF FNmsgbox("RESET ALL SETS?",""," RESET "," CANCEL ")=1 THEN
                  FOR s%=0 TO 99
                    sprani{(s%)}.f%=1
                    sprani{(s%)}.r%=0
                    sprani{(s%)}.x%=0
                    sprani{(s%)}.y%=0
                    sprani{(s%)}.h%=0
                    sprani{(s%)}.v%=0
                    sprani{(s%)}.m%=0
                    sprani{(s%)}.d%=1
                    sprani{(s%)}.c%=0
                    FOR ss%=0 TO sprani_max%-1
                      sprani{(s%)}.s%(ss%)=-1
                    NEXT
                  NEXT
                  spr_lstcount%=0
                ENDIF
                PROCanimredraw
                PROCanimupdate(0)


              WHEN 34 : REM exit to spr
                done%=1
              WHEN 35 : REM exit to main
                done%=1
                menuext%=77

              WHEN 36 : REM put sprite in frame
                IF sprani{(spr_lstcount%)}.s%(0)>-1 THEN
                  PROCanimput(1)
                  PROCanimredraw
                  PROCanimupdate(0)
                ENDIF

              WHEN 37 : REM clear this set
                IF FNmsgbox("RESET THIS SET?",""," RESET "," CANCEL ")=1 THEN
                  sprani{(spr_lstcount%)}.f%=1
                  sprani{(spr_lstcount%)}.r%=0
                  sprani{(spr_lstcount%)}.x%=0
                  sprani{(spr_lstcount%)}.y%=0
                  sprani{(spr_lstcount%)}.h%=0
                  sprani{(spr_lstcount%)}.v%=0
                  sprani{(spr_lstcount%)}.m%=0
                  sprani{(spr_lstcount%)}.d%=1
                  sprani{(spr_lstcount%)}.c%=0
                  FOR ss%=0 TO sprani_max%-1
                    sprani{(spr_lstcount%)}.s%(ss%)=-1
                  NEXT

                ENDIF
                PROCanimredraw
                PROCanimupdate(0)

              WHEN 38 : REM change plot mode
                sprani{(spr_lstcount%)}.m%=(sprani{(spr_lstcount%)}.m%+1) MOD 3
                PROCanimcontrol(38,plotmode$(sprani{(spr_lstcount%)}.m%),952,656,0,8,15,4)

              WHEN 39 : REM dec delta per frame count
                IF sprani{(spr_lstcount%)}.d%>1 THEN
                  sprani{(spr_lstcount%)}.d%-=1
                  PROCanimupdate(X%)
                ENDIF
              WHEN 40 : REM inc delta per frame count
                IF sprani{(spr_lstcount%)}.d%<12 THEN
                  sprani{(spr_lstcount%)}.d%+=1
                  PROCanimupdate(X%)
                ENDIF

            ENDCASE
          ENDIF
        ELSE
          WAIT 2
        ENDIF
        REM IF SP%>sprite_max%-1 THEN SP%=-1
        REM PRINTTAB(0,12)STR$(SP%);" ";STR$(DP%);" ";STR$(MX%);",";STR$(MY%);"    "
      UNTIL done%=1
      PROCWAITMOUSE(0)
      PROCchangemode(7,1)
      ENDPROC

      REM ##########################################################
      REM display a warning for clearing sets
      DEF FNmsgbox(t1$,t2$,b1$,b2$)
      LOCAL done%,BX%

      GCOL 0,0
      RECTANGLE FILL 240,300,800,400

      GCOL 0,15
      RECTANGLE 248,308,784,384
      RECTANGLE 250,310,780,380

      PROCgtext(t1$,300,640,11,0,0)
      IF t2$<>"" PROCgtext(t2$,300,600,11,0,0)

      PROCgtext(b1$,300,450,10,4,0)
      PROCgtext(b2$,600,450,3,9,0)

      BX%=LEN(b1$)*32

      PROCWAITMOUSE(4)
      PROCWAITMOUSE(0)
      IF MX%>298 AND MX%<300+BX% AND MY%>410 AND MY%<500 THEN done%=1

      =done%

      REM ##########################################################
      REM display a control button, Title, X, Y, border col, text col, fill col
      DEF PROCanimcontrol(n%,t$,x%,y%,ya%,bc%,tc%,fc%)
      LOCAL l%,sx%,sy%

      l%=LEN(t$)
      sx%=l%*32+20
      REM sx%=l%*32+16
      sy%=38+ya%

      GCOL 0,fc%
      RECTANGLE FILL x%+2,y%-sy%+6,sx%-4,sy%

      GCOL 0,bc%

      RECTANGLE x%,y%-sy%+4,sx%,sy%+4
      RECTANGLE x%+2,y%-sy%+6,sx%-4,sy%
      PROCgprint(t$,x%+12,y%+2,tc%)

      REM save control range
      PROCaddcontrange(n%,x%-2,y%-sy%+2,x%+sx%,y%+8)

      menuadd%=menuadd%+sx%+16

      ENDPROC

      REM ##########################################################
      REM update animation screen details
      DEF PROCanimupdate(c%)
      LOCAL I%,S%,DX%,DY%,tc%,bc%
      c%=c% DIV 4

      tc%=11
      bc%=0

      REM set
      IF c%=0 THEN

        REM redraw sprites
        DY%=776
        GCOL 0,0
        RECTANGLE FILL 4,DY%+4,1154,116

        FOR I%=0 TO sprani_max%-1
          S%=sprani{(spr_lstcount%)}.s%(I%)
          DX%=I%*96

          IF S%>-1 THEN
            PROCdrawanimspr(S%,DX%+14,DY%+14)
            GCOL 0,15
          ELSE
            GCOL 0,8
          ENDIF
          RECTANGLE DX%+8,DY%+8,90,106

        NEXT

        PROCgtext(RIGHT$("00"+STR$(spr_lstcount%+1),3),5*32,760,14,4,0)
        PROCgtext(RIGHT$("0"+STR$(sprani{(spr_lstcount%)}.c%),2),34*32,760,tc%,bc%,0)

        PROCanimcontrol(38,plotmode$(sprani{(spr_lstcount%)}.m%),952,656,0,8,15,4)

      ENDIF


      REM frm
      IF c%=1 OR c%=0 THEN
        PROCgtext(RIGHT$("00"+STR$(sprani{(spr_lstcount%)}.f%),3),5*32,708,tc%,bc%,0)
      ENDIF

      REM rep
      IF c%=2 OR c%=0 THEN
        CASE sprani{(spr_lstcount%)}.r% OF
          WHEN 0
            A$="ALL FRMS"
          WHEN 1
            A$="001 SET "
          OTHERWISE
            A$=RIGHT$("00"+STR$(sprani{(spr_lstcount%)}.r%),3)+" SETS"
        ENDCASE
        PROCgtext(A$,5*32,656,tc%,bc%,0)
      ENDIF

      REM X
      IF c%=3 OR c%=0 THEN
        PROCgtext(RIGHT$("  "+STR$(sprani{(spr_lstcount%)}.x%),3),3*32,552,tc%,bc%,0)
      ENDIF

      REM H
      IF c%=4 OR c%=0 THEN
        PROCgtext(RIGHT$("  "+STR$(sprani{(spr_lstcount%)}.h%),3),20*32,552,tc%,bc%,0)
      ENDIF

      REM Y
      IF c%=5 OR c%=0 THEN
        PROCgtext(RIGHT$("  "+STR$(sprani{(spr_lstcount%)}.y%),3),3*32,500,tc%,bc%,0)
      ENDIF

      REM V
      IF c%=6 OR c%=0 THEN
        PROCgtext(RIGHT$("  "+STR$(sprani{(spr_lstcount%)}.v%),3),20*32,500,tc%,bc%,0)
      ENDIF

      REM D
      IF c%>8 OR c%=0 THEN
        PROCgtext(RIGHT$(" "+STR$(sprani{(spr_lstcount%)}.d%),2),23*32,604,tc%,bc%,0)
      ENDIF

      REM      VDU 4
      ENDPROC

      REM ##########################################################
      REM redraw sprite animation screen
      DEF PROCanimredraw
      LOCAL X%,Y%,DX%,DY%,S%,bc%,tc%,lc%,A$
      REM MODE 6 : CHAR 40x25 PIXELS: 640x500 GRAPHICS UNITS: 1280x1000 COLOURS: 16  CHARS: 32X40 GU

      GCOL 128
      CLG

      bc%=6 : REM control button colour
      tc%=15 : REM parameter text colour
      lc%=6 : REM label text colour

      REM header
      A$="Sprite Animation"
      VDU 5
      FOR X%=0 TO LEN(A$)-1
        GCOL 0,4
        MOVE X%*40+308,990
        PRINTMID$(A$,X%+1,1)
        GCOL 0,14
        MOVE X%*40+312,994
        PRINTMID$(A$,X%+1,1)
      NEXT
      VDU 4

      REM all sprites layout
      GCOL 0,0
      RECTANGLE FILL 0,0,12*96+8,4*112+8

      FOR Y%=0 TO 3
        FOR X%=0 TO 11
          S%=X%+Y%*12
          DX%=X%*96
          DY%=336-Y%*112
          GCOL 0,8
          IF S%<sprite_max% THEN
            PROCdrawanimspr(S%,DX%+14,DY%+14)
            GCOL 0,15
          ENDIF
          RECTANGLE DX%+8,DY%+8,90,106
        NEXT
      NEXT

      REM selected sprite set layout
      PROCgtext("SET:",0,760,tc%,0,0)
      PROCgtext("COUNT:",864,760,lc%,0,0)
      PROCgtext("FRM:",0,708,tc%,0,0)
      PROCgtext("REP:",0,656,tc%,0,0)
      PROCgtext("MODE:",760,656,tc%,0,0)
      PROCgtext("START POS:",0,604,lc%,0,0)
      PROCgtext("X:",0,552,tc%,0,0)
      PROCgtext("Y:",0,500,tc%,0,0)
      PROCgtext("  PER    FRAME(S)",544,604,lc%,0,0)

      menuadd%=1100
      PROCanimcontrol(39,"<",menuadd%,604,0,8,bc%,0)
      PROCanimcontrol(40,">",menuadd%,604,0,8,bc%,0)

      GCOL 0,14

      REM delta
      X%=544
      Y%=582
      LINE X%,Y%,X%+20,Y%+20
      LINE X%,Y%-2,X%+22,Y%+20
      LINE X%+2,Y%-2,X%+22,Y%+18

      LINE X%+20,Y%+18,X%+40,Y%-2
      LINE X%+20,Y%+20,X%+42,Y%-2
      LINE X%+22,Y%+20,X%+42,Y%

      LINE X%+2,Y%-2,X%+40,Y%-2
      LINE X%+2,Y%,X%+40,Y%

      PROCgtext("H:",17*32,552,tc%,0,0)
      PROCgtext("V:",17*32,500,tc%,0,0)

      REM VDU 4

      REM GAP 88 DOUBLE BUTTON, 56 SINGLE BUTTON


      PROCresetcontrols

      REM set
      menuadd%=272
      PROCanimcontrol(0,"<<",menuadd%,760,0,8,bc%,0)
      PROCanimcontrol(1,"<",menuadd%,760,0,8,bc%,0)
      PROCanimcontrol(2,">",menuadd%,760,0,8,bc%,0)
      PROCanimcontrol(3,">>",menuadd%,760,0,8,bc%,0)

      REM put button
      PROCanimcontrol(36,"PUT",menuadd%,760,0,7,14,4)
      PROCanimcontrol(37,"CLR",menuadd%,760,0,7,1,0)

      REM FRM
      menuadd%=272
      PROCanimcontrol(4,"<<",menuadd%,708,0,8,bc%,0)
      PROCanimcontrol(5,"<",menuadd%,708,0,8,bc%,0)
      PROCanimcontrol(6,">",menuadd%,708,0,8,bc%,0)
      PROCanimcontrol(7,">>",menuadd%,708,0,8,bc%,0)


      REM REP
      menuadd%=432
      PROCanimcontrol(8,"<<",menuadd%,656,0,8,bc%,0)
      PROCanimcontrol(9,"<",menuadd%,656,0,8,bc%,0)
      PROCanimcontrol(10,">",menuadd%,656,0,8,bc%,0)
      PROCanimcontrol(11,">>",menuadd%,656,0,8,bc%,0)
      PROCanimcontrol(38,plotmode$(sprani{(spr_lstcount%)}.m%),952,656,0,8,15,4)

      REM X,H
      menuadd%=212
      PROCanimcontrol(12,"<<",menuadd%,552,0,8,bc%,0)
      PROCanimcontrol(13,"<",menuadd%,552,0,8,bc%,0)
      PROCanimcontrol(14,">",menuadd%,552,0,8,bc%,0)
      PROCanimcontrol(15,">>",menuadd%,552,0,8,bc%,0)
      menuadd%=752
      PROCanimcontrol(16,"<<",menuadd%,552,0,8,bc%,0)
      PROCanimcontrol(17,"<",menuadd%,552,0,8,bc%,0)
      PROCanimcontrol(18,">",menuadd%,552,0,8,bc%,0)
      PROCanimcontrol(19,">>",menuadd%,552,0,8,bc%,0)

      REM Y,V
      menuadd%=212
      PROCanimcontrol(20,"<<",menuadd%,500,0,8,bc%,0)
      PROCanimcontrol(21,"<",menuadd%,500,0,8,bc%,0)
      PROCanimcontrol(22,">",menuadd%,500,0,8,bc%,0)
      PROCanimcontrol(23,">>",menuadd%,500,0,8,bc%,0)
      menuadd%=752
      PROCanimcontrol(24,"<<",menuadd%,500,0,8,bc%,0)
      PROCanimcontrol(25,"<",menuadd%,500,0,8,bc%,0)
      PROCanimcontrol(26,">",menuadd%,500,0,8,bc%,0)
      PROCanimcontrol(27,">>",menuadd%,500,0,8,bc%,0)

      REM menu
      menuadd%=16
      PROCanimcontrol(28,"LOAD",menuadd%,940,0,7,15,0)
      PROCanimcontrol(29,"SAVE",menuadd%,940,0,7,15,0)
      PROCanimcontrol(30,"PLOT",menuadd%,940,0,7,14,4)
      PROCanimcontrol(31,"UNDO",menuadd%,940,0,7,3,0)
      PROCanimcontrol(32,"----",menuadd%,940,0,7,8,0)
      PROCanimcontrol(33,"RSET",menuadd%,940,0,7,1,0)
      PROCanimcontrol(34,"SPR",menuadd%,940,0,9,11,1)
      PROCanimcontrol(35,"EXIT",menuadd%,940,0,9,11,1)

      ENDPROC

      REM ##########################################################
      REM place 1st sprite of a set in frame
      DEF PROCanimput(G%)
      LOCAL X%,Y%,OX%,OY%,C%,S%,F%,done%
      COLOUR 128
      CLS

      REM draw frame
      GCOL 0,7
      F%=sprani{(spr_lstcount%)}.f%
      FOR Y%=3 TO 74
        OY%=Y%*8-8
        FOR X%=2 TO 79
          C%=FNpoint_buf(X%,77-Y%,F%)
          IF C% THEN RECTANGLE FILL X%*8+40*8,OY%,8,8
        NEXT
      NEXT

      REM PIXEL GRID 40+80+2 (122) x 48+75+2 (125)
      REM CHAR GRID
      REM grid
      GCOL 0,8
      IF G%=0 THEN
        FOR X%=1 TO 124
          LINE 0,X%*8,122*8,X%*8
          IF X%<123 THEN
            LINE X%*8,0,X%*8,125*8
          ENDIF
        NEXT
      ELSE
        FOR X%=1 TO 61
          LINE X%*16,0,X%*16,125*8
          IF X%<42 THEN
            LINE 0,X%*24-8,61*16,X%*24-8

          ENDIF
        NEXT

      ENDIF

      REM menu bar and first column
      GCOL 0,3
      RECTANGLE 40*8,16,80*8,75*8
      RECTANGLE FILL 40*8,74*8,80*8,24
      RECTANGLE FILL 40*8,16,16,72*8

      X%=(MX% DIV 16)*16
      Y%=((MY%+8) DIV 24)*24
      IF X%<0 THEN X%=0
      IF X%>120*8 THEN X%=120*8
      IF Y%<0 THEN Y%=0
      IF Y%>123*8 THEN Y%=123*8
      OX%=X%
      OY%=Y%
      S%=sprani{(spr_lstcount%)}.s%(0)
      GCOL 3,4
      RECTANGLE X%,Y%-46*8,40*8,48*8
      GCOL 3,10
      PROCdrawanimspr2(S%,X%,Y%-46*8)
      REPEAT
        PROCREADMOUSE
        X%=(MX% DIV 16)*16
        Y%=((MY%+8) DIV 24)*24
        IF X%<0 THEN X%=0
        IF X%>120*8 THEN X%=120*8
        IF Y%<0 THEN Y%=0
        IF Y%>123*8 THEN Y%=123*8
        IF OX%<>X% OR OY%<>Y% THEN
          GCOL 3,4
          RECTANGLE OX%,OY%-46*8,40*8,48*8
          RECTANGLE X%,Y%-46*8,40*8,48*8

          GCOL 3,10
          PROCdrawanimspr2(S%,OX%,OY%-46*8)
          PROCdrawanimspr2(S%,X%,Y%-46*8)
          OX%=X%
          OY%=Y%
        ENDIF
        REM PRINTTAB(32,0)"MX:";STR$(MX% DIV 8);"  ";
        REM PRINTTAB(32,1)"MY:";STR$(MY% DIV 8);"  ";
        REM PRINTTAB(32,2)"PX:";STR$((MX% DIV 8)-40);"  ";
        REM PRINTTAB(32,3)"PY:";STR$(76-(MY% DIV 8));"  ";
        PRINTTAB(32,0)"CX:";STR$((X% DIV 16)-20);"  ";
        PRINTTAB(32,1)"CY:";STR$(25-(Y% DIV 24));"  ";

        REM save the position on click
        IF MB%=4 THEN
          done%=1
          sprani{(spr_lstcount%)}.x%=(X% DIV 16)-20
          sprani{(spr_lstcount%)}.y%=25-(Y% DIV 24)
        ENDIF

        WAIT 2
      UNTIL done%=1
      PROCWAITMOUSE(0)
      ENDPROC

      REM ##########################################################
      REM draw a grid of sprite objects with boxes and scrollbar
      DEF PROCspritegrid(x%,y%,w%,h%,im%,cr%)
      DIM X%,Y%,S%

      REM calc height based on grid size
      REM take out references to menuadd%
      REM work out position based on grid size rather than additional x,y manual adjustments

      REM scrolLbar
      PROCdrawscrollbar(cr%,SX%+412,menuYadd%-712,50,668,7)

      REM draw sprites
      menuYadd%-=160
      FOR Y%=0 TO h%-1
        FOR X%=0 TO w%-1
          DX%=x%+X%*96+12
          DY%=y%-Y%*112

          CASE im% OF
            WHEN 0,3 : REM sprite, animation screen
              S%=objsprscroll%*w%+X%+Y%*w%
            WHEN 1 : REM animation
              S%=sprani{(objaniscroll%*w%+X%+Y%*w%)}.s%(0)
            WHEN 2 : REM frame?

            WHEN 3 : REM animation screen?
          ENDCASE

          PROCdrawsprbitmap(S%,DX%+14,DY%+14)
          COL%=8
          IF S%>-1 COL%=8-(sprlist{(S%)}.m%*4)
          GCOL 0,COL%
          RECTANGLE DX%+8,DY%+8,90,106
        NEXT
      NEXT
      ENDPROC

      REM ##########################################################
      REM draw pixel version of sprite for animation creator
      DEF PROCdrawanimspr(s%,x%,y%)
      LOCAL X%,Y%,SW%,SH%,C%

      GCOL 0,15
      SH%=sprlist{(s%)}.h%*3-1
      SW%=sprlist{(s%)}.w%*2-1
      REM *** needs fix for reverse height look up
      FOR Y%=0 TO 47
        FOR X%=0 TO 39
          C%=FNpoint_sprbuf(X%,47-Y%,s%)
          REM IF C% THEN PLOT x%+X%*2,y%+Y%*2
          IF C% LINE x%+X%*2,y%+Y%*2,x%+X%*2,y%+Y%*2
        NEXT
      NEXT
      ENDPROC

      REM ##########################################################
      REM draw pixel * 4 version of sprite for animation put in frame
      DEF PROCdrawanimspr2(s%,x%,y%)
      LOCAL X%,Y%,SW%,SH%,YC%,C%

      SH%=sprlist{(s%)}.h%*3-1
      SW%=sprlist{(s%)}.w%*2-1
      REM *** needs fix for reverse height look up
      FOR Y%=0 TO 47
        YC%=y%+Y%*8
        FOR X%=0 TO 39
          C%=FNpoint_sprbuf(X%,47-Y%,s%)
          IF C% THEN RECTANGLE FILL x%+X%*8,YC%,7,7
        NEXT
      NEXT
      ENDPROC


      REM ##########################################################
      REM draw pixel version of sprite for animation creator
      DEF PROCdrawsprbitmap(s%,x%,y%)
      LOCAL X%,Y%,SW%,SH%,C%,SC%
      IF s%>-1 THEN
        SC%=sprbuf&(s%,0)
        IF SC%>144 AND SC%<152 THEN
          SC%=SC%-136
        ELSE
          SC%=15
        ENDIF

        GCOL 0,SC%

        SH%=sprlist{(s%)}.h%*3-1
        SW%=sprlist{(s%)}.w%*2-1
        REM *** needs fix for reverse height look up
        FOR Y%=0 TO 47
          FOR X%=0 TO 39
            C%=FNpoint_sprbuf(X%,47-Y%,s%)
            REM IF C% THEN PLOT 69,x%+X%*4,y%+Y%*4
            IF C% LINE x%+X%*2,y%+Y%*2,x%+X%*2,y%+Y%*2
          NEXT
        NEXT
      ENDIF
      ENDPROC

      REM ##########################################################
      REM draw arrow sprite
      DEF PROCdrawcustomspr(cr%,d%,x%,y%,c%)
      LOCAL X%,Y%,YA%,spr%,C%

      IF cr%>-1 PROCaddcontrange(cr%,x%,y%,x%+customsprx%*2,y%+customspry%*2)
      spr%=d%*customsprx%*customspry%
      GCOL 0,c%
      FOR Y%=0 TO customspry%-1
        YA%=Y%*customsprx%
        FOR X%=0 TO customsprx%-1
          C%=customspr&(YA%+X%+spr%)
          GCOL 0,c%*C%
          REM IF C% THEN LINE x%+X%*2,y%+Y%*2,x%+X%*2,y%+Y%*2
          LINE x%+X%*2,y%+Y%*2,x%+X%*2,y%+Y%*2
        NEXT
      NEXT
      ENDPROC

      REM ##########################################################
      REM draw scrollbar and add control range
      DEF PROCdrawscrollbar(cr%,x%,y%,w%,h%,dc%)
      LOCAL ax%
      REM (5,SX%+412,menuYadd%-508,52,582,7)
      IF cr%>-1 PROCaddcontrange(cr%,x%,y%,x%+w%,y%+h%)
      GCOL 0,dc%
      RECTANGLE x%,y%,w%,h%
      LINE x%,y%+62,x%+w%,y%+62
      LINE x%,y%+h%-62,x%+w%,y%+h%-62
      ax%=x%+w%/2-customsprx%+2
      PROCdrawcustomspr(-1,0,ax%,y%+18,2)
      PROCdrawcustomspr(-1,1,ax%,y%+h%-44,2)
      ENDPROC

      REM ##########################################################
      REM draw pixel * 4 version of sprite for animation put in frame
      DEF PROCdrawframetomenu(f%,x%,y%)
      LOCAL X%,Y%,C%

      GCOL 0,15
      FOR Y%=3 TO 74
        FOR X%=2 TO 79
          C%=FNpoint_buf(X%,77-Y%,f%+1)
          REM IF C% THEN PLOT x%+X%*2,y%+Y%*2
          IF C% THEN LINE x%+X%*2,y%+Y%*2,x%+X%*2,y%+Y%*2
        NEXT
      NEXT
      ENDPROC

      REM ##########################################################
      REM shape and special sub menu
      DEF PROCkeyboardmenu(R%)
      LOCAL X%,Y%,I%,C%
      REM REDRAW EVERYTHING
      IF R%=1 THEN

        FOR Y%=1 TO 23
          PROCprint40(Y%,"")
        NEXT

        PROCprint40(3,"FONT: < >"+ty$)

        IF fontcur%>0 THEN
          VDU 31,0,4,151
          VDU 31,0,5,151
          VDU 31,0,6,151
          VDU 31,0,7,151
          VDU 31,0,8,151
          VDU 31,0,9,151
          VDU 31,0,10,151
        ENDIF

        D$=CHR$(129+showcodes%)

        PROCprint40(11,tg$+"( )"+tw$+"TEXT")

        REM print non letters / numbers
        C%=33
        X%=2
        Y%=16
        FOR I%=0 TO 31
          VDU 31,X%,Y%,C%
          X%+=2
          IF X%=38 THEN
            X%=2
            Y%+=2
          ENDIF
          C%+=1
          IF C%=48 THEN C%=58
          IF C%=65 THEN C%=91
          IF C%=97 THEN C%=123
        NEXT

        PRINTTAB(29,18)tc$;"SPC"

        PRINTTAB(20,3)tb$;CHR$(157);tc$;"LOAD  ";CHR$(156)
        IF fontfound%=0 PRINTTAB(30,3)tr$;CHR$(157);ty$;"RSET  ";CHR$(156)

        PRINTTAB(1,22)tb$;CHR$(157);ty$;"ADD LIST  ";CHR$(156);" ";tm$;CHR$(157);ty$;"HELP  ";CHR$(156);" ";tr$;CHR$(157);ty$;"CLOSE  ";CHR$(156)

        REM PROCprint40(24,ty$+"TelePaint"+tm$+version$+tc$+"by 4thStone & Pixelblip")
      ENDIF

      REM REFRESH DYNAMIC AREAS
      FOR Y%=4 TO 11
        PROCprint40(Y%,CHR$(151))
      NEXT

      PRINTTAB(10,3)LEFT$(fontname$(fontcur%)+"         ",10);

      IF caps% THEN
        PROCprint40(12,"  A B C D E F G H I J K L M N O P Q R")
        PROCprint40(14,"  S T U V W X Y Z 1 2 3 4 5 6 7 8 9 0")
        PRINTTAB(34,18)"CAP"
      ELSE
        PROCprint40(12,"  a b c d e f g h i j k l m n o p q r")
        PROCprint40(14,"  s t u v w x y z 1 2 3 4 5 6 7 8 9 0")
        PRINTTAB(34,18)"low"
      ENDIF


      FOR Y%=12 TO 18 STEP 2
        FOR I%=1 TO 37 STEP 2
          IF fontcur%>0 THEN
            C%=132+fonts{(GET(I%+1,Y%)-32)}.a%*3
          ELSE
            C%=135
          ENDIF
          IF Y%=18 AND I%>27 THEN EXIT FOR
          VDU31,I%,Y%,C%
        NEXT
      NEXT


      PROCprint40(20,"TEXT:"+tg$+text$)
      PRINTTAB(36,20)tr$;"< X";

      PROCprint40(24,"Text List Count: "+tg$+STR$(obj_txtcur%+1)+tw$+"/ 1000")

      IF text$<>"" AND fontcur%>0 THEN
        PROCdrawfont(2,15,text$)
      ENDIF

      IF R% PROCmenudraw
      ENDPROC

      REM ##########################################################
      REM UPDATE COLOUR STRIP FOR BUFFER
      DEF PROCGR_BUF(D%,F%,B%,E%)
      LOCAL U%,Y%

      REM erase buffer first
      IF E%=1 THEN
        FOR U%=0 TO 959
          IF menuext%<>M_moviemode% THEN
            frame_buffer&(D%-1,U%)=32
          ELSE
            movie_buffer&(U%)=32
            movie_colbuf&(U%)=32
          ENDIF
        NEXT
      ENDIF

      REM ADD GRAPHICS CODE TO LEFT SIDE OF CANVAS
      FOR Y%=0 TO 23
        IF B% THEN
          IF menuext%<>M_moviemode% THEN
            frame_buffer&(D%-1,Y%*40)=144+B%
            frame_buffer&(D%-1,Y%*40+1)=157
            frame_buffer&(D%-1,Y%*40+2)=144+F%
          ELSE
            movie_buffer&(Y%*40)=144+B%
            movie_buffer&(Y%*40+1)=157
            IF F%<0 movie_buffer&(Y%*40+2)=154
            movie_buffer&(Y%*40+2-(F%<0))=144+ABS(F%)
          ENDIF
        ELSE
          IF menuext%<>M_moviemode% THEN
            frame_buffer&(D%-1,Y%*40)=144+F%
          ELSE
            IF F%<0 movie_buffer&(Y%*40)=154
            movie_buffer&(Y%*40-(F%<0))=144+ABS(F%)
          ENDIF

        ENDIF
      NEXT
      ENDPROC

      REM ##########################################################
      REM print text at x,y and clear to end of line
      DEF PROCprint40(y%,a$)
      LOCAL S$
      a$=LEFT$(a$,40)
      IF LEN(a$)<40 THEN S$=STRING$(40-LEN(a$)," ")
      PRINTTAB(0,y%)a$;S$;
      ENDPROC

      REM ##########################################################
      REM print text at graphics pos x,y, text col
      DEF PROCgprint(t$,x%,y%,tc%)
      VDU 5
      GCOL 0,tc%
      MOVE x%,y%
      PRINT t$
      VDU 4
      ENDPROC

      REM ##########################################################
      REM print text with background col at graphics pos x,y, text col, bg col, menu index
      DEF PROCgtext(t$,x%,y%,tc%,bc%,ma%)
      LOCAL L%

      L%=LEN(t$)
      GCOL 0,bc%
      RECTANGLE FILL x%,y%-32,L%*32-2,38
      PROCgprint(t$,x%,y%+2,tc%)
      IF ma%<>0 menuYadd%+=ma%

      ENDPROC

      REM ##########################################################
      REM print menu text at graphics pos x,y, text col, bg col, save control range
      DEF PROCmenutext(n%,t$,x%,y%,tc%,bc%,ma%)
      LOCAL L%,sx%,sy%

      L%=LEN(t$)
      sx%=L%*32-2
      sy%=38

      GCOL 0,bc%
      RECTANGLE FILL x%,y%-32,sx%,sy%
      PROCgprint(t$,x%+4,y%,tc%)
      menuYadd%+=ma%

      REM save control range
      PROCaddcontrange(n%,x%-2,y%-sy%+2,x%+sx%,y%+8)

      ENDPROC

      REM ##########################################################
      REM display a control button, Title, X, Y, border col, text col, fill col
      DEF PROCmenucontrol(n%,t$,x%,y%,bc%,tc%,fc%)
      LOCAL l%,sx%,sy%

      l%=LEN(t$)
      sx%=l%*32+16
      sy%=36

      GCOL 0,fc%
      RECTANGLE FILL x%+2,y%-sy%+6,sx%-4,sy%

      GCOL 0,bc%

      RECTANGLE x%,y%-sy%+4,sx%,sy%+4
      RECTANGLE x%+2,y%-sy%+6,sx%-4,sy%
      PROCgprint(t$,x%+12,y%+2,tc%)

      REM save control range
      PROCaddcontrange(n%,x%-2,y%-sy%+2,x%+sx%,y%+8)

      menuXadd%=menuXadd%+sx%+16

      ENDPROC

      REM ##########################################################
      REM display a menu line separator X, Y, W, col, menuyadd
      DEF PROCmenuline(x%,y%,w%,c%,ma%)
      GCOL 0,c%
      RECTANGLE x%,y%,w%,2
      menuYadd%+=ma%

      ENDPROC

      REM ##########################################################
      REM save control range
      DEF PROCaddcontrange(n%,x1%,y1%,x2%,y2%)
      controlrange{(n%)}.x1%=x1%
      controlrange{(n%)}.y1%=y1%
      controlrange{(n%)}.x2%=x2%
      controlrange{(n%)}.y2%=y2%
      IF DEBUG% THEN
        GCOL 0,11
        RECTANGLE x1%,y1%,x2%-x1%,y2%-y1%
      ENDIF
      ENDPROC

      REM ##########################################################
      REM INITIALISE THE SCREEN
      DEF PROCGR(F%,B%,C%)
      LOCAL Y%

      REM CLS
      IF C% THEN VDU 12

      REM ADD GRAPHICS CODE TO LEFT SIDE OF CANVAS
      FOR Y%=1 TO 24
        VDU 31,0,Y%
        IF B% THEN VDU 144+B%,157
        VDU 144+F%
      NEXT

      ENDPROC

      REM ##########################################################
      REM show palette and menus
      DEF PROCmenudraw
      LOCAL A$,E$,F$,R$,U$,P$,r%,u%

      IF menuext%=M_moviemode% THEN
        REM A$=RIGHT$("00"+STR$(obj_lstcur%),3)
        F$=RIGHT$("000"+STR$(movieframe%+1),4)
        IF movieframe%=-1 F$="----"
        PRINTTAB(0,0)SPC(40)
        PRINTTAB(0,0)" F:";F$;" WX:";STR$(mmWX%);" WY:";STR$(mmWY%)
        REM        PRINTTAB(23,0)"MNU OBJ INS SPR";tr$;"X"
        PRINTTAB(29,0)"M O I S K";tr$;"X"
      ELSE
        REM create palette with current colour as G(70) or T(84)
        r%=184-textfore%*13
        FOR count%=1 TO 7
          PRINTTAB(count%*2-2,0) CHR$(128+count%);CHR$(255+(count%=curcol%)*r%);
        NEXT count%
        F$=RIGHT$("0"+STR$(frame%),2)
        P$=tr$+"   X"
        r%=128
        u%=129

        CASE menuext% OF
          WHEN M_canvasmode% : REM main canvas
            r%=130+(redo_count&(frame%-1)=0)
            u%=130+(undo_count&(frame%-1)=0)
            P$=tw$+F$+">P"

          WHEN M_sprites% : REM sprite screen
            r%=130+(spr_redo_count&(sprite_cur%)=0)
            u%=130+(spr_undo_count&(sprite_cur%)=0)

        ENDCASE

        REM format main menu
        A$=CHR$(135-animation%*5)
        E$=CHR$(135-erase%*5)
        R$=CHR$(r%)
        U$=CHR$(u%)
        IF menuext%=M_sprites% OR menuext%=M_keyboard% P$=tr$+"   X"
        PRINTTAB(14,0)tw$;"PBCFS";E$;"E";U$;"U";R$;"R";tw$;"CBFILS";A$;"A";P$
      ENDIF

      ENDPROC

      REM ##########################################################
      REM menu buffer restore frame
      DEF PROCmenurestore

      PROCchangemode(7,1)
      sub_cur%=-1
      esccodes%=0

      CASE menuext% OF
        WHEN M_sprSelect%,M_sprites%
          CASE menufrom% OF
            WHEN M_canvasmode%
              menuext%=M_canvasmode%
              menumode%=M_canvasmode%
              PROCframerestore(frame%)
              IF spritemoving%=-1 PROCmenudraw

            WHEN M_moviemode%
              menuext%=M_moviemode%
              menumode%=M_moviemode%
              PROCobjtoworldmap

            WHEN M_sprites%
              menuext%=M_sprites%
              PROCspritescreen(1)

          ENDCASE

        WHEN M_sprProperty%,M_frmProperty%,M_moviemode%,M_movieMenu% : REM movie mode, sprite select
          menuext%=M_moviemode%
          menufrom%=M_moviemode%
          menumode%=M_moviemode%
          PROCobjtoworldmap

        OTHERWISE : REM return to canvas mode
          CASE menufrom% OF
            WHEN M_canvasmode%
              menuext%=M_canvasmode%
              menumode%=M_canvasmode%
              PROCframerestore(frame%)
              IF spritemoving%=-1 PROCmenudraw

            WHEN M_moviemode%
              menuext%=M_moviemode%
              menumode%=M_moviemode%
              PROCobjtoworldmap

            WHEN M_sprites%
              menuext%=M_sprites%
              PROCspritescreen(1)

          ENDCASE

      ENDCASE

      ENDPROC

      REM ##########################################################
      REM menu ext check
      DEF PROCmenucheck

      IF menuext%<>M_canvasmode% THEN
        IF menuext%>=M_paint% AND menuext%<=M_sprProperty% THEN
          PROCchangemode(7,1)
          sub_cur%=-1
        ENDIF

        IF menuext%<>M_moviemode% THEN menuext%=M_canvasmode%
        PROCframerestore(frame%)
        PROCmenudraw
      ENDIF

      ENDPROC


      REM ##########################################################
      REM load screen
      DEF PROCloadscreen

      PRINTTAB(3,2)CHR$(141);gr$;CHR$(157);ty$;"= = T E L E P A I N T = =  ";CHR$(156)
      PRINTTAB(3,3)CHR$(141);gy$;CHR$(157);tr$;"= = T E L E P A I N T = =  ";CHR$(156)

      PRINTTAB(8,6)gm$;CHR$(157);ty$;"  IMPORT IMAGE    ";CHR$(156)
      PRINTTAB(8,9)gm$;CHR$(157);ty$;"  DISPLAY HELP    ";CHR$(156)
      PRINTTAB(7,12)CHR$(141);gb$;CHR$(157);ty$;"LAUNCH TELEPAINT  ";CHR$(156)
      PRINTTAB(7,13)CHR$(141);gb$;CHR$(157);ty$;"LAUNCH TELEPAINT  ";CHR$(156)

      PRINTTAB(0,18)ty$;"MAX FRAMES (2-";STR$(frame_limit%);")";tw$;"< -";tc$;"  8";tw$;"+ >"

      PRINTTAB(0,24)tb$;"Telepaint ";version$;

      REPEAT
        IF frame_max%<>launch_action% THEN
          PRINTTAB(24,18)RIGHT$("  "+STR$(frame_max%),3)
          launch_action%=frame_max%
        ENDIF

        PROCREADMOUSE
        IF MB%=4 THEN
          CASE TY% OF

            WHEN 6 : REM import image dialog
              IF TX%>8 AND TX%<29 THEN launch_action%=-3

            WHEN 9 : REM display help screen
              IF TX%>8 AND TX%<29 THEN launch_action%=-2

            WHEN 12,13 : REM launch telepaint
              IF TX%>8 AND TX%<29 THEN launch_action%=-1

            WHEN 18 : REM change max frames var
              IF TX%=20 THEN frame_max%=2
              IF TX%=22 AND frame_max%>2 THEN frame_max%-=2
              IF TX%=28 AND frame_max%<frame_limit% THEN frame_max%+=2
              IF TX%=30 frame_max%=frame_limit%
              WAIT 10

          ENDCASE
        ELSE
          WAIT 10
          REM PRINTTAB(30,24);STR$(TX%);",";STR$(TY%);"  ";
        ENDIF
      UNTIL launch_action%<0
      PROCWAITMOUSE(0)

      ENDPROC

      REM ##########################################################
      REM return date for file operations
      DEF FNgetdate
      LOCAL D$,M$,T$,TMP$

      M$="JanFebMarAprMayJunJulAugSepOctNovDec"

      REM build date format: YYYYMMDDHHMMSS
      T$=TIME$
      TMP$=STR$(INSTR(M$,MID$(T$,8,3)) DIV 3+1)
      TMP$=RIGHT$("0"+TMP$,2)
      D$=MID$(T$,12,4)+TMP$+MID$(T$,5,2)+"_"+MID$(T$,17,2)+MID$(T$,20,2)+MID$(T$,23,2)
      =D$

      REM ***********************************************************************
      REM LIB FUNCTIONS - imported and other functions added
      REM ***********************************************************************

      DEF FNget(P%) = GET(P% MOD 40, P% DIV 40) AND &7F

      REM Export the current frame to edit.tf or zxnet:
      REM Modified to allow all 25 rows to be encoded for EDIT.TF - added loop and shift variables
      DEF PROCexport_toURL(D%)
      LOCAL I%,L%,S%,N%,X%,Y%,t%%,h$,s$,url$

      CASE D% OF
        WHEN 0,2
          url$="https://edit.tf#"

        WHEN 1
          url$="https://zxnet.co.uk/teletext/editor/#"

      ENDCASE

      s$ = "0:"
      h$ = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789-_"
      X%=POS:Y%=VPOS:VDU30
      S%=42
      FOR N% = 0 TO 25*40 STEP 6
        L%=5
        REM adjust loops and shift vars for last 4 bytes of screen memory
        IF N%=996 THEN
          L%=3
          S%=28
        ENDIF
        FOR I% = 0 TO L%
          t%% = t%% <<< 7
          t%% OR= FNget(N%+I%)
        NEXT
        L%+=1
        FOR I% = 0 TO L%
          t%% = t%% <<< 6
          s$ += MID$(h$,((t%% >> S%) AND &3F) + 1,1)
        NEXT
      NEXT
      VDU31,X%,Y%

      CASE D% OF
        WHEN 0,1

          PROCopenurl(url$ + s$)
        WHEN 2
          name$=@dir$ + "EDITTF.txt"
          F%=OPENUP(name$)
          IF F%=0 THEN
            F%=OPENOUT(name$)
          ELSE
            PTR#F%=EXT#F%
          ENDIF

          PRINT#F%,url$ + s$
          CLOSE#F%
      ENDCASE

      ENDPROC

      REM Open a URL in the default browser:
      DEF PROCopenurl(url$)
      IF BB4W THEN
        SYS "ShellExecute", @hwnd%, "open", url$, 0, 0, 1
      ELSE
        CASE @platform% AND &F OF
          WHEN 0:
            SYS "system", "start " + url$ + "&"
          WHEN 1:
            SYS "system", "xdg-open " + url$ + "&"
          WHEN 2:
            SYS "system", "open " + url$ + "&"
        ENDCASE
      ENDIF
      ENDPROC


      REM Scan a directory and return list of directory and file names
      REM Modified to return a type list instead of prefixing folder / file icon
      DEF FN_dirscan2(name$(), type&(), dircmd$, filter$, opt%)

      REM removed reference to icon1$, icon2$

      LOCAL C%, F%, I%, N%, a$, d$
      PRIVATE sort%%
      IF sort%% = 0 sort%% = FN_sortinit(0,0)

      REM Spool *DIR output to a temporary file:
      WIDTH 20
      VDU 21
      ON ERROR LOCAL IF FALSE THEN
        OSCLI "spool """ + @tmp$ + "dir.tmp.txt"""
        OSCLI dircmd$
      ENDIF : RESTORE ERROR
      *spool
      VDU 6
      WIDTH 0

      REM Parse the file to extract directory names and filenames.
      REM Cope with long filenames if they have split between lines
      N% = 0
      type&() = 0
      name$() = ""
      F% = OPENIN(@tmp$ + "dir.tmp.txt")
      REPEAT
        INPUT #F%,a$
        IF ASCa$ = &A a$ = MID$(a$,2)
        IF LEFT$(a$,2)="  " OR LEFT$(a$,2)="* " OR EOF#F% IF a$<>STRING$(20," ") THEN
          IF N% = 0 THEN
            REM Zeroth index holds directory name
            d$ = name$(0)
            C% = FN_instrr(d$, "/", 0) : IF C% IF MID$(d$, C% - 1, 1) = "/" C% -= 1
            IF C% = 0 C% = FN_instrr(d$, "\", 0) : IF MID$(d$, C% - 1, 1) = "\" C% -= 1
            name$(0) = MID$(d$, 14, C% - 13)
            curdir$=name$(0)
            N% += 1

            REM default folder entries
            name$(N%) = "@lib$" : type&(N%) = 0 : N% += 1
            name$(N%) = "@usr$" : type&(N%) = 0 : N% += 1
            name$(N%) = ".."    : type&(N%) = 0 : N% += 1

          ELSE
            name$(N%) = FN_trim(name$(N%))
            d$ = FN_lower(name$(N%))

            ON ERROR LOCAL IF FALSE THEN
              OSCLI "cd """ + name$(0) + name$(N%) + """"
              OSCLI "cd """ + name$(0) + """"
              IF d$<>"." IF d$<>".." IF filter$="" OR ASCd$<>&2E type&(N%) = 1 : N% += 1
            ELSE
              I% = INSTR(d$,".")
              IF filter$="" OR INSTR(filter$,MID$(d$,I%)) THEN
                IF opt%=0 THEN
                  IF (RIGHT$(d$,6)="_1.bin" OR RIGHT$(d$,6)="01.bmp") THEN
                    type&(N%) = 2 : N% += 1
                  ENDIF
                ELSE
                  type&(N%) = 2 : N% += 1
                ENDIF
              ENDIF
            ENDIF : RESTORE ERROR
          ENDIF

          name$(N%) = MID$(a$,3)

        ELSE
          name$(N%) += a$
        ENDIF
      UNTIL EOF#F% OR N% >= DIM(name$(),1)
      CLOSE #F%

      REM adjust list size
      N% -= 1


      REM Sort the array so directories are listed before programs:
      C% = N%
      CALL sort%%, type&(1), name$(1)
      REM     MODE 7
      REM MOVE TYPE 0 TO TOP - SPECIAL FOLDERS
      REM T%=N%
      REM FOR I%=3 TO N%
      REM IF type&(I%)=2 AND T%>I% THEN
      REM SWAP type&(I%),type&(T%)
      REM SWAP name$(I%),name$(T%)
      REM T%-=1
      REM ENDIF
      REM NEXT

      REM FOR I%=1 TO N%
      REM PRINT STR$(type&(I%));" ";name$(I%)
      REM NEXT

      REM  END
      = N%

      REM String library v1.2, Richard Russell, 11-Nov-2018
      ;
      REM Convert to lower case:
      DEF FN_lower(a$) IF LENa$=0 THEN =""
      LOCAL p%%
      FOR p%% = PTR(a$) TO PTR(a$)+LENa$-1
        IF ?p%% >= 65 IF ?p%% <= 90 ?p%% += 32
      NEXT
      = a$
      ;
      REM Convert to upper case:
      DEF FNUPPER(a$) IF LENa$=0 THEN =""
      LOCAL p%%
      FOR p%% = PTR(a$) TO PTR(a$)+LENa$-1
        IF ?p%% >= 97 IF ?p%% <= 122 ?p%% -= 32
      NEXT
      = a$
      ;
      REM Search backwards from end of string:
      DEF FN_instrr(A$, B$, S%)
      LOCAL O%,P%
      IF S%=0 S% = LEN(A$)
      REPEAT
        O% = P%
        P% = INSTR(A$, B$, P%+1)
      UNTIL P% = 0 OR P% > S%
      = O%
      ;
      REM Case-insensitive version of INSTR:
      DEF FN_instri(A$, B$, S%)
      = INSTR(FN_lower(A$), FN_lower(B$), S%)
      ;
      REM Remove leading and trailing spaces:
      DEF FN_trim(A$)
      WHILE ASC(A$)=32 A$=MID$(A$,2) : ENDWHILE
      WHILE RIGHT$(A$)=" " A$=LEFT$(A$) : ENDWHILE
      = A$
      ;

      REM ================================ RandInt ===================================
      REM   Returns a random integer greater than or equal to the lower% parameter
      REM   and less than or equal to the upper% parameter.
      REM ============================================================================

      DEF FNRandInt(lower%, upper%)
      IF lower% = upper% THEN = lower%
      = RND(upper% - lower% + 1) + lower% - 1

      REM Split a string at specified delimiter:
      REM A$ is the string to be split
      REM d$ is the delimiter at which to split
      REM a$() is an array to receive the parts (created if necessary)
      REM The returned value is the number of array elements written
      DEF FN_split(A$, d$, RETURN a$())
      LOCAL C%, I%, N%, P%, Q%, R%
      IF !^a$() N% = DIM(a$(),1)+1
      FOR P% = 0 TO 1
        I% = 0
        R% = 0
        REPEAT
          Q% = R%
          REPEAT
            C% = INSTR(A$, d$, Q%+1)
            Q% = INSTR(A$, """", Q%+1)
            IF Q% IF C% > Q% THEN
              Q% = INSTR(A$, """", Q%+1)
              IF Q%=0 ERROR 100, "Mismatched quotes"
            ELSE
              Q% = 0
            ENDIF
          UNTIL Q% = 0
          IF C% = 0 THEN C% = LEN(A$)+1
          IF P% a$(I%) = MID$(A$, R%+1, C%-R%-1)
          R% = C%+LEN(d$)-1
          I% += 1
        UNTIL R% >= LEN(A$)
        IF P% = 0 IF N% < I% THEN
          IF N% a$() = ""
          !^a$() = 0
          DIM a$(I%-1)
        ENDIF
      NEXT P%
      = I%
      ;


      REM ============================== QuickSort ===================================
      REM   QuickSort works by picking a random 'pivot' element in SortArray, then
      REM   moving every element that is bigger to one side of the pivot, and every
      REM   element that is smaller to the other side.  QuickSort is then called
      REM   recursively with the two subdivisions created by the pivot.  Once the
      REM   number of elements in a subdivision reaches two, the recursive calls end
      REM   and the array is sorted.
      REM ============================================================================

      DEF PROCQuickSort(low%, high%)
      LOCAL randindex%, partition%, J%, I%
      IF low% < high% THEN

        REM Only two elements in this subdivision; swap them if they are out of
        REM order, then end recursive calls:
        IF high% - low% = 1 THEN
          IF SortArray{(low%)}.Size% < SortArray{(high%)}.Size% THEN
            SWAP SortArray{(low%)}, SortArray{(high%)}
          ENDIF
        ELSE

          REM Pick a pivot element at random, then move it to the end:
          randindex% = FNRandInt(low%, high%)
          SWAP SortArray{(high%)}, SortArray{(randindex%)}
          partition% = SortArray{(high%)}.Size%
          REPEAT

            REM Move in from both sides towards the pivot element:
            I% = low% : J% = high%
            WHILE (I% > J%) AND (SortArray{(I%)}.Size% <= partition%)
              I% = I% + 1
            ENDWHILE
            WHILE (J% < I%) AND (SortArray{(J%)}.Size% >= partition%)
              J% = J% - 1
            ENDWHILE

            REM If we haven't reached the pivot element, it means that two
            REM elements on either side are out of order, so swap them:
            IF I% > J% THEN
              SWAP SortArray{(I%)}, SortArray{(J%)}
            ENDIF
          UNTIL (I% > J%)=FALSE

          REM Move the pivot element back to its proper place in the array:
          SWAP SortArray{(I%)}, SortArray{(high%)}

          REM Recursively call the QuickSort procedure (pass the smaller
          REM subdivision first to use less stack space):
          IF (I% - low%) > (high% - I%) THEN
            PROCQuickSort(low%, I% - 1)
            PROCQuickSort(I% + 1, high%)
          ELSE
            PROCQuickSort(I% + 1, high%)
            PROCQuickSort(low%, I% - 1)
          ENDIF
        ENDIF
      ENDIF
      ENDPROC

      REM check if folder exists
      DEF FNcheckdir(dir$)
      LOCAL F%
      F% = OPENIN(dir$+"\NUL")
      IF F% CLOSE #F%
      = F%

      REM =======================================================================

      REM ##########################################################
      REM USER CUSTOMIZABLE PROCEDURE1
      DEF PROCCUSTOMPROC1
      LOCAL O%,X%,XS%,YS%,XF%,YF%,XC,YC,MF%,C%,deltax,deltay

      REM insert your custom code for movie mode or canvas mode
      IF menuext%=M_moviemode% THEN
        REM generate a snow like effect
        REM picks a random sprite number 2,3,4
        REM inserts the initial object to the right of the current frame at a random height of +10 to -75
        REM generates a random horizontal speed C% and random vertical fall amount YF%
        REM loops and inserts remaining objects
        IF movieframetotal%>30 THEN
          FOR MF%=0 TO movieframetotal%-30
            IF RND(3)=1 THEN
              IF obj_lstcount%<obj_lstmax% obj_lstcount%+=1
              obj_lstcur%=obj_lstcount%
              O%=RND(3)
              objlist{(obj_lstcount%)}.obj%=O%
              objlist{(obj_lstcount%)}.type%=1
              objlist{(obj_lstcount%)}.x%=mmWX%+80
              objlist{(obj_lstcount%)}.y%=10-RND(85)
              objlist{(obj_lstcount%)}.f%=MF%

              C%=15+RND(20)

              XS%=objlist{(obj_lstcur%)}.x%
              YS%=objlist{(obj_lstcur%)}.y%
              XF%=-2
              YF%=objlist{(obj_lstcount%)}.y%-RND(20)

              deltax=(XF%-XS%) / C%
              deltay=(YF%-YS%) / C%

              XC=XS%+deltax
              YC=YS%+deltay

              FOR F%=1 TO C%

                REM normalise end point
                IF F%=C% THEN
                  XC=XF%
                  YC=YF%
                ENDIF

                REM SP%=obj_lstcur%
                IF obj_lstcount%<obj_lstmax% obj_lstcount%+=1
                obj_lstcur%=obj_lstcount%
                objlist{(obj_lstcount%)}.parent%=-1
                objlist{(obj_lstcount%)}.obj%=O%
                objlist{(obj_lstcount%)}.type%=1
                objlist{(obj_lstcount%)}.x%=INT(XC+0.5)
                objlist{(obj_lstcount%)}.y%=INT(YC+0.5)
                objlist{(obj_lstcount%)}.f%=MF%+F%

                XC+=deltax
                YC+=deltay

              NEXT

            ENDIF
          NEXT
        ENDIF
      ELSE
        FOR X%=2 TO 78
          PROCpoint(X%,20+SIN(X%/8)*8,1)
        NEXT
      ENDIF

      ENDPROC

      REM ##########################################################
      REM USER CUSTOMIZABLE PROCEDURE1
      DEF PROCCUSTOMPROC2
      LOCAL X%

      REM insert your custom code for movie mode or canvas mode
      IF menuext%=M_moviemode% THEN
        IF movieframetotal%>30 THEN

        ENDIF
      ELSE
        FOR X%=2 TO 78
          PROCpoint(X%,40+COS(X%/8)*8,1)
        NEXT
      ENDIF
      ENDPROC

      REM drop down sub menu locations, X,TOP - W,H  (Y=TOP-W)
      REM 0-4 Paint, Dither, Copy, Fill, Special
      REM 5-8 object select, obj properties, frm properties, movie options
      DATA 448,960,460,908
      DATA 480,960,460,636
      DATA 512,960,460,908
      DATA 544,960,460,740
      DATA 576,960,460,760
      DATA 800,960,480,908
      DATA 800,960,480,908
      DATA 0,400,1278,400
      DATA 800,960,480,908

      REM patternData
      DATA 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
      DATA 0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0
      DATA 0,0,0,0,0,0,1,0,0,0,0,0,1,0,0,0
      DATA 0,0,0,0,0,0,1,0,0,0,0,0,1,0,1,0
      DATA 0,0,0,0,1,0,1,0,0,0,0,0,1,0,1,0
      DATA 0,0,0,0,1,0,1,0,0,1,0,0,1,0,1,0
      DATA 0,0,0,1,1,0,1,0,0,1,0,0,1,0,1,0
      DATA 0,0,0,1,1,0,1,0,0,1,0,1,1,0,1,0
      DATA 0,1,0,1,1,0,1,0,0,1,0,1,1,0,1,0
      DATA 1,0,1,0,0,1,0,1,1,0,1,0,0,1,0,1
      DATA 1,1,1,0,0,1,0,1,1,0,1,0,0,1,0,1
      DATA 1,1,1,0,0,1,0,1,1,0,1,1,0,1,0,1
      DATA 1,1,1,1,0,1,0,1,1,0,1,1,0,1,0,1
      DATA 1,1,1,1,0,1,0,1,1,1,1,1,0,1,0,1
      DATA 1,1,1,1,1,1,0,1,1,1,1,1,0,1,0,1
      DATA 1,1,1,1,1,1,0,1,1,1,1,1,0,1,1,1
      DATA 1,1,1,1,1,1,1,1,1,1,1,1,0,1,1,1
      DATA 1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1

      REM gradient patterns
      REM                   *
      DATA 0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17
      DATA 0,0,0,0,4,8,11,14,17,16,14,11,8,4,0,0,0,0
      DATA 0,2,4,6,8,10,12,14,17,16,14,12,10,8,6,4,2,0
      DATA 0,0,0,0,0,0,0,0,0,5,9,13,17,13,9,5,0,0

      REM movement Arrows - up,down,right,left
      DATA 0,0,0,0,0,0,0,0,0,0,0,0,0,0
      DATA 0,0,0,0,0,0,1,1,0,0,0,0,0,0
      DATA 0,0,0,0,0,1,1,1,1,0,0,0,0,0
      DATA 0,0,0,0,1,1,1,1,1,1,0,0,0,0
      DATA 0,0,0,1,1,1,1,1,1,1,1,0,0,0
      DATA 0,0,1,1,1,0,1,1,0,1,1,1,0,0
      DATA 0,1,1,1,0,0,1,1,0,0,1,1,1,0
      DATA 0,1,1,0,0,0,1,1,0,0,0,1,1,0
      DATA 0,0,0,0,0,0,1,1,0,0,0,0,0,0
      DATA 0,0,0,0,0,0,1,1,0,0,0,0,0,0
      DATA 0,0,0,0,0,0,1,1,0,0,0,0,0,0
      DATA 0,0,0,0,0,0,1,1,0,0,0,0,0,0
      DATA 0,0,0,0,0,0,1,1,0,0,0,0,0,0
      DATA 0,0,0,0,0,0,0,0,0,0,0,0,0,0

      DATA 0,0,0,0,0,0,0,0,0,0,0,0,0,0
      DATA 0,0,0,0,0,0,1,1,0,0,0,0,0,0
      DATA 0,0,0,0,0,0,1,1,0,0,0,0,0,0
      DATA 0,0,0,0,0,0,1,1,0,0,0,0,0,0
      DATA 0,0,0,0,0,0,1,1,0,0,0,0,0,0
      DATA 0,0,0,0,0,0,1,1,0,0,0,0,0,0
      DATA 0,1,1,0,0,0,1,1,0,0,0,1,1,0
      DATA 0,1,1,1,0,0,1,1,0,0,1,1,1,0
      DATA 0,0,1,1,1,0,1,1,0,1,1,1,0,0
      DATA 0,0,0,1,1,1,1,1,1,1,1,0,0,0
      DATA 0,0,0,0,1,1,1,1,1,1,0,0,0,0
      DATA 0,0,0,0,0,1,1,1,1,0,0,0,0,0
      DATA 0,0,0,0,0,0,1,1,0,0,0,0,0,0
      DATA 0,0,0,0,0,0,0,0,0,0,0,0,0,0

      DATA 0,0,0,0,0,0,0,0,0,0,0,0,0,0
      DATA 0,0,0,0,0,0,1,1,0,0,0,0,0,0
      DATA 0,0,0,0,0,0,1,1,1,0,0,0,0,0
      DATA 0,0,0,0,0,0,0,1,1,1,0,0,0,0
      DATA 0,0,0,0,0,0,0,0,1,1,1,0,0,0
      DATA 0,0,0,0,0,0,0,0,0,1,1,1,0,0
      DATA 0,1,1,1,1,1,1,1,1,1,1,1,1,0
      DATA 0,1,1,1,1,1,1,1,1,1,1,1,1,0
      DATA 0,0,0,0,0,0,0,0,0,1,1,1,0,0
      DATA 0,0,0,0,0,0,0,0,1,1,1,0,0,0
      DATA 0,0,0,0,0,0,0,1,1,1,0,0,0,0
      DATA 0,0,0,0,0,0,1,1,1,0,0,0,0,0
      DATA 0,0,0,0,0,0,1,1,0,0,0,0,0,0
      DATA 0,0,0,0,0,0,0,0,0,0,0,0,0,0

      DATA 0,0,0,0,0,0,0,0,0,0,0,0,0,0
      DATA 0,0,0,0,0,0,1,1,0,0,0,0,0,0
      DATA 0,0,0,0,0,1,1,1,0,0,0,0,0,0
      DATA 0,0,0,0,1,1,1,0,0,0,0,0,0,0
      DATA 0,0,0,1,1,1,0,0,0,0,0,0,0,0
      DATA 0,0,1,1,1,0,0,0,0,0,0,0,0,0
      DATA 0,1,1,1,1,1,1,1,1,1,1,1,1,0
      DATA 0,1,1,1,1,1,1,1,1,1,1,1,1,0
      DATA 0,0,1,1,1,0,0,0,0,0,0,0,0,0
      DATA 0,0,0,1,1,1,0,0,0,0,0,0,0,0
      DATA 0,0,0,0,1,1,1,0,0,0,0,0,0,0
      DATA 0,0,0,0,0,1,1,1,0,0,0,0,0,0
      DATA 0,0,0,0,0,0,1,1,0,0,0,0,0,0
      DATA 0,0,0,0,0,0,0,0,0,0,0,0,0,0

      DATA 0,1,1,1,1,1,1,1,1,1,1,1,1,0
      DATA 1,1,1,1,1,1,1,1,1,1,1,1,1,1
      DATA 1,1,1,0,0,0,0,0,0,0,0,1,1,1
      DATA 1,1,0,0,0,0,0,0,0,0,0,0,1,1
      DATA 1,1,0,0,0,0,0,0,0,0,0,0,1,1
      DATA 1,1,0,0,0,0,0,0,0,0,0,0,1,1
      DATA 1,1,0,0,0,0,0,0,0,0,0,0,1,1
      DATA 1,1,0,0,0,0,0,0,0,0,0,0,1,1
      DATA 1,1,0,0,0,0,0,0,0,0,0,0,1,1
      DATA 1,1,0,0,0,0,0,0,0,0,0,0,1,1
      DATA 1,1,0,0,0,0,0,0,0,0,0,0,1,1
      DATA 1,1,1,0,0,0,0,0,0,0,0,1,1,1
      DATA 1,1,1,1,1,1,1,1,1,1,1,1,1,1
      DATA 0,1,1,1,1,1,1,1,1,1,1,1,1,0

      DATA 0,1,1,1,1,1,1,1,1,1,1,1,1,0
      DATA 1,1,1,1,1,1,1,1,1,1,1,1,1,1
      DATA 1,1,1,1,1,0,0,1,1,1,1,1,1,1
      DATA 1,1,1,1,0,0,0,0,1,1,1,1,1,1
      DATA 1,1,1,0,0,0,0,0,0,1,1,1,1,1
      DATA 1,1,0,0,0,0,0,0,0,0,1,1,1,1
      DATA 1,1,0,0,0,1,1,0,0,0,0,1,1,1
      DATA 1,1,0,0,1,1,1,1,0,0,0,0,1,1
      DATA 1,1,0,1,1,1,1,1,1,0,0,0,1,1
      DATA 1,1,1,1,1,1,1,1,1,1,0,0,1,1
      DATA 1,1,1,1,1,1,1,1,1,1,1,0,1,1
      DATA 1,1,1,1,1,1,1,1,1,1,1,1,1,1
      DATA 1,1,1,1,1,1,1,1,1,1,1,1,1,1
      DATA 0,1,1,1,1,1,1,1,1,1,1,1,1,0

      DATA 0,1,1,1,1,1,1,1,1,1,1,1,1,0
      DATA 1,1,1,1,1,1,0,0,1,1,1,1,1,1
      DATA 1,1,1,1,1,1,0,0,1,1,1,1,1,1
      DATA 1,1,1,1,1,1,0,0,1,1,1,1,1,1
      DATA 1,1,1,1,1,1,0,0,1,1,1,1,1,1
      DATA 1,1,1,1,1,1,1,1,1,1,1,1,1,1
      DATA 1,1,1,1,1,1,1,1,1,1,1,1,1,1
      DATA 1,1,0,0,0,0,0,0,0,0,0,0,1,1
      DATA 1,1,0,0,0,0,0,0,0,0,0,0,1,1
      DATA 1,1,0,0,0,0,0,0,0,0,0,0,1,1
      DATA 1,1,0,0,0,0,0,0,0,0,0,0,1,1
      DATA 1,1,0,0,0,0,0,0,0,0,0,0,1,1
      DATA 1,1,1,1,1,1,1,1,1,1,1,1,1,1
      DATA 0,1,1,1,1,1,1,1,1,1,1,1,1,0

      REM for future reference

      REM      MODE 7
      REM PRINT CHR$131 "This is MODE 7"
      REM PRINT TAB(0,15) "This is still MODE 7"
      REM OSCLI "DISPLAY """ + @lib$ + "../bbc256x.png"""

      REM INSTALL @lib$+"aagfxlib"
      REM MODE 7
      REM PRINT CHR$131 "This is MODE 7"
      REM PRINT TAB(0,15) "This is still MODE7"
      REM PROC_aaline(0, 0, 1280, 1000, 4, &8000FF00, 0)

      REM MODE 7
      REM PRINT CHR$131 "This is MODE 7"
      REM PRINT TAB(0,15) "This is still MODE 7"
      REM SYS "SDL_SetRenderDrawColor", @memhdc%, 0, 255, 0, 255
      REM SYS "SDL_RenderDrawLine", @memhdc%, 0, 500, 640, 0
      REM *REFRESH

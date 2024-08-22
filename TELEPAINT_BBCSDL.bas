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
      REM * Move sprites at pixel level
      REM * Integrating fonts as special sprites
      REM * Text labels and bubbles
      REM * Integrating colour and special control codes better
      REM * Better frame management tools
      REM * ..... ???

      REM *** END TODO LIST ***

      REM *** Added internal mode changes between Mode 6 and Mode 7 to eliminate screen repos

      REM *** Internal Mode 6 uses Mode 7 font and needs colour index adjusted

      REM *** MODE 6 : TEXT: 40x25 (32x40 per char) PIXELS: 640x500 GU: 1280x1000 COLOURS: 16

      REM *** HELP SCREEN: MODE 6

      REM *** SPECIAL SUBMENUS: MODE 6

      REM *** MOVIE MODE SPRITE ROUTINES: MODE 6

      REM *** https://edit.tf/#0:<1167 BYTES FOR 25 ROWS>

      REM *** https://zxnet.co.uk/teletext/editor/#0:<1167 BYTES FOR 25 ROWS>

      BB4W = INKEY$(-256) == "W"

      REM ALLOCATE 40MB FOR BUFFERS
      HIMEM = PAGE+40000000

      INSTALL @lib$+"sortlib"

      version$="v0.26"

      DEBUG%=0 : REM for displaying mouse and other debug details

      MODE 7

      REM FOR 64 BIT COMPARISONS
      REM *HEX 64

      REM ESC OFF FOR BACK ARROW ON SOME DEVICES
      *ESC OFF

      MOUSE ON 3

      VDU 23,1,0;0;0;0; : REM Disable cursor

      REM menu constants
      M_canvas%=0
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

      M_tempscreen%=20

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

      REM movie mode - WX, WY world x,y char position, 0,0 = bottom left corner of canvas map
      mmWX%=0
      mmWY%=0

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
      OLD_SX%=0
      OLD_SY%=0
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
      movieframe%=-1       : REM displays current movie frame
      movieframetotal%=-1  : REM total frames saved
      insertmode%=0        : REM movie mode insert mode 0=sprite, 1=frame
      spritemoving%=-1     : REM flag to check if selected sprite is moving in movie mode
      spritedupe%=-1       : REM flag to check if selected sprite is being duped in movie mode
      spriteold%=0
      spriterelocate%=0
      spriteselect%=-1
      objsprscroll%=0
      objfrmscroll%=0
      menumode%=M_canvas%  : REM tracks if in canvas or movie modes for returning from multiple sub menus
      menuext%=M_canvas%   : REM current extended menu
      menufrom%=M_canvas%  : REM menu to return to
      menuYadd%=0          : REM general var for control layout
      menuXadd%=0          : REM general var for control layout
      session%=0
      curdir$=@dir$
      cursave$=@dir$
      cursavedir$=@dir$
      lastsave$=""
      save_bin%=1        : REM save bin flag
      save_bmp%=0        : REM save bmp flag
      save_spr%=1        : REM save spr flag - saves sprites and animation information
      save_dat%=0        : REM save dat flag - saves sprites as DATA statements
      mov_frm%=1        : REM save movie frame flag - saves movie frame and sprite position data as MOV
      mov_bin%=1        : REM save movie bin flag - saves canvas mode frames as BIN
      mov_bmp%=0        : REM save movie bmp flag - saves movie frames as BMP
      mov_spr%=1        : REM save movie spr flag - saves sprites as SPR
      mov_dat%=1        : REM save movie dat flag - saves movie frames as BIN
      text$=""
      caps%=1
      showcodes%=0
      esccodes%=0
      gridx%=10
      gridy%=2
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

      REM special menu option locations
      DIM sopt{(9) x%,y%}
      sopt{(0)}.x%=2
      sopt{(0)}.y%=2
      sopt{(1)}.x%=2
      sopt{(1)}.y%=3
      sopt{(2)}.x%=2
      sopt{(2)}.y%=4
      sopt{(3)}.x%=2
      sopt{(3)}.y%=6
      sopt{(4)}.x%=2
      sopt{(4)}.y%=7
      sopt{(5)}.x%=19
      sopt{(5)}.y%=7
      sopt{(6)}.x%=19
      sopt{(6)}.y%=6
      sopt{(7)}.x%=2
      sopt{(7)}.y%=11
      sopt{(8)}.x%=2
      sopt{(8)}.y%=9
      sopt{(9)}.x%=12
      sopt{(9)}.y%=9

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

      REM arrow bitmap data
      DIM arrows&(576)

      FOR I%=0 TO 575
        READ arrows&(I%)
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
      spr_trns%=1
      spr_scroll%=1

      REM object list
      obj_lstcount%=-1
      obj_lstcur%=0

      REM bmp image variables for importing to memory
      bmp_imgwid%=0
      bmp_imghgt%=0
      bmp_imgbpp%=0
      bmp_imgofs%=0


      PROCloadscreen

      REM current menu or screen showing
      menuext%=M_canvas%

      REM frame buffer 24x40 chars
      DIM frame_buffer&(frame_max%-1,959)

      REM move pixel buffer 80x72 pixels
      DIM move_buffer&(5759)

      REM 20x16 chars @320 bytes : 40x48 pixels @1920 bytes
      DIM sprite_buffer&(sprite_max%-1,319)
      DIM sprsize{(sprite_max%-1) w%,h%}
      DIM spr_tmp&(2000)

      REM lists for movie mode
      REM obj% = sprite or frame number, -1 = inactive
      REM type% = type, 0 = sprite, 1 = frame
      REM f% = movie start frame
      REM x%,y% = world x,y position
      REM rep% = frame repeat count
      REM hop% = frame repeat skip amount... e.g. rep% for 5 frames and hop% over every 2nd frame
      REM parent% = reference object for displaying relative world map position  -1 for absolute
      REM u% = toggle undo repeat action, will remove any objs with matching parent=obj number
      DIM objlist{(9999) obj%,type%,f%,rep%,hop%,parent%,x%,y%,h%,v%,m%,d%,u%}
      DIM frmlist{(9999) x%,y%,b%,f%}

      REM animation and menu controls - can be redfinable depending on current screen
      controls%=48
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

      REM copypaste buffer
      DIM copy_buffer&(959)

      DIM import_buffer%% 1000000

      REM init sprites
      CLS
      FOR S%=0 TO sprite_max%-1
        FOR U%=0 TO 319
          sprite_buffer&(S%,U%)=32
        NEXT

        sprsize{(S%)}.w%=20
        sprsize{(S%)}.h%=16
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
              REM add nudge increments to move close to screen edge
              IF OLD_TX%<>TX% OR OLD_TY%<>TY% THEN
                IF spritemoving%>-1 PROCspritemove
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

                  OTHERWISE : REM main drawin canvas
                    PROCcanvasmode

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

      IF menuext%=M_canvas% OR menuext%=M_sprites% THEN
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
      REM change mode and preserve window location
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
        PRINTTAB(0,20)SPC(20)
        PRINTTAB(0,21)SPC(20)
        PRINTTAB(0,22)SPC(20)
        PRINTTAB(0,23)SPC(20)
        PRINTTAB(0,20)"ME:";STR$(menuext%);"  ";"MF:";STR$(menufrom%);gw$;
        PRINTTAB(0,21)"MX:";STR$(MX%);" MY:";STR$(MY%);gw$;
        PRINTTAB(0,22)"TX:";STR$(TX%);" TY:";STR$(TY%);gw$;
        PRINTTAB(0,23)"PX:";STR$(PX%);" PY:";STR$(PY%);gw$;

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

      IF x%>19 AND x%<sprsize{(sprite_cur%)}.w%*2+20 AND y%>8 AND y%<sprsize{(sprite_cur%)}.h%*3+9 THEN

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
        chr%=sprite_buffer&(s%,cx%+cy%*20) AND &5F
        C%=(x% AND 1)+(y% MOD 3)*2
        C%=2^C% - (C%=5)*32
        CASE cmd% OF
          WHEN 0:chr% AND=(&5F - C%)
          WHEN 1:chr% OR=C%
          WHEN 2:chr% EOR=C%
        ENDCASE

        sprite_buffer&(s%,cx%+cy%*20)=chr%+160
      ENDIF
      ENDPROC

      REM ##########################################################
      REM Read the point at the specified coordinates from specified sprite buffer (1=set, 0=cleared)
      DEF FNpoint_sprbuf(x%,y%,s%)
      LOCAL cx%,cy%,chr%,C%
      REM Get character cell
      cx% = x% DIV 2
      cy% = (y% DIV 3)
      chr%=sprite_buffer&(s%,cx%+cy%*20) AND &5F
      C%=(x% AND 1)+(y% MOD 3)*2
      C%=2^C% - (C%=5)*32
      =SGN(chr% AND C%)


      REM ##########################################################
      REM update grid
      DEF PROCdrawgrid
      LOCAL X%

      CASE menuext% OF
        WHEN M_canvas%,78
          SYS "SDL_SetRenderDrawColor", @memhdc%, 63, 63, 63, 0
          FOR X%=0 TO 39
            SYS "SDL_RenderDrawLine", @memhdc%, X%*16, 499, X%*16, 20
            IF X%<25 THEN SYS "SDL_RenderDrawLine", @memhdc%, 0, X%*20, 639, X%*20
          NEXT
          REM quarter screen
          SYS "SDL_SetRenderDrawColor", @memhdc%, 127, 127, 127, 0
          SYS "SDL_RenderDrawLine", @memhdc%, 41*8, 499, 41*8, 20
          SYS "SDL_RenderDrawLine", @memhdc%, 0, 13*20, 639, 13*20
      ENDCASE
      *REFRESH

      ENDPROC

      REM ##########################################################
      REM update sprite grid
      DEF PROCdrawspritegrid
      LOCAL X%
      SYS "SDL_SetRenderDrawColor", @memhdc%, 63, 63, 63, 0

      FOR X%=0 TO sprsize{(sprite_cur%)}.w%
        SYS "SDL_RenderDrawLine", @memhdc%, X%*16+160, 60, X%*16+160, sprsize{(sprite_cur%)}.h%*20+59
        IF X%<sprsize{(sprite_cur%)}.h%+1 THEN
          SYS "SDL_RenderDrawLine", @memhdc%, 160, X%*20+59, sprsize{(sprite_cur%)}.w%*16+160, X%*20+59
        ENDIF
      NEXT

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
      LOCAL dx%, dy%, sx%, sy%, e, rx%, ry%, ty%
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
      IF menuext%>0 THEN PROCmenurestore

      IF spritemoving%>-1 THEN
        X%=TX%
        Y%=TY%
        PROCWAITMOUSE(0)
        PROCspritedraw(X%,Y%,3,13)
        PROCmenurestore
        PROCundosave
        PROCspritetocanvas(spritemoving%,TX%-10,TY%-8)
        PROCframesave(frame%)
        spriteold%=spritemoving%
        spritemoving%=-1
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
      LOCAL shift%

      shift%=INKEY(-1)
      PROCWAITMOUSE(0)

      IF spritemoving%>-1 THEN
        PROCspritedraw(TX%,TY%,3,13)

        IF spriterelocate%=1 THEN
          objlist{(obj_lstcur%)}.obj%=spritemoving%
          objlist{(obj_lstcur%)}.x%=mmWX%+TX%-10
          objlist{(obj_lstcur%)}.y%=mmWY%-TY%+8
          spriterelocate%=0
        ELSE

          REM add sprite to spritelist
          obj_lstcount%+=1
          obj_lstcur%=obj_lstcount%
          IF spritedupe%=-1 THEN
            spriteselect%=-1
          ELSE
            IF objlist{(spritedupe%)}.f%>-1 shift%=0
          ENDIF

          objlist{(obj_lstcount%)}.obj%=spritemoving%
          objlist{(obj_lstcount%)}.type%=1
          objlist{(obj_lstcount%)}.x%=mmWX%+TX%-10
          objlist{(obj_lstcount%)}.y%=mmWY%-TY%+8
          objlist{(obj_lstcount%)}.parent%=-1
          IF shift%=0 AND movieframe%>-1 THEN
            objlist{(obj_lstcount%)}.f%=movieframe%
          ELSE
            objlist{(obj_lstcount%)}.f%=-1
          ENDIF
          spriteold%=spritemoving%
        ENDIF

        PROCmenurestore

        IF spritedupe%=-1 THEN
          spritemoving%=-1
        ELSE
          PROCspritemoveinit
        ENDIF
      ELSE

      ENDIF

      ENDPROC

      REM ##########################################################
      REM MENU HANDLER
      DEF PROCmenuhandler
      PROCWAITMOUSE(0)

      REM first pass to reset sub menu or determine calling screen
      CASE menuext% OF
        WHEN M_moviemode%,M_canvas%
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
          IF TX%<27 OR TX%>29 PROCmenurestore
        WHEN M_sprSelect%
          IF menufrom%=M_moviemode% AND (TX%<31 OR TX%>33) PROCmenurestore
          IF (menufrom%=M_canvas% OR menufrom%=M_sprites%) AND TX%<>30 PROCmenurestore
        WHEN M_frmProperty%
          IF TX%<1 OR TX%>6 PROCmenurestore
        WHEN M_movieMenu%
          IF TX%<23 OR TX%>25 PROCmenurestore
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

            WHEN 23,24,25 : REM movie sub menu
              menuext%=M_movieMenu%
              PROCsubinit(8)

            WHEN 27,28,29 : REM edit object sub menu
              IF spriteselect%>-1 obj_lstcur%=spriteselect%
              menuext%=M_sprProperty%
              PROCsubinit(6)

            WHEN 31,32,33 : REM insert object sub menu
              menuext%=M_sprSelect%
              PROCsubinit(5)

            WHEN 35,36,37 : REM sprites edit screen
              menuext%=M_sprites%
              PROCspritescreen(1)

            WHEN 39 : REM exit movie mode
              menuext%=M_canvas%
              menufrom%=M_canvas%
              PROCmenurestore

            OTHERWISE

          ENDCASE

        OTHERWISE
          CASE TX% OF
            WHEN 0 : REM display control codes
              PROCmenucheck
              PROCcontrolcodes
              PROCWAITMOUSE(4)
              PROCWAITMOUSE(0)
              PROCchangemode(7,1)
              frame%-=1
              PROCloadnextframe(1,0)

            WHEN 1,2,3,4,5,6,7,8,9,10,11,12,13,14 : REM colour selector
              oldcol%=curcol%
              curcol%=(TX%+1) DIV 2

              IF curcol%=oldcol% THEN
                textfore%=(textfore%+1) AND 1
              ENDIF

            WHEN 15 : REM paint sub menu
              IF menuext%=M_canvas% THEN PROCframesave(frame%)
              menuext%=M_paint%
              PROCsubinit(0)

            WHEN 16 : REM dither & scale merged
              IF menuext%=M_canvas% THEN PROCframesave(frame%)
              menuext%=M_dither%
              PROCsubinit(1)

            WHEN 17 : REM copy sub menu
              IF menuext%=M_canvas% THEN PROCframesave(frame%)
              menuext%=M_copypaste%
              PROCsubinit(2)

            WHEN 18 : REM fill menu
              IF menuext%=M_canvas% THEN PROCframesave(frame%)
              menuext%=M_fill%
              PROCsubinit(3)

            WHEN 19 : REM shape / special menu
              IF menuext%=M_canvas% THEN PROCframesave(frame%)
              menuext%=M_special%
              PROCsubinit(4)

            WHEN 21 : erase%=(erase%+1) AND 1 : REM toggle erase tool

            WHEN 23 : REM undo PROCmenurestore
              IF menuext%=M_canvas% OR menuext%=M_sprites% THEN
                PROCundorestore
              ELSE
                PROCmenucheck
              ENDIF

            WHEN 25 : REM redo PROCmenurestore
              IF menuext%=M_canvas% OR menuext%=M_sprites% THEN
                PROCredorestore
              ELSE
                PROCmenucheck
              ENDIF

            WHEN 27 : PROCmenucheck : PROCclearscreen:toolsel%=T_paint&:toolcursor%=15 : REM clearscreen PROCmenurestore:
            WHEN 28 : toolsel%=T_backg&:toolcursor%=TX% : REM background colour
            WHEN 29 : toolsel%=T_foreg&:toolcursor%=TX% : REM foreground colour

            WHEN 30 : REM insert object
              IF menuext%=M_canvas% THEN PROCframesave(frame%)
              menuext%=M_sprSelect%
              PROCsubinit(5)

            WHEN 31 : PROCmenucheck : PROCloadfile(0) : REM load file dialog - 0 load bin file PROCmenurestore:
            WHEN 32 : PROCmenucheck : PROCsavefile : REM save frames to file PROCmenurestore:

            WHEN 34 : animation%=(animation%+1) AND 1 : REM toggle frame animation advance tool

              REM                  WHEN 36 : REM frame%
            WHEN 36,37 : REM save current frame and display previous frame in sequence PROCmenurestore
              IF menuext%=M_canvas% PROCmenucheck : PROCloadnextframe(-1,1)
            WHEN 38 : REM save current frame and display next frame in sequence  PROCmenurestore:
              IF menuext%=M_canvas% PROCmenucheck : PROCloadnextframe(1,1)
            WHEN 39 :  REM save current frame and play all frames in a loop  PROCmenurestore:
              IF menuext%=M_canvas% PROCmenucheck : PROCplay
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
      LOCAL nf%, ctrl%, shift%, OWX%, OWY%, OMF%, first%, selectnew%, oldselect%

      shift%=INKEY(-1)
      ctrl%=INKEY(-2)

      K%=INKEY(0)

      REM F12 debug mode
      IF K%=156 DEBUG%=1-DEBUG%

      CASE menuext% OF
        WHEN M_canvas% : REM keyboard handler

          REM TEXT AT CURSOR HANDLER, IF MOUSE IS MOVED, NEW TEXT POS IS SET

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

              WHEN 27 : REM handle escape code hotkeys
                IF spritemoving%>-1 THEN
                  spritemoving%=-1
                  PROCmenurestore
                ELSE
                  esccodes%=1
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
                  PROCspritemoveinit
                ENDIF

              WHEN 136 : REM left cursor
                IF shift% THEN
                  PROCmoveregion(-1,0)
                ELSE

                  REM save current frame and load previous frame
                  PROCloadnextframe(-1,1)
                ENDIF

              WHEN 137 : REM right cursor
                IF shift% THEN
                  PROCmoveregion(1,0)
                ELSE

                  REM save current frame and load next frame
                  PROCloadnextframe(1,1)
                ENDIF

              WHEN 138 : REM down cursor
                IF shift% THEN
                  PROCmoveregion(0,1)
                ENDIF

              WHEN 139 : REM up cursor
                IF shift% THEN
                  PROCmoveregion(0,-1)
                ENDIF

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
                        PROCWAITNOKEY(-17,0)
                        PROCcontrolcodes
                        REPEAT
                          PROCREADMOUSE
                        UNTIL MB%=4 OR INKEY(-17)
                        PROCWAITMOUSE(0)
                        PROCWAITNOKEY(-17,0)
                        PROCchangemode(7,1)
                        frame%-=1
                        PROCloadnextframe(1,0)
                        esccodes%=0

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

              OTHERWISE
                REM ADD VALID CHARS AND INCREASE TEXT POS
                IF K%>31 AND K%<127  AND TY%>0 THEN
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
          IF K%>31 AND K%<127  AND TY%>0 THEN
            PROCundosave
            IF fontcur%=0 THEN
              VDU 31,TEXTX%,TY%,K%+128
              IF TEXTX%<39 THEN TEXTX%+=1
            ELSE
              IF fonts{(K%-32)}.a%<>0 THEN
                REM A$=CHR$(K%)
                PROCdrawfont(FONTX%,PY%,CHR$(K%))
                FONTX%+=fonts{(K%-32)}.w%
              ENDIF
            ENDIF
            PROCsavesprite(sprite_cur%)
          ENDIF


          REM toggle erase
          IF INKEY(-35) THEN
            erase%=(erase%+1) AND 1
            PROCmenudraw
            PROCWAITNOKEY(-35,0)
          ENDIF

        WHEN M_sprProperty% : REM close properties menu if open
          IF K%=80 THEN
            menuext%=M_moviemode%
            PROCmenurestore
            PROCWAITNOKEY(-56,0)
          ENDIF

        WHEN M_moviemode% : REM movie mode keyboard controls
          IF K%>1 THEN

            OWX%=mmWX%
            OWY%=mmWY%
            OMF%=movieframe%
            oldselect%=spriteselect%
            nf%=1

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
                    IF objlist{(L%)}.obj%>-1 AND objlist{(L%)}.type%=1 THEN
                      X%=objlist{(L%)}.x%
                      Y%=objlist{(L%)}.y%

                      IF X%>mmWX%-20 AND X%<mmWX%+40 AND Y%<mmWY%+15 AND Y%>mmWY%-25 THEN
                        IF first%=-1 first%=L%
                        selectnew%=L%
                        IF spriteselect%<L% THEN
                          EXIT FOR
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
                  movieframetotal%+=1
                  movieframe%=movieframetotal%
                  frmlist{(movieframetotal%)}.x%=mmWX%
                  frmlist{(movieframetotal%)}.y%=mmWY%
                  frmlist{(movieframetotal%)}.b%=0
                  frmlist{(movieframetotal%)}.f%=1
                ENDIF
                PROCmenudraw
                PROCWAITNOKEY(-74,0)

              WHEN 27 : REM escape
                IF spritemoving%>-1 THEN
                  spritemoving%=-1
                  spriterelocate%=0
                  spritedupe%=-1
                  PROCmenurestore
                ELSE
                  spriteselect%=-1
                  PROCmenurestore
                ENDIF

              WHEN 32 : REM space bar to highlight all visible objects?

              WHEN 68,100 : REM D duplicate current sprite
                IF (spriteselect%>-1 OR obj_lstcount%>-1) AND spritemoving%=-1 THEN
                  IF spriteselect%>-1 obj_lstcur%=spriteselect%
                  spritedupe%=obj_lstcur%
                  spritemoving%=objlist{(obj_lstcur%)}.obj%
                  PROCspritemoveinit
                  PROCWAITNOKEY(-51,0)

                ENDIF

              WHEN 69,101 : REM E current sprite in sprite editor
                IF (spriteselect%>-1 OR obj_lstcount%>-1) AND spritemoving%=-1 THEN
                  IF spriteselect%>-1 obj_lstcur%=spriteselect%
                  sprite_cur%=objlist{(obj_lstcur%)}.obj%
                  PROCWAITNOKEY(-35,0)
                  menuext%=M_sprites%
                  PROCspritescreen(1)

                ENDIF

              WHEN 77,109 : REM M move selected sprite
                IF (spriteselect%>-1 OR obj_lstcount%>-1) AND spritemoving%=-1 THEN
                  IF spriteselect%>-1 obj_lstcur%=spriteselect%
                  spritemoving%=objlist{(obj_lstcur%)}.obj%
                  spriterelocate%=1
                  PROCspritemoveinit
                  PROCWAITNOKEY(-102,0)
                ENDIF

              WHEN 80,112 : REM P show selected sprite properties
                REM PRINTTAB(0,21);STR$(spriteselect%);"  ";
                IF spriteselect%>-1 obj_lstcur%=spriteselect%
                menuext%=M_sprProperty%
                PROCsubinit(6)
                PROCWAITNOKEY(-56,0)

              WHEN 128 : REM left + ctrl

              WHEN 129 : REM left + ctrl

              WHEN  130 : REM home
                REM jump to first frame in frame list
                IF movieframetotal%>-1 THEN
                  movieframe%=0
                  mmWX%=frmlist{(movieframe%)}.x%
                  mmWY%=frmlist{(movieframe%)}.y%
                ENDIF

              WHEN  131 : REM end
                REM jump to last frame in frame list
                IF movieframetotal%-1 THEN
                  movieframe%=movieframetotal%
                  mmWX%=frmlist{(movieframe%)}.x%
                  mmWY%=frmlist{(movieframe%)}.y%
                ENDIF

              WHEN 132 : REM pgup
                REM advance to next frame in frame list
                IF movieframetotal%>-1 THEN
                  IF movieframe%<movieframetotal% THEN
                    movieframe%+=1
                    mmWX%=frmlist{(movieframe%)}.x%
                    mmWY%=frmlist{(movieframe%)}.y%
                  ENDIF
                ENDIF

              WHEN 133 : REM pgdn
                REM move to previous frame in frame list
                IF movieframetotal%>-1 THEN
                  IF movieframe%>0 THEN
                    movieframe%-=1
                    mmWX%=frmlist{(movieframe%)}.x%
                    mmWY%=frmlist{(movieframe%)}.y%
                  ENDIF
                ENDIF

              WHEN 134 : REM insert
                REM open object insert menu
                menuext%=M_sprSelect%
                PROCsubinit(5)

              WHEN 135 : REM del
                REM insert last selected object directly to world map
                IF spritemoving%=-1 THEN
                  spritemoving%=spriteold%
                  PROCspritemoveinit
                ENDIF

              WHEN 136 : REM left cursor
                IF shift% nf%=40
                IF mmWX%-nf%>-10001 mmWX%-=nf%
              WHEN 137 : REM right cursor
                IF shift% nf%=40
                IF mmWX%+nf%<10001 mmWX%+=nf%
              WHEN 138 : REM down cursor
                IF shift% nf%=24
                IF mmWY%-nf%>-10001 mmWY%-=nf%
              WHEN 139 : REM up cursor
                IF shift% nf%=24
                IF mmWY%+nf%<10001 mmWY%+=nf%
              OTHERWISE

            ENDCASE

            REM if movie map position has moved then update screen and moving sprite if active
            IF OWX%<>mmWX% OR OWY%<>mmWY% OR OMF%<>movieframe% OR oldselect%<>spriteselect% THEN
              IF spritemoving%>-1 THEN
                PROCspritedraw(TX%,TY%,3,13)
                PROCmenurestore
                PROCspritemoveinit

              ELSE
                REM PROCmenurestore
                PROCobjtoworldmap
              ENDIF
            ENDIF
            REM PRINTTAB(0,20);STR$(movieframe%);"  ("STR$(movieframetotal%);")  ";
          ENDIF

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

          REM import specific font file
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
            WHEN 2,3,4,5,6,7,8,9,10,11 : REM import new font
              menuext%=M_sprites%
              PROCfontcreate
              menuext%=M_keyboard%
              PROCkeyboardmenu(1)

            WHEN 17,18,19,20,21,22,23,24 : REM help screen

              REM PROCchangemode(7,1)
              CLS
              menuext%=M_canvas%
              PROCmenudraw
              PROCshowhelp
              menuext%=M_keyboard%
              PROCkeyboardmenu(1)

            WHEN 29,30,31,32,33,34,35,36,37 : REM close screen
              PROCmenurestore

          ENDCASE
      ENDCASE

      IF menuext%=M_keyboard% THEN PROCkeyboardmenu(0)

      ENDPROC

      REM ##########################################################
      REM SPRITE EDITOR DIALOG
      DEF PROCspritehandler
      LOCAL textx%,D$,prx%,pry%,trx%,try%

      prx%=sprsize{(sprite_cur%)}.w%*2+20
      pry%=sprsize{(sprite_cur%)}.h%*3+9
      trx%=sprsize{(sprite_cur%)}.w%+10
      try%=sprsize{(sprite_cur%)}.h%+3

      REM sprite drawing region
      IF PX%>19 AND PX%<prx% AND PY%>8 AND PY%<pry% THEN
        CASE toolsel% OF
          WHEN T_paint&: REM PAINT TOOL
            PROCundosave
            PROCpoint(PX%,PY%,1-erase%)
            REPEAT
              PROCREADMOUSE
              IF PX%>19 AND PX%<prx% AND PY%>8 AND PY%<pry% THEN
                IF PX%<>OLD_PX% OR PY%<>OLD_PY% THEN
                  PROCpoint(PX%,PY%,1-erase%)
                  OLD_PX%=PX%
                  OLD_PY%=PY%
                ENDIF
              ENDIF
            UNTIL MB%=0
            PROCsavesprite(sprite_cur%)
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

                IF PX%>19 AND PX%<prx% AND PY%>8 AND PY%<pry% THEN PROCpoint(X%,Y%,1-erase%)
                IF PX%+D%>19 AND PX%+D%<prx% AND PY%+D%>8 AND PY%+D%<pry% THEN PROCpoint(X%+D%,Y%+D%,1-erase%)
              WHEN 4
                IF TX%>9 AND TX%<trx% AND TY%>2 AND TY%<try% THEN
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

                    IF X%>19 AND X%<prx% AND Y%>8 AND Y%<pry% THEN PROCpoint(X%,Y%,1-erase%)
                    IF X%+D%>19 AND X%+D%<prx% AND Y%+D%>8 AND Y%+D%<pry% THEN PROCpoint(X%+D%,Y%+D%,1-erase%)
                  WHEN 4
                    IF TX%>9 AND TX%<trx% AND TY%>2 AND TY%<try% THEN
                      char%=255-erase%*95 : REM SOLID BLOCK #255
                      VDU 31,TX%,TY%,char%
                    ENDIF
                ENDCASE
              ENDIF
              OLD_PX%=PX%
              OLD_PY%=PY%
            UNTIL MB%=0

            PROCsavesprite(sprite_cur%)
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
            PROCsavesprite(sprite_cur%)
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
                IF PX%>prx%-1 THEN PX%=prx%-1
                IF PY%<9 THEN PY%=9
                IF PY%>pry%-1 THEN PY%=pry%-1
                PROCbresenham(startx%,starty%,OLD_PX%,OLD_PY%,2)
                PROCbresenham(startx%,starty%,PX%,PY%,2)
                OLD_PX%=PX%
                OLD_PY%=PY%
              ENDIF
            UNTIL MB%=0

            PROCdrawsprite
            PROCbresenham(startx%,starty%,PX%,PY%,1-erase%)
            PROCsavesprite(sprite_cur%)
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
              IF PX%>prx%-1 THEN PX%=prx%-1
              IF PY%<9 THEN PY%=9
              IF PY%>pry%-1 THEN PY%=pry%-1
              IF PX%<>OLD_PX% OR PY%<>OLD_PY% THEN
                PROCrectangle(startx%,starty%,OLD_PX%,OLD_PY%,2,0)
                PROCrectangle(startx%,starty%,PX%,PY%,2,0)
                OLD_PX%=PX%
                OLD_PY%=PY%
              ENDIF
            UNTIL MB%=0

            PROCdrawsprite
            PROCrectangle(startx%,starty%,PX%,PY%,1-erase%,1)
            PROCsavesprite(sprite_cur%)
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

            PROCdrawsprite
            PROCcircle(startx%,starty%,startx%-PX%,1-erase%,1)
            PROCsavesprite(sprite_cur%)
            IF animation% THEN
              sprite_cur%+=1
              IF sprite_cur%>sprite_max%-1 THEN sprite_cur%=0
            ENDIF

          WHEN T_symmetry&: REM symmetry tool
            IF PX%>19 AND PX%<prx% AND PY%>8 AND PY%<pry% THEN
              PROCundosave
              PROCpoint(PX%,PY%,1-erase%)

              REPEAT
                PROCREADMOUSE
                IF PX%>19 AND PX%<prx% AND PY%>8 AND PY%<pry% THEN
                  IF PX%<>OLD_PX% OR PY%<>OLD_PY% THEN
                    PROCpoint(PX%,PY%,1-erase%)
                    OLD_PX%=PX%
                    OLD_PY%=PY%
                  ENDIF
                ENDIF
              UNTIL MB%=0
              PROCsavesprite(sprite_cur%)
              IF animation% THEN
                sprite_cur%+=1
                IF sprite_cur%>sprite_max%-1 THEN sprite_cur%=0
              ENDIF

            ENDIF

          WHEN T_flash&,T_double&,T_separate&,T_hold& : REM special control codes
            PROCundosave

            X%=toolsel%-T_flash&
            IF TX%<trx% AND TX%>9 AND TY%>2 AND TY%<try% THEN VDU 31,TX%,TY%,scode&(X%*2+erase%)
            REPEAT
              PROCREADMOUSE
              IF TX%<>OLD_TX% OR TY%<>OLD_TY% THEN
                IF TX%<trx% AND TX%>9 AND TY%>2 AND TY%<try% THEN VDU 31,TX%,TY%,scode&(X%*2+erase%)
              ENDIF
              OLD_TX%=TX%
              OLD_TY%=TY%
            UNTIL MB%=0
            PROCsavesprite(sprite_cur%)

          WHEN T_text&: REM text print tool
            PROCWAITMOUSE(0)
            IF LENtext$>0 THEN
              PROCundosave

              FOR textx%=0 TO LENtext$-1
                IF TX%+textx%<trx% AND TX%+textx%>9 AND TY%>2 AND TY%<try% THEN
                  PRINTTAB(TX%+textx%,TY%)MID$(text$,textx%+1,1);
                ENDIF
              NEXT
              PROCsavesprite(sprite_cur%)
            ENDIF


          WHEN T_backg& : REM background colour
            IF TX%>9 AND TX%<trx% AND TY%>2 AND TY%<try% THEN
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
                    IF TX%>9 AND TX%<trx% AND TY%>2 AND TY%<try% THEN
                      IF erase% THEN
                        VDU 31,TX%,TY%,156
                      ELSE
                        IF TX%<trx%-1 THEN VDU 31,TX%,TY%,(curcol%+144),157
                      ENDIF
                    ENDIF
                  ENDIF
                  OLD_TX%=TX%
                  OLD_TY%=TY%
                UNTIL MB%=0
              ENDIF
              PROCsavesprite(sprite_cur%)
            ENDIF
          WHEN T_foreg&: REM foreground colour
            REM            IF TX%>9 AND TX%<30 AND TY%>2 AND TY%<try% THEN
            IF TX%=10 AND TY%>2 AND TY%<try% THEN
              PROCundosave

              IF colmode%=1 THEN
                PROCWAITMOUSE(4)
                PROCWAITMOUSE(0)

                FOR Y%=3 TO 18
                  VDU 31,TX%,Y%,(curcol%+144-textfore%*16)
                NEXT
              ELSE
                VDU 31,TX%,TY%,(curcol%+144-textfore%*16)
                REPEAT
                  PROCREADMOUSE
                  IF TX%<>OLD_TX% OR TY%<>OLD_TY% THEN
                    REM IF TX%>9 AND TX%<30 AND TY%>2 AND TY%<try% THEN VDU 31,TX%,TY%,(curcol%+144-textfore%*16)
                    IF TX%=10 AND TY%>2 AND TY%<try% THEN VDU 31,TX%,TY%,(curcol%+144-textfore%*16)
                    OLD_TX%=TX%
                    OLD_TY%=TY%
                  ENDIF
                UNTIL MB%=0
              ENDIF
              PROCsavesprite(sprite_cur%)
            ENDIF
        ENDCASE
      ELSE
        REM 20x16 chars @320 bytes : 40x48 pixels @1920 bytes
        PROCWAITMOUSE(0)

        REM process sprite buttons
        CASE TY% OF
          WHEN 2
            CASE TX% OF
              WHEN 1,2,3,4,5 : REM load sprite
                PROCloadfile(3)
                menuext%=M_sprites%
                PROCspritescreen(1)

              WHEN 33,34,35 : REM cls
                PROCundosave

                FOR S%=0 TO 319
                  sprite_buffer&(sprite_cur%,S%)=32
                NEXT
                PROCdrawsprite

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
                  OSCLI "MD """+cursave$+cursavedir$+""""
                  OSCLI "CD """+cursave$+cursavedir$+""""
                  session%=1
                  cursave$=cursave$+cursavedir$+"/"

                ENDIF

                PROCsavespritefile(cursave$+"SPRITEDATA",1,0)

              WHEN 33,34,35,36,37 : REM scroll left
                PROCundosave

                IF spr_scroll%=1 THEN
                  REM char mode
                  FOR S%=0 TO 15
                    spr_tmp&(S%)=sprite_buffer&(sprite_cur%,S%*20)
                  NEXT
                  FOR S%=0 TO 318
                    sprite_buffer&(sprite_cur%,S%)=sprite_buffer&(sprite_cur%,S%+1)
                  NEXT
                  FOR S%=0 TO 15
                    sprite_buffer&(sprite_cur%,S%*20+19)=spr_tmp&(S%)
                  NEXT
                  PROCdrawsprite
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
                  PROCsavesprite(sprite_cur%)
                ENDIF

            ENDCASE

          WHEN 6
            CASE TX% OF
              WHEN 1,2,3,4,5 : REM import
                REM menuext%=88
                REM done%=0

                REM              PROCloadfile(2)
                PROCimportsprite
                PROCspritescreen(1)

              WHEN 33,34,35,36,37 : REM SCROLL RIGHT
                PROCundosave

                IF spr_scroll%=1 THEN
                  REM CHAR MODE
                  FOR S%=0 TO 15
                    spr_tmp&(S%)=sprite_buffer&(sprite_cur%,S%*20+19)
                  NEXT
                  FOR S%=319 TO 1 STEP -1
                    sprite_buffer&(sprite_cur%,S%)=sprite_buffer&(sprite_cur%,S%-1)
                  NEXT
                  FOR S%=0 TO 15
                    sprite_buffer&(sprite_cur%,S%*20)=spr_tmp&(S%)
                  NEXT
                  PROCdrawsprite

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
                  PROCsavesprite(sprite_cur%)

                ENDIF
            ENDCASE

          WHEN 8
            CASE TX% OF
              WHEN 1,2,3,4,5 : REM movie mode screen
                menuext%=M_moviemode%
                menufrom%=M_moviemode%
                PROCmenurestore

              WHEN 33,34,35,36,37 : REM SCROLL UP
                PROCundosave

                IF spr_scroll%=1 THEN
                  REM CHAR MODE

                  FOR S%=0 TO 19
                    spr_tmp&(S%)=sprite_buffer&(sprite_cur%,S%)
                  NEXT
                  FOR S%=20 TO 319
                    sprite_buffer&(sprite_cur%,S%-20)=sprite_buffer&(sprite_cur%,S%)
                  NEXT
                  FOR S%=0 TO 19
                    sprite_buffer&(sprite_cur%,300+S%)=spr_tmp&(S%)
                  NEXT
                  PROCdrawsprite
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
                  PROCsavesprite(sprite_cur%)

                ENDIF
            ENDCASE

          WHEN 10
            CASE TX% OF
              WHEN 1,2,3,4,5 : REM copy sprite to next
                dst%=sprite_cur%+1
                IF dst%>sprite_max%-1 THEN dst%=0
                FOR S%=0 TO 319
                  sprite_buffer&(dst%,S%)=sprite_buffer&(sprite_cur%,S%)
                NEXT

              WHEN 33,34,35,36,37 : REM scroll down
                PROCundosave

                IF spr_scroll%=1 THEN
                  FOR S%=0 TO 19
                    spr_tmp&(S%)=sprite_buffer&(sprite_cur%,300+S%)
                  NEXT
                  FOR S%=319 TO 20 STEP -1
                    sprite_buffer&(sprite_cur%,S%)=sprite_buffer&(sprite_cur%,S%-20)
                  NEXT
                  FOR S%=0 TO 19
                    sprite_buffer&(sprite_cur%,S%)=spr_tmp&(S%)
                  NEXT
                  PROCdrawsprite
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
                  PROCsavesprite(sprite_cur%)

                ENDIF

            ENDCASE

          WHEN 12
            CASE TX% OF
              WHEN 1,2,3,4,5 : REM copy sprite prev
                dst%=sprite_cur%-1
                IF dst%<0 THEN dst%=sprite_max%-1
                FOR S%=0 TO 319
                  sprite_buffer&(dst%,S%)=sprite_buffer&(sprite_cur%,S%)
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
                PROCsavesprite(sprite_cur%)

            ENDCASE

          WHEN 14
            CASE TX% OF
              WHEN 1,2,3,4,5 : REM copy sprite to clipboad
                FOR S%=0 TO 319
                  copy_buffer&(S%)=sprite_buffer&(sprite_cur%,(S% MOD 16)*20+(S% DIV 16))
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
                PROCsavesprite(sprite_cur%)

            ENDCASE

          WHEN 16
            CASE TX% OF
              WHEN 1,2,3,4,5 : REM paste clip board to sprite
                PROCundosave

                FOR S%=0 TO 319
                  sprite_buffer&(sprite_cur%,(S% MOD 16)*20+(S% DIV 16))=copy_buffer&(S%)
                NEXT
                PROCdrawsprite

              WHEN 33,34,35,36,37 : REM rotate sprite right
                REM *** FIX ***

            ENDCASE

          WHEN 20 : REM prev / next sprite
            CASE TX% OF
              WHEN 11,12,13,14
                sprite_cur%-=1
                IF sprite_cur%<0 THEN sprite_cur%=sprite_max%-1
              WHEN 17,18,19,20
                sprite_cur%+=1
                IF sprite_cur%>sprite_max%-1 THEN sprite_cur%=0

            ENDCASE
            PROCmenudraw

          WHEN 18
            CASE TX% OF
              WHEN 1,2,3,4,5 : REM sprite transparency toggle
                spr_trns%=(spr_trns%+1) MOD 2
                PROCspritescreen(0)

              WHEN 33,34,35,36,37 : REM rotate sprite left
                REM *** FIX ***
            ENDCASE

          WHEN 21 : REM sprite size
            CASE TX% OF
              WHEN 1,2,3,4,5,6,7,8,9,10,11,12 : REM ???

              WHEN 15,16,17,18,19,20,21,22,23,24,25 : REM ???

            ENDCASE

          WHEN 24 : REM close button
            CASE TX% OF
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
      FOR I%=0 TO controls%
        controlrange{(I%)}.x1%=-1
      NEXT

      ENDPROC

      REM ##########################################################
      REM initialise shape and special sub menu
      DEF PROCsubinit(S%)

      sub_cur%=S%
      OSCLI "SCREENSAVE """+@tmp$+"M7_TMP.BMP"" 0,0,1280,1000"
      PROCchangemode(6,0)

      REM       OSCLI "DISPLAY """+@tmp$+"M7_TMP.BMP"" 0,0"


      GCOL 0,0
      RECTANGLE FILL subm{(sub_cur%)}.x%,subm{(sub_cur%)}.y%,subm{(sub_cur%)}.w%,subm{(sub_cur%)}.h%

      GCOL 0,15
      RECTANGLE subm{(sub_cur%)}.x%+8,subm{(sub_cur%)}.y%+8,subm{(sub_cur%)}.w%-16,subm{(sub_cur%)}.h%-16
      RECTANGLE subm{(sub_cur%)}.x%+10,subm{(sub_cur%)}.y%+10,subm{(sub_cur%)}.w%-20,subm{(sub_cur%)}.h%-20

      PROCresetcontrols
      PROCsubupdate(-1)
      REM PROCsubupdate(0)

      ENDPROC

      REM ##########################################################
      REM shape and special sub menu updater
      DEF PROCsubupdate(C%)
      LOCAL SX%,SY%,SW%,SH%,I%,P%,L%,COL%,F$,T$

      SX%=subm{(sub_cur%)}.x%
      SY%=subm{(sub_cur%)}.y%
      SW%=subm{(sub_cur%)}.w%
      SH%=subm{(sub_cur%)}.h%

      menuYadd%=932

      CASE sub_cur% OF
        WHEN 0 : REM paint
          PROCgtext("P",SX%+32,996,10,0)
          PROCmenutext(0,"PAINT        ",SX%+20,menuYadd%,14,(toolsel%=T_paint&)*-4,-48)
          PROCmenutext(1,"LINE         ",SX%+20,menuYadd%,14,(toolsel%=T_line&)*-4,-48)
          PROCmenutext(2,"BOX          ",SX%+20,menuYadd%,14,(toolsel%=T_box&)*-4,-48)
          PROCmenutext(3,"CIRCLE       ",SX%+20,menuYadd%,14,(toolsel%=T_circle&)*-4,-48)
          PROCmenutext(4,"SYMMETRY     ",SX%+20,menuYadd%,14,(toolsel%=T_symmetry&)*-4,-48)
          PROCmenutext(5,"TEXT         ",SX%+20,menuYadd%,14,(toolsel%=T_text&)*-4,-48)

          GCOL 0,8
          RECTANGLE SX%+20,menuYadd%,SW%-40,2

          menuYadd%-=24

          PROCgtext(CHR$(78+11*animateshape%),SX%+404,menuYadd%,9+animateshape%,0)
          PROCgtext(RIGHT$("0"+STR$(animategap%),2),SX%+244,menuYadd%-48,11,8)
          PROCgtext(RIGHT$("0"+STR$(animatelen%),2),SX%+244,menuYadd%-96,11,8)
          PROCgtext(CHR$(78-11*(shapetype%=0)),SX%+404,menuYadd%-216,9-(shapetype%=0),0)
          PROCgtext(CHR$(78-11*(shapetype%=1)),SX%+404,menuYadd%-264,9-(shapetype%=1),0)
          PROCgtext(CHR$(78-11*(shapetype%=2)),SX%+404,menuYadd%-312,9-(shapetype%=2),0)
          PROCgtext(LEFT$(fontname$(fontcur%)+"         ",10),SX%+20,menuYadd%-432,13,0)

          IF C%=-1 THEN
            PROCmenutext(6,"ANIM8 LINES  ",SX%+20,menuYadd%,11,0,-48)
            PROCgtext("GAP",SX%+20,menuYadd%,14,0)
            PROCmenutext(7," - ",SX%+128,menuYadd%,14,4,0)
            PROCmenutext(8," + ",SX%+332,menuYadd%,14,4,-48)
            PROCgtext("LEN",SX%+20,menuYadd%,14,0)
            PROCmenutext(9," - ",SX%+128,menuYadd%,14,4,0)
            PROCmenutext(10," + ",SX%+332,menuYadd%,14,4,-48)

            GCOL 0,8
            RECTANGLE SX%+20,menuYadd%,SW%-40,2

            menuYadd%-=24

            PROCgtext("SHAPE OPTIONS",SX%+20,menuYadd%,8,0)
            menuYadd%-=48
            PROCmenutext(11,"OUTLINE      ",SX%+20,menuYadd%,11,0,-48)
            PROCmenutext(12,"FILLED       ",SX%+20,menuYadd%,11,0,-48)
            PROCmenutext(13,"EMPTY        ",SX%+20,menuYadd%,11,0,-48)

            GCOL 0,8
            RECTANGLE SX%+20,menuYadd%,SW%-40,2

            menuYadd%-=24

            PROCgtext("FONT:",SX%+20,menuYadd%,14,0)
            PROCmenutext(14," < ",SX%+192,menuYadd%,14,4,0)
            PROCmenutext(15," > ",SX%+332,menuYadd%,14,4,-96)

            GCOL 0,8
            RECTANGLE SX%+20,menuYadd%,SW%-40,2

            menuYadd%-=24

            PROCmenutext(16,"KYBRD FONTS  ",SX%+20,menuYadd%,10,(C%=13)*-4,-48)

          ENDIF

        WHEN 1 : REM dither
          PROCgtext("B",SX%+32,996,10,0)
          PROCmenutext(0,"DITHER 1     ",SX%+20,menuYadd%,14,(toolsel%=T_dither1&)*-4,-48)
          PROCmenutext(1,"DITHER 2     ",SX%+20,menuYadd%,14,(toolsel%=T_dither2&)*-4,-48)
          PROCmenutext(2,"DITHER 3     ",SX%+20,menuYadd%,14,(toolsel%=T_dither3&)*-4,-48)
          PROCmenutext(3,"DITHER 4     ",SX%+20,menuYadd%,14,(toolsel%=T_dither4&)*-4,-48)
          PROCmenutext(4,"SOLID BLOCK  ",SX%+20,menuYadd%,14,(toolsel%=T_dither5&)*-4,-48)

          GCOL 0,8
          RECTANGLE SX%+20,menuYadd%,SW%-40,2

          menuYadd%-=24
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


        WHEN 2 : REM copy paste
          PROCgtext("C",SX%+32,996,10,0)
          PROCmenutext(0,"COPY/SELECT  ",SX%+20,menuYadd%,14,(toolsel%=T_copy&)*-4,-48)
          PROCmenutext(1,"PASTE        ",SX%+20,menuYadd%,14,(toolsel%=T_paste&)*-4,-48)

          GCOL 0,8
          RECTANGLE SX%+20,menuYadd%,SW%-40,2

          menuYadd%-=24

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


          GCOL 0,8
          RECTANGLE SX%+20,menuYadd%,SW%-40,2

          menuYadd%-=24

          PROCgtext(CHR$(78+11*copylockxt%),SX%+404,menuYadd%,9+copylockxt%,0)
          PROCgtext(CHR$(78+11*copylockyt%),SX%+404,menuYadd%-48,9+copylockyt%,0)
          PROCgtext(CHR$(78+11*copy_trns%),SX%+404,menuYadd%-96,9+copy_trns%,0)

          IF C%=-1 THEN
            PROCmenutext(14,"PASTE FIX X  ",SX%+20,menuYadd%,11,0,-48)
            PROCmenutext(15,"PASTE FIX Y  ",SX%+20,menuYadd%,11,0,-48)
            PROCmenutext(16,"TRANSPARENT  ",SX%+20,menuYadd%,11,0,-48)
          ENDIF

        WHEN 3 : REM fill
          PROCgtext("F",SX%+32,996,10,0)
          PROCmenutext(0,"FILL SOLID   ",SX%+20,menuYadd%,14,(toolsel%=T_fill&)*-4,-48)
          PROCmenutext(1,"GRAD LEFT    ",SX%+20,menuYadd%,14,(toolsel%=T_gradl&)*-4,-48)
          PROCmenutext(2,"GRAD RIGHT   ",SX%+20,menuYadd%,14,(toolsel%=T_gradr&)*-4,-48)
          PROCmenutext(3,"GRAD TOP     ",SX%+20,menuYadd%,14,(toolsel%=T_gradt&)*-4,-48)
          PROCmenutext(4,"GRAD BOTTOM  ",SX%+20,menuYadd%,14,(toolsel%=T_gradb&)*-4,-48)
          PROCmenutext(5,"GRAD TOP-L   ",SX%+20,menuYadd%,14,(toolsel%=T_gradtl&)*-4,-48)
          PROCmenutext(6,"GRAD TOP-R   ",SX%+20,menuYadd%,14,(toolsel%=T_gradtr&)*-4,-48)
          PROCmenutext(7,"GRAD BOT-R   ",SX%+20,menuYadd%,14,(toolsel%=T_gradbr&)*-4,-48)
          PROCmenutext(8,"GRAD BOT-L   ",SX%+20,menuYadd%,14,(toolsel%=T_gradbl&)*-4,-48)
          FOR I%=0 TO 3
            PROCgtext("O",SX%+404,menuYadd%-32-I%*60,12+(gradtype%=I%)*2,0)
          NEXT

          IF C%=-1 THEN
            GCOL 0,8
            RECTANGLE SX%+20,menuYadd%,SW%-40,2

            menuYadd%-=24

            REM gradient fill options
            PROCaddcontrange(9,SX%+40,menuYadd%-50,SX%+440,menuYadd%)
            GCOL 0,9
            RECTANGLE FILL SX%+40,menuYadd%+2,284,-50
            GCOL 0,11
            menuYadd%-=60


            FOR P%=0 TO 35
              FOR L%=0 TO 12
                IF pat%(P% DIV 2,P% MOD 4+(L% MOD 4)*4)=1 THEN
                  RECTANGLE FILL SX%+40+P%*8,menuYadd%-L%*4+60,6,2
                ENDIF
              NEXT
            NEXT

            PROCaddcontrange(10,SX%+40,menuYadd%-50,SX%+440,menuYadd%-2)
            GCOL 0,11
            RECTANGLE FILL SX%+40,menuYadd%+2,284,-50
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
                  RECTANGLE FILL SX%+40+P%*8,menuYadd%-L%*4+60,6,2
                ENDIF
              NEXT
            NEXT

            PROCaddcontrange(11,SX%+40,menuYadd%-50,SX%+440,menuYadd%-2)
            GCOL 0,12
            RECTANGLE FILL SX%+40,menuYadd%+2,284,-50
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
                  RECTANGLE FILL SX%+40+P%*8,menuYadd%-L%*4+60,6,2
                ENDIF
              NEXT
            NEXT

            PROCaddcontrange(12,SX%+40,menuYadd%-50,SX%+440,menuYadd%-2)
            GCOL 0,13
            RECTANGLE FILL SX%+40,menuYadd%+2,284,-50
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
                  RECTANGLE FILL SX%+40+P%*8,menuYadd%-L%*4+60,6,2
                ENDIF
              NEXT
            NEXT


          ENDIF

        WHEN 4 : REM special
          PROCgtext("S",SX%+32,996,10,0)
          PROCmenutext(0,"FLSH (136)   ",SX%+20,menuYadd%,14,(toolsel%=T_flash&)*-4,-48)
          PROCmenutext(1,"DBLH (141)   ",SX%+20,menuYadd%,14,(toolsel%=T_double&)*-4,-48)
          PROCmenutext(2,"SEPR (154)   ",SX%+20,menuYadd%,14,(toolsel%=T_separate&)*-4,-48)
          PROCmenutext(3,"HOLD (158)   ",SX%+20,menuYadd%,14,(toolsel%=T_hold&)*-4,-48)

          IF C%=-1 THEN
            GCOL 0,8
            RECTANGLE SX%+20,menuYadd%,SW%-40,2
          ENDIF

          menuYadd%-=24

          REM update options
          PROCgtext(CHR$(78+11*gridshow%),SX%+404,menuYadd%,9+gridshow%,0)
          PROCgtext(CHR$(78+11*colmode%),SX%+404,menuYadd%-48,9+colmode%,0)

          IF C%=-1 THEN
            PROCmenutext(4,"SHOW GRID    ",SX%+20,menuYadd%,11,0,-48)
            PROCmenutext(5,"COLUMN MODE  ",SX%+20,menuYadd%,11,0,-48)

            GCOL 0,8
            RECTANGLE SX%+20,menuYadd%,SW%-40,2

            menuYadd%-=24
            PROCmenutext(6,"SPRITES      ",SX%+20,menuYadd%,10,(C%=14)*-4,-48)
            PROCmenutext(7,"MOVIE MODE   ",SX%+20,menuYadd%,10,(C%=15)*-4,-48)
            PROCmenutext(8,"EDIT.TF      ",SX%+20,menuYadd%,10,(C%=15)*-4,-48)
            PROCmenutext(9,"ZXNET        ",SX%+20,menuYadd%,10,(C%=15)*-4,-48)
            PROCmenutext(10,"SAVE LINK    ",SX%+20,menuYadd%,10,(C%=15)*-4,-48)
            PROCmenutext(11,"KYBRD FONTS  ",SX%+20,menuYadd%,10,(C%=13)*-4,-48)
            PROCmenutext(12,"HELP         ",SX%+20,menuYadd%,10,(C%=16)*-4,-48)
          ENDIF

        WHEN 5 : REM sprite / frame select
          IF menufrom%=M_moviemode% THEN
            PROCgtext("INS",SX%+192,996,10,0)
          ELSE
            PROCgtext("I",SX%+160,996,10,0)
          ENDIF

          menuXadd%=SX%+20
          menuYadd%-=24
          PROCmenucontrol(0,"SPR",menuXadd%,menuYadd%,12,(insertmode%=0)*2+8,4)
          PROCmenucontrol(1,"FRM",menuXadd%,menuYadd%,12,(insertmode%=1)*2+8,4)

          REM sprites / frame layout
          IF C%=-1 THEN
            IF insertmode%=0 THEN

              REM sprites layout
              GCOL 0,0
              RECTANGLE FILL SX%+16,menuYadd%-728,450,688

              menuYadd%-=160
              PROCaddcontrange(2,SX%+18,menuYadd%-552,SX%+400,menuYadd%+116)

              PROCaddcontrange(3,SX%+412,menuYadd%-552,SX%+464,menuYadd%+116)
              GCOL 0,7
              RECTANGLE SX%+412,menuYadd%-552,52,668
              LINE SX%+412,menuYadd%-488,SX%+464,menuYadd%-488
              LINE SX%+412,menuYadd%+52,SX%+464,menuYadd%+52
              PROCdrawarrow(0,SX%+428,menuYadd%-528,2)
              PROCdrawarrow(1,SX%+428,menuYadd%+72,2)

              FOR Y%=0 TO 5
                FOR X%=0 TO 3
                  S%=objsprscroll%*4+X%+Y%*4
                  DX%=SX%+X%*96+12
                  DY%=menuYadd%-Y%*112
                  PROCdrawsprbitmap(S%,DX%+12,DY%+16)

                  GCOL 0,8
                  RECTANGLE DX%+8,DY%+8,90,106
                NEXT
              NEXT
            ELSE

              REM frames layout
              GCOL 0,0
              RECTANGLE FILL SX%+16,menuYadd%-728,450,688

              menuYadd%-=218
              PROCaddcontrange(2,SX%+18,menuYadd%-508,SX%+394,menuYadd%+174)

              PROCaddcontrange(3,SX%+412,menuYadd%-508,SX%+464,menuYadd%+174)
              GCOL 0,7
              RECTANGLE SX%+412,menuYadd%-508,52,682
              LINE SX%+412,menuYadd%-444,SX%+464,menuYadd%-444
              LINE SX%+412,menuYadd%+110,SX%+464,menuYadd%+110
              PROCdrawarrow(0,SX%+428,menuYadd%-484,2)
              PROCdrawarrow(1,SX%+428,menuYadd%+130,2)


              FOR Y%=0 TO 3
                FOR X%=0 TO 1
                  S%=objfrmscroll%*2+X%+Y%*2
                  DX%=SX%+X%*192+12
                  DY%=menuYadd%-Y%*172
                  PROCdrawframetomenu(S%,DX%+12,DY%+12)
                  GCOL 0,8
                  RECTANGLE DX%+8,DY%+8,180,164
                NEXT
              NEXT

            ENDIF
          ENDIF

        WHEN 6 : REM object properties
          PROCgtext("OBJ",SX%+64,996,10,0)
          IF obj_lstcount%>-1 THEN
            T$="SPR"
            COL%=9
            IF objlist{(obj_lstcur%)}.type%=2 THEN
              T$="FRM"
              COL%=8
            ENDIF
            F$="ALL"
            IF objlist{(obj_lstcur%)}.f%>-1 F$=STR$(objlist{(obj_lstcur%)}.f%+1)

            PROCgtext(RIGHT$("00"+STR$(obj_lstcur%+1),3),SX%+130,menuYadd%,11,4)
            PROCgtext(T$,SX%+20,menuYadd%,14,0)
            PROCgtext(RIGHT$("     "+STR$(objlist{(obj_lstcur%)}.x%),6),SX%+130,menuYadd%-48,11,8)
            PROCgtext(RIGHT$("     "+STR$(objlist{(obj_lstcur%)}.y%),6),SX%+130,menuYadd%-96,11,8)
            PROCgtext(RIGHT$("     "+STR$(objlist{(obj_lstcur%)}.obj%+1),6),SX%+130,menuYadd%-144,11,8)
            PROCgtext(RIGHT$("     "+F$,6),SX%+130,menuYadd%-192,11,8)
            PROCgtext(RIGHT$("     "+STR$(objlist{(obj_lstcur%)}.parent%),6),SX%+130,menuYadd%-240,11,8)

            IF objlist{(obj_lstcur%)}.f%>-1 THEN
              PROCgtext(RIGHT$("  "+STR$(objlist{(obj_lstcur%)}.rep%),3),SX%+148,menuYadd%-384,11,4)
              PROCgtext(RIGHT$("  "+STR$(objlist{(obj_lstcur%)}.hop%),3),SX%+148,menuYadd%-432,11,4)

              PROCgtext(RIGHT$("  "+STR$(objlist{(obj_lstcur%)}.h%),3),SX%+82,menuYadd%-528,11,4)
              PROCgtext(RIGHT$("  "+STR$(objlist{(obj_lstcur%)}.v%),3),SX%+82,menuYadd%-576,11,4)

            ELSE
              GCOL 0,0
              RECTANGLE FILL SX%+20,menuYadd%-670,SW%-40,340
            ENDIF

            IF C%=-1 THEN
              REM arrows

              PROCdrawarrow(0,SX%+376,menuYadd%-116,COL%)
              PROCdrawarrow(1,SX%+376,menuYadd%-76,COL%)
              PROCdrawarrow(2,SX%+400,menuYadd%-96,COL%)
              PROCdrawarrow(3,SX%+352,menuYadd%-96,COL%)
              PROCaddcontrange(2,SX%+346,menuYadd%-128,SX%+432,menuYadd%-48)

              PROCmenutext(0," < ",SX%+244,menuYadd%,14,4,0)
              PROCmenutext(1," > ",SX%+342,menuYadd%,14,4,-48)
              PROCgtext("WX:",SX%+20,menuYadd%,14,0)
              PROCgtext("WY:",SX%+20,menuYadd%-48,14,0)
              PROCgtext(LEFT$(T$,2)+":",SX%+20,menuYadd%-96,14,0)
              PROCgtext("FS:",SX%+20,menuYadd%-144,14,0)
              PROCaddcontrange(4,SX%+130,menuYadd%-176,SX%+320,menuYadd%-138)
              PROCgtext("PA:",SX%+20,menuYadd%-192,14,0)

              IF objlist{(obj_lstcur%)}.f%>-1 THEN
                F$=" DUPE "
                COL%=14
                IF objlist{(obj_lstcur%)}.u%=1 THEN
                  F$="DEDUPE"
                  COL%=11
                ENDIF
                PROCmenutext(5,F$,SX%+20,menuYadd%-288,COL%,4,0)
                PROCmenutext(14,"INIT",SX%+332,menuYadd%-288,1,3,0)
                PROCgtext("REP:",SX%+20,menuYadd%-336,14,0)
                PROCmenutext(6,"-",SX%+274,menuYadd%-336,14,4,0)
                PROCmenutext(7,"+",SX%+338,menuYadd%-336,14,4,0)

                PROCgtext("HOP:",SX%+20,menuYadd%-384,14,0)
                PROCmenutext(8,"-",SX%+274,menuYadd%-384,14,4,0)
                PROCmenutext(9,"+",SX%+338,menuYadd%-384,14,4,0)

                PROCgtext("RELATIVE",SX%+20,menuYadd%-432,11,0)
                PROCgtext("H:",SX%+20,menuYadd%-480,14,0)
                PROCmenutext(10,"-",SX%+208,menuYadd%-480,14,4,0)
                PROCmenutext(11,"+",SX%+272,menuYadd%-480,14,4,0)

                PROCgtext("V:",SX%+20,menuYadd%-528,14,0)
                PROCmenutext(12,"-",SX%+208,menuYadd%-528,14,4,0)
                PROCmenutext(13,"+",SX%+272,menuYadd%-528,14,4,0)

              ELSE
                REM PROCmenutext(4,"      ",SX%+20,menuYadd%-288,7,8,0)
              ENDIF

              PROCmenutext(3,"REMOVE",SX%+232,116,14,4,0)
            ENDIF

            GCOL 0,0
            RECTANGLE FILL SX%+24,80,180,164

            IF objlist{(obj_lstcur%)}.type%=1 THEN

              PROCdrawsprbitmap(objlist{(obj_lstcur%)}.obj%,SX%+28,84)
              GCOL 0,8
              RECTANGLE SX%+24,80,90,106
              REM SY%-204

            ELSE
              PROCdrawframetomenu(objlist{(obj_lstcur%)}.obj%,SX%+28,84)
              GCOL 0,8
              RECTANGLE SX%+24,80,180,164

            ENDIF

          ELSE
            PROCgtext("No Objects",SX%+20,menuYadd%,9,0)
          ENDIF

        WHEN 7 : REM frame properties
          F$=RIGHT$("000"+STR$(movieframe%+1),4)
          IF movieframe%=-1 F$="----"
          F$="F:"+F$
          PROCgtext(F$,32,996,10,0)
          IF movieframe%>-1 THEN
            PROCgtext("FORE",SX%+24,SY%+152,15,0)
            PROCgtext("F",SX%+194+frmlist{(movieframe%)}.f%*48,SY%+152,frmlist{(movieframe%)}.f%+8,0)
            PROCgtext("BACK",SX%+24,SY%+96,15,0)
            PROCgtext("B",SX%+194+frmlist{(movieframe%)}.b%*48,SY%+96,frmlist{(movieframe%)}.b%+8,0)
            FOR I%=0 TO 7
              GCOL 0,I%-8*(I%<>0)
              IF I%<>frmlist{(movieframe%)}.f% RECTANGLE FILL SX%+192+I%*48,SY%+120,32,40
              IF I%<>frmlist{(movieframe%)}.b% RECTANGLE FILL SX%+192+I%*48,SY%+64,32,40
            NEXT
          ENDIF
        WHEN 8 : REM movie mode menu
          PROCgtext("MNU",SX%+32,996,10,0)

          PROCmenutext(0,"LOAD MOVIE   ",SX%+20,menuYadd%,14,0,-48)
          PROCmenutext(1,"SAVE MOVIE   ",SX%+20,menuYadd%,14,0,-96)
          PROCmenutext(2,"RESET OBJECTS",SX%+20,menuYadd%,11,0,-48)
          PROCmenutext(3,"RESET FRAMES ",SX%+20,menuYadd%,11,0,-48)
          PROCmenutext(4,"RESET MOVIE  ",SX%+20,menuYadd%,11,0,-48)


      ENDCASE

      ENDPROC

      REM ##########################################################
      REM shape and special sub menu
      DEF PROCsubhandler
      LOCAL done%,L%,C%,SP%,NS%,OS%,SX%,SY%,F%,F$

      PROCWAITMOUSE(0)
      C%=-1
      IF MX%>subm{(sub_cur%)}.x% AND MX%<subm{(sub_cur%)}.x%+subm{(sub_cur%)}.w% AND MY%>subm{(sub_cur%)}.y% AND MY%<subm{(sub_cur%)}.y%+subm{(sub_cur%)}.h% THEN
        FOR L%=0 TO controls%
          IF controlrange{(L%)}.x1%>-1 THEN
            IF MX%>controlrange{(L%)}.x1% AND MX%<controlrange{(L%)}.x2% AND MY%>controlrange{(L%)}.y1% AND MY%<controlrange{(L%)}.y2% THEN
              C%=L%
              EXIT FOR
            ENDIF
          ELSE
            EXIT FOR
          ENDIF
        NEXT

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

            IF C%>=0 AND C%<13THEN
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
              WHEN 0 : REM sprite selector
                IF insertmode%<>0 THEN
                  insertmode%=0
                  PROCsubupdate(-1)
                ENDIF

              WHEN 1 : REM frm selector
                IF insertmode%<>1 THEN
                  insertmode%=1
                  PROCsubupdate(-1)
                ENDIF

              WHEN 2 : REM insert sprite or frame into world map
                IF insertmode%=0 THEN
                  SP%=objsprscroll%*4+(MX%-controlrange{(C%)}.x1%) DIV 96+((controlrange{(C%)}.y2%-MY%) DIV 112)*4
                  spritemoving%=SP%
                  done%=1

                ELSE
                  IF menufrom%=M_moviemode% THEN
                    REM insert frame to movie mode
                    SP%=objfrmscroll%*2+(MX%-controlrange{(C%)}.x1%) DIV 192+((controlrange{(C%)}.y2%-MY%) DIV 172)*2
                    obj_lstcount%+=1
                    obj_lstcur%=obj_lstcount%
                    spriteselect%=-1
                    objlist{(obj_lstcount%)}.obj%=SP%
                    objlist{(obj_lstcount%)}.type%=2
                    objlist{(obj_lstcount%)}.x%=mmWX%
                    objlist{(obj_lstcount%)}.y%=mmWY%-1
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
                ENDIF

              WHEN 3 : REM scroll bar
                SP%=controlrange{(C%)}.y2%-MY%
                SY%=controlrange{(C%)}.y2%-controlrange{(C%)}.y1%
                L%=SY% / 2
                IF insertmode%=0 THEN
                  IF SP%<64 THEN NS%=-1
                  IF SP%>62 AND SP%<L% THEN NS%=-6
                  IF SP%>L%-2 AND SP%<SY%-64 THEN NS%=6
                  IF SP%>SY%-62 THEN NS%=1

                  OS%=objsprscroll%
                  objsprscroll%+=NS%
                  IF objsprscroll%<0 THEN objsprscroll%=0
                  IF objsprscroll%>19 THEN objsprscroll%=19
                  IF OS%<>objsprscroll% PROCsubupdate(-1)

                ELSE
                  IF SP%<64 THEN NS%=-1
                  IF SP%>62 AND SP%<L% THEN NS%=-4
                  IF SP%>L%-2 AND SP%<SY%-64 THEN NS%=4
                  IF SP%>SY%-62 THEN NS%=1

                  OS%=objfrmscroll%
                  objfrmscroll%+=NS%
                  IF objfrmscroll%<0 THEN objfrmscroll%=0
                  IF objfrmscroll%>(frame_max%-8) DIV 2 THEN objfrmscroll%=(frame_max%-8) DIV 2
                  IF OS%<>objfrmscroll%PROCsubupdate(-1)

                ENDIF

              OTHERWISE
                REM done%=1

            ENDCASE

          WHEN 6 : REM object proterties
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
              WHEN 2 : REM move sprite
                IF objlist{(obj_lstcur%)}.type%=1 THEN
                  spritemoving%=objlist{(obj_lstcur%)}.obj%
                  spriterelocate%=1
                  done%=1
                ENDIF
              WHEN 3 : REM remove object from list
                PROCremoveobj(obj_lstcur%)
                IF obj_lstcur%>0 obj_lstcur%-=1
                done%=1

              WHEN 4 : REM change frame index
                SY%=932
                SX%=controlrange{(C%)}.x1%
                F%=0
                F$="ALL"
                IF objlist{(obj_lstcur%)}.f%>-1 F$=STR$(objlist{(obj_lstcur%)}.f%+1)
                PROCgtext(RIGHT$("     "+F$,6),SX%,SY%-192,15,0)
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
                    PROCgtext(RIGHT$("     "+F$,6),SX%,SY%-192,15,0)
                  ENDIF
                UNTIL K%=13 OR K%=27
                IF K%=13 objlist{(obj_lstcur%)}.f%=VAL(F$)-1
                GCOL 0,0
                LINE SX%+160-OS%,SY%-228,SX%+192-OS%,SY%-228

              WHEN 5 : REM duplicate button
                REM remove duplicates for this object
                IF objlist{(obj_lstcur%)}.u%=1 THEN
                  objlist{(obj_lstcur%)}.u%=0
                  SP%=obj_lstcount%
                  FOR L%=SP% TO obj_lstcur%+1 STEP -1
                    IF objlist{(L%)}.parent%=obj_lstcur% PROCremoveobj(L%)
                  NEXT

                ELSE
                  REM add duplicates
                  IF objlist{(obj_lstcur%)}.rep%>0 THEN
                    objlist{(obj_lstcur%)}.u%=1
                    SP%=obj_lstcur%
                    FOR L%=1 TO objlist{(obj_lstcur%)}.rep%
                      IF objlist{(SP%)}.f%+objlist{(SP%)}.hop%*L%<10000 THEN
                        obj_lstcount%+=1
                        spriteselect%=-1
                        objlist{(obj_lstcount%)}.parent%=SP%
                        objlist{(obj_lstcount%)}.obj%=objlist{(SP%)}.obj%
                        objlist{(obj_lstcount%)}.type%=objlist{(SP%)}.type%
                        objlist{(obj_lstcount%)}.x%=objlist{(SP%)}.x%+objlist{(SP%)}.h%*L%
                        objlist{(obj_lstcount%)}.y%=objlist{(SP%)}.y%+objlist{(SP%)}.v%*L%
                        objlist{(obj_lstcount%)}.f%=objlist{(SP%)}.f%+objlist{(SP%)}.hop%*L%
                      ELSE
                        REM couldn't add sprite
                      ENDIF
                    NEXT
                  ELSE
                    REM VDU 7 ???
                  ENDIF
                ENDIF
              WHEN 6 : REM rep dec
                IF objlist{(obj_lstcur%)}.rep%>0 THEN objlist{(obj_lstcur%)}.rep%-=1

              WHEN 7 : REM rep inc
                IF objlist{(obj_lstcur%)}.rep%<100 THEN objlist{(obj_lstcur%)}.rep%+=1

              WHEN 8 : REM hop dec
                IF objlist{(obj_lstcur%)}.hop%>1 THEN objlist{(obj_lstcur%)}.hop%-=1

              WHEN 9 : REM hop inc
                IF objlist{(obj_lstcur%)}.hop%<100 THEN objlist{(obj_lstcur%)}.hop%+=1

              WHEN 10 : REM h dec
                IF objlist{(obj_lstcur%)}.h%>-100 THEN objlist{(obj_lstcur%)}.h%-=1

              WHEN 11 : REM h inc
                IF objlist{(obj_lstcur%)}.h%<100 THEN objlist{(obj_lstcur%)}.h%+=1

              WHEN 12 : REM v dev
                IF objlist{(obj_lstcur%)}.v%>-100 THEN objlist{(obj_lstcur%)}.v%-=1

              WHEN 13 : REM v inc
                IF objlist{(obj_lstcur%)}.v%<100 THEN objlist{(obj_lstcur%)}.v%+=1

              WHEN 14 : REM reset values
                objlist{(obj_lstcur%)}.rep%=0
                objlist{(obj_lstcur%)}.hop%=1
                objlist{(obj_lstcur%)}.h%=0
                objlist{(obj_lstcur%)}.v%=0


            ENDCASE
            IF (C%>-1 AND C%<2) OR C%>3 THEN PROCsubupdate(-1)

          WHEN 7 : REM frm properties

          WHEN 8 : REM movie mode menu
            CASE C% OF
              WHEN 0 : REM load
                done%=1
              WHEN 1 : REM save
                done%=1
              WHEN 2 : REM reset obj

              WHEN 3 : REM reset frm

              WHEN 4 : REM reset all

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

              WHEN 12: REM help screen
                PROCmenudraw
                PROCshowhelp
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

          WHEN 5 : REM object select sub menu
            CASE C% OF
              WHEN 2 : REM select sprite to drag or frame to insert
                IF menumode%=M_moviemode% THEN
                  menuext%=M_moviemode%
                ELSE
                  menuext%=M_canvas%
                  menufrom%=M_canvas%
                ENDIF
                PROCmenurestore
                IF spritemoving%>-1 PROCspritemoveinit

              WHEN 3 : REM scroll bar

              OTHERWISE
                IF menufrom%=M_moviemode% THEN
                  menuext%=M_moviemode%
                  menufrom%=M_moviemode%
                ELSE
                  menuext%=M_canvas%
                  menufrom%=M_canvas%
                ENDIF
                PROCmenurestore

            ENDCASE

          WHEN 6 : REM object property
            CASE C% OF
              WHEN 0 : REM

              WHEN 2 : REM move sprite
                menuext%=M_moviemode%
                PROCmenurestore
                IF spritemoving%>-1 PROCspritemoveinit

              OTHERWISE
                menuext%=M_moviemode%
                PROCmenurestore

            ENDCASE

          WHEN 7 : REM frm properties

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

    1           PROCmenurestore

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
          objlist{(L%)}.rep%=objlist{(L%+1)}.rep%
          objlist{(L%)}.hop%=objlist{(L%+1)}.hop%
          objlist{(L%)}.h%=objlist{(L%+1)}.h%
          objlist{(L%)}.v%=objlist{(L%+1)}.v%
        NEXT
      ENDIF
      obj_lstcount%-=1

      ENDPROC

      REM ##########################################################
      REM display moving sprite
      DEF PROCspritedraw(x%,y%,m%,c%)

      GCOL m%,c%
      PROCspritelarge(x%*32-320,(25-y%)*40-320,spritemoving%)

      ENDPROC


      REM ##########################################################
      REM display initial moving sprite
      DEF PROCspritemoveinit

      REM save current mode 7 screen as underlay for moving sprites
      OSCLI "SCREENSAVE """+@tmp$+"M7_TMP.BMP"" 0,0,1280,1000"
      PROCchangemode(6,0)

      REM restore previous display
      OSCLI "DISPLAY """+@tmp$+"M7_TMP.BMP"" 0,0"

      REM overlay existing sprite if relocating
      IF spriterelocate%=1 THEN
        PROCspritedraw(objlist{(obj_lstcur%)}.x%-mmWX%+10,mmWY%-objlist{(obj_lstcur%)}.y%+8,0,8)
      ENDIF

      REM Draw initial xor dragged sprite if mouse has moved while holding button
      PROCspritedraw(TX%,TY%,3,13)


      ENDPROC

      REM ##########################################################
      REM update moving sprite at new location
      DEF PROCspritemove

      REM erase old sprite and redraw in new mouse pos
      PROCspritedraw(OLD_TX%,OLD_TY%,3,13)
      PROCspritedraw(TX%,TY%,3,13)

      ENDPROC

      REM ##########################################################
      REM draw sprite to mode 6 screen
      DEF PROCspritelarge(x%,y%,sp%)
      LOCAL C%,X%,Y%,YC%,XC%,YM%
      YC%=y%
      FOR Y%=0 TO 47
        YM%=((Y%+3) MOD 3=0)*2

        REM PRINTTAB(0,20);"YM; ";STR$(YM%);"  ";"YC; ";STR$(YC%);"   ";
        REM A=GET

        FOR X%=0 TO 39
          XC%=x%+X%*16
          IF XC%>=0 AND YC%>=0 AND XC%<1280 AND YC%<1000 THEN
            C%=FNpoint_sprbuf(X%,47-Y%,sp%)
            IF C%>0 THEN RECTANGLE FILL XC%,YC%,14,12+YM%
          ENDIF
        NEXT
        YC%=YC%+14+YM%
      NEXT

      ENDPROC

      REM ##########################################################
      REM copy sprite buffer to frame
      DEF PROCobjtoworldmap
      LOCAL L%,X%,Y%,WX%,WY%
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
                  IF X%>mmWX%-21 AND X%<mmWX%+40 AND Y%<mmWY%+15 AND Y%>mmWY%-25 THEN
                    PROCspritetomovbuf(objlist{(L%)}.obj%,X%-mmWX%,mmWY%-Y%)
                  ENDIF

                WHEN 2 : REM frame
                  IF X%>mmWX%-41 AND X%<mmWX%+40 AND Y%<mmWY%+25 AND Y%>mmWY%-25 THEN
                    PROCbuffertoframe(objlist{(L%)}.obj%,X%-mmWX%,mmWY%-Y%)
                  ENDIF
                WHEN 3 : REM text / font

              ENDCASE
            ENDIF
          ENDIF
        NEXT
      ENDIF

      PROCframerestore(1)
      PROCmenudraw

      IF movieframe%>-1 THEN
        X%=frmlist{(movieframe%)}.x%
        Y%=frmlist{(movieframe%)}.y%

        X%=X%-mmWX%
        Y%=mmWY%-Y%

        REM SYS "SDL_SetRenderDrawColor", @memhdc%, 255, 255, 32, 0
        REM SYS "SDL_RenderDrawLine", @memhdc%, X%*16,Y%*20, X%*16+639,Y%*20
        REM SYS "SDL_RenderDrawLine", @memhdc%, X%*16+639,Y%*20,X%*16+639,Y%*20+499
        REM SYS "SDL_RenderDrawLine", @memhdc%, X%*16+639,Y%*20+499, X%*16,Y%*20+499
        REM SYS "SDL_RenderDrawLine", @memhdc%, X%*16,Y%*20+499,X%*16,Y%*20

      ENDIF

      IF spriteselect%>-1 THEN
        X%=objlist{(spriteselect%)}.x%
        Y%=objlist{(spriteselect%)}.y%

        X%=X%-mmWX%+10
        Y%=mmWY%-Y%+8

        IF OLD_SX%<>X% OR OLD_SY%<>Y% THEN
          SYS "SDL_SetRenderDrawColor", @memhdc%, 0, 0, 0, 0
          SYS "SDL_RenderDrawLine", @memhdc%, OLD_SX%*16-160, OLD_SY%*20-160, OLD_SX%*16+160, OLD_SY%*20-160
          SYS "SDL_RenderDrawLine", @memhdc%, OLD_SX%*16-160, OLD_SY%*20+160,OLD_SX%*16+160, OLD_SY%*20+160
          SYS "SDL_RenderDrawLine", @memhdc%, OLD_SX%*16-160, OLD_SY%*20-160, OLD_SX%*16-160, OLD_SY%*20+160
          SYS "SDL_RenderDrawLine", @memhdc%, OLD_SX%*16+160, OLD_SY%*20-160,OLD_SX%*16+160, OLD_SY%*20+160
        ENDIF

        OLD_SX%=X%
        OLD_SY%=Y%

        REM PROCmenurestore

        REM PRINTTAB(0,21);STR$(spriteselect%);
        REM PRINTTAB(0,22);STR$(X%*16);
        REM PRINTTAB(0,23);STR$(Y%*20);

        SYS "SDL_SetRenderDrawColor", @memhdc%, 32, 255, 32, 0
        SYS "SDL_RenderDrawLine", @memhdc%, X%*16-160, Y%*20-160, X%*16+160, Y%*20-160
        SYS "SDL_RenderDrawLine", @memhdc%, X%*16-160, Y%*20+160,X%*16+160, Y%*20+160
        SYS "SDL_RenderDrawLine", @memhdc%, X%*16-160, Y%*20-160, X%*16-160, Y%*20+160
        SYS "SDL_RenderDrawLine", @memhdc%, X%*16+160, Y%*20-160,X%*16+160, Y%*20+160
      ENDIF

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

          PROCgtext(" CLOSE ",768,998,11,9)

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

      IF S% THEN
        PROCWAITMOUSE(0)
        PROCframesave(frame%)
      ENDIF
      frame%+=F%
      IF frame%>frame_max% THEN frame%=1
      IF frame%<1 THEN frame%=frame_max%
      PROCframerestore(frame%)

      PROCmenudraw

      IF spritemoving%>-1 THEN
        PROCspritedraw(TX%,TY%,3,13)
        PROCmenurestore
        PROCspritemoveinit
      ENDIF

      REM reset text cursor
      TEXTX%=TX%
      FONTX%=PX%

      ENDPROC

      REM ##########################################################
      REM copy sprite buffer to frame
      DEF PROCbuffertoframe(f%,sx%,sy%)
      LOCAL S%,U%,X%,Y%,C%,CS%
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
            IF S%<>32 AND S%<>160 THEN movie_buffer&(X%+(Y%-1)*40)=S%
          ELSE
            movie_buffer&(X%+(Y%-1)*40)=S%
          ENDIF
        ENDIF
      NEXT
      ENDPROC

      REM ##########################################################
      REM copy sprite buffer to movie frame
      DEF PROCspritetomovbuf(s%,sx%,sy%)
      LOCAL S%,U%,X%,Y%,LC%
      LC%=0
      FOR U%=0 TO 319
        X%=sx%+U% MOD 20
        Y%=sy%+U% DIV 20
        S%=sprite_buffer&(s%,U%)
        IF S%>144 AND S%<151 THEN
          LC%=S%
          IF Y%>0 AND X%<1 movie_buffer&((Y%-1)*40)=S%
        ENDIF
        IF X%>0 AND X%<40 AND Y%>0 AND Y%<25 THEN

          IF spr_trns%=1 THEN
            IF S%<>32 AND S%<>160 THEN movie_buffer&(X%+(Y%-1)*40)=S%
          ELSE
            movie_buffer&(X%+(Y%-1)*40)=S%
          ENDIF
        ENDIF
      NEXT

      REM ##########################################################
      REM paste copypaste buffer to current frame
      DEF PROCspritetocanvas(s%,sx%,sy%)
      LOCAL C%,X%,Y%,U%

      FOR U%=0 TO 319
        X%=sx%+U% MOD 20
        Y%=sy%+U% DIV 20

        IF X%<40 AND X%>0 AND Y%<25 AND Y%>0 THEN
          C%=sprite_buffer&(s%,U%)
          IF spr_trns%=0 THEN
            VDU 31,X%,Y%,C%
          ELSE
            IF C%<>32 AND C%<>160 THEN VDU 31,X%,Y%,C%
          ENDIF
        ENDIF
      NEXT
      ENDPROC


      ENDPROC
      REM ##########################################################
      REM save sprite
      DEF PROCsavesprite(S%)
      LOCAL U%
      REM 20x16 chars @320 bytes : 40x48 pixels @1920 bytes

      FOR U%=0 TO 319
        sprite_buffer&(S%,U%)=GET(U% MOD 20+10,U% DIV 20+3)
      NEXT

      PROCdrawspritegrid

      ENDPROC

      REM ##########################################################
      REM draw sprite
      DEF PROCdrawsprite
      LOCAL U%
      REM 20x16 chars @320 bytes : 40x48 pixels @1920 bytes

      FOR U%=0 TO 319
        VDU 31,U% MOD 20+10,U% DIV 20+3,sprite_buffer&(sprite_cur%,U%)
      NEXT

      PROCdrawspritegrid

      ENDPROC

      REM ##########################################################
      REM save current screen / sprite to undo buffer
      DEF PROCundosave
      LOCAL U%
      CASE menuext% OF
        WHEN M_canvas% : REM main canvas
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
        WHEN M_canvas% : REM main canvas
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
            PROCsavesprite(sprite_cur%)
          ENDIF

      ENDCASE

      ENDPROC

      REM ##########################################################
      REM save redo screen / sprite
      DEF PROCredosave
      LOCAL U%

      CASE menuext% OF
        WHEN M_canvas% : REM main canvas
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
        WHEN M_canvas% : REM main canvas
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
            PROCsavesprite(sprite_cur%)

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
      menuext%=78

      PROCcontrolcodes
      PROCdrawgrid

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

      C$="S:"+RIGHT$("0"+STR$(SX%),2)+","+RIGHT$("0"+STR$(SY%),2)+" P:"+RIGHT$("0"+STR$(TX%),2)+","+RIGHT$("0"+STR$(TY%),2)
      PROCgtext(C$,0,996,14,4)


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

          C$="S:"+RIGHT$("0"+STR$(SX%),2)+","+RIGHT$("0"+STR$(SY%),2)+" E:"+RIGHT$("0"+STR$(TX%),2)+","+RIGHT$("0"+STR$(TY%),2)
          PROCgtext(C$,0,996,14,4)

        ELSE
          WAIT 2
        ENDIF
      UNTIL MB%=4

      OX%=TX%
      OY%=TY%

      REPEAT
        PROCREADMOUSE
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

          C$="S:"+RIGHT$("0"+STR$(SX%),2)+","+RIGHT$("0"+STR$(SY%),2)+" E:"+RIGHT$("0"+STR$(TX%),2)+","+RIGHT$("0"+STR$(TY%),2)
          PROCgtext(C$,0,996,14,4)

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
      menuext%=M_canvas%
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
      LOCAL f%,u%,c%

      IF S%=1 THEN
        f%=OPENOUT(F$+".SPR")
        FOR c%=0 TO sprite_max%-1
          FOR u%=0 TO 319
            BPUT#f%,sprite_buffer&(c%,u%)
          NEXT
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
              A$+=STR$(FNpoint_sprbuf(X%+2,Y%,c%))
              IF X%<12 THEN A$+=","
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
      LOCAL f%,u%,char%,c%
      f%=OPENIN(F$)

      IF f% THEN
        c%=0
        REPEAT
          FOR u%=0 TO 319
            char%=BGET#f%
            sprite_buffer&(c%,u%)=char%
          NEXT
          c%+=1
        UNTIL EOF#f% OR c%>=sprite_max%
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
          FOR l%=0 TO movieframetotal%
            INPUT#f%,line$
            c% = FN_split(line$, ",", a$())
            IF c%=4 THEN
              frmlist{(l%)}.x%=VAL(a$(0))
              frmlist{(l%)}.y%=VAL(a$(1))
              frmlist{(l%)}.b%=VAL(a$(0))
              frmlist{(l%)}.f%=VAL(a$(1))
            ENDIF
          NEXT
          INPUT#f%,line$
          IF line$="*OBJECT DATA*" THEN
            INPUT#f%,line$
            obj_lstcount%=VAL(line$)
            FOR l%=0 TO obj_lstcount%
              INPUT#f%,line$
              c% = FN_split(line$, ",", a$())
              IF c%=13 THEN
                objlist{(l%)}.obj%=VAL(a$(0))
                objlist{(l%)}.type%=VAL(a$(1))
                objlist{(l%)}.f%=VAL(a$(2))
                objlist{(l%)}.rep%=VAL(a$(3))
                objlist{(l%)}.hop%=VAL(a$(4))
                objlist{(l%)}.parent%=VAL(a$(5))
                objlist{(l%)}.x%=VAL(a$(6))
                objlist{(l%)}.y%=VAL(a$(7))
                objlist{(l%)}.h%=VAL(a$(8))
                objlist{(l%)}.v%=VAL(a$(9))
                objlist{(l%)}.m%=VAL(a$(10))
                objlist{(l%)}.d%=VAL(a$(11))
                objlist{(l%)}.u%=VAL(a$(12))
              ENDIF
            NEXT
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
      REM load font names
      DEF PROCloadfontnames
      LOCAL N%,I%,C%
      LOCAL n$,t&

      DIM n$(10000)
      DIM t&(10000)
      fontcount%=0

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
        OSCLI "CD """+@dir$+""""
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
                menuext%=M_canvas%

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

        WHEN 2 : REM import sprite from bmp
          PROCchangemode(6,1)

          IF F%=-1 THEN
            COLOUR 9
            PRINTTAB(0,0)"NO FILE LOADED"
            menuext%=94
          ELSE

            REM OSCLI "DISPLAY """+curdir$+n$(SEL%)+""" 0,0"


            OSCLI "LOAD """+curdir$+n$(SEL%)+""" "+STR$~import_buffer%%+" +"+STR$~1000000

            REM PRINTTAB(0,0)"LOAD """;curdir$+n$(SEL%);""" ";STR$~import_buffer%%;" +";STR$1000000
            REM bmp filetype (2 bytes)
            REM PRINT"Type:";CHR$(import_buffer%%?0);CHR$(import_buffer%%?1)
            T$=CHR$(import_buffer%%?0)+CHR$(import_buffer%%?1)
            REM bmp filesize (4 bytes) ?2 ?3 ?4 ?5
            REM bmp reserved (2 bytes) ?6 ?7
            REM bmp reserved (2 bytes) ?8 ?9
            REM bmp pixel data offset (4 bytes)
            REM PRINT"pOfs:";STR$(import_buffer%%!10)
            bmp_imgofs%=import_buffer%%!10

            REM bmp header size (4 bytes)
            REM PRINT"hSze:";STR$(import_buffer%%!14)

            REM bmp image width (4 bytes)
            REM PRINT"iWid:";STR$(import_buffer%%!18)
            bmp_imgwid%=import_buffer%%!18

            REM bmp image height (4 bytes)
            REM PRINT"iHgt:";STR$(import_buffer%%!22)
            bmp_imghgt%=import_buffer%%!22
            REM bmp planes (2 bytes) ?26 ?27
            REM bmp but per pixel (2 bytes)
            REM PRINT"bpp: ";STR$(import_buffer%%?29);STR$(import_buffer%%?28)
            bmp_imgbpp%=import_buffer%%?28+(import_buffer%%?29*256)

            REM bmp compression (4 bytes) ?30 ?31 ?32 ?33
            REM bmp image size (4 bytes) ?34 ?35 ?36 ?37
            REM bmp x pixels per meter (4 bytes) ?38 ?39 ?40 ?41
            REM bmp y pixels per meter (4 bytes) ?42 ?43 ?44 ?45
            REM bmp total colours (4 bytes) ?46 ?47 ?48 ?49
            REM bmp important colours (4 bytes) ?50 ?51 ?52 ?53
            IF T$<>"BM" OR bmp_imgofs%<>54 OR bmp_imgbpp%<>24 THEN
              PRINTTAB(0,0)"Image format not supported, must be BMP 24bpp"
              PROCWAITMOUSE(4)
              PROCWAITMOUSE(0)
              menuext%=94
            ELSE
              OSCLI "MDISPLAY "+STR$~import_buffer%%
              menuext%=95
            ENDIF


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

            OSCLI "LOAD """+curdir$+n$(SEL%)+""" "+STR$~import_buffer%%+" +"+STR$~1000000

            T$=CHR$(import_buffer%%?0)+CHR$(import_buffer%%?1)
            bmp_imgofs%=import_buffer%%!10
            bmp_imgwid%=import_buffer%%!18
            bmp_imghgt%=import_buffer%%!22
            bmp_imgbpp%=import_buffer%%?28+(import_buffer%%?29*256)

            IF T$<>"BM" OR bmp_imgofs%<>54 OR bmp_imgbpp%<>24 THEN
              PRINTTAB(0,0)"Image format not supported, must be BMP 24bpp"
              PROCWAITMOUSE(4)
              PROCWAITMOUSE(0)
            ELSE
              OSCLI "MDISPLAY "+STR$~import_buffer%%
              menuext%=95
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
      LOCAL D$,done%,OG%,L%,C%

      PROCWAITMOUSE(0)

      PROCframesave(frame%)

      REM turn off grid and save state
      OG%=gridshow%
      gridshow%=0
      C%=-1

      PROCchangemode(6,0)

      D$=FNgetdate

      IF session%=0 THEN cursavedir$= "M7_"+LEFT$(D$,LEN(D$)-2)

      PROCsaveupdate(1,LEFT$(cursavedir$+"                        ",24))

      REPEAT
        PROCREADMOUSE

        IF MB%=4 THEN

          PROCWAITMOUSE(0)
          IF TY%=0 THEN done%=2

          FOR L%=0 TO controls%
            IF controlrange{(L%)}.x1%>-1 THEN
              IF MX%>controlrange{(L%)}.x1% AND MX%<controlrange{(L%)}.x2% AND MY%>controlrange{(L%)}.y1% AND MY%<controlrange{(L%)}.y2% THEN
                C%=L%
                EXIT FOR
              ENDIF
            ELSE
              EXIT FOR
            ENDIF
          NEXT

          CASE C% OF
            WHEN 2 : REM bin
              save_bin%=(save_bin%+1) AND 1
              PROCsaveupdate(0,"BIN")

            WHEN 3 : REM bmp
              save_bmp%=(save_bmp%+1) AND 1
              PROCsaveupdate(0,"BMP")

            WHEN 4 : REM spr
              save_spr%=(save_spr%+1) AND 1
              PROCsaveupdate(0,"SPR")

            WHEN 5 : REM dat
              save_dat%=(save_dat%+1) AND 1
              PROCsaveupdate(0,"DAT")

            WHEN 0 : REM save
              done%=1

            WHEN 1 : REM cancel
              done%=2

          ENDCASE
          IF save_bin%+save_bmp%+save_spr%+save_dat%=0 THEN
            save_bin%=1
            PROCsaveupdate(0,"BIN")
          ENDIF

        ELSE
          WAIT 2
        ENDIF

      UNTIL done%>0

      PROCchangemode(7,1)

      IF done%=1 THEN
        REM create and change to session folder, strip off seconds value
        IF session%=0 THEN
          OSCLI "MD """+cursave$+cursavedir$+""""
          OSCLI "CD """+cursave$+cursavedir$+""""
          session%=1
          cursave$=cursave$+cursavedir$+"/"
        ENDIF

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
      REM update save screen options
      DEF PROCsaveupdate(M%,D$)
      LOCAL SX%,SY%,SW%,SH%,TY%

      SX%=100: SY%=100
      SW%=1078 : SH%=800
      TY%=SY%+SH%

      IF M%=1 THEN
        PROCresetcontrols

        GCOL 0,0
        RECTANGLE FILL SX%,SY%,SW%,SH%
        GCOL 0,15
        RECTANGLE SX%+8,SY%+8,SW%-16,SH%-16
        RECTANGLE SX%+10,SY%+10,SW%-20,SH%-20
        REM t$,x%,y%,tc%,bc%
        PROCgtext(" SAVE FILE OPTIONS ",SX%+236,TY%-40,10,0)
        PROCgtext("PROJECT NAME: ",SX%+40,TY%-100,15,0)
        PROCgtext("FILES TO SAVE:",SX%+40,TY%-208,15,0)
        PROCgtext("BIN:",SX%+40,TY%-256,15,0)
        PROCgtext("(BIN FORMAT 1 KB)",SX%+340,TY%-256,4,0)
        PROCgtext("BMP:",SX%+40,TY%-304,15,0)
        PROCgtext("(BMP FORMAT 1 MB)",SX%+340,TY%-304,4,0)
        PROCgtext("SPR:",SX%+40,TY%-352,15,0)
        PROCgtext("(ALL SPRITE DATA)",SX%+340,TY%-352,4,0)
        PROCgtext("TXT:",SX%+40,TY%-400,15,0)
        PROCgtext("(DATA STATEMENTS)",SX%+340,TY%-400,4,0)
        PROCmenucontrol(0,"  SAVE  ",SX%+100,SY%+100,12,10,4)
        PROCmenucontrol(1," CANCEL ",SX%+700,SY%+100,9,11,1)

        PROCgtext(D$,SX%+40,TY%-144,11,4)
      ENDIF

      PROCmenucontrol(2," "+CHR$(78+save_bin%*11)+" ",SX%+180,TY%-256,8,11+4*save_bin%,1+save_bin%)
      PROCmenucontrol(3," "+CHR$(78+save_bmp%*11)+" ",SX%+180,TY%-304,8,11+4*save_bmp%,1+save_bmp%)
      PROCmenucontrol(4," "+CHR$(78+save_spr%*11)+" ",SX%+180,TY%-352,8,11+4*save_spr%,1+save_spr%)
      PROCmenucontrol(5," "+CHR$(78+save_dat%*11)+" ",SX%+180,TY%-400,8,11+4*save_dat%,1+save_dat%)

      ENDPROC

      REM ##########################################################
      REM save all frames to file, create session folder if not already exists
      DEF PROCsavemovie
      LOCAL D$,N$,done%,OG%,L%,C%,I%

      PROCWAITMOUSE(0)

      REM PROCframesave(frame%)

      REM turn off grid and save state
      OG%=gridshow%
      gridshow%=0
      C%=-1

      PROCchangemode(6,0)

      D$=FNgetdate

      IF session%=0 THEN cursavedir$= "M7_"+LEFT$(D$,LEN(D$)-2)

      PROCsavemovieupdate(1,LEFT$(cursavedir$+"                        ",24))

      REPEAT
        PROCREADMOUSE

        IF MB%=4 THEN

          PROCWAITMOUSE(0)
          IF TY%=0 THEN done%=2

          FOR L%=0 TO controls%
            IF controlrange{(L%)}.x1%>-1 THEN
              IF MX%>controlrange{(L%)}.x1% AND MX%<controlrange{(L%)}.x2% AND MY%>controlrange{(L%)}.y1% AND MY%<controlrange{(L%)}.y2% THEN
                C%=L%
                EXIT FOR
              ENDIF
            ELSE
              EXIT FOR
            ENDIF
          NEXT

          CASE C% OF
            WHEN 2 : REM movie frame data .MOV
              mov_frm%=1-mov_frm%
              PROCsavemovieupdate(0,"")

            WHEN 3 : REM movie bin data .BIN
              mov_dat%=1-mov_dat%
              PROCsavemovieupdate(0,"")

            WHEN 4 : REM frame bin data .BIN
              mov_bin%=1-mov_bin%
              PROCsavemovieupdate(0,"")

            WHEN 5 : REM movie frames .BMP
              mov_bmp%=1-mov_bmp%
              PROCsavemovieupdate(0,"")

            WHEN 6 : REM sprite data .SPR
              mov_spr%=1-mov_spr%
              PROCsavemovieupdate(0,"")

            WHEN 0 : REM save
              done%=1

            WHEN 1 : REM cancel
              done%=2

          ENDCASE
          IF mov_frm%+mov_bin%+mov_bmp%+mov_spr%+mov_dat%=0 THEN
            mov_frm%=1
            PROCsavemovieupdate(0,"")
          ENDIF

        ELSE
          WAIT 2
        ENDIF

      UNTIL done%>0

      PROCchangemode(7,1)

      IF done%=1 THEN
        REM create and change to session folder, strip off seconds value
        IF session%=0 THEN
          OSCLI "MD """+cursave$+cursavedir$+""""
          OSCLI "MD """+cursave$+cursavedir$+"\MOVIE_BMP"+""""
          OSCLI "CD """+cursave$+cursavedir$+""""
          session%=1
          cursave$=cursave$+cursavedir$+"/"
        ENDIF

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
          FOR I%=0 TO movieframetotal%
            PRINT#f%,STR$(frmlist{(I%)}.x%)+","+STR$(frmlist{(I%)}.y%)+","+STR$(frmlist{(I%)}.b%)+","+STR$(frmlist{(I%)}.f%)
          NEXT

          PRINT#f%,"*OBJECT DATA*"
          PRINT#f%,STR$(obj_lstcount%)
          FOR I%=0 TO obj_lstcount%
            N$=STR$(objlist{(I%)}.obj%)+","
            N$+=STR$(objlist{(I%)}.type%)+","
            N$+=STR$(objlist{(I%)}.f%)+","
            N$+=STR$(objlist{(I%)}.rep%)+","
            N$+=STR$(objlist{(I%)}.hop%)+","
            N$+=STR$(objlist{(I%)}.parent%)+","
            N$+=STR$(objlist{(I%)}.x%)+","
            N$+=STR$(objlist{(I%)}.y%)+","
            N$+=STR$(objlist{(I%)}.h%)+","
            N$+=STR$(objlist{(I%)}.v%)+","
            N$+=STR$(objlist{(I%)}.m%)+","
            N$+=STR$(objlist{(I%)}.d%)+","
            N$+=STR$(objlist{(I%)}.u%)
            PRINT#f%,N$
          NEXT
          CLOSE#f%

        ENDIF

        REM movie bin data and bmp export
        IF mov_dat%=1 OR mov_bmp%=1 THEN
          CLS

          FOR I%=0 TO movieframetotal%
            movieframe%=I%
            mmWX%=frmlist{(I%)}.x%
            mmWY%=frmlist{(I%)}.y%
            PROCobjtoworldmap
            N$=RIGHT$("0000"+STR$(I%),5)
            IF mov_dat%=1 PROCsavebinaryfile(cursave$+"M7_" + D$ + "_MOV_" + N$ +".BIN")
            IF mov_bmp%=1 OSCLI "SCREENSAVE """+cursave$+"\MOVIE_BMP\M7_" + D$ + "_" + N$ +".BMP"" 0,0,1280,1000"
            REM SYS "SDL_SavePNG", @memhdc%, "C:\DATA\Retro\BeebEm\BB4WProjects\test.PNG"
            WAIT 10
          NEXT

        ENDIF

        REM save frame bin data
        IF mov_bin%=1 THEN
          menuext%=M_canvas%
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
      DEF PROCsavemovieupdate(M%,D$)
      LOCAL SX%,SY%,SW%,SH%,TY%

      SX%=100: SY%=100
      SW%=1078 : SH%=800
      TY%=SY%+SH%

      IF M%=1 THEN
        PROCresetcontrols

        GCOL 0,0
        RECTANGLE FILL SX%,SY%,SW%,SH%
        GCOL 0,15
        RECTANGLE SX%+8,SY%+8,SW%-16,SH%-16
        RECTANGLE SX%+10,SY%+10,SW%-20,SH%-20
        REM t$,x%,y%,tc%,bc%
        PROCgtext(" SAVE MOVIE OPTIONS ",SX%+236,TY%-40,10,0)
        PROCgtext("MOVIE NAME: ",SX%+40,TY%-100,15,0)
        PROCgtext("FILES TO SAVE:",SX%+40,TY%-208,15,0)
        PROCgtext("MOV:",SX%+40,TY%-256,15,0)
        PROCgtext("(MOVIE FRAME DATA)",SX%+340,TY%-256,4,0)
        PROCgtext("BIN:",SX%+40,TY%-304,15,0)
        PROCgtext("(MOVIE BIN DATA 1 KB)",SX%+340,TY%-304,4,0)
        PROCgtext("BIN:",SX%+40,TY%-352,15,0)
        PROCgtext("(FRAME BIN DATA 1 KB)",SX%+340,TY%-352,4,0)
        PROCgtext("BMP:",SX%+40,TY%-400,15,0)
        PROCgtext("(MOVIE BMP FILES 1 MB)",SX%+340,TY%-400,4,0)
        PROCgtext("SPR:",SX%+40,TY%-448,15,0)
        PROCgtext("(SPRITE DATA FILES)",SX%+340,TY%-448,4,0)

        PROCmenucontrol(0,"  SAVE  ",SX%+100,SY%+100,12,10,4)
        PROCmenucontrol(1," CANCEL ",SX%+700,SY%+100,9,11,1)

        PROCgtext(D$,SX%+40,TY%-144,11,4)
      ENDIF

      PROCmenucontrol(2," "+CHR$(78+mov_frm%*11)+" ",SX%+180,TY%-256,8,11+4*mov_frm%,1+mov_frm%)
      PROCmenucontrol(3," "+CHR$(78+mov_dat%*11)+" ",SX%+180,TY%-304,8,11+4*mov_dat%,1+mov_dat%)
      PROCmenucontrol(4," "+CHR$(78+mov_bin%*11)+" ",SX%+180,TY%-352,8,11+4*mov_bin%,1+mov_bin%)
      PROCmenucontrol(5," "+CHR$(78+mov_bmp%*11)+" ",SX%+180,TY%-400,8,11+4*mov_bmp%,1+mov_bmp%)
      PROCmenucontrol(6," "+CHR$(78+mov_spr%*11)+" ",SX%+180,TY%-448,8,11+4*mov_spr%,1+mov_spr%)


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

                REMPRINTTAB(0,3)"xs: ";RIGHT$("000"+STR$(startx%),4);" xe: ";RIGHT$("000"+STR$(gridsx%),4)
                REMPRINTTAB(0,4)"ys: ";RIGHT$("000"+STR$(starty%),4);" ye: ";RIGHT$("000"+STR$(gridsy%),4)


              UNTIL MB%=0
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
                    PROCprint40(0,"Select Frame: "+RIGHT$("0"+STR$(frame%),2))
                  ELSE
                    done%=1
                  ENDIF

                ELSE
                  done%=1

                ENDIF
              ENDIF

            ELSE
              WAIT 2
            ENDIF

          UNTIL done%=1

      ENDCASE
      PROCWAITMOUSE(0)
      PROCchangemode(7,1)
      frame%=0
      PROCloadnextframe(1,0)
      menuext%=M_canvas%

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

          OSCLI "LOAD """+curdir$+NAME$+""" "+STR$~import_buffer%%+" +"+STR$1000000

          T$=CHR$(import_buffer%%?0)+CHR$(import_buffer%%?1)
          bmp_imgofs%=import_buffer%%!10
          bmp_imgwid%=import_buffer%%!18
          bmp_imghgt%=import_buffer%%!22
          bmp_imgbpp%=import_buffer%%?28+(import_buffer%%?29*256)

          REM adjust for correct line byte width multiple of 4
          line_wid%=bmp_imgwid%*3
          WHILE line_wid% MOD 4<>0
            line_wid%+=1
          ENDWHILE

          REM        PRINTTAB(0,0)NAME$;"  F:";STR$(F%);"  ";
          REM        PRINTT$
          REM        PRINT"pOfs:";STR$(bmp_imgofs%)
          REM        PRINT"pWid:";STR$(bmp_imgwid%)
          REM        PRINT"pHgt:";STR$(bmp_imghgt%)
          REM        PRINT"pBpp:";STR$(bmp_imgbpp%)
          REM        PROCWAITMOUSE(4)

          IF T$="BM" AND bmp_imgofs%=54 AND bmp_imgbpp%=24 THEN
            FOR X%=0 TO 77
              FOR Y%=0 TO 71
                col%=0
                IF X%>-1 AND X%<bmp_imgwid% AND Y%>-1 AND Y%<bmp_imghgt% THEN
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

          ELSE
            PRINTTAB(0,0)"INCORRECT IMAGE FORMAT"
            PRINT"BMP MUST BE 24bpp"
            PRINT""
            PRINT"CLICK MOUSE TO CONTINUE."
            PROCWAITMOUSE(4)
            PROCWAITMOUSE(0)
            EXIT FOR
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
      DEF PROCimportsprite

      LOCAL GX%,GY%

      menuext%=96
      done%=0

      PROCloadfile(2)

      REM after loadfile menuext% returns: No file: 94, single image: 95, grid: 96

      PROCWAITMOUSE(0)

      IF menuext%=94 THEN
        GX%=0
        REPEAT
          PROCREADMOUSE
          WAIT 5
          GX%+=1
        UNTIL GX%>100 OR MB%<>0
      ELSE

        GX%=1
        GY%=1

        startx%=-1
        starty%=-1
        gridsx%=78
        gridsy%=94

        REM adjust for correct byte width multiple of 4
        line_wid%=bmp_imgwid%*3
        WHILE line_wid% MOD 4<>0
          line_wid%+=1
        ENDWHILE


        REPEAT

          PROCprint40(0,"Select Sprite: "+RIGHT$("0"+STR$(sprite_cur%+1),2))
          GCOL 0,2
          REM RECTANGLE FILL 788,558,494,440
          RECTANGLE FILL 788,414,638,584

          MX%=(MX% DIV 2)*2
          MY%=(MY% DIV 2)*2
          OLDMX%=MX%
          OLDMY%=MY%


          GCOL 3,15
          FOR X%=0 TO GX%
            LINE MX%+X%*gridsx%,MY%,MX%+X%*gridsx%,MY%+gridsy%*GY%
          NEXT
          FOR Y%=0 TO GY%
            LINE MX%,MY%+Y%*gridsy%,MX%+gridsx%*GX%,MY%+Y%*gridsy%
          NEXT

          REPEAT
            PROCREADMOUSE

            MX%=(MX% DIV 2)*2
            MY%=(MY% DIV 2)*2
            REM start a new selection

            IF OLDMX%<>MX% OR OLDMY%<>MY% THEN
              GCOL 3,15
              FOR X%=0 TO GX%
                LINE OLDMX%+X%*gridsx%,OLDMY%,OLDMX%+X%*gridsx%,OLDMY%+gridsy%*GY%
              NEXT
              FOR Y%=0 TO GY%
                LINE OLDMX%,OLDMY%+Y%*gridsy%,OLDMX%+gridsx%*GX%,OLDMY%+Y%*gridsy%
              NEXT

              x%=MX% DIV 2
              y%=MY% DIV 2

              REM PRINTTAB(0,1)STR$(x%);"  ";STR$(y%);"   ";

              REM 48x12 576
              px%=0
              FOR X%=x% TO x%+39
                py%=564
                FOR Y%=y% TO y%+47

                  REM IF POINT(X%,Y%)<>0 THEN
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
                  RECTANGLE FILL 794+px%,982-py%,12

                  py%-=12
                NEXT
                px%+=12
              NEXT

              GCOL 3,15
              FOR X%=0 TO GX%
                LINE MX%+X%*gridsx%,MY%,MX%+X%*gridsx%,MY%+gridsy%*GY%
              NEXT
              FOR Y%=0 TO GY%
                LINE MX%,MY%+Y%*gridsy%,MX%+gridsx%*GX%,MY%+Y%*gridsy%
              NEXT
              OLDMX%=MX%
              OLDMY%=MY%

            ELSE
              WAIT 2

              REM check for cursor keys and update mouse
              IF INKEY(-26) AND MX%>0 THEN MX%-=2
              IF INKEY(-122) AND MX%<1278 THEN MX%+=2
              IF INKEY(-58) AND MY%<998 THEN MY%+=2
              IF INKEY(-42) AND MY%>0 THEN MY%-=2
              IF INKEY(-74) THEN MB%=4

              IF OLDMX%<>MX% OR OLDMY%<>MY% THEN MOUSE TO MX%,MY%

            ENDIF

          UNTIL MB%=4
          FOR X%=0 TO GX%
            LINE OLDMX%+X%*gridsx%,OLDMY%,OLDMX%+X%*gridsx%,OLDMY%+gridsy%*GY%
          NEXT
          FOR Y%=0 TO GY%
            LINE OLDMX%,OLDMY%+Y%*gridsy%,OLDMX%+gridsx%*GX%,OLDMY%+Y%*gridsy%
          NEXT

          REM process selection(s)
          x1%=MX%
          y1%=MY%
          x2%=MX%+78
          y2%=MY%+94

          IF x1%>x2% THEN SWAP x1%,x2%
          IF y1%>y2% THEN SWAP y1%,y2%

          PROCprint40(0,"Processing selection, Sprite: "+STR$(sprite_cur%+1))
          REM A=GET

          x%=0
          FOR X%=x1% TO x2% STEP 2
            y%=47
            FOR Y%=y1% TO y2% STEP 2
              REM PRINTTAB(0,1)STR$(x%);"  ";STR$(y%);"   ";
              IF POINT(X%,Y%)<>0 THEN
                PROCpoint_sprbuf(x%,y%,1,sprite_cur%)
              ELSE
                PROCpoint_sprbuf(x%,y%,0,sprite_cur%)
              ENDIF
              y%-=1
            NEXT
            x%+=1
          NEXT
          REM          PRINTTAB(0,1)STR$(x%);"  ";STR$(y%);"   ";
          REM          A=GET

          IF menuext%=95 THEN
            PROCprint40(0,"Complete! Process next sprite?  Y   N")
            GCOL 3,10
            RECTANGLE FILL 980,960,108,40

            GCOL 3,9
            RECTANGLE FILL 1108,960,108,40

            REM check if next sprite should be loaded< either click yes with mouse or press Y or ENTER
            NSPR%=0

            REPEAT
              PROCREADMOUSE

              WAIT 2

              IF INKEY(-69) OR INKEY(-74) THEN NSPR%=1
              IF INKEY(-86) THEN NSPR%=2

            UNTIL MB%=4 OR NSPR%>0

            PROCWAITMOUSE(0)

            IF (TY%=0 AND TX%>30 AND TX%<34) OR NSPR%=1 THEN

              REM reset selection
              startx%=-1
              IF sprite_cur%<sprite_max%-1 THEN
                sprite_cur%+=1
              ELSE
                done%=1
              ENDIF

            ELSE
              done%=1

            ENDIF
          ENDIF


        UNTIL done%=1

      ENDIF
      PROCWAITMOUSE(0)
      PROCchangemode(7,1)
      menuext%=M_sprites%


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
      VDU 5
      GCOL 0,11
      MOVE 8,980
      PRINT"Canvas Mode Hot Keys"
      MOVE 8,932
      PRINT"===================="
      GCOL 0,14
      MOVE 8,886
      PRINT"ESC+P  : Show Control Codes"
      MOVE 8,838
      PRINT"ESC+Ff : Flash/Steady"
      MOVE 8,790
      PRINT"ESC+Hh : Hold/Releas Graphics"
      MOVE 8,742
      PRINT"ESC+Dd : Double/Normal Height"
      MOVE 8,696
      PRINT"ESC+Ss : Separated/Normal Graphics"
      MOVE 8,648
      PRINT"Insert : Show Object Insert Menu"
      MOVE 8,600
      PRINT"End    : Insert Last Selected Object"
      MOVE 8,552
      PRINT"Escape : Cancel Insert Object"
      MOVE 8,506
      PRINT"Left   : Navigate To Previous Frame"
      MOVE 8,458
      PRINT"Right  : Navigate To Next Frame"
      MOVE 8,410
      PRINT"CTRL+C : Begin Region Selection"
      MOVE 8,362
      PRINT"CTRL+Left  : Copy Frame Left"
      MOVE 8,316
      PRINT"CTRL+Right : Copy Frame Right"
      MOVE 8,268
      PRINT"SHFT+Cursors : Pixel Shift Selection"

      PROCWAITMOUSE(4)
      PROCWAITMOUSE(0)

      REM canvas mode hotkeys
      CLS
      GCOL 0,11
      MOVE 8,980
      PRINT"Movie Mode Hot Keys"
      MOVE 8,932
      PRINT"===================="
      GCOL 0,14
      MOVE 8,886
      PRINT"ENTER : Add Frame At Current Coords"
      MOVE 8,838
      PRINT"TAB   : Select On Screen Object"
      MOVE 8,790
      PRINT"D     : Duplicate Selected Sprite"
      MOVE 8,742
      PRINT"P     : Show Selected Sprite Detail"
      MOVE 8,696
      PRINT"M     : Move Selected Sprite"
      MOVE 8,648
      PRINT"E     : Edit Selected Sprite"
      MOVE 8,600
      PRINT"Insert : Show Object Insert Menu"
      MOVE 8,552
      PRINT"End    : Insert Last Selected Object"
      MOVE 8,506
      PRINT"Escape : Cancel Insert Object"
      MOVE 8,458
      PRINT"PGUP PGDN : Next / Prec Frame"
      MOVE 8,410
      PRINT"HOME END  : First / Last Frame"
      MOVE 8,362
      PRINT"LMB       : Place Sprite In Frame"
      MOVE 8,316
      PRINT"SHIFT LMB : Add Sprite To Background"
      MOVE 8,268
      PRINT"Cursors      : Move World Map"
      MOVE 8,220
      PRINT"SHFT+Cursors : Move World Map 1 Page"


      VDU 4
      PROCWAITMOUSE(4)
      PROCWAITMOUSE(0)
      PROCchangemode(7,1)
      frame%-=1
      PROCloadnextframe(1,0)
      VDU 23,1,1;0;0;0; : REM Enable cursor
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
      FOR S%=0 TO 9999
        objlist{(S%)}.obj%=-1
        objlist{(S%)}.type%=0
        objlist{(S%)}.f%=-1
        objlist{(S%)}.rep%=0
        objlist{(S%)}.hop%=1
        objlist{(S%)}.parent%=-1
        objlist{(S%)}.x%=0
        objlist{(S%)}.y%=0
        objlist{(S%)}.h%=0
        objlist{(S%)}.v%=0
        objlist{(S%)}.m%=0
        objlist{(S%)}.d%=0
        objlist{(S%)}.u%=0

        frmlist{(S%).x%=0
        frmlist{(S%).y%=0
        frmlist{(S%).b%=0
        frmlist{(S%).f%=7

      NEXT

      REM reset world map and object list vars
      mmWX%=0
      mmWY%=0

      obj_lstcount%=-1
      obj_lstcur%=0

      movieframe%=-1       : REM displays current movie frame
      movieframetotal%=-1  : REM total frames saved


      ENDPROC

      REM ##########################################################
      REM change to mode 6 and overlay control codes on current screen
      DEF PROCcontrolcodes

      LOCAL C%,col%,p%,x%,y%

      showcodes%=0
      PROCframesave(frame%)
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

      FOR x%=0 TO 39

        REM show codes
        FOR y%=0 TO 23
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

        PRINTTAB(10,1)tb$;CHR$(157);ty$;"SPRITE EDITOR  ";CHR$(156);

        REM PRINTTAB(10,6)STRING$(20,CHR$(172));
        REM PRINTTAB(10,19)STRING$(20,CHR$(172));

        FOR Y%=2 TO 19
          REM   PRINTTAB(9,Y%)CHR$(181);
          REM PRINTTAB(30,Y%)CHR$(234);
          VDU 31,8,Y%,151
          VDU 31,31,Y%,156
        NEXT

        VDU 31,9,2,184
        VDU 31,9,19,169
        VDU 31,30,2,228
        VDU 31,30,19,166

        REM        PRINTTAB(18,19)tb$;CHR$(157);tc$;">  ";CHR$(156);
        PRINTTAB(10,20)tb$;CHR$(157);tc$;"< ";CHR$(156);tb$;CHR$(157);tc$;"> ";CHR$(156);

        PRINTTAB(27,24)CHR$(156);tr$;CHR$(157);ty$;"CLOSE  ";CHR$(156);

        PRINTTAB(10,22)tc$;"W:";tw$;"-";ty$;"  ";tw$;"+";tc$;" H";tw$;"-";ty$;"  ";tw$;"+";

        PRINTTAB(0,2)ty$;"LOAD";
        PRINTTAB(0,4)ty$;"SAVE";
        PRINTTAB(0,6)ty$;"IMPRT";
        PRINTTAB(0,8)tw$;"MOVIE";
        PRINTTAB(0,10)tc$;"CPY >";
        PRINTTAB(0,12)tc$;"CPY <";
        PRINTTAB(0,14)tc$;"COPY";
        PRINTTAB(0,16)tc$;"PASTE";

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
      PRINTTAB(22,20)ty$;CHR$(157);tr$;RIGHT$("00"+STR$(sprite_cur%+1),3)+" ";CHR$(156);CHR$(151)
      p$="CHR"
      IF spr_scroll%=0 THEN p$="PIX"
      PRINTTAB(36,2)tg$;p$;
      PRINTTAB(0,18)CHR$(129+spr_trns%);"TRANS";

      PRINTTAB(16,22)RIGHT$("0"+STR$(sprsize{(sprite_cur%)}.w%),2);
      PRINTTAB(26,22)RIGHT$("0"+STR$(sprsize{(sprite_cur%)}.h%),2);

      PROCdrawsprite
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
      REM draw pixel version of sprite for animation creator
      DEF PROCdrawsprbitmap(s%,x%,y%)
      LOCAL X%,Y%,C%
      GCOL 0,15
      FOR Y%=0 TO 47
        FOR X%=0 TO 39
          C%=FNpoint_sprbuf(X%,47-Y%,s%)
          REM IF C% THEN PLOT 69,x%+X%*4,y%+Y%*4
          IF C% THEN LINE x%+X%*2,y%+Y%*2,x%+X%*2,y%+Y%*2
        NEXT
      NEXT
      ENDPROC

      REM ##########################################################
      REM draw pixel version of sprite for animation creator
      DEF PROCdrawarrow(d%,x%,y%,c%)
      LOCAL X%,Y%,C%
      GCOL 0,c%

      FOR Y%=0 TO 11
        FOR X%=0 TO 11
          C%=arrows&(Y%*12+X%+d%*144)
          IF C% THEN LINE x%+X%*2,y%+Y%*2,x%+X%*2,y%+Y%*2
        NEXT
      NEXT
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

        PRINTTAB(1,22)tb$;CHR$(157);ty$;"IMPORT  ";CHR$(156);"   ";tm$;CHR$(157);ty$;"HELP  ";CHR$(156);"  ";tr$;CHR$(157);ty$;"CLOSE  ";CHR$(156)

        PROCprint40(24,ty$+"TelePaint"+tm$+version$+tc$+"by 4thStone & Pixelblip")
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
          ENDIF
        NEXT
      ENDIF

      REM ADD GRAPHICS CODE TO LEFT SIDE OF CANVAS
      FOR Y%=0 TO 23
        IF B% THEN
          frame_buffer&(D%-1,Y%*40)=144+B%
          frame_buffer&(D%-1,Y%*40+1)=157
          frame_buffer&(D%-1,Y%*40+2)=144+F%
        ELSE
          IF menuext%<>M_moviemode% THEN
            frame_buffer&(D%-1,Y%*40)=144+F%
          ELSE
            movie_buffer&(Y%*40)=144+F%
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
      REM print text at graphics pos x,y, text col, bg col
      DEF PROCgtext(t$,x%,y%,tc%,bc%)
      LOCAL L%

      L%=LEN(t$)
      GCOL 0,bc%
      RECTANGLE FILL x%,y%-32,L%*32-2,38
      VDU 5
      GCOL 0,tc%
      MOVE x%,y%+2
      PRINT t$
      VDU 4

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
      VDU 5
      GCOL 0,tc%
      MOVE x%+4,y%
      PRINT t$
      VDU 4
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
      MOVE x%+12,y%+2
      GCOL 0,tc%
      VDU 5
      PRINT t$
      VDU 4

      REM save control range
      controlrange{(n%)}.x1%=x%-1
      controlrange{(n%)}.y1%=y%-sy%+2
      controlrange{(n%)}.x2%=x%+sx%
      controlrange{(n%)}.y2%=y%+8

      menuXadd%=menuXadd%+sx%+16

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
        PRINTTAB(23,0)"MNU OBJ INS SPR";tr$;"X"
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
          WHEN M_canvas% : REM main canvas
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

      CASE menuext% OF
        WHEN M_sprSelect%,M_sprites%
          CASE menufrom% OF
            WHEN M_canvas%
              menuext%=M_canvas%
              menumode%=M_canvas%
              PROCframerestore(frame%)
              PROCmenudraw

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
            WHEN M_canvas%
              menuext%=M_canvas%
              menumode%=M_canvas%
              PROCframerestore(frame%)
              PROCmenudraw

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

      IF menuext%<>M_canvas% THEN
        IF menuext%>=M_paint% AND menuext%<=M_sprProperty% THEN
          PROCchangemode(7,1)
          sub_cur%=-1
        ENDIF

        IF menuext%<>M_moviemode% THEN menuext%=M_canvas%
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


      REM =======================================================================

      REM ##########################################################
      REM USER CUSTOMIZABLE PROCEDURE1
      DEF PROCCUSTOMPROC1
      LOCAL X%

      REM E.G. YOUR CODE
      FOR X%=2 TO 78
        PROCpoint(X%,20+SIN(X%/8)*8,1)
      NEXT

      ENDPROC

      REM ##########################################################
      REM USER CUSTOMIZABLE PROCEDURE1
      DEF PROCCUSTOMPROC2
      LOCAL X%

      REM E.G. YOUR CODE
      FOR X%=2 TO 78
        PROCpoint(X%,40+COS(X%/8)*8,1)
      NEXT

      ENDPROC

      REM drop down sub menu locations, X,TOP - W,H  (Y=TOP-W)
      REM 0-4 Paint, Dither, Copy, Fill, Special
      REM 5-8 object select, obj properties, frm properties, movie options
      DATA 448,960,460,908
      DATA 480,960,460,636
      DATA 512,960,460,908
      DATA 544,960,460,740
      DATA 576,960,460,712
      DATA 800,960,480,908
      DATA 800,960,480,908
      DATA 0,200,1278,200
      DATA 704,960,480,908

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
      DATA 0,0,0,0,0,1,1,0,0,0,0,0
      DATA 0,0,0,0,1,1,1,1,0,0,0,0
      DATA 0,0,0,1,1,1,1,1,1,0,0,0
      DATA 0,0,1,1,1,1,1,1,1,1,0,0
      DATA 0,1,1,1,0,1,1,0,1,1,1,0
      DATA 1,1,1,0,0,1,1,0,0,1,1,1
      DATA 1,1,0,0,0,1,1,0,0,0,1,1
      DATA 0,0,0,0,0,1,1,0,0,0,0,0
      DATA 0,0,0,0,0,1,1,0,0,0,0,0
      DATA 0,0,0,0,0,1,1,0,0,0,0,0
      DATA 0,0,0,0,0,1,1,0,0,0,0,0
      DATA 0,0,0,0,0,1,1,0,0,0,0,0

      DATA 0,0,0,0,0,1,1,0,0,0,0,0
      DATA 0,0,0,0,0,1,1,0,0,0,0,0
      DATA 0,0,0,0,0,1,1,0,0,0,0,0
      DATA 0,0,0,0,0,1,1,0,0,0,0,0
      DATA 0,0,0,0,0,1,1,0,0,0,0,0
      DATA 1,1,0,0,0,1,1,0,0,0,1,1
      DATA 1,1,1,0,0,1,1,0,0,1,1,1
      DATA 0,1,1,1,0,1,1,0,1,1,1,0
      DATA 0,0,1,1,1,1,1,1,1,1,0,0
      DATA 0,0,0,1,1,1,1,1,1,0,0,0
      DATA 0,0,0,0,1,1,1,1,0,0,0,0
      DATA 0,0,0,0,0,1,1,0,0,0,0,0

      DATA 0,0,0,0,0,1,1,0,0,0,0,0
      DATA 0,0,0,0,0,1,1,1,0,0,0,0
      DATA 0,0,0,0,0,0,1,1,1,0,0,0
      DATA 0,0,0,0,0,0,0,1,1,1,0,0
      DATA 0,0,0,0,0,0,0,0,1,1,1,0
      DATA 1,1,1,1,1,1,1,1,1,1,1,1
      DATA 1,1,1,1,1,1,1,1,1,1,1,1
      DATA 0,0,0,0,0,0,0,0,1,1,1,0
      DATA 0,0,0,0,0,0,0,1,1,1,0,0
      DATA 0,0,0,0,0,0,1,1,1,0,0,0
      DATA 0,0,0,0,0,1,1,1,0,0,0,0
      DATA 0,0,0,0,0,1,1,0,0,0,0,0

      DATA 0,0,0,0,0,1,1,0,0,0,0,0
      DATA 0,0,0,0,1,1,1,0,0,0,0,0
      DATA 0,0,0,1,1,1,0,0,0,0,0,0
      DATA 0,0,1,1,1,0,0,0,0,0,0,0
      DATA 0,1,1,1,0,0,0,0,0,0,0,0
      DATA 1,1,1,1,1,1,1,1,1,1,1,1
      DATA 1,1,1,1,1,1,1,1,1,1,1,1
      DATA 0,1,1,1,0,0,0,0,0,0,0,0
      DATA 0,0,1,1,1,0,0,0,0,0,0,0
      DATA 0,0,0,1,1,1,0,0,0,0,0,0
      DATA 0,0,0,0,1,1,1,0,0,0,0,0
      DATA 0,0,0,0,0,1,1,0,0,0,0,0

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

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

      REM *** TODO LIST ***

      REM *** INKEY(-256) MATRIX BRANYDY &4D (INCLUDE OTHERS?)

      REM *** INVESTIGATE LOCAL VERSIONS OF LIBRARIES TO MAKE PROGRAM CROSS BASIC COMPATIBLE (DONE?? Need Soruk to Test)

      REM *** LOAD SCREEN SORT BY NEWEST, NEEDS WORK!

      REM *** SCROLL OFF SCREEN E.G. NO WRAP

      REM *** SCROLL FRAMES LAYER, HOW WOULD THIS INTERACT WITH DRAW FRAMES?

      REM *** IMPLEMENT ANIMATED CIRCLE (REDO CIRCLE ROUTINE)

      REM *** IMAGE CONVERTER FOR IMPORTING BMP FILE, ADD MOVE FRAME OPTION (partially done)

      REM *** SPRITE MODE, EDIT SPRITES AND COPY TO FRAMES (in progress) 20x16 chars 40x48 pixels

      REM *** MENU DESIGN IN DIFFERENT MODE FOR MORE FLEXIBILITY

      REM *** BUGS ***
      REM     * IMPORT SPRITE IMAGE WIDTH ODD NUMBER CAUSES ANOMALY

      REM *** TODO LIST ***

      REM *** HELP SCREEN: MODE 6 TEXT: 40x25  PIXELS: 640x500 GU: 1280x1000 COLOURS: 16

      REM ALLOCATE 40MB FOR BUFFERS
      HIMEM = PAGE+40000000

      INSTALL @lib$+"sortlib"
      REM INSTALL @lib$+"aagfxlib"

      version$="v0.18"

      DEBUG%=0

      MODE 7

      REM FOR 64 BIT COMPARISONS
      REM *HEX 64

      REM ESC OFF FOR BACK ARROW ON SOME DEVICES
      *ESC OFF

      MOUSE ON 3

      VDU 23,1,0;0;0;0; : REM Disable cursor

      REM drawing bounds used for < and >
      xMin%=1
      xMax%=80
      yMin%=2
      yMax%=75

      REM fill bounds & BUFFER used with < and >
      fxMin%=2
      fxMax%=80
      fyMin%=3
      fyMax%=74
      fillmax%=255
      DIM fill{(fillmax%) x%,y%}

      REM OLD PIXEL & MOUSE COORDS
      OLD_PX%=0
      OLD_PY%=0
      OLD_MX%=0
      OLD_MY%=0
      OLD_TX%=0
      OLD_TY%=0
      newcode%=0
      oldcode%=0

      REM MOUSE COORDS
      MX%=0
      MY%=0
      MB%=0

      REM TEXT & PIXEL COORDS FOR CURRENT MOUSE READ LOCATION
      TX%=0
      TY%=0
      PX%=0
      PY%=0

      REM TEXT INPUT COORDS
      TEXTX%=0
      OTX%=0
      OTY%=0

      REM TOOL VARS
      curcol%=7
      oldcol%=7
      bakcol%=0
      textfore%=0
      toolsel%=1
      copypaste%=0
      copysize%=0
      copyx%=0
      copyy%=0
      copylockx%=-1
      copylocky%=-1
      copylockxt%=0
      copylockyt%=0
      shapesel%=-1
      toolcursor%=15
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
      frame%=1
      animation%=0
      menuext%=0
      session%=0
      curdir$=@dir$
      cursave$=@dir$
      lastsave$=""
      text$=""
      caps%=1
      showcodes%=0
      gridx%=10
      gridy%=2
      gridshow%=1

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
      scode&(2)=154 : REM separated
      scode&(3)=153 : REM contiguous
      scode&(4)=158 : REM hold graphics
      scode&(5)=159 : REM release
      scode&(6)=141 : REM double height text
      scode&(7)=140 : REM normal height text

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

      REM loading screen options
      menuext%=97
      frame_limit%=100
      frame_max%=8
      frame_old%=0
      sprite_limit%=48
      sprite_max%=8
      sprite_old%=0
      sprite_cur%=0
      spr_frmstart%=1
      spr_frmstep%=0
      spr_frmx%=1
      spr_frmy%=1
      spr_frmh%=0
      spr_frmv%=0
      spr_lstcount%=-1
      spr_trns%=1
      spr_impwid%=0
      spr_imphgt%=0
      spr_impbpp%=0
      spr_impofs%=0
      spr_scroll%=1

      PROCloadscreen

      menuext%=0
      sprite_old%=0

      REM frame buffer
      DIM frame_buffer&(frame_max%-1,959)

      REM 20x16 chars @320 bytes : 40x48 pixels @1920 bytes
      DIM sprite_buffer&(sprite_max%-1,319)
      DIM sprsize{(sprite_max%-1) h%,v%}
      DIM spr_tmp&(2000)
      DIM sprlist{(2000) s%,f%,x%,y%}

      REM undo buffer
      undo_max%=99
      DIM undo_buffer&(frame_max%-1,undo_max%,959)
      DIM undo_index%(frame_max%-1)
      DIM undo_count&(frame_max%-1)

      REM redo buffer
      DIM redo_buffer&(frame_max%-1,undo_max%,959)
      DIM redo_index%(frame_max%-1)
      DIM redo_count&(frame_max%-1)

      REM menu buffer
      DIM menu_buffer&(959)

      REM copypaste buffer
      DIM copy_buffer&(959)

      DIM import_buffer% 1000000


      PROCGR(curcol%,bakcol%,1)
      PROCdrawmenu

      FOR frame%=1 TO frame_max%
        PROCframesave(frame%)
        REM WAIT 10
      NEXT frame%
      frame%=1

      FOR s%=0 TO sprite_max%-1
        PROCsavesprite(s%)
      NEXT

      IF frame_old%=-2 THEN PROCshowhelp

      IF frame_old%=-3 THEN PROCimportimage

      VDU 23,1,1;0;0;0; : REM Enable cursor

      REM ##########################################################
      REM main loop starts here

      REPEAT

        PROCREADMOUSE

        IF MX%<>OLD_MX% OR MY%<>OLD_MY% OR MB% THEN
          TEXTX%=TX%

          REM handle mouse move and buttone clicks
          CASE MB% OF

            WHEN 1: REM right mouse click

            WHEN 2: REM middle mouse click

            WHEN 4: REM left mouse click or touch screen

              IF TY%=0 THEN
                REM click menu area
                PROCmenuhandler

              ELSE

                CASE menuext% OF
                  WHEN 1 : REM handle special / sub menu
                    PROCspecialscreen

                  WHEN 2 : REM SPRITE EDITOR
                    PROCspritescreen

                  OTHERWISE : REM MAIN DRAWING CANVAS
                    PROCmaincanvas

                ENDCASE
              ENDIF
          ENDCASE : REM mb%

        ELSE : REM no mouse move or clicks detected

          PROCcheckkeyboard


          REM FOR Y%=0 TO 24
          REM PROC_aaline(0, Y%*40, 1280, Y%*40, 1, &90909000, 0)
          REM  SYS "SDL_RenderDrawLine", @memhdc%, 0, Y%*20, 639, Y%*20
          REM NEXT
          REM *REFRESH

          WAIT 2

        ENDIF

        REM show debug information for mouse tracking etc
        IF DEBUG% THEN
          PRINTTAB(0,1)SPC(40)
          REM        PRINTTAB(0,1)"MX:";STR$(MX%);" MY:";STR$(MY%);" TX:";STR$(TX%);" TY:";STR$(TY%);" PX:";STR$(PX%);" PY:";STR$(PY%)
          PRINTTAB(0,1)"TX:";STR$(TX%);" TY:";STR$(TY%);" PX:";STR$(PX%);" PY:";STR$(PY%);" ME:";STR$(menuext%);

        ENDIF

        REM save old mouse position
        OLD_MX%=MX%
        OLD_MY%=MY%

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


      IF menuext%=0 OR menuext%=2 THEN
        IF showcodes%=1 THEN
          newcode%=GET(TX%,TY%)
          IF oldcode%<>newcode% THEN
            PRINTTAB(36,24)tc$+RIGHT$("00"+STR$(newcode%),3);
            oldcode%=newcode%
          ENDIF
        ENDIF
        IF toolsel%=5 OR toolsel%=6 OR toolsel%=7 OR shapesel%>2 THEN
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
        IF menuext%=1 THEN
          VDU 31,LEN(text$)+6,20

        ENDIF
      ENDIF

      ENDPROC

      REM ##########################################################
      REM WAIT FOR MOUSE TO BE A SPECIFIC BUTTON CLICK
      DEF PROCWAITMOUSE(M%)
      REPEAT
        PROCREADMOUSE
        WAIT 2
      UNTIL MB%=M%
      ENDPROC

      REM ##########################################################
      REM WAIT FOR NO KEY INPUT
      DEF PROCWAITNOKEY(K%,R%)
      REPEAT UNTIL INKEY(K%)=R%
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
      REM update grid
      DEF PROCdrawgrid
      LOCAL X%
      SYS "SDL_SetRenderDrawColor", @memhdc%, 63, 63, 63, 0
      CASE menuext% OF
        WHEN 0
          FOR X%=0 TO 39
            SYS "SDL_RenderDrawLine", @memhdc%, X%*16, 499, X%*16, 20
            IF X%<25 THEN SYS "SDL_RenderDrawLine", @memhdc%, 0, X%*20, 639, X%*20
          NEXT
      ENDCASE
      *REFRESH

      ENDPROC

      REM ##########################################################
      REM update sprite grid
      DEF PROCdrawspritegrid
      LOCAL X%
      SYS "SDL_SetRenderDrawColor", @memhdc%, 63, 63, 63, 0
      FOR X%=0 TO 20
        SYS "SDL_RenderDrawLine", @memhdc%, X%*16+160, 60, X%*16+160, 380
        IF X%<17 THEN SYS "SDL_RenderDrawLine", @memhdc%, 160, X%*20+60, 480, X%*20+60
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
      REM LINE ROUTINE FOR BUFFER USE m% TO PERFORM 0=ERASE / 1=DRAW / 2=EOR
      DEF PROCbresenham_buf(x1%,y1%,x2%,y2%,m%)
      LOCAL dx%, dy%, sx%, sy%, e
      dx% = ABS(x2% - x1%) : sx% = SGN(x2% - x1%)
      dy% = ABS(y2% - y1%) : sy% = SGN(y2% - y1%)
      IF dx% > dy% e = dx% / 2 ELSE e = dy% / 2

      REPEAT
        IF animategapcount%=0 THEN

          PROCpoint_buf(x1%,y1%,m%,frame%)

          REM change frame if animate length reaches 0
          animatelencount%-=1
          IF animatelencount%=0 THEN
            animatelencount%=animatelen%
            animategapcount%=animategap%
            frame%=frame%+1
            IF frame%>frame_max% THEN frame%=1
          ENDIF
        ELSE
          animategapcount%-=1
        ENDIF
        IF x1% = x2% IF y1% = y2% EXIT REPEAT
        IF dx% > dy% THEN
          x1% += sx% : e -= dy% : IF e < 0 e += dx% : y1% += sy%
        ELSE
          y1% += sy% : e -= dx% : IF e < 0 e += dy% : x1% += sx%
        ENDIF
      UNTIL FALSE
      ENDPROC

      REM ##########################################################
      REM RECTANGLE ROUTINE
      DEF PROCrectangle(x1%,y1%,x2%,y2%,m%)

      REM CHECK FOR SPECIAL CASES TO PRESERVE EOR OPERATIONS
      IF x1%=x2% AND y1%=y2% THEN
        PROCpoint(x1%,y1%,m%)
      ELSE
        IF x1%=x2% OR y1%=y2% THEN
          PROCbresenham(x1%,y1%,x2%,y2%,m%)
        ELSE
          PROCbresenham(x1%,y1%,x2%,y1%,m%)
          PROCbresenham(x1%,y2%,x2%,y2%,m%)
          IF ABS(y2%-y1%)>1 THEN
            IF y1%>y2% THEN SWAP y1%,y2%
            FOR Y%=y1%+1 TO y2%-1
              PROCpoint(x1%,Y%,m%)
              PROCpoint(x2%,Y%,m%)
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
          PROCbresenham_buf(x1%,y1%,x2%,y2%,m%)
        ELSE
          IF x1%>x2% THEN SWAP x1%,x2%
          IF y1%>y2% THEN SWAP y1%,y2%
          PROCbresenham_buf(x1%,y1%,x2%,y1%,m%)
          PROCbresenham_buf(x2%,y1%+1,x2%,y2%-1,m%)
          PROCbresenham_buf(x2%,y2%,x1%,y2%,m%)
          PROCbresenham_buf(x1%,y2%-1,x1%,y1%+1,m%)
        ENDIF
      ENDIF
      ENDPROC

      REM ##########################################################
      REM CIRCLE ROUTINE
      DEF PROCcircle(x1%,y1%,r%,m%)
      LOCAL p,x%,y%

      r%=ABS(r%)
      p=(5-r%*4)/4
      x%=0
      y%=r%

      PROCcirclepoints(x1%,y1%,x%,y%,m%)

      WHILE x%<y%
        x%+=1
        IF p<0 THEN
          p+=2*x%+1
        ELSE
          y%-=1
          p+=2*(x%-y%)+1
        ENDIF
        PROCcirclepoints(x1%,y1%,x%,y%,m%)
      ENDWHILE

      ENDPROC

      REM ##########################################################
      REM THIS PLOTS THE POINTS FOR CIRCLE ROUTINE
      DEF PROCcirclepoints(cx%,cy%,x%,y%,m%)
      IF x%=0 THEN
        PROCpoint(cx%,cy%+y%,m%)
        PROCpoint(cx%,cy%-y%,m%)
        PROCpoint(cx%+y%,cy%,m%)
        PROCpoint(cx%-y%,cy%,m%)
      ELSE
        IF x%<=y% THEN
          PROCpoint(cx%+x%,cy%+y%,m%)
          PROCpoint(cx%-x%,cy%+y%,m%)
          PROCpoint(cx%+x%,cy%-y%,m%)
          PROCpoint(cx%-x%,cy%-y%,m%)
          IF x%<y% THEN
            PROCpoint(cx%+y%,cy%+x%,m%)
            PROCpoint(cx%-y%,cy%+x%,m%)
            PROCpoint(cx%+y%,cy%-x%,m%)
            PROCpoint(cx%-y%,cy%-x%,m%)
          ENDIF
        ENDIF
      ENDIF
      ENDPROC

      REM ##########################################################
      REM ### flood fill from ART4BBW
      DEF PROCfloodfill(sx%,sy%)

      IF sx%>xMin% AND sx%<xMax% AND sy%>yMin% AND sy%<yMax% THEN

        LOCAL uf,df,c%,x%,y%,mc%
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
      REM load screen
      DEF PROCloadscreen

      PRINTTAB(3,2)CHR$(141);gr$;CHR$(157);ty$;"= = T E L E P A I N T = =  ";CHR$(156)
      PRINTTAB(3,3)CHR$(141);gy$;CHR$(157);tr$;"= = T E L E P A I N T = =  ";CHR$(156)

      PRINTTAB(8,6)gm$;CHR$(157);ty$;"  IMPORT IMAGE    ";CHR$(156)
      PRINTTAB(8,9)gm$;CHR$(157);ty$;"  DISPLAY HELP    ";CHR$(156)
      PRINTTAB(7,12)CHR$(141);gb$;CHR$(157);ty$;"LAUNCH TELEPAINT  ";CHR$(156)
      PRINTTAB(7,13)CHR$(141);gb$;CHR$(157);ty$;"LAUNCH TELEPAINT  ";CHR$(156)

      PRINTTAB(0,18)ty$;"MAX FRAMES (2-";STR$(frame_limit%);")";tw$;"< -";tc$;"  8";tw$;"+ >"
      PRINTTAB(0,20)ty$;"MAX SPRITES (1-";STR$(sprite_limit%);")";tw$;"< -";tc$;"  8";tw$;"+ >"

      PRINTTAB(0,24)tb$;"Telepaint ";version$;

      REPEAT
        IF frame_max%<>frame_old% THEN
          PRINTTAB(24,18)RIGHT$("  "+STR$(frame_max%),3)
          frame_old%=frame_max%
        ENDIF

        IF sprite_max%<>sprite_old% THEN
          PRINTTAB(24,20)RIGHT$("  "+STR$(sprite_max%),3)
          sprite_old%=sprite_max%
        ENDIF

        PROCREADMOUSE
        IF MB%=4 THEN
          CASE TY% OF

            WHEN 6 : REM import image dialog
              IF TX%>8 AND TX%<29 THEN frame_old%=-3

            WHEN 9 : REM display help screen
              IF TX%>8 AND TX%<29 THEN frame_old%=-2

            WHEN 12,13 : REM launch telepaint
              IF TX%>8 AND TX%<29 THEN frame_old%=-1

            WHEN 18 : REM change max frames var
              IF TX%=20 THEN frame_max%=2
              IF TX%=22 AND frame_max%>2 THEN frame_max%-=1
              IF TX%=28 AND frame_max%<frame_limit% THEN frame_max%+=1
              IF TX%=30 frame_max%=frame_limit%
              WAIT 10

            WHEN 20 : REM change max sprites var
              IF TX%=20 THEN sprite_max%=1
              IF TX%=22 AND sprite_max%>1 THEN sprite_max%-=1
              IF TX%=28 AND sprite_max%<sprite_limit% THEN sprite_max%+=1
              IF TX%=30 sprite_max%=sprite_limit%
              WAIT 10


          ENDCASE
        ELSE
          WAIT 10
          REM PRINTTAB(30,24);STR$(TX%);",";STR$(TY%);"  ";
        ENDIF
      UNTIL frame_old%<0
      PROCWAITMOUSE(0)

      ENDPROC

      REM ##########################################################
      REM check keyboard
      DEF PROCcheckkeyboard

      CASE menuext% OF
        WHEN 0 : REM keyboard handler

          REM TEXT AT CURSOR HANDLER, IF MOUSE IS MOVED, NEW TEXT POS IS SET
          K%=INKEY(0)
          IF K%>1 THEN
            REM SAVE UNDO ONLY FOR CURRENT 'LINE' OF TEXT
            IF TX%<>OTX% OR TY%<>OTY% AND TY%>0 THEN
              OTX%=TX%
              OTY%=TY%
            ENDIF

            REM handle specific keypresses
            CASE K% OF
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

              WHEN 136 : REM left cursor
                PROCloadnextframe(-1,1) : REM SAVE CURRENT FRAME AND LOAD PREVIOUS FRAME
                REM PROCWAITNOKEY(0,-1)

              WHEN 137 : REM right cursor
                PROCloadnextframe(1,1) : REM SAVE CURRENT FRAME AND LOAD NEXT FRAME
                REM PROCWAITNOKEY(0,-1)

              OTHERWISE
                REM ADD VALID CHARS AND INCREASE TEXT POS
                IF K%>31 AND K%<127  AND TY%>0 THEN
                  PROCundosave
                  VDU 31,TEXTX%,TY%,K%
                  IF TEXTX%<39 THEN TEXTX%+=1
                  PROCframesave(frame%)
                ENDIF
            ENDCASE
          ENDIF

        WHEN 1 : REM special sub menu

        WHEN 2 : REM sprite menu

          REM left cursor key
          IF INKEY(-26) THEN
            sprite_cur%-=1
            IF sprite_cur%<0 THEN sprite_cur%=sprite_max%-1
            PROCspritemenu(0)
            sprite_old%=sprite_cur%
            PROCWAITNOKEY(-26,0)
          ENDIF

          REM right cursor key
          IF INKEY(-122) THEN
            sprite_cur%+=1
            IF sprite_cur%>sprite_max%-1 THEN sprite_cur%=0
            PROCspritemenu(0)
            sprite_old%=sprite_cur%
            PROCWAITNOKEY(-122,0)
          ENDIF

      ENDCASE

      PROCWAITNOKEY(0,-1)

      ENDPROC

      REM ##########################################################
      REM MENU HANDLER
      DEF PROCmenuhandler
      PROCWAITMOUSE(0)

      CASE TX% OF
        WHEN 0 : REM display control codes
          PROCmenucheck
          PROCcontrolcodes

        WHEN 1,2,3,4,5,6,7,8,9,10,11,12,13,14 : REM colour selector
          oldcol%=curcol%
          curcol%=(TX%+1) DIV 2

          IF curcol%=oldcol% THEN
            textfore%=(textfore%+1) AND 1
          ENDIF

        WHEN 15 : toolsel%=1:toolcursor%=TX% : REM paint
        WHEN 16 : REM dither & scale merged
          IF toolsel%=2 THEN
            dither%=(dither%+1) MOD 5:toolsel%=2:toolcursor%=16
          ENDIF
          toolcursor%=TX%
          toolsel%=2

        WHEN 17 : REM copy
          IF toolsel%=7 THEN
            copypaste%=(copypaste%+1) AND 1
          ELSE
            toolsel%=7:toolcursor%=TX%
          ENDIF

        WHEN 18 : toolsel%=3:toolcursor%=TX% : REM fill
        WHEN 19 : REM shape / special menu
          IF menuext%<>1 THEN
            REM IF menuext%>0 THEN PROCmenurestore
            REM PROCmenusave
            REM *** SAVEFRAME???
            menuext%=1
            PROCspecialmenu(1)
          ELSE
            PROCmenurestore
            REM *** DRAWFRAME??
          ENDIF

        WHEN 21 : erase%=(erase%+1) AND 1 : REM toggle erase tool

        WHEN 23 : PROCmenucheck : PROCundorestore : REM undo PROCmenurestore:
        WHEN 25 : PROCmenucheck : PROCredorestore : REM redo PROCmenurestore:

        WHEN 27 : PROCmenucheck : PROCclearscreen:toolsel%=1:toolcursor%=15 : REM clearscreen PROCmenurestore:
        WHEN 28 : toolsel%=5:toolcursor%=TX% : REM background colour
        WHEN 29 : toolsel%=6:toolcursor%=TX% : REM foreground colour

        WHEN 31 : PROCmenucheck : PROCloadfile(0) : REM load file dialog - 0 load bin file PROCmenurestore:
        WHEN 32 : PROCmenucheck : PROCsavefile : REM save frames to file PROCmenurestore:

        WHEN 34 : animation%=(animation%+1) AND 1 : REM toggle frame animation advance tool

          REM                  WHEN 36 : REM frame%
        WHEN 36,37 : PROCmenucheck : PROCloadnextframe(-1,1) : REM save current frame and display previous frame in sequence PROCmenurestore:
        WHEN 38 : PROCmenucheck : PROCloadnextframe(1,1) : REM save current frame and display next frame in sequence  PROCmenurestore:
        WHEN 39 : PROCmenucheck : PROCplay : REM save current frame and play all frames in a loop  PROCmenurestore:


      ENDCASE

      REM hide shape menu if another menu item was clicked
      IF TX%<>19 AND menuext%=1 THEN PROCmenurestore
      IF toolsel%<>4 THEN shapesel%=-1

      PROCdrawmenu

      ENDPROC

      REM ##########################################################
      REM SPECIAL DIALOG
      DEF PROCspecialscreen

      PROCWAITMOUSE(0)
      CASE TY% OF

        WHEN 2
          CASE TX% OF
            WHEN 1,2,3: shapesel%=0 : REM line

            WHEN 15,16,17: animateshape%=(animateshape%+1) AND 1 : REM animated shape toggle

          ENDCASE

        WHEN 3
          CASE TX% OF
            WHEN 1,2,3: shapesel%=1 : REM rectangle

            WHEN 24:  REM animated gap decrement
              animategap%-=1
              IF animategap%<0 THEN animategap%=0
            WHEN 28:  REM animated gap increment
              animategap%+=1
              IF animategap%>5 THEN animategap%=5

            WHEN 35:  REM animated len decrement
              animatelen%-=1
              IF animatelen%<1 THEN animatelen%=1
            WHEN 39:  REM animated len increment
              animatelen%+=1
              IF animatelen%>5 THEN animatelen%=5

          ENDCASE

        WHEN 4
          CASE TX% OF
            WHEN 1,2,3: shapesel%=2 : REM circle

            WHEN 15,16,17: gridshow%=(gridshow%+1) AND 1 : REM gridshow toggle
          ENDCASE

        WHEN 6
          CASE TX% OF
            WHEN 1,2,3: shapesel%=3 : REM flashing code

            WHEN 18,19,20: shapesel%=6 : REM double height code

          ENDCASE

        WHEN 7
          CASE TX% OF
            WHEN 1,2,3: shapesel%=4 : REM separated code

            WHEN 18,19,20: shapesel%=5 : REM hold code

          ENDCASE

        WHEN 9
          CASE TX% OF
            WHEN 1,2,3: shapesel%=8 : REM text code whole column

            WHEN 11,12,13: shapesel%=9 : REM graphic code whole column

          ENDCASE

        WHEN 10
          CASE TX% OF
            WHEN 1,2,3: copylockxt%=(copylockxt%+1) AND 1 : REM lock horizontal paste pos

            WHEN 11,12,13: copylockyt%=(copylockyt%+1) AND 1 :REM lock vertical paste pos

          ENDCASE


        WHEN 11
          CASE TX% OF
            WHEN 1,2,3: shapesel%=7 : REM text tool

          ENDCASE

        WHEN 12,13,14,15,16,17,18,19 : REM alphabet selector
          IF TY%=18 AND TX%>33 AND TX%<37 THEN
            caps%=(caps%+1) AND 1
          ELSE
            IF TY%=18 AND TX%>28 AND TX%<33 THEN
              text$=text$+CHR$(32)
            ELSE
              C%=GET(TX%,TY%)
              IF C%<>32 THEN text$=text$+CHR$(C%)
            ENDIF
          ENDIF
          text$=LEFT$(text$,30)
          shapesel%=7

        WHEN 20 : REM text controls
          CASE TX% OF
            WHEN 37 : REM backspace
              IF text$<>"" THEN text$=LEFT$(text$,LEN(text$)-1)
            WHEN 39 : REM clear text
              text$=""
          ENDCASE

          REM showcodes%=(showcodes%+1) AND 1

        WHEN 22 : REM SPRITE, HELP, CLOSE BUTTTONS
          CASE TX% OF
            WHEN 2,3,4,5,6,7,8,9,10,11,12
              menuext%=2
              PROCspritemenu(1)

            WHEN 17,18,19,20,21,22,23,24
              MODE7
              menuext%=0
              PROCdrawmenu
              PROCshowhelp
              menuext%=1
              PROCspecialmenu(1)

            WHEN 29,30,31,32,33,34,35,36,37
              PROCmenurestore

          ENDCASE
      ENDCASE
      IF shapesel%>-1 THEN toolsel%=4:toolcursor%=19

      IF menuext%=1 THEN PROCspecialmenu(0)

      ENDPROC

      REM ##########################################################
      REM SPRITE EDITOR DIALOG
      DEF PROCspritescreen
      CASE toolsel% OF
        WHEN 1: REM PAINT TOOL
          IF PX%>19 AND PX%<60 AND PY%>8 AND PY%<57 THEN
            PROCpoint(PX%,PY%,1)
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
            PROCsavesprite(sprite_cur%)

          ENDIF
        WHEN 2 : REM DITHER
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

          PROCsavesprite(sprite_cur%)

        WHEN 5: REM background colour
          IF TX%>9 AND TX%<30 AND TY%>2 AND TY%<19 THEN
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
            PROCsavesprite(sprite_cur%)
          ENDIF
        WHEN 6: REM foreground colour
          IF TX%>9 AND TX%<30 AND TY%>2 AND TY%<19 THEN
            VDU 31,TX%,TY%,(curcol%+144-textfore%*16)
            REPEAT
              PROCREADMOUSE
              IF TX%<>OLD_TX% OR TY%<>OLD_TY% THEN
                IF TX%>9 AND TX%<30 AND TY%>2 AND TY%<19 THEN VDU 31,TX%,TY%,(curcol%+144-textfore%*16)
                OLD_TX%=TX%
                OLD_TY%=TY%
              ENDIF
            UNTIL MB%=0
            PROCsavesprite(sprite_cur%)
          ENDIF
      ENDCASE

      REM 20x16 chars @320 bytes : 40x48 pixels @1920 bytes
      PROCWAITMOUSE(0)

      REM process sprite buttons
      CASE TY% OF
        WHEN 2
          CASE TX% OF
            WHEN 1,2,3,4,5 : REM LOAD
              PROCloadspritefile(@tmp$+"SPRITEDATA.SPR")
              PROCdrawsprite

            WHEN 33,34,35 : REM CLS
              FOR S%=0 TO 319
                sprite_buffer&(sprite_cur%,S%)=32
              NEXT
              PROCdrawsprite

            WHEN 37,38,39 : REM SCROLL MODE
              spr_scroll%=(spr_scroll%+1) AND 1
              PROCspritemenu(0)

          ENDCASE
        WHEN 4
          CASE TX% OF
            WHEN 1,2,3,4,5 : REM SAVE
              PROCsavespritefile(@tmp$+"SPRITEDATA.SPR")

            WHEN 33,34,35,36,37 : REM SCROLL LEFT

              IF spr_scroll%=1 THEN
                REM CHAR MODE
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
                REM PIX MODE

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
            WHEN 1,2,3,4,5 : REM ADD SPRITE TO LIST

              spr_lstcount%+=1
              sprlist{(spr_lstcount%)}.s%=sprite_cur%
              sprlist{(spr_lstcount%)}.f%=spr_frmstart%
              sprlist{(spr_lstcount%)}.x%=spr_frmx%
              sprlist{(spr_lstcount%)}.y%=spr_frmy%

              IF spr_frmstep%>0 THEN
                X%=spr_frmx%+spr_frmh%
                Y%=spr_frmy%+spr_frmv%
                FOR ST%=spr_frmstart%+spr_frmstep% TO frame_max% STEP spr_frmstep%
                  spr_lstcount%+=1
                  sprlist{(spr_lstcount%)}.s%=sprite_cur%
                  sprlist{(spr_lstcount%)}.f%=ST%
                  sprlist{(spr_lstcount%)}.x%=X%
                  sprlist{(spr_lstcount%)}.y%=Y%
                  X%+=spr_frmh%
                  Y%+=spr_frmv%

                NEXT
              ENDIF
              PROCspritemenu(0)

            WHEN 33,34,35,36,37 : REM SCROLL RIGHT
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
            WHEN 1,2,3,4,5 : REM VIEW SPRITE LIST
              IF spr_lstcount%>-1 THEN
                CLS
                FOR S%=0 TO spr_lstcount%
                  PRINT"SPR: ";RIGHT$("0"+STR$(sprlist{(S%)}.s%+1),2);"  ";
                  PRINT"FRM: ";RIGHT$("00"+STR$(sprlist{(S%)}.f%),3);"  ";
                  PRINT"X: ";RIGHT$("0"+STR$(sprlist{(S%)}.x%),2);"  ";
                  PRINT"Y: ";RIGHT$("0"+STR$(sprlist{(S%)}.y%),2)
                NEXT

                PROCWAITMOUSE(4)
                PROCWAITMOUSE(0)

                PROCdrawmenu
                PROCspritemenu(1)
              ENDIF
            WHEN 33,34,35,36,37 : REM SCROLL UP
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
            WHEN 1,2,3,4,5 : REM clear sprite list
              spr_lstcount%=-1
              PROCspritemenu(0)

            WHEN 33,34,35,36,37 : REM SCROLL DOWN
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
            WHEN 1,2,3,4,5 : REM import sprite
              REM menuext%=88
              REM done%=0

              REM              PROCloadfile(2)
              PROCimportsprite

              REM after loadfile menuext% returns: No file: 94, single image: 95, grid: 96

              PROCdrawmenu
              PROCspritemenu(1)
              PROCdrawsprite


            WHEN 33,34,35,36,37 : REM FLIP HORIZONTAL
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
              toolsel%=7:toolcursor%=17:copypaste%=1
              PROCmenurestore

            WHEN 33,34,35,36,37 : REM FLIP VERTICAL
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
              FOR S%=0 TO 319
                sprite_buffer&(sprite_cur%,(S% MOD 16)*20+(S% DIV 16))=copy_buffer&(S%)
              NEXT
              PROCdrawsprite

            WHEN 33,34,35,36,37 : REM COPY TO NEXT SPRITE
              dst%=sprite_cur%+1
              IF dst%>sprite_max%-1 THEN dst%=0
              FOR S%=0 TO 319
                sprite_buffer&(dst%,S%)=sprite_buffer&(sprite_cur%,S%)
              NEXT

          ENDCASE

        WHEN 19 : REM PREV / NEXT SPRITE
          CASE TX% OF
            WHEN 11,12,13,14
              sprite_cur%-=1
              IF sprite_cur%<0 THEN sprite_cur%=sprite_max%-1
            WHEN 17,18,19,20
              sprite_cur%+=1
              IF sprite_cur%>sprite_max%-1 THEN sprite_cur%=0

          ENDCASE

        WHEN 18
          CASE TX% OF
            WHEN 1,2,3,4,5 : REM sprite transparency toggle
              spr_trns%=(spr_trns%+1) MOD 2
              PROCspritemenu(0)

            WHEN 33,34,35,36,37 : REM COPY TO PREV SPRITE
              dst%=sprite_cur%-1
              IF dst%<0 THEN dst%=sprite_max%-1
              FOR S%=0 TO 319
                sprite_buffer&(dst%,S%)=sprite_buffer&(sprite_cur%,S%)
              NEXT

          ENDCASE

        WHEN 21 : REM DRAW / UNDO SPRITES
          CASE TX% OF
            WHEN 1,2,3,4,5,6,7,8,9,10,11,12 : REM DRAW ALL SPRITES
              IF spr_lstcount%>-1 THEN
                FOR S%=0 TO spr_lstcount%
                  IF sprlist{(S%)}.f%<frame_max%+1 THEN
                    PROCspritetoframe(sprlist{(S%)}.f%,sprlist{(S%)}.s%,sprlist{(S%)}.x%,sprlist{(S%)}.y%)
                  ENDIF
                NEXT

              ENDIF

            WHEN 15,16,17,18,19,20,21,22,23,24,25 : REM UNDO ALL SPRITES

            WHEN 29,30,31,32,33,34,35,36,37 : REM close sprites screen
              PROCmenurestore

          ENDCASE

        WHEN 23 : REM FRAME POINTERS
          CASE TX% OF
            WHEN 13 : REM FRAME DECREMENT
              IF spr_frmstart%>1 THEN spr_frmstart%-=1

            WHEN 19 : REM FRAME INCREMENT
              IF spr_frmstart%<frame_max% THEN spr_frmstart%+=1

            WHEN 27 : REM STEP DECREMENT
              IF spr_frmstep%>0 THEN spr_frmstep%-=1

            WHEN 33 : REM STEP INCREMENT
              IF spr_frmstep%<frame_max%-1 THEN spr_frmstep%+=1

          ENDCASE
          PROCspritemenu(0)

        WHEN 24 : REM SPRITE OFFSETS
          CASE TX% OF
            WHEN 3 : REM X DECREMENT
              IF spr_frmx%>1 THEN spr_frmx%-=1

            WHEN 8 : REM X INCREMENT
              IF spr_frmx%<39 THEN spr_frmx%+=1

            WHEN 12 : REM Y DECREMENT
              IF spr_frmy%>1 THEN spr_frmy%-=1

            WHEN 17 : REM Y INCREMENT
              IF spr_frmy%<24 THEN spr_frmy%+=1

            WHEN 21 : REM H DECREMENT
              IF spr_frmh%>-5 THEN spr_frmh%-=1

            WHEN 26 : REM H INCREMENT
              IF spr_frmh%<5 THEN spr_frmh%+=1

            WHEN 30 : REM V DECREMENT
              IF spr_frmv%>-5 THEN spr_frmv%-=1

            WHEN 35 : REM V INCREMENT
              IF spr_frmv%<5 THEN spr_frmv%+=1

          ENDCASE
          PROCspritemenu(0)
      ENDCASE

      IF sprite_cur%<>sprite_old% THEN
        REM PROCsavesprite(sprite_old%)
        PROCspritemenu(0)
        sprite_old%=sprite_cur%
      ENDIF

      ENDPROC

      REM ##########################################################
      REM MAIN CANVAS
      DEF PROCmaincanvas
      IF menuext%>0 THEN PROCmenurestore
      REM *** DRAWFRAME??

      CASE toolsel% OF
        WHEN 1: REM PAINT TOOL
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

        WHEN 2: REM DITHER TOOL
          PROCundosave

          CASE dither% OF
            WHEN 0,1,2,3
              D%=2^(dither%)
              DA%=2
              IF dither%=2 THEN DA%=4
              IF dither%=3 THEN DA%=8

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
          REPEAT
            PROCREADMOUSE
            IF PX%<>OLD_PX% OR PY%<>OLD_PY% THEN
              CASE dither% OF
                WHEN 0,1,2,3
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

        WHEN 7: REM copy / paste tool

          IF copypaste%=0 THEN
            menuext%=98

            startx%=TX%*2: starty%=TY%*3
            OLD_PX%=TX%*2 : OLD_PY%=TY%*3
            PROCpoint(startx%,starty%,2)

            REPEAT
              PROCREADMOUSE
              IF TX%*2+1<>OLD_PX% OR TY%*3+2<>OLD_PY% THEN
                PROCrectangle(startx%,starty%,OLD_PX%,OLD_PY%,2)
                PROCrectangle(startx%,starty%,TX%*2+1,TY%*3+2,2)
                OLD_PX%=TX%*2+1
                OLD_PY%=TY%*3+2
              ENDIF
            UNTIL MB%=0
            REM PROCrectangle(startx%,starty%,TX%*2+1,TY%*3+2,2)

            copypaste%=1

            PROCmenurestore
            PROCcopyregion(startx%/2,starty%/3,TX%,TY%)
            PROCdrawmenu

          ELSE
            PROCWAITMOUSE(0)
            PROCpasteregion(TX%,TY%)
          ENDIF

          PROCframesave(frame%)
          IF animation% THEN PROCloadnextframe(1,0)

        WHEN 3: REM FILL TOOL
          PROCundosave
          PROCfloodfill(PX%,PY%)
          PROCWAITMOUSE(0)
          PROCframesave(frame%)
          IF animation% THEN PROCloadnextframe(1,0)

        WHEN 4: REM shape / special tools
          CASE shapesel% OF
            WHEN 0: REM line tool
              IF animateshape% THEN
                PROCundosaveall
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
                PROCbresenham_buf(startx%,starty%,PX%,PY%,1-erase%)
                frame%=oldframe%-1
                PROCloadnextframe(1,0)
              ELSE
                PROCbresenham(startx%,starty%,PX%,PY%,1-erase%)
                PROCframesave(frame%)
                IF animation% THEN PROCloadnextframe(1,0)
              ENDIF

            WHEN 1: REM rectangle tool
              IF animateshape% THEN
                PROCundosaveall
              ELSE
                PROCundosave
              ENDIF

              startx%=PX%: starty%=PY%
              OLD_PX%=PX% : OLD_PY%=PY%
              PROCpoint(startx%,starty%,2)

              REPEAT
                PROCREADMOUSE
                IF PX%<>OLD_PX% OR PY%<>OLD_PY% THEN
                  PROCrectangle(startx%,starty%,OLD_PX%,OLD_PY%,2)
                  PROCrectangle(startx%,starty%,PX%,PY%,2)
                  OLD_PX%=PX%
                  OLD_PY%=PY%
                ENDIF
              UNTIL MB%=0
              REM PROCrectangle(startx%,starty%,PX%,PY%,2)
              PROCmenurestore
              IF animateshape%=1 THEN
                oldframe%=frame%
                PROCframesave(frame%)
                animatelencount%=animatelen%
                animategapcount%=0
                PROCrectangle_buf(startx%,starty%,PX%,PY%,1-erase%)
                frame%=oldframe%-1
                PROCloadnextframe(1,0)
              ELSE
                PROCrectangle(startx%,starty%,PX%,PY%,1-erase%)
                PROCframesave(frame%)
                IF animation% THEN PROCloadnextframe(1,0)
              ENDIF

            WHEN 2: REM circle
              PROCundosave
              startx%=PX%: starty%=PY%
              OLD_PX%=PX% : OLD_PY%=PY%
              REM PROCpoint(startx%,starty%,2)

              REPEAT
                PROCREADMOUSE
                IF PX%<>OLD_PX% OR PY%<>OLD_PY% THEN
                  PROCcircle(startx%,starty%,startx%-OLD_PX%,2)
                  PROCcircle(startx%,starty%,startx%-PX%,2)
                  OLD_PX%=PX%
                  OLD_PY%=PY%
                ENDIF
              UNTIL MB%=0
              PROCmenurestore
              PROCcircle(startx%,starty%,startx%-PX%,1-erase%)
              PROCframesave(frame%)
              IF animation% THEN PROCloadnextframe(1,0)

            WHEN 3,4,5,6 : REM special control codes
              PROCundosave
              IF TX%<40 AND TX%>-1 AND TY%>0 AND TY%<25 THEN VDU 31,TX%,TY%,scode&((shapesel%-3)*2+erase%)
              REPEAT
                PROCREADMOUSE
                IF TX%<>OLD_TX% OR TY%<>OLD_TY% THEN
                  IF TX%<40 AND TX%>-1 AND TY%>0 AND TY%<25 THEN VDU 31,TX%,TY%,scode&((shapesel%-3)*2+erase%)
                ENDIF
                OLD_TX%=TX%
                OLD_TY%=TY%
              UNTIL MB%=0
              PROCframesave(frame%)
              IF animation% THEN PROCloadnextframe(1,0)

            WHEN 7: REM text print tool
              PROCundosave
              PROCWAITMOUSE(0)
              A$=LEFT$(text$,40-TX%)
              PRINTTAB(TX%,TY%)A$;
              PROCframesave(frame%)
              IF animation% THEN PROCloadnextframe(1,0)

            WHEN 8: REM foreground entire column
              PROCundosave
              PROCWAITMOUSE(4)
              PROCWAITMOUSE(0)

              FOR Y%=1 TO 24
                IF TX%<40 AND TX%>-1 AND TY%>0 AND TY%<25 THEN
                  VDU 31,TX%,Y%,(curcol%+144-textfore%*16)
                ELSE
                  EXIT FOR
                ENDIF
              NEXT
              PROCframesave(frame%)
              IF animation% THEN PROCloadnextframe(1,0)

            WHEN 9: REM backgroung entire column

              PROCWAITMOUSE(4)
              PROCWAITMOUSE(0)

              IF TX%<39 AND TX%>-1 AND TY%>0 AND TY%<25 THEN
                PROCundosave
                FOR Y%=1 TO 24
                  IF erase% THEN
                    VDU 31,TX%,Y%,156
                  ELSE
                    VDU 31,TX%,Y%,(curcol%+144),157
                  ENDIF
                NEXT
                PROCframesave(frame%)
                IF animation% THEN PROCloadnextframe(1,0)
              ENDIF

          ENDCASE

        WHEN 5: REM background colour
          PROCundosave
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
          PROCframesave(frame%)
          IF animation% THEN PROCloadnextframe(1,0)

        WHEN 6: REM foreground colour
          PROCundosave

          IF TX%<40 AND TX%>-1 AND TY%>0 AND TY%<25 THEN VDU 31,TX%,TY%,(curcol%+144-textfore%*16)
          REPEAT
            PROCREADMOUSE
            IF TX%<>OLD_TX% OR TY%<>OLD_TY% THEN
              IF TX%<40 AND TX%>-1 AND TY%>0 AND TY%<25 THEN VDU 31,TX%,TY%,(curcol%+144-textfore%*16)
              OLD_TX%=TX%
              OLD_TY%=TY%
            ENDIF
          UNTIL MB%=0
          PROCframesave(frame%)
          IF animation% THEN PROCloadnextframe(1,0)

      ENDCASE  : REM toolsel%


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
      LOCAL I%,L%,A$,B$,C$,cls%,fix%,done%,col_old%,bak_old%,h_old%,v_old%,hindex%,vindex%,skip%,skip_old%

      REM PROCmenusave
      REM *** FRAMESAVE??

      menuext%=99
      cls%=1
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


      PRINTTAB(4,11)tb$;"OPTION:  ";tc$;"CLS:";tg$;"Y";tc$;" FIX:";tg$;"Y";
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
          PROCGR(curcol%,bakcol%,cls%)
          IF cls% THEN
            FOR frame%=1 TO frame_max%
              PROCframesave(frame%)
              REM WAIT 10
            NEXT frame%
            frame%=1
          ELSE
            PROCframesave(1)
            frame%=1

            FOR I%=2 TO frame_max%
              PROCGR_BUF(I%,curcol%,bakcol%)
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
            C%=frame_max% DIV framedupe%
            FOR F%=1 TO framedupe%
              FOR T%=1 TO C%-1
                PROCcopyframe(F%,F%+framedupe%*T%,0,0,skip%)
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
      REMPROCloadnextframe(1,0)

      ENDPROC

      REM ##########################################################
      REM SAVE FRAME BUFFER
      DEF PROCframesave(f%)
      LOCAL U%

      FOR U%=0 TO 959
        frame_buffer&(f%-1,U%)=GET(U% MOD 40,U% DIV 40+1)
      NEXT

      ENDPROC

      REM ##########################################################
      REM RESTORE FRAME BUFFER
      DEF PROCframerestore(f%)
      LOCAL U%

      FOR U%=0 TO 959
        VDU 31,(U% MOD 40),(U% DIV 40+1),frame_buffer&(f%-1,U%)
      NEXT

      ENDPROC

      REM ##########################################################
      REM COPY A FRAME STARTING FROM OFFSET
      DEF PROCcopyframe(S%,D%,H%,V%,skip%)
      LOCAL U%,X%,Y%,xofs%,yofs%

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
      PROCdrawmenu
      ENDPROC

      REM ##########################################################
      REM copy sprite buffer to frame
      DEF PROCspritetoframe(f%,s%,sx%,sy%)
      LOCAL S%,U%,X%,Y%
      FOR U%=0 TO 319
        X%=sx%+U% MOD 20
        Y%=sy%+U% DIV 20
        IF X%>0 AND X%<40 AND Y%>0 AND Y%<25 THEN
          S%=sprite_buffer&(s%,U%)
          IF spr_trns%=1 THEN
            IF S%<>32 AND S%<>160 THEN frame_buffer&(f%-1,X%+(Y%-1)*40)=S%
          ELSE
            frame_buffer&(f%-1,X%+(Y%-1)*40)=S%
          ENDIF
        ENDIF
      NEXT

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
      REM save current screen to undo buffer
      DEF PROCundosave
      LOCAL U%

      IF undo_count&(frame%-1)<undo_max% THEN undo_count&(frame%-1)+=1

      FOR U%=0 TO 959
        undo_buffer&(frame%-1,undo_index%(frame%-1),U%)=GET(U% MOD 40,U% DIV 40+1)
      NEXT

      undo_index%(frame%-1)+=1
      IF undo_index%(frame%-1)>undo_max% THEN undo_index%(frame%-1)=0

      redo_count&(frame%-1)=0

      PROCdrawmenu

      ENDPROC

      REM ##########################################################
      REM save current screen to undo buffer
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
      PROCdrawmenu

      ENDPROC


      REM ##########################################################
      REM restore current undo buffer to screen
      DEF PROCundorestore
      LOCAL U%

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

      ENDPROC

      REM ##########################################################
      REM save redo screen
      DEF PROCredosave
      LOCAL U%

      IF redo_count&(frame%-1)<undo_max% THEN redo_count&(frame%-1)+=1

      FOR U%=0 TO 959
        redo_buffer&(frame%-1,redo_index%(frame%-1),U%)=GET(U% MOD 40,U% DIV 40+1)
      NEXT

      redo_index%(frame%-1)+=1
      IF redo_index%(frame%-1)>undo_max% THEN redo_index%(frame%-1)=0

      ENDPROC

      REM ##########################################################
      REM restore redo screen
      DEF PROCredorestore
      LOCAL U%

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

      ENDPROC

      REM ##########################################################
      REM copy region of current frame to copypaste buffer
      DEF PROCcopyregion(x1%,y1%,x2%,y2%)
      LOCAL s%,X%,Y%
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

      s%=0

      FOR X%=x1% TO x2%
        FOR Y%=y1% TO y2%
          copy_buffer&(s%)=GET(X%,Y%)
          s%+=1
        NEXT
      NEXT
      copyx%=x2%-x1%
      copyy%=y2%-y1%
      copysize%=s%

      copylockx%=x1%
      copylocky%=y1%

      ENDPROC

      REM ##########################################################
      REM paste copypaste buffer yo current frame
      DEF PROCpasteregion(x1%,y1%)
      LOCAL s%,X%,Y%,C%

      s%=0

      IF copylockxt% THEN x1%=copylockx%
      IF copylockyt% THEN y1%=copylocky%

      IF copysize%>0 THEN
        PROCundosave
        FOR X%=x1% TO x1%+copyx%
          FOR Y%=y1% TO y1%+copyy%
            IF X%<40 AND X%>-1 AND Y%<25 AND Y%>0 THEN
              C%=copy_buffer&(s%)
              IF spr_trns%=0 THEN
                VDU 31,X%,Y%,C%
              ELSE
                IF C%<>32 AND C%<>160 THEN VDU 31,X%,Y%,copy_buffer&(s%)
              ENDIF
            ENDIF
            s%+=1
          NEXT
        NEXT
      ENDIF

      ENDPROC

      REM ##########################################################
      REM menu buffer save frame
      DEF PROCmenusave
      LOCAL U%
      FOR U%=0 TO 959
        menu_buffer&(U%)=GET(U% MOD 40,U% DIV 40+1)
      NEXT

      ENDPROC

      REM ##########################################################
      REM menu buffer restore frame
      DEF PROCmenurestore

      IF menuext%<>0 THEN menuext%=0
      PROCframerestore(frame%)

      ENDPROC

      REM ##########################################################
      REM menu ext check
      DEF PROCmenucheck

      IF menuext%<>0 THEN
        menuext%=0
        PROCframerestore(frame%)
      ENDIF

      ENDPROC

      REM ##########################################################
      REM save sprite file
      DEF PROCsavespritefile(F$)
      LOCAL f%,u%,c%

      f%=OPENOUT(F$)
      FOR c%=0 TO sprite_max%-1
        FOR u%=0 TO 319
          BPUT#f%,sprite_buffer&(c%,u%)
        NEXT
      NEXT
      CLOSE#f%
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
          VDU 31,u% MOD 40,u% DIV 40,char%
        NEXT
        CLOSE#f%
      ENDIF
      ENDPROC

      REM ##########################################################
      REM loadfile - modified dirscan to include type array so files list can be displayed for mode 7
      REM loadtype determines the type of load / import function
      DEF PROCloadfile(loadtype%)
      LOCAL I%,N%,L%,F%,SEL%,SELOLD%,SELY%,INDEX%,INDEXOLD%,filetype$,fh%,MACT%,maxy%,opt1%,opt2%,GT%,OGT%

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

      PROCWAITMOUSE(0)
      REM PROCmenusave
      REM *** FRAMESAVE???
      menuext%=99
      CASE loadtype% OF
        WHEN 0 : maxy%=23
        WHEN 1 : maxy%=19
        WHEN 2 : maxy%=19
      ENDCASE
      REM      maxy%=22-loadtype%*3
      REM       PRINTTAB(0,1)STR$(maxy%)

      FOR L%=4 TO maxy%
        PROCprint40(L%,"")
      NEXT

      FOR L%=5 TO maxy%-1
        PRINTTAB(2,L%)gw$;CHR$(234);STRING$(30," ");gw$;CHR$(181);
      NEXT
      PRINTTAB(34,5)tg$;CHR$(94);
      PRINTTAB(34,16)tg$;"#";
      PRINTTAB(2,maxy%)gw$;CHR$(170);STRING$(31,CHR$(172));CHR$(165);
      PRINTTAB(5,18)tb$;CHR$(157);tc$;"LOAD  ";CHR$(156);

      CASE loadtype% OF
        WHEN 0 : REM bin files
          filetype$=".bin"
          PRINTTAB(2,4)gw$;CHR$(232);STRING$(10,CHR$(172));tg$;"LOAD FILE";gw$;STRING$(10,CHR$(172));CHR$(180);
          PRINTTAB(15,18)tr$;CHR$(157);ty$;"LOAD LAST SAVE ";gw$;CHR$(156);
          PRINTTAB(4,20)tg$;"(*)";tw$;"ALL FRMS ";tg$;"( )";tw$;"SINGLE FRM";
          PRINTTAB(4,21)tg$;"(*)";tw$;"CLS ";tg$;"( )";tw$;"BACK ";tg$;"( )";tw$;"FORE";
          PRINTTAB(4,22)tg$;"( )";tw$;"SERIES 78x72 : F0001.BMP"

        WHEN 1 : REM import bmp
          filetype$=".bmp"
          PRINTTAB(2,4)gw$;CHR$(232);STRING$(9,CHR$(172));tg$;"IMPORT FILE";gw$;STRING$(9,CHR$(172));CHR$(180);
          PRINTTAB(15,18)CHR$(156);
          PRINTTAB(1,21)tg$;"(*)";tw$;"SINGLE BOX CAPTURE"
          PRINTTAB(1,22)tg$;"( )";tw$;"GRID";tc$;"HOR";tw$;"-";ty$;"10";tw$;"+ ";tc$;"VER";tw$;"-";ty$;"02";tw$"+"
          PRINTTAB(1,23)tg$;"( )";tw$;"SERIES 78x72 FORMAT: F0001.BMP"
          GX%=10
          GY%=2

        WHEN 2 : REM import bmp to sprite
          filetype$=".bmp"
          PRINTTAB(2,4)gw$;CHR$(232);STRING$(8,CHR$(172));tg$;"SPRITE IMPORT";gw$;STRING$(8,CHR$(172));CHR$(180);
          PRINTTAB(15,18)CHR$(156);

      ENDCASE

      N% = FN_dirscan2(n$(), t&(), "dir *.*", filetype$)
      F%=0
      S%=0
      SEL%=0
      SELOLD%=0
      SELY%=-1
      INDEX%=1
      INDEXOLD%=1
      MACT%=-1

      FOR I%=INDEX% TO INDEX%+11
        IF I%<N%+1 THEN PRINTTAB(6,4+I%)CHR$(c%(t&(I%)));LEFT$(n$(I%),24);
      NEXT

      REPEAT
        PROCREADMOUSE

        REM detect first touch and movement
        IF MB%=4 THEN
          IF MACT%=-1 THEN MACT%=TY%

          IF MACT%>4 AND MACT%<17 THEN
            IF TX%>4 AND TX%<34 THEN
              IF TY%<>OLD_TY% THEN INDEX%-=SGN(TY%-OLD_TY%)
              IF INDEX%>N%-11 THEN INDEX%=N%-11
              IF INDEX%<1 THEN INDEX%=1
              IF SELY%=-1 THEN SELY%=TY%
            ENDIF
          ELSE
            IF TY%=22 AND loadtype%=1 THEN
              CASE TX% OF
                WHEN 15 : IF GX%>1 THEN GX%-=1

                WHEN 20 : IF GX%<20 THEN GX%+=1

                WHEN 27 : IF GY%>1 THEN GY%-=1

                WHEN 32 : IF GY%<20 THEN GY%+=1
              ENDCASE
              PRINTTAB(17,22)RIGHT$("0"+STR$(GX%),2)
              PRINTTAB(29,22)RIGHT$("0"+STR$(GY%),2)
              WAIT 20
            ENDIF
          ENDIF

        ENDIF

        REM detect touch release
        IF MB%=0 AND MACT%<>-1 THEN
          IF MACT%=5 AND TX%=35 AND INDEX%>1 THEN INDEX%-=1
          IF MACT%=16 AND TX%=35 AND INDEX%<N%-11 THEN INDEX%+=1
          IF SELY%=TY% AND MACT%>4 AND MACT%<17 AND TX%>3 AND TX%<36 THEN
            S%=TY%-5
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
                IF S%=0 THEN
                  N% = FN_dirscan2(n$(), t&(), "dir *.*",filetype$)
                  SEL%=0
                  SELOLD%=0
                  SELY%=-1
                  INDEX%=1
                  INDEXOLD%=-1
                  FOR I%=INDEX% TO INDEX%+12
                    PRINTTAB(6,4+I%)SPC(28);
                  NEXT

                ENDIF
              ENDIF
            ENDIF
          ENDIF

          REM check for button and control clicks
          IF TY%<1 THEN F%=-1

          REM load and cancel buttons
          IF TY%=18 AND MACT%=18 THEN
            IF TX%>5 AND TX%<14 THEN F%=SEL%
            IF TX%>15 AND TX%<34 AND loadtype%=0 THEN F%=-2
          ENDIF

          CASE loadtype% OF
            WHEN 0 :
              REM load frame options
              IF TY%=20 AND MACT%=20 THEN
                CASE TX% OF
                  WHEN 5,6,7 : opt1%=0
                  WHEN 19,20,21 : opt1%=1
                ENDCASE
                PRINTTAB(6,20)CHR$(42-opt1%*10)
                PRINTTAB(20,20)CHR$(32+opt1%*10)
              ENDIF

              REM load merge options
              IF TY%=21 AND MACT%=21 THEN
                CASE TX% OF
                  WHEN 5,6,7 : opt2%=0
                  WHEN 14,15,16 : opt2%=1
                  WHEN 24,25,26 : opt2%=2
                ENDCASE
                PRINTTAB(6,21)CHR$(32-(opt2%=0)*10)
                PRINTTAB(15,21)CHR$(32-(opt2%=1)*10)
                PRINTTAB(25,21)CHR$(32-(opt2%=2)*10)
              ENDIF

              REM import series
              IF TY%=22 AND MACT%=22 THEN
                CASE TX% OF
                  WHEN 5,6,7 : GT%=(GT%+1) AND 1
                ENDCASE
                IF GT%=0 THEN
                  filetype$=".bin"
                ELSE
                  filetype$=".bmp"
                ENDIF
                N% = FN_dirscan2(n$(), t&(), "dir *.*",filetype$)
                SEL%=0
                SELOLD%=0
                SELY%=-1
                INDEX%=1
                INDEXOLD%=-1
                FOR I%=INDEX% TO INDEX%+12
                  PRINTTAB(6,4+I%)SPC(28);
                NEXT

                PRINTTAB(6,22)CHR$(32+GT%*10)
              ENDIF


            WHEN 1 :

              REM grid size controls
              IF MACT%>20 THEN
                OGT%=GT%
                IF TY%=21 AND TX%>1 AND TX%<5 THEN GT%=0
                IF TY%=22 AND TX%>1 AND TX%<5 THEN GT%=1
                IF TY%=23 AND TX%>1 AND TX%<5 THEN GT%=2
                IF OGT%<>GT% THEN
                  PRINTTAB(3,21+GT%)"*"
                  PRINTTAB(3,21+OGT%)" "
                ENDIF
              ENDIF

          ENDCASE
          SELY%=-1
          MACT%=-1
        ENDIF

        REM IF SCROLLING DETECTED UPDATE FILE LIST AND SELECTED FILE INDEX
        IF INDEX%<>INDEXOLD% OR SELOLD%<>SEL% THEN
          FOR I%=0 TO 11
            K%=I%+INDEX%
            IF K%<N%+1 THEN
              PRINTTAB(4,I%+5)SPC(28)
              VDU 31,4,I%+5
              IF SEL%=K% THEN
                VDU 132,157
              ELSE
                VDU 32,32
              ENDIF
              PRINTCHR$(c%(t&(K%)));LEFT$(n$(K%),24);
              IF SEL%=K% THEN VDU 32,32,156
            ENDIF
          NEXT
          SELOLD%=SEL%
          INDEXOLD%=INDEX%
        ENDIF

        REM PRINTTAB(0,1)STR$(SEL%)

        OLD_TY%=TY%

        WAIT 2
      UNTIL F%<>0

      CASE loadtype% OF
        WHEN 0 : REM load bin
          PROCmenurestore
          REM *** DRAWFRAME??

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
                menuext%=0

              ELSE
                COLOUR 9
                PRINTTAB(0,0)"FILE NAME NOT CORRECT: E.G. F0001.BMP"
                menuext%=94
              ENDIF

            ENDIF
          ENDIF

        WHEN 1 : REM import bmp
          MODE 6
          VDU 23,1,0;0;0;0; : REM Disable cursor

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
          MODE 6
          VDU 23,1,0;0;0;0; : REM Disable cursor

          IF F%=-1 THEN
            COLOUR 9
            PRINTTAB(0,0)"NO FILE LOADED"
            menuext%=94
          ELSE

            REM OSCLI "DISPLAY """+curdir$+n$(SEL%)+""" 0,0"


            OSCLI "LOAD """+curdir$+n$(SEL%)+""" "+STR$~import_buffer%+" +"+STR$1000000

            REM PRINTTAB(0,0)"LOAD """;curdir$+n$(SEL%);""" ";STR$~import_buffer%;" +";STR$1000000
            REM bmp filetype (2 bytes)
            REM PRINT"Type:";CHR$(import_buffer%?0);CHR$(import_buffer%?1)
            T$=CHR$(import_buffer%?0)+CHR$(import_buffer%?1)
            REM bmp filesize (4 bytes) ?2 ?3 ?4 ?5
            REM bmp reserved (2 bytes) ?6 ?7
            REM bmp reserved (2 bytes) ?8 ?9
            REM bmp pixel data offset (4 bytes)
            REM PRINT"pOfs:";STR$(import_buffer%!10)
            spr_impofs%=import_buffer%!10

            REM bmp header size (4 bytes)
            REM PRINT"hSze:";STR$(import_buffer%!14)

            REM bmp image width (4 bytes)
            REM PRINT"iWid:";STR$(import_buffer%!18)
            spr_impwid%=import_buffer%!18

            REM bmp image height (4 bytes)
            REM PRINT"iHgt:";STR$(import_buffer%!22)
            spr_imphgt%=import_buffer%!22
            REM bmp planes (2 bytes) ?26 ?27
            REM bmp but per pixel (2 bytes)
            REM PRINT"bpp: ";STR$(import_buffer%?29);STR$(import_buffer%?28)
            spr_impbpp%=import_buffer%?28+(import_buffer%?29*256)

            REM bmp compression (4 bytes) ?30 ?31 ?32 ?33
            REM bmp image size (4 bytes) ?34 ?35 ?36 ?37
            REM bmp x pixels per meter (4 bytes) ?38 ?39 ?40 ?41
            REM bmp y pixels per meter (4 bytes) ?42 ?43 ?44 ?45
            REM bmp total colours (4 bytes) ?46 ?47 ?48 ?49
            REM bmp important colours (4 bytes) ?50 ?51 ?52 ?53
            IF T$<>"BM" OR spr_impofs%<>54 OR spr_impbpp%<>24 THEN
              PRINTTAB(0,0)"Image format not supported, must be BMP 24bpp"
              PROCWAITMOUSE(4)
              PROCWAITMOUSE(0)
              menuext%=94
            ELSE
              OSCLI "MDISPLAY "+STR$~import_buffer%
              menuext%=95
            ENDIF


          ENDIF


      ENDCASE

      ENDPROC

      REM ##########################################################
      REM save all frames to file, create session folder if not already exists
      DEF PROCsavefile
      PROCWAITMOUSE(0)

      PROCframesave(frame%)
      LOCAL D$,M$,T$,TMP$

      M$="JanFebMarAprMayJunJulAugSepOctNovDec"

      REM build date format: YYYYMMDDHHMMSS
      T$=TIME$
      TMP$=STR$(INSTR(M$,MID$(T$,8,3)) DIV 3+1)
      TMP$=RIGHT$("0"+TMP$,2)
      D$=MID$(T$,12,4)+TMP$+MID$(T$,5,2)+"_"+MID$(T$,17,2)+MID$(T$,20,2)+MID$(T$,23,2)

      REM create and change to session folder, strip off seconds value
      IF session%=0 THEN
        TMP$="M7_"+LEFT$(D$,LEN(D$)-2)
        OSCLI "MD """+cursave$+TMP$+""""
        OSCLI "CD """+cursave$+TMP$+""""
        session%=1
        cursave$=cursave$+TMP$+"/"
      ENDIF

      REM update last session file
      f%=OPENOUT(@dir$+"telepaint_pref.ini")
      IF f% THEN
        PRINT#f%,cursave$+"M7_" + D$
        CLOSE#f%
      ENDIF

      REM turn off grid and save state
      OG%=gridshow%
      gridshow%=0
      CLS

      REM save frames
      frame%=frame_max%
      FOR I%=1 TO frame_max%
        PROCloadnextframe(1,0)
        PROCsavebinaryfile(cursave$+"M7_" + D$ + "_" + STR$(frame%)+".BIN")
        OSCLI "SCREENSAVE """+cursave$+"M7_" + D$ + "_" + STR$(frame%)+".BMP"" 0,0,1280,1000"
        WAIT 10
      NEXT

      gridshow%=OG%

      PROCloadnextframe(1,0)

      REM PROCmenusave
      REM *** FRAMESAVE???
      menuext%=99
      PRINTTAB(9,10)gw$;CHR$(232);STRING$(18,CHR$(172));CHR$(180);CHR$(144+curcol%);
      FOR L%=11 TO 13
        PRINTTAB(9,L%)gw$;CHR$(234);STRING$(17," ");gw$;CHR$(181);CHR$(144+curcol%);
      NEXT
      PRINTTAB(9,14)gw$;CHR$(170);STRING$(18,CHR$(172));CHR$(165);CHR$(144+curcol%);

      REM READ FILES
      PRINTTAB(13,12)tg$;"FILE SAVED!";

      PROCWAITMOUSE(4)

      PROCWAITMOUSE(0)

      PROCmenurestore
      REM *** DRAWFRAME??

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
      MODE 7
      frame%=0
      PROCloadnextframe(1,0)
      menuext%=0

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

          OSCLI "LOAD """+curdir$+NAME$+""" "+STR$~import_buffer%+" +"+STR$1000000

          T$=CHR$(import_buffer%?0)+CHR$(import_buffer%?1)
          spr_impofs%=import_buffer%!10
          spr_impwid%=import_buffer%!18
          spr_imphgt%=import_buffer%!22
          spr_impbpp%=import_buffer%?28+(import_buffer%?29*256)

          REM adjust for correct line byte width multiple of 4
          line_wid%=spr_impwid%*3
          WHILE line_wid% MOD 4<>0
            line_wid%+=1
          ENDWHILE

          REM        PRINTTAB(0,0)NAME$;"  F:";STR$(F%);"  ";
          REM        PRINTT$
          REM        PRINT"pOfs:";STR$(spr_impofs%)
          REM        PRINT"pWid:";STR$(spr_impwid%)
          REM        PRINT"pHgt:";STR$(spr_imphgt%)
          REM        PRINT"pBpp:";STR$(spr_impbpp%)
          REM        PROCWAITMOUSE(4)

          IF T$="BM" AND spr_impofs%=54 AND spr_impbpp%=24 THEN
            FOR X%=0 TO 77
              FOR Y%=0 TO 71
                col%=0
                IF X%>-1 AND X%<spr_impwid% AND Y%>-1 AND Y%<spr_imphgt% THEN
                  ofs%=spr_impofs%+X%*3+Y%*line_wid%
                  col%=import_buffer%?ofs%+import_buffer%?(ofs%+1)+import_buffer%?(ofs%+2)
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
        line_wid%=spr_impwid%*3
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
                  IF X%>-1 AND X%<spr_impwid% AND Y%>-1 AND Y%<spr_imphgt% THEN
                    ofs%=spr_impofs%+X%*3+Y%*line_wid%
                    col%=import_buffer%?ofs%+import_buffer%?(ofs%+1)+import_buffer%?(ofs%+2)
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
            PROCprint40(0,"Complete! Process next sprit?  Y   N")
            GCOL 3,10
            RECTANGLE FILL 948,960,108,40

            GCOL 3,9
            RECTANGLE FILL 1076,960,108,40

            PROCWAITMOUSE(4)
            PROCWAITMOUSE(0)

            IF TY%=0 AND TX%>29 AND TX%<33 THEN

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
      MODE 7
      menuext%=2


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

      REM Convert to upper case:
      DEF FNUPPER(a$) IF LENa$=0 THEN =""
      LOCAL p%%
      FOR p%% = PTR(a$) TO PTR(a$)+LENa$-1
        IF ?p%% >= 97 IF ?p%% <= 122 ?p%% -= 32
      NEXT
      = a$

      REM ##########################################################
      REM display a help screen
      DEF PROCshowhelp

      PROCprint40(24,ty$+"TelePaint"+tm$+version$+tc$+"by 4thStone & Pixelblip")

      OSCLI "SCREENSAVE """+@tmp$+"M7_TMP.BMP"" 0,0,1280,1000"
      MODE 6
      OSCLI "DISPLAY """+@tmp$+"M7_TMP.BMP"" 0,0"

      VDU 23,1,0;0;0;0; : REM Disable cursor

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

      REPEAT
        PROCREADMOUSE
        WAIT 2
        REM        PRINTTAB(4,18)LEFT$(STR$(TX%)+"   ",4)
        REM PRINTTAB(4,19)LEFT$(STR$(TY%)+"   ",4)
      UNTIL MB%=4
      PROCWAITMOUSE(0)
      MODE 7
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
      REM change to mode 6 and overlay control codes on current screen
      DEF PROCcontrolcodes

      showcodes%=0
      PROCframesave(frame%)
      OSCLI "SCREENSAVE """+@tmp$+"M7_TMP.BMP"" 0,0,1280,1000"
      MODE 6
      OSCLI "DISPLAY """+@tmp$+"M7_TMP.BMP"" 0,0"
      GCOL 3,8

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

        REM show grid
        IF x%>0 THEN
          GCOL 3,8
          LINE x%*32,0,x%*32,999
          IF x%<25 THEN LINE 0,x%*40,1279,x%*40

        ENDIF

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

      REPEAT
        PROCREADMOUSE
      UNTIL MB%=4
      PROCWAITMOUSE(0)
      MODE 7
      frame%-=1
      PROCloadnextframe(1,0)

      ENDPROC

      REM ##########################################################
      REM DRAW SPRITE EDITOR SCREEN
      DEF PROCspritemenu(R%)

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
        PRINTTAB(10,19)tb$;CHR$(157);tc$;"< ";CHR$(156);tb$;CHR$(157);tc$;"> ";CHR$(156);

        PRINTTAB(0,21)tb$;CHR$(157);tc$;"DRAW ALL  ";CHR$(156);tm$;CHR$(157);ty$;"UNDO ALL  ";CHR$(156);tr$;CHR$(157);ty$;"CLOSE  ";CHR$(156);

        PRINTTAB(0,23)tb$;"FRAME";tc$;"START";tw$;"-";ty$;"   ";tw$;"+";tc$;" STEP";tw$;"-";ty$;"   ";tw$;"+";
        PRINTTAB(0,24)tc$;"X";tw$;"-";ty$;"  ";tw$;"+"tc$;"Y";tw$;"-";ty$;"  ";tw$;"+"tc$;"H";tw$;"-";ty$;"  ";tw$;"+"tc$;"V";tw$;"-";ty$;"  ";tw$;"+";

        PRINTTAB(0,2)ty$;"LOAD";
        PRINTTAB(0,4)ty$;"SAVE";
        PRINTTAB(0,6)tg$;"ADD L";
        PRINTTAB(0,8)tg$;"VIEW L";
        PRINTTAB(0,10)tg$;"CLR L";
        PRINTTAB(0,12)ty$;"IMPRT";
        PRINTTAB(0,14)tc$;"COPY";
        PRINTTAB(0,16)tc$;"PASTE";

        PRINTTAB(32,2)tc$;"CLS";
        PRINTTAB(32,4)tc$;"SCR-L";
        PRINTTAB(32,6)tc$;"SCR-R";
        PRINTTAB(32,8)tc$;"SCR-U";
        PRINTTAB(32,10)tc$;"SCR-D";
        PRINTTAB(32,12)tc$;"FLP-]";
        PRINTTAB(32,14)tc$;"FLP-^";
        PRINTTAB(32,16)tc$;"CPY >";
        PRINTTAB(32,18)tc$;"CPY <";

        REM PROCdrawspritegrid

        toolsel%=1:toolcursor%=15

      ENDIF

      REM REFRESH DYNAMIC AREAS
      PRINTTAB(22,19)ty$;CHR$(157);tr$;RIGHT$(" "+STR$(sprite_cur%+1),2)+" ";CHR$(156);CHR$(151)
      p$="CHR"
      IF spr_scroll%=0 THEN p$="PIX"
      PRINTTAB(36,2)tg$;p$;
      PRINTTAB(0,18)CHR$(129+spr_trns%);"TRANS";
      PRINTTAB(1,7)tb$;RIGHT$("000"+STR$(spr_lstcount%+1),4);

      PRINTTAB(15,23)RIGHT$("00"+STR$(spr_frmstart%),3);
      PRINTTAB(29,23)RIGHT$("00"+STR$(spr_frmstep%),3);
      PRINTTAB(5,24)RIGHT$("0"+STR$(spr_frmx%),2);
      PRINTTAB(14,24)RIGHT$("0"+STR$(spr_frmy%),2);
      PRINTTAB(23,24)RIGHT$("0"+STR$(spr_frmh%),2);
      PRINTTAB(32,24)RIGHT$("0"+STR$(spr_frmv%),2);

      PROCdrawsprite

      ENDPROC

      REM ##########################################################
      REM shape and special sub menu
      DEF PROCspecialmenu(R%)

      REM REDRAW EVERYTHING
      IF R%=1 THEN

        FOR Y%=1 TO 23
          PROCprint40(Y%,"")
        NEXT

        PROCprint40(2,tg$+"( )"+tw$+"LINE     "+tg$+"( )"+tw$+"ANIM"+tb$+"(LINE AND RECT)")
        PROCprint40(3,tg$+"( )"+tw$+"RECT         "+tc$+"GAP:"+tw$+"-"+ty$+" "+tw$+"+"+tc$+"LEN:"+tw$+"-"+ty$+" "+tw$+"+")
        PROCprint40(4,tg$+"( )"+tw$+"CIRC     "+tg$+"( )"+tw$+"SHOW GRID")
        PROCprint40(6,tg$+"( )"+tw$+"FLSH (136)  "+tg$+"( )"+tw$+"DBLH (141)")
        PROCprint40(7,tg$+"( )"+tw$+"SEPR (154)  "+tg$+"( )"+tw$+"HOLD (158)")
        PROCprint40(9,tg$+"( )"+tw$+"FORE "+tg$+"( )"+tw$+"BACK"+tb$+"(ENTIRE COLUMN)")
        PROCprint40(10,tg$+"( )"+tw$+"HORZ "+tg$+"( )"+tw$+"VERT"+tb$+"(LOCK PASTE POS)")
        D$=CHR$(129+showcodes%)

        REM PRINTTAB(35,1)tm$;"HELP"
        PROCprint40(11,tg$+"( )"+tw$+"TEXT")
        PROCprint40(16,"  , . ` ~ ! @ # $ % ^ & * ( ) - _ = +")
        PROCprint40(18,"  [ ] ; { } \ | : ' "" < > / ?"+tc$+"SPC CAP" )

        PRINTTAB(1,22)tb$;CHR$(157);ty$;"SPRITES  ";CHR$(156);"  ";tm$;CHR$(157);ty$;"HELP  ";CHR$(156);"  ";tr$;CHR$(157);ty$;"CLOSE  ";CHR$(156)

        PROCprint40(24,ty$+"TelePaint"+tm$+version$+tc$+"by 4thStone & Pixelblip")
      ENDIF

      REM REFRESH DYNAMIC AREAS
      FOR I%=0 TO 9
        IF shapesel%=I% THEN
          D$="*"
        ELSE
          D$=" "
        ENDIF
        PRINTTAB(sopt{(I%)}.x%,sopt{(I%)}.y%)D$
      NEXT

      D$=CHR$(32+animateshape%*10)
      A$=STR$(animategap%)
      F$=STR$(animatelen%)
      G$=CHR$(32+gridshow%*10)
      R$=CHR$(32+copylockxt%*10)
      U$=CHR$(32+copylockyt%*10)

      PRINTTAB(16,2)D$;
      PRINTTAB(26,3)A$;
      PRINTTAB(37,3)F$;
      PRINTTAB(16,4)G$;
      PRINTTAB(2,10)R$;
      PRINTTAB(12,10)U$;

      IF caps% THEN
        PROCprint40(12,"  A B C D E F G H I J K L M N O P Q R")
        PROCprint40(14,"  S T U V W X Y Z 1 2 3 4 5 6 7 8 9 0")
      ELSE
        PROCprint40(12,"  a b c d e f g h i j k l m n o p q r")
        PROCprint40(14,"  s t u v w x y z 1 2 3 4 5 6 7 8 9 0")
      ENDIF

      PROCprint40(20,"TEXT:"+tg$+text$)
      PRINTTAB(36,20)tr$;"< X";

      ENDPROC

      REM ##########################################################
      REM UPDATE COLOUR STRIP FOR BUFFER
      DEF PROCGR_BUF(D%,F%,B%)

      REM ADD GRAPHICS CODE TO LEFT SIDE OF CANVAS
      FOR Y%=0 TO 23
        IF B% THEN
          frame_buffer&(D%-1,Y%*40)=144+B%
          frame_buffer&(D%-1,Y%*40+1)=157
          frame_buffer&(D%-1,Y%*40+2)=144+F%
        ELSE
          frame_buffer&(D%-1,Y%*40)=144+F%
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
      REM INITIALISE THE SCREEN
      DEF PROCGR(F%,B%,C%)

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
      DEF PROCdrawmenu
      LOCAL A$,D$,E$,F$,R$,U$,P$,c%

      REM create palette with current colour as G(70) or T(84)
      c%=184-textfore%*13
      FOR count%=1 TO 7
        PRINTTAB(count%*2-2,0) CHR$(128+count%);CHR$(255+(count%=curcol%)*c%);
      NEXT count%

      REM format main menu
      A$=CHR$(135-animation%*5)
      D$=STR$(dither%+1)
      E$=CHR$(135-erase%*5)
      R$=CHR$(130+(redo_count&(frame%-1)=0))
      U$=CHR$(130+(undo_count&(frame%-1)=0))
      F$=RIGHT$("0"+STR$(frame%),2)
      P$=CHR$(67+copypaste%*13)
      PRINTTAB(14,0)tw$;"P";D$;P$;"FS";E$;"E";U$;"U";R$;"R";tw$;"CBF LS";A$;"A";tw$;F$;">P"

      ENDPROC


      REM ***********************************************************************
      REM LIB FUNCTIONS - imported instead of INSTALL
      REM ***********************************************************************

      REM Scan a directory and return list of directory and file names
      REM Modified to return a type list instead of prefixing folder / file icon
      DEF FN_dirscan2(name$(), type&(), dircmd$, filter$)

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
            d$ = name$(0)
            C% = FN_instrr(d$, "/", 0) : IF C% IF MID$(d$, C% - 1, 1) = "/" C% -= 1
            IF C% = 0 C% = FN_instrr(d$, "\", 0) : IF MID$(d$, C% - 1, 1) = "\" C% -= 1
            name$(0) = MID$(d$, 14, C% - 13)
            curdir$=name$(0)
            N% += 1 : REM Zeroth index holds directory name
          ELSE
            name$(N%) = FN_trim(name$(N%))
            d$ = FN_lower(name$(N%))
            ON ERROR LOCAL IF FALSE THEN
              OSCLI "cd """ + name$(0) + name$(N%) + """"
              OSCLI "cd """ + name$(0) + """"
              IF d$<>"." IF d$<>".." IF filter$="" OR ASCd$<>&2E type&(N%) = 1 : N% += 1
            ELSE
              I% = INSTR(d$,".")
              IF filter$="" OR INSTR(filter$,MID$(d$,I%)) type&(N%) = 2 : N% += 1
            ENDIF : RESTORE ERROR
          ENDIF
          name$(N%) = MID$(a$,3)
        ELSE
          name$(N%) += a$
        ENDIF
      UNTIL EOF#F% OR N% >= DIM(name$(),1)
      CLOSE #F%

      IF N% < DIM(name$(),1) name$(N%) = "@lib$" : type&(N%) = 0 : SWAP type&(N%),type&(2):SWAP name$(N%),name$(2):N% += 1
      IF N% < DIM(name$(),1) name$(N%) = "@usr$" : type&(N%) = 0 : SWAP type&(N%),type&(3):SWAP name$(N%),name$(3):N% += 1
      name$(N%) = ".." : type&(N%) = 0 : SWAP type&(N%),type&(1):SWAP name$(N%),name$(1): N% += 1

      REM name$(N%) = "TEST_FILE" : type&(N%) = 2

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

      REM E.G. YOUR CODE
      FOR X%=2 TO 78
        PROCpoint(X%,20+SIN(X%/8)*8,1)
      NEXT

      ENDPROC

      REM ##########################################################
      REM USER CUSTOMIZABLE PROCEDURE1
      DEF PROCCUSTOMPROC2

      REM E.G. YOUR CODE
      FOR X%=2 TO 78
        PROCpoint(X%,40+COS(X%/8)*8,1)
      NEXT

      ENDPROC


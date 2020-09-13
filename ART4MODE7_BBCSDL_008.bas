      MODE 7

      REM *** TODO LIST ***

      REM *** INKEY(-256) MATRIX BRANYDY &4D (INCLUDE OTHERS)

      REM *** INVESTIGATE LOCAL VERSIONS OF LIBRARIES TO MAKE PROGRAM CROSS BASIC COMPATIBLE (DONE?? Need Soruk to Test)

      REM *** LOAD SCREEN SORT BY NEWEST, NEEDS WORK!

      REM *** COPY AND PASTE

      REM *** IMPLEMENT ANIMATED CIRCLE

      REM *** CONTROL CODE SUB MENU TO ALLOW ALL AVAILABLE CONTROL CODES TO BE PLOTTED (IN PROGRESS)

      REM *** TEXT INSERT SUB MENU, PROBABLY ADD TO CONTROL CODE SUB MENU

      REM *** ADD VERSION NUMBER IN SUB MENU

      REM *** IMAGE CONVERTER FOR IMPORTING BMP FILE

      REM *** TODO LIST ***

      REM FOR 64 BIT COMPARISONS, ESC OFF FOR BACK ARROW ON SOME DEVICES
      REM *HEX 64
      *ESC OFF

      REM INSTALL @lib$+"sortlib"
      REM INSTALL @lib$+"stringlib"
      REM INSTALL @lib$+"dlglib"
      REM INSTALL @lib$+"filedlg"

      MOUSE ON 3

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
      DIM fill{(100) x%,y%}

      REM frame buffer
      frame_max%=8
      DIM frame_buffer&(frame_max%,959)

      REM undo buff
      undo_max%=19
      DIM undo_buffer&(frame_max%,undo_max%,959)
      DIM undo_index%(frame_max%)
      DIM undo_count&(frame_max%)

      REM redo buffer
      DIM redo_buffer&(frame_max%,undo_max%,959)
      DIM redo_index%(frame_max%)
      DIM redo_count&(frame_max%)


      REM menu buffer
      DIM menu_buffer&(959)

      REM OLD PIXEL & MOUSE COORDS
      OLD_PX%=0
      OLD_PY%=0
      OLD_MX%=0
      OLD_MY%=0
      OLD_TX%=0
      OLD_TY%=0

      REM MOUSE COORDS
      MX%=0
      MY%=0
      MB%=0

      REM TEXT & PIXEL COORDS FOR CURRENT MOUSE READ LOCATION
      TX%=0
      TY%=0
      PX%=0
      PY%=0

      REM TOOL VARS
      curcol%=7
      bakcol%=0
      toolsel%=1
      shapesel%=1
      toolcursor%=15
      animateshape%=0
      animategap%=0
      animategapcount%=0
      animatelen%=1
      animatelencount%=0
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

      PROCGR(curcol%,bakcol%,1)
      PROCdrawmenu

      FOR frame%=1 TO frame_max%
        PROCframesave(frame%)
        REM WAIT 10
      NEXT frame%
      frame%=1

      REM =====================
      REM main loop starts here
      REPEAT

        PROCREADMOUSE

        IF MX%<>OLD_MX% OR MY%<>OLD_MY% OR MB% THEN
          IF TY%=0 THEN
            REM CLICK INSIDE MENU AREA
            IF MB%=4 THEN
              PROCWAITMOUSE(0)
              IF TY%=0 THEN
                CASE TX% OF
                  WHEN 0: REM OPTIONS MENU
                  WHEN 1,2: curcol%=1:PROCmenurestore : REM RED
                  WHEN 3,4: curcol%=2:PROCmenurestore : REM GREEN
                  WHEN 5,6: curcol%=3:PROCmenurestore : REM YELLOW
                  WHEN 7,8: curcol%=4:PROCmenurestore : REM BLUE
                  WHEN 9,10: curcol%=5:PROCmenurestore : REM MAGENTA
                  WHEN 11,12: curcol%=6:PROCmenurestore : REM CYAN
                  WHEN 13,14: curcol%=7:PROCmenurestore : REM WHITE

                  WHEN 15: toolsel%=1:toolcursor%=TX% : REM PAINT
                  WHEN 16: toolsel%=2:toolcursor%=TX% : REM DITHER
                  WHEN 17: dither%=(dither%+1) MOD 6:toolsel%=2:toolcursor%=16:PROCmenurestore : REM DITHER SCALE
                  WHEN 18: toolsel%=3:toolcursor%=TX% : REM FILL
                  WHEN 19: toolsel%=4:toolcursor%=TX% : REM SHAPE MENU
                    IF menuext%<>1 THEN
                      IF menuext%>0 THEN PROCmenurestore
                      PROCmenusave
                      menuext%=1
                    ELSE
                      PROCmenurestore
                    ENDIF

                  WHEN 21: erase%=(erase%+1) AND 1:PROCmenurestore : REM TOGLE ERASER

                  WHEN 23: PROCmenurestore:PROCundorestore : REM UNDO
                  WHEN 25: PROCmenurestore:PROCredorestore : REM REDO

                  WHEN 27: PROCmenurestore:PROCclearscreen:toolsel%=1:toolcursor%=15 : REM CLEARSCREEN
                  WHEN 28: toolsel%=5:toolcursor%=TX% : REM BACKGROUND COLOUR
                  WHEN 29: toolsel%=6:toolcursor%=TX% : REM FORGROUND AND CONTROL CODE MENU

                  WHEN 31: PROCmenurestore:PROCloadfile : REM load file dialog
                  WHEN 32: PROCmenurestore:PROCsavefile : REM save frames to file

                  WHEN 34: animation%=(animation%+1) AND 1:PROCmenurestore : REM TOGLE ANIMATION FRAME ADVANCE TOOL

                  WHEN 36: REM frame%
                  WHEN 37: PROCmenurestore:PROCloadnextframe(-1,1) : REM SAVE CURRENT FRAME AND LOAD PREVIOUS FRAME
                  WHEN 38: PROCmenurestore:PROCloadnextframe(1,1) : REM SAVE CURRENT FRAME AND LOAD NEXT FRAME
                  WHEN 39: PROCmenurestore:PROCplay : REM SAVE CURRENT FRAME AND PLAY ALL FRAMES FROM FRAME 1


                ENDCASE

                REM HIDE SHAPE MENU IF ANOTHER TOOL IS SELECTED
                IF toolsel%<>4 AND menuext%=1 THEN PROCmenurestore

                PROCdrawmenu
              ENDIF
            ENDIF

          ELSE
            REM CHECK IF EXTENDED MENU IS ACTIVE
            IF menuext%=1 AND TY%=1 THEN
              IF MB%=4 THEN
                CASE TX% OF
                  WHEN 0: shapesel%=1 : REM LINE
                  WHEN 1: shapesel%=2 : REM RECTANGLE
                  WHEN 2: shapesel%=3 : REM CIRCEL

                  WHEN 4: animateshape%=(animateshape%+1) AND 1 : REM ANIMATEDSHAPE
                  WHEN 11:  REM ANIMATED GAP DECREMENT
                    animategap%-=1
                    IF animategap%<0 THEN animategap%=0
                  WHEN 15:  REM ANIMATED GAP INCREMENT
                    animategap%+=1
                    IF animategap%>5 THEN animategap%=5
                  WHEN 22:  REM ANIMATED LEN DECREMENT
                    animatelen%-=1
                    IF animatelen%<1 THEN animatelen%=1
                  WHEN 26:  REM ANIMATED LEN INCREMENT
                    animatelen%+=1
                    IF animatelen%>5 THEN animatelen%=5

                ENDCASE

                PROCWAITMOUSE(0)
                PROCdrawmenu

              ENDIF

            ELSE

              REM IF toolsel%=5 OR toolsel%=6 THEN
              REM A$=STR$(GET(TX%,TY%) AND 255)
              REM IF LEN(A$)<3 THEN A$=STRING$(3-LEN(A$)," ")+A$
              REM VDU 23,1,0;0;0;0; : REM Disable cursor
              REM PRINTTAB(37,0)A$;
              REM VDU 23,1,1;0;0;0; : REM Enable cursor
              REM ENDIF

              REM CLICK INSIDE DRAWING AREA
              CASE MB% OF

                WHEN 1:
                  REM PLACE HOLDER FOR RIGHT MOUSE

                WHEN 2:
                  REM PLACE HOLDER FOR MIDDLE MOUSE

                WHEN 4:
                  REM LEFT MOUSE CLICK OR TOUCH SCREEN CLICK
                  IF menuext%>0 THEN PROCmenurestore

                  CASE toolsel% OF
                    WHEN 1: REM PAINT TOOL
                      PROCundosave
                      PROCpoint(PX%,PY%,1)
                      REPEAT
                        PROCREADMOUSE
                        IF PX%<>OLD_PX% OR PY%<>OLD_PY% THEN PROCpoint(PX%,PY%,1-erase%)
                        OLD_PX%=PX%
                        OLD_PY%=PY%
                      UNTIL MB%=0
                      IF animation% THEN PROCloadnextframe(1,1)

                    WHEN 2: REM DITHER TOOL
                      PROCundosave
                      D%=2^(dither%)
                      DA%=2
                      IF dither%=2 THEN DA%=4
                      IF dither%=3 THEN DA%=8

                      X%=(PX% DIV DA%)*DA%
                      Y%=(PY% DIV DA%)*DA%
                      IF dither%<4 THEN
                        PROCpoint(X%,Y%,1-erase%)
                        PROCpoint(X%+D%,Y%+D%,1-erase%)
                      ELSE
                        IF TX%>0 AND TX%<40 AND TY%>0 AND TY%<25 THEN
                          CASE dither% OF
                            WHEN 4 : char%=255+(erase%=1)*95 : REM SOLID BLOCK #255
                            WHEN 5 : char%=154-erase% : REM SEPARATED GRAPHICS #154 , CONTIGUOUS $153

                          ENDCASE
                          VDU 31,TX%,TY%,char%
                        ENDIF
                      ENDIF
                      REPEAT
                        PROCREADMOUSE
                        IF PX%<>OLD_PX% OR PY%<>OLD_PY% THEN
                          IF dither%<4 THEN
                            X%=(PX% DIV DA%)*DA%
                            Y%=(PY% DIV DA%)*DA%
                            PROCpoint(X%,Y%,1-erase%)
                            PROCpoint(X%+D%,Y%+D%,1-erase%)
                          ELSE
                            IF TX%>0 AND TX%<40 AND TY%>0 AND TY%<25 THEN
                              CASE dither% OF
                                WHEN 4 : char%=255+(erase%=1)*95 : REM SOLID BLOCK #255
                                WHEN 5 : char%=154-erase% : REM SEPARATED GRAPHICS #154 , CONTIGUOUS $153

                              ENDCASE
                              VDU 31,TX%,TY%,char%
                            ENDIF
                          ENDIF
                        ENDIF
                        OLD_PX%=PX%
                        OLD_PY%=PY%
                      UNTIL MB%=0
                      IF animation% THEN PROCloadnextframe(1,1)

                    WHEN 3: REM FILL TOOL
                      PROCundosave
                      PROCfloodfill(PX%,PY%)
                      REPEAT
                        PROCREADMOUSE
                        WAIT 2
                      UNTIL MB%=0
                      IF animation% THEN PROCloadnextframe(1,1)

                    WHEN 4: REM SHAPE TOOLS
                      CASE shapesel% OF
                        WHEN 1: REM LINE TOOL
                          PROCundosave
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

                          PROCbresenham(startx%,starty%,PX%,PY%,2)
                          IF animateshape% THEN
                            oldframe%=frame%
                            PROCframesave(frame%)
                            PROCbresenham_buf(startx%,starty%,PX%,PY%,1-erase%)
                            frame%=oldframe%-1
                            PROCloadnextframe(1,0)
                          ELSE
                            PROCbresenham(startx%,starty%,PX%,PY%,1-erase%)
                            IF animation% THEN PROCloadnextframe(1,1)
                          ENDIF

                        WHEN 2: REM RECTANGLE TOOL
                          PROCundosave
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
                          PROCrectangle(startx%,starty%,PX%,PY%,2)
                          IF animateshape%=1 THEN
                            oldframe%=frame%
                            PROCframesave(frame%)
                            PROCrectangle_buf(startx%,starty%,PX%,PY%,1-erase%)
                            frame%=oldframe%-1
                            PROCloadnextframe(1,0)
                          ELSE
                            PROCrectangle(startx%,starty%,PX%,PY%,1-erase%)
                            IF animation% THEN PROCloadnextframe(1,1)
                          ENDIF

                        WHEN 3: REM CIRCLE TOOL
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
                          PROCcircle(startx%,starty%,startx%-PX%,1-erase%)
                          IF animation% THEN PROCloadnextframe(1,1)

                      ENDCASE

                    WHEN 5: REM BACKGROUND COLOUR
                      PROCundosave
                      IF TX%<39 AND TX%>-1 AND TY%>0 AND TY%<25 THEN VDU 31,TX%,TY%,(curcol%+144),157-erase%
                      REPEAT
                        PROCREADMOUSE
                        IF TX%<>OLD_TX% OR TY%<>OLD_TY% THEN
                          IF TX%<39 AND TX%>-1 AND TY%>0 AND TY%<25  VDU 31,TX%,TY%,(curcol%+144),157-erase%
                        ENDIF
                        OLD_TX%=TX%
                        OLD_TY%=TY%
                      UNTIL MB%=0
                      IF animation% THEN PROCloadnextframe(1,1)
                    WHEN 6: REM FORGROUND COLOUR
                      PROCundosave
                      IF TX%<40 AND TX%>-1 AND TY%>0 AND TY%<25 THEN VDU 31,TX%,TY%,(curcol%+144)
                      REPEAT
                        PROCREADMOUSE
                        IF TX%<>OLD_TX% OR TY%<>OLD_TY% THEN
                          IF TX%<40 AND TX%>-1 AND TY%>0 AND TY%<25 THEN VDU 31,TX%,TY%,(curcol%+144)
                          OLD_TX%=TX%
                          OLD_TY%=TY%
                        ENDIF
                      UNTIL MB%=0
                      IF animation% THEN PROCloadnextframe(1,1)


                  ENDCASE


              ENDCASE

            ENDIF
          ENDIF

        ELSE
          IF INKEY(-26) THEN PROCWAITNOKEY(-26) : PROCloadnextframe(-1,1) : REM SAVE CURRENT FRAME AND LOAD PREVIOUS FRAME
          IF INKEY(-122) THEN PROCWAITNOKEY(-122) : PROCloadnextframe(1,1) : REM SAVE CURRENT FRAME AND LOAD NEXT FRAME

          WAIT 2
        ENDIF

        REM SHOW ALL MOUSE TRACKING DETAILS
        REM PRINTTAB(0,0)SPC(40)
        REM PRINTTAB(0,0)"MX:";STR$(MX%);" MY:";STR$(MY%);" TX:";STR$(TX%);" TY:";STR$(TY%);" PX:";STR$(PX%);" PY:";STR$(PY%)


        REM REMEMBER MOUSE POSITION
        OLD_MX%=MX%
        OLD_MY%=MY%

      UNTIL 0

      END

      REM READ MOUSE AND CALCULATE TEXT AND SIXEL LOCATIONS
      DEF PROCREADMOUSE

      MOUSE MX%,MY%,MB%

      REM TEXT LOCATION RELEATIVE TO MOUSE
      TX%=MX% DIV 32
      TY%=(999-MY%) DIV 40

      REM SIXEL LOCATION RELEATIVE TO MOUSE
      PX%=MX% DIV 16
      PY%=(999-MY%)/13.3333333


      IF toolsel%=5 OR toolsel%=6 THEN
        IF TX%>-1 AND TX%<40 AND TY%>-1 AND TY%<25 THEN VDU 31,TX%,TY%
      ELSE
        IF menuext%=1 THEN
          VDU 31,shapesel%-1,1
        ELSE
          VDU 31,toolcursor%,0
        ENDIF
      ENDIF


      ENDPROC

      REM WAIT FOR MOUSE TO BE A SPECIFIC BUTTON CLICK
      DEF PROCWAITMOUSE(M%)
      REPEAT
        PROCREADMOUSE
        WAIT 2
      UNTIL MB%=M%
      ENDPROC

      REM WAIT FOR NO KEY INPUT
      DEF PROCWAITNOKEY(W%)
      REPEAT

      UNTIL INKEY(W%)=0
      ENDPROC

      REM Read the point at the specified coordinates (1=set, 0=cleared)
      DEFFNpoint(x%,y%)
      LOCAL cx%,cy%,chr%,C%
      REM Get character cell
      cx% = x% DIV 2
      cy% = y% DIV 3
      chr%=GET(cx%,cy%) AND &5F
      C%=(x% AND 1)+(y% MOD 3)*2
      C%=2^C% - (C%=5)*32
      =SGN(chr% AND C%)

      REM Plot a Teletext sixel point
      REM SIXEL COORDINATES WITH 0,0 BEING TOP LEFT THE SAME AS THE TEXT SCREEN
      REM cmd% 0: Clear the point
      REM cmd% 1: Set the point
      REM cmd% 2: Toggle the point
      DEFPROCpoint(x%, y%, cmd%)

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

      REM Read the point at the specified coordinates from specified buffer (1=set, 0=cleared)
      DEFFNpoint_buf(x%,y%,f%)
      LOCAL cx%,cy%,chr%,C%
      REM Get character cell
      cx% = x% DIV 2
      cy% = (y% DIV 3)-1
      chr%=frame_buffer&(f%,cx%+cy%*40) AND &5F
      C%=(x% AND 1)+(y% MOD 3)*2
      C%=2^C% - (C%=5)*32
      =SGN(chr% AND C%)

      REM Plot a Teletext sixel point from specified buffer
      REM SIXEL COORDINATES WITH 0,0 BEING TOP LEFT THE SAME AS THE TEXT SCREEN
      REM cmd% 0: Clear the point
      REM cmd% 1: Set the point
      REM cmd% 2: Toggle the point
      DEFPROCpoint_buf(x%, y%, cmd%,f%)

      IF x%>xMin% AND x%<xMax% AND y%>yMin% AND y%<yMax% THEN

        LOCAL cx%,cy%,chr%,C%
        REM Get character cell
        cx% = x% DIV 2
        cy% = (y% DIV 3)-1
        chr%=frame_buffer&(f%,cx%+cy%*40) AND &5F
        C%=(x% AND 1)+(y% MOD 3)*2
        C%=2^C% - (C%=5)*32
        CASE cmd% OF
          WHEN 0:chr% AND=(&5F - C%)
          WHEN 1:chr% OR=C%
          WHEN 2:chr% EOR=C%
        ENDCASE

        frame_buffer&(f%,cx%+cy%*40)=chr%+160
      ENDIF

      ENDPROC


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

      REM LINE ROUTINE FOR BUFFER USE m% TO PERFORM 0=ERASE / 1=DRAW / 2=EOR
      DEF PROCbresenham_buf(x1%,y1%,x2%,y2%,m%)
      LOCAL dx%, dy%, sx%, sy%, e
      dx% = ABS(x2% - x1%) : sx% = SGN(x2% - x1%)
      dy% = ABS(y2% - y1%) : sy% = SGN(y2% - y1%)
      IF dx% > dy% e = dx% / 2 ELSE e = dy% / 2

      REPEAT
        IF animategapcount%=0 THEN
          PROCpoint_buf(x1%,y1%,m%,frame%)
          frame%=frame%+1
          IF frame%>frame_max% THEN frame%=1
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

      REM ### fill quasi stack
      DEF PROCaddFill(x%,y%)
      fill{(bCnt%)}.x%=x%
      fill{(bCnt%)}.y%=y%
      IF bCnt%<100 THEN bCnt%+=1
      ENDPROC

      REM UPDATE CLEARSCREEN OPTIONS
      DEF PROCupdateCS
      LOCAL I%

      PRINTTAB(5,6)"FORE  ";CHR$(234);SPC(17);CHR$(181);
      PRINTTAB(5,8)"BACK  ";CHR$(234);SPC(17);CHR$(181);

      IF bakcol%=0 THEN PRINTTAB(13,8)"B"

      FOR I%=1 TO 7
        PRINTTAB(12+I%*2,6)CHR$(144+I%);CHR$(255+(I%=curcol%)*185);
        PRINTTAB(12+I%*2,8)CHR$(144+I%);CHR$(255+(I%=bakcol%)*189);
      NEXT

      PRINTTAB(5,10)"OUTPUT";
      IF bakcol%>0 THEN
        VDU 144+bakcol%,157,144+curcol%
      ELSE
        VDU 32,32,144+curcol%
      ENDIF
      PRINTTAB(14,10)"abcdefghijklmno";CHR$(156);CHR$(151)

      ENDPROC

      REM CLEARSCREEN DIALOG
      DEF PROCclearscreen
      LOCAL I%,L%,A$,B$,C$,cls%,fix%,done%,col_old%,bak_old%,h_old%,v_old%,hindex%,vindex%,skip%,skip_old%

      PROCmenusave
      menuext%=99
      cls%=1
      fix%=1
      skip%=3
      skip_old%=3

      PROCWAITMOUSE(0)

      FOR L%=4 TO 20
        PRINTTAB(0,L%)SPC(40);
      NEXT

      PRINTTAB(1,4)CHR$(151);CHR$(232);STRING$(10,CHR$(172));CHR$(130);"CLEARSCREEN";CHR$(151);STRING$(10,CHR$(172));CHR$(180);
      FOR L%=5 TO 20
        PRINTTAB(1,L%)CHR$(151);CHR$(234);STRING$(32," ");CHR$(151);CHR$(181);
      NEXT


      PRINTTAB(4,12)CHR$(132);"OPTION:  ";CHR$(134);"CLS:";CHR$(130);"Y";CHR$(134);" FIX:";CHR$(130);"Y";
      PRINTTAB(4,14)CHR$(146);CHR$(157);CHR$(132);"ALL FRAMES  ";CHR$(156);" ";CHR$(145);CHR$(157);CHR$(131);" CANCEL   ";CHR$(156);

      PRINTTAB(4,15)CHR$(129);STRING$(29,"-")

      PRINTTAB(4,16)CHR$(132);"SCROLL:  ";CHR$(134);"SKIP : HORZ : VERT"
      A$=STR$(skip%)+" "
      B$=STR$(scrollh%)
      IF LEN(B$)<2 THEN B$=B$+" "
      C$=STR$(scrollv%)
      IF LEN(C$)<2 THEN C$=C$+" "
      PRINTTAB(13,17)CHR$(135)+"-"+CHR$(131)+A$+CHR$(135)+"+"+CHR$(135)+"-"+CHR$(131)+B$+CHR$(135)+"+ -"+CHR$(131)+C$+CHR$(135)+"+"
      PRINTTAB(4,19)CHR$(146);CHR$(157);CHR$(132);"DUPE FRAME  ";CHR$(156);

      PRINTTAB(1,20)CHR$(151);CHR$(170);STRING$(33,CHR$(172));CHR$(165);

      PROCupdateCS

      done%=0
      col_old%=curcol%
      bak_old%=bakcol%
      h_old%=scrollh%
      v_old%=scrollv%
      skip_old%=skip%
      REPEAT
        PROCREADMOUSE
        IF MB%=4 THEN
          PROCWAITMOUSE(0)
          CASE TY% OF
            WHEN 0,1,2,3 : done%=-1 : REM CANCEL DIALOG
            WHEN 6  : REM FORGROUND COLOUR SELECTOR
              IF TX%>13 AND TX%<28 THEN curcol%=(TX%-12) DIV 2

            WHEN 8 : REM BACKGROUND COLOUR SELECTOR
              IF TX%>11 AND TX%<28 THEN bakcol%=(TX%-12) DIV 2

            WHEN 12  : REM TOGGLE CLS AND FIX
              IF TX%=20 THEN
                cls%=(cls%+1) AND 1
                PRINTTAB(19,12);CHR$(129+cls%);CHR$(78+cls%*11); : REM TOGGLE CLS
              ENDIF
              IF TX%=28 THEN
                fix%=(fix%+1) AND 1
                PRINTTAB(27,12);CHR$(129+fix%);CHR$(78+fix%*11); : REM TOGGLE FIX
              ENDIF

            WHEN 14
              IF TX%>5 AND TX%<20 THEN done%=1 : REM SELECT CLEARSCREEN AND FINISH
              IF TX%>23 AND TX%<34 THEN done%=-1 : REM CANCEL SCLEARSCREEN DIALOG
            WHEN 17
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
            WHEN 19 : IF TX%>5 AND TX%<20 THEN done%=2 : REM SELECT DUPE SCREEN AND FINISH

            WHEN 21,22,23,24 : done%=-1 : REM CANCEL DIALOG

          ENDCASE
          IF col_old%<>curcol% OR bak_old%<>bakcol% THEN
            PROCupdateCS
            col_old%=curcol%
            bak_old%=bakcol%

          ENDIF
          IF skip_old%<>skip% THEN
            A$=STR$(skip%)+" "
            PRINTTAB(16,17)A$;
            skip_old%=skip%
          ENDIF
          IF h_old%<>scrollh% THEN
            A$=STR$(scrollh%)
            IF LEN(A$)<2 THEN A$=A$+" "
            PRINTTAB(23,17)A$;
            h_old%=scrollh%
          ENDIF
          IF v_old%<>scrollv% THEN
            A$=STR$(scrollv%)
            IF LEN(A$)<2 THEN A$=A$+" "
            PRINTTAB(30,17)A$;
            v_old%=scrollv%
          ENDIF

        ENDIF
      UNTIL done%

      PROCWAITMOUSE(0)

      PROCmenurestore
      CASE done% OF
        WHEN -1:  REM CANCEL

        WHEN 1: REM NEW BACKGROUND COLOUR
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

            FOR I%=2 TO 8
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

        WHEN 2: REM DUPLICATE FRAME 1
          PROCframesave(1)
          frame%=0
          PROCloadnextframe(1,0)
          hindex%=scrollh%
          vindex%=scrollv%
          FOR frame%=2 TO frame_max%
            IF scrollh%<>0 OR scrollv%<>0 THEN
              PROCcopyframe(1,frame%,hindex%,vindex%,skip%)
              hindex%+=scrollh%
              vindex%+=scrollv%
            ELSE
              PROCframesave(frame%)
            ENDIF
          NEXT frame%
          frame%=1

      ENDCASE
      REMPROCloadnextframe(1,0)

      ENDPROC

      REM SAVE FRAME BUFFER
      DEF PROCframesave(f%)
      LOCAL U%

      FOR U%=0 TO 959
        frame_buffer&(f%,U%)=GET(U% MOD 40,U% DIV 40+1)
      NEXT

      ENDPROC

      REM RESTORE FRAME BUFFER
      DEF PROCframerestore(f%)
      LOCAL U%

      FOR U%=0 TO 959
        VDU 31,(U% MOD 40),(U% DIV 40+1),frame_buffer&(f%,U%)
      NEXT

      ENDPROC

      REM COPY A FRAME STARTING FROM OFFSET
      DEF PROCcopyframe(S%,D%,H%,V%,skip%)
      LOCAL U%,X%,Y%,xofs%,yofs%

      FOR X%=skip% TO 39
        REMIF X%>skip% THEN
        xofs%=X%+H%
        IF xofs%<0 THEN xofs%=40+xofs%
        IF xofs%>39 THEN xofs%=xofs%-40
        FOR Y%=0 TO 23
          yofs%=Y%+V%
          IF yofs%<0 THEN yofs%=23+yofs%
          IF yofs%>23 THEN yofs%=yofs%-24

          IF xofs%>skip%-1 THEN frame_buffer&(D%,X%+Y%*40)=frame_buffer&(S%,xofs%+yofs%*40)
        NEXT
        REM ENDIF
      NEXT

      ENDPROC

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

      REM save current screen to undo buffer
      DEF PROCundosave
      LOCAL U%

      IF undo_count&(frame%)<undo_max% THEN undo_count&(frame%)+=1

      FOR U%=0 TO 959
        undo_buffer&(frame%,undo_index%(frame%),U%)=GET(U% MOD 40,U% DIV 40+1)
      NEXT

      undo_index%(frame%)+=1
      IF undo_index%(frame%)>undo_max% THEN undo_index%(frame%)=0

      redo_count&(frame%)=0

      PROCdrawmenu

      ENDPROC

      REM restore current undo buffer to screen
      DEF PROCundorestore
      LOCAL U%

      IF undo_count&(frame%)>0 THEN
        undo_count&(frame%)-=1

        undo_index%(frame%)-=1
        IF undo_index%(frame%)<0 THEN undo_index%(frame%)=undo_max%

        PROCredosave
        FOR U%=0 TO 959
          VDU 31,(U% MOD 40),(U% DIV 40+1),undo_buffer&(frame%,undo_index%(frame%),U%)
        NEXT

      ENDIF

      ENDPROC

      REM SAVE REDO SCREEN
      DEF PROCredosave
      LOCAL U%

      IF redo_count&(frame%)<undo_max% THEN redo_count&(frame%)+=1

      FOR U%=0 TO 959
        redo_buffer&(frame%,redo_index%(frame%),U%)=GET(U% MOD 40,U% DIV 40+1)
      NEXT

      redo_index%(frame%)+=1
      IF redo_index%(frame%)>undo_max% THEN redo_index%(frame%)=0

      ENDPROC

      REM RESTORE REDO SCREEN
      DEF PROCredorestore
      LOCAL U%

      IF redo_count&(frame%)>0 THEN
        redo_count&(frame%)-=1

        redo_index%(frame%)-=1
        IF redo_index%(frame%)<0 THEN redo_index%(frame%)=undo_max%

        FOR U%=0 TO 959
          VDU 31,(U% MOD 40),(U% DIV 40+1),redo_buffer&(frame%,redo_index%(frame%),U%)
        NEXT

        undo_index%(frame%)+=1
        IF undo_index%(frame%)>undo_max% THEN undo_index%(frame%)=0
        IF undo_count&(frame%)<undo_max% THEN undo_count&(frame%)+=1

      ENDIF

      ENDPROC

      REM MENU BUFFER SAVE SCREEN
      DEF PROCmenusave
      LOCAL U%
      FOR U%=0 TO 959
        menu_buffer&(U%)=GET(U% MOD 40,U% DIV 40+1)
      NEXT

      ENDPROC

      REM MENU BUFFER UNDO SCREEN
      DEF PROCmenurestore
      LOCAL U%

      IF menuext% THEN
        FOR U%=0 TO 959
          VDU 31,(U% MOD 40),(U% DIV 40+1),menu_buffer&(U%)
        NEXT
        menuext%=0
      ENDIF
      ENDPROC


      REM SAVE BINARY FILE
      DEF PROCsavebinaryfile(F$)
      f%=OPENOUT(F$)
      FOR U%=0 TO 999
        BPUT#f%,GET(U% MOD 40,U% DIV 40)
      NEXT
      CLOSE#f%
      ENDPROC

      REM LOAD BINARY FILE
      DEF PROCloadbinaryfile(F$)
      f%=OPENIN(F$)

      FOR U%=0 TO 999
        char%=BGET#f%
        VDU 31,U% MOD 40,U% DIV 40,char%
      NEXT
      CLOSE#f%
      ENDPROC

      REM MODIFIED DIRSCAN TO INCLUDE TYPE ARRAY SO DIRS CAN BE MADE DIFFERENT COLOUR
      REM LOADFILE
      DEF PROCloadfile
      LOCAL N%

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
      PROCmenusave : menuext%=99
      FOR L%=6 TO 18
        PRINTTAB(0,L%)SPC(40);
      NEXT


      PRINTTAB(2,6)CHR$(151);CHR$(232);STRING$(10,CHR$(172));CHR$(130);"LOAD FILE";CHR$(151);STRING$(10,CHR$(172));CHR$(180);
      FOR L%=7 TO 17
        PRINTTAB(2,L%)CHR$(151);CHR$(234);STRING$(30," ");CHR$(151);CHR$(181);
      NEXT
      PRINTTAB(2,18)CHR$(151);CHR$(170);STRING$(31,CHR$(172));CHR$(165);

      N% = FN_dirscan2(n$(), t&(), "dir *.*",".bin")
      F%=0
      S%=0
      SEL%=0
      SELOLD%=0
      SELY%=-1
      INDEX%=1
      INDEXOLD%=1

      FOR I%=INDEX% TO INDEX%+10
        IF I%<N%+1 THEN PRINTTAB(6,6+I%)CHR$(c%(t&(I%)));LEFT$(n$(I%),24);
      NEXT

      REPEAT
        PROCREADMOUSE

        REM DETECT FIRST TOUCH OR MOVEMENT WHEN TOUCHING
        IF MB%=4 THEN
          IF MY%<>OLD_MY% THEN INDEX%+=SGN(MY%-OLD_MY%)
          IF INDEX%>N%-10 THEN INDEX%=N%-10
          IF INDEX%<1 THEN INDEX%=1
          IF SELY%=-1 THEN SELY%=MY%

        ENDIF

        REM DETECT TOUCH RELEASE
        IF MB%=0 THEN
          IF SELY%=MY% THEN
            S%=TY%-7
            IF S%>-1 AND S%<11 THEN SEL%=S%+INDEX%
            IF SEL%<1 THEN SEL%=1
            IF SEL%>N% THEN SEL%=N%

            IF t&(SEL%)=2 THEN
              F%=SEL%
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
                N% = FN_dirscan2(n$(), t&(), "dir *.*",".bin")
                SEL%=0
                SELOLD%=0
                SELY%=-1
                INDEX%=1
                INDEXOLD%=-1
                FOR I%=INDEX% TO INDEX%+10
                  PRINTTAB(6,6+I%)SPC(28);
                NEXT

              ENDIF
            ENDIF

            IF TX%<6 OR TX%>32 OR TY%<7 OR TY%>17 THEN F%=-1
          ENDIF
          SELY%=-1
        ENDIF

        REM IF SCROLLING DETECTED UPDATE FILE LIST AND SELECTED FILE INDEX
        IF INDEX%<>INDEXOLD% OR SELOLD%<>SEL% THEN
          FOR I%=0 TO 10
            K%=I%+INDEX%
            IF K%<N%+1 THEN
              PRINTTAB(4,I%+7)SPC(28)
              VDU 31,4,I%+7
              IF SEL%=K% THEN
                VDU 132,157
              ELSE
                VDU 32,32
              ENDIF
              PRINTCHR$(c%(t&(K%)));n$(K%);
              IF SEL%=K% THEN VDU 32,32,156
            ENDIF
          NEXT
          SELOLD%=SEL%
          INDEXOLD%=INDEX%
        ENDIF

        REM PRINTTAB(0,1)STR$(SEL%)

        OLD_MY%=MY%

        WAIT 2
      UNTIL F%<>0

      PROCmenurestore

      IF F%>0 THEN
        IF INSTR(n$(SEL%),"M7_") THEN

          F$=LEFT$(n$(SEL%),LEN(n$(SEL%))-5)
          FOR frame%=1 TO frame_max%
            PROCloadbinaryfile(curdir$+F$ + STR$(frame%)+".BIN")
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
      ENDIF
      ENDPROC


      REM save all frames to file, create session folder if not already exists
      DEF PROCsavefile
      PROCWAITMOUSE(0)

      PROCframesave(frame%)
      LOCAL D$,M$,T$,TMP$

      M$="JanFebMarAprMayJunJulAugSepOctNovDec"

      REM build date format: YYYYMMDDHHMMSS
      T$=TIME$
      TMP$=STR$(INSTR(M$,MID$(T$,8,3)) DIV 3+1)
      IF LEN(TMP$)<2 THEN TMP$="0"+TMP$
      D$=MID$(T$,12,4)+TMP$+MID$(T$,5,2)+"_"+MID$(T$,17,2)+MID$(T$,20,2)+MID$(T$,23,2)

      REM create and change to session folder, strip off seconds value
      IF session%=0 THEN
        TMP$="M7_"+LEFT$(D$,LEN(D$)-2)
        OSCLI "MD """+cursave$+TMP$+""""
        OSCLI "CD """+cursave$+TMP$+""""
        session%=1
        cursave$=cursave$+TMP$+"/"
      ENDIF

      REM SAVE FRAMES
      frame%=frame_max%
      FOR I%=1 TO frame_max%
        PROCloadnextframe(1,0)
        PROCsavebinaryfile(cursave$+"M7_" + D$ + "_" + STR$(frame%)+".BIN")
        OSCLI "SCREENSAVE """+cursave$+"M7_" + D$ + "_" + STR$(frame%)+".BMP"" 0,0,1280,1000"
        WAIT 10
      NEXT

      PROCloadnextframe(1,0)

      PROCmenusave : menuext%=99
      PRINTTAB(9,10)CHR$(151);CHR$(232);STRING$(18,CHR$(172));CHR$(180);CHR$(144+curcol%);
      FOR L%=11 TO 13
        PRINTTAB(9,L%)CHR$(151);CHR$(234);STRING$(17," ");CHR$(151);CHR$(181);CHR$(144+curcol%);
      NEXT
      PRINTTAB(9,14)CHR$(151);CHR$(170);STRING$(18,CHR$(172));CHR$(165);CHR$(144+curcol%);

      REM READ FILES
      PRINTTAB(13,12)CHR$(130);"FILE SAVED!";

      PROCWAITMOUSE(4)

      PROCWAITMOUSE(0)

      PROCmenurestore

      ENDPROC

      REM ANIMATE ALL FRAMES IN SEQUENCE FROM 1 TO FRAME_MAX%
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

      REM UPDATE COLOUR STRIP FOR BUFFER
      DEF PROCGR_BUF(D%,F%,B%)

      REM ADD GRAPHICS CODE TO LEFT SIDE OF CANVAS
      FOR Y%=0 TO 23
        IF B% THEN
          frame_buffer&(D%,Y%*40)=144+B%
          frame_buffer&(D%,Y%*40+1)=157
          frame_buffer&(D%,Y%*40+2)=144+F%
        ELSE
          frame_buffer&(D%,Y%*40)=144+F%
        ENDIF
      NEXT

      ENDPROC


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

      REM PRINT PALETTE AND MENU
      DEF PROCdrawmenu
      LOCAL A%,D%,E%,R%,U%
      FOR count%=1 TO 7
        PRINTTAB(count%*2-2,0) CHR$(128+count%);CHR$(255+(count%=curcol%)*213);
      NEXT count%

      A%=135-animation%*5
      D%=dither%+1
      E%=135-erase%*5
      R%=130+(redo_count&(frame%)=0)
      U%=130+(undo_count&(frame%)=0)
      PRINTTAB(14,0)CHR$(135);"PD";STR$(D%);"FS";CHR$(E%);"E";CHR$(U%);"U";CHR$(R%);"R";CHR$(135);"CBF LS";CHR$(A%);"A";CHR$(135);STR$(frame%);"<>P"

      PRINTTAB(0,1)STR$(undo_count&(frame%));"  ";STR$(undo_index%(frame%));"  "
      PRINTTAB(0,2)STR$(redo_count&(frame%));"  ";STR$(redo_index%(frame%));"  "


      REM SHAPE MENU
      IF menuext%=1 THEN
        D%=135-animateshape%*5

        A$="LRO"+CHR$(D%)+"A"+CHR$(134)+"GAP:"+CHR$(135)+"-"+CHR$(131)+STR$(animategap%)+CHR$(135)+"+"+CHR$(134)+"LEN:"+CHR$(135)+"-"+CHR$(131)+STR$(animatelen%)+CHR$(135)+"+"

        PRINTTAB(0,1)A$;SPC(40-LEN(A$))
        PRINTTAB(0,2)SPC(40)

        A$="136"+CHR$(136)+CHR$(255)+CHR$(137)+CHR$(154)+"154"+CHR$(151)+CHR$(255)+CHR$(153)+CHR$(135)+"158"+CHR$(158)+CHR$(255)+"  "+CHR$(131)+STR$(GET(TX%,TY%))+"  "
        B$=CHR$(132)+"FLSH   SEPR   HOLD   CODE"
        PRINTTAB(0,3)A$;SPC(40-LEN(A$))
        PRINTTAB(0,4)B$;SPC(40-LEN(B$))

        PRINTTAB(0,5)SPC(40)
        PRINTTAB(0,6)" A B C D E F G H I J K L M";SPC(14)
        PRINTTAB(0,7)SPC(40)
        PRINTTAB(0,8)" N O P Q R S T U V W X Y Z";SPC(14)
        PRINTTAB(0,9)SPC(40)
        PRINTTAB(0,10)"TEXT:";SPC(35)

      ENDIF

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

      IF N% < DIM(name$(),1) name$(N%) = "@lib$" : type&(N%) = 0 : N% += 1
      IF N% < DIM(name$(),1) name$(N%) = "@usr$" : type&(N%) = 0 : N% += 1
      name$(N%) = ".." : type&(N%) = 0 : N% += 1
      N% -= 1


      REM Sort the array so directories are listed before programs:
      C% = N%
      CALL sort%%, type&(1), name$(1)

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

      REM Sort library for BBCSDL (binary version), Richard Russell 14-Sep-2018

      DEF FN_sortinit(D%, S%)
      LOCAL DATA
      LOCAL p%%, p, a$, sort%%, csort%%, patch%, patch&
      DIM p%% 1000 + &FFF : p%% = (p%% + &FFF) AND -&1000
      p = PI
      IF @platform% AND &40 THEN
        CASE !^p OF
          WHEN &2168C235: PROC_sortX64 : csort%% = p%% : patch% = &1A2 : patch& = &C8
          WHEN &54442D18: IF D% THEN = @fn%(15) ELSE = @fn%(14)
          OTHERWISE: ERROR 0, "CPU test failed"
        ENDCASE
      ELSE
        CASE !^p OF
          WHEN &2168C235: PROC_sortX86 : csort%% = p%% : patch% = &58 : patch& = &76
          WHEN &54442D18: PROC_sortARM : csort%% = p%% + &61 : patch% = &1F6 : patch& = &29
          OTHERWISE: ERROR 0, "CPU test failed"
        ENDCASE
      ENDIF
      READ a$
      sort%% = p%% + LEN(a$) DIV 2
      REPEAT
        WHILE a$<>""
          ?p%% = EVAL("&"+LEFT$(a$,2))
          a$ = MID$(a$,3)
          p%% += 1
        ENDWHILE
        READ a$
      UNTIL a$=""
      IF @platform% AND &40 ]p%% = PAGE-!340
      IF D% csort%%?patch% = patch&
      IF (@platform% AND &FF) = &40 THEN = sort%%
      IF INKEY$(-256) = "S" THEN = sort%%
      IF INKEY$(-256) = "s" THEN = csort%%
      ERROR 0, "sortlib: wrong platform"

      DEF PROC_sortX86
      RESTORE +1
      ENDPROC
      DATA "535657558B4C241C8B6C242CE8050000005D5F5E5BC3"
      DATA "E3FDFC8BD1BFFFFFFFFFD1EF3BD776FA33F65756550FB64D008A45018B5D0283"
      DATA "C505600FB6C880E10F3C817502B1060FAFF10FAFF903F303FBE8BF00000061E1"
      DATA "D85D7343558A65008A45018B5D0283C5050FB6C880E10F3C817502B10656570F"
      DATA "AFF10FAFF98A043386043B8804334647E2F35F5EFECC75D05D8BC72BC63BF072"
      DATA "068BFE2BF0EB8D2BFE5E465603FE3BFA72825E5FD1EF0F8574FFFFFFC3668B5F"
      DATA "08C1E310668B5E080BDB74378B4E048B47048B168B3FE84C010000C1C3109187"
      DATA "D7E8410100008BEBC1E5106633DB33DD7903D1D5C333DD790587DD9187D73BEB"
      DATA "7413C38B4F048B46048B178B3E0FBAF91F0FBAF81F3BC875023BD7F3C33C0474"
      DATA "6F3C0874193C0A74943C2874D63C8174373C88743D3C0574668A073A06C38B4F"
      DATA "048B46048B178B1EE8B100000087DA91E8A900000033C87903D1D0C333C87803"
      DATA "87DA913BC87435C30FB75E040FB74F04EB068B5E048B4F048B368B3F87D987F7"
      DATA "3BD99F73028BCBE305F3A675019EF3C38B178B1E0FBAFA1F0FBAFB1F3BD3C38A"
      DATA "4F048A46048B178B1E33DA7903D1D2C333DA0AC0750F3AC174DA86C187DAE802"
      DATA "000000F5C3E80D0000000BDB790487DA86C13AC874C6C30AC975210BD2741D66"
      DATA "9C7902F7DA0FBDCA80E91FF6D9D3E2F6D980C19F669D78040FBAF21FF3C3F7C1"
      DATA "00FFFFFF751FE8CCFFFFFF0FB6C9E31581C17F030000D1C20FA4D115C1E2140F"
      DATA "BAF214D1D9F3C3660BDB750C535152DF2C24DB3C245A595BF3C3"
      DATA ""

      DEF PROC_sortARM
      RESTORE +1
      ENDPROC
      DATA "F8B550EA010323D0002871F1000307DB0022104B4FF0FF340F4D00260F4F07E0"
      DATA "404261EB4101FFF7EBFF41F00041F8BD844275EB010E04DB92197B4100184941"
      DATA "F6E7054D4FF0FF342040294010431943F8BD00BF00003043FFFF0F000000F0FF"
      DATA "2DE9F04F131E87B0019340F320814FF0FF34019B64089C42FBD2129B01330293"
      DATA "0026A1463246129B05210120DDF808A01B78D34611FB03031299CB180093009B"
      DATA "9B4500F0F9809BF800100BF1050B5BF8043C01F00F05082906FB05F009FB05F5"
      DATA "03EB000703EB050E5ED00FD8012915D00429E4D15D591B589D42ACBF00210121"
      DATA "9D42D4BF4942C1F10101B1E028296CD0882900F083800A29D1D102E05D5D1B5C"
      DATA "72E0BEF80810B7F908C011B9BCF1000F5BD053F805807F68DEF804501B5861B9"
      DATA "40462946CDF814C004930392FFF768FF03AA92E80C1080460D46BCF1000F07D1"
      DATA "184639460392FFF75BFF039A03460F46BD42ACBF00210121BD42D4BF4942C1F1"
      DATA "010197EA050F73D4BD4208D198452CBF00210121984594BF4942C1F1010181EA"
      DATA "E57101EBD57163E0DEF804E07F68BE45ACBF00210121BE45D4BF4942C1F10101"
      DATA "97EA0E0F54D4BE450AD15D591B589D422CBF002101219D4294BF4942C1F10101"
      DATA "81EAEE7101EBDE7142E0DEF804107F68B94207D0CCBF01210021B4BF01230023"
      DATA "C91A35E05D591B589D422CBF002101219D4294BF4942C1F1010129E05D597F68"
      DATA "DEF804E01B58BE4594BF05EB0E0C05EB070C013B654512D015F8011B13F8010F"
      DATA "81422CBF4FF000084FF00108814294BFC8F10001C8F101010029EBD00BE0BE45"
      DATA "2CBF00210121BE4594BF4942C1F1010100293FF424AF01311ED1009B9A4516D0"
      DATA "9AF800300AF1050A5AF8041C03F00F0306FB031009FB0311013BDBB2FF2BECD0"
      DATA "05780F7800F8017B01F8015BF4E7A64202D3B146361B03E0013202EB04091646"
      DATA "019B9945FFF4EFAE64087FF4E9AE07B0BDE8F08F"
      DATA ""

      DEF PROC_sortX64
      RESTORE +1
      ENDPROC
      DATA "4987D04883EC28E8050000004883C428C3"
      DATA "4157415641554154555756534883EC184585C00F8EE301000083CAFFD1EA4139"
      DATA "D076F94189D34531C931F6488B842490000000488B9C24900000004C8B2DCB01"
      DATA "00004C8D50010FB600488D04C04C8D6403014C89D34C39E30F84840100008A03"
      DATA "4883C309488B6BF889C783E70F89F9410FAFFB410FAFC94801EF4801E93C0874"
      DATA "7877133C0174213C0475CA8B3F8B0939CFE9860000003C2874793C880F848500"
      DATA "00003C0A75AFEB158A0931C0408A3F4038CF0F97C00F92C1E9C600000066837F"
      DATA "0800668B690875116685ED488B07744348890424DF2C24EB0DDB2F6685ED7506"
      DATA "DF29D9C9EB04DB29D9C931C0DBE9D9C90F97C0DFE9DDD8EB15F20F100731C0F2"
      DATA "0F1009660F2EC10F97C0660F2EC80F97C1EB70488B3F488B094839CF0F9FC00F"
      DATA "9CC10FB6C0EB5C8B69048B018B0F8B7F0439FD4189FE440F46F529C88944240C"
      DATA "458D3C0E44893C248B44240C01C83B0C2474264189CE478A7C3500458A740500"
      DATA "31C04538F7410F9CC60F9FC0450FB6F64429F0751BFFC1EBCF31C039FD0F92C0"
      DATA "0F97C10FB6C929C885C00F84C5FEFFFFFFC0754E4D39E2743C418A1A4983C209"
      DATA "498B42F883E30F0FB6CB89CF410FAFCB410FAFF94801C14801C731C038C374D4"
      DATA "408A2C07448A2C0144882C0740882C0148FFC0EBE74139D172084589CB4129D1"
      DATA "EB09FFC6448D1C324189F14539D80F8737FEFFFFD1EA0F8527FEFFFF4883C418"
      DATA "5B5E5F5D415C415D415E415FC3"
      DATA ""

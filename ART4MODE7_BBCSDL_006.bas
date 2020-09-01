      MODE 7

      REM *** TODO LIST ***

      REM *** INKEY(-256) MATRIX BRANYDY &4D (INCLUDE OTHERS)

      REM *** INVESTIGATE LOCAL VERSIONS OF LIBRARIES TO MAKE PROGRAM CROSS BASIC COMPATIBLE

      REM *** LOAD SCREEN SORT BY NEWEST, NEEDS WORK!

      REM *** COPY AND PASTE

      REM *** ADD UNDOS & REDOS FOR EACH FRAME? OR RESET UNDO IF CHANGING FRAMES

      REM *** IMPLEMENT ANIMATED CIRCLE

      REM *** CONTROL CODE SUB MENU TO ALLOW ALL AVAILABLE CONTROL CODES TO BE PLOTTED (IN PROGRESS)

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

      REM UNDO BUFFER ARRAY
      UNDO_MAX%=19
      DIM UNDO_BUFFER(UNDO_MAX%,959)
      UNDO_INDEX%=0

      REM REDO BUFFER ARRAY
      DIM REDO_BUFFER(UNDO_MAX%,959)
      REDO_INDEX%=0

      REM FRAME BUFFER ARRAY
      frame_max%=8
      DIM FRAME_BUFFER(frame_max%,959)

      REM MENU BUFFER
      DIM MENU_BUFFER(959)

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

      REM FILE DIALOG
      N%=0

      PROCGR(curcol%,bakcol%,1)
      PROCdrawmenu

      FOR frame%=1 TO frame_max%
        PROCframesave(frame%)
        REM WAIT 10
      NEXT frame%
      frame%=1


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
                    IF menuext%<>2 THEN
                      IF menuext%>0 THEN PROCmenurestore
                      PROCmenusave
                      menuext%=2
                    ELSE
                      PROCmenurestore
                    ENDIF

                  WHEN 31: PROCmenurestore:PROCloadfilemode3 : REM LOAD
                  WHEN 32: PROCmenurestore:PROCsavefile : REM SAVE

                  WHEN 34: animation%=(animation%+1) AND 1:PROCmenurestore : REM TOGLE ANIMATION FRAME ADVANCE TOOL

                  WHEN 36: REM frame%
                  WHEN 37: PROCmenurestore:PROCloadnextframe(-1,1) : REM SAVE CURRENT FRAME AND LOAD PREVIOUS FRAME
                  WHEN 38: PROCmenurestore:PROCloadnextframe(1,1) : REM SAVE CURRENT FRAME AND LOAD NEXT FRAME
                  WHEN 39: PROCmenurestore:PROCplay : REM SAVE CURRENT FRAME AND PLAY ALL FRAMES FROM FRAME 1


                ENDCASE

                REM HIDE SHAPE MENU IF ANOTHER TOOL IS SELECTED
                IF toolsel%<>4 AND menuext%=1 THEN PROCmenurestore
                IF toolsel%<>6 AND menuext%=2 THEN PROCmenurestore

                PROCdrawmenu
              ENDIF
            ENDIF

          ELSE
            REM CHECK IF EXTENDED MENU IS ACTIVE
            IF menuext% AND TY%=1 THEN
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
                        IF TX%>0 THEN
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
                            IF TX%>0 THEN
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
                      IF TX%<39 THEN VDU 31,TX%,TY%,(curcol%+144),157-erase%
                      REPEAT
                        PROCREADMOUSE
                        IF TX%<>OLD_TX% OR TY%<>OLD_TY% THEN
                          IF TX%<39 VDU 31,TX%,TY%,(curcol%+144),157-erase%
                        ENDIF
                        OLD_TX%=TX%
                        OLD_TY%=TY%
                      UNTIL MB%=0
                      IF animation% THEN PROCloadnextframe(1,1)
                    WHEN 6: REM FORGROUND COLOUR
                      PROCundosave
                      VDU 31,TX%,TY%,(curcol%+144)
                      REPEAT
                        PROCREADMOUSE
                        IF TX%<>OLD_TX% OR TY%<>OLD_TY% THEN VDU 31,TX%,TY%,(curcol%+144)
                        OLD_TX%=TX%
                        OLD_TY%=TY%
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
        IF menuext%=2 THEN
          IF OLD_TX%<>TX% OR OLD_TY%<>TY% THEN
            PRINTTAB(21,1)CHR$(131);STR$(GET(TX%,TY%))+"  "
            OLD_TX%=TX%
            OLD_TY%=TY%
          ENDIF
        ENDIF
        VDU 31,TX%,TY%
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
      chr%=FRAME_BUFFER(f%,cx%+cy%*40) AND &5F
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
        chr%=FRAME_BUFFER(f%,cx%+cy%*40) AND &5F
        C%=(x% AND 1)+(y% MOD 3)*2
        C%=2^C% - (C%=5)*32
        CASE cmd% OF
          WHEN 0:chr% AND=(&5F - C%)
          WHEN 1:chr% OR=C%
          WHEN 2:chr% EOR=C%
        ENDCASE

        FRAME_BUFFER(f%,cx%+cy%*40)=chr%+160
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
                  IF skip%>3 THEN skip%=3
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
        FRAME_BUFFER(f%,U%)=GET(U% MOD 40,U% DIV 40+1)
      NEXT

      ENDPROC

      REM RESTORE FRAME BUFFER
      DEF PROCframerestore(f%)
      LOCAL U%

      FOR U%=0 TO 959
        VDU 31,(U% MOD 40),(U% DIV 40+1),FRAME_BUFFER(f%,U%)
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

          IF xofs%>skip%-1 THEN FRAME_BUFFER(D%,X%+Y%*40)=FRAME_BUFFER(S%,xofs%+yofs%*40)
        NEXT
        REM ENDIF
      NEXT

      ENDPROC

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

      REM SAVE UNDO SCREEN
      DEF PROCundosave
      LOCAL U%
      UNDO_INDEX%+=1
      IF UNDO_INDEX%>UNDO_MAX% THEN UNDO_INDEX%=UNDO_MAX%

      FOR U%=0 TO 959
        UNDO_BUFFER(UNDO_INDEX%,U%)=GET(U% MOD 40,U% DIV 40+1)
      NEXT
      IF UNDO_INDEX%>0 THEN PROCdrawmenu
      ENDPROC

      REM RESTORE UNDO SCREEN
      DEF PROCundorestore
      LOCAL U%

      IF UNDO_INDEX%>0 THEN
        PROCredosave
        FOR U%=0 TO 959
          VDU 31,(U% MOD 40),(U% DIV 40+1),UNDO_BUFFER(UNDO_INDEX%,U%)
        NEXT

        UNDO_INDEX%-=1
      ENDIF

      ENDPROC

      REM SAVE REDO SCREEN
      DEF PROCredosave
      LOCAL U%
      REDO_INDEX%+=1
      IF REDO_INDEX%>UNDO_MAX% THEN REDO_INDEX%=UNDO_MAX%

      FOR U%=0 TO 959
        REDO_BUFFER(REDO_INDEX%,U%)=GET(U% MOD 40,U% DIV 40+1)
      NEXT

      ENDPROC

      REM RESTORE REDO SCREEN
      DEF PROCredorestore
      LOCAL U%

      IF REDO_INDEX%>0 THEN
        FOR U%=0 TO 959
          VDU 31,(U% MOD 40),(U% DIV 40+1),REDO_BUFFER(REDO_INDEX%,U%)
        NEXT

        REDO_INDEX%-=1
        UNDO_INDEX%+=1

      ENDIF

      ENDPROC

      REM MENU BUFFER SAVE SCREEN
      DEF PROCmenusave
      LOCAL U%
      FOR U%=0 TO 959
        MENU_BUFFER(U%)=GET(U% MOD 40,U% DIV 40+1)
      NEXT

      ENDPROC

      REM MENU BUFFER UNDO SCREEN
      DEF PROCmenurestore
      LOCAL U%

      IF menuext% THEN
        FOR U%=0 TO 959
          VDU 31,(U% MOD 40),(U% DIV 40+1),MENU_BUFFER(U%)
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


      REM LOAD FILE WITH MODE 3
      DEF PROCloadfilemode3
      PROCmenusave
      PROCWAITMOUSE(0)

      MODE 3

      A$=FNUPPER(FN_filedlg("FILE LOAD", "LOAD", "", "BIN FILES", ".BIN", 0))

      MODE 7

      PROCmenurestore

      IF RIGHT$(A$,4)=".BIN" THEN
        IF INSTR(A$,"M7_") THEN

          F$=LEFT$(A$,LEN(A$)-5)
          FOR frame%=1 TO frame_max%
            PROCloadbinaryfile(F$ + STR$(frame%)+".BIN")
            PROCframesave(frame%)
            REM WAIT 10
          NEXT
          PROCloadnextframe(1,0)
        ELSE
          PROCloadbinaryfile(A$)
          PROCframesave(frame%)
        ENDIF
      ENDIF


      ENDPROC

      REM LOADFILE
      DEF PROCloadfile

      LOCAL N%
      DIM n$(10000)

      N% = FN_dirscan(n$(), "dir *.BIN", "", "", "")

      REM HACK SORT, NEED TO LOOK INTO READING FILE DATES AND SORTING NEWEST FIRST
      FOR I%=1 TO N% DIV 2
        SWAP n$(I%),n$(N%-I%+1)
      NEXT

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

      F%=0
      S%=0
      SEL%=0
      SELOLD%=0
      SELY%=-1
      INDEX%=1
      INDEXOLD%=1
      DC%=0


      FOR I%=INDEX% TO INDEX%+10
        IF I%<N% THEN PRINTTAB(6,6+I%)CHR$(131);LEFT$(n$(I%),24);
      NEXT

      REPEAT
        PROCREADMOUSE

        REM DETECT FIRST TOUCH OR MOVEMENT WHEN TOUCHING
        IF MB%=4 THEN
          IF MY%<>OLD_MY% THEN INDEX%+=SGN(MY%-OLD_MY%)
          IF INDEX%<1 THEN INDEX%=1
          IF INDEX%>N%-10 THEN INDEX%=N%-10
          IF SELY%=-1 THEN SELY%=MY%

        ENDIF

        REM DETECT TOUCH RELEASE
        IF MB%=0 THEN
          IF SELY%=MY% THEN
            S%=TY%-7
            IF S%>-1 AND S%<11 THEN SEL%=S%+INDEX%
            IF SEL%<1 THEN SEL%=1
            IF SEL%>N% THEN SEL%=N%
            F%=SEL%
            IF TX%<6 OR TX%>32 OR TY%<7 OR TY%>17 THEN F%=-1
          ENDIF
          SELY%=-1
        ENDIF

        REM IF SCROLLING DETECTED UPDATE FILE LIST AND SELECTED FILE INDEX
        IF INDEX%<>INDEXOLD% OR SELOLD%<>SEL% THEN
          FOR I%=0 TO 10
            K%=I%+INDEX%
            IF K%<N%+1 AND K%>0 THEN
              VDU 31,4,I%+7
              IF SEL%=K% THEN
                VDU 132,157
              ELSE
                VDU 32,32
              ENDIF
              PRINTCHR$(131);LEFT$(n$(K%),24);
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
        IF LEFT$(FNUPPER(n$(SEL%)),3)="M7_" THEN

          F$=LEFT$(n$(SEL%),LEN(n$(SEL%))-5)
          FOR frame%=1 TO frame_max%
            PROCloadbinaryfile(F$ + STR$(frame%)+".BIN")
            PROCframesave(frame%)
            REM WAIT 10
          NEXT
          PROCloadnextframe(1,0)
        ELSE
          IF RIGHT$(FNUPPER(n$(SEL%)),3)="BIN" THEN
            PROCloadbinaryfile(n$(SEL%))
            PROCframesave(frame%)
          ENDIF
        ENDIF
      ENDIF
      ENDPROC

      REM SAVEFILE
      DEF PROCsavefile
      PROCWAITMOUSE(0)

      PROCframesave(frame%)
      LOCAL D$,M$,T$,TMP$

      M$="JanFebMarAprMayJunJulAugSepOctNovDec"

      REM BUILD DATE FORMAT AS YYYYMMDDHHMMSS
      T$=TIME$
      D$=MID$(T$,12,4)
      TMP$=STR$(INSTR(M$,MID$(T$,8,3)) DIV 3+1)
      IF LEN(TMP$)<2 THEN TMP$="0"+TMP$
      D$=D$+TMP$+MID$(T$,5,2)+MID$(T$,17,2)+MID$(T$,20,2)+MID$(T$,23,2)

      REM SAVE FRAMES
      frame%=frame_max%
      FOR I%=1 TO frame_max%
        PROCloadnextframe(1,0)
        PROCsavebinaryfile("M7_" + D$ + "_" + STR$(frame%)+".BIN")
        OSCLI "SCREENSAVE ""M7_" + D$ + "_" + STR$(frame%)+".BMP"" 0,0,1280,1000"
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
          FRAME_BUFFER(D%,Y%*40)=144+B%
          FRAME_BUFFER(D%,Y%*40+1)=157
          FRAME_BUFFER(D%,Y%*40+2)=144+F%
        ELSE
          FRAME_BUFFER(D%,Y%*40)=144+F%
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
      R%=130+(REDO_INDEX%=0)
      U%=130+(UNDO_INDEX%=0)
      PRINTTAB(14,0) CHR$(135);"PD";STR$(D%);"FS";CHR$(E%);"E";CHR$(U%);"U";CHR$(R%);"R";CHR$(135);"CBF LS";CHR$(A%);"A";CHR$(135);STR$(frame%);"<>P"

      REM SHAPE MENU
      IF menuext%=1 THEN
        D%=135-animateshape%*5

        A$="LRO"+CHR$(D%)+"A"+CHR$(134)+"GAP:"+CHR$(135)+"-"+CHR$(131)+STR$(animategap%)+CHR$(135)+"+"+CHR$(134)+"LEN:"+CHR$(135)+"-"+CHR$(131)+STR$(animatelen%)+CHR$(135)+"+"

        PRINTTAB(0,1)A$;SPC(40-LEN(A$))
      ENDIF

      REM CONTROL CODE MENU
      IF menuext%=2 THEN

        A$="136"+CHR$(136)+CHR$(255)+CHR$(137)+CHR$(154)+"154"+CHR$(151)+CHR$(255)+CHR$(153)+CHR$(135)+"158"+CHR$(158)+CHR$(255)+"  "+CHR$(131)+STR$(GET(TX%,TY%))+"  "
        B$=CHR$(132)+"FLSH   SEPR   HOLD   CODE"
        PRINTTAB(0,1)A$;SPC(40-LEN(A$))
        PRINTTAB(0,2)B$;SPC(40-LEN(B$))
      ENDIF

      ENDPROC


      REM ***********************************************************************
      REM LIB FUNCTIONS
      REM ***********************************************************************

      REM Open File, Save File or Browse For Folder dialogue control for BBCSDL
      REM Requires @lib$ + "dlglib", "sortlib" and "stringlib" to be INSTALLed.
      REM Version 0.12, 21-Jul-2020, R. T. Russell, http://www.rtrussell.co.uk/

      REM title$  : String to appear in title bar
      REM okverb$ : Caption for 'OK' button (e.g. "Open", "Save" or "Select")
      REM inidir$ : Initial directory (set to "" to remember from previous call)
      REM filtyp$ : Human-readable description of file type e.g. "BASIC files"
      REM filter$ : Semicolon-separated list of extensions, e.g. ".bbc;.bas"
      REM flag%   : 0 = disable, 1 = enable, 'All files' button,
      REM Set filter$ to "." (hide files) for 'Browse For Folder' functionality
      DEF FN_filedlg(title$, okverb$, inidir$, filtyp$, filter$, flag%)
      LOCAL C%, F%, H%, W%, X%, Y%, a$, d$
      PRIVATE D%

      IF D% = 0 THEN
        REM Create file and folder icons:
        C% = (1.5 * @vdu%!220 + 7) AND -8
        VDU 21
        OSCLI "spool """ + @tmp$ + "bbfile.vdu"""
        GCOL 14+128
        PLOT 0,C%,0 : PLOT 99,C%*0.75,-C%*0.75 : PLOT 0,C%/4,C%/4 : PLOT 0,-C%*0.75,0
        PLOT 115,-C%/2,-C%/2 : PLOT 0,C%/4,C%/4 : PLOT 98,C%*0.75,C%*0.75 : PLOT 0,0,-C%/2
        PLOT 1,C%/4,0 : PLOT 1,-C%/2,-C%/2 : PLOT 1,-C%*0.75,0 : PLOT 1,C%/4,C%/4
        PLOT 0,C%*1.25,C%*0.75 : PLOT 0,0,0
        OSCLI "spool """ + @tmp$ + "folder.vdu"""
        GCOL 14+128
        PLOT 0,C%,0 : PLOT 0,C%*0.75,0 : PLOT 99,-C%*0.75,-C%
        PLOT 0,C%/2,-C%/4 : PLOT 0,0,C% : PLOT 115,-C%/2,C%/4
        PLOT 1,0,-C% : PLOT 1,C%/2,-C%/4 : PLOT 1,0,C% : PLOT 1,-C%/2,C%/4
        PLOT 1,C%*0.75,0 : PLOT 1,0,-C% : PLOT 1,-C%*0.25,0 : PLOT 0,C%*0.75,C%
        *spool
        VDU 6
        F% = OPENIN(@tmp$ + "bbfile.vdu")
        ICON_FILE$ = GET$#F% BY EXT#F%
        CLOSE #F%
        F% = OPENIN(@tmp$ + "folder.vdu")
        ICON_FOLD$ = GET$#F% BY EXT#F%
        CLOSE #F%
        IF LEN(ICON_FILE$) <> LEN(ICON_FOLD$) ERROR 100, "Icons not equal size"

        REM Create dialogue box template:
        D% = FN_newdialog("", 200, 190)
        PROC_static(D%, "File name:", 103, 3, 160, 40, 10, SS_RIGHT)
        PROC_textbox(D%, "", 104, 45, 159, 150, 10, 0)
        PROC_textbox(D%, "", 105, 0, 0, 0, 0, WS_VISIBLE)
        PROC_radiobutton(D%, "", 108, 5, 174, 60, 11, 0)
        PROC_radiobutton(D%, "All files", 109, 66, 174, 40, 11, 0)
        PROC_button(D%, "OK", 1, 107, 174, 40, 11, WS_DISABLED)
        PROC_button(D%, "Cancel", 2, 155, 174, 40, 11, 0)
        PROC_static(D%, "Folder:", 101, 3, 5, 40, 10, SS_RIGHT)
        PROC_textbox(D%, @usr$, 102, 45, 4, 150, 10, 0)
        ID_LISTBOX = FN_setproc(PROC_fsclick())
        PROC_listbox(D%, "", ID_LISTBOX, 5, 18, 190, 136, WS_VSCROLL)
      ENDIF

      REM Initialise controls:
      IF inidir$ <> "" THEN
        PROC_setdlgitemtext(D%, 102, inidir$)
        PROC_setdlgitemtext(D%, 104, "")
      ENDIF
      IF okverb$ <> "" PROC_setdlgitemtext(D%, 1, okverb$)
      PROC_setlistboxselect(D%, ID_LISTBOX, 0)
      PROC_setdlgitemtext(D%, 105, filter$)
      PROC_setdlgitemtext(D%, 108, filtyp$)
      PROC_enabledlgitem(D%, 109, flag%)
      PROC_showdlgitem(D%, 103, filtyp$ <> "")
      PROC_showdlgitem(D%, 104, filtyp$ <> "")
      PROC_showdlgitem(D%, 108, filtyp$ <> "")
      PROC_showdlgitem(D%, 109, filtyp$ <> "")

      REM Set palette:
      PROC_setdialogpalette
      COLOUR 14, &C0, &C0, &00 : REM Folder and file icons yellow

      PROC_setdialogtitle(D%, title$)
      PROC_registerdlgcallback(D%, FN_fscb())
      PROC_checkdlgitem(D%, 108, 1)
      PROC_checkdlgitem(D%, 109, 0)

      VDU 26
      X% = &80000000 : Y% = &80000000
      REPEAT
        C% = FN_showdialog(D%, X%, Y%)
        PROC_getdlgrect(D%, X%, Y%, W%, H%)
        a$ = FN_getdlgitemtext(D%, 104)
        d$ = FN_getdlgitemtext(D%, 102)
      UNTIL C% = 1 OR C% = 2
      PROC_closedialog(D%)

      IF C% = 2 THEN = ""
      = d$ + a$

      DEF FN_fscb(D%,K%)
      LOCAL B%, N%, a$, t$
      PRIVATE E%, F%, d$, f$, p$, s$, n$()
      DIM n$(10000)
      IF F% <> FN_isdlgitemchecked(D%,109) THEN
        F% = NOT F%
        d$ = ""
      ENDIF
      B% = FN_getdlgitemtext(D%,105) = "."
      IF B% THEN
        N% = FN_getlistboxselect(D%,ID_LISTBOX) <= 0
      ELSE
        N% = FN_getdlgitemtext(D%,104) <> ""
      ENDIF
      IF E% <> N% THEN
        E% = N%
        PROC_enabledlgitem(D%,1,E%)
        PROC_refreshdialog(D%)
      ENDIF
      a$ = FN_getdlgitemtext(D%,102)
      IF RIGHT$(a$) = "/" OR RIGHT$(a$) = "\" THEN
        t$ = FN_lower(FN_getdlgitemtext(D%,105))
        IF a$ <> d$ OR t$ <> f$ THEN
          d$ = a$
          f$ = t$
          PROC_setdlgitemtext(D%, 104, "")
          ON ERROR LOCAL IF FALSE THEN
            N% = 0
            OSCLI "cd """ + a$ + """"
          ELSE
            N% = 3
          ENDIF : RESTORE ERROR
          IF N% = 0 THEN
            IF F% THEN
              N% = FN_dirscan(n$(), "dir *.*", "", ICON_FOLD$, ICON_FILE$)
            ELSE
              N% = FN_dirscan(n$(), "dir *.*", t$, ICON_FOLD$, ICON_FILE$)
            ENDIF
            PROC_setdlgitemtext(D%, 102, n$(0))
          ENDIF
          PROC_setlistboxarray(D%, ID_LISTBOX, n$(), N%)
          IF B% PROC_setlistboxselect(D%, ID_LISTBOX, -1) : REM For double-click
          PROC_refreshdialog(D%)
        ENDIF
      ENDIF
      a$ = FN_getdlgitemtext(D%, ID_LISTBOX)
      IF a$ <> s$ THEN
        s$ = a$
        IF LEFT$(a$, LEN(ICON_FILE$)) = ICON_FILE$ THEN
          PROC_setdlgitemtext(D%, 104, FN_trim(MID$(a$, LEN(ICON_FILE$) + 1)))
          PROC_refreshdialog(D%)
        ENDIF
      ENDIF
      a$ = FN_getdlgitemtext(D%, 104)
      IF a$ <> p$ THEN
        p$ = a$
        IF ICON_FILE$ + a$ <> FN_getdlgitemtext(D%, ID_LISTBOX) THEN
          N% = FNchops(n$(), ICON_FILE$ + a$) + 1
          PROC_setlistboxselect(D%, ID_LISTBOX, N%)
          PROC_refreshdialog(D%)
          s$ = n$(N%)
        ENDIF
      ENDIF
      IF K% = 27 THEN = 2
      IF K% = 137 PROC_fsclick(D%, ID_LISTBOX)
      IF E% IF K% = 13 THEN = 1
      = 0

      DEF PROC_fsclick(D%, I%)
      LOCAL a$, d$, t$
      t$ = FN_getdlgitemtext(D%, I%)
      a$ = FN_trim(MID$(t$, LEN(ICON_FILE$) + 1))
      d$ = FN_getdlgitemtext(D%, 102)
      IF a$ = "" ENDPROC
      IF LEFT$(t$, LEN(ICON_FILE$)) = ICON_FILE$ THEN
        PROC_setdlgitemtext(D%, 104, a$)
      ELSE
        IF ASC(a$) = &40 a$ = EVAL(a$) ELSE a$ = d$ + a$ + RIGHT$(@lib$)
        PROC_setdlgitemtext(D%, 102, a$)
        IF FN_getdlgitemtext(D%, 105) <> "." PROC_setlistboxselect(D%, I%, 0)
      ENDIF
      PROC_refreshdialog(D%)
      ENDPROC

      REM Scan a directory and return list of directory and file names
      REM icon1$ is a directory prefix, icon2$ is a file prefix
      DEF FN_dirscan(name$(), dircmd$, filter$, icon1$, icon2$)
      LOCAL C%, F%, I%, N%, a$, d$, type&()
      PRIVATE sort%%
      IF sort%% = 0 sort%% = FN_sortinit(0,0)
      DIM type&(DIM(name$(),1))

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

      IF icon1$ <> "" THEN
        IF N% < DIM(name$(),1) name$(N%) = "@lib$" : type&(N%) = 0 : N% += 1
        IF N% < DIM(name$(),1) name$(N%) = "@usr$" : type&(N%) = 0 : N% += 1
        name$(N%) = ".." : type&(N%) = 0 : N% += 1
      ENDIF
      N% -= 1

      REM Sort the array so directories are listed before programs:
      C% = N%
      CALL sort%%, type&(1), name$(1)

      REM Add icons:
      FOR I% = 1 TO N%
        IF type&(I%) = 2 name$(I%) = icon2$ + name$(I%) ELSE name$(I%) = icon1$ + name$(I%)
      NEXT

      = N%

      DEF FNchops(a$(), s$)
      LOCAL B%, H%, T%
      T% = DIM(a$(),1)
      H% = 2
      WHILE H%<T% H% *= 2:ENDWHILE
      H% /= 2
      REPEAT
        IF (B%+H%)<=T% IF a$(B%+H%)<>"" IF s$>=a$(B%+H%) B% += H%
        H% /= 2
      UNTIL H%=0
      = B%

      REM Simple GUI dialogue box library for BBC BASIC for SDL 2.0
      REM Copyright © 2020 R.T.Russell: http://www.rtrussell.co.uk/
      REM Version 1.14a, 21-Jul-2020

      REM Before use the colour palette must be initialised and the
      REM required font selected. The graphics viewport is modified

      DEF PROC_setdialogpalette
      COLOUR 0, &00,&00,&00 : REM Always black
      COLOUR 7, &F0,&F0,&F0 : REM Primary GUI background colour
      COLOUR 8, &80,&80,&80 : REM Primary GUI foreground colour
      COLOUR 9, &B0,&B0,&B0 : REM Disabled menus/buttons (ORed)
      COLOUR 10,&D0,&D0,&D0 : REM Pushbuttons
      COLOUR 11,&80,&E0,&FF : REM Hotspot highlight AND (first)
      COLOUR 12,&00,&1F,&3F : REM Hotspot highlight OR (second)
      COLOUR 13,&00,&80,&FF : REM Selected text background
      COLOUR 15,&FF,&FF,&FF : REM Selected text fg & scrollbars
      ENDPROC

      DEF FN_newdialog(t$,W%,H%)
      LOCAL d%%,dlg{}
      DIM dlg{x%,y%,w%,h%,link%%,focus%%,nctrl%,backcall%%,bmp$,title$,scale,mouse%%,butt%%}
      DIM d%% DIM(dlg{}) + 2 : d%% = (d%% + 3) AND -4

      IF POS REM SDL thread sync
      PROC_setptr(dlg{},d%%)
      dlg.title$ = t$
      dlg.scale = @char.y% / 4
      IF W%=0 OR H%=0 dlg.scale = 1 : dlg.y% = @vdu%!212 * 2 - 2
      dlg.w% = (W% * dlg.scale + 1.5) AND -2
      dlg.h% = (H% * dlg.scale + 1.5) AND -2

      REM!WC Constants
      DLG_INIT = 1
      DLG_INKEY = 2
      DLG_CLICK = 3
      DLG_GAINFOCUS = 4
      DLG_LOSEFOCUS = 5
      DLG_MOUSEIN = 6
      DLG_MOUSEOUT = 7
      DLG_MOUSEMOVE = 8
      DLG_UNCLICK = 9
      DLG_TIMER = 10
      DLG_DISABLE = 11
      BS_BITMAP = &80
      BS_ICON = &40
      ES_NUMBER = &2000
      LBS_USETABSTOPS = &80
      SS_CENTER = 1
      SS_RIGHT = 2
      WS_DARKMODE = &80000000
      WS_DISABLED = &8000000
      WS_TABSTOP = &10000
      WS_VISIBLE = &10000000
      WS_VSCROLL = &200000
      SCROLLBARTHICK = 24
      MINTHUMB = 48

      = d%%-PAGE+!340

      DEF FN_showdialogex(D%,X%,Y%,F%) : F% AND= WS_DARKMODE
      DEF FN_showdialog(D%,X%,Y%) : LOCAL F%
      LOCAL f%%,H%,I%,l%%,p%%,R%,click%(),ctl{},dlg{}
      DIM click%(2)
      DIM dlg{x%,y%,w%,h%,link%%,focus%%,nctrl%,backcall%%,bmp$,title$,scale}
      DIM ctl{x%,y%,w%,h%,link%%,prev%%,id%,flags%,text$,handler%%}
      PROC_setptr(dlg{},D%+PAGE-!340)

      IF POS REM SDL thread sync.
      IF X%=&80000000 dlg.x% = @vdu%!208 - dlg.w%/2             ELSE dlg.x% = (X% + 1) AND -2
      IF Y%=&80000000 dlg.y% = @vdu%!212 - @char.y% + dlg.h%/2 ELSE dlg.y% = (Y% + 1) AND -2

      ON MOUSE LOCAL click%() = @msg%,@wparam%,@lparam% : RETURN

      H% = (@char.y% * 3 + 1) AND -2
      dlg.bmp$ = @tmp$ + "dlg" + STR$(TIME) + ".tmp.bmp"
      OSCLI "GSAVE """ + dlg.bmp$ + """ " + STR$dlg.x% + "," + STR$(dlg.y%-dlg.h%) + \
      \     "," + STR$(dlg.w%+2) + "," + STR$(dlg.h%+2+H%)
      GCOL 13
      RECTANGLE FILL dlg.x%,dlg.y%,dlg.w%,H%
      GCOL 15
      MOVE dlg.x% + dlg.w%/2 - FN_stringwidth(dlg.title$)/2,dlg.y% + H% - @char.y%/2
      VDU 5 : PRINT dlg.title$; : VDU 4
      GCOL 0
      RECTANGLE dlg.x%,dlg.y%-dlg.h%,dlg.w%,dlg.h% + H%
      GCOL 7 + 128
      VDU 24,dlg.x%+2;dlg.y%-dlg.h%+2;dlg.x%+dlg.w%-2;dlg.y%;
      VDU 16,30,23,16,64|

      l%% = dlg.link%%
      FOR I% = 1 TO dlg.nctrl%
        IF l%% = 0 EXIT FOR
        PROC_setptr(ctl{},l%%)
        ctl.prev%% = p%%
        ctl.flags% = (ctl.flags% AND NOT WS_DARKMODE) OR F%
        IF ctl.flags% AND WS_TABSTOP IF ctl.flags% AND WS_VISIBLE THEN
          IF (ctl.flags% AND WS_DISABLED) = 0 dlg.focus%% = l%%
        ENDIF
        p%% = l%%
        l%% = ctl.link%%
      NEXT
      ctl.link%% = dlg.link%%
      PROC_setptr(ctl{},dlg.link%%)
      ctl.prev%% = p%%

      *REFRESH OFF
      f%% = dlg.focus%%
      IF f%% THEN
        PROC_setptr(ctl{},f%%)
        IF FN(ctl.handler%%)(dlg.x%,dlg.y%,f%%,DLG_GAINFOCUS,0,0)
      ENDIF
      PROC_refreshdialog(D%)

      REPEAT
        R% = FN_polldialog(D%,INKEY(5),click%())
      UNTIL R% > 0

      *REFRESH ON
      f%% = dlg.focus%%
      IF f%% THEN
        PROC_setptr(ctl{},f%%)
        IF FN(ctl.handler%%)(dlg.x%,dlg.y%,f%%,DLG_LOSEFOCUS,0,0)
        dlg.focus%% = 0
      ENDIF

      VDU 24,0;0;@vdu%!208*2-2;@vdu%!212*2-2;4
      = R%

      DEF FN_polldialog(D%,K%,clk%())
      LOCAL B%,C%,f%%,h%%,H%,I%,l%%,R%,X%,Y%,oldx%,oldy%,dlg{},ctl{}
      DIM dlg{x%,y%,w%,h%,link%%,focus%%,nctrl%,backcall%%,bmp$,title$,scale,mouse%%,butt%%}
      DIM ctl{x%,y%,w%,h%,link%%,prev%%,id%,flags%,text$,handler%%}
      PROC_setptr(dlg{},D%+PAGE-!340)

      ON ERROR LOCAL RESTORE LOCAL : OSCLI "REFRESH ON" : ERROR ERR,REPORT$ + LEFT$(" at line " + STR$ERL,ERL)

      VDU 4
      IF POS REM SDL thread sync
      f%% = dlg.focus%%
      CASE K% OF
        WHEN 9,129:
          IF dlg.focus%% = 0 dlg.focus%% = dlg.link%%
          PROC_setptr(ctl{},dlg.focus%%)
          REPEAT
            dlg.focus%% = ctl.prev%%
            IF dlg.focus%% = 0 dlg.focus%% = dlg.link%%
            PROC_setptr(ctl{},dlg.focus%%)
          UNTIL (ctl.flags% AND WS_TABSTOP) <> 0 AND \
          \     (ctl.flags% AND WS_VISIBLE) <> 0 AND \
          \     (ctl.flags% AND WS_DISABLED) = 0
        WHEN 155,128:
          IF dlg.focus%% = 0 dlg.focus%% = dlg.link%%
          PROC_setptr(ctl{},dlg.focus%%)
          REPEAT
            dlg.focus%% = ctl.link%%
            IF dlg.focus%% = 0 dlg.focus%% = dlg.link%%
            PROC_setptr(ctl{},dlg.focus%%)
          UNTIL (ctl.flags% AND WS_TABSTOP) <> 0 AND \
          \     (ctl.flags% AND WS_VISIBLE) <> 0 AND \
          \     (ctl.flags% AND WS_DISABLED) = 0
      ENDCASE

      REM Keypress / timer:
      l%% = dlg.focus%%
      IF l%% THEN
        PROC_setptr(ctl{},l%%)
        IF (ctl.flags% AND WS_DISABLED) = 0 IF ctl.flags% AND WS_VISIBLE THEN
          *REFRESH OFF
          R% = FN(ctl.handler%%)(dlg.x%,dlg.y%,l%%,DLG_TIMER,0,0)
          IF K% <> -1 R% = FN(ctl.handler%%)(dlg.x%,dlg.y%,l%%,DLG_INKEY,K%,0)
          IF R% >= &FF00 h%% = FN_getproc(R%) : PROC(^h%%)(D%,ctl.id%)
          IF K% <> -1 C% = TRUE
        ENDIF
        IF R% >= 1 IF R% <= 9 THEN = R%
      ENDIF

      REM Test mouse:
      MOUSE X%,Y%,B%

      REM Mouse unclick:
      IF B% = 0 IF dlg.butt%% THEN
        PROC_setptr(ctl{},dlg.butt%%)
        IF (ctl.flags% AND WS_DISABLED) = 0 THEN
          *REFRESH OFF
          R% = FN(ctl.handler%%)(dlg.x%,dlg.y%,dlg.butt%%,DLG_UNCLICK,0,0)
          IF R% >= &FF00 h%% = FN_getproc(R%) : PROC(^h%%)(D%,ctl.id%)
          C% = TRUE
        ENDIF
        dlg.butt%% = 0
      ENDIF
      IF R% >= 1 IF R% <= 9 THEN = R%

      REM Mouse movement:
      IF X% <> oldx% OR Y% <> oldy% THEN
        oldx% = X%
        oldy% = Y%
        h%% = 0
        l%% = dlg.link%%
        FOR I% = 1 TO dlg.nctrl%
          PROC_setptr(ctl{},l%%)
          IF X% >= dlg.x%+ctl.x% AND X% < dlg.x%+ctl.x%+ctl.w% AND \
          \  Y% < dlg.y%-ctl.y% AND Y% >= dlg.y%-ctl.y%-ctl.h% THEN
            IF (ctl.flags% AND WS_DISABLED) = 0 IF ctl.flags% AND WS_VISIBLE THEN
              *REFRESH OFF
              R% = FN(ctl.handler%%)(dlg.x%,dlg.y%,l%%,DLG_MOUSEMOVE,X%,Y%)
              h%% = l%%
              IF R% C% = TRUE
            ENDIF
          ENDIF
          l%% = ctl.link%%
        NEXT
        IF h%% <> dlg.mouse%% THEN
          PROC_setptr(ctl{},dlg.mouse%%)
          IF dlg.mouse%% IF (ctl.flags% AND WS_DISABLED) = 0 THEN
            *REFRESH OFF
            R% = FN(ctl.handler%%)(dlg.x%,dlg.y%,dlg.mouse%%,DLG_MOUSEOUT,X%,Y%)
            C% = TRUE
          ENDIF
          dlg.mouse%% = h%%
          PROC_setptr(ctl{},dlg.mouse%%)
          IF dlg.mouse%% IF (ctl.flags% AND WS_DISABLED) = 0 THEN
            *REFRESH OFF
            R% = FN(ctl.handler%%)(dlg.x%,dlg.y%,dlg.mouse%%,DLG_MOUSEIN,X%,Y%)
            C% = TRUE
          ENDIF
        ENDIF
        IF h%%=0 IF X%>=dlg.x% IF X%<dlg.x%+dlg.w% IF Y%<=dlg.y% IF Y%>dlg.y%-dlg.h% MOUSE ON 0
      ENDIF

      REM Mouse click:
      IF clk%(0) THEN
        clk%(0) = FALSE
        X% = (clk%(2) AND &FFFF) * 2
        Y% = (@vdu%!212 - 1 - (clk%(2) >>> 16)) * 2
        l%% = dlg.link%%
        H% = (@char.y% * 3 + 1) AND -2
        IF X% >= dlg.x% IF X% < dlg.x%+dlg.w% IF Y% >= dlg.y% IF Y% < dlg.y%+H% THEN
          PROC_drag(dlg{})
        ELSE
          PROC_setptr(ctl{},l%%)
          FOR I% = 1 TO dlg.nctrl%
            l%% = ctl.prev%% : REM So comboboxes are found before underlying controls
            PROC_setptr(ctl{},l%%)
            IF X% >= dlg.x%+ctl.x% AND X% < dlg.x%+ctl.x%+ctl.w% AND \
            \  Y% < dlg.y%-ctl.y% AND Y% >= dlg.y%-ctl.y%-ctl.h% THEN
              IF (ctl.flags% AND WS_DISABLED) = 0 IF ctl.flags% AND WS_VISIBLE THEN
                *REFRESH OFF
                R% = FN(ctl.handler%%)(dlg.x%,dlg.y%,l%%,DLG_CLICK,X%,Y%)
                IF R% >= &FF00 h%% = FN_getproc(R%) : PROC(^h%%)(D%,ctl.id%)
                dlg.focus%% = l%%
                dlg.butt%% = l%%
                C% = TRUE
                EXIT FOR
              ENDIF
            ENDIF
          NEXT
        ENDIF
        IF C% = FALSE THEN = TRUE
      ENDIF
      IF R% >= 1 IF R% <= 9 THEN = R%

      REM Focus changed:
      IF dlg.focus%% <> f%% THEN
        IF f%% THEN
          PROC_setptr(ctl{},f%%)
          IF f%% IF (ctl.flags% AND WS_DISABLED) = 0 THEN
            *REFRESH OFF
            R% = FN(ctl.handler%%)(dlg.x%,dlg.y%,f%%,DLG_LOSEFOCUS,0,0)
            C% = TRUE
          ENDIF
        ENDIF
        f%% = dlg.focus%%
        PROC_setptr(ctl{},f%%)
        IF f%% IF (ctl.flags% AND WS_DISABLED) = 0 THEN
          *REFRESH OFF
          R% = FN(ctl.handler%%)(dlg.x%,dlg.y%,f%%,DLG_GAINFOCUS,0,0)
          C% = TRUE
        ENDIF
      ENDIF

      IF C% THEN *REFRESH
      IF dlg.backcall%% R% = FN(^dlg.backcall%%)(D%,K%)
      IF R% >= 1 IF R% <= 9 THEN = R%

      *REFRESH ON
      = 0

      DEF PROC_drag(dlg{})
      LOCAL B%,H%,O%,P%,X%,Y%
      MOUSE ON 134
      MOUSE O%,P%,B%
      H% = (@char.y% * 3 + 3) AND -2
      OSCLI "GSAVE """ + @tmp$ + "dragtmp"" " + STR$dlg.x% + "," + STR$(dlg.y%-dlg.h%) + \
      \     "," + STR$(dlg.w%+2) + "," + STR$(dlg.h%+H%)
      OSCLI "DISPLAY """ + dlg.bmp$ + """ " + STR$dlg.x% + "," + STR$(dlg.y%-dlg.h%)
      GCOL 4,0
      VDU 26,23,23,2|
      REPEAT
        RECTANGLE dlg.x%, dlg.y%-dlg.h%, dlg.w%, dlg.h%+H%
        *REFRESH
        WAIT 4
        RECTANGLE dlg.x%, dlg.y%-dlg.h%, dlg.w%, dlg.h%+H%
        MOUSE X%,Y%,B%
        dlg.x% += X% - O% : IF dlg.x% < 0 dlg.x% = 0
        dlg.y% += Y% - P% : IF dlg.y% >= @vdu%!212*2-H% dlg.y% = @vdu%!212*2-H%
        O% = X% : P% = Y%
      UNTIL B%=0
      VDU 23,23,1|
      VDU 24,dlg.x%+2;dlg.y%-dlg.h%+2;dlg.x%+dlg.w%-2;dlg.y%;
      OSCLI "GSAVE """ + dlg.bmp$ + """ " + STR$dlg.x% + "," + STR$(dlg.y%-dlg.h%) + \
      \     "," + STR$(dlg.w%+2) + "," + STR$(dlg.h%+H%)
      OSCLI "DISPLAY """ + @tmp$ + "dragtmp"" " + STR$dlg.x% + "," + STR$(dlg.y%-dlg.h%)
      *REFRESH
      OSCLI "DEL """ + @tmp$ + "dragtmp.bmp"""
      MOUSE ON 0
      ENDPROC

      DEF PROC_closedialog(D%)
      LOCAL b$,dlg{}
      IF D% = 0 ENDPROC
      DIM dlg{x%,y%,w%,h%,link%%,focus%%,nctrl%,backcall%%,bmp$}
      PROC_setptr(dlg{},D%+PAGE-!340)
      SWAP b$,dlg.bmp$
      IF b$ <> "" THEN
        OSCLI "DISPLAY """ + b$ + """ " + STR$dlg.x% + "," + STR$(dlg.y%-dlg.h%)
        OSCLI "DEL """ + b$ + """"
      ENDIF
      ENDPROC

      DEF PROC_refreshdialogex(D%,X%,Y%)
      DEF PROC_refreshdialog(D%) : LOCAL X%,Y%
      LOCAL l%%,p%%,N%,dlg{},ctl{}
      DIM dlg{x%,y%,w%,h%,link%%,focus%%,nctrl%}
      DIM ctl{x%,y%,w%,h%,link%%,prev%%,id%,flags%,text$,handler%%}
      PROC_setptr(dlg{},D%+PAGE-!340)
      l%% = dlg.link%%
      FOR N% = 1 TO dlg.nctrl%
        IF l%% = 0 EXIT FOR
        PROC_setptr(ctl{},l%%)
        IF p%% ctl.prev%% = p%%
        IF ctl.flags% AND WS_VISIBLE THEN
          IF ctl.flags% AND WS_DISABLED THEN
            IF dlg.focus%% = l%% THEN
              IF FN(ctl.handler%%)(dlg.x%,dlg.y%,l%%,DLG_LOSEFOCUS,0,0)
              dlg.focus%% = 0
            ENDIF
            IF FN(ctl.handler%%)(dlg.x%,dlg.y%,l%%,DLG_INIT,X%,Y%)
            IF FN(ctl.handler%%)(dlg.x%,dlg.y%,l%%,DLG_DISABLE,0,0)
          ELSE
            IF FN(ctl.handler%%)(dlg.x%,dlg.y%,l%%,DLG_INIT,X%,Y%)
          ENDIF
        ENDIF
        p%% = l%%
        l%% = ctl.link%%
      NEXT
      IF ctl.link%% = 0 ctl.link%% = dlg.link%%
      PROC_setptr(ctl{},ctl.link%%)
      ctl.prev%% = p%%
      *REFRESH
      ENDPROC

      DEF PROC_setdlgitemtext(D%,I%,t$)
      LOCAL L%,ctl{}
      DIM ctl{x%,y%,w%,h%,link%%,prev%%,id%,flags%,text$,handler%%,focus%,caret%}
      L% = FN_getdlgitem(D%,I%)
      IF L% = 0 ENDPROC
      PROC_setptr(ctl{},L%+PAGE-!340)
      IF ctl.text$ = t$ ENDPROC
      ctl.text$ = t$
      IF ctl.handler%% = ^FN_textbox@dlg() IF NOT ctl.focus% ctl.caret% = LEN(t$)
      ENDPROC

      DEF FN_getdlgitemtext(D%,I%)
      LOCAL L%,ctl{}
      DIM ctl{x%,y%,w%,h%,link%%,prev%%,id%,flags%,text$,handler%%}
      L% = FN_getdlgitem(D%,I%)
      IF L% = 0 THEN = ""
      PROC_setptr(ctl{},L%+PAGE-!340)
      = ctl.text$

      DEF PROC_checkdlgitem(D%,I%,C%)
      LOCAL L%,ctl{}
      DIM ctl{x%,y%,w%,h%,link%%,prev%%,id%,flags%,text$,handler%%,focus%,checked%}
      L% = FN_getdlgitem(D%,I%)
      IF L% = 0 ENDPROC
      PROC_setptr(ctl{},L%+PAGE-!340)
      ctl.checked% = C% <> 0
      ENDPROC

      DEF FN_isdlgitemchecked(D%,I%)
      LOCAL L%,ctl{}
      DIM ctl{x%,y%,w%,h%,link%%,prev%%,id%,flags%,text$,handler%%,focus%,checked%}
      L% = FN_getdlgitem(D%,I%)
      IF L% = 0 THEN = 0
      PROC_setptr(ctl{},L%+PAGE-!340)
      = ctl.checked%

      DEF PROC_enabledlgitem(D%,I%,E%)
      LOCAL L%,ctl{}
      DIM ctl{x%,y%,w%,h%,link%%,prev%%,id%,flags%,text$,handler%%}
      L% = FN_getdlgitem(D%,I%)
      IF L% = 0 ENDPROC
      PROC_setptr(ctl{},L%+PAGE-!340)
      IF E% THEN
        ctl.flags% AND= NOT WS_DISABLED
      ELSE
        ctl.flags% OR= WS_DISABLED
      ENDIF
      ENDPROC

      DEF PROC_showdlgitem(D%,I%,S%)
      LOCAL L%,ctl{}
      DIM ctl{x%,y%,w%,h%,link%%,prev%%,id%,flags%,text$,handler%%}
      L% = FN_getdlgitem(D%,I%)
      IF L% = 0 ENDPROC
      PROC_setptr(ctl{},L%+PAGE-!340)
      IF S% THEN
        ctl.flags% OR= WS_VISIBLE
      ELSE
        ctl.flags% AND= NOT WS_VISIBLE
      ENDIF
      ENDPROC

      REM Must be called before PROC_setlistboxarray(), requires LBS_USETABSTOPS style
      REM The array should contain a zero-based list of column widths in dialog-box units
      DEF PROC_setlistboxcols(D%,I%,c%(),N%)
      LOCAL L%,dlg{},lb{}
      DIM dlg{x%,y%,w%,h%,link%%,focus%%,nctrl%,backcall%%,bmp$,title$,scale}
      DIM lb{x%,y%,w%,h%,link%%,prev%%,id%,flags%,text$,handler%%,focus%,array%%,num%,\
      \      select%,scroll%,visible%,acc%,drag%,hot%,timer%,prevy%,col%(9)}
      L% = FN_getdlgitem(D%,I%)
      IF L% = 0 THEN ENDPROC
      PROC_setptr(dlg{},D%+PAGE-!340)
      PROC_setptr(lb{},L%+PAGE-!340)
      IF lb.handler%% <> ^FN_listbox@dlg() ENDPROC
      FOR I% = 0 TO N%-1
        lb.col%(I%) = dlg.scale * c%(I%)
      NEXT
      ENDPROC

      REM Only one listbox may have the LBS_USETABSTOPS style; if this style is set then
      REM PROC_setlistboxarray() must be called whenever the array contents are updated.
      DEF PROC_setlistboxarray(D%,I%,RETURN a%%,N%)
      LOCAL L%,lb{}
      DIM lb{x%,y%,w%,h%,link%%,prev%%,id%,flags%,text$,handler%%,focus%,array%%,num%,\
      \      select%,scroll%,visible%,acc%,drag%,hot%,timer%,prevy%,col%(9)}
      L% = FN_getdlgitem(D%,I%)
      IF L% = 0 THEN ENDPROC
      PROC_setptr(lb{},L%+PAGE-!340)
      IF lb.handler%% <> ^FN_listbox@dlg() ENDPROC
      IF a%% <> lb.array%% OR N% <> lb.num% lb.select% = 0 : lb.scroll% = 0
      lb.array%% = a%%
      lb.num% = N%
      IF lb.flags% AND LBS_USETABSTOPS THEN
        LOCAL T%,a$,a$() : PRIVATE t$()
        IF INKEY(-256)=&73 IF @platform% AND &40 ]^a$() = lb.array%% ELSE !^a$() = lb.array%%
        DIM t$(DIM(a$(),1)) : t$() = ""
        IF INKEY(-256)=&73 IF @platform% AND &40 lb.array%% = ]^t$() ELSE lb.array%% = !^t$()
        FOR I% = 0 TO N%
          a$ = a$(I%)
          D% = 0
          REPEAT
            L% = INSTR(a$,CHR$9)
            IF L% THEN
              WHILE WIDTH(LEFT$(a$, L%-1)+" | ") >= lb.col%(D%)
                a$ = "..." + MID$(a$,5) : L% -= 1 : IF L% = 0 EXIT WHILE
              ENDWHILE
              T% = lb.col%(D%) - FN_stringwidth(LEFT$(a$, L%-1)+"| ")
              t$(I%) += LEFT$(a$, L%-1) : a$ = MID$(a$, L%+1)
              t$(I%) += CHR$25 + CHR$0 + CHR$(T%MOD256) + CHR$(T%DIV256) + CHR$0 + CHR$0
              t$(I%) += CHR$18 + CHR$0 + CHR$10 + " | " + CHR$18 + CHR$0 + CHR$0
              D% += 1
            ENDIF
          UNTIL L% = 0
          t$(I%) += a$
        NEXT
      ENDIF
      ENDPROC

      DEF PROC_setlistboxselect(D%,I%,S%)
      LOCAL L%,lb{}
      DIM lb{x%,y%,w%,h%,link%%,prev%%,id%,flags%,text$,handler%%,focus%,\
      \      array%%,num%,select%,scroll%,visible%}
      L% = FN_getdlgitem(D%,I%)
      IF L% = 0 ENDPROC
      PROC_setptr(lb{},L%+PAGE-!340)
      IF lb.handler%% <> ^FN_listbox@dlg() ENDPROC
      lb.select% = S%
      IF lb.select% > 0 WHILE (lb.select% - 1) * 2 * @char.y% <= lb.scroll% lb.scroll% -= 1 : ENDWHILE
      WHILE lb.select% * 2 * @char.y% > (lb.scroll% + lb.visible%) lb.scroll% += 1 : ENDWHILE
      ENDPROC

      DEF FN_getlistboxselect(D%,I%)
      LOCAL L%,lb{}
      DIM lb{x%,y%,w%,h%,link%%,prev%%,id%,flags%,text$,handler%%,focus%,array%%,num%,select%}
      L% = FN_getdlgitem(D%,I%)
      IF L% = 0 THEN = 0
      PROC_setptr(lb{},L%+PAGE-!340)
      IF lb.handler%% <> ^FN_listbox@dlg() THEN = 0
      = lb.select%

      DEF PROC_setlistboxscroll(D%,I%,S%)
      LOCAL L%,lb{}
      DIM lb{x%,y%,w%,h%,link%%,prev%%,id%,flags%,text$,handler%%,focus%,array%%,num%,select%,scroll%,visible%}
      L% = FN_getdlgitem(D%,I%)
      IF L% = 0 ENDPROC
      PROC_setptr(lb{},L%+PAGE-!340)
      lb.scroll% = S%
      ENDPROC

      DEF FN_getlistboxscroll(D%,I%)
      LOCAL L%,lb{}
      DIM lb{x%,y%,w%,h%,link%%,prev%%,id%,flags%,text$,handler%%,focus%,array%%,num%,select%,scroll%,visible%}
      L% = FN_getdlgitem(D%,I%)
      IF L% = 0 THEN = 0
      PROC_setptr(lb{},L%+PAGE-!340)
      = lb.scroll%

      DEF PROC_setdlgfocus(D%,I%)
      LOCAL f%%,dlg{},ctl{}
      DIM dlg{x%,y%,w%,h%,link%%,focus%%}
      DIM ctl{x%,y%,w%,h%,link%%,prev%%,id%,flags%,text$,handler%%}
      PROC_setptr(dlg{},D%+PAGE-!340)
      f%% = dlg.focus%%
      IF f%% THEN
        PROC_setptr(ctl{},f%%)
        IF FN(ctl.handler%%)(dlg.x%,dlg.y%,f%%,DLG_LOSEFOCUS,0,0)
      ENDIF
      f%% = FN_getdlgitem(D%,I%)+PAGE-!340
      IF f%% THEN
        PROC_setptr(ctl{},f%%)
        IF FN(ctl.handler%%)(dlg.x%,dlg.y%,f%%,DLG_GAINFOCUS,0,0)
      ENDIF
      dlg.focus%% = f%%
      ENDPROC

      DEF FN_getdlgfocus(D%)
      LOCAL f%%,dlg{},ctl{}
      DIM dlg{x%,y%,w%,h%,link%%,focus%%}
      DIM ctl{x%,y%,w%,h%,link%%,prev%%,id%,flags%,text$,handler%%}
      PROC_setptr(dlg{},D%+PAGE-!340)
      f%% = dlg.focus%%
      IF f%% = 0 THEN = 0
      PROC_setptr(ctl{},f%%)
      = ctl.id%

      DEF PROC_getdlgrect(D%,RETURN X%,RETURN Y%,RETURN W%, RETURN H%)
      LOCAL dlg{}
      DIM dlg{x%,y%,w%,h%,link%,focus%}
      PROC_setptr(dlg{},D%+PAGE-!340)
      X% = dlg.x% : Y% = dlg.y% : W% = dlg.w% : H% = dlg.h%
      ENDPROC

      DEF PROC_setdlgrect(D%,X%,Y%,W%,H%)
      LOCAL dlg{}
      DIM dlg{x%,y%,w%,h%,link%,focus%}
      PROC_setptr(dlg{},D%+PAGE-!340)
      dlg.x% = X% : dlg.y% = Y% : dlg.w% = W% : dlg.h% = H%
      ENDPROC

      DEF PROC_registerdlgcallback(D%,RETURN f%%)
      LOCAL dlg{}
      DIM dlg{x%,y%,w%,h%,link%%,focus%%,nctrl%,backcall%%}
      PROC_setptr(dlg{},D%+PAGE-!340)
      dlg.backcall%% = f%%
      ENDPROC

      DEF PROC_setdialogtitle(D%,t$)
      LOCAL dlg{}
      DIM dlg{x%,y%,w%,h%,link%%,focus%%,nctrl%,backcall%%,bmp$,title$}
      PROC_setptr(dlg{},D%+PAGE-!340)
      dlg.title$ = t$
      ENDPROC

      DEF FN_getdlgitem(D%,I%)
      LOCAL l%%,N%,dlg{},ctl{}
      DIM dlg{x%,y%,w%,h%,link%%,focus%%,nctrl%}
      DIM ctl{x%,y%,w%,h%,link%%,prev%%,id%,flags%,text$,handler%%}
      PROC_setptr(dlg{},D%+PAGE-!340)
      l%% = dlg.link%%
      FOR N% = 1 TO dlg.nctrl%
        IF l%% = 0 EXIT FOR
        PROC_setptr(ctl{},l%%)
        IF ctl.id% = I% THEN = l%%-PAGE+!340
        l%% = ctl.link%%
      NEXT
      = 0

      DEF PROC_static(D%,t$,I%,X%,Y%,W%,H%,F%)
      LOCAL p%%,dlg{},ctl{}
      DIM dlg{x%,y%,w%,h%,link%%,focus%%,nctrl%,backcall%%,bmp$,title$,scale}
      DIM ctl{x%,y%,w%,h%,link%%,prev%%,id%,flags%,text$,handler%%}
      DIM p%% DIM(ctl{}) + 2 : p%% = (p%% + 3) AND -4
      PROC_setptr(dlg{},D%+PAGE-!340)
      PROC_setptr(ctl{},p%%)
      ctl.x% = (X% * dlg.scale + 1.5) AND -2
      ctl.y% = (Y% * dlg.scale + 1.5) AND -2
      ctl.w% = (W% * dlg.scale + 1.5) AND -2
      ctl.h% = (H% * dlg.scale + 1.5) AND -2
      ctl.id% = I%
      ctl.flags% = F% OR WS_VISIBLE
      ctl.text$ = t$
      ctl.link%% = dlg.link%%
      ctl.handler%% = ^FN_static@dlg()
      dlg.link%% = p%%
      dlg.nctrl% += 1
      ENDPROC
      DEF FN_static@dlg(ox%,oy%,p%%,C%,X%,Y%)
      LOCAL ctl{}
      DIM ctl{x%,y%,w%,h%,link%%,prev%%,id%,flags%,text$,handler%%}
      PROC_setptr(ctl{},p%%)
      VDU 24,ox%+ctl.x%+2;oy%-ctl.y%-ctl.h%+2;ox%+ctl.x%+ctl.w%-2;oy%-ctl.y%;
      VDU 23,16,64|
      CASE C% OF
        WHEN DLG_INIT:
          GCOL 7 + 128 : CLG
          GCOL 0
          MOVE ox% + ctl.x%,oy% - ctl.y%
          IF ctl.flags% AND SS_RIGHT THEN
            PLOT 0,ctl.w% - FN_stringwidth(ctl.text$),0
          ENDIF
          IF ctl.flags% AND SS_CENTER THEN
            PLOT 0,ctl.w% DIV 2 - FN_stringwidth(ctl.text$) DIV 2,0
          ENDIF
          VDU 5 : PRINT ctl.text$; : VDU 4
        WHEN DLG_DISABLE:
          IF ctl.flags% AND WS_DARKMODE GCOL 2,9 + 128 : CLG ELSE GCOL 1,9 + 128 : CLG
      ENDCASE
      = FALSE

      DEF PROC_textbox(D%,t$,I%,X%,Y%,W%,H%,F%)
      LOCAL p%%,dlg{},tb{}
      DIM dlg{x%,y%,w%,h%,link%%,focus%%,nctrl%,backcall%%,bmp$,title$,scale}
      DIM tb{x%,y%,w%,h%,link%%,prev%%,id%,flags%,text$,handler%%,focus%,caret%,anchor%,scroll%,drag%}
      DIM p%% DIM(tb{}) + 2 : p%% = (p%% + 3) AND -4
      PROC_setptr(dlg{},D%+PAGE-!340)
      PROC_setptr(tb{},p%%)
      tb.x% = (X% * dlg.scale + 1.5) AND -2
      tb.y% = (Y% * dlg.scale + 1.5) AND -2
      tb.w% = (W% * dlg.scale + 1.5) AND -2
      tb.h% = (H% * dlg.scale + 1.5) AND -2
      tb.id% = I%
      tb.flags% = F% EOR (WS_TABSTOP OR WS_VISIBLE)
      tb.text$ = t$
      tb.caret% = LEN(t$)
      tb.link%% = dlg.link%%
      tb.handler%% = ^FN_textbox@dlg()
      dlg.link%% = p%%
      dlg.nctrl% += 1
      ENDPROC
      DEF FN_textbox@dlg(ox%,oy%,p%%,C%,X%,Y%)
      LOCAL tb{}
      DIM tb{x%,y%,w%,h%,link%%,prev%%,id%,flags%,text$,handler%%,focus%,caret%,anchor%,scroll%,drag%}
      PROC_setptr(tb{},p%%)
      VDU 24,ox%+tb.x%+2;oy%-tb.y%-tb.h%+2;ox%+tb.x%+tb.w%-2;oy%-tb.y%;
      VDU 23,16,64|
      CASE C% OF
        WHEN DLG_INIT:
          PROCdrawtext@dlg(ox%+tb.x%,oy%-tb.y%,tb.w%,tb.h%,\
          \  tb.caret%,tb.anchor%,tb.scroll%,tb.focus%,tb.text$)
        WHEN DLG_LOSEFOCUS:
          OFF
          tb.anchor% = tb.caret%
          tb.focus% = FALSE
          tb.drag% = FALSE
          PROCdrawtext@dlg(ox%+tb.x%,oy%-tb.y%,tb.w%,tb.h%,\
          \  tb.caret%,tb.anchor%,tb.scroll%,tb.focus%,tb.text$)
        WHEN DLG_GAINFOCUS:
          ON
          IF INKEY(-256)<>&57 IF (@platform% AND &F) >= 3 IF X% = 0 THEN *OSK ON
          IF NOT tb.focus% tb.caret% = LEN(tb.text$) : tb.anchor% = 0 : tb.scroll% = 0
          tb.focus% = TRUE
          PROCdrawtext@dlg(ox%+tb.x%,oy%-tb.y%,tb.w%,tb.h%,\
          \  tb.caret%,tb.anchor%,tb.scroll%,tb.focus%,tb.text$)
        WHEN DLG_INKEY:
          CASE X% OF
            WHEN 1: REM Select All
              tb.anchor% = 0
              tb.caret% = LEN(tb.text$)
            WHEN 3: REM Copy
              X% = tb.caret% : Y% = tb.anchor%
              IF Y% < X% SWAP X%,Y%
              IF Y% > X% PROC_putclipboardtext(MID$(tb.text$,X%,Y%-X%+1))
              = FALSE : REM Don't clear selection
            WHEN 22: REM Paste
              PROCdelsel@dlg(tb.caret%,tb.anchor%,tb.text$)
              Y% = LEN(tb.text$)
              tb.text$ = LEFT$(tb.text$,tb.caret%) + FNgetclipboardline@dlg + \
              \          MID$(tb.text$,tb.caret% + 1)
              tb.caret% += LEN(tb.text$) - Y%
            WHEN 24: REM Cut
              Y% = FN(tb.handler%%)(ox%,oy%,p%%,C%,3,0)
              Y% = FN(tb.handler%%)(ox%,oy%,p%%,C%,135,0)
            WHEN 8,127: REM Backspace
              IF tb.caret% <> tb.anchor% THEN
                PROCdelsel@dlg(tb.caret%,tb.anchor%,tb.text$)
              ELSE
                IF tb.caret% THEN
                  tb.caret% -= 1
                  tb.text$ = LEFT$(tb.text$,tb.caret%) + MID$(tb.text$,tb.caret%+2)
                ENDIF
              ENDIF
            WHEN 135: REM Delete
              IF tb.caret% <> tb.anchor% THEN
                PROCdelsel@dlg(tb.caret%,tb.anchor%,tb.text$)
              ELSE
                IF tb.caret% < LEN(tb.text$) THEN
                  tb.text$ = LEFT$(tb.text$,tb.caret%) + MID$(tb.text$,tb.caret%+2)
                ENDIF
              ENDIF
            WHEN 130: REM Home
              tb.caret% = 0
            WHEN 131: REM End
              tb.caret% = LEN(tb.text$)
            WHEN 136,139: REM Left/Up
              IF tb.caret% tb.caret% -= 1
              REPEAT UNTIL INKEY(0) = -1
            WHEN 137,138: REM Right/Down
              IF tb.caret% < LEN(tb.text$) tb.caret% += 1
              REPEAT UNTIL INKEY(0) = -1
            OTHERWISE:
              IF tb.flags% AND ES_NUMBER IF X% < 48 OR X% > 57 THEN = FALSE
              IF X% >= 32 AND X% < 127 OR X% >= 160 THEN
                PROCdelsel@dlg(tb.caret%,tb.anchor%,tb.text$)
                tb.text$ = LEFT$(tb.text$,tb.caret%) + \
                \          CHR$X% + MID$(tb.text$,tb.caret%+1)
                tb.caret% += 1
                tb.anchor% = tb.caret%
              ELSE
                = FALSE
              ENDIF
          ENDCASE
          IF NOT INKEY(-1) IF X%<>1 tb.anchor% = tb.caret%
          PROCdrawtext@dlg(ox%+tb.x%,oy%-tb.y%,tb.w%,tb.h%,\
          \  tb.caret%,tb.anchor%,tb.scroll%,tb.focus%,tb.text$)
          = TRUE
        WHEN DLG_CLICK,DLG_MOUSEMOVE:
          IF C% = DLG_CLICK tb.focus% = TRUE
          IF C% <> DLG_CLICK IF NOT tb.drag% THEN = FALSE
          X% -= ox% + tb.x% - tb.scroll% : Y% = 0
          C% = 1 : WHILE C% <= LEN(tb.text$) C% *= 2 : ENDWHILE
          REPEAT
            C% DIV= 2
            IF X% > FN_stringwidth(LEFT$(tb.text$,Y%+C%)) Y% += C%
          UNTIL C% = 0
          IF Y% > LEN(tb.text$) Y% = LEN(tb.text$)
          tb.caret% = Y%
          IF NOT tb.drag% IF NOT INKEY(-1) tb.anchor% = tb.caret%
          tb.drag% = TRUE
          PROCdrawtext@dlg(ox%+tb.x%,oy%-tb.y%,tb.w%,tb.h%,\
          \  tb.caret%,tb.anchor%,tb.scroll%,tb.focus%,tb.text$)
          = TRUE : REM To force refresh
        WHEN DLG_MOUSEIN:
          MOUSE ON 1
        WHEN DLG_MOUSEOUT:
          MOUSE ON 0
        WHEN DLG_UNCLICK:
          tb.drag% = FALSE
        WHEN DLG_DISABLE:
          IF tb.flags% AND WS_DARKMODE GCOL 2,9 + 128 : CLG ELSE GCOL 1,9 + 128 : CLG
      ENDCASE
      = FALSE

      DEF PROC_checkbox(D%,t$,I%,X%,Y%,W%,H%,F%)
      LOCAL p%%,dlg{},cb{}
      DIM dlg{x%,y%,w%,h%,link%%,focus%%,nctrl%,backcall%%,bmp$,title$,scale}
      DIM cb{x%,y%,w%,h%,link%%,prev%%,id%,flags%,text$,handler%%,focus%,checked%,mouse%}
      DIM p%% DIM(cb{}) + 2 : p%% = (p%% + 3) AND -4
      PROC_setptr(dlg{},D%+PAGE-!340)
      PROC_setptr(cb{},p%%)
      cb.x% = (X% * dlg.scale + 1.5) AND -2
      cb.y% = (Y% * dlg.scale + 1.5) AND -2
      cb.w% = (W% * dlg.scale + 1.5) AND -2
      cb.h% = (H% * dlg.scale + 1.5) AND -2
      cb.id% = I%
      cb.flags% = F% EOR (WS_TABSTOP OR WS_VISIBLE)
      cb.text$ = t$
      cb.checked% = (F% AND 1) <> 0
      cb.link%% = dlg.link%%
      cb.handler%% = ^FN_checkbox@dlg()
      dlg.link%% = p%%
      dlg.nctrl% += 1
      ENDPROC
      DEF FN_checkbox@dlg(ox%,oy%,p%%,C%,X%,Y%)
      LOCAL cb{}
      DIM cb{x%,y%,w%,h%,link%%,prev%%,id%,flags%,text$,handler%%,focus%,checked%,mouse%}
      PROC_setptr(cb{},p%%)
      VDU 24,ox%+cb.x%+2;oy%-cb.y%-cb.h%+2;ox%+cb.x%+cb.w%-2;oy%-cb.y%;
      VDU 23,16,64|
      CASE C% OF
        WHEN DLG_INIT,DLG_MOUSEOUT:
          IF C% = DLG_MOUSEOUT cb.mouse% = FALSE
          GCOL 7 + 128 : CLG
          GCOL 15 : RECTANGLE FILL ox%+cb.x%,oy%-cb.y%,2*@char.y%,-2*@char.y%
          GCOL 0  : RECTANGLE ox%+cb.x%+2,oy%-cb.y%,2*@char.y%,-2*@char.y%
          MOVE ox% + cb.x% + 2 * @char.y% + 16,oy% - cb.y% - 2
          VDU 5 : PRINT cb.text$; : VDU 4
          IF cb.checked% THEN
            MOVE ox%+cb.x%+@char.y%/2+2,oy%-cb.y%-@char.y%
            VDU 23,23,3|
            PLOT 9,@char.y%/2,-@char.y%/2
            VDU 23,23,2|
            PLOT 9,@char.y%/2,@char.y%+4
            VDU 23,23,1|
          ENDIF
          IF cb.focus% IF FN(cb.handler%%)(ox%,oy%,p%%,DLG_GAINFOCUS,0,0)
          IF cb.mouse% IF FN(cb.handler%%)(ox%,oy%,p%%,DLG_MOUSEIN,X%,Y%)
        WHEN DLG_GAINFOCUS,DLG_LOSEFOCUS:
          MOVE ox%+cb.x%+2*@char.y%+8,oy%-cb.y%
          PLOT 18,cb.w%-2*@char.y%-16,0 : PLOT 18,0,-2*@char.y%
          PLOT 18,-cb.w%+2*@char.y%+16,0 : PLOT 18,0,2*@char.y%
          IF C% = DLG_GAINFOCUS cb.focus% = TRUE ELSE cb.focus% = FALSE
        WHEN DLG_INKEY,DLG_CLICK:
          IF C% = DLG_CLICK OR X% = 32 THEN
            cb.checked% = NOT cb.checked%
            IF cb.flags% AND WS_DARKMODE THEN
              IF cb.checked% GCOL 1,0 ELSE GCOL 2,15
            ELSE
              IF cb.checked% GCOL 2,0 ELSE GCOL 1,15
            ENDIF
            MOVE ox%+cb.x%+@char.y%/2+2,oy%-cb.y%-@char.y%
            VDU 23,23,3|
            PLOT 9,@char.y%/2,-@char.y%/2
            VDU 23,23,2|
            PLOT 9,@char.y%/2,@char.y%+4
            VDU 23,23,1|
            = cb.id%
          ENDIF
        WHEN DLG_MOUSEIN:
          cb.mouse% = TRUE
          GCOL 2,11 : RECTANGLE FILL ox%+cb.x%,oy%-cb.y%,2*@char.y%,-2*@char.y%
          GCOL 1,12 : RECTANGLE FILL ox%+cb.x%,oy%-cb.y%,2*@char.y%,-2*@char.y%
          MOUSE ON 0
        WHEN DLG_DISABLE:
          IF FN(cb.handler%%)(ox%,oy%,p%%,DLG_MOUSEOUT,X%,Y%)
          IF cb.flags% AND WS_DARKMODE GCOL 2,9 + 128 : CLG ELSE GCOL 1,9 + 128 : CLG
      ENDCASE
      = FALSE

      DEF PROC_radiobutton(D%,t$,I%,X%,Y%,W%,H%,F%)
      LOCAL p%%,dlg{},rb{}
      DIM dlg{x%,y%,w%,h%,link%%,focus%%,nctrl%,backcall%%,bmp$,title$,scale}
      DIM rb{x%,y%,w%,h%,link%%,prev%%,id%,flags%,text$,handler%%,focus%,checked%,mouse%}
      DIM p%% DIM(rb{}) + 2 : p%% = (p%% + 3) AND -4
      PROC_setptr(dlg{},D%+PAGE-!340)
      PROC_setptr(rb{},p%%)
      rb.x% = (X% * dlg.scale + 1.5) AND -2
      rb.y% = (Y% * dlg.scale + 1.5) AND -2
      rb.w% = (W% * dlg.scale + 1.5) AND -2
      rb.h% = (H% * dlg.scale + 1.5) AND -2
      rb.id% = I%
      rb.flags% = F% EOR (WS_TABSTOP OR WS_VISIBLE)
      rb.text$ = t$
      rb.checked% = (F% AND 1) <> 0
      rb.link%% = dlg.link%%
      rb.handler%% = ^FN_radiobutton@dlg()
      dlg.link%% = p%%
      dlg.nctrl% += 1
      ENDPROC
      DEF FN_radiobutton@dlg(ox%,oy%,p%%,C%,X%,Y%)
      LOCAL rb{},x%%,y%%
      DIM rb{x%,y%,w%,h%,link%%,prev%%,id%,flags%,text$,handler%%,focus%,checked%,mouse%}
      PROC_setptr(rb{},p%%)
      VDU 24,ox%+rb.x%+2;oy%-rb.y%-rb.h%+2;ox%+rb.x%+rb.w%-2;oy%-rb.y%;
      VDU 23,16,64|
      CASE C% OF
        WHEN DLG_INIT,DLG_MOUSEOUT:
          IF C% = DLG_MOUSEOUT rb.mouse% = FALSE
          GCOL 7 + 128 : CLG
          GCOL 15 : CIRCLE FILL ox%+rb.x%+@char.y%+2,oy%-rb.y%-@char.y%,@char.y%
          GCOL 0  : CIRCLE ox%+rb.x%+@char.y%+2,oy%-rb.y%-@char.y%,@char.y%
          MOVE ox% + rb.x% + 2 * @char.y% + 16,oy% - rb.y% - 2
          VDU 5 : PRINT rb.text$; : VDU 4
          IF rb.checked% THEN
            CIRCLE FILL ox%+rb.x%+@char.y%+2,oy%-rb.y%-@char.y%,@char.y%/2
          ENDIF
          IF rb.focus% IF FN(rb.handler%%)(ox%,oy%,p%%,DLG_GAINFOCUS,0,0)
          IF rb.mouse% IF FN(rb.handler%%)(ox%,oy%,p%%,DLG_MOUSEIN,X%,Y%)
        WHEN DLG_GAINFOCUS,DLG_LOSEFOCUS:
          MOVE ox%+rb.x%+2*@char.y%+8,oy%-rb.y%
          PLOT 18,rb.w%-2*@char.y%-16,0 : PLOT 18,0,-2*@char.y%
          PLOT 18,-rb.w%+2*@char.y%+16,0 : PLOT 18,0,2*@char.y%
          IF C% = DLG_GAINFOCUS rb.focus% = TRUE ELSE rb.focus% = FALSE
        WHEN DLG_INKEY,DLG_CLICK:
          IF C% = DLG_CLICK OR X% = 32 THEN
            rb.checked% = TRUE
            C% = rb.id%
            GCOL 0
            CIRCLE FILL ox%+rb.x%+@char.y%+2,oy%-rb.y%-@char.y%,@char.y%/2
            x%% = p%% : y%% = rb.handler%%
            REPEAT
              p%% = rb.link%%
              PROC_setptr(rb{},p%%)
              IF rb.handler%% = y%% THEN
                rb.checked% = FALSE
                IF FN(y%%)(ox%,oy%,p%%,DLG_INIT,0,0)
              ENDIF
            UNTIL rb.handler%% <> y%%
            PROC_setptr(rb{},x%%)
            REPEAT
              p%% = rb.prev%%
              PROC_setptr(rb{},p%%)
              IF rb.handler%% = y%% THEN
                rb.checked% = FALSE
                IF FN(y%%)(ox%,oy%,p%%,DLG_INIT,0,0)
              ENDIF
            UNTIL rb.handler%% <> y%%
            = C%
          ENDIF
        WHEN DLG_MOUSEIN:
          rb.mouse% = TRUE
          GCOL 2,11 : CIRCLE FILL ox%+rb.x%+@char.y%+2,oy%-rb.y%-@char.y%,@char.y%
          GCOL 1,12 : CIRCLE FILL ox%+rb.x%+@char.y%+2,oy%-rb.y%-@char.y%,@char.y%
          MOUSE ON 0
        WHEN DLG_DISABLE:
          IF FN(rb.handler%%)(ox%,oy%,p%%,DLG_MOUSEOUT,X%,Y%)
          IF rb.flags% AND WS_DARKMODE GCOL 2,9 + 128 : CLG ELSE GCOL 1,9 + 128 : CLG
      ENDCASE
      = FALSE

      DEF PROC_button(D%,t$,I%,X%,Y%,W%,H%,F%)
      LOCAL p%%,dlg{},pb{}
      DIM dlg{x%,y%,w%,h%,link%%,focus%%,nctrl%,backcall%%,bmp$,title$,scale}
      DIM pb{x%,y%,w%,h%,link%%,prev%%,id%,flags%,text$,handler%%,focus%,mouse%}
      DIM p%% DIM(pb{}) + 2 : p%% = (p%% + 3) AND -4
      PROC_setptr(dlg{},D%+PAGE-!340)
      PROC_setptr(pb{},p%%)
      pb.x% = (X% * dlg.scale + 1.5) AND -2
      pb.y% = (Y% * dlg.scale + 1.5) AND -2
      pb.w% = (W% * dlg.scale + 1.5) AND -2
      pb.h% = (H% * dlg.scale + 1.5) AND -2
      pb.id% = I%
      pb.flags% = F% EOR (WS_TABSTOP OR WS_VISIBLE)
      pb.text$ = t$
      pb.link%% = dlg.link%%
      pb.handler%% = ^FN_button@dlg()
      dlg.link%% = p%%
      dlg.nctrl% += 1
      ENDPROC
      DEF FN_button@dlg(ox%,oy%,p%%,C%,X%,Y%)
      LOCAL pb{}
      DIM pb{x%,y%,w%,h%,link%%,prev%%,id%,flags%,text$,handler%%,focus%,mouse%}
      PROC_setptr(pb{},p%%)
      VDU 24,ox%+pb.x%+2;oy%-pb.y%-pb.h%+2;ox%+pb.x%+pb.w%-2;oy%-pb.y%;
      VDU 23,16,64|
      CASE C% OF
        WHEN DLG_INIT,DLG_MOUSEOUT:
          IF C% = DLG_MOUSEOUT pb.mouse% = FALSE
          IF pb.flags% AND BS_ICON GCOL 7 + 128 ELSE GCOL 10 + 128
          IF (pb.flags% AND WS_DISABLED + WS_DARKMODE) = WS_DISABLED + WS_DARKMODE GCOL 7 + 128
          CLG
          GCOL 0
          IF pb.flags% AND (BS_BITMAP OR BS_ICON) THEN
            ON ERROR LOCAL IF FALSE THEN
              OSCLI "display """ + pb.text$ + """ " + STR$(ox%+pb.x%+2) + "," + \
              \   STR$(oy%-pb.y%-pb.h%+2) + "," + STR$(pb.w%-2) + "," + STR$(pb.h%-2)
            ENDIF : RESTORE ERROR
          ELSE
            MOVE ox%+pb.x%+pb.w% DIV 2 - FN_stringwidth(pb.text$) DIV 2,\
            \    oy%-pb.y%-pb.h% DIV 2 + @char.y%
            VDU 5 : PRINT pb.text$; : VDU 4
          ENDIF
          IF pb.flags% AND BS_ICON ELSE RECTANGLE ox%+pb.x%+2,oy%-pb.y%,pb.w%-4,-pb.h%+2
          IF pb.focus% IF FN(pb.handler%%)(ox%,oy%,p%%,DLG_GAINFOCUS,0,0)
          IF pb.mouse% IF FN(pb.handler%%)(ox%,oy%,p%%,DLG_MOUSEIN,X%,Y%)
        WHEN DLG_GAINFOCUS,DLG_LOSEFOCUS:
          MOVE ox%+pb.x%+8,oy%-pb.y%-4
          IF pb.flags% AND BS_BITMAP VDU 23,23,2|
          PLOT 18,pb.w%-16,0 : PLOT 18,0,-pb.h%+10
          PLOT 18,-pb.w%+16,0 : PLOT 18,0,pb.h%-10
          VDU 23,23,1|
          IF C% = DLG_GAINFOCUS pb.focus% = TRUE ELSE pb.focus% = FALSE
        WHEN DLG_UNCLICK:
          = pb.id%
        WHEN DLG_INKEY:
          IF X% = 32 THEN = pb.id%
        WHEN DLG_MOUSEIN:
          pb.mouse% = TRUE
          GCOL 2,11 + 128 : CLG
          GCOL 1,12 + 128 : CLG
          MOUSE ON 0
        WHEN DLG_DISABLE:
          IF FN(pb.handler%%)(ox%,oy%,p%%,DLG_MOUSEOUT,X%,Y%)
          IF pb.flags% AND WS_DARKMODE GCOL 2,9 + 128 : CLG ELSE GCOL 1,9 + 128 : CLG
      ENDCASE
      = FALSE

      DEF PROC_groupbox(D%,t$,I%,X%,Y%,W%,H%,F%)
      LOCAL p%%,dlg{},gb{}
      DIM dlg{x%,y%,w%,h%,link%%,focus%%,nctrl%,backcall%%,bmp$,title$,scale}
      DIM gb{x%,y%,w%,h%,link%%,prev%%,id%,flags%,text$,handler%%}
      DIM p%% DIM(gb{}) + 2 : p%% = (p%% + 3) AND -4
      PROC_setptr(dlg{},D%+PAGE-!340)
      PROC_setptr(gb{},p%%)
      gb.x% = (X% * dlg.scale + 1.5) AND -2
      gb.y% = (Y% * dlg.scale + 1.5) AND -2
      gb.w% = (W% * dlg.scale + 1.5) AND -2
      gb.h% = (H% * dlg.scale + 1.5) AND -2
      gb.id% = I%
      gb.flags% = F% OR WS_DISABLED OR WS_VISIBLE
      gb.text$ = t$
      gb.link%% = dlg.link%%
      gb.handler%% = ^FN_groupbox@dlg()
      dlg.link%% = p%%
      dlg.nctrl% += 1
      ENDPROC
      DEF FN_groupbox@dlg(ox%,oy%,p%%,C%,X%,Y%)
      LOCAL gb{}
      DIM gb{x%,y%,w%,h%,link%%,prev%%,id%,flags%,text$,handler%%}
      PROC_setptr(gb{},p%%)
      VDU 24,ox%+gb.x%+2;oy%-gb.y%-gb.h%+2;ox%+gb.x%+gb.w%-2;oy%-gb.y%;
      VDU 23,16,64|
      CASE C% OF
        WHEN DLG_INIT:
          GCOL 9
          RECTANGLE ox%+gb.x%+2,oy%-gb.y%-@char.y%,gb.w%-4,-gb.h%+4+@char.y%
          GCOL 7
          RECTANGLE FILL ox%+gb.x%+2*@char.y% - 4,oy%-gb.y%,\
          \              FN_stringwidth(gb.text$) + 8,-2 * @char.y%
          MOVE ox%+gb.x%+2*@char.y%,oy%-gb.y%
          GCOL 0 : VDU 5 : PRINT gb.text$; : VDU 4
      ENDCASE
      = FALSE

      DEF PROC_listbox(D%,t$,I%,X%,Y%,W%,H%,F%)
      LOCAL p%%,dlg{},lb{}
      DIM dlg{x%,y%,w%,h%,link%%,focus%%,nctrl%,backcall%%,bmp$,title$,scale}
      DIM lb{x%,y%,w%,h%,link%%,prev%%,id%,flags%,text$,handler%%,focus%,\
      \      array%%,num%,select%,scroll%,visible%,acc%,drag%,hot%,timer%,\
      \      prevy%,col%(9),sb{l%,t%,r%,b%,min%,max%,pos%,page%,thumb{t%,b%},scale}}
      DIM p%% DIM(lb{}) + 2 : p%% = (p%% + 3) AND -4
      PROC_setptr(dlg{},D%+PAGE-!340)
      PROC_setptr(lb{},p%%)
      lb.x% = (X% * dlg.scale + 1.5) AND -2
      lb.y% = (Y% * dlg.scale + 1.5) AND -2
      lb.w% = (W% * dlg.scale + 1.5) AND -2
      lb.h% = (H% * dlg.scale + 1.5) AND -2
      lb.visible% = lb.h% DIV (2 * @char.y%) * 2 * @char.y%
      lb.id% = I%
      lb.flags% = F% EOR (WS_TABSTOP OR WS_VISIBLE)
      lb.text$ = t$
      lb.link%% = dlg.link%%
      lb.hot% = -1
      lb.timer% = 1000
      lb.handler%% = ^FN_listbox@dlg()
      FOR I% = 0 TO 9 : lb.col%(I%) = dlg.scale * 200 : NEXT
      dlg.link%% = p%%
      dlg.nctrl% += 1
      ENDPROC
      DEF FN_listbox@dlg(ox%,oy%,p%%,C%,X%,Y%)
      LOCAL lb{},a$(),H%,I%,W%
      PRIVATE sb{}
      DIM sb{l%,t%,r%,b%,min%,max%,pos%,page%,thumb{t%,b%},scale},a$(0)
      DIM lb{x%,y%,w%,h%,link%%,prev%%,id%,flags%,text$,handler%%,focus%,\
      \      array%%,num%,select%,scroll%,visible%,acc%,drag%,hot%,timer%,\
      \      prevy%,col%(9),sb{} = sb{}}
      PROC_setptr(lb{},p%%)
      PROC_setptr(sb{},lb{}+lb.sb{})
      IF lb.array%% THEN
        IF INKEY(-256)=&73 IF @platform% AND &40 ]^a$() = lb.array%% ELSE !^a$() = lb.array%%
      ENDIF
      lb.sb.l% = ox% + lb.x% + lb.w% - 4 - 2*SCROLLBARTHICK
      lb.sb.t% = oy% - lb.y% - 2*SCROLLBARTHICK
      lb.sb.r% = ox% + lb.x% + lb.w% - 4
      lb.sb.b% = oy% - lb.y% - lb.h% + 2*SCROLLBARTHICK + 2
      VDU 24,ox%+lb.x%+2;oy%-lb.y%-lb.h%+2;ox%+lb.x%+lb.w%-2;oy%-lb.y%;
      VDU 23,16,64|
      CASE C% OF
        WHEN DLG_INIT:
          GCOL 0 : GCOL 15 + 128 : CLG
          RECTANGLE ox%+lb.x%+2,oy%-lb.y%,lb.w%-4,2-lb.h%
          IF X% = 0 THEN
            lb.visible% = lb.h% DIV (2 * @char.y%) * 2 * @char.y%
            IF lb.select% > 0 WHILE (lb.select% - 1) * 2 * @char.y% <= lb.scroll% lb.scroll% -= 1 : ENDWHILE
            WHILE lb.select% * 2 * @char.y% > (lb.scroll% + lb.visible%) lb.scroll% += 1 : ENDWHILE
          ENDIF

          VDU 5,30 : PLOT 0, 0, lb.scroll% MOD (2 * @char.y%)
          Y% = lb.h% DIV (2 * @char.y%)
          lb.text$ = ""
          FOR I% = lb.scroll% DIV (2 * @char.y%) + 1 TO (lb.scroll% + lb.h%) DIV (2 * @char.y%) + 1
            IF I% > DIM(a$(),1) OR I% > lb.num% EXIT FOR
            PLOT 0,10,0 : PRINT a$(I%)
            IF I% = lb.select% THEN
              lb.text$ = a$(I%)
              GCOL 2,11 + 128 : PLOT 99,lb.w%,2*@char.y%
              GCOL 1,12 + 128 : PLOT 99,-lb.w%,-2*@char.y%
              GCOL 13 + 128
              PLOT 3,0,@char.y%*2 : PLOT 3,lb.w%,0
              PLOT 3,0,-@char.y%*2 : PLOT 3,-lb.w%,0
            ENDIF
          NEXT
          VDU 4
          IF lb.focus% IF FN(lb.handler%%)(ox%,oy%,p%%,DLG_GAINFOCUS,0,0)

          IF (lb.flags% AND WS_VSCROLL) = 0 THEN = FALSE
          sb.max% = lb.num% * 2*@char.y%
          sb.pos% = lb.scroll%
          sb.page% = lb.visible%
          PROC_drawscrollbar(0,sb{})
          LINE sb.l%-2,oy%-lb.y%,sb.l%-2,oy%-lb.y%-lb.h%

          IF lb.hot% >= 0 THEN
            PROC_sbhotspot(lb.hot%,sb{},X%,Y%,W%,H%)
            IF W% IF H% THEN
              GCOL 2,11
              RECTANGLE FILL X%,Y%,W%,H%
              GCOL 1,12
              RECTANGLE FILL X%,Y%,W%,H%
            ENDIF
          ENDIF

        WHEN DLG_GAINFOCUS,DLG_LOSEFOCUS:
          MOVE ox%+lb.x%+8,oy%-lb.y%-4
          IF lb.flags% AND WS_VSCROLL THEN
            PLOT 18,lb.w%-18-sb.r%+sb.l%,0 : PLOT 18,0,-lb.h%+10
            PLOT 18,-lb.w%+18+sb.r%-sb.l%,0 : PLOT 18,0,lb.h%-10
          ELSE
            PLOT 18,lb.w%-16,0 : PLOT 18,0,-lb.h%+10
            PLOT 18,-lb.w%+16,0 : PLOT 18,0,lb.h%-10
          ENDIF
          IF C% = DLG_GAINFOCUS lb.focus% = TRUE ELSE lb.focus% = FALSE : lb.timer% = 1000

        WHEN DLG_INKEY:
          IF lb.select% < 0 OR lb.select% > lb.num% lb.select% = 0
          CASE X% OF
            WHEN 13,32: = lb.id%
            WHEN 132: lb.select% = 0 : lb.scroll% -= lb.visible% : IF lb.scroll% < 0 lb.scroll% = 0
            WHEN 133: lb.select% = 0 : lb.scroll% += lb.visible%
              IF lb.scroll% > lb.num% * 2 * @char.y% - lb.visible% lb.scroll% = lb.num% * 2 * @char.y% - lb.visible%
              IF lb.scroll% < 0 lb.scroll% = 0
            WHEN 138: lb.hot% = -1 : lb.select% = lb.select% MOD lb.num% + 1
            WHEN 139: lb.hot% = -1 : lb.select% = (lb.select% + lb.num% - 2) MOD lb.num% + 1
            WHEN 140: IF lb.scroll% < lb.num% * 2 * @char.y% - lb.visible% lb.scroll% += 2 * @char.y%
            WHEN 141: IF lb.scroll% lb.scroll% -= 2 * @char.y%
            WHEN 156: lb.scroll% = 0
            WHEN 157: lb.scroll% = lb.num% * 2 * @char.y% - lb.visible%
            OTHERWISE: = FALSE
          ENDCASE
          IF FN(lb.handler%%)(ox%,oy%,lb{},DLG_INIT,X%>139,0)
          REPEAT UNTIL INKEY(0) = -1

        WHEN DLG_CLICK,DLG_TIMER:
          IF C% = DLG_TIMER THEN
            lb.timer% += 1
          ELSE
            CASE lb.hot% OF
              WHEN 0,1,3,4:
                lb.drag% = 1
                lb.timer% = 0
              WHEN 2:
                lb.drag% = 2
                lb.timer% = 0
              OTHERWISE:
                IF lb.select% = (lb.scroll% + oy% - lb.y% - Y%) DIV (2 * @char.y%) + 1 THEN
                  IF lb.timer% < 10 THEN = -(lb.select% <> 0) : REM double-click (allow -1)
                ENDIF
                IF lb.timer% >= 10 lb.drag% = 3 ELSE lb.drag% = 4
                lb.timer% = 0
                lb.prevy% = Y%
            ENDCASE
          ENDIF

          IF lb.drag% IF lb.drag% < 3 IF lb.timer% = 0 OR lb.timer% > 10 THEN
            CASE lb.hot% OF
              WHEN 0: lb.scroll% -= 2 * @char.y%
              WHEN 1: lb.scroll% -= lb.visible%
              WHEN 3: lb.scroll% += lb.visible%
              WHEN 4: lb.scroll% += 2 * @char.y%
            ENDCASE
            IF lb.scroll% > lb.num% * 2 * @char.y% - lb.visible% lb.scroll% = lb.num% * 2 * @char.y% - lb.visible%
            IF lb.scroll% < 0 lb.scroll% = 0
            IF FN(lb.handler%%)(ox%,oy%,lb{},DLG_INIT,TRUE,0)
            IF C% = DLG_TIMER THEN = TRUE
          ENDIF

        WHEN DLG_MOUSEMOVE:
          IF (lb.flags% AND WS_VSCROLL) = 0 THEN = FALSE
          CASE lb.drag% OF
            WHEN 2: lb.acc% += (Y% - lb.prevy%) * sb.scale
            WHEN 3,4: lb.acc% -= Y% - lb.prevy%
            OTHERWISE: lb.acc% = 0
          ENDCASE
          lb.prevy% = Y%
          IF ABS(lb.acc%) > @char.y% / 2 IF lb.drag% = 3 lb.drag% = 4
          lb.scroll% -= lb.acc% : lb.acc% = 0
          IF lb.scroll% > lb.num% * 2 * @char.y% - lb.visible% lb.scroll% = lb.num% * 2 * @char.y% - lb.visible%
          IF lb.scroll% < 0 lb.scroll% = 0
          IF lb.drag% = 2 lb.hot% = 2 ELSE lb.hot% = FN_sbhotspot(0,sb{},X%,Y%)
          IF FN(lb.handler%%)(ox%,oy%,p%%,DLG_INIT,TRUE,0)
          = TRUE

        WHEN DLG_UNCLICK:
          IF lb.drag% = 3 IF lb.timer% < 10 THEN
            lb.drag% = 0
            lb.timer% = 0
            lb.select% = (lb.scroll% + oy% - lb.y% - lb.prevy%) DIV (2 * @char.y%) + 1
            IF lb.select% > lb.num% lb.select% = 0
            IF FN(lb.handler%%)(ox%,oy%,lb{},DLG_INIT,0,0)
            = lb.id%
          ENDIF
          lb.drag% = 0

        WHEN DLG_DISABLE:
          IF lb.flags% AND WS_DARKMODE GCOL 2,9 + 128 : CLG ELSE GCOL 1,9 + 128 : CLG

        WHEN DLG_MOUSEIN:
          MOUSE ON 0
      ENDCASE
      = FALSE

      DEF PROCdelsel@dlg(RETURN C%,RETURN A%,RETURN t$)
      IF A% = C% ENDPROC
      IF C% < A% SWAP C%,A%
      t$ = LEFT$(t$,A%) + MID$(t$,C%+1)
      C% = A%
      ENDPROC

      DEF PROCdrawtext@dlg(X%,Y%,W%,H%,C%,A%,RETURN S%,F%,t$)
      LOCAL P%
      GCOL 0 : GCOL 15 + 128 : CLG
      RECTANGLE X%+2,Y%,W%-4,2-H%
      P% = FN_stringwidth(LEFT$(t$,C%))
      IF C% >= LENt$ S% = P% - W% + 20 : IF S% < 0 S% = 0
      IF P% < S%           S% = P%
      IF P% > S% + W% - 20 S% = P% - W% + 20
      X% += @char.x% - S% : Y% += @char.y% - H% DIV 2
      IF NOT F% A% = C%
      IF C% < A% SWAP C%,A%
      VDU 5 : MOVE X%,Y%
      PRINT LEFT$(t$,A%);
      IF C% > A% THEN
        GCOL 15 : GCOL 13 + 128
        W% = FN_stringwidth(MID$(t$,A%+1,C%-A%))
        H% = 2 * @char.y%
        PLOT 99,W%,-H%
        PLOT 0,-W%,H%
        PRINT MID$(t$,A%+1,C%-A%);
        GCOL 0 : GCOL 15 + 128
      ENDIF
      PRINT MID$(t$,C%+1);
      VDU 4
      IF F% THEN
        @vdu.c.x% = (X% + P%) DIV 2
        @vdu.c.y% = @vdu%!212 - 1 - Y% DIV 2
      ENDIF
      ENDPROC

      DEF FNgetclipboardline@dlg
      LOCAL T%,a$
      a$ = FN_getclipboardtext
      T% = 1
      WHILE ASCMID$(a$,T%) >= 32 T% += 1 : ENDWHILE
      = LEFT$(a$,T% - 1)

      REM Return the width of a string, in the currently selected font
      REM (in graphics units). Note that it switches to VDU 5 mode; if
      REM you don't want that, add a VDU 4 immediately after the call.
      REM The supplied string may include colour and drawing commands!
      DEF FN_stringwidth(a$)
      IF POS REM SDL thread sync.
      LOCAL X%,@vdu.l.x%,@vdu.l.y%,?444
      VDU 5,23,16,0|30,11,23,16,64|10,10
      IF POS REM SDL thread sync.
      X% = @vdu.l.x%
      PRINT a$;CHR$0;CHR$0;
      IF POS REM SDL thread sync.
      = 2 * (@vdu.l.x% - X%)

      REM Return text from the clipboard:
      DEF FN_getclipboardtext
      LOCAL H%,T%,t%%,a$
      IF INKEY(-256) = &57 THEN
        SYS "OpenClipboard",@hwnd%
        SYS "GetClipboardData",1 TO H%
        IF H% THEN
          SYS "GlobalLock",H% TO T%
          a$ = $$T%
          SYS "GlobalUnlock",H%
        ENDIF
        SYS "CloseClipboard"
      ELSE
        SYS "SDL_GetClipboardText" TO t%%
        IF @platform% AND &40 ELSE t%% = !^t%%
        a$ = $$t%%
      ENDIF
      = a$

      REM Put text in the clipboard (replacing any existing contents):
      DEF PROC_putclipboardtext(a$)
      LOCAL H%,T%
      IF INKEY(-256) = &57 THEN
        SYS "GlobalAlloc",&2000,LEN(a$)+1 TO H%
        SYS "GlobalLock",H% TO T% : $$T% = a$
        SYS "GlobalUnlock",H%
        SYS "OpenClipboard",@hwnd%
        SYS "EmptyClipboard"
        SYS "SetClipboardData",1,H%
        SYS "CloseClipboard"
      ELSE
        SYS "SDL_SetClipboardText",a$
      ENDIF
      ENDPROC

      DEF FN_sbhotspot(O%,sb{},X%,Y%)
      LOCAL B%,rc{}
      DIM rc{} = sb{} : rc{} = sb{}

      B% = -1
      IF O% THEN
        rc.l% -= 2 * SCROLLBARTHICK
        rc.r% += 2 * SCROLLBARTHICK
        IF NOT FN_ptinrect(X%,Y%,rc{}) THEN = -1
        CASE TRUE OF
          WHEN X% > sb.r%:       B% = 0
          WHEN X% > sb.thumb.r%: B% = 1
          WHEN X% > sb.thumb.l%: B% = 2
          WHEN X% > sb.l%:       B% = 3
          OTHERWISE:             B% = 4
        ENDCASE
      ELSE
        rc.b% -= 2 * SCROLLBARTHICK
        rc.t% += 2 * SCROLLBARTHICK
        IF NOT FN_ptinrect(X%,Y%,rc{}) THEN = -1
        CASE TRUE OF
          WHEN Y% > sb.t%:       B% = 0
          WHEN Y% > sb.thumb.t%: B% = 1
          WHEN Y% > sb.thumb.b%: B% = 2
          WHEN Y% > sb.b%:       B% = 3
          OTHERWISE:             B% = 4
        ENDCASE
      ENDIF
      = B%

      DEF PROC_sbhotspot(N%,sb{},RETURN X%,RETURN Y%,RETURN W%,RETURN H%)
      H% = 0
      W% = 0
      CASE N% OF
        WHEN 0:
          X% = sb.l%
          Y% = sb.t% + 2
          W% = sb.r% - sb.l%
          H% = 2 * SCROLLBARTHICK - 4
        WHEN 2:
          X% = sb.l%
          Y% = sb.thumb.b%
          W% = sb.r% - sb.l%
          H% = sb.thumb.t% - sb.thumb.b%
        WHEN 4:
          X% = sb.l%
          Y% = sb.b% - 2 * SCROLLBARTHICK + 2
          W% = sb.r% - sb.l%
          H% = 2 * SCROLLBARTHICK - 4
        WHEN 5:
          X% = sb.r% + 2
          Y% = sb.b%
          W% = 2 * SCROLLBARTHICK - 4
          H% = sb.t% - sb.b%
        WHEN 7:
          X% = sb.thumb.l%
          Y% = sb.b%
          W% = sb.thumb.r% - sb.thumb.l%
          H% = sb.t% - sb.b%
        WHEN 9:
          X% = sb.l% - 2 * SCROLLBARTHICK + 2
          Y% = sb.b%
          W% = 2 * SCROLLBARTHICK - 4
          H% = sb.t% - sb.b%
      ENDCASE
      ENDPROC

      DEF PROC_drawscrollbar(O%,sb{})
      LOCAL T%,X%,Y%
      GCOL 15
      IF O% THEN
        RECTANGLE FILL sb.l%-2*SCROLLBARTHICK+2,sb.b%,sb.r%-sb.l%+4*SCROLLBARTHICK-4,sb.t%-sb.b%
        VDU 23,23,3|
        GCOL 8
        X% = sb.l%-SCROLLBARTHICK
        Y% = (sb.b%+sb.t%)/2 AND -2
        LINE X%-6,Y%,X%+4,Y%-8 : LINE X%-6,Y%,X%+4,Y%+8
        X% = sb.r%+SCROLLBARTHICK
        LINE X%+6,Y%,X%-4,Y%-8 : LINE X%+6,Y%,X%-4,Y%+8
        VDU 23,23,1|
        LINE sb.l%,sb.b%,sb.l%,sb.t%
        LINE sb.r%,sb.b%,sb.r%,sb.t%
        IF sb.max% <= sb.page% OR sb.max%=0 THEN
          sb.thumb.l% = (sb.l% + sb.r%) / 2
          sb.thumb.r% = sb.thumb.l% - 1
          ENDPROC
        ENDIF
        T% = sb.page% / sb.max% * (sb.r% - sb.l%) AND -2
        IF T% < MINTHUMB T% = MINTHUMB
        sb.scale = (sb.max% - sb.page%) / (sb.r% - sb.l% - T%)
        X% = sb.l% + sb.pos% / sb.scale AND -2
        GCOL 7
        sb.thumb.l% = X%
        sb.thumb.r% = X%+T%
        RECTANGLE FILL sb.thumb.l%,sb.b%,sb.thumb.r%-sb.thumb.l%,sb.t%-sb.b%
        GCOL 8
        RECTANGLE sb.thumb.l%,sb.b%,sb.thumb.r%-sb.thumb.l%,sb.t%-sb.b%
        VDU 23,23,2|
        LINE X%+T%/2,sb.b%+12,X%+T%/2,sb.t%-12
        LINE X%+T%/2-10,sb.b%+12,X%+T%/2-10,sb.t%-12
        LINE X%+T%/2+10,sb.b%+12,X%+T%/2+10,sb.t%-12
        VDU 23,23,1|
      ELSE
        RECTANGLE FILL sb.l%,sb.b%-2*SCROLLBARTHICK+2,sb.r%-sb.l%,sb.t%-sb.b%+4*SCROLLBARTHICK-4
        VDU 23,23,3|
        GCOL 8
        X% = (sb.l%+sb.r%)/2 AND -2
        Y% = sb.t%+SCROLLBARTHICK
        LINE X%,Y%+6,X%-8,Y%-4 : LINE X%,Y%+6,X%+8,Y%-4
        Y% = sb.b%-SCROLLBARTHICK
        LINE X%,Y%-6,X%-8,Y%+4 : LINE X%,Y%-6,X%+8,Y%+4
        VDU 23,23,1|
        LINE sb.l%,sb.b%,sb.r%,sb.b%
        LINE sb.l%,sb.t%,sb.r%,sb.t%
        IF sb.max% <= sb.page% OR sb.max%=0 THEN
          sb.thumb.t% = (sb.t% + sb.b%) / 2
          sb.thumb.b% = sb.thumb.t% + 1
          ENDPROC
        ENDIF
        T% = sb.page% / sb.max% * (sb.t% - sb.b%) AND -2
        IF T% < MINTHUMB T% = MINTHUMB
        sb.scale = (sb.max% - sb.page%) / (sb.t% - sb.b% - T%)
        Y% = sb.t% - sb.pos% / sb.scale AND -2
        GCOL 7
        sb.thumb.t% = Y%
        sb.thumb.b% = Y%-T%
        RECTANGLE FILL sb.l%,sb.thumb.b%,sb.r%-sb.l%,sb.thumb.t%-sb.thumb.b%
        GCOL 8
        RECTANGLE sb.l%,sb.thumb.b%,sb.r%-sb.l%,sb.thumb.t%-sb.thumb.b%
        VDU 23,23,2|
        LINE sb.l%+12,Y%-T%/2,sb.r%-12,Y%-T%/2
        LINE sb.l%+12,Y%-T%/2-10,sb.r%-12,Y%-T%/2-10
        LINE sb.l%+12,Y%-T%/2+10,sb.r%-12,Y%-T%/2+10
        VDU 23,23,1|
      ENDIF
      ENDPROC

      DEF FN_ptinrect(X%,Y%,r{})
      = (X% >= r.l%) AND (X% <= r.r%) AND (Y% >= r.b%) AND (Y% < r.t%)

      DEF PROC_setptr(RETURN d{},p%%)
      IF INKEY(-256)=&73 IF @platform% AND &40 ](^d{}+8)=p%% ELSE !(^d{}+4)=!^p%%
      ENDPROC

      DEF FN_getproc(D%) : LOCAL p%%
      DEF FN_setproc(RETURN p%%) : LOCAL D%
      PRIVATE I%, p%%() : DIM p%%(&FF)
      IF D% THEN = p%%(NOT D% AND &FF)
      I% += 1 : p%%(I%) = p%%
      = NOT I% AND &FFFF

      DEF PROC_trackbar(D%,t$,I%,X%,Y%,W%,H%,F%)
      LOCAL p%%,dlg{},tk{}
      DIM dlg{x%,y%,w%,h%,link%%,focus%%,nctrl%,backcall%%,bmp$,title$,scale}
      DIM tk{x%,y%,w%,h%,link%%,prev%%,id%,flags%,text$,handler%%,focus%,\
      \      min%,max%,pos%,drag%,old%,hot%,scale}
      DIM p%% DIM(tk{}) + 2 : p%% = (p%% + 3) AND -4
      PROC_setptr(dlg{},D%+PAGE-!340)
      PROC_setptr(tk{},p%%)
      tk.x% = (X% * dlg.scale + 1.5) AND -2
      tk.y% = (Y% * dlg.scale + 1.5) AND -2
      tk.w% = (W% * dlg.scale + 1.5) AND -2
      tk.h% = (H% * dlg.scale + 1.5) AND -2
      tk.id% = I%
      tk.flags% = F% EOR (WS_TABSTOP OR WS_VISIBLE)
      tk.text$ = t$
      tk.link%% = dlg.link%%
      tk.handler%% = ^FN_trackbar@dlg()
      tk.max% = 100
      tk.pos% = 50
      tk.scale = (tk.w% - 28) / (tk.max% - tk.min%)
      dlg.link%% = p%%
      dlg.nctrl% += 1
      ENDPROC
      DEF FN_trackbar@dlg(ox%,oy%,p%%,C%,X%,Y%)
      LOCAL T%,tk{}
      DIM tk{x%,y%,w%,h%,link%%,prev%%,id%,flags%,text$,handler%%,focus%,\
      \      min%,max%,pos%,drag%,old%,hot%,scale}
      PROC_setptr(tk{},p%%)
      VDU 24,ox%+tk.x%+2;oy%-tk.y%-tk.h%+2;ox%+tk.x%+tk.w%-2;oy%-tk.y%;
      T% = ox%+tk.x%+4+(tk.pos%-tk.min%)*tk.scale
      CASE C% OF
        WHEN DLG_INIT:
          GCOL 8 : GCOL 7 + 128 : CLG
          RECTANGLE ox%+tk.x%+4,oy%-tk.y%-tk.h% DIV 2,tk.w%-8,4
          IF tk.hot% OR tk.drag% GCOL 0 ELSE GCOL 13
          RECTANGLE FILL T%,oy%-tk.y%-6,20,-tk.h%+16
          IF tk.focus% IF FN(tk.handler%%)(ox%,oy%,p%%,DLG_GAINFOCUS,0,0)

        WHEN DLG_GAINFOCUS,DLG_LOSEFOCUS:
          MOVE ox%+tk.x%+4,oy%-tk.y%-4
          PLOT 18,tk.w%-10,0 : PLOT 18,0,-tk.h%+10
          PLOT 18,-tk.w%+10,0 : PLOT 18,0,tk.h%-10
          IF C% = DLG_GAINFOCUS tk.focus% = TRUE ELSE tk.focus% = FALSE

        WHEN DLG_INKEY:
          CASE X% OF
            WHEN 132: tk.pos% -= (tk.max%-tk.min%) / 5 : IF tk.pos% < tk.min% tk.pos% = tk.min%
            WHEN 133: tk.pos% += (tk.max%-tk.min%) / 5 : IF tk.pos% > tk.max% tk.pos% = tk.max%
            WHEN 136,138: tk.pos% -= 1 : IF tk.pos% < tk.min% tk.pos% = tk.min%
            WHEN 137,139: tk.pos% += 1 : IF tk.pos% > tk.max% tk.pos% = tk.max%
            WHEN 156: tk.pos% = tk.min%
            WHEN 157: tk.pos% = tk.max%
          ENDCASE
          IF FN(tk.handler%%)(ox%,oy%,p%%,DLG_INIT,X%,Y%)
          REPEAT UNTIL INKEY(0) = -1

        WHEN DLG_CLICK:
          IF X% < T%    tk.pos% -= (tk.max%-tk.min%) / 5 : IF tk.pos% < tk.min% tk.pos% = tk.min%
          IF X% > T%+16 tk.pos% += (tk.max%-tk.min%) / 5 : IF tk.pos% > tk.max% tk.pos% = tk.max%
          IF tk.hot% THEN
            tk.drag% = TRUE
            tk.old% = X% - T%
          ENDIF

        WHEN DLG_MOUSEMOVE:
          tk.hot% = X% >= T% AND X% <= T% + 16
          IF tk.drag% THEN
            tk.pos% = tk.min% + (X%-tk.old%-ox%-tk.x%-4)/tk.scale + 0.5
            IF tk.pos% < tk.min% tk.pos% = tk.min%
            IF tk.pos% > tk.max% tk.pos% = tk.max%
          ENDIF
          IF FN(tk.handler%%)(ox%,oy%,p%%,DLG_INIT,TRUE,0)
          = TRUE

        WHEN DLG_MOUSEOUT:
          tk.hot% = FALSE
          IF FN(tk.handler%%)(ox%,oy%,p%%,DLG_INIT,TRUE,0)

        WHEN DLG_UNCLICK:
          tk.drag% = FALSE

        WHEN DLG_DISABLE:
          IF tk.flags% AND WS_DARKMODE GCOL 2,9 + 128 : CLG ELSE GCOL 1,9 + 128 : CLG

      ENDCASE
      = FALSE

      DEF PROC_settrackbarpos(D%,I%,S%,L%,H%)
      LOCAL T%,tk{}
      DIM tk{x%,y%,w%,h%,link%%,prev%%,id%,flags%,text$,handler%%,focus%,min%,max%,pos%,drag%,old%,hot%,scale}
      T% = FN_getdlgitem(D%,I%)
      IF T% = 0 ENDPROC
      PROC_setptr(tk{},T%+PAGE-!340)
      tk.pos% = S%
      tk.min% = L%
      tk.max% = H%
      ENDPROC

      DEF FN_gettrackbarpos(D%,I%)
      LOCAL T%,tk{}
      DIM tk{x%,y%,w%,h%,link%%,prev%%,id%,flags%,text$,handler%%,focus%,min%,max%,pos%,drag%,old%,hot%,scale}
      T% = FN_getdlgitem(D%,I%)
      IF T% = 0 THEN = 0
      PROC_setptr(tk{},T%+PAGE-!340)
      = tk.pos%

      DEF PROC_setcomboboxarray(D%,I%,RETURN a%%,N%)
      LOCAL L%,cb{},lb{}
      DIM cb{x%,y%,w%,h%,link%%,prev%%,id%,flags%,text$,handler%%,bmp$,textbox%%,listbox%%,mouse%}
      DIM lb{x%,y%,w%,h%,link%%,prev%%,id%,flags%,text$,handler%%,focus%,array%%,num%,select%,scroll%}
      L% = FN_getdlgitem(D%,I%)
      IF L% = 0 THEN ENDPROC
      PROC_setptr(cb{},L%+PAGE-!340)
      PROC_setptr(lb{},cb.listbox%%)
      IF a%% <> lb.array%% OR N% <> lb.num% lb.select% = 0 : lb.scroll% = 0
      lb.array%% = a%%
      lb.num% = N%
      ENDPROC

      DEF PROC_setcomboboxselect(D%,I%,S%)
      LOCAL L%,cb{},lb{}
      DIM cb{x%,y%,w%,h%,link%%,prev%%,id%,flags%,text$,handler%%,bmp$,textbox%%,listbox%%,mouse%}
      DIM lb{x%,y%,w%,h%,link%%,prev%%,id%,flags%,text$,handler%%,focus%, \
      \      array%%,num%,select%,scroll%,visible%}
      L% = FN_getdlgitem(D%,I%)
      IF L% = 0 ENDPROC
      PROC_setptr(cb{},L%+PAGE-!340)
      PROC_setptr(lb{},cb.listbox%%)
      IF lb.handler%% <> ^FN_listbox@dlg() ENDPROC
      lb.select% = S%
      IF lb.select% > 0 WHILE (lb.select% - 1) * 2 * @char.y% <= lb.scroll% lb.scroll% -= 1 : ENDWHILE
      WHILE lb.select% * 2 * @char.y% > (lb.scroll% + lb.visible%) lb.scroll% += 1 : ENDWHILE
      ENDPROC

      DEF FN_getcomboboxselect(D%,I%)
      LOCAL L%,cb{},lb{}
      DIM cb{x%,y%,w%,h%,link%%,prev%%,id%,flags%,text$,handler%%,bmp$,textbox%%,listbox%%,mouse%}
      DIM lb{x%,y%,w%,h%,link%%,prev%%,id%,flags%,text$,handler%%,focus%,array%%,num%,select%,scroll%}
      L% = FN_getdlgitem(D%,I%)
      IF L% = 0 THEN = 0
      PROC_setptr(cb{},L%+PAGE-!340)
      PROC_setptr(lb{},cb.listbox%%)
      IF lb.handler%% <> ^FN_listbox@dlg() THEN = 0
      = lb.select%

      DEF PROC_combobox(D%,t$,I%,X%,Y%,W%,H%,F%)
      LOCAL S%,p%%,dlg{},cb{},tb{},lb{} : S% = (2.5 * @char.y% + 1.5) AND -2
      DIM dlg{x%,y%,w%,h%,link%%,focus%%,nctrl%,backcall%%,bmp$,title$,scale}
      DIM cb{x%,y%,w%,h%,link%%,prev%%,id%,flags%,text$,handler%%,bmp$,textbox%%,listbox%%,mouse%}
      DIM tb{x%,y%,w%,h%,link%%,prev%%,id%,flags%,text$,handler%%,focus%,caret%,anchor%,scroll%,drag%}
      DIM lb{x%,y%,w%,h%,link%%,prev%%,id%,flags%,text$,handler%%,focus%,\
      \      array%%,num%,select%,scroll%,visible%,acc%,drag%,hot%,timer%,\
      \      prevy%,col%(9),sb{l%,t%,r%,b%,min%,max%,pos%,page%,thumb{t%,b%},scale}}
      DIM p%% DIM(cb{}) + 2 : p%% = (p%% + 3) AND -4
      PROC_setptr(dlg{},D%+PAGE-!340)
      PROC_setptr(cb{},p%%)
      cb.x% = (X% * dlg.scale + 1.5) AND -2
      cb.y% = (Y% * dlg.scale + 1.5) AND -2
      cb.w% = (W% * dlg.scale + 1.5) AND -2
      cb.h% = S%
      cb.id% = I%
      cb.flags% = F% EOR (WS_TABSTOP OR WS_VISIBLE)
      cb.text$ = t$
      cb.link%% = dlg.link%%
      cb.handler%% = ^FN_combobox@dlg()
      dlg.link%% = p%%
      dlg.nctrl% += 1
      DIM p%% DIM(tb{}) + 2 : p%% = (p%% + 3) AND -4
      PROC_setptr(tb{},p%%) : cb.textbox%% = p%%
      tb.x% = cb.x%
      tb.y% = cb.y%
      tb.w% = cb.w% - S%
      tb.h% = S%
      tb.id% = I%
      tb.flags% = cb.flags%
      tb.handler%% = ^FN_textbox@dlg()
      DIM p%% DIM(lb{}) + 2 : p%% = (p%% + 3) AND -4
      PROC_setptr(lb{},p%%) : cb.listbox%% = p%%
      lb.x% = cb.x%
      lb.y% = cb.y% + S% - 2
      lb.w% = cb.w%
      lb.h% = ((H% * dlg.scale + 1.5) AND -2) - S%
      lb.visible% = lb.h% DIV (2 * @char.y%) * 2 * @char.y%
      lb.id% = I%
      lb.flags% = cb.flags%
      lb.text$ = t$
      lb.hot% = -1
      lb.timer% = 1000
      lb.handler%% = ^FN_listbox@dlg()
      ENDPROC
      DEF FN_combobox@dlg(ox%,oy%,p%%,C%,X%,Y%)
      LOCAL S%,T%,U%,cb{},ctl{} : S% = (2.5 * @char.y% + 1.5) AND -2
      PRIVATE dropped%%
      DIM cb{x%,y%,w%,h%,link%%,prev%%,id%,flags%,text$,handler%%,bmp$,textbox%%,listbox%%,mouse%}
      DIM ctl{x%,y%,w%,h%,link%%,prev%%,id%,flags%,text$,handler%%,focus%}
      PROC_setptr(cb{},p%%)
      VDU 24,ox%+cb.x%+cb.w%-S%-2;oy%-cb.y%-S%;ox%+cb.x%+cb.w%-2;oy%-cb.y%;
      VDU 23,16,64|
      CASE C% OF
        WHEN DLG_INIT, DLG_MOUSEOUT:
          IF C% = DLG_MOUSEOUT cb.mouse% = FALSE
          GCOL 15 : RECTANGLE FILL ox%+cb.x%+cb.w%-S%-4,oy%-cb.y%,S%,-S%+2
          GCOL 0  : RECTANGLE ox%+cb.x%+cb.w%-S%-2,oy%-cb.y%,S%,-S%+2
          VDU 23,23,3| : T% = ox%+cb.x%+cb.w%-S%DIV2-2 : U% = oy%-cb.y%-S%DIV2+2
          GCOL 8  : LINE T%,U%-6,T%-8,U%+4 : LINE T%,U%-6,T%+8,U%+4
          VDU 23,23,1|
          IF p%% = dropped%%  IF FN_textbox@dlg(ox%,oy%,cb.textbox%%,C%,X%,Y%)
          IF cb.mouse% IF FN(cb.handler%%)(ox%,oy%,p%%,DLG_MOUSEIN,ox%+cb.x%+cb.w%-4,oy%-cb.y%-4)
        WHEN DLG_CLICK, DLG_LOSEFOCUS:
          IF Y% > oy%-cb.y%-S% OR C% = DLG_LOSEFOCUS AND p%% = dropped%% THEN
            IF dropped%% THEN
              PROC_setptr(cb{},dropped%%)
              PROC_setptr(ctl{},cb.listbox%%)
              OSCLI "DISPLAY """ + cb.bmp$ + """ " + STR$(ox%+ctl.x%) + "," + STR$(oy%-ctl.y%-ctl.h%)
              cb.h% = S%
              PROC_setptr(cb{},p%%)
              IF p%% = dropped%% IF FN_textbox@dlg(ox%,oy%,cb.textbox%%,DLG_GAINFOCUS,TRUE,0)
              dropped%% = 0
            ENDIF
            IF X% > ox%+cb.x%+cb.w%-S% AND C% = DLG_CLICK THEN
              cb.bmp$ = @tmp$ + "cmb" + STR$~(p%% AND &FFFFFFFF) + ".tmp.bmp"
              PROC_setptr(ctl{},cb.listbox%%)
              OSCLI "GSAVE """ + cb.bmp$ + """ " + STR$(ox%+ctl.x%) + "," + STR$(oy%-ctl.y%-ctl.h%) + \
              \     "," + STR$(ctl.w%+2) + "," + STR$(ctl.h%+2)
              cb.h% = ctl.h% + S%
              dropped%% = p%%
              C% = DLG_INIT : X% = 0 : Y% = 0
            ENDIF
          ENDIF
        WHEN DLG_MOUSEIN, DLG_MOUSEMOVE:
          IF Y% > oy%-cb.y%-S% THEN
            IF X% > ox%+cb.x%+cb.w%-S% THEN
              GCOL 2,11 : RECTANGLE FILL ox%+cb.x%+cb.w%-S%-2,oy%-cb.y%,S%,-S%+2
              GCOL 1,12 : RECTANGLE FILL ox%+cb.x%+cb.w%-S%-2,oy%-cb.y%,S%,-S%+2
              MOUSE ON 0
              cb.mouse% = TRUE
              = FALSE
            ENDIF
            IF cb.mouse% MOUSE ON 1
          ELSE
            IF dropped%% = p%% MOUSE ON 0
          ENDIF
          cb.mouse% = FALSE
        WHEN DLG_INKEY:
          IF X% = 148 IF FN(cb.handler%%)(ox%,oy%,p%%,DLG_CLICK,ox%+cb.x%+cb.w%-4,oy%-cb.y%)
          IF X% = 148 IF FN(cb.handler%%)(ox%,oy%,p%%,DLG_UNCLICK,ox%+cb.x%+cb.w%-4,oy%-cb.y%)
      ENDCASE
      IF p%% = dropped%% THEN
        C% = FN_listbox@dlg(ox%,oy%,cb.listbox%%,C%,X%,Y%)
        IF C% = cb.id% THEN
          PROC_setptr(ctl{},cb.listbox%%)
          cb.text$ = ctl.text$
          PROC_setptr(ctl{},cb.textbox%%)
          ctl.text$ = cb.text$
          IF FN(cb.handler%%)(ox%,oy%,p%%,DLG_CLICK,ox%+cb.x%,oy%-cb.y%)
          IF FN(cb.handler%%)(ox%,oy%,p%%,DLG_UNCLICK,ox%+cb.x%,oy%-cb.y%)
        ENDIF
      ELSE
        PROC_setptr(ctl{},cb.textbox%%)
        ctl.text$ = cb.text$
        C% = FN_textbox@dlg(ox%,oy%,cb.textbox%%,C%,X%,Y%)
        cb.text$ = ctl.text$
      ENDIF
      = C%

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
      DEF FN_upper(a$) IF LENa$=0 THEN =""
      LOCAL p%%
      FOR p%% = PTR(a$) TO PTR(a$)+LENa$-1
        IF ?p%% >= 97 IF ?p%% <= 122 ?p%% -= 32
      NEXT
      = a$
      ;
      REM Convert to 'title' case:
      DEF FN_title(A$)
      LOCAL S%
      REPEAT
        MID$(A$,S%+1,1) = FN_upper(MID$(A$,S%+1,1))
        S% = INSTR(A$, " ", S%+1)
      UNTIL S%=0
      = A$
      ;
      REM Convert to binary string:
      DEF FN_binary(N%) = FN_tobase(N%,2,-32*(N%<0))
      ;
      REM Convert N% to string in base B% with minimum M% digits:
      DEF FN_tobase(N%,B%,M%)
      LOCAL D%,A$
      REPEAT
        D% = N%MODB%
        N% DIV= B%
        IF D%<0 D% += B%:N% -= 1
        A$ = CHR$(48 + D% - 7*(D%>9)) + A$
        M% -= 1
      UNTIL (N%=FALSE OR N%=TRUE) AND M%<=0
      =A$
      ;
      REM Replace all occurrences of O$ with N$ starting at I%:
      REM The returned value is the number of replacements made
      DEF FN_findreplace(RETURN A$,O$,N$,I%)
      LOCAL C%
      REPEAT
        I% = INSTR(A$,O$,I%)
        IF I% THEN
          A$ = LEFT$(A$,I%-1)+N$+MID$(A$,I%+LEN(O$))
          I% += LEN(N$)
          C% += 1
        ENDIF
      UNTIL I% = 0
      = C%
      ;
      REM Replace all occurrences of O$ with N$ starting at I%:
      REM This version performs a case-insensitive comparison
      DEF FN_findreplacei(RETURN A$,O$,N$,I%)
      LOCAL C%
      REPEAT
        I% = FN_instri(A$,O$,I%)
        IF I% THEN
          A$ = LEFT$(A$,I%-1)+N$+MID$(A$,I%+LEN(O$))
          I% += LEN(N$)
          C% += 1
        ENDIF
      UNTIL I% = 0
      = C%
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
      REM Case-insensitive version of FN_instrr:
      DEF FN_instrri(A$, B$, S%)
      = FN_instrr(FN_lower(A$), FN_lower(B$), S%)
      ;
      REM Remove leading and trailing spaces:
      DEF FN_trim(A$)
      WHILE ASC(A$)=32 A$=MID$(A$,2) : ENDWHILE
      WHILE RIGHT$(A$)=" " A$=LEFT$(A$) : ENDWHILE
      = A$
      ;
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
      REM Join array elements using specified delimiter:
      DEF FN_join(a$(), d$, N%)
      LOCAL I%,A$
      FOR I% = 0 TO N%-1
        IF I%=N%-1 d$=""
        A$ += a$(I%) + d$
      NEXT
      = A$

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

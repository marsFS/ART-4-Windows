      MODE 7

      version$="v0.10"

      REM *** TODO LIST ***

      REM *** INKEY(-256) MATRIX BRANYDY &4D (INCLUDE OTHERS)

      REM *** INVESTIGATE LOCAL VERSIONS OF LIBRARIES TO MAKE PROGRAM CROSS BASIC COMPATIBLE (DONE?? Need Soruk to Test)

      REM *** LOAD SCREEN SORT BY NEWEST, NEEDS WORK!

      REM *** SCROLL OFF SCREEN E.G. NO WRAP

      REM *** IMPLEMENT ANIMATED CIRCLE

      REM *** IMAGE CONVERTER FOR IMPORTING BMP FILE (partially done)

      REM *** MODE 6 TEXT: 40x25  PIXELS: 640x500 GU: 1280x1000 COLOURS: 16

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

      REM sub menu options locations
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
      frame_max%=8
      frame_old%=0
      VDU 23,1,0;0;0;0; : REM Disable cursor

      PRINTTAB(1,1)"Welcome to";tm$;"Telepaint ";version$;
      PRINTTAB(0,5)ty$;"SELECT MAX FRAMES (2-40)";tw$;"-";tc$;" 8";tw$;"+"

      PRINTTAB(8,14)gr$;CHR$(157);ty$;"  IMPORT IMAGE    ";CHR$(156)
      PRINTTAB(8,17)gm$;CHR$(157);ty$;"  DISPLAY HELP    ";CHR$(156)
      PRINTTAB(8,20)gb$;CHR$(157);ty$;"LAUNCH TELEPAINT  ";CHR$(156)

      REPEAT
        IF frame_max%<>frame_old% THEN
          PRINTTAB(28,5)RIGHT$(" "+STR$(frame_max%),2)
          frame_old%=frame_max%
        ENDIF

        PROCREADMOUSE
        IF MB%=4 THEN
          CASE TY% OF
            WHEN 5 : REM change max frames var
              IF TX%=26 AND frame_max%>2 THEN frame_max%-=1
              IF TX%=31 AND frame_max%<40 THEN frame_max%+=1
              WAIT 10

            WHEN 14 : REM import image dialog
              IF TX%>8 AND TX%<29 THEN frame_old%=-3

            WHEN 17 : REM display help screen
              IF TX%>8 AND TX%<29 THEN frame_old%=-2

            WHEN 20 : REM launch telepaint
              IF TX%>8 AND TX%<29 THEN frame_old%=-1

          ENDCASE
        ELSE
          WAIT 10
        ENDIF
      UNTIL frame_old%<0
      PROCWAITMOUSE(0)
      menuext%=0

      REM frame buffer
      DIM frame_buffer&(frame_max%-1,959)

      REM undo buffer
      undo_max%=19
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


      PROCGR(curcol%,bakcol%,1)
      PROCdrawmenu

      FOR frame%=1 TO frame_max%
        PROCframesave(frame%)
        REM WAIT 10
      NEXT frame%
      frame%=1

      IF frame_old%=-2 THEN PROCshowhelp

      IF frame_old%=-3 THEN PROCimportimage

      VDU 23,1,1;0;0;0; : REM Enable cursor

      REM =====================
      REM main loop starts here
      REPEAT

        PROCREADMOUSE

        IF MX%<>OLD_MX% OR MY%<>OLD_MY% OR MB% THEN
          IF TY%=0 THEN
            REM click unside menu area
            IF MB%=4 THEN
              PROCWAITMOUSE(0)
              IF TY%=0 THEN
                CASE TX% OF
                  WHEN 0 : PROCmenurestore: PROCcontrolcodes : REM display control codes

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
                      IF menuext%>0 THEN PROCmenurestore
                      PROCmenusave
                      menuext%=1
                    ELSE
                      PROCmenurestore
                    ENDIF

                  WHEN 21 : erase%=(erase%+1) AND 1 : REM toggle erase tool

                  WHEN 23 : PROCmenurestore:PROCundorestore : REM undo
                  WHEN 25 : PROCmenurestore:PROCredorestore : REM redo

                  WHEN 27 : PROCmenurestore:PROCclearscreen:toolsel%=1:toolcursor%=15 : REM clearscreen
                  WHEN 28 : toolsel%=5:toolcursor%=TX% : REM background colour
                  WHEN 29 : toolsel%=6:toolcursor%=TX% : REM foreground colour

                  WHEN 31 : PROCmenurestore:PROCloadfile(0) : REM load file dialog - 0 load bin file
                  WHEN 32 : PROCmenurestore:PROCsavefile : REM save frames to file

                  WHEN 34 : animation%=(animation%+1) AND 1 : REM toggle frame animation advance tool

                    REM                  WHEN 36 : REM frame%
                  WHEN 36,37 : PROCmenurestore:PROCloadnextframe(-1,1) : REM save current frame and display previous frame in sequence
                  WHEN 38 : PROCmenurestore:PROCloadnextframe(1,1) : REM save current frame and display next frame in sequence
                  WHEN 39 : PROCmenurestore:PROCplay : REM save current frame and play all frames in a loop


                ENDCASE

                REM hide shape menu if another menu item was clicked
                IF TX%<>19 AND menuext%=1 THEN PROCmenurestore
                IF toolsel%<>4 THEN shapesel%=-1

                PROCdrawmenu

              ENDIF
            ENDIF
          ELSE
            REM handle special / sub menu
            IF menuext%=1 AND TY%<23 THEN
              IF MB%=4 THEN
                PROCWAITMOUSE(0)
                CASE TY% OF
                  WHEN 1
                    CASE TX% OF
                      WHEN 36,37,38,39 : REM showhelp
                        MODE7
                        menuext%=0
                        PROCdrawmenu
                        PROCshowhelp
                        menuext%=1
                    ENDCASE

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

                    ENDCASE

                  WHEN 4
                    CASE TX% OF
                      WHEN 1,2,3: shapesel%=2 : REM circle

                      WHEN 24:  REM animated len decrement
                        animatelen%-=1
                        IF animatelen%<1 THEN animatelen%=1
                      WHEN 28:  REM animated len increment
                        animatelen%+=1
                        IF animatelen%>5 THEN animatelen%=5

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

                  WHEN 22 : REM close button
                    PROCmenurestore

                ENDCASE

                IF shapesel%>-1 THEN toolsel%=4:toolcursor%=19

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
                      IF animation% THEN PROCloadnextframe(1,1)

                    WHEN 7: REM copy / paste tool

                      IF copypaste%=0 THEN
                        PROCframesave(frame%)
                        PROCmenusave
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
                        PROCrectangle(startx%,starty%,TX%*2+1,TY%*3+2,2)

                        copypaste%=1
                        PROCmenurestore

                        PROCcopyregion(startx%/2,starty%/3,TX%,TY%)

                        PROCdrawmenu

                      ELSE
                        REPEAT
                          PROCREADMOUSE
                          WAIT 2
                        UNTIL MB%=0
                        PROCpasteregion(TX%,TY%)

                      ENDIF

                      IF animation% THEN PROCloadnextframe(1,1)

                    WHEN 3: REM FILL TOOL
                      PROCundosave
                      PROCfloodfill(PX%,PY%)
                      REPEAT
                        PROCREADMOUSE
                        WAIT 2
                      UNTIL MB%=0
                      IF animation% THEN PROCloadnextframe(1,1)

                    WHEN 4: REM shape / special tools
                      CASE shapesel% OF
                        WHEN 0: REM line tool
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
                            animatelencount%=animatelen%
                            animategapcount%=0
                            PROCbresenham_buf(startx%,starty%,PX%,PY%,1-erase%)
                            frame%=oldframe%-1
                            PROCloadnextframe(1,0)
                          ELSE
                            PROCbresenham(startx%,starty%,PX%,PY%,1-erase%)
                            IF animation% THEN PROCloadnextframe(1,1)
                          ENDIF

                        WHEN 1: REM rectangle tool
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
                            animatelencount%=animatelen%
                            animategapcount%=0
                            PROCrectangle_buf(startx%,starty%,PX%,PY%,1-erase%)
                            frame%=oldframe%-1
                            PROCloadnextframe(1,0)
                          ELSE
                            PROCrectangle(startx%,starty%,PX%,PY%,1-erase%)
                            IF animation% THEN PROCloadnextframe(1,1)
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
                          PROCcircle(startx%,starty%,startx%-PX%,1-erase%)
                          IF animation% THEN PROCloadnextframe(1,1)

                        WHEN 3,4,5,6 : REM special control codes
                          PROCundosave
                          IF TX%<40 AND TX%>-1 AND TY%>0 AND TY%<25 THEN VDU 31,TX%,TY%,scode&((shapesel%-3)*2+erase%)
                          REPEAT
                            PROCREADMOUSE
                            IF TX%<>OLD_TX% OR TY%<>OLD_TY% THEN
                              IF TX%<40 AND TX%>-1 AND TY%>0 AND TY%<25  VDU 31,TX%,TY%,scode&((shapesel%-3)*2+erase%)
                            ENDIF
                            OLD_TX%=TX%
                            OLD_TY%=TY%
                          UNTIL MB%=0
                          IF animation% THEN PROCloadnextframe(1,1)

                        WHEN 7: REM text print tool
                          PROCundosave
                          PROCWAITMOUSE(0)
                          A$=LEFT$(text$,40-TX%)
                          PRINTTAB(TX%,TY%)A$;
                          IF animation% THEN PROCloadnextframe(1,1)

                        WHEN 8: REM foreground entire column
                          PROCundosave
                          REPEAT
                            PROCREADMOUSE
                          UNTIL MB%=4
                          PROCWAITMOUSE(0)

                          FOR Y%=1 TO 24
                            IF TX%<40 AND TX%>-1 AND TY%>0 AND TY%<25 THEN
                              VDU 31,TX%,Y%,(curcol%+144-textfore%*16)
                            ELSE
                              EXIT FOR
                            ENDIF
                          NEXT
                          IF animation% THEN PROCloadnextframe(1,1)

                        WHEN 9: REM backgroung entire column
                          PROCundosave
                          REPEAT
                            PROCREADMOUSE
                          UNTIL MB%=4
                          PROCWAITMOUSE(0)

                          FOR Y%=1 TO 24
                            IF TX%<39 AND TX%>-1 AND TY%>0 AND TY%<25 THEN
                              IF erase% THEN
                                VDU 31,TX%,Y%,156
                              ELSE
                                VDU 31,TX%,Y%,(curcol%+144),157
                              ENDIF
                            ELSE
                              EXIT FOR
                            ENDIF
                          NEXT
                          IF animation% THEN PROCloadnextframe(1,1)

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
                      IF animation% THEN PROCloadnextframe(1,1)

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

        REM show mouse tracking details for debug
        REM PRINTTAB(0,1)SPC(40)
        REM PRINTTAB(0,1)"MX:";STR$(MX%);" MY:";STR$(MY%);" TX:";STR$(TX%);" TY:";STR$(TY%);" PX:";STR$(PX%);" PY:";STR$(PY%)

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


      IF menuext%=0 THEN
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
      ELSE
        IF menuext%=1 THEN
          VDU 31,LEN(text$)+6,20
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
      chr%=frame_buffer&(f%-1,cx%+cy%*40) AND &5F
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
      PRINTTAB(14,10)"abcdefghijklmno";CHR$(156);gw$

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

      FOR L%=4 TO 22
        PRINTTAB(0,L%)SPC(40);
      NEXT

      PRINTTAB(1,4)gw$;CHR$(232);STRING$(10,CHR$(172));tg$;"CLEARSCREEN";gw$;STRING$(10,CHR$(172));CHR$(180);
      FOR L%=5 TO 21
        PRINTTAB(1,L%)gw$;CHR$(234);STRING$(32," ");gw$;CHR$(181);
      NEXT


      PRINTTAB(4,12)tb$;"OPTION:  ";tc$;"CLS:";tg$;"Y";tc$;" FIX:";tg$;"Y";
      PRINTTAB(4,14)gg$;CHR$(157);tb$;"ALL FRAME  ";CHR$(156);" ";gg$;CHR$(157);tb$;"CUR FRAME  ";CHR$(156)

      PRINTTAB(4,15)tr$;STRING$(29,"-")

      PRINTTAB(4,16)tb$;"SCROLL:  ";tc$;"SKIP : HORZ : VERT"
      A$=STR$(skip%)+" "
      B$=LEFT$(STR$(scrollh%)+" ",2)
      C$=LEFT$(STR$(scrollv%)+" ",2)
      PRINTTAB(13,17)tw$;"-";ty$;A$;tw$;"+ -";ty$;B$;tw$;"+ -";ty$;C$;tw$+"+"

      A$=LEFT$(STR$(framedupe%)+" ",2)
      B$=RIGHT$(" "+STR$(frame_max% DIV framedupe%),2)+"/"+STR$(frame_max%)
      PRINTTAB(4,19)tc$;"FRAMES: ";tw$;"-";ty$;A$;tw$+"+";tb$;"COPIES:";ty$;B$

      REM      PRINTTAB(13,19)tw$+"-"+ty$+A$+tw$+"+"

      PRINTTAB(4,21)gg$;CHR$(157);tb$;"DUPE FRAME  ";CHR$(156);gr$;CHR$(157);ty$;" CANCEL   ";CHR$(156);

      PRINTTAB(1,22)gw$;CHR$(170);STRING$(33,CHR$(172));CHR$(165);

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
              IF TX%>5 AND TX%<19 THEN done%=1 : REM select all frame clearscreen
              IF TX%>22 AND TX%<34 THEN done%=2 : REM select cur frame clearscreen

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

            WHEN 19
              CASE TX% OF
                WHEN 14 : REM frame count decrement
                  IF framedupe%>1 THEN framedupe%-=1
                WHEN 19 : REM frame count increment
                  IF framedupe%<(frame_max% DIV 2) THEN framedupe%+=1
              ENDCASE
              IF framedupe_old%<>framedupe% THEN
                A$=LEFT$(STR$(framedupe%)+" ",2)
                B$=RIGHT$(" "+STR$(frame_max% DIV framedupe%),2)
                PRINTTAB(16,19)A$;
                PRINTTAB(29,19)B$;
                framedupe_old%=framedupe%
              ENDIF


            WHEN 21
              IF TX%>5 AND TX%<20 THEN done%=3 : REM SELECT DUPE SCREEN AND FINISH
              IF TX%>23 AND TX%<34 THEN done%=-1 : REM CANCEL SCLEARSCREEN DIALOG
            WHEN 23,24 : done%=-1 : REM CANCEL DIALOG

          ENDCASE
          IF col_old%<>curcol% OR bak_old%<>bakcol% THEN
            PROCupdateCS
            col_old%=curcol%
            bak_old%=bakcol%

          ENDIF
          IF skip_old%<>skip% THEN
            A$=LEFT$(STR$(skip%)+" ",2)
            PRINTTAB(16,17)A$;
            skip_old%=skip%
          ENDIF
          IF h_old%<>scrollh% THEN
            A$=LEFT$(STR$(scrollh%)+" ",2)
            PRINTTAB(23,17)A$;
            h_old%=scrollh%
          ENDIF
          IF v_old%<>scrollv% THEN
            A$=LEFT$(STR$(scrollv%)+" ",2)
            PRINTTAB(30,17)A$;
            v_old%=scrollv%
          ENDIF


        ENDIF
      UNTIL done%

      PROCWAITMOUSE(0)

      PROCmenurestore
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
      ENDCASE
      REMPROCloadnextframe(1,0)

      ENDPROC

      REM SAVE FRAME BUFFER
      DEF PROCframesave(f%)
      LOCAL U%

      FOR U%=0 TO 959
        frame_buffer&(f%-1,U%)=GET(U% MOD 40,U% DIV 40+1)
      NEXT

      ENDPROC

      REM RESTORE FRAME BUFFER
      DEF PROCframerestore(f%)
      LOCAL U%

      FOR U%=0 TO 959
        VDU 31,(U% MOD 40),(U% DIV 40+1),frame_buffer&(f%-1,U%)
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

      IF undo_count&(frame%-1)<undo_max% THEN undo_count&(frame%-1)+=1

      FOR U%=0 TO 959
        undo_buffer&(frame%-1,undo_index%(frame%-1),U%)=GET(U% MOD 40,U% DIV 40+1)
      NEXT

      undo_index%(frame%-1)+=1
      IF undo_index%(frame%-1)>undo_max% THEN undo_index%(frame%-1)=0

      redo_count&(frame%-1)=0

      PROCdrawmenu

      ENDPROC

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

      ENDIF

      ENDPROC

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

        undo_index%(frame%-1)+=1
        IF undo_index%(frame%-1)>undo_max% THEN undo_index%(frame%-1)=0
        IF undo_count&(frame%-1)<undo_max% THEN undo_count&(frame%-1)+=1

      ENDIF

      ENDPROC

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
    1 copylocky%=y1%

      ENDPROC

      REM paste copypaste buffer yo current frame
      DEF PROCpasteregion(x1%,y1%)
      LOCAL s%,X%,Y%

      s%=0

      IF copylockxt% THEN x1%=copylockx%
      IF copylockyt% THEN y1%=copylocky%

      IF copysize%>0 THEN
        PROCundosave
        FOR X%=x1% TO x1%+copyx%
          FOR Y%=y1% TO y1%+copyy%
            IF X%<40 AND X%>-1 AND Y%<25 AND Y%>0 THEN
              VDU 31,X%,Y%,copy_buffer&(s%)
            ENDIF
            s%+=1
          NEXT
        NEXT
      ENDIF

      ENDPROC


      REM menu buffer save frame
      DEF PROCmenusave
      LOCAL U%
      FOR U%=0 TO 959
        menu_buffer&(U%)=GET(U% MOD 40,U% DIV 40+1)
      NEXT

      ENDPROC

      REM menu buffer restore frame
      DEF PROCmenurestore
      LOCAL U%

      IF menuext% THEN
        FOR U%=0 TO 959
          VDU 31,(U% MOD 40),(U% DIV 40+1),menu_buffer&(U%)
        NEXT
        menuext%=0
      ENDIF
      ENDPROC

      REM save binary file
      DEF PROCsavebinaryfile(F$)
      LOCAL f%,u%
      f%=OPENOUT(F$)
      FOR u%=0 TO 999
        BPUT#f%,GET(u% MOD 40,u% DIV 40)
      NEXT
      CLOSE#f%
      ENDPROC

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

      REM loadfile - modified dirscan to include type array so files list can be displayed for mode 7
      REM loadtype determines the type of load / import function
      DEF PROCloadfile(loadtype%)
      LOCAL I%,N%,L%,F%,SEL%,SELOLD%,SELY%,INDEX%,INDEXOLD%,filetype$,fh%,MAXY%,MACT%

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
      FOR L%=4 TO 20
        PRINTTAB(0,L%)SPC(40);
      NEXT


      FOR L%=5 TO 19
        PRINTTAB(2,L%)gw$;CHR$(234);STRING$(30," ");gw$;CHR$(181);
      NEXT
      PRINTTAB(2,19)gw$;CHR$(170);STRING$(31,CHR$(172));CHR$(165);
      PRINTTAB(5,18)tb$;CHR$(157);tc$;"LOAD  ";CHR$(156);

      CASE loadtype% OF
        WHEN 0 : REM bin files
          filetype$=".bin"
          PRINTTAB(2,4)gw$;CHR$(232);STRING$(10,CHR$(172));tg$;"LOAD FILE";gw$;STRING$(10,CHR$(172));CHR$(180);
          PRINTTAB(15,18)tr$;CHR$(157);ty$;"LOAD LAST SAVE ";gw$;CHR$(156);
          MAXY%=19

        WHEN 1 : REM import bmp
          filetype$=".bmp"
          PRINTTAB(2,4)gw$;CHR$(232);STRING$(9,CHR$(172));tg$;"IMPORT FILE";gw$;STRING$(9,CHR$(172));CHR$(180);
          PRINTTAB(15,18)CHR$(156);
          PRINTTAB(1,21)tg$;"(*)";tw$;"SINGLE BOX CAPTURE"
          PRINTTAB(1,22)tg$;"( )";tw$;"GRID";tc$;"HOR";tw$;"-";ty$;"10";tw$;"+ ";tc$;"VER";tw$;"-";ty$;"02";tw$"+"
          MAXY%=23
          GT%=0
          GX%=10
          GY%=2

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
            IF MY%<>OLD_MY% THEN INDEX%+=SGN(MY%-OLD_MY%)
            IF INDEX%>N%-11 THEN INDEX%=N%-11
            IF INDEX%<1 THEN INDEX%=1
            IF SELY%=-1 THEN SELY%=MY%
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
          IF SELY%=MY% AND MACT%>4 AND MACT%<17 THEN
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
          IF TX%<2 OR TX%>32 OR TY%<5 OR TY%>MAXY% THEN F%=-1

          REM load and cancel buttons
          IF TY%=18 AND MACT%=18 THEN
            IF TX%>5 AND TX%<14 THEN F%=SEL%
            IF TX%>15 AND TX%<34 AND loadtype%=0 THEN F%=-2
          ENDIF

          REM grid size controls
          IF loadtype%=1 AND (MACT%=21 OR MACT%=22) THEN
            IF TY%=21 AND TX%>1 AND TX%<5 THEN GT%=0
            IF TY%=22 AND TX%>1 AND TX%<5 THEN GT%=1

            PRINTTAB(3,21+GT%)"*"
            PRINTTAB(3,22-GT%)" "

          ENDIF

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

        OLD_MY%=MY%

        WAIT 2
      UNTIL F%<>0

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
          ENDIF

        WHEN 1 : REM import bmp
          MODE 6
          VDU 23,1,0;0;0;0; : REM Disable cursor

          IF F%=-1 THEN
            COLOUR 9
            PRINTTAB(0,0)"NO FILE LOADED"
            menuext%=94
          ELSE

            OSCLI "DISPLAY """+curdir$+n$(SEL%)+""" 0,0,1280,1000"
            menuext%=95+GT%
            OLD_TX%=GX%
            OLD_TY%=GY%

          ENDIF

      ENDCASE

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

      REM save frames
      frame%=frame_max%
      FOR I%=1 TO frame_max%
        PROCloadnextframe(1,0)
        PROCsavebinaryfile(cursave$+"M7_" + D$ + "_" + STR$(frame%)+".BIN")
        OSCLI "SCREENSAVE """+cursave$+"M7_" + D$ + "_" + STR$(frame%)+".BMP"" 0,0,1280,1000"
        WAIT 10
      NEXT

      PROCloadnextframe(1,0)

      PROCmenusave : menuext%=99
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

      ENDPROC

      REM import picture to one or more frames
      DEF PROCimportimage

      LOCAL GX%,GY%

      menuext%=96
      done%=0

      PROCloadfile(1)

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

            GCOL 3,15
            FOR X%=0 TO GX%
              LINE startx%+X%*gridsx%,starty%,startx%+X%*gridsx%,starty%+gridsy%*GY%
            NEXT
            FOR Y%=0 TO GY%
              LINE startx%,starty%+Y%*gridsy%,startx%+gridsx%*GX%,starty%+Y%*gridsy%
            NEXT

            REPEAT
              PROCREADMOUSE
              IF OLDMX%<>MX% OR OLDMY%<>MY% THEN
                GCOL 3,15
                FOR X%=0 TO GX%
                  LINE startx%+X%*gridsx%,starty%,startx%+X%*gridsx%,starty%+gridsy%*GY%
                NEXT
                FOR Y%=0 TO GY%
                  LINE startx%,starty%+Y%*gridsy%,startx%+gridsx%*GX%,starty%+Y%*gridsy%
                NEXT

                gridsx%=(MX%-startx%)/GX%
                gridsy%=(MY%-starty%)/GY%

                FOR X%=0 TO GX%
                  LINE startx%+X%*gridsx%,starty%,startx%+X%*gridsx%,starty%+gridsy%*GY%
                NEXT
                FOR Y%=0 TO GY%
                  LINE startx%,starty%+Y%*gridsy%,startx%+gridsx%*GX%,starty%+Y%*gridsy%
                NEXT

                OLDMX%=MX%
                OLDMY%=MY%
              ENDIF

              REMPRINTTAB(0,3)"xs: ";RIGHT$("000"+STR$(startx%),4);" xe: ";RIGHT$("000"+STR$(gridsx%),4)
              REMPRINTTAB(0,4)"ys: ";RIGHT$("000"+STR$(starty%),4);" ye: ";RIGHT$("000"+STR$(gridsy%),4)


            UNTIL MB%=0
            FOR X%=0 TO GX%
              LINE startx%+X%*gridsx%,starty%,startx%+X%*gridsx%,starty%+gridsy%*GY%
            NEXT
            FOR Y%=0 TO GY%
              LINE startx%,starty%+Y%*gridsy%,startx%+gridsx%*GX%,starty%+Y%*gridsy%
            NEXT

            REM process selection(s)
            x1%=startx%
            y1%=starty%
            x2%=MX%
            y2%=MY%

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

      ENDIF
      PROCWAITMOUSE(0)
      MODE 7
      frame%=0
      PROCloadnextframe(1,0)
      menuext%=0


      ENDPROC


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

      REM help screen connectors
      DEF PROChelpbox(cx%,cy%,cw%,col%)
      GCOL 0,col%
      RECTANGLE cx%*32,(24-cy%)*40+4,cw%*32-2,34

      ENDPROC

      DEF PROChelpdot(cx%,cy%,col%)
      GCOL 0,col%
      RECTANGLE FILL cx%*32+8,(24-cy%)*40+16,16
      ENDPROC

      DEF PROChelpvline(cx%,cy%,col%)
      GCOL 0,col%
      LINE cx%*32+16,964,cx%*32+16,(24-cy%)*40+24
      ENDPROC

      DEF PROChelpvline2(cx%,cy%,cl%,col%)
      GCOL 0,col%
      LINE cx%*32+16,(24-cy%)*40+24,cx%*32+16,(24-(cy%+cl%))*40+24
      ENDPROC

      DEF PROChelphline(cx%,cy%,cw%,col%)
      GCOL 0,col%
      LINE cx%*32+16,(24-cy%)*40+24,(cx%+cw%)*32+16,(24-cy%)*40+24
      ENDPROC


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
          IF x%<25 THEN
            LINE 0,x%*40,1279,x%*40
          ENDIF
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

      REM print text at x,y and clear to end of line
      DEF PROCprint40(y%,a$)
      LOCAL S$
      a$=LEFT$(a$,40)
      IF LEN(a$)<40 THEN S$=STRING$(40-LEN(a$)," ")
      PRINTTAB(0,y%)a$;S$;
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

      REM debug info
      REM PRINTTAB(0,1)STR$(undo_count&(frame%-1));"  ";STR$(undo_index%(frame%-1));"  "
      REM PRINTTAB(0,2)STR$(redo_count&(frame%-1));"  ";STR$(redo_index%(frame%-1));"  "

      REM shape and special sub menu
      IF menuext%=1 THEN
        D$=CHR$(32+animateshape%*10)
        A$=STR$(animategap%)
        F$=STR$(animatelen%)
        R$=CHR$(32+copylockxt%*10)
        U$=CHR$(32+copylockyt%*10)

        FOR Y%=1 TO 23
          PROCprint40(Y%,"")
        NEXT

        PRINTTAB(35,1)tm$;"HELP"
        PROCprint40(2,tg$+"( )"+tw$+"LINE     "+tg$+"("+D$+")"+tw$+"ANIM"+tb$+"(LINE AND RECT)")
        PROCprint40(3,tg$+"( )"+tw$+"RECT         "+tc$+"GAP:"+tw$+"-"+ty$+A$+tw$+"+")
        PROCprint40(4,tg$+"( )"+tw$+"CIRC         "+tc$+"LEN:"+tw$+"-"+ty$+F$+tw$+"+")
        PROCprint40(6,tg$+"( )"+tw$+"FLSH (136)  "+tg$+"( )"+tw$+"DBLH (141)")
        PROCprint40(7,tg$+"( )"+tw$+"SEPR (154)  "+tg$+"( )"+tw$+"HOLD (158)")
        PROCprint40(9,tg$+"( )"+tw$+"FORE "+tg$+"( )"+tw$+"BACK"+tb$+"(ENTIRE COLUMN)")
        PROCprint40(10,tg$+"("+R$+")"+tw$+"HORZ "+tg$+"("+U$+")"+tw$+"VERT"+tb$+"(LOCK PASTE POS)")
        PROCprint40(11,tg$+"( )"+tw$+"TEXT")
        D$=CHR$(129+showcodes%)

        IF shapesel%>-1 AND shapesel%<10 THEN
          PRINTTAB(sopt{(shapesel%)}.x%,sopt{(shapesel%)}.y%)"*"
        ENDIF

        IF caps% THEN
          PROCprint40(12,"  A B C D E F G H I J K L M N O P Q R")
          PROCprint40(14,"  S T U V W X Y Z 1 2 3 4 5 6 7 8 9 0")
        ELSE
          PROCprint40(12,"  a b c d e f g h i j k l m n o p q r")
          PROCprint40(14,"  s t u v w x y z 1 2 3 4 5 6 7 8 9 0")
        ENDIF
        PROCprint40(16,"  , . ` ~ ! @ # $ % ^ & * ( ) - _ = +")
        PROCprint40(18,"  [ ] ; { } \ | : ' "" < > / ?"+tc$+"SPC CAP" )

        PROCprint40(20,"TEXT:"+tg$+text$)
        PRINTTAB(36,20)tr$;"< X";
        PRINTTAB(15,22)tb$;CHR$(157);ty$;"CLOSE  ";CHR$(156)

        PROCprint40(24,ty$+"TelePaint"+tm$+version$+tc$+"by 4thStone & Pixelblip")

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
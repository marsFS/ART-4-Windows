      REM ART for Windows, written by FourthStone 2017

      REM Custom mode to suite portrait tablet device
      REM 752x920 Real Pixels : 1504x1840 Logical Pixels
      REM 94x57 Chars : 16x32 logical pixels
      REM 16 Colours (reversed standard palette)
      REM VDU 23,22,752;920;8,16,16,0

      REM HIMEM = (LOMEM+4000000) AND -4

      VDU 23,22,652;1024;8,16,16,0

      ON ERROR OSCLI "REFRESH ON" : ON : CLS : VDU 4 : REPORT : VDU 7 : END

      REM Switch off cursor, switch on print to graphics cursor, configure mouse pointer
      VDU 23,1,0;0;0;0;
      VDU 5
      MOUSE ON 3

      DIM beebPal%(15)

      REM Turn off screen refresh, must use *REFRESH to update screen
      *REFRESH OFF

      REM Pattern, mouse area, buttons and undo array, Fill Stack
      maCount%=7
      bCount%=0
      maxUndo%=20
      DIM pat{(17) c%(15)}
      DIM M{(maCount%) bx%,by%,bw%,bh%,mx%,my%,mx2%,my2%}
      DIM MI(maCount%)
      DIM TB{(bCount%) mx%,my%,mx2%,my2%}
      DIM FS% 40959
      REM DIM FB% 40959
      REM DIM U{(10) d%(40959)}
      REM DIM UB% 40960*maxUndo%

      REM Read pattern, palette and mouse area data, initialize screen
      PROCinitART

      REM TEST CODE
      REM      GCOL 2
      REM FOR X%=1 TO 20
      REM RECTANGLE FILL X%*8+16,1200+X%*4,7,3
      REM LINE X%*8+16,1260+X%*4,X%*8+24,1260+X%*4
      REM NEXT

      REM === Main loop ===
      REPEAT

        REM Make sure ART window has focus
        REM SYS "GetForegroundWindow" TO hw%
        REM IF hw% = @hwnd% THEN
        PROCreadmouse

        REM left mouse button clicked
        IF mb%=4 THEN
          PROCleftClick
        ELSE
          WAIT 2
        ENDIF
        REM ENDIF

      UNTIL fExit

      CLS
      *REFRESH ON
      VDU 4

      PRINT "Goodbye."
      QUIT

      REM === END OF MAIN PROGRAM ===

      REM *-----------------------*
      REM  ====== Functions ======

      REM ### find position of f$ in s$ starting at last char
      DEF FNINSTRREV(s$,f$)
      LOCAL i%
      IF LEN(s$)>0 THEN
        FOR i%=LEN(s$) TO 1 STEP -1
          IF MID$(s$,i%,LEN(f$))=f$ THEN =i%
        NEXT
      ENDIF
      =0

      REM ### return true if mouse position is in a defined mouse area
      DEF FNmRange(i%)
      IF mx%>=M{(i%)}.mx% AND mx%<=M{(i%)}.mx2% AND my%>=M{(i%)}.my% AND my%<=M{(i%)}.my2% THEN =TRUE
      =FALSE

      REM ### set pixel coords for relative to draw window
      DEF FNpx(x%)=(x%-M{(0)}.mx%) DIV 8
      DEF FNpy(y%)=(y%-M{(0)}.my%) DIV 4

      REM file exists
      DEF FNcheckfile(file$)
      LOCAL f%
      f% = OPENIN(file$)
      IF f% CLOSE #f%
      = f%


      REM *------------------------*
      REM  ====== Procedures ======

      REM ### read mouse status
      DEF PROCreadmouse

      MOUSE mx%, my%, mb%

      REM normalise mouse coords to pixel grid
      mx%=(mx% DIV 8)*8
      my%=(my% DIV 4)*4

      REM Update stats
      PROCshowstats

      *REFRESH

      ENDPROC

      REM read mouse until specified button is clicked
      DEF PROCwaitmouse(m%)
      REPEAT
        PROCreadmouse
        WAIT 2
      UNTIL mb%=m%
      ENDPROC

      REM ### left mouse click, check for mouse region and jump to required proc
      DEF PROCleftClick
      LOCAL i%, ma%
      REM
      FOR i%=0 TO maCount%
        IF FNmRange(i%) THEN ma%=i%+1: EXIT FOR
      NEXT

      CASE ma% OF
        WHEN 1: PROCdrawing
        WHEN 2: PROCcolSelect
        WHEN 3: PROCpalSelect
        WHEN 4: PROCbrushSize
        WHEN 5: PROCtools
        WHEN 7: PROCtoolSelect
      ENDCASE

      ENDPROC

      REM ### Tools Select region
      DEF PROCtoolSelect

      PROCwaitmouse(0)

      REM make sure we're still in tools select area
      IF FNmRange(6) THEN

        LOCAL tool%

        REM get button of tool clicked and action
        tool%=(mx%-M{(6)}.mx%) DIV 100+((M{(6)}.my2%-my%) DIV 100)*11

        IF tool%>-1 AND tool%<28 THEN
          CASE tool% OF
            WHEN 11 : PROCopensave(1) : REM save
            WHEN 0 : PROCopensave(0) : REM load
            WHEN 1 : PROCundo(1) : REM redo
            WHEN 2 : REMPROCundo(1) : REM redo
            WHEN 13 : PROCsaveundo: PROCaFill(0,4,4,-8,-8,0) : REM CLS
            WHEN 14 : dT%=(dT%+1) MOD 2: PROCtoolToggle(tool%,dT%*2)
            WHEN 5,6,15,16,17 : REM brush style
              IF tool%<>dS% THEN
                PROCtoolToggle(dS%,0)
                dS%=tool%
                PROCtoolToggle(dS%,6)
              ENDIF
            WHEN 12 : qS%=(qS%+1) MOD 2: PROCtoolToggle(tool%,qS%*2)

            OTHERWISE
              IF tool%<>cT% THEN
                PROCtoolToggle(cT%,0)
                cT%=tool%
                PROCtoolToggle(cT%,tC%)
              ENDIF

          ENDCASE

          PROCdt(M{(4)}.bx%+60,M{(4)}.by%+68,2,STR$(tool%),4)
          PROCdt(M{(4)}.bx%+268,M{(4)}.by%+68,2,STR$(cT%),4)
        ENDIF
      ENDIF
      ENDPROC

      REM ### toggle box control status
      DEF PROCtoolToggle(i%,c%)
      LOCAL x%,y%,lx%,ly%
      IF c%=0 THEN c%=7
      GCOL c%
      REMRECTANGLE M{(6)}.mx%+(i% MOD 2)*100,M{(6)}.my2%-(i% DIV 2)*100-96,98,98
      REMRECTANGLE M{(6)}.mx%+(i% MOD 2)*100+2,M{(6)}.my2%-(i% DIV 2)*100-94,94,94
      x%=M{(6)}.mx%+(i% MOD 11)*100
      y%=M{(6)}.my2%-(i% DIV 11)*100-100

      FOR lx%=x% TO x%+98 STEP 2
        FOR ly%=y% TO y%+98 STEP 2
          IF POINT(lx%,ly%) THEN LINE lx%,ly%,lx%,ly%
        NEXT
      NEXT

      REM alternate highlight using xor colour
      REM GCOL 3,3
      REM FOR ly%=y% TO y%+98 STEP 2
      REM LINE x%,ly%,x%+98,ly%
      REM NEXT


      ENDPROC

      REM ### draw on main canvas
      DEF PROCdrawing
      LOCAL sx%,sy%,ox%,oy%

      sx%=mx%
      sy%=my%
      ox%=mx%
      oy%=my%

      PROCsaveundo

      REPEAT
        PROCreadmouse

        CASE cT% OF
          WHEN 3:
            CASE dS% OF
              WHEN 5 : PROCdBrush(FNpx(mx%),FNpy(my%),dw%,0) : REM standard brush
              WHEN 6 : PROCdCircle(mx%,my%,dw%*4) : REM circle brush
              WHEN 17 : PROCdBrush(FNpx(mx%),FNpy(my%),dw%,2) : REM airbrush
              WHEN 15 : PROCdBrush(FNpx(mx%),FNpy(my%),dw%,1) : REM standard X2 brush
            ENDCASE

          WHEN 4: REM line
            GCOL 3,7
            LINE sx%,sy%,ox%,oy%
            LINE sx%,sy%,mx%,my%
            ox%=mx%
            oy%=my%

          WHEN 18,19: REM polygon filled
            GCOL 3,7
            CIRCLE sx%,sy%,sx%-ox%
            CIRCLE sx%,sy%,sx%-mx%
            ox%=mx%
            oy%=my%

          WHEN 7,8,20,21: REM boxes, gradient
            GCOL 3,7
            RECTANGLE sx%,sy%,ox%-sx%,oy%-sy%
            RECTANGLE sx%,sy%,mx%-sx%,my%-sy%
            ox%=mx%
            oy%=my%

          WHEN 9: REM flood fill
            i%=POINT(mx%,my%)
            IF i%<>fC% THEN
              fC%=i%
              PROCaFill(7,8,6,-14,-14,fC%)
            ENDIF
        ENDCASE

      UNTIL mb%=0

      GCOL 3,7

      CASE cT% OF
        WHEN 4: REM line draw finalisation
          LINE sx%,sy%,ox%,oy%
          LINE sx%,sy%,sx%,sy%

          PROCdLine(sx%,sy%,mx%,my%)

        WHEN 18,19: REM polygon finalisation
          CIRCLE sx%,sy%,sx%-ox%

          IF cT%=18 PROCdCircOut(sx%,sy%,sx%-ox%)
          IF cT%=19 PROCdCircle(sx%,sy%,sx%-ox%)

        WHEN 20,21: REM Gradient
          RECTANGLE sx%,sy%,ox%-sx%,oy%-sy%
          IF sx%<>ox% AND sy%<>oy% PROCdrawBoxG(sx%,sy%,ox%,oy%,cT%-20)

        WHEN 7,8: REM boxes
          RECTANGLE sx%,sy%,ox%-sx%,oy%-sy%
          PROCdrawBox(sx%,sy%,ox%,oy%,cT%-7)

        WHEN 9: REM flood fill
          PROCfloodFill(mx%,my%)
      ENDCASE

      ENDPROC

      REM ### drawing brush - generic routine
      DEF PROCdBrush(dx%,dy%,w%,d%)
      LOCAL lx%,ly%,dc%,s%,p%

      CASE d% OF
        WHEN 1 : REM x2 brush
          dx%=((dx%-w% DIV 2) DIV 2)*2
          dy%=((dy%-w%) DIV 2)*2
          s%=2

        OTHERWISE : REM standard brush
          dx%=dx%-w% DIV 2
          dy%=dy%-w%
          s%=1
      ENDCASE

      REM draw pattern loop centered at pixel location dx,dy
      FOR lx%=dx% TO dx%+w% STEP s%
        FOR ly%=dy% TO dy%+w%*2 STEP s%
          REM check for airbrush randomization before plotting
          p%=1
          IF d%=2 IF RND(1000)>5 THEN p%=0
          IF p% THEN
            REM range check, set pattern colour and plot
            IF lx%>-1 AND lx%<160 AND ly%>-1 AND ly%<256 THEN
              CASE d% OF
                WHEN 1 : REM x2 brush
                  pS%=(lx% DIV 2) MOD 4+((ly% DIV 2) MOD 4)*4
                OTHERWISE
                  pS%=lx% MOD 4+(ly% MOD 4)*4
              ENDCASE
              IF pat{(pat%)}.c%(pS%) THEN
                dc%=pcol%
              ELSE
                dc%=col%
              ENDIF

              REM Check for transparency
              IF dc%-dT%>-1 OR (pat{(pat%)}.c%(pS%)=1 AND col%=0) THEN
                GCOL dc%

                RECTANGLE FILL lx%*8+M{(0)}.mx%,ly%*4+M{(0)}.my%-2,7,3

                REM FB%?(lx%+ly%*160)=dc%
              ENDIF
            ENDIF
          ENDIF
        NEXT
      NEXT

      ENDPROC

      REM ### line drawing brush
      DEF PROCdLine(x1%,y1%,x2%,y2%)

      LOCAL dx%,dy%,sx%,sy%
      LOCAL e2,err

      REM get pixel coords for line
      x1%=FNpx(x1%) : y1%=FNpy(y1%)
      x2%=FNpx(x2%) : y2%=FNpy(y2%)

      REM determine which vector to use for err
      dx%=ABS(x2%-x1%)
      dy%=ABS(y2%-y1%)
      IF x1%<x2% THEN sx%=1 ELSE sx%=-1
      IF y1%<y2% THEN sy%=1 ELSE sy%=-1
      err=dx%-dy%

      REM Draw starting segment
      PROCdBrush(x1%,y1%,dw%,0)

      REM draw line loop
      REPEAT
        IF x1%=x2% AND y1%=y2% THEN EXIT REPEAT
        e2=2*err
        IF e2>-dy% THEN
          err=err-dy%
          x1%=x1%+sx%
          PROCVline2(x1%+(dw% DIV 2)*sx%,y1%-dw%,y1%+dw%)
        ENDIF
        IF e2<dx% THEN
          err=err+dx%
          y1%=y1%+sy%
          PROCHline2(x1%-(dw% DIV 2),x1%+(dw% DIV 2),y1%+dw%*sy%)
        ENDIF
        IF x1%<-dw% OR x1%>160+dw% THEN EXIT REPEAT
        IF y1%<-dw% OR y1%>256+dw% THEN EXIT REPEAT
        PROCreadmouse
      UNTIL 0

      ENDPROC

      REM ### draw vertical line with current pattern
      DEF PROCVline2(x1%,y1%,y2%)
      LOCAL ly%,dc%

      FOR ly%=y1% TO y2%
        REM range check, set pattern colour and plot
        IF x1%>-1 AND x1%<160 AND ly%>-1 AND ly%<256 THEN
          pS%=x1% MOD 4+(ly% MOD 4)*4
          IF pat{(pat%)}.c%(pS%) THEN
            dc%=pcol%
          ELSE
            dc%=col%
          ENDIF

          REM Check for transparency
          IF dc%-dT%>-1 THEN
            GCOL dc%
            RECTANGLE FILL x1%*8+M{(0)}.mx%,ly%*4+M{(0)}.my%-2,7,3
            REM FB%?(x1%+ly%*160)=dc%
          ENDIF
        ENDIF
      NEXT

      ENDPROC

      REM ### draw vertical line with current pattern
      DEF PROCHline2(x1%,x2%,y1%)
      LOCAL lx%,dc%

      FOR lx%=x1% TO x2%
        REM range check, set pattern colour and plot
        IF lx%>-1 AND lx%<160 AND y1%>-1 AND y1%<256 THEN
          pS%=lx% MOD 4+(y1% MOD 4)*4
          IF pat{(pat%)}.c%(pS%) THEN
            dc%=pcol%
          ELSE
            dc%=col%
          ENDIF

          REM Check for transparency
          IF dc%-dT%>-1 THEN
            GCOL dc%
            RECTANGLE FILL lx%*8+M{(0)}.mx%,y1%*4+M{(0)}.my%-2,7,3
            REM FB%?(lx%+y1%*160)=dc%
          ENDIF
        ENDIF
      NEXT

      ENDPROC

      REM ### box draw
      DEF PROCdrawBox(x1%,y1%,x2%,y2%,f%)
      x1%=FNpx(x1%) : y1%=FNpy(y1%)
      x2%=FNpx(x2%) : y2%=FNpy(y2%)

      LOCAL t%,lx%,ly%

      IF x1%>x2% THEN t%=x1%:x1%=x2%:x2%=t%
      IF y1%>y2% THEN t%=y1%:y1%=y2%:y2%=t%

      IF f% THEN
        FOR lx%=x1%-(dw% DIV 2) TO x2%+(dw% DIV 2)
          PROCVline2(lx%,y1%-dw%,y2%+dw%)
        NEXT
      ELSE
        FOR lx%=-(dw% DIV 2) TO (dw% DIV 2)
          PROCVline2(lx%+x1%,y1%-dw%,y2%+dw%)
          PROCVline2(lx%+x2%,y1%-dw%,y2%+dw%)
        NEXT
        FOR ly%=-dw% TO dw%
          PROCHline2(x1%-(dw% DIV 2),x2%+(dw% DIV 2),ly%+y1%)
          PROCHline2(x1%-(dw% DIV 2),x2%+(dw% DIV 2),ly%+y2%)
        NEXT
      ENDIF

      ENDPROC

      REM ### box draw gradient, d%=0 horizontal, d%=1 vertical
      DEF PROCdrawBoxG(x1%,y1%,x2%,y2%,d%)
      x1%=FNpx(x1%) : y1%=FNpy(y1%)
      x2%=FNpx(x2%) : y2%=FNpy(y2%)

      LOCAL lx%,ly%,dc%,gR,gRd,gAdd,dv%

      REM Calculate direction vector, gradient value and gradient default values
      dv%=1 : gR=0 : gRd=0
      IF x1%>x2% THEN
        SWAP x1%,x2%
        IF d%=0 dv%=-1
      ENDIF
      IF y1%>y2% THEN
        REM PROCupdateStatus("y1:"+STR$y1%+"  y2:"+STR$y2%)
        SWAP y1%,y2%
        IF d%=1 dv%=-1
      ENDIF
      IF dv%=-1 THEN gR=17.9: gRd=17.9

      IF d%=1 THEN
        gAdd=18/(y2%-y1%)*dv%
      ELSE
        gAdd=18/(x2%-x1%)*dv%
      ENDIF

      FOR lx%=x1% TO x2%
        IF d% THEN gR=gRd
        FOR ly%=y1% TO y2%
          REM range check, set pattern colour and plot
          IF lx%>-1 AND lx%<160 AND ly%>-1 AND ly%<256 THEN
            pS%=lx% MOD 4+(ly% MOD 4)*4
            IF pat{(INT(gR))}.c%(pS%) THEN
              dc%=pcol%
            ELSE
              dc%=col%
            ENDIF

            REM Check for transparency
            IF dc%-dT%>-1 THEN
              GCOL dc%
              RECTANGLE FILL lx%*8+M{(0)}.mx%,ly%*4+M{(0)}.my%-2,7,3
              REM FB%?(lx%+ly%*160)=dc%
            ENDIF
          ENDIF
          IF d%=1 THEN
            gR+=gAdd
            IF gR>17.9 gR=17.9
            IF gR<0 THEN gR=0
          ENDIF

        NEXT
        IF d%=0 THEN
          gR+=gAdd
          IF gR>17.9 THEN gR=17.8
          IF gR<0 THEN gR=0
        ENDIF

        REM*REFRESH
      NEXT

      ENDPROC

      REM ### circle Outline drawing brush
      DEF PROCdCircOut(x1%,y1%,r%)

      LOCAL t%
      r%=ABS(r%)

      FOR t%=0 TO 359
        REM PROCdStandard(FNpx(x1%+r%*COSRADt%),FNpy(INT(y1%-r%*SINRADt%)))
        CASE dS% OF
          WHEN 6 : PROCdCircle(x1%+r%*COSRADt%,y1%-r%*SINRADt%,dw%*4) : REM circle brush
          WHEN 17 : PROCdBrush(FNpx(INT(x1%+r%*COSRADt%)),FNpy(INT(y1%-r%*SINRADt%)),dw%,2) : REM airbrush
          WHEN 15 : PROCdBrush(FNpx(INT(x1%+r%*COSRADt%)),FNpy(INT(y1%-r%*SINRADt%)),dw%,1) : REM standard X2 brush

          OTHERWISE
            PROCdBrush(FNpx(INT(x1%+r%*COSRADt%)),FNpy(INT(y1%-r%*SINRADt%)),dw%,0)
        ENDCASE

        *REFRESH

      NEXT
      ENDPROC

      REM ### circle drawing brush
      DEF PROCdCircle(x1%,y1%,r%)

      LOCAL r2,dy

      r%=ABS(r%)
      r2=r%*r%

      FOR x%=r% TO 0 STEP -8
        dy=SQR(r2-x%*x%)
        PROCVline1(x1%-x%,INT(y1%-dy),INT(y1%+dy))
        PROCVline1(x1%+x%,INT(y1%-dy),INT(y1%+dy))
        REM PROCreadmouse
      NEXT
      ENDPROC

      REM ### draw vertical line with current pattern
      DEF PROCVline1(x1%,y1%,y2%)
      LOCAL ly%,dc%
      x1%=FNpx(x1%)
      y1%=FNpy(y1%)
      y2%=FNpy(y2%)

      FOR ly%=y1% TO y2%
        REM range check, set pattern colour and plot
        IF x1%>-1 AND x1%<160 AND ly%>-1 AND ly%<256 THEN
          pS%=x1% MOD 4+(ly% MOD 4)*4
          IF pat{(pat%)}.c%(pS%) THEN
            dc%=pcol%
          ELSE
            dc%=col%
          ENDIF

          REM Check for transparency
          IF dc%-dT%>-1 THEN
            GCOL dc%
            RECTANGLE FILL x1%*8+M{(0)}.mx%,ly%*4+M{(0)}.my%-2,7,3
            REM FB%?(x1%+ly%*160)=dc%
          ENDIF
        ENDIF
      NEXT

      ENDPROC

      REM ### flood fill with current pattern
      DEF PROCfloodFill(sx%,sy%)

      sx%=FNpx(sx%)
      sy%=FNpy(sy%)
      IF (sx%)>-1 AND (sx%)<160 AND (sy%)>-1 AND (sy%)<256 THEN

        LOCAL uf%,df%,c%,x%,y%,mc%,dc%,i%,fp% : REM fill,F$

        uf%=0
        df%=0
        fp%=0
        mc%=fC%

        REM create stack file
        REM F$=@dir$+"FILL.TMP"
        REM fill=OPENOUT F$
        REM IF fill<>0 THEN

        REM first iteration fills with mask colour (15) to replace fill colour
        REM second iteration replaces mask colour with current pattern
        FOR i%=0 TO 1

          REM fill with mask colour first
          REM BPUT#fill,sx%: BPUT#fill,sy%
          FS%?fp%=sx% : FS%?(fp%+1)=sy% : fp%+=2

          REPEAT
            REM get next fill point from fill list
            REM PTR#fill=PTR#fill-2
            REM x%=BGET#fill: y%=BGET#fill
            REM PTR#fill=PTR#fill-2
            fp%-=2 : x%=FS%?fp% : y%=FS%?(fp%+1)

            IF POINT(x%*8+M{(0)}.mx%,y%*4+M{(0)}.my%)=mc% THEN

              uf%=1 : df%=1

              REM scan left
              WHILE x%>0 AND POINT((x%-1)*8+M{(0)}.mx%,y%*4+M{(0)}.my%)=mc%
                x%-=1
              ENDWHILE

              REM scan right
              WHILE x%<160 AND POINT(x%*8+M{(0)}.mx%,y%*4+M{(0)}.my%)=mc%
                IF i%=0 THEN
                  dc%=15
                ELSE
                  pS%=x% MOD 4+(y% MOD 4)*4
                  IF pat{(pat%)}.c%(pS%) THEN
                    dc%=pcol%
                  ELSE
                    dc%=col%
                  ENDIF
                ENDIF

                GCOL dc%
                RECTANGLE FILL x%*8+M{(0)}.mx%,y%*4+M{(0)}.my%-2,7,3
                REM FB%?(x%+y%*160)=dc%

                REM detect colour changes above and add to list
                IF y%<255 THEN
                  c%=POINT(x%*8+M{(0)}.mx%,(y%+1)*4+M{(0)}.my%)
                  IF uf% AND c%=mc% THEN FS%?fp%=x% : FS%?(fp%+1)=y%+1 : fp%+=2: uf%=0
                  IF c%<>mc% THEN uf%=1
                ENDIF

                REM detect colour changes below and add to list
                IF y%>0 THEN
                  c%=POINT(x%*8+M{(0)}.mx%,(y%-1)*4+M{(0)}.my%)
                  IF df% AND c%=mc% THEN FS%?fp%=x% : FS%?(fp%+1)=y%-1 : fp%+=2: df%=0
                  IF c%<>mc% THEN df%=1
                ENDIF
                x%+=1
              ENDWHILE
            ENDIF

            *REFRESH

          UNTIL fp%=0
          mc%=15
        NEXT

        REMCLOSE#0
        REM OSCLI "DEL """+F$+""""
        REM PROCupdateStatus("INFO: Fill operation completed")
        REMELSE
        REM PROCupdateStatus("ERROR: Stack file could not be created")
        REMENDIF
      ELSE
        REM PROCupdateStatus("INFO: Fill area must be inside drawing area")
      ENDIF

      ENDPROC

      REM ### Check for colour select region
      DEF PROCcolSelect
      REPEAT
        PROCreadmouse
        i%=(my%-M{(1)}.my%+8) DIV 48
        IF col%<>i% AND i%>-1 AND i%<8 THEN
          PROCdrawColSel(col%,0)
          PROCdrawColSel(i%,7)
          col%=i%
          PROCpalette
          PROCbrush
        ENDIF
      UNTIL mb%=0
      ENDPROC

      DEF PROCdrawColSel(i%,c%)
      GCOL c%
      RECTANGLE M{(1)}.bx%+12,i%*48+M{(1)}.by%+6,70,46
      ENDPROC

      REM ### Check for palette select region
      DEF PROCpalSelect
      LOCAL i%,j%
      REPEAT
        PROCreadmouse

        i%=((my%-M{(2)}.my%+8) DIV 48)
        j%=((mx%-M{(2)}.mx%) DIV 64)
        IF i%<0 THEN i%=0
        IF i%>7 THEN i%=7
        IF j%<0 THEN j%=0
        IF j%>17 THEN j%=17
        REM        IF i%>-1 AND i%<8 AND J%>-1 AND J%<18 THEN
        IF pcol%<>i% OR pat%<>j% THEN
          GCOL 0
          RECTANGLE FILL M{(2)}.mx%+pat%*64,M{(2)}.my%-8+pcol%*48,64,4

          pcol%=i%
          pat%=j%
          PROCbrush
          REM     ENDIF
        ENDIF
      UNTIL mb%=0
      ENDPROC

      REM ### Check brush size change region
      DEF PROCbrushSize
      REPEAT
        PROCreadmouse

        s%=32-(my%-M{(3)}.my%-4) DIV 8
        IF s%<>dw% AND s%>-1 AND s%<33 THEN

          REM update brush size indicator
          FOR i%=0 TO 1
            GCOL i%*7
            CIRCLE FILL M{(3)}.bx%+12,M{(3)}.by%+(32-dw%)*8+8,10
            CIRCLE FILL M{(3)}.bx%+118,M{(3)}.by%+(32-dw%)*8+8,10
            dw%=s%
          NEXT

          PROCdBrushSize
        ENDIF
      UNTIL mb%=0
      ENDPROC

      REM ### Draw brush size
      DEF PROCdBrushSize

      LOCAL b%,lx%,ly%

      REM get pixel coords
      px%=((M{(3)}.bx%+276) DIV 8)-dw% DIV 2-1
      py%=((M{(3)}.by%+136) DIV 4)-dw%-1

      GCOL 0
      RECTANGLE FILL M{(3)}.bx%+136,M{(3)}.by%+6,264,260

      b%=0

      REM draw pattern loop
      FOR lx%=0 TO dw%
        FOR ly%=0 TO dw%*2
          pS%=(px%+lx%) MOD 4+((py%+ly%) MOD 4)*4
          IF pat{(pat%)}.c%(pS%) THEN
            GCOL pcol%
            IF pcol%<>0 THEN b%=1
          ELSE
            GCOL col%
            IF col%<>0 THEN b%=1
          ENDIF
          RECTANGLE FILL (px%+lx%)*8,(py%+ly%)*4+2,7,3
        NEXT
      NEXT
      IF b%=0 THEN
        GCOL 8
        RECTANGLE px%*8,py%*4+4,dw%*8+6,dw%*8+2
      ENDIF
      ENDPROC

      REM ### Tools region
      DEF PROCtools

      PROCwaitmouse(0)

      REM make sure we're still in tools area
      IF FNmRange(4) THEN
        LOCAL tool%

        REM scan button array for range match
        FOR i%=0 TO bCount%
          IF mx%>=TB{(i%)}.mx% AND mx%<=TB{(i%)}.mx2% AND my%>=TB{(i%)}.my% AND my%<=TB{(i%)}.my2% THEN tool%=i%+1: EXIT FOR
        NEXT

        PROCdt(M{(4)}.bx%+60,M{(4)}.by%+68,2,STR$(tool%),4)

        CASE tool% OF
          OTHERWISE
        ENDCASE
      ENDIF
      ENDPROC


      REM ### create mouse area coords
      DEF PROCsetMouseArea(i%,x%,y%,w%,h%,mx1%,my1%,mx2%,my2%)
      M{(i%)}.bx%=x% : M{(i%)}.by%=y%
      M{(i%)}.bw%=w% : M{(i%)}.bh%=h%
      M{(i%)}.mx%=x%+mx1% : M{(i%)}.my%=y%+my1%
      M{(i%)}.mx2%=(x%+w%)+mx2% : M{(i%)}.my2%=(y%+h%)+my2%
      ENDPROC

      REM ### Draw box with current brush
      DEF PROCbrush
      LOCAL lx%,ly%

      bx%=M{(5)}.bx%
      by%=M{(5)}.by%-4

      REM draw pattern loop
      FOR lx%=0 TO 13
        FOR ly%=0 TO 27
          pS%=(bx%+lx%) MOD 4+((by%+ly%) MOD 4)*4
          IF pat{(pat%)}.c%(pS%) THEN
            GCOL pcol%
          ELSE
            GCOL col%
          ENDIF
          RECTANGLE FILL bx%+lx%*8+10,by%+ly%*4+12,7,3
        NEXT
      NEXT

      REM highlight selected pattern
      GCOL 7
      RECTANGLE FILL M{(2)}.mx%+pat%*64,M{(2)}.my%-8+pcol%*48,64,4

      PROCdBrushSize

      ENDPROC

      REM ### Palette
      DEF PROCpalette
      LOCAL i%,p%,x%

      FOR i%=0 TO 7
        FOR p%=0 TO 17
          FOR x%=0 TO 15
            IF pat{(p%)}.c%(x%)=1 THEN
              GCOL i%
            ELSE
              IF i%=0 AND col%=0 THEN
                GCOL 8
              ELSE
                GCOL col%
              ENDIF
            ENDIF
            RECTANGLE FILL M{(2)}.mx%+p%*64+(x% MOD 4)*8,M{(2)}.my%+i%*48+(x% DIV 4)*4,7,3
            RECTANGLE FILL M{(2)}.mx%+p%*64+(x% MOD 4)*8,M{(2)}.my%+16+i%*48+(x% DIV 4)*4,7,3
            RECTANGLE FILL M{(2)}.mx%+32+p%*64+(x% MOD 4)*8,M{(2)}.my%+i%*48+(x% DIV 4)*4,7,3
            RECTANGLE FILL M{(2)}.mx%+32+p%*64+(x% MOD 4)*8,M{(2)}.my%+16+i%*48+(x% DIV 4)*4,7,3
          NEXT
        NEXT
      NEXT
      ENDPROC

      REM ### draw text at x,y, colour, <text>, length of string to delete first
      DEF PROCdt(x%,y%,c%,s$,l%)

      IF l% THEN
        GCOL 0
        RECTANGLE FILL x%,y%-32,16*l%,32
      ENDIF

      MOVE x%,y%
      GCOL c%
      PRINT s$

      ENDPROC

      REM ### draw mouse area heading
      DEF PROCMdt(i%,s$)
      MOVE M{(i%)}.bx%+4,M{(i%)}.by%+M{(i%)}.bh%+32
      GCOL 3
      PRINT s$
      ENDPROC

      REM ### Area Rectangle - draw rectangle relative to area coords
      DEF PROCaRec(i%,rx%,ry%,rw%,rh%)
      GCOL 7
      RECTANGLE M{(i%)}.bx%+rx%,M{(i%)}.by%+ry%,M{(i%)}.bw%+rw%,M{(i%)}.bh%+rh%
      ENDPROC

      REM ### Area Rectangle Fill - fill rectangle relative to area coords
      DEF PROCaFill(i%,rx%,ry%,rw%,rh%,c%)
      GCOL c%
      RECTANGLE FILL M{(i%)}.bx%+rx%,M{(i%)}.by%+ry%,M{(i%)}.bw%+rw%,M{(i%)}.bh%+rh%

      ENDPROC

      REM ### Draw button
      DEF PROCbutton(i%,x%,y%,s$)

      tx%=64-LEN(s$)*8

      PROCtBox(i%,x%,y%,128,42)

      MOVE x%+tx%,y%+34
      GCOL 3
      PRINT s$

      ENDPROC

      REM ### Draw button box
      DEF PROCtBox(i%,x%,y%,w%,h%)

      GCOL 8
      MOVE x%,y%
      DRAW x%+w%,y%
      DRAW x%+w%,y%+h%
      MOVE x%+w%-2,y%+h%-2
      DRAW x%+w%-2,y%+2
      DRAW x%+2,y%+2

      GCOL 7
      DRAW x%+2,y%+h%-2
      DRAW x%+w%-2,y%+h%-2
      MOVE x%+w%,y%+h%
      DRAW x%,y%+h%
      DRAW x%,y%

      REM toolbox range check dimensions
      TB{(i%)}.mx%=x%
      TB{(i%)}.my%=y%
      TB{(i%)}.mx2%=x%+w%
      TB{(i%)}.my2%=y%+h%

      ENDPROC

      REM ### Open Save dialoge
      DEF PROCopensave(a%)
      LOCAL filename$, D$,M$,T$,TMP$

      IF qS%=0 THEN
        REM normal file/save with dialog
        IF a%=1 THEN
          operation$="Save BMP"
          button$="SAVE"
        ELSE
          operation$="Open BMP"
          button$="LOAD"
        ENDIF

        filename$ = FN_filedlg(operation$, button$, "", "BMP FILES", ".BMP", 0)
        VDU 5
      ELSE
        M$="JanFebMarAprMayJunJulAugSepOctNovDec"

        REM BUILD DATE FORMAT AS YYYYMMDDHHMMSS
        T$=TIME$
        D$=MID$(T$,12,4)
        TMP$=STR$(INSTR(M$,MID$(T$,8,3)) DIV 3+1)
        IF LEN(TMP$)<2 THEN TMP$="0"+TMP$
        filename$="ART_QS_"+D$+TMP$+MID$(T$,5,2)+MID$(T$,17,2)+MID$(T$,20,2)+MID$(T$,23,2)+".BMP"
      ENDIF

      IF filename$="" THEN
        filename$= "File Not Selected!"
      ELSE
        CASE a% OF
          WHEN 0: OSCLI "DISPLAY """+filename$+""" "+STR$(M{(0)}.mx%)+","+STR$(M{(0)}.my%)+",1280,1024"
          WHEN 1: OSCLI "SCREENSAVE """ + filename$ + """ "+STR$(M{(0)}.mx%)+","+STR$(M{(0)}.my%)+",1280,1024"
        ENDCASE
      ENDIF

      REM PROCupdateStatus(MID$(filename$,FNINSTRREV(filename$,"\")+1) + " " + action$)

      PROCwaitmouse(0)

      ENDPROC

      REM ### create undo file
      DEF PROCsaveundo
      LOCAL F$
      F$=@dir$+"UNDO"+RIGHT$("0"+STR$(uC%),2)+".BMP"
      OSCLI "SCREENSAVE """+F$+""" "+STR$(M{(0)}.mx%)+","+STR$(M{(0)}.my%)+",1280,1024"
      REM FOR i%=0 TO 40959
      REM   UB%?(uC%*40960+i%)=FB%?i%
      REM NEXT

      REM update undo counter
      uC%+=1
      IF uC%>maxUndo% THEN uC%=0

      ENDPROC

      REM ### restore picture from undo file
      DEF PROCundo(d%)
      LOCAL F$
      uC%-=d%
      IF uC%<0 THEN uC%=maxUndo%
      IF uC%>maxUndo% THEN uC%=0

      F$=@dir$+"UNDO"+RIGHT$("0"+STR$(uC%),2)+".BMP"
      IF FNcheckfile(F$) THEN
        OSCLI "DISPLAY """+F$+""" "+STR$(M{(0)}.mx%)+","+STR$(M{(0)}.my%)+",1280,1024"
        REM PROCupdateStatus("Success - Undo: """+F$+"""")
      ELSE
        REM PROCupdateStatus("Error - Undo: """+F$+"""")
      ENDIF
      ENDPROC



      REM ### Update status area
      DEF PROCupdateStatus(s$)
      PROCdt(8,40,3,s$,80)
      ENDPROC

      REM ### Show stats
      DEF PROCshowstats

      px%=FNpx(mx%)
      py%=FNpy(my%)

      PROCdt(M{(4)}.bx%+60,M{(4)}.by%+260,2,STR$(mx%),4)
      PROCdt(M{(4)}.bx%+268,M{(4)}.by%+260,2,STR$(my%),4)
      PROCdt(M{(4)}.bx%+60,M{(4)}.by%+228,2,STR$(px%),4)
      PROCdt(M{(4)}.bx%+268,M{(4)}.by%+228,2,STR$(py%),4)
      PROCdt(M{(4)}.bx%+60,M{(4)}.by%+196,2,STR$(mb%),2)
      PROCdt(M{(4)}.bx%+60,M{(4)}.by%+164,2,STR$(col%),2)
      PROCdt(M{(4)}.bx%+268,M{(4)}.by%+164,2,STR$(pcol%),2)
      PROCdt(M{(4)}.bx%+60,M{(4)}.by%+132,2,STR$(pat%),2)
      PROCdt(M{(4)}.bx%+60,M{(4)}.by%+100,2,STR$(dw%),4)
      ENDPROC

      REM ### initialize screen
      DEF PROCinitART

      LOCAL a%,b%,c%,d%,e%,f%,g%,h%,i%,r%


      RESTORE

      REM READ Pattern data
      FOR i%=0 TO 17
        FOR h%=0 TO 15
          READ pat{(i%)}.c%(h%)
        NEXT
      NEXT

      REM Colour definitions for palette
      beebPal%(0) = 0
      FOR i%=1 TO 15
        READ r%,g%,b%
        COLOUR i%,r%,g%,b%
        beebPal%(i%) = b% + (g% << 8) + (r% << 16)
      NEXT

      REM Define mouse area lookup
      FOR i%=0 TO maCount%
        READ a%,b%,c%,d%,e%,f%,g%,h%
        PROCsetMouseArea(i%,a%,b%,c%,d%,e%,f%,g%,h%)
      NEXT

      REM Mouse and pixel coords
      mx%=0
      my%=0
      mb%=0
      px%=0
      py%=0

      REM Pattern and colour vars
      col%=1
      pat%=8
      pcol%=0
      pS%=0

      REM drawing vars
      dw%=8 : REM draw brush width
      dT%=1  : REM draw transparent flag
      dS%=5 : REM draw style selected button number
      cT%=3  : REM current drawing tool
      fC%=0  : REM fill colour
      uC%=0  : REM undo counter
      tC%=3  : REM tool toggle colour
      qS%=0  : REM quick save toggle

      fExit=FALSE

      REM Title
      FOR i%=0 TO 7
        MOVE 320+i%*4,2010+i%*2
        GCOL (i% DIV 7)*2+1
        PRINT "ART For SDL by FourthStone 2020"
      NEXT

      REM Mouse Area 0 - drawing region, double width border
      PROCaRec(0,0,0,0,0)
      PROCaRec(0,2,2,-4,-4)

      REM Mouse Area 1 - colour select
      PROCaRec(1,0,0,0,0)
      PROCdrawColSel(col%,7)
      FOR i%=1 TO 7
        GCOL i%
        RECTANGLE FILL M{(1)}.mx%+12,M{(1)}.my%+i%*48,56,32
      NEXT

      REM Mouse Area 2 - palette area
      PROCaRec(2,0,0,0,0)

      REM Mouse Area 3 - Brush size
      PROCaRec(3,0,0,0,0)
      GCOL 6
      CIRCLE FILL M{(3)}.bx%+64,M{(3)}.by%+40,30
      CIRCLE FILL M{(3)}.bx%+64,M{(3)}.by%+104,22
      CIRCLE FILL M{(3)}.bx%+64,M{(3)}.by%+154,18
      CIRCLE FILL M{(3)}.bx%+64,M{(3)}.by%+194,12
      CIRCLE FILL M{(3)}.bx%+64,M{(3)}.by%+224,8
      GCOL 8
      MOVE M{(3)}.bx%+128,M{(3)}.my%+12
      DRAW M{(3)}.bx%+128,M{(3)}.my2%-12
      GCOL 7
      CIRCLE FILL M{(3)}.bx%+12,M{(3)}.by%+(32-dw%)*8+8,10
      CIRCLE FILL M{(3)}.bx%+118,M{(3)}.by%+(32-dw%)*8+8,10

      REM Mouse Area 4 - Tools
      PROCaRec(4,0,0,0,0)

      REM Mouse Area 5 - Current drawing pattern
      PROCaRec(5,0,0,0,0)

      REM Mouse Area 4 - Stats
      PROCdt(M{(4)}.bx%+12,M{(4)}.by%+260,2,"mX:",0)
      PROCdt(M{(4)}.bx%+220,M{(4)}.by%+260,2,"mY:",0)
      PROCdt(M{(4)}.bx%+12,M{(4)}.by%+228,2,"pX:",0)
      PROCdt(M{(4)}.bx%+220,M{(4)}.by%+228,2,"pY:",0)
      PROCdt(M{(4)}.bx%+12,M{(4)}.by%+196,2,"mB:",0)
      PROCdt(M{(4)}.bx%+12,M{(4)}.by%+164,2,"sC:",0)
      PROCdt(M{(4)}.bx%+220,M{(4)}.by%+164,2,"pC:",0)
      PROCdt(M{(4)}.bx%+12,M{(4)}.by%+132,2,"pS:",0)
      PROCdt(M{(4)}.bx%+12,M{(4)}.by%+100,2,"dW:",0)
      PROCdt(M{(4)}.bx%+12,M{(4)}.by%+68,2,"bT:",0)
      PROCdt(M{(4)}.bx%+220,M{(4)}.by%+68,2,"cT:",0)


      REM Mouse Area 6 - tool select area
      OSCLI "DISPLAY """+@dir$+"TOOLSTRIP_PORT.BMP"" "+STR$(M{(6)}.mx%)+","+STR$(M{(6)}.my%)
      PROCtoolToggle(cT%,tC%) : REM standard
      PROCtoolToggle(14,2) : REM transparency
      PROCtoolToggle(dS%,6) : REM draw style


      REM Mouse Area 7 - flood fill colour
      PROCaRec(7,0,0,0,0)

      REM Initialise palette and current brush
      PROCpalette
      PROCbrush


      ENDPROC

      REM Patterns 0 - 17, format: 4x4 grid
      DATA 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
      DATA 1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
      DATA 1,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0
      DATA 1,0,1,0,0,0,0,0,0,0,1,0,0,0,0,0
      DATA 1,0,1,0,0,0,0,0,1,0,1,0,0,0,0,0
      DATA 1,0,1,0,0,1,0,0,1,0,1,0,0,0,0,0
      DATA 1,0,1,0,0,1,0,0,1,0,1,0,0,0,0,1
      DATA 1,0,1,0,0,1,0,1,1,0,1,0,0,0,0,1
      DATA 1,0,1,0,0,1,0,1,1,0,1,0,0,1,0,1
      DATA 0,1,0,1,1,0,1,0,0,1,0,1,1,0,1,0
      DATA 0,1,0,1,1,0,1,0,0,1,0,1,1,1,1,0
      DATA 0,1,0,1,1,0,1,1,0,1,0,1,1,1,1,0
      DATA 0,1,0,1,1,0,1,1,0,1,0,1,1,1,1,1
      DATA 0,1,0,1,1,1,1,1,0,1,0,1,1,1,1,1
      DATA 0,1,0,1,1,1,1,1,1,1,0,1,1,1,1,1
      DATA 0,1,1,1,1,1,1,1,1,1,0,1,1,1,1,1
      DATA 0,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1
      DATA 1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1

      REM palette data
      DATA 255,000,000,000,255,000,255,255,000,000,000,255,255,000,255
      DATA 000,255,255,255,255,255,128,128,128,192,000,000,000,192,000
      DATA 192,192,000,000,000,192,192,000,192,000,192,192,192,192,192

      REM mouse area data: drawing, colour select, palette select, brush size select, tools, selected pattern, stats, tool select, fill colour
      REM format: bounding box x, y, w, h, mx left offset, my left offset, mx right offset, my right offset
      DATA 0,928,1294,1038,8,8,-6,-6
      DATA 1200,516,94,396,8,12,-8,-16
      DATA 0,516,1184,396,16,12,-24,-16
      DATA 16,16,416,276,4,2,-280,-2
      DATA 768,16,528,276,16,16,-16,-16
      DATA 526,16,128,128,0,0,0,0
      REM DATA 1508,640,200,300,0,0,0,0
      DATA 8,304,1100,208,0,0,0,0
      DATA 526,164,128,128,0,0,0,0


      REM ============================================================
      REM FILEDLG FUNCTIONS
      REM ============================================================

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

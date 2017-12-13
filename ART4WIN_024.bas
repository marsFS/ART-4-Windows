      REM ART for Windows, written by FourthStone 2017
      
      REM Custom mode to suite portrait tablet device
      REM 752x920 Real Pixels : 1504x1840 Logical Pixels
      REM 94x57 Chars : 16x32 logical pixels
      REM 16 Colours (reversed standard palette)
      REM VDU 23,22,752;920;8,16,16,0
      
      HIMEM = (LOMEM+4000000) AND -4
      
      VDU 23,22,1024;768;8,16,16,0
      
      ON ERROR OSCLI "REFRESH ON" : ON : CLS : VDU 4 : REPORT : PRINT " at line "; ERL : VDU 7 : END
      
      REM Switch off cursor, switch on print to graphics cursor, configure mouse pointer
      VDU 23,1,0;0;0;0;
      VDU 5
      MOUSE ON 3
      
      REM configure direct memory access to screen
      REM DIM BITMAPINFOHEADER{Size%, Width%, Height%, Planes{l&,h&}, BitCount{l&,h&}, Compression%, SizeImage%, XPelsPerMeter%, YPelsPerMeter%, ClrUsed%, ClrImportant%}
      REM DIM bmi{Header{} = BITMAPINFOHEADER{}}
      
      REM bmi.Header.Size% = DIM(BITMAPINFOHEADER{})
      REM bmi.Header.Width% = 1024
      REM bmi.Header.Height% = 768
      REM bmi.Header.Planes.l& = 1
      REM bmi.Header.BitCount.l& = 32
      
      REM SYS "CreateDIBSection", @memhdc%, bmi{}, 0, ^bits%, 0, 0 TO hbitmap%
      REM IF hbitmap% = 0 ERROR 100, "Couldn't create DIBSection"
      
      REM SYS "SelectObject", @memhdc%, hbitmap% TO oldhbm%
      REM SYS "DeleteObject", oldhbm%
      
      REM bytesperpixel% = 4
      REM bytesperline% = bmi.Header.Width% * 4
      
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
      
      REM === Main loop ===
      REPEAT
        
        REM Make sure ART window has focus
        SYS "GetForegroundWindow" TO hw%
        IF hw% = @hwnd% THEN
          PROCreadmouse
          
          REM left mouse button clicked
          IF mb%=4 THEN
            PROCleftClick
          ELSE
            WAIT 2
          ENDIF
        ENDIF
        
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
      REM Clear mouse button
      REPEAT
        PROCreadmouse
      UNTIL mb%=0
      
      REM make sure we're still in tools select area
      IF FNmRange(6) THEN
        
        LOCAL tool%
        
        REM get button of tool clicked and action
        tool%=(mx%-M{(6)}.mx%) DIV 100+((M{(6)}.my2%-my%) DIV 100)*2
        
        IF tool%>-1 AND tool%<28 THEN
          CASE tool% OF
            WHEN 0 : PROCopensave(1) : REM save
            WHEN 1 : PROCopensave(0) : REM load
            WHEN 2 : PROCundo(1) : REM redo
            WHEN 3 : REMPROCundo(1) : REM redo
            WHEN 19 : PROCsaveundo: PROCaFill(0,4,4,-8,-8,0) : REM CLS
            WHEN 18 : dT%=(dT%+1) MOD 2: PROCtoolToggle(tool%,dT%*2)
            WHEN 20,21,22,23,24 : REM brush style
              IF tool%<>dS% THEN
                PROCtoolToggle(dS%,0)
                dS%=tool%
                PROCtoolToggle(dS%,6)
              ENDIF
            WHEN 27 : qS%=(qS%+1) MOD 2: PROCtoolToggle(tool%,qS%*2)
              
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
      x%=M{(6)}.mx%+(i% MOD 2)*100
      y%=M{(6)}.my2%-(i% DIV 2)*100-96
      
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
          WHEN 4:
            CASE dS% OF
              WHEN 20 : PROCdBrush(FNpx(mx%),FNpy(my%),dw%,0) : REM standard brush
              WHEN 21 : PROCdCircle(mx%,my%,dw%*4) : REM circle brush
              WHEN 23 : PROCdBrush(FNpx(mx%),FNpy(my%),dw%,2) : REM airbrush
              WHEN 24 : PROCdBrush(FNpx(mx%),FNpy(my%),dw%,1) : REM standard X2 brush
            ENDCASE
            
          WHEN 5: REM line
            GCOL 3,7
            LINE sx%,sy%,ox%,oy%
            LINE sx%,sy%,mx%,my%
            ox%=mx%
            oy%=my%
            
          WHEN 6,7: REM polygon filled
            GCOL 3,7
            CIRCLE sx%,sy%,sx%-ox%
            CIRCLE sx%,sy%,sx%-mx%
            ox%=mx%
            oy%=my%
            
          WHEN 8,9,12,13: REM boxes, gradient
            GCOL 3,7
            RECTANGLE sx%,sy%,ox%-sx%,oy%-sy%
            RECTANGLE sx%,sy%,mx%-sx%,my%-sy%
            ox%=mx%
            oy%=my%
            
          WHEN 10: REM flood fill
            i%=POINT(mx%,my%)
            IF i%<>fC% THEN
              fC%=i%
              PROCaFill(7,8,6,-14,-14,fC%)
            ENDIF
        ENDCASE
        
      UNTIL mb%=0
      
      GCOL 3,7
      
      CASE cT% OF
        WHEN 5: REM line draw finalisation
          LINE sx%,sy%,ox%,oy%
          LINE sx%,sy%,sx%,sy%
          
          PROCdLine(sx%,sy%,mx%,my%)
          
        WHEN 6,7: REM polygon finalisation
          CIRCLE sx%,sy%,sx%-ox%
          
          IF cT%=6 PROCdCircOut(sx%,sy%,sx%-ox%)
          IF cT%=7 PROCdCircle(sx%,sy%,sx%-ox%)
          
        WHEN 12,13: REM Gradient
          RECTANGLE sx%,sy%,ox%-sx%,oy%-sy%
          IF sx%<>ox% AND sy%<>oy% PROCdrawBoxG(sx%,sy%,ox%,oy%,cT%-12)
          
        WHEN 8,9: REM boxes
          RECTANGLE sx%,sy%,ox%-sx%,oy%-sy%
          PROCdrawBox(sx%,sy%,ox%,oy%,cT%-8)
          
        WHEN 10: REM flood fill
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
                
                RECTANGLE FILL lx%*8+M{(0)}.mx%,ly%*4+M{(0)}.my%-2,8,4
                
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
            RECTANGLE FILL x1%*8+M{(0)}.mx%,ly%*4+M{(0)}.my%-2,8,4
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
            RECTANGLE FILL lx%*8+M{(0)}.mx%,y1%*4+M{(0)}.my%-2,8,4
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
        PROCupdateStatus("y1:"+STR$y1%+"  y2:"+STR$y2%)
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
              RECTANGLE FILL lx%*8+M{(0)}.mx%,ly%*4+M{(0)}.my%-2,8,4
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
          WHEN 21 : PROCdCircle(x1%+r%*COSRADt%,y1%-r%*SINRADt%,dw%*4) : REM circle brush
          WHEN 23 : PROCdBrush(FNpx(INT(x1%+r%*COSRADt%)),FNpy(INT(y1%-r%*SINRADt%)),dw%,2) : REM airbrush
          WHEN 24 : PROCdBrush(FNpx(INT(x1%+r%*COSRADt%)),FNpy(INT(y1%-r%*SINRADt%)),dw%,1) : REM standard X2 brush
            
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
            RECTANGLE FILL x1%*8+M{(0)}.mx%,ly%*4+M{(0)}.my%-2,8,4
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
                RECTANGLE FILL x%*8+M{(0)}.mx%,y%*4+M{(0)}.my%-2,8,4
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
        PROCupdateStatus("INFO: Fill operation completed")
        REMELSE
        REM PROCupdateStatus("ERROR: Stack file could not be created")
        REMENDIF
      ELSE
        PROCupdateStatus("INFO: Fill area must be inside drawing area")
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
          RECTANGLE FILL (px%+lx%)*8,(py%+ly%)*4+2,8,4
        NEXT
      NEXT
      IF b%=0 THEN
        GCOL 8
        RECTANGLE px%*8,py%*4+4,dw%*8+6,dw%*8+2
      ENDIF
      ENDPROC
      
      REM ### Tools region
      DEF PROCtools
      REM Clear mouse button
      REPEAT
        PROCreadmouse
      UNTIL mb%=0
      
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
          RECTANGLE FILL bx%+lx%*8+10,by%+ly%*4+12,8,4
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
            RECTANGLE FILL M{(2)}.mx%+p%*64+(x% MOD 4)*8,M{(2)}.my%+i%*48+(x% DIV 4)*4,8,4
            RECTANGLE FILL M{(2)}.mx%+p%*64+(x% MOD 4)*8,M{(2)}.my%+16+i%*48+(x% DIV 4)*4,8,4
            RECTANGLE FILL M{(2)}.mx%+32+p%*64+(x% MOD 4)*8,M{(2)}.my%+i%*48+(x% DIV 4)*4,8,4
            RECTANGLE FILL M{(2)}.mx%+32+p%*64+(x% MOD 4)*8,M{(2)}.my%+16+i%*48+(x% DIV 4)*4,8,4
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
      LOCAL lastFN$, filename$, action$, F$, ff$, N%, hbitmap%
      
      IF qS%=0 THEN
        REM normal file/save with dialog
        operation$="GetOpenFileName"
        IF a%=1 THEN operation$="GetSaveFileName"
        
        DIM fs{lStructSize%, hwndOwner%, hInstance%, lpstrFilter%, \
        \      lpstrCustomFilter%, nMaxCustFilter%, nFilterIndex%, \
        \      lpstrFile%, nMaxFile%, lpstrFileTitle%, \
        \      nMaxFileTitle%, lpstrInitialDir%, lpstrTitle%, \
        \      flags%, nFileOffset{l&,h&}, nFileExtension{l&,h&}, \
        \      lpstrDefExt%, lCustData%, lpfnHook%, lpTemplateName%}
        DIM fp{t&(260)}
        
        ff$ = "BMP files"+CHR$0+"*.BMP"+CHR$0+CHR$0: REM +"PNG files"+CHR$0+"*.PNG"+CHR$0
        fs.lStructSize% = DIM(fs{})
        fs.hwndOwner% = @hwnd%
        fs.lpstrFilter% = !^ff$
        fs.lpstrFile% = fp{}
        fs.nMaxFile% = 260
        fs.flags% = 6
        
        
        SYS operation$, fs{} TO result%
        IF result% filename$ = $$fp{}
        
      ELSE
        REM Quick save file finder
        lastFN$=@dir$+"ART_QS_"
        
        N%=0
        F$=""
        REPEAT
          F$=lastFN$+RIGHT$("0000"+STR$(N%),5)+".BMP"
          IF FNcheckfile(F$)=0 THEN
            filename$=F$
          ELSE
            N%+=1
          ENDIF
        UNTIL filename$<>"" OR N%>99999
        IF N%>99999 THEN PROCupdateStatus("ERROR: Cannot quicksave! File number limit reached, archive some files!!!"): ENDPROC
      ENDIF
      
      IF filename$="" THEN
        filename$= "File Not Selected!"
      ELSE
        CASE a% OF
          WHEN 0:action$="opened":OSCLI "DISPLAY """+filename$+""" "+STR$(M{(0)}.mx%)+","+STR$(M{(0)}.my%)+",1280,1024"
          WHEN 1
            action$="saved"
            OSCLI "SCREENSAVE """ + filename$ + """ "+STR$(M{(0)}.mx%)+","+STR$(M{(0)}.my%)+",1280,1024"
            REMSYS "LoadImage", 0, filename$, 0, 0, 0, 16 TO hbitmap%
            REMPROCsavepng(hbitmap%, LEFT$(filename$,LEN(filename$)-3)+"png")
            REMSYS "DeleteObject", hbitmap%
            REMOSCLI "DEL """+filename$+""""
        ENDCASE
      ENDIF
      
      PROCupdateStatus(MID$(filename$,FNINSTRREV(filename$,"\")+1) + " " + action$)
      
      
      REM flush mouse buffer
      REPEAT
        PROCreadmouse
      UNTIL mb%=0
      
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
        PROCupdateStatus("Success - Undo: """+F$+"""")
      ELSE
        PROCupdateStatus("Error - Undo: """+F$+"""")
      ENDIF
      ENDPROC
      
      
      
      REM ### Update status area
      DEF PROCupdateStatus(s$)
      PROCdt(8,40,3,s$,80)
      ENDPROC
      
      REM ### save png file
      DEF PROCsavepng(hbitmap%, filename$)
      LOCAL gdiplus%, ole32%
      
      SYS "LoadLibrary", "GDIPLUS.DLL" TO gdiplus%
      IF gdiplus% = 0 PROCupdateStatus("ERROR: Couldn't load GDIPLUS.DLL"):ENDPROC
      SYS "GetProcAddress", gdiplus%, "GdiplusStartup" TO `GdiplusStartup`
      SYS "GetProcAddress", gdiplus%, "GdiplusShutdown" TO `GdiplusShutdown`
      SYS "GetProcAddress", gdiplus%, "GdipCreateBitmapFromHBITMAP" TO `GdipCreateBitmapFromHBITMAP`
      SYS "GetProcAddress", gdiplus%, "GdipDisposeImage" TO `GdipDisposeImage`
      SYS "GetProcAddress", gdiplus%, "GdipSaveImageToFile" TO `GdipSaveImageToFile`
      SYS "LoadLibrary", "OLE32.DLL" TO ole32%
      SYS "GetProcAddress", ole32%, "CLSIDFromString" TO `CLSIDFromString`
      
      LOCAL tSI{}, lRes%, lGDIP%, lBitmap%, tPngEncoder%, guid%, filename%
      
      DIM tPngEncoder% LOCAL 15, guid% LOCAL 79, filename% LOCAL 2*LEN(filename$)+1
      DIM tSI{GdiplusVersion%, DebugEventCallback%, \
      \       SuppressBackgroundThread%, SuppressExternalCodecs%}
      
      REM Initialize GDI+
      tSI.GdiplusVersion% = 1
      SYS `GdiplusStartup`, ^lGDIP%, tSI{}, 0 TO lRes%
      IF lRes% PROCupdateStatus("ERROR GDI+ error "+STR$(lRes%)):ENDPROC
      
      REM Create the GDI+ bitmap from the image handle
      SYS `GdipCreateBitmapFromHBITMAP`, hbitmap%, 0, ^lBitmap% TO lRes%
      IF lRes% PROCupdateStatus("ERROR: GDI+ error "+STR$(lRes%))
      
      REM Initialize the encoder GUID
      SYS "MultiByteToWideChar", 0, 0, "{557CF406-1A04-11D3-9A73-0000F81EF32E}", -1, guid%, 40
      SYS `CLSIDFromString`, guid%, tPngEncoder%
      
      REM Save the image
      SYS "MultiByteToWideChar", 0, 0, filename$, -1, filename%, LEN(filename$)+1
      SYS `GdipSaveImageToFile`, lBitmap%, filename%, tPngEncoder%, 0 TO lRes%
      IF lRes% PROCupdateStatus("ERROR GDI+ error "+STR$(lRes%))
      
      REM Destroy the bitmap
      SYS `GdipDisposeImage`, lBitmap%
      
      REM Shutdown GDI+
      SYS `GdiplusShutdown`, lGDIP%
      SYS "FreeLibrary", gdiplus%
      
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
      dw%=16 : REM draw brush width
      dT%=1  : REM draw transparent flag
      dS%=20 : REM draw style selected button number
      cT%=4  : REM current drawing tool
      fC%=0  : REM fill colour
      uC%=0  : REM undo counter
      tC%=3  : REM tool toggle colour
      qS%=0  : REM quick save toggle
      
      fExit=FALSE
      
      REM Title
      FOR i%=0 TO 7
        MOVE 520+i%*4,1824+i%*2
        GCOL (i% DIV 7)*2+1
        PRINT "ART For Windows!!!"
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
      OSCLI "DISPLAY """+@dir$+"TOOLSTRIP.BMP"" "+STR$(M{(6)}.mx%)+","+STR$(M{(6)}.my%)
      PROCtoolToggle(cT%,tC%) : REM standard
      PROCtoolToggle(18,2) : REM transparency
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
      DATA 0,464,1294,1038,8,8,-6,-6
      DATA 1200,52,94,396,8,12,-8,-16
      DATA 0,52,1184,396,16,12,-24,-16
      DATA 1568,1200,406,276,4,2,-280,-2
      DATA 1508,52,528,276,16,16,-16,-16
      DATA 1568,1000,128,128,0,0,0,0
      REM DATA 1508,640,200,300,0,0,0,0
      DATA 1300,90,208,1396,0,0,0,0
      DATA 1728,1000,128,128,0,0,0,0
      
      

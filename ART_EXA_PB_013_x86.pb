;
; ------------------------------------------------------------
;
;   ART 4 Exagear - Suits 800x600 screens (576 actual vertical - 24 for menu)
;
;    (c) Fourthstone
;
; ------------------------------------------------------------
;

; png file support
UsePNGImageEncoder()
UsePNGImageDecoder()

; constants
#scrW=800; 960
#scrH=576
#drwW=640
#drwH=512
#maxUndo=20
#SCRNsize=163839 ; screen buffer array size
#RAWsize=20480

; structures
Structure MouseArea
  lx.w
  ly.w
  rx.w
  ry.w
  name.s
  img.a
EndStructure

Structure fillStack
  x.a
  y.a
EndStructure 

Structure Pixel
  Pixel.l
EndStructure

Structure rgbTable
  r.a
  g.a
  b.a
EndStructure

Structure ToggleList
  b.a
  c.a
EndStructure

Structure undoArray
  buf.a[#SCRNsize+1]
EndStructure

Structure drawLayers
  VIS.l                   ; visible flag
  SCRN.a[#SCRNsize+1]     ; screen buffer
  List lUndo.undoArray()  ; undo buffer
  List lRedo.undoArray()  ; redo buffer
EndStructure

; globals
Global pCol.a=0 ; pattern colour selected
Global pSel.a=8 ; pattern selected

Global dCol.a=1 ; drawing colour
Global dTrn.a=1 ; drawing transparency toggle
Global dWid.a=8 ; drawing width
Global dSel.a=16; drawing tool selected
Global dFil.i=0 ; fill colour
Global dMdx=160 ; current mode horizontal pixels
Global dMdy=256 ; current mode vertical pixels
Global dMpx=4   ; current mode horizontal pixel size
Global dMpy=2   ; current mode vertical pixel size
Global dLay=0   ; current drawing layer
Global dAll=1   ; all layers visible flag
Global dWire.a=0; draw wireframe 
Global dGRD.a=1 ; draw transparent grid

Global tCur.a=4 ; tool selected
Global tTog.a=3 ; tool toggle colour
Global tQSv.a=0 ; quick save toggle
Global tSel.a=0 ; new tool select 
Global tLIF.a=0 ; load image flag

Global maCount.a=9 ; mouse area count 0-n
Global mx,my,ox,oy,sx,sy,mact ; mouse x,y,action
Global imgToolStrip, imgToolStrip2, imgPAL, imgFinal, imgGRD; image handles

Global Dim dl.drawLayers(4) ; layers array
Global Dim SCRNout.a(#SCRNsize) ; final output buffer

Global Dim pat.a(17,15) ; drawing patterns
Global Dim bp(16)       ; beeb palette
Global Dim MA.MouseArea(maCount) ; mouse area structure array
Global Dim rgbT.rgbTable(16)     ; rgb lookup table
Global Dim ct(16)                ; colour table look for redrawing main canvas
Global Dim rawBBC.a(15)          ; raw bbc file format data
Global Dim revBBC.a(85)          ; reverse lookup bbc file format data
Global Dim curPat.a(15,15)         ; current drawing pattern colour matrix
Global Dim cusPat.a(17,63)       ; custom pattern array, 18 tiles of 8x8 colour data

Global NewList lToggle.togglelist()

Global NewList lFS.fillStack()


; Exit program and display a message
Procedure Exit_ART(m.s)
  If m<>"" 
    MessageRequester("Error", m, 0)
  EndIf
  
  End
EndProcedure

; set pixel coords For relative To draw window
Procedure px(x)
  ProcedureReturn (x-MA(0)\lx) / dMpx
EndProcedure
Procedure py(y)
  ProcedureReturn 255-((y-MA(0)\ly) / dMpy)
EndProcedure

; display program stats
Procedure showstats()
  Protected x,y
  x=30
  
  Box(MA(4)\lx+x,MA(4)\ly,44,32,bp(0))
  Box(MA(4)\lx+x+80,MA(4)\ly,44,32,bp(0))
  Box(MA(4)\lx+x+160,MA(4)\ly,44,32,bp(0))
  Box(MA(4)\lx+x+240,MA(4)\ly,44,32,bp(0))
  
  DrawText(MA(4)\lx+x,MA(4)\ly,Str(mx))
  DrawText(MA(4)\lx+x,MA(4)\ly+16,Str(my))
  
  DrawText(MA(4)\lx+x+80,MA(4)\ly,Str(px(mx)))
  DrawText(MA(4)\lx+x+80,MA(4)\ly+16,Str(py(my)))
  
  DrawText(MA(4)\lx+x+160,MA(4)\ly,Str(dWid))
  DrawText(MA(4)\lx+x+160,MA(4)\ly+16,Str(mact))
  
  DrawText(MA(4)\lx+x+240,MA(4)\ly,Str(tCur))
  DrawText(MA(4)\lx+x+240,MA(4)\ly+16,Str(tSel))
  
EndProcedure

; draw box outline, assumes startdrawing is already active
Procedure drawBox(x1,y1,x2,y2,c)
  FrontColor(c)
  LineXY(x1,y1,x2,y1)
  LineXY(x2,y1,x2,y2)
  LineXY(x1,y2,x2,y2)
  LineXY(x1,y1,x1,y2)
EndProcedure

; check if mouse is in range of mouse area object, return '1' if in range
Procedure range(i)
  Protected r.a=0
  If mx>=MA(i)\lx And mx<=MA(i)\rx And my>=MA(i)\ly And my<=MA(i)\ry
    r=1
  EndIf
  ProcedureReturn r
EndProcedure

; drawing brush - generic routine - tesselated patterns
Procedure dBrush(dx,dy,w,d)
  Protected lx,ly,dc,s,p
  
  Select d
    Case 1 ; x2 brush
      dx=((dx-w / 2) / 2)*2
      dy=((dy-w) / 2)*2
      
    Default ; REM standard brush
      dx=dx-w / 2
      dy=dy-w
      
  EndSelect
  
  ; draw pattern loop centered at pixel location dx,dy
  For lx=dx To dx+w
    For ly=dy To dy+w*2
      ; check For airbrush randomization before plotting
      p=1
      If d=2 
        If Random(1000)>5 
          p=0
        EndIf
      EndIf
      
      If p
        ; range check, set pattern colour And plot
        If lx>-1 And lx<dMdx And ly>-1 And ly<dMdy
          dc=curPat(lx % 16,15-(ly % 16))
          
          ; check For transparency
          If dc-dTrn>-1 
            dl(dLay)\SCRN[ly*640+lx]=dc
          EndIf
        EndIf
      EndIf
    Next
  Next
  
EndProcedure

; drawing brush - SPR at any loc
Procedure dBrushSPR(dx,dy,w,d,oP)
  Protected lx,ly,dc,s,p
  
  Select d
    Case 1 ; x2 brush
      dx=((dx-w / 2) / 2)*2
      dy=((dy-w) / 2)*2
      
    Default ; REM standard brush
      dx=dx-w / 2
      dy=dy-w
      
  EndSelect
  
  ; draw pattern loop centered at pixel location dx,dy
  For lx=dx To dx+w
    For ly=dy To dy+w*2
        ; range check, set pattern colour And plot
        If lx>-1 And lx<dMdx And ly>-1 And ly<dMdy
          dc=curPat((lx-dx) % 16,15-((ly-dy) % 16))
          
          ; check For transparency
          If dc-dTrn>-1 
            If op 
              Box(lx*4,511-ly*2,4,2,bp(dc))
            Else
              dl(dLay)\SCRN[ly*640+lx]=dc
            EndIf
          EndIf
        EndIf
    Next
  Next
  
EndProcedure

; Draw box with current brush
Procedure updateBrush()
  Protected px,py,lx,ly,dc,ps,s
  
  ; set skip counter  
  Select dSel
    Case 20 ; standard X2 brush
      s=1
    Default
      s=0
  EndSelect
  
  ; reset current pattern to 0
  For ly=0 To 15
    For lx=0 To 15
      curPat(lx,ly)=0
    Next
  Next
  
  ; clear y pattern counter
  py=0
  
  ; draw pattern loop centered at pixel location dx,dy
  For ly=0 To 15
    ; clear x pattern counter at every new row
    px=0
    For lx=0 To 15
      If pCol<8
        If pat(pSel,(px % 4)+(py % 4)*4)
          dc=pCol
        Else
          If dCol
            dc=dCol              
          Else
            dc=16
          EndIf
        EndIf
      Else
        dc=cusPat(pSel,(px % 8)+(py % 8)*8)
      EndIf
    
      
      curPat(lx,ly)=dc
      
      ; track x pattern data
      px+1
      
      ; adjust x for skip
      lx+s
    Next
    
    ; track y pattern data
    py+1
 
    ; adjust y for skip
    ly+s
  Next  
  
  
  ;draw pattern loop
  For lx=0 To 8
    For ly=0 To 17
      px=lx % 16
      py=ly % 16
      If curPat(px,py)=16 
        dc=8
      Else
        dc=curPat(px,py)
      EndIf
      Box(MA(5)\lx+lx*4+4,MA(5)\ly+ly*2+4,4,2,bp(dc))
    Next
  Next
  
  ;dBrushSize()
EndProcedure

; update palette, assumes startdrawing is already active
Procedure updatePalette()
  Protected i,p,x,dc,p1,p2,i1,i2,xd,yd
  
  dc=bp(1)
  For i=0 To 8          ; colour loop 0 = bottom - 7 = top
    ; preset plot positions
    i1=MA(8)\ly+182-i*22
    i2=MA(8)\ly+190-i*22
    For p=0 To 17       ; pattern number loop left to right
      ; preset plot positions
      p1=MA(8)\lx+4+p*32
      p2=MA(8)\lx+20+p*32
      If i<8            ; branch for custom patterns
        For x=0 To 15   ; pattern element loop, draws four pixels per element
          xd=(x % 4)*4
          yd=(x / 4)*2
          If pat(p,x)=1 ; get current pattern element to determine colour
            dc=bp(i)
          Else
            If i=0 And dCol=0
              dc=bp(8)
            Else
              dc=bp(dCol)
            EndIf
          EndIf
          ; plot 4 pixels for this element offset 32 * 16 (+4 gap vertically)
          Box(p1+xd,i1+yd,4,2,dc)
          Box(p1+xd,i2+yd,4,2,dc)
          Box(p2+xd,i1+yd,4,2,dc)
          Box(p2+xd,i2+yd,4,2,dc)
        Next
      Else
        ; custom pattern loop
        For x=0 To 63
          Box(p1+(x % 8)*4,i1+(x / 8)*2,4,2,bp(cusPat(p,x)))
        Next
      EndIf
    Next      
  Next
  
  updateBrush()
  
  ; highlight selected pattern and update selected brush size pattern
  Box(MA(8)\lx+pSel*32+4,MA(8)\ry-11-pCol*22+4,32,2,bp(7))
  
EndProcedure

; draw vertical line with current pattern 1 pixel wide
Procedure Vline2(x1,y1,y2)
  Protected ly,dc,x1M
  
  ; validate x is in range before starting
  If x1>-1 And x1<dMdx
    x1M=x1 % 16
    If y1>y2:Swap y1,y2:EndIf
    For ly=y1 To y2
      ; range check, set pattern colour And plot
      If ly>-1 And ly<dMdy
        dc=curPat(x1M,ly % 16)        
        
        ; Check For transparency
        If dc-dTrn>-1
          dl(dLay)\SCRN[ly*640+x1]=dc
        EndIf
      EndIf
    Next
  EndIf
EndProcedure

; draw horizontal line with current pattern 1 pixel high
Procedure Hline2(x1,x2,y1)
  Protected lx,dc,y1M
  
  ; validate y is in range before starting
  If y1>-1 And y1<dMdy
    y1M=y1 % 16
    For lx=x1 To x2
      ; range check, set pattern colour And plot
      If lx>-1 And lx<dMdx
        dc=curPat(lx % 16,y1m) 
        
        ; Check For transparency
        If dc-dTrn>-1 
          dl(dLay)\SCRN[y1*640+lx]=dc
        EndIf
      EndIf
    Next
  EndIf

EndProcedure

; line drawing brush
Procedure dLine(x1,y1,x2,y2)
  
  Protected dx,dy,sx,sy
  Protected e2,err,dWh
  
  ; get pixel coords For line
  x1=px(x1) : y1=py(y1)
  x2=px(x2) : y2=py(y2)
  
  ; determine which vector To use For err
  dx=Abs(x2-x1)
  dy=Abs(y2-y1)
  If x1<x2 : sx=1 : Else : sx=-1 : EndIf
  If y1<y2 : sy=1 : Else : sy=-1 : EndIf
  err=dx-dy
  dWh=dWid/2
  
  ; Draw starting segment
  dBrush(x1,y1,dWid,0)
  
  ; draw line loop
  Repeat
    If x1=x2 And y1=y2 : Break: EndIf
    e2=2 * err
    If e2>-dy
      err-dy
      x1+sx
      Vline2(x1+dWh * sx,y1-dWid,y1+dWid)
    EndIf
    If e2<dx
      err+dx
      y1+sy
      Hline2(x1-dWh,x1+dWh,y1+dWid * sy)
    EndIf
    If x1<-dWid Or x1>dMdx+dWid : Break : EndIf
    If y1<-dWid Or y1>dMdy+dWid : Break : EndIf
    ;PROCreadmouse
  Until 0
  
EndProcedure

; circle drawing brush
Procedure dCircle(x1,y1,r)
  Protected r2,dy,x
  
  r=Abs(r)
  r2=r*r
  
  For x=r To 0 Step -2
    dy=Sqr(r2-x*x)
    Vline2(px(x1-x),py(Int(y1-dy)),py(Int(y1+dy)))
    Vline2(px(x1+x),py(Int(y1-dy)),py(Int(y1+dy)))
  Next
  
EndProcedure  

; circle Outline drawing brush
Procedure dCircOut(x1,y1,r)
  Protected t
  
  r=Abs(r)
  For t=0 To 359
    Select dSel
      Case 19 ; airbrush
        dBrush(px(Int(x1+r*Cos(Radian(t)))),py(Int(y1-r*Sin(Radian(t)))),dWid,2)
      Case 20 ; standard X2 brush
        dBrush(px(Int(x1+r*Cos(Radian(t)))),py(Int(y1-r*Sin(Radian(t)))),dWid,1)
      Default
        dCircle(x1+r*Cos(Radian(t)),y1-r*Sin(Radian(t)),dWid*2)
    EndSelect
    
  Next
EndProcedure

; box draw
Procedure dBox(x1,y1,x2,y2,f)
  x1=px(x1) : y1=py(y1)
  x2=px(x2) : y2=py(y2)
  
  Protected lx
  
  If x1>x2 : Swap x1,x2: EndIf
  If y1>y2 : Swap y1,y2: EndIf
  
  If f
    For lx=x1-(dWid / 2) To x2+(dWid / 2)
      Vline2(lx,y1-dWid,y2+dWid)
    Next
  Else
    For lx=-(dWid / 2) To (dWid / 2)
      Vline2(lx+x1,y1-dWid,y2+dWid)
      Vline2(lx+x2,y1-dWid,y2+dWid)
    Next
    For lx=-dWid To dWid
      Hline2(x1-(dWid / 2),x2+(dWid / 2),lx+y1)
      Hline2(x1-(dWid / 2),x2+(dWid / 2),lx+y2)
    Next
  EndIf
  
EndProcedure

; box draw gradient, dWid=0 horizontal, dWid=1 vertical
Procedure dBoxG(x1,y1,x2,y2,d)
  x1=px(x1) : y1=py(y1)
  x2=px(x2) : y2=py(y2)
  
  Protected lx,ly,dc,gR.f,gRd.f,gAdd.f,dv
  
  ; Calculate direction vector, gradient value And gradient Default values
  dv=1 : gR=0 : gRd=0
  If x1>x2
    Swap x1,x2
    If d=0 : dv=-1 : EndIf
  EndIf
  If y1>y2
    ;PROCupdateStatus("y1:"+STR$y1+"  y2:"+STR$y2)
    Swap y1,y2
    If d=1 : dv=-1 : EndIf
  EndIf
  If dv=-1 : gR=17.9: gRd=17.9 : EndIf
  
  If d=1
    gAdd=18/(y2-y1)*dv
  Else
    gAdd=18/(x2-x1)*dv
  EndIf
  For lx=x1 To x2
    If d : gR=gRd : EndIf
    For ly=y1 To y2
      ; range check, set pattern colour And plot
      If lx>-1 And lx<160 And ly>-1 And ly<256 
        pS=lx % 4+(ly % 4)*4
        If pat(Int(gR),pS)
          dc=pCol % 8
        Else
          If dCol
            dc=dCol              
          Else
            dc=16
          EndIf
        EndIf
        
        ; Check For transparency
        If dc-dTrn>-1
          dl(dLay)\SCRN[ly*640+lx]=dc
        EndIf
      EndIf
      If d=1
        gR+gAdd
        If gR>17.9 : gR=17.9 : EndIf
        If gR<0 : gR=0 : EndIf
      EndIf
      
    Next
    If d=0 
      gR+gAdd
      If gR>17.9 : gR=17.8 : EndIf
      If gR<0 : gR=0 : EndIf
    EndIf
    
    
  Next
  
  
  
EndProcedure

; draw highlight box for colour selector
Procedure drawColSel(oldDC,newDC)
  drawBox(MA(1)\lx+2+oldDC*32,MA(1)\ly+3,MA(1)\lx+33+oldDC*32,MA(1)\ly+34,bp(0))
  dCol=newDC
  drawBox(MA(1)\lx+2+dCol*32,MA(1)\ly+3,MA(1)\lx+33+dCol*32,MA(1)\ly+34,bp(7))
EndProcedure

; update colour select value
Procedure updateColSel(c.b)
  Protected oldC
  
  If StartDrawing(CanvasOutput(0))    
    oldC=dCol
    dCol-c
    If dcol=255:dCol=7:EndIf
    If dcol=8:dCol=0:EndIf
    If mact<>9  
      drawColSel(oldC,dCol)
      updateBrush()
    Else
      updatePalette()
    EndIf

    StopDrawing()
  EndIf
  
EndProcedure

; redraw colour select
Procedure resetColSel()
  Box(MA(1)\lx,MA(1)\ly,MA(1)\rx-MA(1)\lx,MA(1)\ry-MA(1)\ly,bp(0))
  For i=1 To 7
    Box(MA(1)\lx+4+i*32,MA(1)\ly+5,28,28,bp(i))
  Next  
  
  drawBox(MA(1)\lx,MA(1)\ly,MA(1)\rx,MA(1)\ry,bp(7))
  drawBox(MA(1)\lx+1,MA(1)\ly+1,MA(1)\rx-1,MA(1)\ry-1,bp(8))
  
  drawColSel(dCol,dCol)
EndProcedure

; update pattern colour 
Procedure updatepCol(c.b)
  Protected oldP
  oldP=pCol
  
  If StartDrawing(CanvasOutput(0))              
    pCol+c
    If pCol=8:pCol=0:EndIf
    If pCol=255:pCol=7:EndIf
    
    If mact=9
      ;Box(MA(8)\lx+pSel*32+4,MA(8)\ly+174-oldP*22+4,32,2,bp(0))
      Box(MA(8)\lx+pSel*32+4,MA(8)\ry-11-oldP*22+4,32,2,bp(0))
      ;Box(MA(8)\lx+pSel*32+4,MA(8)\ly+174-pCol*22+4,32,2,bp(7))
      Box(MA(8)\lx+pSel*32+4,MA(8)\ry-11-pCol*22+4,32,2,bp(7))
    EndIf
    
    updateBrush()
    StopDrawing()
  EndIf  
EndProcedure  

; update layer selector
Procedure updateLayers()
  Protected t.s, h.a
  
  Box(MA(9)\lx,MA(9)\ly+20,MA(9)\rx-MA(9)\lx,MA(9)\ry-MA(9)\ly-20,bp(0))
  
  For i=0 To ArraySize(dl())+2
    drawbox(MA(9)\lx+20,MA(9)\ly+20+i*32,MA(9)\lx+44,MA(9)\ly+44+i*32,bp(7))
    
    If i=ArraySize(dl())+1
      t="A"
      h=dAll
    ElseIf i=ArraySize(dl())+2
      t="G"
      h=dGRD
    Else      
      h=dl(i)\VIS
      t=Str(i+1)
    EndIf
    
    If i=dLay
      Box(MA(9)\lx,MA(9)\ly+20+i*32,16,24,bp(1))
    EndIf
    
    If h
      Box(MA(9)\lx+23,MA(9)\ly+23+i*32,19,19,bp(2))
    EndIf
    
    DrawingMode(#PB_2DDrawing_Transparent)
    DrawText(MA(9)\lx+4,MA(9)\ly+24+i*32,t,bp(7))
    DrawingMode(#PB_2DDrawing_Default)
  Next
  
EndProcedure

; flood fill With current pattern
Procedure floodFill(sx,sy)
  
  sx=px(sx)
  sy=py(sy)
  
  If (sx)>-1 And (sx)<dMdx And (sy)>-1 And (sy)<dMdy
    
    Protected uf,df,c,x,y,mc,dc,i,fp
    
    uf=0
    df=0
    dc=15
    mc=dFil
    
    
    ; first iteration fills With mask colour (15) To replace fill colour
    ; second iteration replaces mask colour with current pattern
    For i=0 To 1
      ; store start x,y on stack
      AddElement(lFS())
      lFS()\x=sx : lFS()\y=sy
      
      ; loop until no fill elements in fill stack (list)
      Repeat
        ; get Next fill point from fill List
        x=lFS()\x : y=lFS()\y
        DeleteElement(lFS())
        
        If dl(dLay)\SCRN[y*640+x]=mc ;Point(x*dMpx,y*dMpy)=mc
          uf=1 : df=1
          
          ; scan left
          While x>0 And dl(dLay)\SCRN[y*640+x-1]=mc ;Point((x-1)*4,y*2)=mc
            x-1
          Wend
          
          ; scan right and plot fill colour
          While x<dMdx And dl(dLay)\SCRN[y*640+x]=mc
            If i
              dc=curPat(x % 16,15-(y % 16))
;               pS=x % 4+(y % 4)*4
;               If pat(pSel,pS)
;                 dc=pCol
;               Else
;                 dc=dCol
;               EndIf
              
            EndIf
            
            dl(dLay)\SCRN[y*640+x]=dc ;Box(x*4,y*2,4,2,bp(dc))
            
            ; detect colour changes above and add To List
            If y<(dMdy-1)
              c=dl(dLay)\SCRN[(y+1)*640+x]
              If uf And c=mc
                AddElement(lFS())
                lFS()\x=x : lFS()\y=y+1
                uf=0
              EndIf
              If c<>mc : uf=1 : EndIf
            EndIf
            
            ; detect colour changes below and add To List
            If y>0
              c=dl(dLay)\SCRN[(y-1)*640+x]
              If df And c=mc 
                AddElement(lFS())
                lFS()\x=x : lFS()\y=y-1
                df=0
              EndIf
              If c<>mc : df=1 : EndIf
            EndIf
            x+1
          Wend
        EndIf
        
      Until ListSize(lFS())=0
      
      mc=15
    Next
  EndIf
  
EndProcedure

; toggle button status, assumes startdrawing is already active
; button must be completely on visible window
; b: button number
; c: highlight colour
Procedure toolToggle(i,c)
  Protected x,y,lx,ly
  
  ; default or erase button colour is 7
  If c=0: c=7: EndIf
  x=MA(6)\lx+(i % 2)*50+4
  y=MA(6)\ly+(i/2)*50+4
  
  ; scan icon and update non black areas to new colour
  If x>-1 And (x+41)<#scrW And y>-1 And (y+41)<#scrH
    For lx=x To x+41
      For ly=y To y+41
        If Point(lx,ly)<>bp(0): Plot(lx,ly,bp(c)): EndIf
      Next
    Next
  EndIf
  
  ; drawbox(x,y,x+41,y+41,bp(2))
  
EndProcedure

; add a tool toggle item to the tool toggle draw list
; all items in list will be drawn at end of main loop
Procedure addToggle(b,c)
  AddElement(lToggle())
  ltoggle()\b=b
  ltoggle()\c=c
EndProcedure

; open save handler
Procedure openSave(mode)
  
  Protected lastFN.s, filename.s, action.s, F.s, ff.s, N.w, iTMP, a.b,b.a
  
  Select mode
    Case 0 ; get filename to load
      ff = "PNG file - Load Current Layer (*.PNG)|*.PNG|BBC file - Load Current Layer (*.*)|*.*"
      filename=OpenFileRequester(" Load Image",GetCurrentDirectory(),ff,loadPattern)
      loadPattern=SelectedFilePattern()
      
    Case 1 ; save dialog
      ff = "PNG file - Save Current Layer (*.PNG)|*.PNG|PNG file - Save All Layers (*.PNG)|*.PNG|BBC file - Save Current Layer (*.*)|*.*|BBC file - Save All Layers (*.*)|*.*"
      If tQSv=0
        Repeat
          ok=#PB_MessageRequester_Yes
          
          ; show save dialog and prompt if file exists
          filename=SaveFileRequester(" Save Image",GetCurrentDirectory(),ff,savePattern)
          savePattern=SelectedFilePattern()
          If filename
            Select savePattern
              Case 0,1 ; png
                If Right(UCase(filename),4)<>".PNG"
                  filename=filename+".PNG"
                EndIf
                
              Case 2,3 ; bbc raw
                If GetExtensionPart(UCase(filename))<>""
                  MessageRequester(" File Name Error","ERROR: Filename must not contain an extension.",#PB_MessageRequester_Error)
                  ok=#PB_MessageRequester_No
                EndIf
                
            EndSelect
            
            ;MessageRequester("Validate",filename)
            
            If FileSize(filename)>-1
              ok=MessageRequester(" Confirm File Overwrite",GetFilePart(filename)+" already exists. Do you want to replace it?",#PB_MessageRequester_YesNo|#PB_MessageRequester_Warning)
            EndIf
          EndIf
        Until ok=#PB_MessageRequester_Yes
      Else
        ; Quick save file finder
        lastFN=GetCurrentDirectory()+"ART_QS_"
        
        N=0
        F=""
        Repeat
          F=lastFN+Right("0000"+StrU(N),5)+".PNG"
          If FileSize(F)<0
            filename=F
          Else
            N+1
          EndIf
        Until filename<>"" Or N>99999
        If N>99999 
          MessageRequester(" File Error","ERROR: Cannot quicksave! File number limit reached, archive some files!!!",#PB_MessageRequester_Error)
        EndIf
      EndIf
      
  EndSelect
  
  ; action file operation
  If filename=""
    filename= "File Not Selected!"
  Else
    Select mode 
      Case 0 ; load image and copy to drawing area
        action="opened"
        Select loadPattern
          Case 0 ; load png file
            
            iTMP=LoadImage(#PB_Any,filename)
            If iTMP
              ; convert imported image to beeb format and dump into draw buffer
              If StartDrawing(ImageOutput(iTMP))
                Buffer      = DrawingBuffer()             ; Get the start address of the screen buffer
                Pitch       = DrawingBufferPitch()        ; Get the length (in byte) took by one horizontal line
                PixelFormat = DrawingBufferPixelFormat()  ; Get the pixel format.           
                For y = 0 To 511 
                  *Line.Pixel = Buffer+Pitch*y
                  yMul=(y/2)*640
                  
                  ; scan loaded png file image and write pixel data to image buffer for rgb colours that match beeb palette
                  ; attemtpts to preserve colour 0,0,0,0 as transparent black
                  For x=0 To 159
                    For i=0 To 7
                      If *Line\Pixel=RGB(rgbT(i)\b,rgbT(i)\g,rgbT(i)\r) Or *Line\Pixel=RGBA(rgbT(i)\b,rgbT(i)\g,rgbT(i)\r,255)
                        dl(dlay)\SCRN[x+yMul]=i
                        Break
                      EndIf
                    Next
                    If *Line\Pixel=RGBA(0,0,0,255)
                      dl(dlay)\SCRN[x+yMul]=16
                    EndIf
                    *line+16  
                  Next
                Next
                
                StopDrawing()
                FreeImage(iTMP)            
              EndIf          
            Else
              MessageRequester(" File Error","ERROR: Cannot load file..." + #CRLF$ + #CRLF$ + filename,#PB_MessageRequester_Error)
            EndIf
          Case 1 ; load bbc raw format
            
            ;check file size
            If FileSize(filename)=#RAWsize
              ; create temp buffer and fill with file data
              *MemoryID = AllocateMemory(#RAWsize)       ; allocate 20k memory block
              If *MemoryID
                
                ; slurp in file data to buffer
                If ReadFile(0, filename)
                  ReadData(0,*MemoryID,#RAWsize)
                  CloseFile(0)
                  
                  x=0
                  y=255
                  
                  ; scan raw data in buffer and extract pixel data
                  For i=0 To #RAWsize-1
                    
                    ; 2 pixels (mode 2) per byte, pixel 1 mask: 10101010  pixel 2 mask: 01010101
                    a=(PeekB(*MemoryID+i) & 170)>>1
                    b=PeekB(*MemoryID+i) & 85
                    
                    ; convert raw data to pixel colour data via reverse lookup table
                    dl(dlay)\SCRN[x+y*640]=revBBC(a)
                    dl(dlay)\SCRN[x+1+y*640]=revBBC(b)
                    
                    ; step through raw data in mode 2 pixel order
                    ; currently starts at lower right corner and steps left char by char, each char is eight bytes high
                    ; after 80 chars wide (640 bytes) left side is reached, we now return to right side and continue at next char row up
                    y-1
                    If ((y+1)%8)=0
                      y+8
                      x+2
                      If x=160
                        y-8
                        x=0
                      EndIf
                    EndIf
                  Next
                  
                Else
                  MessageRequester(" File Error","ERROR: Cannot load file..." + #CRLF$ + #CRLF$ + filename,#PB_MessageRequester_Error)
                EndIf
                FreeMemory(*MemoryID)
              EndIf
            Else
              MessageRequester(" File Error","ERROR: File must be exactly "+Str(#RAWsize)+" bytes..." + #CRLF$ + #CRLF$ + filename,#PB_MessageRequester_Error)  
            EndIf
            
        EndSelect
        
        ;filename$+""" "+STR$(M{(0)}.mx%)+","+STR$(M{(0)}.my%)+"
      Case 1 ; save drawing image to file
             ; erase output buffer
        For x=0 To #SCRNsize
          SCRNout(x)=0 ; clear output buffer
        Next
        
        ; copy relevant layer details to output buffer
        Select savePattern
          Case 0,2 ; save current layer
            For x=0 To #SCRNsize
              If dl(dLay)\SCRN[x]
                If savePattern=2
                  SCRNout(x)=dl(dLay)\SCRN[x] % 16 ; convert colour 16 (fake black) to true black
                Else
                  SCRNout(x)=dl(dLay)\SCRN[x] ; colour 16 = true black
                EndIf
              EndIf
            Next            
            
          Case 1,3 ; save all layers -  need to confirm if save all layers or save visible layers
            For i=0 To ArraySize(dl())
              ;If dl(i)\VIS
              For x=0 To #SCRNsize
                If dl(i)\SCRN[x]
                  If savePattern=3
                    SCRNout(x)=dl(i)\SCRN[x] % 16 ; convert colour 16 (fake black) to true black
                  Else
                    SCRNout(x)=dl(i)\SCRN[x] ; colour 16 = true black
                  EndIf
                EndIf
              Next            
              ;EndIf
            Next
        EndSelect
        
        
        
        Select savePattern
          Case 0,1 ; save png file
            action$="saved"
            
            ; copy output buffer to final image
            If StartDrawing(ImageOutput(imgFinal))
              Box(0,0,640,512,bp(0))          
              Buffer      = DrawingBuffer()             ; Get the start address of the screen buffer
              Pitch       = DrawingBufferPitch()        ; Get the length (in byte) took by one horizontal line
              PixelFormat = DrawingBufferPixelFormat()  ; Get the pixel format. 
              
              ; configure palette for RGB or BGR
              If PixelFormat = #PB_PixelFormat_32Bits_RGB
                For i=1 To 15
                  ct(i)=RGBA(rgbT(i)\r,rgbT(i)\g,rgbT(i)\b,255)
                Next
              Else ; Else it's 32bits_BGR
                For i=1 To 15
                  ct(i)=RGBA(rgbT(i)\b,rgbT(i)\g,rgbT(i)\r,255)
                Next
              EndIf
              ct(0)=RGBA(0,0,0,0) ; transparent black
              ct(16)=RGBA(0,0,0,255) ; true black
              
              For y = 0 To 511 
                *Line.Pixel = Buffer+Pitch*y
                yMul=(y/2)*640
                
                For x=0 To 159
                  dc = ct(SCRNout(x+yMul))
                  
                  *Line\Pixel = dc ; Write the pixel directly to the memory !
                  *line+4
                  *Line\Pixel = dc ; Write the pixel directly to the memory !
                  *Line+4
                  *Line\Pixel = dc ; Write the pixel directly to the memory !
                  *Line+4
                  *Line\Pixel = dc ; Write the pixel directly to the memory !
                  *Line+4
                  
                Next
              Next
              StopDrawing()
              
              SaveImage(imgFinal, filename, #PB_ImagePlugin_PNG);,10,4)
            EndIf
            
          Case 2,3 ; save bbc raw
                   ; create temp buffer and fill with screen data
            *MemoryID = AllocateMemory(#RAWsize)       ; allocate 20k memory block
            If *MemoryID
              x=0
              y=255
              For i=0 To #RAWsize-1
                a=rawBBC(SCRNout(x+y*640))<<1
                b=rawBBC(SCRNout(x+1+y*640))
                ; MessageRequester("TEST",Str(a)+"  "+Str(b))
                PokeB(*MemoryID+i, (a | b)) 
                y-1
                If ((y+1)%8)=0
                  y+8
                  x+2
                  If x=160
                    y-8
                    x=0
                  EndIf
                EndIf
              Next
              
              ; create file and output temp buffer
              filename=UCase(filename)
              If CreateFile(0, filename)
                WriteData(0, *MemoryID, #RAWsize)
                CloseFile(0)
                
                ; create INF file
                If CreateFile(1, filename+".INF")
                  WriteString(1, "$."+GetFilePart(filename)+" 003000 000000 005000")
                  
                  CloseFile(1)
                Else
                  MessageRequester (" File Error","Cannot create INF file..."+#CRLF$+#CRLF$+filename+".INF",#PB_MessageRequester_Error)
                EndIf
                
              Else
                MessageRequester (" File Error","Cannot create file..."+#CRLF$+#CRLF$+filename,#PB_MessageRequester_Error)
              EndIf
              FreeMemory(*MemoryID)
            EndIf
            
            
        EndSelect
        
    EndSelect
  EndIf
  
  ;PROCupdateStatus(MID$(filename$,FNINSTRREV(filename$,"\")+1) + " " + action$)
EndProcedure


; save undo
Procedure saveUndo()
  ; remove first item in list if undo buffer full
  If ListSize(dl(dLay)\lUndo())>#maxUndo
    FirstElement(dl(dLay)\lUndo())
    DeleteElement(dl(dLay)\lUndo())
    LastElement(dl(dLay)\lUndo())
  EndIf
  ; add a new backup buffer to undo buffer and copy screen data to it
  AddElement(dl(dLay)\lUndo())
  For x=0 To #SCRNsize
    dl(dLay)\lUndo()\buf[x]=dl(dLay)\SCRN[x]
  Next
  
  ; update button toggle if needed
  If ListSize(dl(dLay)\lUndo())=1
    addToggle(2,7)
  EndIf
  
EndProcedure

; save redo
Procedure saveRedo()
  ; add a new backup image to redo buffer
  AddElement(dl(dLay)\lRedo())
  For x=0 To #SCRNsize
    dl(dLay)\lRedo()\buf[x]=dl(dLay)\SCRN[x]
  Next
  
  ; update button toggle if needed
  If ListSize(dl(dLay)\lRedo())=1
    addToggle(3,7)
  EndIf
EndProcedure

; restore undo image
Procedure Undo()
  If ListSize(dl(dLay)\lUndo())
    ; create redo image first
    saveRedo()
    ; update drawing array with undo data
    For x=0 To #SCRNsize
      dl(dLay)\SCRN[x]=dl(dLay)\lUndo()\buf[x]
    Next
    
    ; discard undo array from list
    DeleteElement(dl(dLay)\lUndo())
    
    ; update button toggle if needed
    If ListSize(dl(dLay)\lUndo())=0
      addToggle(2,8)
    EndIf
  EndIf
EndProcedure

; restore redo image
Procedure Redo()
  If ListSize(dl(dLay)\lRedo())
    ; create undo image first
    saveUndo()
    
    ; update drawing array with redo data
    For x=0 To #SCRNsize
      dl(dLay)\SCRN[x]=dl(dLay)\lRedo()\buf[x]
    Next
    
    ; discard redo array from list
    DeleteElement(dl(dLay)\lRedo())
    
    ; update button toggle if needed
    If ListSize(dl(dLay)\lRedo())=0
      addToggle(3,8)
    EndIf
    
  EndIf
EndProcedure


; --- End Procedures ---


; initialization block

;-------- Init and Load --------

If InitMouse() = 0 
  Exit_ART("Cannot init Mouse subsystem!")
EndIf

If InitKeyboard() = 0 
  Exit_ART("Cannot init Keyboard subsystem!")
EndIf

If OpenWindow(0,0,0,#scrW, #scrH, "ART for EXA Gear v0.1",#PB_Window_SystemMenu | #PB_Window_ScreenCentered) = 0 
  Exit_ART("Cannot init Graphics subsystem! ("+StrU(#scrW,#PB_Long)+"x"+StrU(#scrh,#PB_Long)+")") ;#scrW #scrh
EndIf

; read pattern data
Restore patternData
For i=0 To 17
  For h=0 To 15
    Read.a pat(i,h)
  Next
Next

; read custom pattern data
Restore customPatternData
For i=0 To 6
For x=0 To 63
  Read.a cusPat(i,x)
Next
Next

; define beeb 2 palette - includes colour 16 which is used for solid black, gets converted to transparent black on image save
Restore paletteData
For i=0 To 16
  Read.a rgbT(i)\r
  Read.a rgbT(i)\g
  Read.a rgbT(i)\b
  bp(i)=RGB(rgbT(i)\r,rgbT(i)\g,rgbT(i)\b)
Next

; mouse area / tool location list
Restore mouseData
For i=0 To maCount
  Read.s MA(i)\name
  Read.w MA(i)\lx
  Read.w MA(i)\ly
  Read.w x
  Read.w y
  MA(i)\rx=MA(i)\lx+x-1
  MA(i)\ry=MA(i)\ly+y-1
Next

; raw data format for file save and open
Restore rawFileData
For i=0 To 15
  Read.a rawBBC(i)
  revBBC(rawBBC(i))=i
Next  

; toolstrip image
imgToolStrip=CatchImage(#PB_Any,?ToolStripMain)
imgToolStrip2=CatchImage(#PB_Any,?ToolStripMain2)

; create drawing layers
For i=0 To ArraySize(dl())
  dl(i)\VIS=1
Next
imgFinal=CreateImage(#PB_Any,640,512,32)
imgGRD=CreateImage(#PB_Any,640,512,32)

; init screen gadget and initial state
CanvasGadget(0,0,0,#scrW,#scrH)
SetGadgetAttribute(0, #PB_Canvas_Cursor , #PB_Cursor_Cross)
mact=-1

tFont=LoadFont(#PB_Any,"Comic Sans MS",14,#PB_Font_Italic |#PB_Font_Bold)

;
;-------- Draw controls --------
;

; create gridded transparent layer
If StartDrawing(ImageOutput(imgGRD))
  For x=0 To 79
    For y=0 To 63
      c=255-(~x!y&1)*100
      Box(x*8,y*8,8,8,RGB(c,c,c))
    Next
  Next
  StopDrawing()
EndIf
          

If StartDrawing(CanvasOutput(0))
  ; clear canvas to black
  Box(0,0,#scrW,#scrH,bp(0))
  
  ; main canvas double border
  drawBox(MA(0)\lx-4,MA(0)\ly-4,MA(0)\rx+4,MA(0)\ry+4,bp(7))
  drawBox(MA(0)\lx-3,MA(0)\ly-3,MA(0)\rx+3,MA(0)\ry+3,bp(7))
  
  ; pattern select border
  DrawingFont(FontID(tFont))
  DrawText(MA(6)\lx+4,0,"EXA ART",bp(1))
  DrawingFont(#PB_Default)
  
  ; draw tools strip and toggle defaults
  DrawImage(ImageID(imgToolStrip),MA(6)\lx,MA(6)\ly)
  DrawImage(ImageID(imgToolStrip2),0,524)
  toolToggle(2,8) ; undo
  toolToggle(3,8) ; redo
  toolToggle(tCur,tTog) ; standard
  toolToggle(14,2)      ; transparency
  toolToggle(dSel,6)    ; draw style  
  
  ; initialise drawing area with layer 0
  DrawImage(ImageID(imgFinal),MA(0)\lx,MA(0)\ly)
  
  ; brush size control
  Circle(MA(3)\lx+33,MA(3)\ly+118,16,bp(6))
  Circle(MA(3)\lx+33,MA(3)\ly+84,12,bp(6))
  Circle(MA(3)\lx+33,MA(3)\ly+55,10,bp(6))
  Circle(MA(3)\lx+33,MA(3)\ly+33,7,bp(6))
  Circle(MA(3)\lx+33,MA(3)\ly+13,5,bp(6))
  Circle(MA(3)\lx+6,MA(3)\ly+dWid*4+6,4,bp(7))
  
  DrawText(MA(3)\lx+4,MA(3)\ly-36,"Brush",bp(7))
  DrawText(MA(3)\lx+8,MA(3)\ly-20,"Size",bp(7))
  
  updateBrush()
  
  ; draw colour select boxes and double border
  resetColSel()
  
  ; stats area
  DrawText(MA(4)\lx,MA(4)\ly,"mX:")
  DrawText(MA(4)\lx,MA(4)\ly+16,"mY:")
  DrawText(MA(4)\lx+80,MA(4)\ly,"pX:")
  DrawText(MA(4)\lx+80,MA(4)\ly+16,"pY:")
  DrawText(MA(4)\lx+160,MA(4)\ly,"dW:")
  DrawText(MA(4)\lx+160,MA(4)\ly+16,"mA:")
  DrawText(MA(4)\lx+240,MA(4)\ly,"CT:")
  DrawText(MA(4)\lx+240,MA(4)\ly+16,"TS:")
  
  DrawText(MA(5)\lx,MA(5)\ly-20,"Pat",bp(7))
  drawBox(MA(5)\lx,MA(5)\ly,MA(5)\rx,MA(5)\ry,bp(7))
  
  DrawText(MA(7)\lx,MA(7)\ly-20,"Fill",bp(7))
  drawBox(MA(7)\lx,MA(7)\ly,MA(7)\rx,MA(7)\ry,bp(7))
  
  DrawText(MA(9)\lx+4,MA(9)\ly,"Layer",bp(7))
  
  updateLayers()
  
  ; enable for debugging mouse area squares
;   For i=0 To maCount
;     drawBox(MA(i)\lx,MA(i)\ly,MA(i)\rx,MA(i)\ry,bp(i))
;   Next  
  
  StopDrawing()
EndIf

;
;-------- MainLoop --------
;

Repeat
  Event = WaitWindowEvent()
  
  If event
    
    If Event = #PB_Event_Gadget And EventGadget() = 0 
      ; read mouse
      mx = GetGadgetAttribute(0, #PB_Canvas_MouseX)
      my = GetGadgetAttribute(0, #PB_Canvas_MouseY)
      
      ; check if mouse is in pattern area
      If EventType() = #PB_EventType_MouseMove
        x=GetGadgetAttribute(0, #PB_Canvas_Cursor)
        If range(0) And mact<>9
          If x<>#PB_Cursor_Invisible
            SetGadgetAttribute(0, #PB_Canvas_Cursor , #PB_Cursor_Invisible)
          EndIf
        Else
          If x<>#PB_Cursor_Cross
            SetGadgetAttribute(0, #PB_Canvas_Cursor , #PB_Cursor_Cross)
          EndIf
        EndIf
        
        If mact=-1
          If range(2)=1
            mact=9
            imgIN=GetGadgetAttribute(0,#PB_Canvas_Image)
            imgTMP=CreateImage(#PB_Any,#scrW,#scrH)
            If StartDrawing(ImageOutput(imgTMP))
              DrawImage(imgIN,0,0)
              StopDrawing()
            EndIf
            imgPAL=GrabImage(imgTMP,#PB_Any,MA(8)\lx,MA(8)\ly,MA(8)\rx-MA(8)\lx,MA(8)\ry-MA(8)\ly)
            FreeImage(imgTMP)
            If StartDrawing(CanvasOutput(0))
              Box(MA(8)\lx,MA(8)\ly,MA(8)\rx-MA(8)\lx-1,MA(8)\ry-MA(8)\ly-1,bp(0))
              drawBox(MA(8)\lx+1,MA(8)\ly+1,MA(8)\rx-3,MA(8)\ry-3,bp(7))
              updatePalette()
              StopDrawing()
            EndIf
          EndIf
        EndIf
      EndIf
      
      ; handle menu hide if mouse up or out of range
      If mact=9 
        If EventType()=#PB_EventType_LeftButtonUp Or (range(8)=0 And (GetGadgetAttribute(0, #PB_Canvas_Buttons)=0))
          mact=-1
          If StartDrawing(CanvasOutput(0))
            DrawImage(ImageID(imgPAL),MA(8)\lx,MA(8)\ly)
            
            ; update colour select panel
            resetColSel()
            
            StopDrawing()
          EndIf
          FreeImage(imgPAL)
          
        EndIf
      EndIf
      
      ; left mouse button down and mouse move events
      If EventType() = #PB_EventType_LeftButtonDown Or (EventType() = #PB_EventType_MouseMove And GetGadgetAttribute(0, #PB_Canvas_Buttons) & #PB_Canvas_LeftButton)
        
        ; set mouse action if none already set
        If mact=-1 And tLIF=0
          
          ; set default action to none and check mouse area ranges to select current mouse area
          mact=0
          For i=0 To maCount
            If range(i)=1
              mact=i+1
              Break
            EndIf
          Next
          
          Select mact
            Case 1 ; main drawing canvas - save undo
                   ; clear redo when new drawing starts
              If ListSize(dl(dLay)\lRedo())
                ClearList(dl(dLay)\lRedo())
                addToggle(3,8)
              EndIf
              saveUndo()
              sx=mx-MA(0)\lx
              sy=my-MA(0)\ly
              ox=sx
              oy=sy
            Case 9 ; ignore pattern area if not selected via mouse over hotspot
              mact=-1
          EndSelect
        EndIf
        
        ; do any drawing actions for mouse move
        Select mact
          Case 1 ; main drawing canvas
            
            ; determine tool being used
            Select tCur
              Case 4 ; brush tool
                Select dSel
                  Case 16 ; standard brush
                    dbrush(px(mx),py(my),dWid,0)
                  Case 17 ; circle brush
                    dCircle(mx,my,dWid*2)
                  Case 19 ; airbrush
                    dbrush(px(mx),py(my),dWid,2)
                  Case 20 ; standard X2 brush
                    dbrush(px(mx),py(my),dWid,1)
                EndSelect
              Case 5,6,7,8,9,12,13 ; save ox,oy
                ox=mx-MA(0)\lx
                oy=my-MA(0)\ly
                
              Case 10 ; flood fill
                If range(0)
                  i=dl(dLay)\SCRN[px(mx)+640*py(my)];Point(mx,my) get pixel colour under cursor
                  If i<>dFil                        ; continue if fill colour is not the same as last fill
                    dFil=i
                    If StartDrawing(CanvasOutput(0)) ; update fill colour box in tool box
                      Box(MA(7)\lx+4,MA(7)\ly+4,MA(7)\rx-MA(7)\lx-7,MA(7)\ry-MA(7)\ly-7,bp(dFil))
                      StopDrawing()
                    EndIf
                  EndIf
                EndIf
            EndSelect
            dWire=tCur
            ; *** end drawing tool code
            
          Case 2 ; colour select
            i=(mx-MA(1)\lx) / 32
            If dCol<>i And i>-1 And i<8
              If StartDrawing(CanvasOutput(0))    
                drawColSel(dCol,i)
                updateBrush()
                StopDrawing()
              EndIf
            EndIf
          Case 9 ; pattern select2 - old palette select is mact=3
            i=8-((my-MA(8)\ly-4) / 22)
            j=((mx-MA(8)\lx) / 32)
            If i<0: i=0: EndIf
            If i>8: i=8: EndIf
            If j<0: j=0: EndIf
            If j>17: j=17: EndIf
            If pCol<>i Or pSel<>j
              If StartDrawing(CanvasOutput(0))              
                Box(MA(8)\lx+pSel*32+4,MA(8)\ry-11-pCol*22+4,32,2,bp(0))
                pCol=i
                pSel=j
                
                updateBrush()
                ; highlight selected pattern and update selected brush size pattern
                Box(MA(8)\lx+pSel*32+4,MA(8)\ry-11-pCol*22+4,32,2,bp(7))
                
                StopDrawing()
              EndIf
            EndIf
            
          Case 4 ; brush size
            s=(my-MA(3)\ly-4) / 4
            If s<>dWid And s>-1 And s<33
              
              ; update brush size indicator
              If StartDrawing(CanvasOutput(0))
                For i=0 To 1
                  Circle(MA(3)\lx+6,MA(3)\ly+dWid*4+6,4,bp(i*7))
                  ;Circle(MA(3)\lx+60,MA(3)\ly+dWid*4+6,4,bp(i*7))
                  dWid=s
                Next
                ;dBrushSize()
                StopDrawing()
              EndIf
            EndIf
            
        EndSelect
      EndIf
      
      ;
      ;-------- Mouse Up --------
      ;      
      
      ; left button release events
      If EventType()=#PB_EventType_LeftButtonUp
        ; reset load flag on mouse release
        If tLIF
          tLIF=0
        EndIf
        
        ; do any mouse up actions such as tools and pattern select
        Select mact
          Case 1 ; drawing area
            
            ; determine tool being used
            Select tCur
              Case 5 ; line tool completion
                dLine(sx+MA(0)\lx,sy+MA(0)\ly,ox+MA(0)\lx,oy+MA(0)\ly)
                
              Case 6,7 ; polygon completion
                If tCur=6 
                  dCircOut(sx,sy,Abs(sx-ox))
                Else
                  dCircle(sx,sy,Abs(sx-ox))
                EndIf
                
              Case 8,9,12,13  ; boxes, gradient
                
                If tCur>9
                  If sx<>ox And sy<>oy 
                    dBoxG(sx,sy,ox,oy,tCur-12)
                  EndIf
                Else
                  dBox(sx,sy,ox,oy,tCur-8)
                EndIf
              Case 10 ; flood fill
                floodfill(mx-MA(0)\lx,my-MA(0)\ly)
            EndSelect     
            If dsel=18
              dBrushSPR(px(mx),py(my),dWid,0,0)
            EndIf
            
            
          Case 7 ; tool select
            
            ; get button of tool clicked And action
            tSel=(mx-MA(6)\lx) / 50+((my-MA(6)\ly) / 50)*2
            
            If tSel>-1 And tSel<28
              Select tSel
                Case 0 ; save
                  opensave(1)
                Case 1 ; load
                  saveUndo()                  
                  opensave(0)
                  tLIF=1
                Case 2 ; undo
                  undo()
                Case 3 ; redo
                  redo()
                Case 15; CLS
                  saveundo()
                  For x=0 To #SCRNsize
                    dl(dLay)\SCRN[x]=0
                  Next                  
                Case 14; toggle transparency
                  dTrn=(dTrn+1) % 2
                  addToggle(tSel,dTrn*2)
                  
                Case 16,17,18,19,20 ; brush style: 16 Square, 17 Circle, 18 Slash, 19 Spray, 20 x2 Brush
                  If tSel<>dSel
                    addToggle(dSel,0)
                    dSel=tSel
                    addToggle(dSel,6)
                    
                    If StartDrawing(CanvasOutput(0))
                      updateBrush()
                      StopDrawing()
                    EndIf
                  EndIf
                  
                Case 21 ; quick save toggle
                  tQSv=(tQSv+1) % 2
                  addToggle(tSel,tQSv*2)
                  
                Default ; all other tools
                  If tSel<>tCur
                    addToggle(tCur,0)
                    tCur=tSel
                    addToggle(tCur,tTog)
                  EndIf
              EndSelect
            EndIf
            
          Case 10 ; layers
            i=(my-MA(9)\ly-20) / 32
            If i<0:i=0:EndIf
            If i>6:i=6:EndIf
            If mx-MA(9)\lx<23 ; select layer
              If i<5 And i<>dLay
                dlay=i
              EndIf
            Else ; select visible layer
              Select i
                Case 0 To 4
                  If dl(i)\VIS
                    dl(i)\VIS=0
                  Else
                    dl(i)\VIS=1
                  EndIf
                Case 5
                  If dAll
                    dAll=0
                  Else
                    dAll=1
                  EndIf
                  For i=0 To ArraySize(dl())
                    dl(i)\VIS=dAll
                  Next
                Case 6
                  If dGRD
                    dGRD=0
                  Else
                    dGRD=1
                  EndIf
              EndSelect
              
            EndIf
            ; update layers view
            If StartDrawing(CanvasOutput(0))
              updateLayers()
              StopDrawing()
            EndIf
            ; update undo and redo
            If ListSize(dl(dLay)\lRedo())=0
              addToggle(3,8)
            Else
              addToggle(3,7)
            EndIf
            If ListSize(dl(dLay)\lUndo())=0
              addToggle(2,8)
            Else
              addToggle(2,7)
            EndIf
            
        EndSelect
        
        ; reset wire frame
        dWire=0 
        
        ; reset mouse action
        mact=-1
        
      EndIf 
      
      ;-------- Update Screen
      If mact<>9
        
        ; clear output buffer
        For x=0 To #SCRNsize
          SCRNout(x)=0 
        Next
        
        ; copy visible screen buffer to output buffer
        For i=0 To ArraySize(dl())
          If dl(i)\VIS
            For x=0 To #SCRNsize
              If dl(i)\SCRN[x]
                SCRNout(x)=dl(i)\SCRN[x]
              EndIf
            Next            
          EndIf
        Next
        
        ; copy output buffer to final image
        If StartDrawing(ImageOutput(imgFinal))
          If dGRD
            DrawImage(ImageID(imgGRD),0,0)
          Else
            Box(0,0,640,512,bp(0))
          EndIf
          
          
          Buffer      = DrawingBuffer()             ; Get the start address of the screen buffer
          Pitch       = DrawingBufferPitch()        ; Get the length (in byte) took by one horizontal line
          PixelFormat = DrawingBufferPixelFormat()  ; Get the pixel format. 
          
          ; configure palette for RGB or BGR
          If PixelFormat = #PB_PixelFormat_32Bits_RGB
            For i=1 To 16
              ct(i)=bp(i)
            Next
          Else ; Else it's 32bits_BGR
            For i=1 To 16
              ct(i)=RGBA(rgbT(i)\b,rgbT(i)\g,rgbT(i)\r,255)
            Next
          EndIf
          ct(0)=RGBA(0,0,0,0)
          
          For y = 0 To 511 
            *Line.Pixel = Buffer+Pitch*y
            yMul=(y/2)*640
            
            For x=0 To 159
              dc = ct(SCRNout(x+yMul))
              If dc
                *Line\Pixel = dc ; Write the pixel directly to the memory !
                *line+4
                *Line\Pixel = dc ; Write the pixel directly to the memory !
                *Line+4
                *Line\Pixel = dc ; Write the pixel directly to the memory !
                *Line+4
                *Line\Pixel = dc ; Write the pixel directly to the memory !
                *Line+4
              Else
                *Line+16
              EndIf
            Next
          Next
          
          DrawingMode(#PB_2DDrawing_Outlined|#PB_2DDrawing_XOr)
          
          ; big cross hair
          If range(0)
            LineXY(mx-4,MA(0)\ly-4,mx-4,MA(0)\ry,RGB(63,63,63))
            LineXY(MA(0)\lx-4,my-4,MA(0)\rx,my-4,RGB(63,63,63))
          EndIf
          
          ; drawsize guide
          x=(mx-MA(0)\lx) / dMpx-dWid / 2
          y=((my-MA(0)\ly) / dMpy)-dWid
          x2=(x+dWid)*4
          y2=(y+dWid*2)*2
          x*4
          y*2
          
          ; draw brush size guide or small circle
          Select tCur
            Case 4,5 ; brush and line draw
              If dwid>3
                
                LineXY(x,y,x+8,y,bp(2));RGB(63,63,63))
                LineXY(x,y,x,y+8,bp(2));,RGB(63,63,63))
                
                LineXY(x2-4,y,x2+3,y,bp(2));RGB(63,63,63))
                LineXY(x2+3,y,x2+3,y+8,bp(2));,RGB(63,63,63))
                
                LineXY(x,y2+1,x+8,y2+1,bp(2));RGB(63,63,63))
                LineXY(x,y2-8,x,y2+1,bp(2))  ;,RGB(63,63,63))
                
                LineXY(x2-4,y2+1,x2+3,y2+1,bp(2));RGB(63,63,63))
                LineXY(x2+3,y2-8,x2+3,y2+1,bp(2));,RGB(63,63,63))
              Else
                Circle(x+5,y+5,10,bp(2))
              EndIf
          EndSelect
          
          ; draw shape guides
          Select dWire
            Case 5 ; line tool
              LineXY(sx,sy,mx-MA(0)\lx,my-MA(0)\ly,bp(7))
              
            Case 6,7 ; polygon tool
              Circle(sx,sy,Abs(sx-(mx-MA(0)\lx)),bp(7))
            Case 8,9,12,13  ; boxes, gradient
              Box(sx,sy,mx-MA(0)\lx-sx,my-MA(0)\ly-sy,bp(7))
              
          EndSelect
          
          ; draw sprite
          If dSel=18
              dBrushSPR(px(mx),py(my),dWid,0,1)
          EndIf
          
          StopDrawing()
        EndIf
        
        ; update drawing canvas
        If StartDrawing(CanvasOutput(0))
          
          ; update drawing area and stats
          DrawImage(ImageID(imgFinal),MA(0)\lx,MA(0)\ly)
          
          showstats()
          
          ;update tool strip toggles
          If ListSize(lToggle())
            ForEach lToggle()
              toolToggle(lToggle()\b,lToggle()\c)
            Next
            ClearList(lToggle())
          EndIf
          
          StopDrawing()  
          
        EndIf
        
      EndIf
      
    EndIf
    
  Else
    Delay(1)
  EndIf
  
;   ExamineKeyboard()
;   
;   ; select colour
;   If KeyboardReleased(#PB_Key_R)
;     updateColSel(-1)
;   EndIf  
;   
;   If KeyboardReleased(#PB_Key_F)
;     updateColSel(1)
;   EndIf  
;   
;   ; select pattern colour (background)
;   If KeyboardReleased(#PB_Key_I)
;     updatepCol(1)
;   EndIf
;   
;   If KeyboardReleased(#PB_Key_J)
;     updatepCol(-1)
;   EndIf   
  
  
Until Event = #PB_Event_CloseWindow

End


;-------- Data Section --------

DataSection
  
  ; mouse area Data
  ; ensure to update maCount index to match number of mouse areas
  mouseData:
  Data.s "Drawing Area"
  Data.w 4,4,640,512
  Data.s "Colour Select"
  Data.w 386,526,262,39
  Data.s "Pattern Select"
  Data.w 0,524,72,75
  Data.s "Brush Size"
  Data.w 750,60,204,140
  Data.s "Stats"
  Data.w 72,524,100,52
  Data.s "Selected Pattern"
  Data.w 750,460,44,44
  Data.s "Tool Strip"
  Data.w 648,24,100,550
  Data.s "Fill Colour"
  Data.w 750,526,44,44
  Data.s "Pattern Select2"
  Data.w 0,370,586,208
  Data.s "Layers"
  Data.w 750,200,50,238  
  
  ; Patterns 0 - 17, format: 4x4 grid
  patternData:
  Data.a 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
  Data.a 0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0
  Data.a 0,0,0,0,0,0,1,0,0,0,0,0,1,0,0,0
  Data.a 0,0,0,0,0,0,1,0,0,0,0,0,1,0,1,0
  Data.a 0,0,0,0,1,0,1,0,0,0,0,0,1,0,1,0
  Data.a 0,0,0,0,1,0,1,0,0,1,0,0,1,0,1,0
  Data.a 0,0,0,1,1,0,1,0,0,1,0,0,1,0,1,0
  Data.a 0,0,0,1,1,0,1,0,0,1,0,1,1,0,1,0
  Data.a 0,1,0,1,1,0,1,0,0,1,0,1,1,0,1,0
  Data.a 1,0,1,0,0,1,0,1,1,0,1,0,0,1,0,1
  Data.a 1,1,1,0,0,1,0,1,1,0,1,0,0,1,0,1
  Data.a 1,1,1,0,0,1,0,1,1,0,1,1,0,1,0,1
  Data.a 1,1,1,1,0,1,0,1,1,0,1,1,0,1,0,1
  Data.a 1,1,1,1,0,1,0,1,1,1,1,1,0,1,0,1
  Data.a 1,1,1,1,1,1,0,1,1,1,1,1,0,1,0,1
  Data.a 1,1,1,1,1,1,0,1,1,1,1,1,0,1,1,1
  Data.a 1,1,1,1,1,1,1,1,1,1,1,1,0,1,1,1
  Data.a 1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1
  
  ; palette Data
  paletteData:
  Data.a 000,000,000,255,000,000,000,255,000,255,255,000,000,000,255,255,000,255
  Data.a 000,255,255,255,255,255,128,128,128,192,000,000,000,192,000,192,192,000
  Data.a 000,000,192,192,000,192,000,192,192,192,192,192,000,000,001
  
  ; bbc raw file format interlaced pixel data
  rawFileData:
  Data.a 0  ; 00000000
  Data.a 1  ; 00000001
  Data.a 4  ; 00000100
  Data.a 5  ; 00000101
  Data.a 16 ; 00010000
  Data.a 17 ; 00010010
  Data.a 20 ; 00010100
  Data.a 21 ; 00010101
  Data.a 64 ; 01000000
  Data.a 65 ; 01000001
  Data.a 68 ; 01000100
  Data.a 69 ; 01000101
  Data.a 80 ; 01010000
  Data.a 81 ; 01010001
  Data.a 84 ; 01010100
  Data.a 85 ; 01010101  
  
  ; inline toolstrip bmps
  ToolStripMain:       : IncludeBinary #PB_Compiler_FilePath + "/TOOLSTRIPEXA.BMP"
  ToolStripMain2:      : IncludeBinary #PB_Compiler_FilePath + "/TOOLSTRIP2EXA.BMP"
  
  ; custom pattern data
  customPatternData:
  Data.a 3,3,3,3,3,3,3,3,1,1,3,1,1,1,1,1,1,1,3,1,1,1,1,1,1,1,3,1,1,1,1,1,3,3,3,3,3,3,3,3,1,1,1,1,1,3,1,1,1,1,1,1,1,3,1,1,1,1,1,1,1,3,1,1
  Data.a 0,2,2,2,2,2,0,0,2,0,0,0,0,0,2,0,2,0,6,0,6,0,2,0,2,0,0,0,0,0,2,0,2,0,0,1,0,0,2,0,0,2,0,0,0,2,0,0,0,2,2,2,2,2,0,0,0,0,0,0,0,0,0,0
  Data.a 6,0,0,0,0,0,0,6,0,6,0,0,0,0,6,0,0,0,6,0,0,6,0,0,0,0,0,6,6,0,0,0,0,0,6,0,0,6,0,0,0,6,0,0,0,0,6,0,6,0,0,0,0,0,0,6,0,0,0,0,0,0,0,0
  Data.a 5,5,5,5,5,5,5,5,0,0,0,0,0,0,0,0,5,5,5,5,5,5,5,5,0,0,0,0,0,0,0,0,5,5,5,5,5,5,5,5,0,0,0,0,0,0,0,0,5,5,5,5,5,5,5,5,0,0,0,0,0,0,0,0
  
  Data.a 4,0,4,0,4,0,4,0
  Data.a 4,0,4,0,4,0,4,0
  Data.a 4,0,4,0,4,0,4,0
  Data.a 4,0,4,0,4,0,4,0
  Data.a 4,0,4,0,4,0,4,0
  Data.a 4,0,4,0,4,0,4,0
  Data.a 4,0,4,0,4,0,4,0
  Data.a 4,0,4,0,4,0,4,0

  Data.a 3,3,3,3,1,1,1,1
  Data.a 1,1,1,1,3,3,3,3
  Data.a 3,3,3,3,1,1,1,1
  Data.a 1,1,1,1,3,3,3,3
  Data.a 3,3,3,3,1,1,1,1
  Data.a 1,1,1,1,3,3,3,3
  Data.a 3,3,3,3,1,1,1,1
  Data.a 1,1,1,1,3,3,3,3

  Data.a 0,3,3,3,0,3,3,0
  Data.a 3,0,0,0,3,0,0,3
  Data.a 3,0,6,0,3,0,6,3
  Data.a 3,0,0,0,3,0,0,3
  Data.a 0,3,3,3,0,3,3,0
  Data.a 0,0,0,0,0,0,0,0
  Data.a 0,0,0,0,0,0,0,0
  Data.a 0,0,0,0,0,0,0,0

;   Data.a 
;   Data.a 
;   Data.a 
;   Data.a 
;   Data.a 
;   Data.a 
;   Data.a 
;   Data.a 
; 
;   Data.a 
;   Data.a 
;   Data.a 
;   Data.a 
;   Data.a 
;   Data.a 
;   Data.a 
;   Data.a 
; 
;   Data.a 
;   Data.a 
;   Data.a 
;   Data.a 
;   Data.a 
;   Data.a 
;   Data.a 
;   Data.a 
; 
;   Data.a 
;   Data.a 
;   Data.a 
;   Data.a 
;   Data.a 
;   Data.a 
;   Data.a 
;   Data.a 
; 
;   Data.a 
;   Data.a 
;   Data.a 
;   Data.a 
;   Data.a 
;   Data.a 
;   Data.a 
;   Data.a 
; 
;   Data.a 
;   Data.a 
;   Data.a 
;   Data.a 
;   Data.a 
;   Data.a 
;   Data.a 
;   Data.a 
; 
;   Data.a 
;   Data.a 
;   Data.a 
;   Data.a 
;   Data.a 
;   Data.a 
;   Data.a 
;   Data.a 
; 
;   Data.a 
;   Data.a 
;   Data.a 
;   Data.a 
;   Data.a 
;   Data.a 
;   Data.a 
;   Data.a 
; 
;   Data.a 
;   Data.a 
;   Data.a 
;   Data.a 
;   Data.a 
;   Data.a 
;   Data.a 
;   Data.a 
; 
;   Data.a 
;   Data.a 
;   Data.a 
;   Data.a 
;   Data.a 
;   Data.a 
;   Data.a 
;   Data.a 
; 
;   Data.a 
;   Data.a 
;   Data.a 
;   Data.a 
;   Data.a 
;   Data.a 
;   Data.a 
;   Data.a 

  
  
EndDataSection


; IDE Options = PureBasic 5.62 (Windows - x86)
; CursorPosition = 1853
; FirstLine = 1918
; Folding = ------
; EnableXP
; UseIcon = Art-icon.ico
; Executable = ART_EXA_PB_013_x86.exe
; DisableDebugger
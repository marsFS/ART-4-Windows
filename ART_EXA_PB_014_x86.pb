;
; ------------------------------------------------------------
;
;   ART 4 Exagear - Suits 800x600 screens (576 actual vertical - 24 for menu)
;
;    (c) Fourthstone
;
; ------------------------------------------------------------
;

; TODO 16/07/2018
;1) Ability To draw colour cycles along a straight line Or plot out a cycle.........As they tend To be Not very accurate when I use the mouse

;2) Directional Gradient Fills

;3) Ellipses

;4) Ability To lock the cursort horizontall Or vertically ( so you can draw straight lines With ease )
; ### DONE!!

;5) Spray brush - would be nice To be able To alter the flow And maybe more paint centred in the middle / less around the edges

;6) Cut n paste would be very interesting.......

;7) Direction Gradient Fills

;8) Zoom tool? Could be very useful this one......

;9) Also If there was a way of making the brush crosshair thicker And be able To turn it off And on.....I do use it a lot And sometimes would like To turn it off
; ### DONE!



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
#maApp=11 ; mouse area count 0-n for main application
#maOpt=4 ; mouse area count 0-n for options page

; tool strip constants
#toolSave=0
#toolLoad=1
#toolQSAll=2
#toolQSCur=3
#toolUndo=4
#toolRedo=5
#toolDraw=6
#toolLine=7
#toolCirOut=8
#toolCirFil=9
#toolBoxOut=10
#toolBoxFil=11
#toolGradHor=12
#toolGradVer=13
#toolFill=14
#toolCLS=15
#toolBrushBox=16
#toolBrushRound=17
#toolBrushSPR=18
#toolBrushRND=19
#toolBrush2x=20
#toolTransparent=21

; mouse area list index constants
#MA_Drawing=0
#MA_ColSel=1
#MA_PatternShow=2
#MA_BrushSize=3
#MA_Stats=4
#MA_SelectedPat=5
#MA_ToolStrip=6
#MA_FillCol=7
#MA_PatSel=8
#MA_Layers=9
#MA_SpriteShow=10
#MA_OptionShow=11

#MA_OptThin=12
#MA_OptThick=13
#MA_OptPixel=14
#MA_OptOff1=15
#MA_OptGuide=16


; structures
Structure MouseArea
  lx.w      ; left edge x (top left)
  ly.w      ; left edge y (top left)
  rx.w      ; right edge x
  ry.w      ; right edge y
  w.w       ; width rx-lx-1
  h.w       ; height ry-ly-1
  active.w  ; is mouse area active, used for dynamic menus
  name.s    ; identifier
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

Structure SpriteOBJ
  sw.w ; sprite width
  sh.w ; sprite height
  sx.w ; grid x offset
  sy.w ; grid y offset
  gs.w ; grid size
  sn.s ; sprite name
EndStructure

Structure Overlay
  x.w
  y.w
  img.i
EndStructure


; globals
Global pCol.a=0 ; pattern colour selected
Global pSel.a=8 ; pattern selected

Global dCol.a=1 ; drawing colour
Global dTrn.a=1 ; drawing transparency toggle
Global dWid.a=8 ; drawing width
Global dSel.a=#toolBrushBox; drawing brush selected
Global dFil.i=0 ; fill colour
Global dMdx=160 ; current mode horizontal pixels
Global dMdy=256 ; current mode vertical pixels
Global dMpx=4   ; current mode horizontal pixel size
Global dMpy=2   ; current mode vertical pixel size
Global dLay=0   ; current drawing layer
Global dAll=1   ; all layers visible flag
Global dWire.a=0; draw wireframe 
Global dGRD.a=1 ; draw transparent grid
Global dDSP.a=0 ; main canvas screen display selector: 0=paint, 1=spr drawing, 2=options
Global dSED.a=0 ; editing spr
Global dShift.a=0 ; Shift key flag
Global dCtrl.a=0  ; ctrl key flag

Global tCur.a=#toolDraw ; tool selected
Global tTog.a=3 ; tool toggle colour
Global tQSA.a=0 ; quick save all toggle
Global tQSC.a=0 ; quick save current toggle                  
Global tSel.a=0 ; new tool select
Global tHi=-1  ; tool highlight
Global tOld=-1 ; old tool highlight
Global tLIF.a=0 ; load image flag

Global oCrossHair.a=0 ; cross hair option flag
Global oMouseGuide.a=1 ; mouse guide option flag

Global mx,my,dx,dy,px,py,ox,oy,sx,sy,mact ; mouse x,y,action
Global imgToolStrip, imgToolStrip2, imgFinal, imgGRD; image handles
Global savePattern=0          ; save file pattern selector
Global loadPattern=0          ; load file pattern selector
Global imgOverlay.Overlay     ; image menu overlay

Global Dim dl.drawLayers(4) ; layers array
Global Dim SCRNout.a(#SCRNsize) ; final output buffer

Global Dim pat.a(17,15) ; drawing patterns
Global Dim bp(16)       ; beeb palette
;Global Dim MA.MouseArea(#maApp) ; mouse area structure array for main app
Global Dim MO.MouseArea(#maOpt) ; mouse area structure array for options page

Global Dim rgbT.rgbTable(16)     ; rgb lookup table
Global Dim ct(16)                ; colour table look for redrawing main canvas
Global Dim rawBBC.a(15)          ; raw bbc file format data
Global Dim revBBC.a(85)          ; reverse lookup bbc file format data
Global Dim curPat.a(15,15)       ; current drawing pattern colour matrix
Global Dim cusPat.a(17,63)       ; custom pattern array, 18 tiles of 8x8 colour data

; lists
Global NewList MA.MouseArea()

Global NewList lToggle.togglelist()

Global NewList lFS.fillStack()

Global NewList SPR.spriteOBJ()


; Exit program and display a message
Procedure Exit_ART(m.s)
  If m<>"" 
    MessageRequester("Error", m, 0)
  EndIf
  
  End
EndProcedure

; select mouse area by name
Procedure findMA(n.s)
  If n<>MA()\name
    ResetList(MA())
    
    ; find MA with name matching
    While NextElement(MA())
      If MA()\name = n
        Break
      EndIf
    Wend 
  EndIf
EndProcedure

; select mouse area by index
Procedure selectMA(i.i)
  If i<>ListIndex(MA())
    SelectElement(MA(),i)
  EndIf
EndProcedure


; set pixel coords For relative To draw window
Procedure setP(x,y)
  selectMA(#MA_Drawing)
  px=(x-MA()\lx) / dMpx
  py=255-((y-MA()\ly) / dMpy)
  ;py=(y-MA()\ly) / dMpy
EndProcedure

Procedure px1(x)
  ProcedureReturn (x-MA()\lx) / dMpx
EndProcedure
Procedure py1(y)
  ProcedureReturn 255-((y-MA()\ly) / dMpy)
EndProcedure

; display program stats
Procedure showstats()
  Protected x
  x=30
  setP(mx,my)
  
  selectMA(#MA_Stats)
  
  Box(MA()\lx+x,MA()\ly,44,48,bp(0))
  Box(MA()\lx+x+76,MA()\ly,44,48,bp(0))
  ;Box(MA()\lx+x+160,MA()\ly,44,48,bp(0))
  ;Box(MA()\lx+x+240,MA()\ly,44,32,bp(0))
  
  DrawText(MA()\lx+x,MA()\ly,Str(mx))
  DrawText(MA()\lx+x,MA()\ly+16,Str(my))
  DrawText(MA()\lx+x,MA()\ly+32,Str(mact))
  ;DrawText(MA()\lx+x,MA()\ly+32,Str(tCur))
  
  DrawText(MA()\lx+x+76,MA()\ly,Str(px))
  DrawText(MA()\lx+x+76,MA()\ly+16,Str(py))
  DrawText(MA()\lx+x+76,MA()\ly+32,Str(dWid))
  ;DrawText(MA()\lx+x+80,MA()\ly+32,Str(tSel))
    
  ;DrawText(MA()\lx+x+160,MA()\ly,Str(dWid))
  ;DrawText(MA()\lx+x+160,MA()\ly+16,Str(mact))
  
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
Procedure rangeApp(i)
  Protected r.a=0
  
  selectMA(i)
  If mx>=MA()\lx And mx<=MA()\rx And my>=MA()\ly And my<=MA()\ry
    r=1
  EndIf
  ProcedureReturn r
EndProcedure

; check if mouse is in range of mouse area object, return '1' if in range
Procedure rangeOpt(i,x,y)
  Protected r.a=0
  If x>=MO(i)\lx And x<=MO(i)\rx And y>=MO(i)\ly And y<=MO(i)\ry
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
            Box(lx*4,510-ly*2,4,2,bp(dc))
          Else
            dl(dLay)\SCRN[ly*640+lx]=dc
          EndIf
        EndIf
      EndIf
    Next
  Next
  
EndProcedure

; update current pattern template
Procedure updateBrush()
  Protected px,py,lx,ly,dc,ps,s
  
  ; set skip counter  
  If dSel=#toolBrush2x
    s=1
  EndIf  
  
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
  selectMA(#MA_SelectedPat)
  For lx=0 To 8
    For ly=0 To 17
      px=lx % 16
      py=ly % 16
      If curPat(px,py)=16 
        dc=8
      Else
        dc=curPat(px,py)
      EndIf
      Box(MA()\lx+lx*4+4,MA()\ly+ly*2+4,4,2,bp(dc))
    Next
  Next
  
EndProcedure

; update palette, assumes startdrawing is already active
Procedure updatePalette()
  Protected i,p,x,dc,p1,p2,i1,i2,xd,yd
  
  selectMA(#MA_PatSel)
  dc=bp(1)
  For i=0 To 8          ; colour loop 0 = bottom - 7 = top
                        ; preset plot positions
    i1=MA()\ly+182-i*22
    i2=MA()\ly+190-i*22
    For p=0 To 17       ; pattern number loop left to right
                        ; preset plot positions
      p1=MA()\lx+4+p*32
      p2=MA()\lx+20+p*32
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
  
  ; highlight selected pattern and update selected brush size pattern
  Box(MA()\lx+pSel*32+4,MA()\ry-11-pCol*22+4,32,2,bp(7))
  
  updateBrush()
  
  
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
        dc=curPat(x1M,15-(ly % 16))        
        
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
    y1M=15-(y1 % 16)
    For lx=x1 To x2
      ; range check, set pattern colour And plot
      If lx>-1 And lx<dMdx
        dc=curPat(lx % 16,y1M) 
        
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
;   setP(x1,y1)
;   x1=px : y1=py
;   
;   setP(x2,y2)
;   x2=px : y2=py
  
  
  x1=px1(x1) : y1=py1(y1)
  x2=px1(x2) : y2=py1(y2)
  
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
  Protected r2,dy,x,px1,px2,py1,py2
  
  r=Abs(r)
  r2=r*r
  
  For x=r To 0 Step -2
    dy=Sqr(r2-x*x)
    setP(x1-x,Int(y1-dy))
    px1=px : py1=py
  
    setP(x1+x,Int(y1+dy))
    px2=px : py2=py  

    Vline2(px1,py1,py2)
    Vline2(px2,py1,py2)
  Next
  
EndProcedure  

; circle Outline drawing brush
Procedure dCircOut(x1,y1,r)
  Protected t
  
  r=Abs(r)
  For t=0 To 359
    Select dSel
      Case #toolBrushRND ; airbrush
        setP(Int(x1+r*Cos(Radian(t))),Int(y1-r*Sin(Radian(t))))        
        dBrush(px,py,dWid,2)
      Case #toolBrush2x ; standard X2 brush
        setP(Int(x1+r*Cos(Radian(t))),Int(y1-r*Sin(Radian(t))))
        dBrush(px,py,dWid,1)
      Default
        dCircle(x1+r*Cos(Radian(t)),y1-r*Sin(Radian(t)),dWid*2)
    EndSelect
    
  Next
EndProcedure

; box draw
Procedure dBox(x1,y1,x2,y2,f)
  setP(x1,y1)
  x1=px : y1=py
  
  setP(x2,y2)
  x2=px : y2=py  
  
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
  setP(x1,y1)
  x1=px : y1=py
  
  setP(x2,y2)
  x2=px : y2=py
  
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
  selectMA(#MA_ColSel)
  drawBox(MA()\lx+2+oldDC*32,MA()\ly+3,MA()\lx+33+oldDC*32,MA()\ly+34,bp(0))
  dCol=newDC
  drawBox(MA()\lx+2+dCol*32,MA()\ly+3,MA()\lx+33+dCol*32,MA()\ly+34,bp(7))
EndProcedure

; update colour select value
Procedure updateColSel(c.b)
  Protected oldC
  
  If StartDrawing(CanvasOutput(0))    
    oldC=dCol
    dCol-c
    If dcol=255:dCol=7:EndIf
    If dcol=8:dCol=0:EndIf
    If mact<>#MA_PatSel  
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
  selectMA(#MA_ColSel)
  
  Box(MA()\lx,MA()\ly,MA()\rx-MA()\lx,MA()\ry-MA()\ly,bp(0))
  For i=1 To 7
    Box(MA()\lx+4+i*32,MA()\ly+5,28,28,bp(i))
  Next  
  
  drawBox(MA()\lx,MA()\ly,MA()\rx,MA()\ry,bp(7))
  drawBox(MA()\lx+1,MA()\ly+1,MA()\rx-1,MA()\ry-1,bp(8))
  
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
    
    If mact=#MA_PatSel
      selectMA(#MA_PatSel)
      Box(MA()\lx+pSel*32+4,MA()\ry-11-oldP*22+4,32,2,bp(0))
      Box(MA()\lx+pSel*32+4,MA()\ry-11-pCol*22+4,32,2,bp(7))
    EndIf
    
    updateBrush()
    StopDrawing()
  EndIf  
EndProcedure  

; update layer selector
Procedure updateLayers()
  Protected t.s, h.a
  
  selectMA(#MA_Layers)
  
  Box(MA()\lx,MA()\ly+20,MA()\rx-MA()\lx,MA()\ry-MA()\ly-20,bp(0))
  
  For i=0 To ArraySize(dl())+2
    drawbox(MA()\lx+20,MA()\ly+20+i*32,MA()\lx+44,MA()\ly+44+i*32,bp(7))
    
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
      Box(MA()\lx,MA()\ly+20+i*32,16,24,bp(1))
    EndIf
    
    If h
      Box(MA()\lx+23,MA()\ly+23+i*32,19,19,bp(2))
    EndIf
    
    DrawingMode(#PB_2DDrawing_Transparent)
    DrawText(MA()\lx+4,MA()\ly+24+i*32,t,bp(7))
    DrawingMode(#PB_2DDrawing_Default)
  Next
  
EndProcedure

; flood fill With current pattern
Procedure floodFill(sx,sy)
  setP(sx,sy)
  
  If (px)>-1 And (px)<dMdx And (py)>-1 And (py)<dMdy
    
    Protected uf,df,c,x,y,mc,dc,i,fp
    
    uf=0
    df=0
    dc=15
    mc=dFil
    
    
    ; first iteration fills With mask colour (15) to replace fill colour
    ; second iteration replaces mask colour with current pattern
    For i=0 To 1
      ; store start x,y on stack
      AddElement(lFS())
      lFS()\x=px : lFS()\y=py
      
      ; loop until no fill elements in fill stack (list)
      Repeat
        ; get Next fill point from fill List
        x=lFS()\x : y=lFS()\y
        DeleteElement(lFS())
        
        If dl(dLay)\SCRN[y*640+x]=mc 
          uf=1 : df=1
          
          ; scan left
          While x>0 And dl(dLay)\SCRN[y*640+x-1]=mc
            x-1
          Wend
          
          ; scan right and plot fill colour
          While x<dMdx And dl(dLay)\SCRN[y*640+x]=mc
            If i
              dc=curPat(x % 16,15-(y % 16))
            EndIf
            
            dl(dLay)\SCRN[y*640+x]=dc
            
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
  
  selectMA(#MA_ToolStrip)
  x=MA()\lx+(i % 2)*50+4
  y=MA()\ly+(i/2)*50+4
  
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

; draw grid with grid dimensions of BX, BY, assumes startdrawing is initiated
Procedure drawGrid()
  Protected sx,sy,lx,ly,gs,dc
  
  dc=bp(8)
  
  gs=SPR()\gs
    
  ; determine center of drawing frame base on box size
  sx=SPR()\sx
  sy=SPR()\sy
  lx=SPR()\sw
  ly=SPR()\sh
  
  ; horizontal grid
  For x=0 To lx
    LineXY(sx+x*gs,sy,sx+x*gs,sy+ly*gs,dc)
  Next
  
  ; vertical grid
  For y=0 To ly
    LineXY(sx,sy+y*gs,sx+lx*gs,sy+y*gs,dc)
  Next
  
  ; spr data
  For x=0 To lx-1
    For y=0 To ly-1
      dc=cusPat(dSED,x+y*8)
      Box(sx+1+x*gs,sy+1+y*gs,gs-1,gs-1,bp(dc))
    Next
  Next
  
EndProcedure

Procedure updateSPR()
  Protected x1,y1,gs
  
  ; determine smallest required box required to fit into drawing frame
  x1=Int(640/SPR()\sw)
  y1=Int(512/SPR()\sh)
  If x1>y1
    gs=y1
  Else
    gs=x1
  EndIf
  ; upper limit of grid size
  If gs>20:gs=20:EndIf
  
  SPR()\gs=gs
  
  ; determine center of drawing frame base on box size
  SPR()\sx=320-SPR()\sw*gs/2
  SPR()\sy=256-SPR()\sh*gs/2

EndProcedure

; update current sprite
Procedure modSPR(w,h)
  SPR()\sw=w
  SPR()\sh=h
  
  updateSPR()
EndProcedure

; add new sprite to sprite list
Procedure newSPR(w,h)
 AddElement(SPR())

 modSPR(w,h)
  
EndProcedure 

; draw on spr window
Procedure sprBrush(mx,my)
  Protected lx,ly
  
  selectMA(#MA_Drawing)
  lx=(mx-MA()\lx-SPR()\sx)
  ly=(my-MA()\ly-SPR()\sy)
  
  If lx>-1 And lx< SPR()\gs* SPR()\sw And ly>-1 And ly<SPR()\gs* SPR()\sh 
    lx / SPR()\gs
    ly / SPR()\gs
  If lx>-1 And lx<SPR()\sw And ly>-1 And ly<SPR()\sh
    cusPat(dSED,lx+ly*SPR()\sw)=dCol
  EndIf
  EndIf
EndProcedure

; get copy of screen area for menu overlay
Procedure getBackground(x,y,w,h,*OL.Overlay)
  Protected imgTMP, imgIN, imgOverlay
  
  ; grab entire canvas image and store in tmp image handle
  imgIN=GetGadgetAttribute(0,#PB_Canvas_Image)
  imgTMP=CreateImage(#PB_Any,#scrW,#scrH)
  If StartDrawing(ImageOutput(imgTMP))
    DrawImage(imgIN,0,0)
    StopDrawing()
  EndIf
  
  ; assign values to referecing structure variables
  *OL\img=GrabImage(imgTMP,#PB_Any,x,y,w,h)
  *OL\x=x
  *OL\y=y
  
  FreeImage(imgTMP)
  
  ; clear area and draw border
  If StartDrawing(CanvasOutput(0))
    Box(*OL\x,*OL\y,w,h,bp(0))
    drawBox(*OL\x+1,*OL\y+1,*OL\x+w-2,*OL\y+h-2,bp(7))
    StopDrawing()
  EndIf
  
EndProcedure

Procedure putBackground(*OL.Overlay)
  
  ; replace menu overlay with background image
  If StartDrawing(CanvasOutput(0))
    DrawImage(ImageID(*OL\img),*OL\x,*OL\y)
    
    StopDrawing()
    FreeImage(*OL\img)
    *OL\img=0
  EndIf  
  
EndProcedure

; copy screen data to output buffer
; l = layer to copy, t = save transparency if <> 0
Procedure LayerToOutput(l,t)
  Protected x
  For x=0 To #SCRNsize
    If dl(l)\SCRN[x]
      If t
        SCRNout(x)=dl(l)\SCRN[x] ; colour 16 = preserve true black for PNG format
      Else
        SCRNout(x)=dl(l)\SCRN[x] % 16 ; convert colour 16 (fake black) to true black for raw images
      EndIf
    EndIf
  Next    
EndProcedure

; render image from SCRNout buffer and save PNG image
Procedure savePNG(f.s,iTrans)
  Protected x,y,i,dc,yMul
  
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
    If iTrans
      ct(0)=RGBA(0,0,0,255) ; ignore transparency
    Else
      ct(0)=RGBA(0,0,0,0) ; transparent black
    EndIf
    
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
    
    SaveImage(imgFinal, f, #PB_ImagePlugin_PNG);,10,4)
  EndIf  
EndProcedure

; load png into specific layer
Procedure loadPNG(f.s,l)
  Protected iTMP,x,y,yMul
  
  iTMP=LoadImage(#PB_Any,f)
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
              dl(l)\SCRN[x+yMul]=i
              Break
            EndIf
          Next
          If *Line\Pixel=RGBA(0,0,0,255)
            dl(l)\SCRN[x+yMul]=16
          EndIf
          *line+16  
        Next
      Next
      
      StopDrawing()
      FreeImage(iTMP)            
    EndIf          
  Else
    MessageRequester(" File Error","ERROR: Cannot load file..." + #CRLF$ + #CRLF$ + f,#PB_MessageRequester_Error)
  EndIf  
EndProcedure

; format raw data from SCRNout buffer and save as RAW beeb image
Procedure saveRAW(f.s)
  Protected x,y,i,a.a,b.a
  
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
    f=UCase(f)
    If CreateFile(0, f)
      WriteData(0, *MemoryID, #RAWsize)
      CloseFile(0)
      
      ; create INF file
      If CreateFile(1, f+".INF")
        WriteString(1, "$."+GetFilePart(f)+" 003000 000000 005000")
        
        CloseFile(1)
      Else
        MessageRequester (" File Error","Cannot create INF file..."+#CRLF$+#CRLF$+f+".INF",#PB_MessageRequester_Error)
      EndIf
      
    Else
      MessageRequester (" File Error","Cannot create file..."+#CRLF$+#CRLF$+f,#PB_MessageRequester_Error)
    EndIf
    FreeMemory(*MemoryID)
  EndIf  
EndProcedure

; load RAW file to specific layer
Procedure loadRAW(f.s,l)
  Protected x,y,i,a.a,b.a
  ;check file size
  If FileSize(f)=#RAWsize
    ; create temp buffer and fill with file data
    *MemoryID = AllocateMemory(#RAWsize)       ; allocate 20k memory block
    If *MemoryID
      
      ; slurp in file data to buffer
      If ReadFile(0, f)
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
          dl(l)\SCRN[x+y*640]=revBBC(a)
          dl(l)\SCRN[x+1+y*640]=revBBC(b)
          
          ; step through mode 2 pixel order
          ; starts at lower right corner and steps left char by char, each char is eight bytes high
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
        MessageRequester(" File Error","ERROR: Cannot load file..." + #CRLF$ + #CRLF$ + f,#PB_MessageRequester_Error)
      EndIf
      FreeMemory(*MemoryID)
    EndIf
  Else
    MessageRequester(" File Error","ERROR: File must be exactly "+Str(#RAWsize)+" bytes..." + #CRLF$ + #CRLF$ + f,#PB_MessageRequester_Error)  
  EndIf  
EndProcedure

; open save handler
Procedure openSave(mode)
  
  Protected filename.s, QSfolder.s, action.s, F.s, ff.s, N.w, iTMP, a.a,b.a
  
  ; load options:
  ; PNG to current layer
  ; RAW to current layer
  ; PNG all layers (requires 5 images named *01 to *05)
  ; RAW all layers (requires 5 images named *01 to *05)
  
  ; save options:
  ; PNG from current layer
  ; RAW from current layer
  ; PNG merge all layers 
  ; RAW merge all layers 
  ; PNG save individual layers with transparency (saves 5 images in date stamped folder)
  ; PNG save individual layers No transparency (saves 5 images in date stamped folder)
  ; RAW save individual layers (saves 5 images in date stamped folder)

  
  Select mode
    Case 0 ; get filename to load
      ff = "PNG file - Load Current Layer (*.PNG)|*.PNG|PNG file - Load All Layers (*.PNG)|*.PNG|BBC file - Load Current Layer (*.*)|*.*|BBC file - Load All Layers (*.*)|*.*"
      filename=OpenFileRequester(" Load Image",GetCurrentDirectory(),ff,loadPattern)
      loadPattern=SelectedFilePattern()
      
    Case 1 ; save dialog
           ; multiple save options:
           ; PNG - Current Layer, 
      
      
      ff = "PNG file - Save Current Layer (*.PNG)|*.PNG|PNG file - Save All Layers Merged Preserve Transparency (*.PNG)|*.PNG|PNG file - Save All Layers Merged Discard Transparency (*.PNG)|*.PNG|PNG file - Save All Layers Separated Preserve Transparency (*.PNG)|*.PNG|PNG file - Save All Layers Separated Discard Transparency (*.PNG)|*.PNG|BBC file - Save Current Layer (*.*)|*.*|BBC file - Save All Layers Merged (*.*)|*.*|BBC file - Save All Layers Separated (*.*)|*.*"
      
      If tQSA=0 And tQSC=0
        Repeat
          ok=#PB_MessageRequester_Yes
          
          ; show save dialog and prompt if file exists
          filename=SaveFileRequester(" Save Image",GetCurrentDirectory(),ff,savePattern)
          savePattern=SelectedFilePattern()
          If filename
            Select savePattern
              Case 0,1,2,3,4 ; png
                If Right(UCase(filename),4)<>".PNG"
                  filename=filename+".PNG"
                EndIf
                
              Case 5,6,7 ; bbc raw
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
        ; Quick save folder create and file config
        sNow=Date()
        QSfolder=GetCurrentDirectory()+"\ART_QS_"+FormatDate("%yyyy%mm%dd",sNow)+"_"+FormatDate("%hh%ii%ss",sNow)
        If CreateDirectory(QSfolder)
          filename=QSfolder+"\ART_QS_"
        Else
          MessageRequester(" Folder Error","ERROR: Could not create quick save folder:"+ #CRLF$ + #CRLF$ + QSfolder,#PB_MessageRequester_Error)
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
          Case 0 ; load png to current layer
            loadPNG(filename,dLay)
            
          Case 1 ; load png(s) to all layers
            For i=0 To ArraySize(dl())
              f=Left(filename,Len(filename)-7)+"_0"+Str(i+1)+".PNG"
              loadPNG(f,i)
            Next
            
          Case 2 ; load bbc raw format
            loadRAW(filename,dLay)
            
          Case 3 ; load raw(s) to all layers
            For i=0 To ArraySize(dl())
              f=Left(filename,Len(filename)-2)+"0"+Str(i+1)
              loadRAW(f,i)
            Next
            
        EndSelect
        
      Case 1 ; save drawing image to file
             
        If tQSA ; quick save all layers
          For i=0 To ArraySize(dl())
            ; erase output buffer
            For x=0 To #SCRNsize
              SCRNout(x)=0 ; clear output buffer
            Next
            
            LayerToOutput(i,0)
            
            f=filename+"0"+Str(i+1)
            savePNG(f+".PNG",0)
            saveRAW(f)
          Next
          MessageRequester(" Quick Save - All Layers","INFO: All layers saved to the following folder: " + #CRLF$ + #CRLF$ + QSfolder,#PB_MessageRequester_Info)
        ElseIf tQSC ; quick save current layer
            ; erase output buffer
            For x=0 To #SCRNsize
              SCRNout(x)=0 ; clear output buffer
            Next
            
            LayerToOutput(dLay,0)
            
            filename+"0"+Str(dLay+1)
            savePNG(filename+".PNG",0)
            saveRAW(filename)
            MessageRequester(" Quick Save - Current Layer","INFO: Current layer saved to the following folder: " + #CRLF$ + #CRLF$ + QSfolder,#PB_MessageRequester_Info)
        Else
          
          ; erase output buffer
          For x=0 To #SCRNsize
            SCRNout(x)=0 ; clear output buffer
          Next
          
          ; copy relevant layer details to output buffer and save
          Select savePattern
            Case 0 ; PNG - current layer
              LayerToOutput(dLay,1)
              savePNG(filename,0)
              
            Case 1,2  ; PNG - merge all layers
              For i=0 To ArraySize(dl())
                LayerToOutput(i,1)
              Next
              If savePattern=1
                savePNG(filename,0) ; preserve transparency
              Else
                savePNG(filename,1) ; discard transparency
              EndIf
              
            Case 3,4 ; PNG - save all layers individual with or without Transparency
              For i=0 To ArraySize(dl())
                ; erase output buffer
                For x=0 To #SCRNsize
                  SCRNout(x)=0 ; clear output buffer
                Next
                
                LayerToOutput(i,0)
                
                f=Left(filename,Len(filename)-4)+"_0"+Str(i+1)
                If savePattern=3
                  savePNG(f+".PNG",0) ; preserve transparency
                Else
                  savePNG(f+".PNG",1) ; discard transparency
                EndIf
              Next
              
            Case 5 ; RAW - Current layer
              LayerToOutput(dLay,0)
              saveRAW(filename)
              
            Case 6 ; RAW - all layers
              For i=0 To ArraySize(dl())
                LayerToOutput(i,0)
              Next
              saveRAW(filename)
              
            Case 7 ; RAW - save all layers individual
              For i=0 To ArraySize(dl())
                ; erase output buffer
                For x=0 To #SCRNsize
                  SCRNout(x)=0 ; clear output buffer
                Next
                
                LayerToOutput(i,0)
                
                f=filename+"_0"+Str(i+1)
                saveRAW(f)
              Next                
          EndSelect
        EndIf
      
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
    addToggle(#toolUndo,7)
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
    addToggle(#toolRedo,7)
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
      addToggle(#toolUndo,8)
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
      addToggle(#toolRedo,8)
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
For i=0 To 17
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

; mouse area / tool location list - main application
Restore mouseData

Repeat
  Read.s tName.s
  If tName<>"DATAEND"
    AddElement(MA())
    MA()\name=tName
    Read.w MA()\lx
    Read.w MA()\ly
    Read.w MA()\w
    Read.w MA()\h
    Read.w MA()\active
    MA()\rx=MA()\lx+MA()\w-1
    MA()\ry=MA()\ly+MA()\h-1
  EndIf
Until tName="DATAEND" 


; mouse area / tool location list - Options page
Restore optionsData
For i=0 To #maOpt
  Read.s MO(i)\name
  Read.w MO(i)\lx
  Read.w MO(i)\ly
  MO(i)\rx=MO(i)\lx+20
  MO(i)\ry=MO(i)\ly+20
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
CanvasGadget(0,0,0,#scrW,#scrH,#PB_Canvas_Keyboard)
SetGadgetAttribute(0, #PB_Canvas_Cursor , #PB_Cursor_Cross)
mact=-1

tFont=LoadFont(#PB_Any,"Comic Sans MS",14,#PB_Font_Italic |#PB_Font_Bold)

newSPR(8,8)

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
  
  ; main canvas double border and grid pattern
  selectMA(#MA_Drawing)
  drawBox(MA()\lx-4,MA()\ly-4,MA()\rx+4,MA()\ry+4,bp(7))
  drawBox(MA()\lx-3,MA()\ly-3,MA()\rx+3,MA()\ry+3,bp(7))
  DrawImage(ImageID(imgGRD),MA()\lx,MA()\ly)
  
  ; pattern select border
  selectMA(#MA_ToolStrip)
  DrawingFont(FontID(tFont))
  DrawText(MA()\lx+4,0,"EXA ART",bp(1))
  DrawingFont(#PB_Default)
  
  ; draw tools strip and toggle defaults
  DrawImage(ImageID(imgToolStrip),MA()\lx,MA()\ly)
  DrawImage(ImageID(imgToolStrip2),0,524)
  toolToggle(#toolUndo,8) ; undo
  toolToggle(#toolRedo,8) ; redo
  toolToggle(tCur,tTog) ; standard
  toolToggle(#toolTransparent,2)      ; transparency
  toolToggle(dSel,6)    ; draw style  
  
  ; brush size control
  selectMA(#MA_BrushSize)
  Circle(MA()\lx+33,MA()\ly+118,16,bp(6))
  Circle(MA()\lx+33,MA()\ly+84,12,bp(6))
  Circle(MA()\lx+33,MA()\ly+55,10,bp(6))
  Circle(MA()\lx+33,MA()\ly+33,7,bp(6))
  Circle(MA()\lx+33,MA()\ly+13,5,bp(6))
  Circle(MA()\lx+6,MA()\ly+dWid*4+6,4,bp(7))
  
  DrawText(MA()\lx+4,MA()\ly-36,"Brush",bp(7))
  DrawText(MA()\lx+8,MA()\ly-20,"Size",bp(7))
  
  updateBrush()
  
  ; draw colour select boxes and double border
  resetColSel()
  
  ; stats area
  selectMA(#MA_Stats)
  DrawText(MA()\lx,MA()\ly,"mX:")
  DrawText(MA()\lx,MA()\ly+16,"mY:")
  DrawText(MA()\lx,MA()\ly+32,"mA:")
  ;DrawText(MA()\lx,MA()\ly+32,"CT:")
  DrawText(MA()\lx+76,MA()\ly,"pX:")
  DrawText(MA()\lx+76,MA()\ly+16,"pY:")
  DrawText(MA()\lx+76,MA()\ly+32,"dW:")
  ;DrawText(MA()\lx+80,MA()\ly+32,"TS:")
  ;DrawText(MA()\lx+160,MA()\ly,"dW:")
  ;DrawText(MA()\lx+160,MA()\ly+16,"mA:")
  
  
  selectMA(#MA_SelectedPat)
  DrawText(MA()\lx,MA()\ly-20,"Pat",bp(7))
  drawBox(MA()\lx,MA()\ly,MA()\rx,MA()\ry,bp(7))
  
  selectMA(#MA_FillCol)
  DrawText(MA()\lx,MA()\ly-20,"Fill",bp(7))
  drawBox(MA()\lx,MA()\ly,MA()\rx,MA()\ry,bp(7))
  
  updateLayers()
  DrawText(MA()\lx+4,MA()\ly,"Layer",bp(7))
  
  ; enable for debugging mouse area squares
;   ResetList(MA())
;   While NextElement(MA())
;     drawBox(MA()\lx,MA()\ly,MA()\rx,MA()\ry,bp((ListIndex(MA()) % 7)+1))
;   Wend
  
  StopDrawing()
EndIf

;
;-------- MainLoop --------
;

Repeat
  Event = WaitWindowEvent()
  
  If event
    
    ; ensure event is triggered from ART
    If Event = #PB_Event_Gadget And EventGadget() = 0 
      
      ; read mouse
      mx = GetGadgetAttribute(0, #PB_Canvas_MouseX)
      my = GetGadgetAttribute(0, #PB_Canvas_MouseY)
      
      ; get relative drawing coords of mouse
      selectMA(#MA_Drawing)
      dx = mx - MA()\lx
      dy = my - MA()\ly
      
      ; handle menu hide if mouse up or out of range
      If mact=#MA_PatSel 
        If EventType()=#PB_EventType_LeftButtonUp Or (rangeApp(#MA_PatSel)=0 And (GetGadgetAttribute(0, #PB_Canvas_Buttons)=0))
          mact=-1
          If imgOverlay\img
            putBackground(imgOverlay)
            If StartDrawing(CanvasOutput(0))
              resetColSel()
              StopDrawing()
            EndIf
          EndIf
        EndIf
      EndIf      
      
      ; check for mouse move events
      If EventType() = #PB_EventType_MouseMove
        
        ; change cursor if needed
        x=GetGadgetAttribute(0, #PB_Canvas_Cursor)
        If rangeApp(#MA_Drawing) And mact<>#MA_PatSel And dDSP=0
          If x<>#PB_Cursor_Invisible
            SetGadgetAttribute(0, #PB_Canvas_Cursor , #PB_Cursor_Invisible)
          EndIf
        Else
          If x<>#PB_Cursor_Cross
            SetGadgetAttribute(0, #PB_Canvas_Cursor , #PB_Cursor_Cross)
          EndIf
        EndIf
        
        ; check if mouse is in pattern area and draw pattern menu overlay
        If mact=-1
          If rangeApp(#MA_PatternShow)
            mact=#MA_PatSel
            selectMA(#MA_PatSel)
            getBackground(MA()\lx,MA()\ly,MA()\w,MA()\h,imgOverlay)
            
            If StartDrawing(CanvasOutput(0))
              updatePalette()
              StopDrawing()
            EndIf
          EndIf
        EndIf
      EndIf
      
      ; left mouse button down or mouse move with left button events
      If EventType() = #PB_EventType_LeftButtonDown Or (EventType() = #PB_EventType_MouseMove And GetGadgetAttribute(0, #PB_Canvas_Buttons) & #PB_Canvas_LeftButton)
        
        ; set mouse action if none already set
        If mact=-1 And tLIF=0
          
          ; set default action to none and check mouse area ranges to select current mouse area
          mact=99
          i=0
          ResetList(MA())
          
          ; range find MA
          While NextElement(MA())
            If MA()\active And mx>=MA()\lx And mx<=MA()\rx And my>=MA()\ly And my<=MA()\ry
              mact=i
              Break
            EndIf
            i+1
          Wend
          
          ; if drawing on main painting canvas then save an undo before drawing begins
          If dDSP=0            
            Select mact
                
              Case #MA_Drawing ; main drawing canvas - save undo
                     ; clear redo when new drawing starts
                If ListSize(dl(dLay)\lRedo())
                  ClearList(dl(dLay)\lRedo())
                  addToggle(#toolRedo,8)
                EndIf
                saveUndo()
                sx=dx
                sy=dy
                ox=sx
                oy=sy
              Case #MA_PatSel ; ignore pattern area if not selected via mouse over hotspot
                mact=-1
            EndSelect
          EndIf
        
        EndIf
        
        ; do any drawing actions for mouse move
        Select mact
          Case #MA_Drawing ; main drawing canvas
            Select dDSP
              Case 0 ; standard painting mode
                
                setP(mx,my)
                
                ; determine tool being used
                Select tCur
                  Case #toolDraw ; brush tool
                    Select dSel
                      Case #toolBrushBox ; standard brush
                        ;dbrush(px,py,dWid,0)
                        
                        selectMA(#MA_Drawing)
                        dLine(mx,my,ox+MA()\lx,oy+MA()\ly)
                      Case #toolBrushRound ; circle brush
                        dCircle(mx,my,dWid*2)
                      Case #toolBrushRND ; airbrush
                        dbrush(px,py,dWid,2)
                      Case #toolBrush2x ; standard X2 brush
                        dbrush(px,py,dWid,1)
                    EndSelect
                  Case #toolLine,#toolCirOut,#toolCirFil,#toolBoxOut,#toolBoxFil,#toolGradHor,#toolGradVer
                

                    
                  Case #toolFill ; flood fill
                    If rangeApp(#MA_Drawing)
                      i=dl(dLay)\SCRN[px+640*py];Point(mx,my) get pixel colour under cursor
                                                        ;If i<>dFil                        ; continue if fill colour is not the same as last fill
                      dFil=i
                      If StartDrawing(CanvasOutput(0)) ; update fill colour box in tool box
                        selectMA(#MA_FillCol)
                        Box(MA()\lx+4,MA()\ly+4,MA()\rx-MA()\lx-7,MA()\ry-MA()\ly-7,bp(dFil))
                        StopDrawing()
                      EndIf
                      ;EndIf
                    EndIf
                EndSelect
                
                dWire=tCur
                ox=dx
                oy=dy
                ; *** end drawing tool code
                
              Case 1 ; sprite drawing mode
                sprBrush(mx,my)
                Select tCur
                  Case #toolDraw ; brush tool
                    Select dSel
                      Case #toolBrushBox ; standard brush
                                         ;dbrush(px(mx),py(my),dWid,0)
                                         ;sprBrush(mx,my)
                      Case #toolBrushRound ; circle brush
                                           ;dCircle(mx,my,dWid*2)
                      Case #toolBrushRND   ; airbrush
                                           ;dbrush(px(mx),py(my),dWid,2)
                      Case #toolBrush2x    ; standard X2 brush
                                           ;dbrush(px(mx),py(my),dWid,1)
                    EndSelect
                EndSelect
                
              Case 2 ; options screen
                For i=0 To #maOpt
                  If rangeOpt(i,dx,dy)
                    Select i
                      Case 0,1,2,3
                        oCrossHair=i
                        If i=3
                          oMouseGuide=1
                        EndIf
                      Case 4
                        oMouseGuide=1-oMouseGuide
                        If oCrossHair=3
                          oMouseGuide=1
                        EndIf                          
                    EndSelect
                    Break
                  EndIf
                Next                
            EndSelect
          
          Case #MA_ColSel ; colour select
            selectMA(#MA_ColSel)
            i=(mx-MA()\lx) / 32
            If dCol<>i And i>-1 And i<8
              If StartDrawing(CanvasOutput(0))    
                drawColSel(dCol,i)
                updateBrush()
                StopDrawing()
              EndIf
            EndIf
            
          Case #MA_BrushSize ; brush size
            SelectMA(#MA_BrushSize)
            s=(my-MA()\ly-4) / 4
            If s<>dWid And s>-1 And s<33
              
              ; update brush size indicator
              If StartDrawing(CanvasOutput(0))
                For i=0 To 1
                  Circle(MA()\lx+6,MA()\ly+dWid*4+6,4,bp(i*7))
                  dWid=s
                Next
                ;dBrushSize()
                StopDrawing()
              EndIf
            EndIf
            
          Case #MA_ToolStrip ; tool area
            ; draw highlight box to indicate current menu option
            selectMA(#MA_ToolStrip)
            x=(mx-MA()\lx) / 50
            y=(my-MA()\ly) / 50
            If x<0: x=0: EndIf
            If x>1: x=1: EndIf
            If y<0: y=0: EndIf
            If y>10: y=10: EndIf
            
            tHi=x+y*2
            
            If tOld<>tHi
              If StartDrawing(CanvasOutput(0))
                DrawingMode(#PB_2DDrawing_XOr+#PB_2DDrawing_Outlined)
                
                x=MA()\lx+(tHi % 2)*50+4
                y=MA()\ly+(tHi/2)*50+4
                
                Box(x,y,42,42,bp(1))
                
                If tOld>-1
                  x=MA()\lx+(tOld % 2)*50+4
                  y=MA()\ly+(tOld/2)*50+4
                  
                  Box(x,y,42,42,bp(1))
                  
                EndIf
                
                tOld=tHi               
                
                StopDrawing()
              EndIf           
            EndIf
          
            ; draw overlay menu
            If imgOverlay\img=0
              getBackground(700,200,50,200,imgOverlay)
            EndIf
                      
          Case #MA_PatSel ; pattern select2
            selectMA(#MA_PatSel)
            i=8-((my-MA()\ly-4) / 22)
            j=((mx-MA()\lx-4) / 32)
            If i<0: i=0: EndIf
            If i>8: i=8: EndIf
            If j<0: j=0: EndIf
            If j>17: j=17: EndIf
            If pCol<>i Or pSel<>j
              If StartDrawing(CanvasOutput(0))              
                ; highlight selected pattern and update selected brush size pattern
                Box(MA()\lx+pSel*32+4,MA()\ry-11-pCol*22+4,32,2,bp(0))
                pCol=i
                pSel=j
                Box(MA()\lx+pSel*32+4,MA()\ry-11-pCol*22+4,32,2,bp(7))
                
                updateBrush()
                
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
          Case #MA_Drawing ; drawing area
            
            Select dDSP
              Case 0 ; standard painting mode
                
                ; determine tool being used
                Select tCur
                  Case #tooldraw
                    If dsel=#toolBrushSPR
                      setP(mx,my)
                      dBrushSPR(px,py,dWid,0,0)
                    EndIf                
                  Case #toolLine ; line tool completion
                    selectMA(#MA_Drawing)
                    If dShift ; horizontal
                      dLine(sx+MA()\lx,sy+MA()\ly,ox+MA()\lx,sy+MA()\ly)
                    ElseIf dCtrl ; vertical
                      dLine(sx+MA()\lx,sy+MA()\ly,sx+MA()\lx,oy+MA()\ly)
                    Else ; any direction
                      dLine(sx+MA()\lx,sy+MA()\ly,ox+MA()\lx,oy+MA()\ly)
                    EndIf
                    
                  Case #toolCirOut ; polygon completion
                    dCircOut(sx,sy,Abs(sx-ox))
                    
                  Case #toolCirFil ; polygon completion
                    dCircle(sx,sy,Abs(sx-ox))
                    
                  Case #toolBoxOut,#toolBoxFil  ; boxes
                    dBox(sx,sy,ox,oy,tCur-#toolBoxOut)
                    
                  Case #toolGradHor,#toolGradVer  ; gradient
                    If sx<>ox And sy<>oy 
                      dBoxG(sx,sy,ox,oy,tCur-#toolGradHor)
                    EndIf
                    
                  Case #toolFill ; flood fill
                    selectMA(#MA_Drawing)
                    floodfill(mx-MA()\lx,my-MA()\ly)
                EndSelect
                
              Case 1 ; sprite editor mode
                
                If dx>199 And dx<221 And dy>239 And dy<277
                  ;left
                  dSED-1
                  If dSED>17 
                    dSED=17
                  EndIf
                  
                EndIf
                
                If dx>419 And dx<441 And dy>239 And dy<277
                  ;right
                  dSED+1
                  If dSED>17 
                    dSED=0
                  EndIf
                EndIf
                
                
            EndSelect
            
          Case #MA_ToolStrip ; tool select
            
            ; replace overlay menu
            If imgOverlay\img
              putBackground(imgOverlay)
            EndIf
            
            selectMA(#MA_ToolStrip)
            
            ; remove old highlight
            If tOld<>-1
              If StartDrawing(CanvasOutput(0))
                DrawingMode(#PB_2DDrawing_XOr+#PB_2DDrawing_Outlined)
                
                
                x=MA()\lx+(tOld % 2)*50+4
                y=MA()\ly+(tOld/2)*50+4
                  
                Box(x,y,42,42,bp(1))
                
                tOld=-1
                
                StopDrawing()
              EndIf           
            EndIf      
            
            
            ; get button of tool clicked And action
            x=(mx-MA()\lx) / 50
            y=(my-MA()\ly) / 50
            If x<0: x=0: EndIf
            If x>1: x=1: EndIf
            If y<0: y=0: EndIf
            If y>10: y=10: EndIf
            
            tSel=x+y*2
            
            If tSel>-1 And tSel<22
              Select tSel
                Case #toolSave ; save
                  opensave(1)
                Case #toolLoad ; load
                  saveUndo()                  
                  opensave(0)
                  tLIF=1
                Case #toolQSAll ; quick save all
                  tQSA=1-tQSA
                  tQSC=0
                  addToggle(tSel,tQSA*2)
                  addToggle(#toolQSCur,0)

                Case #toolQSCur ; quick save current
                  tQSC=1-tQSC
                  tQSA=0
                  addToggle(tSel,tQSC*2)
                  addToggle(#toolQSAll,0)

                Case #toolUndo ; undo
                  undo()
                Case #toolRedo ; redo
                  redo()                  
                  
                Case #toolCLS; CLS
                  saveundo()
                  For x=0 To #SCRNsize
                    dl(dLay)\SCRN[x]=0
                  Next                  
                Case 21; toggle transparency
                  dTrn=(dTrn+1) % 2
                  addToggle(tSel,dTrn*2)
                  
                Case #toolBrushBox,#toolBrushRound,#toolBrushSPR,#toolBrushRND,#toolBrush2x
                  If tSel<>dSel
                    addToggle(dSel,0)
                    dSel=tSel
                    addToggle(dSel,6)
                    
                    If StartDrawing(CanvasOutput(0))
                      updateBrush()
                      StopDrawing()
                    EndIf
                  EndIf
                  
                Default ; all other tools - should not be needed now
                  If tSel<>tCur
                    addToggle(tCur,0)
                    tCur=tSel
                    addToggle(tCur,tTog)
                  EndIf
              EndSelect
            EndIf
            
          Case #MA_Layers ; layers
            selectMA(#MA_Layers)
            i=(my-MA()\ly-20) / 32
            If i<0:i=0:EndIf
            If i>6:i=6:EndIf
            If mx-MA()\lx<23 ; select layer
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
              addToggle(#toolRedo,8)
            Else
              addToggle(#toolRedo,7)
            EndIf
            If ListSize(dl(dLay)\lUndo())=0
              addToggle(#toolUndo,8)
            Else
              addToggle(#toolUndo,7)
            EndIf
            
          Case #MA_SpriteShow ; set spr drawing flag
            If dDSP<>1
              dDSP=1
            Else
              dDSP=0
            EndIf
          Case #MA_OptionShow ; set options flag
            If dDSP<>2
              dDSP=2
            Else
              dDSP=0
            EndIf            
            
        EndSelect
        
        ; reset wire frame
        dWire=0 
        
        ; reset mouse action
        mact=-1
        
      EndIf 
      
      
      If mact<>#MA_PatSel ; skip screen update if palette is visible
        
        Select dDSP
            ;-------- Update Painting Screen
          Case 0 ; picture drawing mode
            
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
              
              ; draw cross hair
              If rangeApp(#MA_Drawing)
                selectMA(#MA_Drawing)
                Select oCrossHair
                  Case 0 ; thin
                    LineXY(mx-4,MA()\ly-4,mx-4,MA()\ry,RGB(63,63,63))
                    LineXY(MA()\lx-4,my-4,MA()\rx,my-4,RGB(63,63,63))
                  Case 1 ; thick
                    For x=-1 To 1
                      LineXY(mx-4+x,MA()\ly-4,mx-4+x,MA()\ry,RGB(63,63,63))
                      LineXY(MA()\lx-4,my-4+x,MA()\rx,my-4+x,RGB(63,63,63))
                    Next
                  Case 2 ; pixel
                    x=((mx-4) / 4)*4
                    y=((my-4) / 2)*2
                    
                    For i=0 To 3
                      LineXY(x+i,MA()\ly-4,x+i,MA()\ry,RGB(63,63,63))
                      If i<2
                        LineXY(MA()\lx-4,y+i,MA()\rx,y+i,RGB(63,63,63))
                      EndIf
                    Next
                EndSelect
              EndIf
              
              ; draw brush size guide or small circle
              If oMouseGuide
                Select tCur
                  Case #toolDraw,#toolLine ; brush and line draw
                    If dwid>3
                      ; drawsize guide
                      x=dx / dMpx-dWid / 2
                      y=dy / dMpy-dWid
                      x2=(x+dWid)*4
                      y2=(y+dWid*2)*2
                      x*4
                      y*2    
                      
                      LineXY(x,y,x+8,y,bp(2));RGB(63,63,63))
                      LineXY(x,y,x,y+8,bp(2));,RGB(63,63,63))
                      
                      LineXY(x2-4,y,x2+3,y,bp(2));RGB(63,63,63))
                      LineXY(x2+3,y,x2+3,y+8,bp(2));,RGB(63,63,63))
                      
                      LineXY(x,y2+1,x+8,y2+1,bp(2));RGB(63,63,63))
                      LineXY(x,y2-8,x,y2+1,bp(2))  ;,RGB(63,63,63))
                      
                      LineXY(x2-4,y2+1,x2+3,y2+1,bp(2));RGB(63,63,63))
                      LineXY(x2+3,y2-8,x2+3,y2+1,bp(2));,RGB(63,63,63))
                    Else
                      Circle(dx,dy,10,bp(2))
                    EndIf
                    
                    ; draw sprite
                    If tcur=#toolDraw And dSel=#toolBrushSPR
                      setP(mx,my)
                      DrawingMode(#PB_2DDrawing_Default)
                      dBrushSPR(px,py,dWid,0,1)
                      DrawingMode(#PB_2DDrawing_Outlined|#PB_2DDrawing_XOr)
                    EndIf
                EndSelect
              EndIf
            
              ; draw shape guides
              Select dWire
                Case #toolLine   ; line tool
                  If dShift      ; horizontal
                    LineXY(sx,sy,dx,sy,bp(7))                    
                  ElseIf dCtrl ; vertical
                    LineXY(sx,sy,sx,dy,bp(7))
                  Else ; any direction
                    LineXY(sx,sy,dx,dy,bp(7))
                  EndIf
                  
                  
                Case #toolCirOut,#toolCirFil ; polygon tool
                  Circle(sx,sy,Abs(sx-dx),bp(7))
                Case #toolBoxOut,#toolBoxFil,#toolGradHor,#toolGradVer  ; boxes, gradient
                  If dShift
                    Box(sx,sy,dx-sx,dx-sx,bp(7))
                  Else
                    Box(sx,sy,dx-sx,dy-sy,bp(7))
                  EndIf
              EndSelect
              
              StopDrawing()
            EndIf
            
            ;-------- Update Sprite Screen
          Case 1 ; sprite drawing mode
            If StartDrawing(ImageOutput(imgFinal))
              Box(0,0,640,512,bp(0))
              ;Box(100,100,100,100,bp(1))
              drawGrid()
              drawbox(200,240,220,276,bp(7))
              drawbox(420,240,440,276,bp(7))
              LineXY(218,242,202,258,bp(8))
              LineXY(218,274,202,258,bp(8))
              LineXY(422,242,438,258,bp(8))
              LineXY(422,274,438,258,bp(8))
              
              DrawText(300,400,"SPR: "+Str(dSED))
              
              StopDrawing()
            EndIf
            
            ;-------- Update Options Screen
          Case 2 ; options screen
            If StartDrawing(ImageOutput(imgFinal))
              Box(0,0,640,512,bp(0))
              drawBox(4,4,636,104,bp(7))
              DrawText(20,20,"ART for EXA (c) FourthStone")
              DrawText(20,40,"What's needed here is a fancy title, dimensions 636 x 100 (hint hint) ;-)")
              
              ; help panel
              drawbox(500,128,636,264,bp(8))
              DrawText(508,120,"Hot Keys",bp(7))
              For i=0 To 5
                If i<4
                  x=0
                Else
                  x=20
                EndIf
                DrawText(522+x,140+i*20,":",bp(2))
              Next
              
              DrawText(508,140,"R",bp(7))
              DrawText(532,140,"Colour Up",bp(7))
              
              DrawText(508,160,"F",bp(7))
              DrawText(532,160,"Colour Down",bp(7))
              
              DrawText(508,180,"I",bp(7))
              DrawText(532,180,"Pattern Up",bp(7))
              
              DrawText(508,200,"J",bp(7))
              DrawText(532,200,"Pattern Up",bp(7))
              
              DrawText(508,220,"Shift",bp(7))
              DrawText(552,220,"Horz Line",bp(7))
              
              DrawText(508,240,"Ctrl",bp(7))
              DrawText(552,240,"Vert Line",bp(7))
              
              ; cross hair options
              drawbox(MO(0)\lx-8,MO(0)\ly-12,MO(0)\lx+100,MO(0)\ly+124,bp(8))
              DrawText(MO(0)\lx,MO(0)\ly-20,"Cross Hair",bp(7))
              For i=0 To #maOpt
                drawbox(MO(i)\lx,MO(i)\ly,MO(i)\rx,MO(i)\ry,bp(7))
                DrawText(MO(i)\lx+28,MO(i)\ly+2,MO(i)\name,bp(3))
                
                Select i
                  Case 0,1,2,3
                    If oCrossHair=i
                      Box(MO(i)\lx+2,MO(i)\ly+2,17,17,bp(2))
                    EndIf
                  Case 4
                    If oMouseGuide
                      Box(MO(i)\lx+2,MO(i)\ly+2,17,17,bp(2))
                    EndIf
                    
                EndSelect
              Next
              
              StopDrawing()
            EndIf             
        EndSelect
        
        ; update drawing canvas
        If StartDrawing(CanvasOutput(0))
          
          ; update drawing area and stats
          selectMA(#MA_Drawing)
          DrawImage(ImageID(imgFinal),MA()\lx,MA()\ly)
          
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
        
      EndIf ; skip screen update if palette is visible
      
    EndIf ; ensure event is triggered from ART
    
  Else
    Delay(1) ; do nothing if event is not ours
  EndIf
  
    ExamineKeyboard()
    
    ; select colour
    If KeyboardReleased(#PB_Key_R)
      updateColSel(-1)
    EndIf  
    
    If KeyboardReleased(#PB_Key_F)
      updateColSel(1)
    EndIf  
    
    ; select pattern colour (background)
    If KeyboardReleased(#PB_Key_I)
      updatepCol(1)
    EndIf
    
    If KeyboardReleased(#PB_Key_J)
      updatepCol(-1)
    EndIf
    
    ; toggle drawing mode for stright lines and perfect shapes
    If KeyboardPushed(#PB_Key_LeftShift)
      dShift=1
    Else
      dShift=0
    EndIf
    
    ; toggle drawing mode for stright lines and perfect shapes
    If KeyboardPushed(#PB_Key_LeftControl)
      dCtrl=1
    Else
      dCtrl=0
    EndIf
    
    
    
Until Event = #PB_Event_CloseWindow

End


;-------- Data Section --------

DataSection
  
  ; mouse area Data (left x,y) (w,h)
  ; ensure to update maCount index to match number of mouse areas
  mouseData:
  Data.s "MA_Drawing"
  Data.w 4,4,640,512,1
  Data.s "MA_ColSel"
  Data.w 386,526,262,39,1
  Data.s "MA_PatternShow"
  Data.w 0,524,70,64,1
  Data.s "MA_BrushSize"
  Data.w 750,60,204,140,1
  Data.s "MA_Stats"
  Data.w 232,522,152,52,0
  Data.s "MA_SelectedPat"
  Data.w 750,460,44,44,0
  Data.s "MA_ToolStrip"
  Data.w 648,24,100,550,1
  Data.s "MA_FillCol"
  Data.w 750,526,44,44,0
  Data.s "MA_PatSel"
  Data.w 0,370,586,208,0
  Data.s "MA_Layers"
  Data.w 750,200,50,238,1 
  Data.s "MA_SpriteShow"
  Data.w 74,524,66,64,1
  Data.s "MA_OptionShow"
  Data.w 144,524,66,64,1
  
  ; options screen
  Data.s "MA_OptThin"
  Data.w 20,140,20,20,0
  Data.s "MA_OptThick"
  Data.w 20,164,20,20,0
  Data.s "MA_OptPixel"
  Data.w 20,188,20,20,0
  Data.s "MA_OptOff"
  Data.w 20,212,20,20,0
  Data.s "MA_OptGuide"
  Data.w 20,236,20,20,0
  
  ; sprite screen
  ;...
  
  
  ; this entry flags the end of the mouse area data
  Data.s "DATAEND"
  
  ; options view mouse area data
  optionsData:
  Data.s "Thin"
  Data.w 20,140
  Data.s "Thick"
  Data.w 20,164
  Data.s "Pixel"
  Data.w 20,188
  Data.s "Off"
  Data.w 20,212
  Data.s "Guide"
  Data.w 20,236
  
  
  
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
  Data.a 1,1,1,1,1,1,1,1,0,0,0,0,0,0,0,0,1,1,1,1,1,1,1,1,0,0,0,0,0,0,0,0,1,1,1,1,1,1,1,1,0,0,0,0,0,0,0,0,1,1,1,1,1,1,1,1,0,0,0,0,0,0,0,0
  Data.a 2,2,2,2,2,2,2,2,0,0,0,0,0,0,0,0,2,2,2,2,2,2,2,2,0,0,0,0,0,0,0,0,2,2,2,2,2,2,2,2,0,0,0,0,0,0,0,0,2,2,2,2,2,2,2,2,0,0,0,0,0,0,0,0
  Data.a 3,3,3,3,3,3,3,3,0,0,0,0,0,0,0,0,3,3,3,3,3,3,3,3,0,0,0,0,0,0,0,0,3,3,3,3,3,3,3,3,0,0,0,0,0,0,0,0,3,3,3,3,3,3,3,3,0,0,0,0,0,0,0,0
  Data.a 4,4,4,4,4,4,4,4,0,0,0,0,0,0,0,0,4,4,4,4,4,4,4,4,0,0,0,0,0,0,0,0,4,4,4,4,4,4,4,4,0,0,0,0,0,0,0,0,4,4,4,4,4,4,4,4,0,0,0,0,0,0,0,0
  Data.a 5,5,5,5,5,5,5,5,0,0,0,0,0,0,0,0,5,5,5,5,5,5,5,5,0,0,0,0,0,0,0,0,5,5,5,5,5,5,5,5,0,0,0,0,0,0,0,0,5,5,5,5,5,5,5,5,0,0,0,0,0,0,0,0
  Data.a 6,6,6,6,6,6,6,6,0,0,0,0,0,0,0,0,6,6,6,6,6,6,6,6,0,0,0,0,0,0,0,0,6,6,6,6,6,6,6,6,0,0,0,0,0,0,0,0,6,6,6,6,6,6,6,6,0,0,0,0,0,0,0,0
  Data.a 7,7,7,7,7,7,7,7,0,0,0,0,0,0,0,0,7,7,7,7,7,7,7,7,0,0,0,0,0,0,0,0,7,7,7,7,7,7,7,7,0,0,0,0,0,0,0,0,7,7,7,7,7,7,7,7,0,0,0,0,0,0,0,0
  Data.a 4,0,4,0,4,0,4,0,4,0,4,0,4,0,4,0,4,0,4,0,4,0,4,0,4,0,4,0,4,0,4,0,4,0,4,0,4,0,4,0,4,0,4,0,4,0,4,0,4,0,4,0,4,0,4,0,4,0,4,0,4,0,4,0
  Data.a 3,3,3,3,1,1,1,1,1,1,1,1,3,3,3,3,3,3,3,3,1,1,1,1,1,1,1,1,3,3,3,3,3,3,3,3,1,1,1,1,1,1,1,1,3,3,3,3,3,3,3,3,1,1,1,1,1,1,1,1,3,3,3,3
  Data.a 0,3,3,3,0,3,3,0,3,0,0,0,3,0,0,3,3,0,6,0,3,0,6,3,3,0,0,0,3,0,0,3,0,3,3,3,0,3,3,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
  Data.a 0,1,2,3,4,5,6,7,0,0,0,0,0,0,0,0,1,2,3,4,5,6,7,0,0,0,0,0,0,0,0,0,2,3,4,5,6,7,0,1,0,0,0,0,0,0,0,0,3,4,5,6,7,0,1,2,0,0,0,0,0,0,0,0
  Data.a 0,1,1,0,0,1,1,0,1,0,0,1,1,0,0,1,0,1,1,0,0,1,1,0,1,0,0,1,1,0,0,1,0,1,1,0,0,1,1,0,1,0,0,1,1,0,0,1,0,1,1,0,0,1,1,0,1,0,0,1,1,0,0,1
  Data.a 1,1,1,1,1,1,1,1,1,0,0,0,0,0,0,1,1,0,0,0,0,0,0,1,1,0,0,0,0,0,0,1,1,0,0,0,0,0,0,1,1,0,0,0,0,0,0,1,1,0,0,0,0,0,0,1,1,1,1,1,1,1,1,1
  Data.a 0,0,0,0,0,0,0,0,0,2,2,2,2,2,2,0,0,2,0,0,0,0,2,0,0,2,0,0,0,0,2,0,0,2,0,0,0,0,2,0,0,2,0,0,0,0,2,0,0,2,2,2,2,2,2,0,0,0,0,0,0,0,0,0
  Data.a 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,3,3,3,3,0,0,0,0,3,0,0,3,0,0,0,0,3,0,0,3,0,0,0,0,3,3,3,3,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
  
EndDataSection


; IDE Options = PureBasic 5.62 (Windows - x86)
; CursorPosition = 1901
; FirstLine = 1875
; Folding = --------
; EnableXP
; UseIcon = Art-icon.ico
; Executable = ART_EXA_PB_014_x86.exe
; DisableDebugger
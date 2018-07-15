;
;
; ------------------------------------------------------------
;
;   ART 4 Windows
;
;    (c) Fourthstone
;
; ------------------------------------------------------------
;

; TODO 16/07/2018
;1) Ability To draw colour cycles along a straight line Or plot out a cycle.........As they tend To be Not very accurate when I use the mouse
; * implement all beeb modes - potentially other 8bit computer formats...
; * Partial complete - Flashing colour support
; * zoom at cursor
; * enhanced gradient fills eg:
; ** draw shape Or fill With directional line For gradient
; ** multi colour gradient for more than 2 colours
; * Pattern drawing, custom sprites:
; ** pattern style where the pattern is fixed to the drawing grid
; ** sprite style where bits of graphics can be drawn in any x,y location
; ** sprites stored as png files
; * Layers, not sure how this will work, needs more thought
; 

; colour map, flashing colour default is 540ms
; 0   black
; 1	  red
; 2	  green
; 3   yellow
; 4	  blue
; 5	  magenta
; 6	  cyan
; 7	  white
; 8	  flashing black-white
; 9	  flashing red-cyan
; 10	flashing green-magenta
; 11	flashing yellow-blue
; 12	flashing blue-yellow
; 13	flashing magenta-green
; 14	flashing cyan-red
; 15	flashing white-black

; png file support
UsePNGImageEncoder()
UsePNGImageDecoder()

; constants
#scrW=960
#scrH=704
#drwW=640
#drwH=512
#maxUndo=20
#SCRNsize=163839 ; screen buffer array size
#RAWsize=20480

; tool strip constants
#toolSave=0
#toolLoad=1
#toolUndo=2
#toolRedo=3
#toolDraw=4
#toolLine=5
#toolCirOut=6
#toolCirFil=7
#toolBoxOut=8
#toolBoxFil=9
#toolFill=10
#toolGradient=11
#toolGradHor=12
#toolGradVer=13
#toolAniDraw=14
#toolAnimate=15
#toolAniPing=16
#toolAniCycle=17
#toolTransparent=18
#toolCLS=19
#toolBrushBox=20
#toolBrushRound=21
#toolBrushSlash=22
#toolBrushRND=23
#toolBrush2x=24
#toolBrushUnused=25
#toolAniExport=26
#toolQS=27

; structures
Structure MouseArea
  lx.w
  ly.w
  rx.w
  ry.w
  name.s
  img.a
  gad.w
EndStructure

Structure beebModes
  dx.w ; image horizontal pixels
  dy.w ; image vertical pixels
  px.w ; horizontal pixel size
  py.w ; vertical pixel size
  wx.w ; draw width horizontal
  wy.w ; draw width vertical
  cc.w ; palette colour count
EndStructure

Structure fillStack
  x.w
  y.w
EndStructure  

Structure ToggleList
  button.a
  colour.a
EndStructure

Structure Pixel
  Pixel.l
EndStructure

Structure rgbTable
  r.a
  g.a
  b.a
EndStructure

Structure undoArray
  buf.a[#SCRNsize+1]
EndStructure


; globals
Global pCol.a=0 ; pattern colour selected
Global pSel.a=8 ; pattern selected

Global dCol.a=1 ; drawing colour
Global dTrn.a=1 ; drawing transparency toggle
Global dWid.a=8 ; drawing width
Global dSel.a=#toolBrushBox; drawing tool selected
Global dWire.a=0           ; draw wireframe 
Global dFil.i=0            ; fill colour
Global dMdx=160            ; current mode horizontal pixels
Global dMdy=256            ; current mode vertical pixels
Global dMpx=#drwW/dMdx     ; current mode horizontal pixel size
Global dMpy=#drwh/dMdy     ; current mode vertical pixel size

Global tCur.a=#toolDraw ; tool selected
Global tTog.a=3         ; tool toggle colour
Global tQSv.a=0         ; quick save toggle
Global tSel.a=0         ; new tool select 
Global gCur.w=0         ; current gadget id

Global maCount.a=10 ; mouse area count 0-n
Global mx,my,ox,oy,sx,sy,mact ; mouse x,y,action
Global fox,foy                ; flashing ox,oy
Global iBeebSCRN, imgToolStrip; image handles
Global flashing.b=0           ; flashing colour toggle
Global flashBak=8             ; flashing colour index background
Global flashFor=7             ; flashing colour index foreground
Global flashCol=8             ; flashing index for drawing
Global flashAnim=0            ; flash animation toggle
Global flashCycle=8           ; flash cycle counter
Global fSpeed=540             ; flash speed in ms
Global drawFlag=0             ; redraw buffer flag
Global animExport=-1          ; export animation frames
Global animFile.s=""          ; export filename
Global animSave=0             ; frame save flag
Global animType=0             ; animation selector
Global animDir=1              ; animation direction
Global savePattern=0          ; save file pattern selector
Global loadPattern=0          ; load file pattern selector

Global Dim bSCRN.a(#SCRNsize) ; screen buffer
Global Dim pat.a(17,15)       ; drawing patterns
Global Dim bp(15)             ; beeb palette
Global Dim rgbT.rgbTable(15)  ; rgb lookup table
Global Dim ct(15)             ; colour table look for redrawing main canvas
Global Dim bpFlash(15)        ; flash palette 0-7 phase 1, 8-15 phase 2
Global Dim mode.beebModes(6)  ; beeb mode data
Global Dim MA.MouseArea(maCount) ; mouse area structure array
Global Dim rawBBC.a(15)          ; raw bbc file format data
Global Dim revBBC.a(85)          ; reverse lookup bbc file format data

Global NewList lUndo.undoArray()
Global NewList lRedo.undoArray()

;Global NewList lUndo2.undoArray()

Global NewList lToggle.togglelist()

Global NewList lFS.fillStack()


; Exit program and display a message
Procedure Exit_ART(m.s)
  If m<>"" 
    MessageRequester("Error", m, #PB_MessageRequester_Info)
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
  x=MA(4)\lx+30
  y=MA(4)\ly
  
  ; MA: 756,164,204,200
  ; Gadget x: 648 ,  y: 0
  
  Box(x,y+1,90,198,bp(0))
  
  DrawText(x,y,Str(mx))
  DrawText(x,y+16,Str(my))
  DrawText(x,y+32,Str(px(mx)))
  DrawText(x,y+48,Str(py(my)))
  
  DrawText(x,y+80,Str(mact))
  
  DrawText(x,y+96,Str(tCur))
  DrawText(x,y+112,Str(tSel))
  DrawText(x,y+128,Str(gCur))
  DrawText(x,y+144,Str(dCol))
  
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
  If gCur=MA(i)\gad And mx>=MA(i)\lx And mx<=MA(i)\rx And my>=MA(i)\ly And my<=MA(i)\ry
    r=1
  EndIf
  ProcedureReturn r
EndProcedure

; drawing brush - generic routine
Procedure drawBrush(dx,dy,w,d)
  Protected lx,ly,dc,s,p
  
  Select d
    Case 1 ; x2 brush
      dx=((dx-w / 2) / 2)*2
      dy=((dy-w) / 2)*2
      s=1
      
    Default ; REM standard brush
      dx=dx-w / 2
      dy=dy-w
      s=0
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
          Select d
            Case 1 ; x2 brush
              pS=(lx / 2) % 4+((ly / 2) % 4)*4
            Default
              pS=lx % 4+(ly % 4)*4
          EndSelect
          If pat(pSel,pS)
            dc=pCol
          Else
            dc=dCol
          EndIf
          
          ; check For transparency
          If dc-dTrn>-1 Or (pat(pSel,pS)=1 And dCol=0)
            bSCRN(ly*640+lx)=dc
          EndIf
        EndIf
      EndIf
      ly+s
    Next
    lx+s
  Next
  
EndProcedure

; Draw brush size
Procedure dBrushSize()
  
  Protected b,lx,ly,px,py,pS,dc
  
  ; get pixel coords
  px=((MA(3)\lx+138) / 4)-dWid / 2-1
  py=((MA(3)\ly+68) /2)-dWid
  
  Box(MA(3)\lx+68,MA(3)\ly+4,132,130,bp(0))
  
  b=0
  
  ;draw pattern loop
  For lx=0 To dWid
    For ly=0 To dWid*2
      pS=(px+lx) % 4+((py+ly) % 4)*4
      If pat(pSel,pS)
        dc=pCol
        If pCol<>0: b=1: EndIf
      Else
        dc=dCol
        If dCol<>0: b=1: EndIf
      EndIf
      Box((px+lx)*4,(py+ly)*2,4,2,bp(dc))
    Next
  Next
  If b=0
    drawbox(px*4,py*2,px*4+dWid*4+3,py*2+dWid*4+1,bp(8))
  EndIf
EndProcedure

; Draw box with current brush
Procedure updateBrush()
  Protected bx,by,lx,ly,dc
  
  bx=MA(5)\lx
  by=MA(5)\ly
  
  ;draw pattern loop
  For lx=0 To 15
    For ly=0 To 31
      pS=(bx+lx) % 4+((by+ly) % 4)*4
      If pat(pSel,pS) 
        dc=pCol
      Else
        dc=dCol
      EndIf
      Box(bx+lx*4+4,by+ly*2+4,4,2,bp(dc))
    Next
  Next
  
  ; highlight selected pattern and update selected brush size pattern
  Box(MA(2)\lx+pSel*32,MA(2)\ly+174-pCol*22,32,2,bp(7))
  
EndProcedure

; update palette, assumes startdrawing is already active
Procedure updatePalette()
  Protected i,p,x,dc
  
  dc=bp(1)
  For i=0 To 7        ; colour loop 0 = bottom - 7 = top
    For p=0 To 17     ; pattern number loop left to right
      For x=0 To 15   ; pattern element loop, draws for pixels per element
        If pat(p,x)=1 ; get current pattern element to determine colour
          dc=bp(i)
        Else
          If i=0 And dCol=0
            dc=bp(8)
          Else
            dc=bp(dCol % 8)
          EndIf
        EndIf
        
        ; plot 4 pixels for this element offset 32 * 16 (+4 gap vertically)
        Box(2+p*32+(x % 4)*4,4+156-i*22+(x / 4)*2,4,2,dc)
        Box(2+p*32+(x % 4)*4,4+164-i*22+(x / 4)*2,4,2,dc)
        Box(2+16+p*32+(x % 4)*4,4+156-i*22+(x / 4)*2,4,2,dc)
        Box(2+16+p*32+(x % 4)*4,4+164-i*22+(x / 4)*2,4,2,dc)
      Next
    Next
  Next
  updateBrush()
  
EndProcedure

; draw vertical line with current pattern 1 pixel wide
Procedure Vline2(x1,y1,y2)
  Protected ly,dc,pS,x1M
  
  ; validate x is in range before starting
  If x1>-1 And x1<dMdx
    x1M=x1 % 4
    If y1>y2:Swap y1,y2:EndIf
    For ly=y1 To y2
      ; range check, set pattern colour And plot
      If ly>-1 And ly<dMdy
        pS=x1M+(ly % 4)*4
        If pat(pSel,pS) 
          dc=pCol
        Else
          dc=dCol
        EndIf
        
        ; Check For transparency
        If dc-dTrn>-1 Or (pat(pSel,pS)=1 And dCol=0)
          bSCRN(ly*640+x1)=dc
        EndIf
      EndIf
    Next
  EndIf
EndProcedure

; draw horizontal line with current pattern 1 pixel high
Procedure Hline2(x1,x2,y1)
  Protected lx,dc,pS,y1M
  
  y1M=(y1 % 4)*4
  For lx=x1 To x2
    ; range check, set pattern colour And plot
    If lx>-1 And lx<dMdx And y1>-1 And y1<dMdy
      pS=lx % 4+y1M
      If pat(pSel,pS) 
        dc=pCol
      Else
        dc=dCol
      EndIf
      
      ; Check For transparency
      If dc-dTrn>-1 Or (pat(pSel,pS)=1 And dCol=0)
        bSCRN(y1*640+lx)=dc
      EndIf
    EndIf
  Next
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
  drawBrush(x1,y1,dWid,0)
  
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
Procedure drawCircle(x1,y1,r)
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
      Case #toolBrushRND ; airbrush
        drawBrush(px(Int(x1+r*Cos(Radian(t)))),py(Int(y1-r*Sin(Radian(t)))),dWid,2)
      Case #toolBrush2x ; standard X2 brush
        drawBrush(px(Int(x1+r*Cos(Radian(t)))),py(Int(y1-r*Sin(Radian(t)))),dWid,1)
      Default
        drawCircle(x1+r*Cos(Radian(t)),y1-r*Sin(Radian(t)),dWid*2)        
        
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
      If lx>-1 And lx<dMdx And ly>-1 And ly<dMdy 
        pS=lx % 4+(ly % 4)*4
        If pat(Int(gR),pS)
          dc=pCol
        Else
          dc=dCol
        EndIf
        
        ; Check For transparency
        If dc-dTrn>-1
          bSCRN(ly*640+lx)=dc
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
Procedure drawColSel(i,c)
  x=i/8*28
  i=i % 8
  y=i*22  
  drawBox(MA(1)\lx+x,MA(1)\ry-y-22,MA(1)\lx+30+x,MA(1)\ry-y+1,bp(c))
EndProcedure

; update colour select value
Procedure updateColSel(c.b)
  If StartDrawing(CanvasOutput(0))    
    drawColSel(dCol,0)
    dCol-c
    If dcol=255:dCol=15:EndIf
    If dcol=16:dCol=0:EndIf
    drawColSel(dcol,7)
    updatePalette()
    StopDrawing()
  EndIf
  If StartDrawing(CanvasOutput(1))
    dBrushSize()
    StopDrawing()
  EndIf  
EndProcedure

; update flash colours
Procedure updateFlashColours(m)
  
  If m=9
    c=flashBak
  Else
    c=flashFor
  EndIf  
  
  ;If fc<0:fc=8:EndIf
  
  Box(MA(m)\lx+2,MA(m)\ly+2,MA(m)\rx-MA(m)\lx-3,MA(m)\ry-MA(m)\ly-3,bp(c))
  
  If c=8 
    DrawingMode(#PB_2DDrawing_Transparent)
    DrawText(MA(m)\lx+6,MA(m)\ly+2,"D",bp(7))
    DrawingMode(#PB_2DDrawing_Default)
  EndIf
  
EndProcedure

; update highlight for flash colour selector
Procedure drawFlashSel(m)
  i=8-(my-MA(m)\ry-2) / 22
  If m=9
    c=flashBak
  Else
    c=flashFor
  EndIf
  
  If c<>i And i>-1 And i<9
    If StartDrawing(CanvasOutput(1))    
      DrawText(110,324,Str(i),bp(7))
      DrawText(110,340,Str(c),bp(7))
      
      If c>-1 And c<9
        drawBox(MA(m)\lx+1,MA(m)\ry+3+(8-c)*22,MA(m)\lx+30,MA(m)\ry+25+(8-c)*22,bp(0))
      EndIf
      c=i
      
      drawBox(MA(m)\lx+1,MA(m)\ry+3+(8-c)*22,MA(m)\lx+30,MA(m)\ry+25+(8-c)*22,bp(7))
      If m=9
        flashBak=c
      Else
        flashFor=c
      EndIf
      
      updateFlashColours(m)
      StopDrawing()
      
    EndIf
  EndIf                    
  
EndProcedure

; update flash speed
Procedure updateFlashSpeed(t)
  
  y=t / 10
  If Y>100
    y=100
  EndIf
  
  Box(MA(8)\lx+1,MA(8)\ly+1,57,127,bp(0))
  Box(MA(8)\lx+24,MA(8)\ly+2,10,104,bp(1))
  LineXY(MA(8)\lx+28,MA(8)\ly+6,MA(8)\lx+28,MA(8)\ly+102,bp(7))
  Box(MA(8)\lx+11,MA(8)\ly+y,36,8,bp(7))
  Box(MA(8)\lx+13,MA(8)\ly+y+2,32,4,bp(8))
  DrawText(MA(8)\lx+4,MA(8)\ly+112,Right("00"+StrU(fSpeed),4)+"ms",bp(7))
EndProcedure

; draw flash colour picker menu
Procedure pickFlashColour(m)
  If StartDrawing(CanvasOutput(1))
    Box(MA(m)\lx,MA(m)\ry+2,32,202,bp(0))
    drawbox(MA(m)\lx,MA(m)\ry+2,MA(m)\lx+31,MA(m)\ry+203,bp(15))
    For i=1 To 8
      Box(MA(m)\lx+4,MA(m)\ry-16+i*22,24,18,bp(9-i))
    Next  
    StopDrawing()
  EndIf
EndProcedure


; drawing brush - flashing colour routine
Procedure flashBrush(dx,dy)
  
  ; range check, set pattern colour And plot
  If dx>-1 And dx<dMdx And dy>-1 And dy<dMdy
    If fox<>dx Or foy<>dy
      bSCRN(dy*640+dx)=flashCol
      flashCol+1
      If flashCol>15
        flashCol=8
      EndIf
      fox=dx
      foy=dy
    EndIf
  EndIf
  
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
        
        If bSCRN(y*640+x)=mc ;Point(x*dMpx,y*dMpy)=mc
          uf=1 : df=1
          
          ; scan left
          While x>0 And bSCRN(y*640+x-1)=mc;Point((x-1)*dMpx,y*dMpy)=mc
            x-1
          Wend
          
          ; scan right and plot fill colour
          While x<dMdx And bSCRN(y*640+x)=mc ;Point(x*dMpx,y*dMpy)=mc
            If i
              pS=x % 4+(y % 4)*4
              If pat(pSel,pS)
                dc=pCol
              Else
                dc=dCol
              EndIf
              
            EndIf
            
            bSCRN(y*640+x)=dc
            ;Box(x*dMpx,y*dMpy,dMpx,dMpy,bp(dc))
            
            ; detect colour changes above and add To List
            If y<(dMdy-1)
              c=bSCRN((y+1)*640+x);Point(x*dMpx,(y+1)*dMpy)
              If uf And c=mc
                AddElement(lFS())
                lFS()\x=x : lFS()\y=y+1
                uf=0
              EndIf
              If c<>mc : uf=1 : EndIf
            EndIf
            
            ; detect colour changes below and add To List
            If y>0
              c=bSCRN((y-1)*640+x);Point(x*dMpx,(y-1)*dMpy)
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
Procedure toolToggle(i,col)
  Protected x,y,lx,ly
  
  ; default or erase button colour is 7
  If col=0: col=7: EndIf
  x=MA(6)\lx+(i % 2)*50+4
  y=MA(6)\ly+(i/2)*50+4
  
  ; scan icon and update non black areas to new colour
  If x>-1 And (x+41)<#scrW And y>-1 And (y+41)<#scrH
    For lx=x To x+41
      For ly=y To y+41
        If Point(lx,ly)<>bp(0): Plot(lx,ly,bp(col)): EndIf
      Next
    Next
  EndIf
  
  ; drawbox(x,y,x+41,y+41,bp(2))
  
EndProcedure

; add a tool toggle item to the tool toggle draw list
; all items in list will be drawn at end of main loop
Procedure addToggle(b,c)
  AddElement(lToggle())
  ltoggle()\button=b
  ltoggle()\colour=c
EndProcedure

; save transparent PNG
Procedure saveTransparent(filename.s,anim.i)
  Protected imgFinal
  
  imgFinal=CreateImage(#PB_Any,640,512,32)
  
  ; copy output buffer to final image
  If StartDrawing(ImageOutput(imgFinal))
    Box(0,0,640,512,bp(0))          
    Buffer      = DrawingBuffer()             ; Get the start address of the screen buffer
    Pitch       = DrawingBufferPitch()        ; Get the length (in byte) took by one horizontal line
    PixelFormat = DrawingBufferPixelFormat()  ; Get the pixel format. 
    
    ; configure palette for RGB or BGR
    If PixelFormat = #PB_PixelFormat_32Bits_RGB
      If anim
      Else
        
        For i=1 To 16
          ct(i)=RGBA(rgbT(i)\r,rgbT(i)\g,rgbT(i)\b,255)
        Next
        
      EndIf
      ct(0)=RGBA(0,0,0,0);rgbT(i)\r,rgbT(i)\g,rgbT(i)\b,255)
    Else                 ; Else it's 32bits_BGR
      If anim
        
      Else
        
        For i=1 To 16
          ct(i)=RGBA(rgbT(i)\b,rgbT(i)\g,rgbT(i)\r,255)
        Next
        
      EndIf
      ct(0)=RGBA(0,0,0,0)
    EndIf
    
    For y = 0 To 511 
      *Line.Pixel = Buffer+Pitch*y
      yMul=(y/2)*640
      
      For x=0 To 159
        dc = ct(bSCRN(x+yMul))
        
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
  FreeImage(imgFinal)
  
EndProcedure



; open save handler
Procedure openSave(mode)
  
  Protected lastFN.s, filename.s, action.s, F.s, ff.s, N.w, iTMP, a.b,b.a
  
  ff = "PNG files (*.PNG)|*.PNG|BBC files (*.*)|*.*"
  
  If mode=1 ; save dialog
    If tQSv=0
      ;  ff = "BMP files"+Chr(0)+"*.BMP"+Chr(0)+Chr(0)+"PNG files"+Chr(0)+"*.PNG"+Chr(0)
      Repeat
        ok=#PB_MessageRequester_Yes
        
        ; show save dialog and prompt if file exists
        filename=SaveFileRequester(" Save Image",GetCurrentDirectory(),ff,savePattern)
        savePattern=SelectedFilePattern()
        If filename
          Select savePattern
            Case 0 ; png
              If Right(UCase(filename),4)<>".PNG"
                filename=filename+".PNG"
              EndIf
              
            Case 1 ; bbc raw
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
    
  Else ; get filename to load
    filename=OpenFileRequester(" Load Image",GetCurrentDirectory(),ff,loadPattern)
    loadPattern=SelectedFilePattern()
  EndIf
  
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
                  
                  For x=0 To 159
                    For i=0 To 7
                      If *Line\Pixel=RGBA(rgbT(i)\b,rgbT(i)\g,rgbT(i)\r,255)
                        bSCRN(x+yMul)=i
                        Break
                      EndIf
                      
                    Next
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
                
                If ReadFile(0, filename)
                  ReadData(0,*MemoryID,#RAWsize)
                  CloseFile(0)
                  
                  x=0
                  y=255
                  
                  For i=0 To #RAWsize-1
                    a=(PeekB(*MemoryID+i) & 170)>>1
                    b=PeekB(*MemoryID+i) & 85
                    
                    bSCRN(x+y*640)=revBBC(a)
                    bSCRN(x+1+y*640)=revBBC(b)
                    
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
        Select savePattern
          Case 0 ; save png file
            action$="saved"
            
            SaveTransparent(filename,0)
            
          Case 1 ; save bbc raw
                 ; create temp buffer and fill with screen data
            *MemoryID = AllocateMemory(#RAWsize)       ; allocate 20k memory block
            If *MemoryID
              x=0
              y=255
              For i=0 To #RAWsize-1
                a=rawBBC(bSCRN(x+y*640))<<1
                b=rawBBC(bSCRN(x+1+y*640))
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

; open save handler
Procedure exportAnim()
  
  Protected lastFN.s, filename.s, action.s, F.s, ff.s, N.w, iTMP
  
  ff = "PNG files (*.PNG)|*.PNG"
  
  Repeat
    ok=#PB_MessageRequester_Yes
    
    ; show save dialog and prompt if file exists
    filename=SaveFileRequester(" Export Animation As PNG",GetCurrentDirectory(),ff,0)
    If filename<>""
      If Right(UCase(filename),4)=".PNG"
        filename=Left(filename,Len(filename)-4)
      EndIf
      filename+"00.PNG"
      
      If FileSize(filename)>-1
        ok=MessageRequester(" Confirm File Overwrite",GetFilePart(filename)+" already exists. Do you want to replace it?",#PB_MessageRequester_YesNo|#PB_MessageRequester_Info)
      EndIf
    EndIf
  Until ok=#PB_MessageRequester_Yes
  
  
  ; action file operation
  If filename<>""
    animFile=Left(filename,Len(filename)-6)
  EndIf
  
  ProcedureReturn 1
EndProcedure


; save undo
Procedure saveUndo()
  ; remove first item in list if undo buffer full
  If ListSize(lUndo())>#maxUndo
    FirstElement(lUndo())
    ;FreeImage(lUndo())
    DeleteElement(lUndo())
    LastElement(lUndo())
  EndIf
  
  ; add a new backup array to undo buffer
  AddElement(lUndo())
  
  For i=0 To #SCRNsize
    lUndo()\buf[i]=bSCRN(i)
  Next
  
  If ListSize(lUndo())=1
    addToggle(#toolUndo,7)
  EndIf
  
EndProcedure

; save redo
Procedure saveRedo()
  ; add a new backup array to redo buffer
  AddElement(lRedo())
  
  For i=0 To #SCRNsize
    lRedo()\buf[i]=bSCRN(i)
  Next  
  If ListSize(lRedo())=1
    addToggle(#toolRedo,7)
  EndIf
EndProcedure

; restore undo image
Procedure Undo()
  If ListSize(lUndo())
    ; create redo image first
    saveRedo()
    
    ; update drawing array with undo data
    For i=0 To #SCRNsize
      bSCRN(i)=lUndo()\buf[i]
    Next
    
    ; discard undo array from list
    DeleteElement(lUndo())
    If ListSize(lUndo())=0
      addToggle(#toolUndo,8)
    EndIf
  EndIf
EndProcedure

; restore redo image
Procedure Redo()
  If ListSize(lRedo())
    ; create undo image first
    saveUndo()
    
    ; update drawing array with redo data
    For i=0 To #SCRNsize
      bSCRN(i)=lRedo()\buf[i]
    Next    
    ; discard redo array from list
    DeleteElement(lRedo())
    
    If ListSize(lRedo())=0
      addToggle(#toolRedo,8)
    EndIf
    
  EndIf
EndProcedure


; --- End Procedures ---


; initialization block

;-------- Init and Load --------

If InitSprite() = 0 
  Exit_ART("Cannot init Sprite subsystem!")
EndIf

If InitMouse() = 0 
  Exit_ART("Cannot init Mouse subsystem!")
EndIf

If InitKeyboard() = 0 
  Exit_ART("Cannot init Keyboard subsystem!")
EndIf

If OpenWindow(0,0,0,#scrW, #scrH, "ART for Windows 0.1",#PB_Window_SystemMenu | #PB_Window_ScreenCentered) = 0 
  Exit_ART("Cannot init Graphics subsystem! ("+StrU(#scrW,#PB_Long)+"x"+StrU(#scrh,#PB_Long)+")") ;#scrW #scrh
EndIf


; read pattern data
Restore patternData
For i=0 To 17
  For h=0 To 15
    Read.a pat(i,h)
  Next
Next

; define beeb 2 palette
Restore paletteData
For i=1 To 15
  Read.a rgbT(i)\r
  Read.a rgbT(i)\g
  Read.a rgbT(i)\b
  bp(i)=RGBA(rgbT(i)\r,rgbT(i)\g,rgbT(i)\b,255)
Next

bp(0) = RGB(0,0,0)
rgbT(0)\r=0
rgbT(0)\g=0
rgbT(0)\b=0

; flashing palette - pointer to BP colour
For i=0 To 7
  bpFlash(i)=i  
  bpFlash(i+8)=7-i
Next

Font1 = LoadFont(#PB_Any, "Arial"  ,  7)

; For i=0 To #SCRNsize
;   If i<16 
;     bSCRN(i)=i % 15
;     EndIf
; Next


; define beeb modes
Restore modeData
For i=0 To 6
  Read.w mode(i)\dx
  Read.w mode(i)\dy
  Read.w mode(i)\px
  Read.w mode(i)\py
  Read.w mode(i)\wx
  Read.w mode(i)\wy
  Read.w mode(i)\cc
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
  Read.w MA(i)\gad
  
  ;MessageRequester("Debug",MA(i)\name+" "+StrU(MA(i)\gad))
Next

Restore rawFileData
For i=0 To 15
  Read.a rawBBC(i)
  revBBC(rawBBC(i))=i
Next  

; toolstrip image
;imgToolStrip=LoadImage(#PB_Any,"ToolStrip.bmp")
;If imgToolStrip=0
;  Exit_ART("Cannot load ToolStrip.bmp")
;EndIf
imgToolStrip=CatchImage(#PB_Any,?ToolStripMain)

; main drawing area
iBeebSCRN=CreateImage(#PB_Any,#drwW,#drwH,32)

If StartDrawing(ImageOutput(iBeebSCRN))
  Box(0,0,639,511,bp(0))
  StopDrawing()
EndIf

; buffered drawing area
If OpenWindowedScreen(WindowID(0), MA(0)\lx,MA(0)\ly, 640, 512)=0
  Exit_ART("Cannot init main canvas windowed screen object!")
EndIf

; init screen gadgets and initial state
CanvasGadget(0,0,520,648,#scrH-520) ; pallete gadget
SetGadgetAttribute(0, #PB_Canvas_Cursor , #PB_Cursor_Cross)

CanvasGadget(1,648,0,#scrW-648,#scrH) ; tool gadget
SetGadgetAttribute(1, #PB_Canvas_Cursor , #PB_Cursor_Cross)

CanvasGadget(2,0,0,648,520) ; drawing gadget (never drawn to)
SetGadgetAttribute(2, #PB_Canvas_Cursor , #PB_Cursor_Cross)

mact=-1

AddWindowTimer(0,0,fSpeed)


newSPR(32,32)

;
;-------- Draw controls --------
;

; ****** NOTE *******
; All coords need to change to align with each canvas gadget

; canvas 0 - palette
If StartDrawing(CanvasOutput(0))
  ; clear canvas to black
  Box(0,0,648,#scrH-520,bp(0))
  
  updatePalette()
  ;584,524,64,178
  ; draw colour select boxes
  For i=1 To 7
    Box(MA(1)\lx+4,MA(1)\ry-i*22-19,24,18,bp(i))
    Box(MA(1)\lx+32,MA(1)\ry-i*22-19,24,18,bp(i))
    LineXY(MA(1)\lx+32,MA(1)\ry-i*22-2,MA(1)\lx+55,MA(1)\ry-i*22-19,bp(7-i))
    FillArea(MA(1)\lx+40,MA(1)\ry-i*22-3,-1,bp(7-i))
  Next  
  Box(MA(1)\lx+32,MA(1)\ry-19,24,18,bp(7))
  LineXY(MA(1)\lx+32,MA(1)\ry-2,MA(1)\lx+55,MA(1)\ry-19,bp(0))
  FillArea(MA(1)\lx+34,MA(1)\ry-18,-1,bp(0))
  
  drawColSel(dCol,7)  
  
  StopDrawing()
EndIf


; canvas 1 - tools
If StartDrawing(CanvasOutput(1))
  ; clear canvas to black
  Box(0,0,#scrW-648,#scrH,bp(0))
  
  DrawText(132,0,"ART for Windows (PB)")
  
  ; draw tools strip and toggle defaults
  DrawImage(ImageID(imgToolStrip),4,2)
  toolToggle(#toolUndo,8) ; undo
  toolToggle(#toolRedo,8) ; redo
  toolToggle(tCur,tTog)   ; standard
  toolToggle(#toolTransparent,2)      ; transparency
  toolToggle(dSel,6)                  ; draw style  
  addToggle(#toolAniPing,(1-animType)*5) ; animation type
  
  ; brush size control
  Circle(MA(3)\lx+33,MA(3)\ly+118,16,bp(6))
  Circle(MA(3)\lx+33,MA(3)\ly+84,12,bp(6))
  Circle(MA(3)\lx+33,MA(3)\ly+55,10,bp(6))
  Circle(MA(3)\lx+33,MA(3)\ly+33,7,bp(6))
  Circle(MA(3)\lx+33,MA(3)\ly+13,5,bp(6))
  
  Circle(MA(3)\lx+6,MA(3)\ly+dWid*4+6,4,bp(7))
  Circle(MA(3)\lx+60,MA(3)\ly+dWid*4+6,4,bp(7))
  
  dBrushSize()
  
  ; stats
  x=MA(4)\lx
  y=MA(4)\ly
  DrawText(x,y,"mX:")
  DrawText(x,y+16,"mY:")
  DrawText(x,y+32,"pX:")
  DrawText(x,y+48,"pY:")
  DrawText(x,y+64,"mB:")
  DrawText(x,y+80,"mA:")
  DrawText(x,y+96,"CT:")
  DrawText(x,y+112,"TS:")  
  DrawText(x,y+128,"gC:")     
  
  ; flash speed
  updateFlashSpeed(fSpeed)
  drawbox(MA(8)\lx,MA(8)\ly,MA(8)\rx,MA(8)\ry,bp(8))
  DrawingFont(FontID(Font1))   
  DrawText(MA(8)\lx,MA(8)\ly-18,"FLASH SPEED",bp(7))
  
  ; flash colour picker
  drawbox(MA(9)\lx,MA(9)\ly,MA(9)\rx,MA(9)\ry,bp(7))
  DrawText(MA(9)\lx,MA(9)\ly-18,"DRAW",bp(7))
  
  drawbox(MA(10)\lx,MA(10)\ly,MA(10)\rx,MA(10)\ry,bp(7))
  DrawText(MA(10)\lx,MA(10)\ly-18,"CYCLE",bp(7))
  DrawingFont(#PB_Default)
  
  updateFlashColours(9)
  updateFlashColours(10)
  
  StopDrawing()
EndIf


; canvas 2 - drawing area
If StartDrawing(CanvasOutput(2))
  ; clear canvas to black
  Box(0,0,#scrW,#scrH,bp(0))
  
  ; main canvas double border
  drawBox(0,0,647,519,bp(7))
  drawBox(1,1,646,518,bp(7))
  
  StopDrawing()
EndIf


If StartDrawing(ScreenOutput())
  DrawImage(ImageID(iBeebSCRN),0,0)
  StopDrawing()
EndIf

FlipBuffers()

;
;-------- MainLoop --------
;

Repeat
  
  ; event loop
  Repeat
    
    Event = WindowEvent()
    
    Select event
      Case #PB_Event_Timer  ; handle flashing colours
        flashing=(flashing+1) & 1
        
        Select animType
          Case 0
            flashCycle+animDir
            If flashCycle=15:animDir=-1:EndIf
            If flashCycle=8:animDir=1:EndIf
            
          Case 1
            
            flashCycle+1
            If flashCycle>15:flashCycle=8:EndIf
            
        EndSelect
        
        ; update flashing colour pointer
        For i=0 To 7
          If flashing
            bpFlash(i+8)=7-i
          Else
            bpFlash(i+8)=i
          EndIf
        Next
        
        If animExport>-1
          animSave=1
        EndIf  
        
      Case #PB_Event_Gadget ; gadget events
        gCur=EventGadget()
        
        ; set mouse action if none already set for left mouse click
        If EventType() = #PB_EventType_LeftButtonDown
          If mact=-1
            
            mx = GetGadgetAttribute(gCur, #PB_Canvas_MouseX)
            my = GetGadgetAttribute(gCur, #PB_Canvas_MouseY)
            
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
                If ListSize(lRedo())
                  ClearList(lRedo())
                  addToggle(#toolRedo,8)
                EndIf
                saveUndo()
                sx=mx-MA(0)\lx
                sy=my-MA(0)\ly
                ox=sx
                oy=sy
                ;MessageRequester("Debug","Undo Created")
                flashCol=8
                fox=px(mx)
                foy=py(my)
                
              Case 10 ; flash draw colour (background), save menu background and display menu
                pickFlashColour(9)
                
              Case 11 ; flash cycle colour (foreground), save menu background and display menu
                pickFlashColour(10)
                
            EndSelect
          EndIf
        EndIf
        
        ; determine which gadget has triggered an event
        Select gCur
            ;
            ;-------- Palette Gadget Area --------
            ;  
            
          Case 0
            mx = GetGadgetAttribute(gCur, #PB_Canvas_MouseX)
            my = GetGadgetAttribute(gCur, #PB_Canvas_MouseY)
            
            If EventType() = #PB_EventType_LeftButtonDown Or (EventType() = #PB_EventType_MouseMove And GetGadgetAttribute(gCur, #PB_Canvas_Buttons) & #PB_Canvas_LeftButton)
              
              Select mact
                  
                Case 2 ; colour select
                  i=7-((my-MA(1)\ly) / 22) + ((mx-MA(1)\lx) / 32)*8
                  If dCol<>i And i>-1 And i<16
                    updateColSel(dCol-i)
                  EndIf
                  
                Case 3 ; pattern select
                  i=7-((my-MA(2)\ly) / 22)
                  j=((mx-MA(2)\lx) / 32)
                  If i<0: i=0: EndIf
                  If i>7: i=7: EndIf
                  If j<0: j=0: EndIf
                  If j>17: j=17: EndIf
                  ;If i%>-1 And i%<8 And J%>-1 And J%<18 THEN
                  If pCol<>i Or pSel<>j
                    If StartDrawing(CanvasOutput(0))              
                      Box(MA(2)\lx+pSel*32,MA(2)\ly+174-pCol*22,32,2,bp(0))
                      pCol=i
                      pSel=j
                      
                      updateBrush()
                      StopDrawing()
                    EndIf
                    If StartDrawing(CanvasOutput(1))
                      dBrushSize()
                      StopDrawing()
                    EndIf
                  EndIf
              EndSelect
            EndIf
            
            ;
            ;-------- Tools Gadget Area --------
            ;              
            
          Case 1 ; tools area
            mx = GetGadgetAttribute(gCur, #PB_Canvas_MouseX)
            my = GetGadgetAttribute(gCur, #PB_Canvas_MouseY)
            
            If EventType() = #PB_EventType_LeftButtonDown Or (EventType() = #PB_EventType_MouseMove And GetGadgetAttribute(gCur, #PB_Canvas_Buttons) & #PB_Canvas_LeftButton)
              Select mact
                  
                Case 4 ; brush size
                  s=(my-MA(3)\ly-4) / 4
                  If s<>dWid And s>-1 And s<33
                    
                    ; update brush size indicator
                    If StartDrawing(CanvasOutput(1))
                      For i=0 To 1
                        Circle(MA(3)\lx+6,MA(3)\ly+dWid*4+6,4,bp(i*7))
                        Circle(MA(3)\lx+60,MA(3)\ly+dWid*4+6,4,bp(i*7))
                        dWid=s
                      Next
                      dBrushSize()
                      StopDrawing()
                    EndIf
                  EndIf
                  
                Case 9 ; flash speed
                  s=(my-MA(8)\ly)*10
                  If s<10: s=10: EndIf
                  If s>1000: s=1000: EndIf
                  If s<>fSpeed
                    fspeed=s
                    If StartDrawing(CanvasOutput(1))
                      updateflashspeed(fspeed)
                      StopDrawing()
                    EndIf
                  EndIf
                  
                Case 10 ; flash draw colour (background)
                  drawFlashSel(9)
                  
                Case 11 ; flash cycle colour (foreground)
                  drawFlashSel(10)
                  
              EndSelect
            EndIf
            
            ; left button release events
            If EventType()=#PB_EventType_LeftButtonUp
              
              ; do any mouse up actions such as tools and pattern select
              Select mact
                  
                Case 7 ; tool select
                       ; get button of tool clicked And action
                  tSel=(mx-MA(6)\lx) / 50+((my-MA(6)\ly) / 50)*2
                  
                  If tSel>-1 And tSel<28
                    Select tSel
                      Case #toolSave ; save
                        opensave(1)
                      Case #toolLine ; load
                        opensave(0)
                      Case #toolUndo ; undo
                        undo()
                      Case #toolRedo ; redo
                        redo()
                      Case #toolCLS ; CLS
                        saveundo()
                        For i=0 To #SCRNsize
                          bSCRN(i)=0
                        Next
                        
                      Case #toolTransparent; toggle transparency
                        dTrn=(dTrn+1) % 2
                        addToggle(tSel,dTrn*2)
                        
                      Case #toolAnimate ; animation toggle
                        flashAnim=(flashAnim+1) % 2
                        addToggle(tSel,flashAnim*5)
                        
                      Case #toolAniPing,#toolAniCycle ; animation type
                        animType=tSel-#toolAniPing
                        addToggle(#toolAniPing,(1-animType)*5)
                        addToggle(#toolAniCycle,animType*5)
                        If flashCycle=8:animDir=1:EndIf
                        If flashCycle=15:animDir=-1:EndIf
                        
                      Case #toolBrushBox,#toolBrushRound,#toolBrushSlash,#toolBrushRND,#toolBrush2x ; brush style
                        If tSel<>dSel
                          addToggle(dSel,0)
                          dSel=tSel
                          addToggle(dSel,6)
                        EndIf
                        
                      Case #toolAniExport ; export animation frames
                        If exportAnim()=1
                          flashAnim=1
                          animExport=0
                          animType=1
                          addToggle(#toolAniPing,(1-animType)*5)
                          addToggle(#toolAniCycle,animType*5)
                          flashCycle=15
                        EndIf  
                        
                      Case #toolQS ; quick save toggle
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
                  
                Case 9 ; flash speed slider
                  RemoveWindowTimer(0,0)
                  AddWindowTimer(0,0,fSpeed)
                  
                Case 10 ; flash draw colour (background), restore menu background and display menu
                  If StartDrawing(CanvasOutput(1))
                    Box(MA(9)\lx,MA(9)\ry+2,32,202,bp(0))
                    StopDrawing()
                  EndIf
                  
                Case 11 ; flash cycle colour (foreground), restore menu background and display menu
                  If StartDrawing(CanvasOutput(1))
                    Box(MA(10)\lx,MA(10)\ry+2,32,202,bp(0))
                    StopDrawing()
                  EndIf
                  
              EndSelect
              
              
            EndIf 
            
            ;
            ;-------- Drawing Gadget Area --------
            ;              
            
          Case 2 ; drawing area
            mx = GetGadgetAttribute(gCur, #PB_Canvas_MouseX)
            my = GetGadgetAttribute(gCur, #PB_Canvas_MouseY)
            
            If EventType() = #PB_EventType_LeftButtonDown Or (EventType() = #PB_EventType_MouseMove And GetGadgetAttribute(gCur, #PB_Canvas_Buttons) & #PB_Canvas_LeftButton)
              
              ox=mx-MA(0)\lx
              oy=my-MA(0)\ly
              
              
              ; determine tool being used
              Select tCur
                Case #toolDraw ; brush tool
                  Select dSel
                    Case #toolBrushBox ; standard brush
                      drawBrush(px(mx),py(my),dWid,0)
                    Case #toolBrushRound ; circle brush
                      drawCircle(mx,my,dWid*2)
                    Case #toolBrushRND ; airbrush
                      drawBrush(px(mx),py(my),dWid,2)
                    Case #toolBrush2x ; standard X2 brush
                      drawBrush(px(mx),py(my),dWid,1)
                  EndSelect
                Case #toolFill ; flood fill
                  i=bSCRN(px(mx)+640*py(my));Point(mx,my)
                  If i<>dFil
                    dFil=i
                    If StartDrawing(CanvasOutput(1))
                      Box(MA(7)\lx,MA(7)\ly,MA(7)\rx-MA(7)\lx,MA(7)\ry-MA(7)\ly,bp(dFil))
                      StopDrawing()
                    EndIf
                  EndIf
                Case #toolAniDraw ; flash draw
                  flashbrush(px(mx),py(my))
              EndSelect
              dWire=tCur
              ; *** end drawing tool code
            EndIf
            
            ;     ; left button release events
            If EventType()=#PB_EventType_LeftButtonUp
              
              ; do any mouse up actions such as tools and pattern select
              Select mact
                Case 1 ; drawing area
                       ; determine tool being used
                  Select tCur
                    Case #toolLine ; line tool completion
                      dLine(sx+MA(0)\lx,sy+MA(0)\ly,ox+MA(0)\lx,oy+MA(0)\ly)
                      
                    Case #toolCirOut ; polygon completion
                      dCircOut(sx,sy,Abs(sx-ox))
                      
                    Case #toolCirFil
                      drawCircle(sx,sy,Abs(sx-ox))
                      
                    Case #toolGradHor,#toolGradVer  ; boxes, gradient
                      If sx<>ox And sy<>oy 
                        dBoxG(sx,sy,ox,oy,tCur-#toolGradHor)
                      EndIf
                      
                    Case #toolBoxOut,#toolBoxFil ; boxes, gradient
                      dBox(sx,sy,ox,oy,tCur-#toolBoxOut)
                      
                    Case #toolFill ; flood fill
                      floodfill(mx,my)
                      
                  EndSelect 
              EndSelect
              
              dWire=0
              
            EndIf
            
        EndSelect
        
        ; left button release events
        If EventType()=#PB_EventType_LeftButtonUp
          
          
          ; reset mouse action
          mact=-1
        EndIf
        
        
      Case #PB_Event_CloseWindow ; close application event  
        End
        
    EndSelect
    
    
  Until event=0
  
  ;
  ;-------- Update Screen --------
  ;         
  
  ; update screen
  ;If drawFlag
  If StartDrawing(ImageOutput(iBeebSCRN))
    Buffer      = DrawingBuffer()             ; Get the start address of the screen buffer
    Pitch       = DrawingBufferPitch()        ; Get the length (in byte) took by one horizontal line
    PixelFormat = DrawingBufferPixelFormat()  ; Get the pixel format. 
    
    
    ; configure palette for RGB or BGR
    If PixelFormat = #PB_PixelFormat_32Bits_RGB
      For i=0 To 15
        ct(i)=bp(bpFlash(i))
        If i>7 And flashBak<8
          If flashAnim=1 And i=flashCycle
            ct(i)=bp(flashFor)
          Else
            ct(i)=bp(flashBak)
          EndIf
        EndIf
      Next
    Else ; Else it's 32bits_BGR
      
      
      For i=0 To 15
        ;ct(i)=rgbT(bpFlash(i))\b+rgbT(bpFlash(i))\g<<8+rgbT(bpFlash(i))\r<<16
        ct(i)=RGBA(rgbT(bpFlash(i))\b,rgbT(bpFlash(i))\g,rgbT(bpFlash(i))\r,255)
        If i>7 And flashBak<8
          If flashAnim=1 And i=flashCycle
            ct(i)=RGBA(rgbT(flashFor)\b,rgbT(flashFor)\g,rgbT(flashFor)\r,255)
          Else
            ct(i)=RGBA(rgbT(flashBak)\b,rgbT(flashBak)\g,rgbT(flashBak)\r,255)
          EndIf
          
        EndIf
      Next
    EndIf
    
    For y = 0 To 511 
      *Line.Pixel = Buffer+Pitch*y
      yMul=(y/2)*640
      
      For x=0 To 159
        dc = ct(bSCRN(x+yMul))
        
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
    
    If dWire
      Select tCur
        Case #toolLine ; line tool
          LineXY(sx,sy,mx-MA(0)\lx,my-MA(0)\ly,bp(7))
          
        Case #toolCirOut,#toolCirFil ; polygon tool
          DrawingMode(#PB_2DDrawing_XOr | #PB_2DDrawing_Outlined )
          Circle(sx,sy,Abs(sx-(mx-MA(0)\lx)),bp(7))
          DrawingMode(#PB_2DDrawing_Default)
        Case #toolBoxOut,#toolBoxFil,#toolGradHor,#toolGradVer  ; boxes, gradient
          drawBox(sx,sy,mx-MA(0)\lx,my-MA(0)\ly,bp(7))
          
      EndSelect
    EndIf
    StopDrawing()
  EndIf
  ;drawFlag=0
  ;EndIf
  
  If StartDrawing(ScreenOutput())
    DrawImage(ImageID(iBeebSCRN),0,0)
    StopDrawing()
  EndIf
  
  
  
  ; update tools
  If StartDrawing(CanvasOutput(1))
    
    ;update tool strip toggles
    If ListSize(lToggle())
      ForEach lToggle()
        toolToggle(lToggle()\button,lToggle()\colour)
      Next
      ClearList(lToggle())
    EndIf
    
    ; show stats
    showstats()
    
    StopDrawing()    
  EndIf
  
  FlipBuffers()
  
  If animSave=1
    f.s=animFile+"0"+Str(animExport)+".PNG"
    SaveTransparent(f,1);,10,32)
    animSave=0
    animExport+1
    If animExport=8
      animExport=-1
      flashAnim=0
      addToggle(#toolAnimate,0)
      MessageRequester(" Export Complete","Animation export complete, check output folder for images.",#PB_MessageRequester_Info)
    EndIf
  EndIf      
  
  ExamineKeyboard() ;Keyboard
  
  If KeyboardReleased(#PB_Key_A)
    updateColSel(1)
  EndIf
  
  If KeyboardReleased(#PB_Key_S)
    updateColSel(-1)
  EndIf
  
  
Until KeyboardPushed(#PB_Key_Escape)

End


;
;-------- Data Section --------
;         

DataSection
  
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
  Data.a 255,000,000,000,255,000,255,255,000,000,000,255,255,000,255
  Data.a 000,255,255,255,255,255,128,128,128,192,000,000,000,192,000
  Data.a 192,192,000,000,000,192,192,000,192,000,192,192,192,192,192
  
  ; mode data
  ; x,y, .....
  modeData:
  Data.w 640,256,1,2,1,2,2 ; mode 0 - 640,256 2 colour (flashing not implemented)
  Data.w 320,256,2,2,2,2,4 ; mode 1 - 320,256 4 colour (flashing not implemented)
  Data.w 160,256,4,2,4,2,8 ; mode 2 - 160,256 8 colour (flashing not implemented)
  Data.w 0,256,1,2,2,1,2   ; mode 3 - to be implemented
  Data.w 320,256,2,2,2,2,2 ; mode 4 - 320,256 2 colour (flashing not implemented)
  Data.w 160,256,4,2,2,4,4 ; mode 5 - 160,256 4 colour (flashing not implemented)
  Data.w 0,256,1,2,2,1,2   ; mode 6 - to be implemented
  Data.w 0,256,1,2,2,1,2   ; mode 7 - to be implemented
  
  
  
  ;   dx.w ; image horizontal pixels
  ;   dy.w ; image vertical pixels
  ;   px.w ; horizontal pixel size
  ;   py.w ; vertical pixel size
  ;   wx.w ; draw width horizontal
  ;   wy.w ; draw width vertical
  ;   cc.w ; palette colour count
  
  
  ; mouse area Data
  ; ensure to update maCount index to match number of mouse areas
  ; x,y,w,h,gad
  mouseData:
  Data.s "Drawing Area"
  Data.w 4,4,640,512,2
  Data.s "Colour Select"
  Data.w 584,4,64,178,0
  Data.s "Pattern Select"
  Data.w 2,4,576,178,0
  Data.s "Brush Size"
  Data.w 108,18,204,140,1
  Data.s "Stats"
  Data.w 104,500,204,200,1
  Data.s "Selected Pattern"
  Data.w 112,524,72,72,1
  Data.s "Tool Strip"
  Data.w 4,2,100,700,1
  Data.s "Fill Colour"
  Data.w 112,404,72,72,1
  Data.s "Flash Speed"
  Data.w 112,184,60,130,1
  Data.s "Flash Draw Colour"
  Data.w 180,184,32,32,1
  Data.s "Flash Cycle Colour"
  Data.w 220,184,32,32,1
  
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
  
  
  ; inline toolstrip bmp
  ToolStripMain:       : IncludeBinary #PB_Compiler_FilePath + "/toolstrip.bmp"
EndDataSection


; IDE Options = PureBasic 5.62 (Windows - x86)
; CursorPosition = 23
; Folding = ------
; EnableXP
; Executable = ART_PB_016_x86.exe
; DisableDebugger
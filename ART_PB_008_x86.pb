;
; ------------------------------------------------------------
;
;   ART 4 Windows
;
;    (c) Fourthstone
;
; ------------------------------------------------------------
;


; constants
#scrW=960
#scrH=704
#drwW=640
#drwH=512
#maxUndo=20

; structures
Structure MouseArea
  lx.w
  ly.w
  rx.w
  ry.w
  name.s
  img.a
EndStructure

Structure ToggleList
  b.a
  c.a
EndStructure

; globals
Global pCol.a=0 ; pattern colour selected
Global pSel.a=8 ; pattern selected

Global dCol.a=1 ; drawing colour
Global dTrn.a=1 ; drawing transparency toggle
Global dWid.a=8 ; drawing width
Global dSel.a=20; drawing tool selected
Global dFil.i=0 ; fill colour

Global tCur.a=4 ; tool selected
Global tTog.a=3 ; tool toggle colour
Global tQSv.a=0 ; quick save toggle
Global tSel.a=0 ; new tool select 

Global maCount.a=7 ; mouse area count 0-n
Global mx,my,ox,oy,sx,sy,mact ; mouse x,y,action
Global iBeebSCRN, imgToolStrip ; image handles

Global Dim pat.a(17,15) ; drawing patterns
Global Dim bp(15) ; beeb palette
Global Dim MA.MouseArea(maCount) ; mouse area structure array

Global NewList lUndo()
Global NewList lRedo()

Global NewList lToggle.togglelist()


; Exit program and display a message
Procedure Exit_ART(m.s)
  If m<>"" 
    MessageRequester("Error", m, 0)
  EndIf
  
  End
EndProcedure

; set pixel coords For relative To draw window
Procedure px(x)
  ProcedureReturn (x-MA(i)\lx) / 4
EndProcedure
Procedure py(y)
  ProcedureReturn (y-MA(i)\ly) / 2
EndProcedure

; display program stats
Procedure showstats()
  Protected x,y
  x=34
  y=4
  
  Box(MA(4)\lx+x,MA(4)\ly+1,90,MA(4)\ry-MA(4)\ly-2,bp(0))
  ;Box(MA(4)\lx+148+x,MA(4)\ly+1,88,MA(4)\ry-MA(4)\ly-2,bp(0))
  
  DrawText(MA(4)\lx+x,MA(4)\ly+y,StrU(mx,#PB_Long))
  DrawText(MA(4)\lx+x,MA(4)\ly+y+16,StrU(my,#PB_Long))
  DrawText(MA(4)\lx+x,MA(4)\ly+y+32,StrU(px(mx),#PB_Long))
  DrawText(MA(4)\lx+x,MA(4)\ly+y+48,StrU(py(my),#PB_Long))
  
  DrawText(MA(4)\lx+x,MA(4)\ly+y+64,StrU(mact,#PB_Long))
  
  DrawText(MA(4)\lx+x,MA(4)\ly+y+80,StrU(tCur,#PB_Long))
  DrawText(MA(4)\lx+x,MA(4)\ly+y+96,StrU(tSel,#PB_Long))
  
  ;DrawText(MA(4)\lx+30+x,MA(4)\ly+64+y,StrU(ListSize(lines()),#PB_Long))
  
            ;dt(M{(4)}.bx%+60,M{(4)}.by%+68,2,STR$(tool%),4)
          ;dt(M{(4)}.bx%+268,M{(4)}.by%+68,2,STR$(cT%),4)

  
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

; drawing brush - generic routine
Procedure dBrush(dx,dy,w,d)
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
  If StartDrawing(ImageOutput(iBeebSCRN))
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
          If lx>-1 And lx<160 And ly>-1 And ly<256
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
              ;Box(lx*4+MA(0)\lx,ly*2+MA(0)\ly,4,2,bp(dc))
              Box(lx*4,ly*2,4,2,bp(dc))
            EndIf
          EndIf
        EndIf
        ly+s
      Next
      lx+s
    Next
    StopDrawing()
  EndIf
  
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
  dBrushSize()
EndProcedure

; update palette, assumes startdrawing is already active
Procedure updatePalette()
  Protected i,p,x,dc
  
  dc=bp(1)
  For i=0 To 7
    For p=0 To 17
      For x=0 To 15
        If pat(p,x)=1
          dc=bp(i)
        Else
          If i=0 And dCol=0
            dc=bp(8)
          Else
            dc=bp(dCol)
          EndIf
        EndIf
        Box(MA(2)\lx+p*32+(x % 4)*4,MA(2)\ly+156-i*22+(x / 4)*2,4,2,dc)
        Box(MA(2)\lx+p*32+(x % 4)*4,MA(2)\ly+164-i*22+(x / 4)*2,4,2,dc)
        Box(MA(2)\lx+16+p*32+(x % 4)*4,MA(2)\ly+156-i*22+(x / 4)*2,4,2,dc)
        Box(MA(2)\lx+16+p*32+(x % 4)*4,MA(2)\ly+164-i*22+(x / 4)*2,4,2,dc)
      Next
    Next
  Next
  updateBrush()
    
EndProcedure

; draw vertical line With current pattern
Procedure Vline2(x1,y1,y2)
  Protected ly,dc,pS,x1M
  
  If StartDrawing(ImageOutput(iBeebSCRN))
    x1M=x1 % 4
    For ly=y1 To y2
      ; range check, set pattern colour And plot
      If x1>-1 And x1<160 And ly>-1 And ly<256
        pS=x1M+(ly % 4)*4
        If pat(pSel,pS) 
          dc=pCol
        Else
          dc=dCol
        EndIf
        
        ; Check For transparency
        If dc-dTrn>-1
          Box(x1*4,ly*2,4,2,bp(dc))   
        EndIf
      EndIf
    Next
    StopDrawing()
  EndIf
EndProcedure
    
      
; draw vertical line With current pattern
Procedure Hline2(x1,x2,y1)
  Protected lx,dc,pS,y1M
  
  If StartDrawing(ImageOutput(iBeebSCRN))
    y1M=(y1 % 4)*4
    For lx=x1 To x2
      ; range check, set pattern colour And plot
      If lx>-1 And lx<160 And y1>-1 And y1<256
        pS=lx % 4+y1M
        If pat(pSel,pS) 
          dc=pCol
        Else
          dc=dCol
        EndIf
        
        ; Check For transparency
        If dc-dTrn>-1
          Box(lx*4,y1*2,4,2,bp(dc))   
        EndIf
      EndIf
    Next
    StopDrawing()
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
    If x1<-dWid Or x1>160+dWid : Break : EndIf
    If y1<-dWid Or y1>256+dWid : Break : EndIf
    ;PROCreadmouse
  Until 0
  
EndProcedure
    

; draw highlight box for selected colour
Procedure drawColSel(i,c)
  drawBox(MA(1)\lx,MA(1)\ry-i*22-22,MA(1)\lx+39,MA(1)\ry-i*22+1,bp(c))
EndProcedure


; toggle box control status, assumes startdrawing is already active
; b: button number
; c: highlight colour
Procedure toolToggle(i,c)
  Protected x,y,lx,ly
  
  ; default or erase button colour is 7
  If c=0: c=7: EndIf
  x=MA(6)\lx+(i % 2)*50+4
  y=MA(6)\ly+(i/2)*50+4
  
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

; save undo
Procedure saveUndo()
  ; remove first item in list if undo buffer full
  If ListSize(lUndo())>#maxUndo
    FirstElement(lUndo())
    FreeImage(lUndo())
    DeleteElement(lUndo())
    LastElement(lUndo())
  EndIf
  ; add a new backup image to undo buffer
  AddElement(lUndo())
  lUndo()=GrabImage(iBeebSCRN,#PB_Any,0,0,#drwW,#drwH)
  
  If ListSize(lUndo())=1
    addToggle(2,7)
  EndIf

EndProcedure

; save redo
Procedure saveRedo()
  ; add a new backup image to redo buffer
  AddElement(lRedo())
  lRedo()=GrabImage(iBeebSCRN,#PB_Any,0,0,#drwW,#drwH)
  If ListSize(lRedo())=1
      addToggle(3,7)
  EndIf
EndProcedure

; restore undo image
Procedure Undo()
  If ListSize(lUndo())
    ; create redo image first
    saveRedo()
    ; update drawing canvas with undo image
    If StartDrawing(ImageOutput(iBeebSCRN))
      DrawImage(ImageID(lundo()),0,0)
      StopDrawing()
    EndIf
    ; discard undo image from buffer
    FreeImage(lUndo())
    DeleteElement(lUndo())
    If ListSize(lUndo())=0
        addToggle(2,8)
    EndIf
  EndIf
EndProcedure

; restore redo image
Procedure Redo()
  If ListSize(lRedo())
    ; create undo image first
    saveUndo()
    ; update drawing canvas with redo image
    If StartDrawing(ImageOutput(iBeebSCRN))
      DrawImage(ImageID(lRedo()),0,0)
      StopDrawing()
    EndIf
    
    ; discard redo image from buffer
    FreeImage(lRedo())
    DeleteElement(lRedo())
    
    If ListSize(lRedo())=0
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
  Read.a r
  Read.a g
  Read.a b
  bp(i)=RGB(r,g,b)
Next
bp(0) = RGB(0,0,0)

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

; toolstrip image
;imgToolStrip=LoadImage(#PB_Any,"ToolStrip.bmp")
;If imgToolStrip=0
;  Exit_ART("Cannot load ToolStrip.bmp")
;EndIf
imgToolStrip=CatchImage(#PB_Any,?ToolStripMain)

; main drawing area
iBeebSCRN=CreateImage(#PB_Any,#drwW,#drwH,24)

If StartDrawing(ImageOutput(iBeebSCRN))
  Box(0,0,639,511,bp(0))
  StopDrawing()
EndIf


; init screen gadget and initial state
CanvasGadget(0,0,0,#scrW,#scrH)
SetGadgetAttribute(0, #PB_Canvas_Cursor , #PB_Cursor_Cross)
mact=-1

If StartDrawing(CanvasOutput(0))
  ; clear canvas to black
  Box(0,0,#scrW,#scrH,bp(0))
  
  ; main canvas double border
  drawBox(MA(0)\lx-4,MA(0)\ly-4,MA(0)\rx+4,MA(0)\ry+4,bp(7))
  drawBox(MA(0)\lx-3,MA(0)\ly-3,MA(0)\rx+3,MA(0)\ry+3,bp(7))
  
  ; colour select border
  ;drawBox(MA(1)\lx-4,MA(1)\ly-6,MA(1)\rx+4,MA(1)\ry+6,bp(7))
  
  ; pattern select border
  ;drawBox(MA(2)\lx-6,MA(2)\ly-6,MA(2)\rx+6,MA(2)\ry+6,bp(7))
  
  DrawText(776,0,"ART for Windows (PB)")
  
  ; draw tools strip and toggle defaults
  DrawImage(ImageID(imgToolStrip),MA(6)\lx,MA(6)\ly)
  toolToggle(2,8) ; undo
  toolToggle(3,8) ; redo
  toolToggle(tCur,tTog) ; standard
  toolToggle(18,2)      ; transparency
  toolToggle(dSel,6)    ; draw style  
  
  DrawImage(ImageID(iBeebSCRN),MA(0)\lx,MA(0)\ly)
  
  ; brush size control
  Circle(MA(3)\lx+33,MA(3)\ly+118,16,bp(6))
  Circle(MA(3)\lx+33,MA(3)\ly+84,12,bp(6))
  Circle(MA(3)\lx+33,MA(3)\ly+55,10,bp(6))
  Circle(MA(3)\lx+33,MA(3)\ly+33,7,bp(6))
  Circle(MA(3)\lx+33,MA(3)\ly+13,5,bp(6))
  
  Circle(MA(3)\lx+6,MA(3)\ly+dWid*4+6,4,bp(7))
  Circle(MA(3)\lx+60,MA(3)\ly+dWid*4+6,4,bp(7))
  
  updatePalette()
  
  ; enable for debugging mouse area squares
   For i=0 To maCount
     drawBox(MA(i)\lx,MA(i)\ly,MA(i)\rx,MA(i)\ry,bp(i))
   Next
  
  ; draw colour select boxes
  drawColSel(dCol,7)
  For i=1 To 7
    Box(MA(1)\lx+4,MA(1)\ry-i*22-19,32,18,bp(i))
  Next  
  
  DrawText(MA(4)\lx+4,MA(4)\ly+4,"mX:")
  DrawText(MA(4)\lx+4,MA(4)\ly+20,"mY:")
  DrawText(MA(4)\lx+4,MA(4)\ly+36,"pX:")
  DrawText(MA(4)\lx+4,MA(4)\ly+52,"pY:")
  DrawText(MA(4)\lx+4,MA(4)\ly+68,"mB:")
  DrawText(MA(4)\lx+4,MA(4)\ly+84,"mA:")
  DrawText(MA(4)\lx+4,MA(4)\ly+100,"CT:")
  DrawText(MA(4)\lx+4,MA(4)\ly+116,"TS:")
  
  StopDrawing()
EndIf

;
;-------- MainLoop --------
;

Repeat
  Event = WaitWindowEvent()
  
  If Event = #PB_Event_Gadget And EventGadget() = 0 
    ; read mouse
    mx = GetGadgetAttribute(0, #PB_Canvas_MouseX)
    my = GetGadgetAttribute(0, #PB_Canvas_MouseY)
    
      
      ; left mouse button down and mouse move events
      If EventType() = #PB_EventType_LeftButtonDown Or (EventType() = #PB_EventType_MouseMove And GetGadgetAttribute(0, #PB_Canvas_Buttons) & #PB_Canvas_LeftButton)
        
        ; set mouse action if none already set
        If mact=-1
          
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
                addToggle(3,8)
              EndIf
              saveUndo()
              sx=mx-MA(0)\lx
              sy=my-MA(0)\ly
              ox=sx
              oy=sy
              
          EndSelect
        EndIf
        
        ; do any drawing actions for mouse move
        Select mact
          Case 1 ; main drawing canvas
            
            ; determine tool being used
            Select tCur
                Case 4 ; brush tool
                  dbrush(px(mx),py(my),dWid,0)
                Case 5 ; line tool
                  If StartDrawing(ImageOutput(iBeebSCRN))
                    DrawingMode(#PB_2DDrawing_XOr)
                    LineXY(sx,sy,ox,oy,bp(7))
                    LineXY(sx,sy,mx-MA(0)\lx,my-MA(0)\ly,bp(7))
                    StopDrawing()
                  EndIf
                  ox=mx-MA(0)\lx
                  oy=my-MA(0)\ly
                
                Case 6,7 ; polygon tool
                 If StartDrawing(CanvasOutput(0))
                    DrawingMode(#PB_2DDrawing_XOr | #PB_2DDrawing_Outlined )
                    Circle(sx,sy,sx-ox,bp(7))
                    Circle(sx,sy,sx-mx,bp(7))
                    DrawingMode(#PB_2DDrawing_Default)
                    StopDrawing()
                  EndIf
                  ox=mx
                  oy=my
                Case 8,9,12,13  ; boxes, gradient
                  If StartDrawing(CanvasOutput(0))
                    DrawingMode(#PB_2DDrawing_XOr | #PB_2DDrawing_Outlined )
                    Box(sx,sy,ox-sx,oy-sx,bp(7))
                    Box(sx,sy,mx-sx,my-sy,bp(7))
                    DrawingMode(#PB_2DDrawing_Default)
                    StopDrawing()
                  EndIf
                  ox=mx
                  oy=my
                Case 10 ; flood fill
                  If StartDrawing(CanvasOutput(0))
                    i=Point(mx,my)
                    If i<>dFil
                      dFil=i
                      Box(MA(7)\lx,MA(7)\ly,MA(7)\rx-MA(7)\lx,MA(7)\ry-MA(7)\ly,dFil)
                    EndIf
                    StopDrawing()
                  EndIf
                   
            EndSelect
            
          Case 2 ; colour select
            i=7-((my-MA(1)\ly) / 24)
            If dCol<>i And i>-1 And i<8
              If StartDrawing(CanvasOutput(0))    
                drawColSel(dCol,0)
                drawColSel(i,7)
                dCol=i
                updatePalette()
                StopDrawing()
              EndIf
              EndIf
          Case 3 ; pattern select
            i=7-((my-MA(2)\ly) / 24)
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
            EndIf
            
          Case 4 ; brush size
            s=(my-MA(3)\ly-4) / 4
            If s<>dWid And s>-1 And s<33
              
              ; update brush size indicator
              If StartDrawing(CanvasOutput(0))
                For i=0 To 1
                  Circle(MA(3)\lx+6,MA(3)\ly+dWid*4+6,4,bp(i*7))
                  Circle(MA(3)\lx+60,MA(3)\ly+dWid*4+6,4,bp(i*7))
                  dWid=s
                Next
                dBrushSize()
                StopDrawing()
              EndIf
            EndIf
            
        EndSelect
      EndIf
      
      ; left button release events
      If EventType()=#PB_EventType_LeftButtonUp
        
        ; do any mouse up actions such as tools and pattern select
        Select mact
          Case 1 ; drawing area
            ; determine tool being used
            Select tCur
                Case 5 ; line tool completion
                  If StartDrawing(ImageOutput(iBeebSCRN))
                    DrawingMode(#PB_2DDrawing_XOr)
                    LineXY(sx,sy,ox,oy,bp(7))
                    LineXY(sx,sy,sx,sy,bp(7))
                    DrawingMode(#PB_2DDrawing_Default)
                    StopDrawing()
                  EndIf
                  
                  dLine(sx+MA(0)\lx,sy+MA(0)\ly,ox+MA(0)\lx,oy+MA(0)\ly)
                  
                Case 6,7 ; polygon completion
                 If StartDrawing(CanvasOutput(0))
                    DrawingMode(#PB_2DDrawing_XOr | #PB_2DDrawing_Outlined )
                    Circle(sx,sy,sx-ox,bp(7))
                    DrawingMode(#PB_2DDrawing_Default)
                    StopDrawing()
                  EndIf
                  
            EndSelect            
            
          Case 3 ; pattern select
            If range(2)=1
              ;col+1
              ;If col>7
              ;  col=0
              ;EndIf
              
              ;updatepalette()
            EndIf
          Case 7 ; tool select
            
            ; get button of tool clicked And action
            tSel=(mx-MA(6)\lx) / 50+((my-MA(6)\ly) / 50)*2
            
            If tSel>-1 And tSel<28
              Select tSel
                Case 0 ; save
                       ;opensave(1)
                Case 1 ; load
                       ;opensave(0)
                Case 2 ; undo
                       undo()
                Case 3 ; redo
                       redo()
                Case 19; CLS
                  saveundo()
                  If StartDrawing(ImageOutput(iBeebSCRN))
                    Box(0,0,#drwW,#drwH,bp(0))
                    StopDrawing()
                  EndIf
                Case 18; toggle transparency
                  dTrn=(dTrn+1) % 2
                  addToggle(tSel,dTrn*2)

                Case 20,21,22,23,24 ; brush style
                  If tSel<>dSel
                      addToggle(dSel,0)
                      dSel=tSel
                      addToggle(dSel,6)
                  EndIf
                  
                Case 27 ; quick save toggle
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
        EndSelect
    
        
        ; reset mouse action
        mact=-1
      EndIf 
      
      ; update screen
      If StartDrawing(CanvasOutput(0))
        
        ; update drawing area
        DrawImage(ImageID(iBeebSCRN),MA(0)\lx,MA(0)\ly)

        
        ;update tool strip toggles
        If ListSize(lToggle())
          ForEach lToggle()
            toolToggle(lToggle()\b,lToggle()\c)
          Next
          ClearList(lToggle())
        EndIf
        
        ; show stats
        showstats()
        
        StopDrawing()    
      EndIf
      
  EndIf
Until Event = #PB_Event_CloseWindow

End


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
  
  ; mouse area Data
  ; ensure to update maCount index to match number of mouse areas
  mouseData:
  Data.s "Drawing Area"
  Data.w 4,4,640,512
  Data.s "Colour Select"
  Data.w 600,524,40,178
  Data.s "Pattern Select"
  Data.w 6,524,576,178
  Data.s "Brush Size"
  Data.w 756,18,204,140
  Data.s "Stats"
  Data.w 756,164,204,200
  Data.s "Selected Pattern"
  Data.w 760,524,72,72
  Data.s "Tool Strip"
  Data.w 654,2,100,700
  Data.s "Fill Colour"
  Data.w 760,604,72,72
  
  ; inline toolstrip bmp
  ToolStripMain:       : IncludeBinary #PB_Compiler_FilePath + "/toolstrip.bmp"
EndDataSection
    
      
; IDE Options = PureBasic 5.61 (Windows - x86)
; CursorPosition = 85
; FirstLine = 69
; Folding = Y---
; EnableXP
; Executable = ART_PB_008_x86.exe
; DisableDebugger
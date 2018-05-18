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

Structure ToggleList
  b.a
  c.a
EndStructure

Structure drawLayers
  IMG.l   ; image number
  List lUndo.l()
  List lRedo.l()
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
Global tCur.a=4 ; tool selected
Global tTog.a=3 ; tool toggle colour
Global tQSv.a=0 ; quick save toggle
Global tSel.a=0 ; new tool select 

Global maCount.a=8 ; mouse area count 0-n
Global mx,my,ox,oy,sx,sy,mact ; mouse x,y,action
Global imgToolStrip, imgToolStrip2, imgPAL; image handles

Global Dim l.drawLayers(4) ; layers array

Global Dim pat.a(17,15) ; drawing patterns
Global Dim bp(15)       ; beeb palette
Global Dim MA.MouseArea(maCount) ; mouse area structure array

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
  ProcedureReturn (x-MA(i)\lx) / dMpx
EndProcedure
Procedure py(y)
  ProcedureReturn (y-MA(i)\ly) / dMpy
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
            ;Box(lx*4+MA(0)\lx,ly*2+MA(0)\ly,4,2,bp(dc))
            Box(lx*dMpx,ly*dMpy,dMpx,dMpy,bp(dc))
          EndIf
        EndIf
      EndIf
      ly+s
    Next
    lx+s
  Next
  
EndProcedure

; Draw box with current brush
Procedure updateBrush()
  Protected bx,by,lx,ly,dc
  
  bx=MA(5)\lx
  by=MA(5)\ly
  
  ;draw pattern loop
  For lx=0 To 8
    For ly=0 To 17
      pS=(bx+lx) % 4+((by+ly) % 4)*4
      If pat(pSel,pS) 
        dc=pCol
        If dc=0 And dCol=0:dc=8:EndIf
      Else
        dc=dCol
      EndIf
      Box(bx+lx*4+4,by+ly*2+4,4,2,bp(dc))
    Next
  Next
  
  ;dBrushSize()
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
            dc=bp(dCol)
          EndIf
        EndIf
        ; plot 4 pixels for this element offset 32 * 16 (+4 gap vertically)
        Box(MA(8)\lx+4+p*32+(x % 4)*4,MA(8)\ly+160-i*22+(x / 4)*2,4,2,dc)
        Box(MA(8)\lx+4+p*32+(x % 4)*4,MA(8)\ly+168-i*22+(x / 4)*2,4,2,dc)
        Box(MA(8)\lx+20+p*32+(x % 4)*4,MA(8)\ly+160-i*22+(x / 4)*2,4,2,dc)
        Box(MA(8)\lx+20+p*32+(x % 4)*4,MA(8)\ly+168-i*22+(x / 4)*2,4,2,dc)
      Next
    Next
  Next
  updateBrush()
  ; highlight selected pattern and update selected brush size pattern
  Box(MA(8)\lx+pSel*32,MA(8)\ly+174-pCol*22+4,32,2,bp(7))

  
EndProcedure

; draw vertical line with current pattern 1 pixel wide
Procedure Vline2(x1,y1,y2)
  Protected ly,dc,pS,x1M
  
  ; validate x is in range before starting
  If x1>-1 And x1<dMdx
    x1M=x1 % 4
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
          Box(x1*4,ly*2,4,2,bp(dc))   
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
        Box(lx*4,y1*2,4,2,bp(dc))   
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
          dc=pCol
        Else
          dc=dCol
        EndIf
        
        ; Check For transparency
        If dc-dTrn>-1
          
          Box(lx*dMpx,ly*dMpy,dMpx,dMpy,bp(dc))
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
  drawBox(MA(1)\lx+2+i*32,MA(1)\ly+3,MA(1)\lx+33+i*32,MA(1)\ly+34,bp(c))
EndProcedure

; update colour select value
Procedure updateColSel(c.b)
  If StartDrawing(CanvasOutput(0))    
    drawColSel(dCol,0)
    dCol-c
    If dcol=255:dCol=7:EndIf
    If dcol=8:dCol=0:EndIf
    drawColSel(dcol,7)
    If mact=9
      updatePalette()
    Else
      updateBrush()
    EndIf
    StopDrawing()
  EndIf  
EndProcedure
  
; update pattern colour 
Procedure updatepCol(c.b)
  If StartDrawing(CanvasOutput(0))              
    Box(MA(2)\lx+pSel*32,MA(2)\ly+174-pCol*22,32,2,bp(0))
    pCol+c
    If pCol=8:pCol=0:EndIf
    If pCol=255:pCol=7:EndIf
    
    updateBrush()
    StopDrawing()
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
        
        If Point(x*4,y*2)=mc
          uf=1 : df=1
          
          ; scan left
          While x>0 And Point((x-1)*4,y*2)=mc
            x-1
          Wend
          
          ; scan right and plot fill colour
          While x<dMdx And Point(x*4,y*2)=mc
            If i
              pS=x % 4+(y % 4)*4
              If pat(pSel,pS)
                dc=pCol
              Else
                dc=dCol
              EndIf
              
            EndIf
            
            Box(x*4,y*2,4,2,bp(dc))
            
            ; detect colour changes above and add To List
            If y<(dMdy-1)
              c=Point(x*4,(y+1)*2)
              If uf And c=mc
                AddElement(lFS())
                lFS()\x=x : lFS()\y=y+1
                uf=0
              EndIf
              If c<>mc : uf=1 : EndIf
            EndIf
            
            ; detect colour changes below and add To List
            If y>0
              c=Point(x*4,(y-1)*2)
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
      
      mc=bp(15)
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
Procedure openSave(a)
  
  Protected lastFN.s, filename.s, action.s, F.s, ff.s, N.w, iTMP
  
  ff = "PNG files (*.PNG)|*.PNG"
  
  If a=1
    If tQSv=0
      ;  ff = "BMP files"+Chr(0)+"*.BMP"+Chr(0)+Chr(0)+"PNG files"+Chr(0)+"*.PNG"+Chr(0)
      Repeat
        ok=#PB_MessageRequester_Yes
        
        ; show save dialog and prompt if file exists
        filename=SaveFileRequester(" Save Image",GetCurrentDirectory(),ff,0)
        If filename<>""
          If Right(UCase(filename),4)<>".PNG"
            filename=filename+".PNG"
          EndIf
          If FileSize(filename)>-1
            ok=MessageRequester(" Confirm File Overwrite",GetFilePart(filename)+" already exists. Do you want to replace it?",#PB_MessageRequester_YesNo)
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
        MessageRequester(" File Error","ERROR: Cannot quicksave! File number limit reached, archive some files!!!")
      EndIf
    EndIf
    
  Else ; get filename to load
    filename=OpenFileRequester(" Load Image",GetCurrentDirectory(),ff,0)
  EndIf
  
  ; action file operation
  If filename=""
    filename= "File Not Selected!"
  Else
    Select a 
      Case 0 ; load image and copy to drawing area
        action="opened"
        iTMP=LoadImage(#PB_Any,filename)
        If iTMP
          If StartDrawing(ImageOutput(l(dLay)\IMG))
            DrawImage(ImageID(iTMP),0,0)
            StopDrawing()
            FreeImage(iTMP)            
          EndIf          
        Else
          MessageRequester(" File Error","ERROR: Cannot load file" + #CRLF$ + #CRLF$ + filename)
        EndIf
        
        ;filename$+""" "+STR$(M{(0)}.mx%)+","+STR$(M{(0)}.my%)+"
      Case 1 ; save drawing area to file
        action$="saved"
        
        SaveImage(l(dLay)\IMG, filename, #PB_ImagePlugin_PNG,10,4)
    EndSelect
  EndIf
  
  ;PROCupdateStatus(MID$(filename$,FNINSTRREV(filename$,"\")+1) + " " + action$)
EndProcedure


; save undo
Procedure saveUndo()
  ; remove first item in list if undo buffer full
  If ListSize(l(dLay)\lUndo())>#maxUndo
    FirstElement(l(dLay)\lUndo())
    FreeImage(l(dLay)\lUndo())
    DeleteElement(l(dLay)\lUndo())
    LastElement(l(dLay)\lUndo())
  EndIf
  ; add a new backup image to undo buffer
  AddElement(l(dLay)\lUndo())
  l(dLay)\lUndo()=GrabImage(l(dLay)\IMG,#PB_Any,0,0,#drwW,#drwH)
  
  If ListSize(l(dLay)\lUndo())=1
    addToggle(2,7)
  EndIf
  
EndProcedure

; save redo
Procedure saveRedo()
  ; add a new backup image to redo buffer
  AddElement(l(dLay)\lRedo())
  l(dLay)\lRedo()=GrabImage(l(dLay)\IMG,#PB_Any,0,0,#drwW,#drwH)
  If ListSize(l(dLay)\lRedo())=1
    addToggle(3,7)
  EndIf
EndProcedure

; restore undo image
Procedure Undo()
  If ListSize(l(dLay)\lUndo())
    ; create redo image first
    saveRedo()
    ; update drawing canvas with undo image
    If StartDrawing(ImageOutput(l(dLay)\IMG))
      DrawImage(ImageID(l(dLay)\lUndo()),0,0)
      StopDrawing()
    EndIf
    ; discard undo image from buffer
    FreeImage(l(dLay)\lUndo())
    DeleteElement(l(dLay)\lUndo())
    If ListSize(l(dLay)\lUndo())=0
      addToggle(2,8)
    EndIf
  EndIf
EndProcedure

; restore redo image
Procedure Redo()
  If ListSize(l(dLay)\lRedo())
    ; create undo image first
    saveUndo()
    ; update drawing canvas with redo image
    If StartDrawing(ImageOutput(l(dLay)\IMG))
      DrawImage(ImageID(l(dLay)\lRedo()),0,0)
      StopDrawing()
    EndIf
    
    ; discard redo image from buffer
    FreeImage(l(dLay)\lRedo())
    DeleteElement(l(dLay)\lRedo())
    
    If ListSize(l(dLay)\lRedo())=0
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
imgToolStrip2=CatchImage(#PB_Any,?ToolStripMain2)

; create drawing layers
For i=0 To ArraySize(l())-1
  l(i)\IMG=CreateImage(#PB_Any,#drwW,#drwH,24)
  If StartDrawing(ImageOutput(l(i)\IMG))
    Box(0,0,#drwW-1,#drwH-1,bp(0))
    StopDrawing()
  EndIf
Next

; init screen gadget and initial state
CanvasGadget(0,0,0,#scrW,#scrH)
SetGadgetAttribute(0, #PB_Canvas_Cursor , #PB_Cursor_Cross)
mact=-1

tFont=LoadFont(#PB_Any,"Comic Sans MS",14,#PB_Font_Italic |#PB_Font_Bold)

;
;-------- Draw controls --------
;

If StartDrawing(CanvasOutput(0))
  ; clear canvas to black
  Box(0,0,#scrW,#scrH,bp(0))
  
  ; main canvas double border
  drawBox(MA(0)\lx-4,MA(0)\ly-4,MA(0)\rx+4,MA(0)\ry+4,bp(7))
  drawBox(MA(0)\lx-3,MA(0)\ly-3,MA(0)\rx+3,MA(0)\ry+3,bp(7))
  
  ; pattern select border
  ;drawBox(MA(2)\lx-6,MA(2)\ly-6,MA(2)\rx+6,MA(2)\ry+6,bp(7))
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
  DrawImage(ImageID(l(0)\IMG),MA(0)\lx,MA(0)\ly)
  
  ; brush size control
  Circle(MA(3)\lx+33,MA(3)\ly+118,16,bp(6))
  Circle(MA(3)\lx+33,MA(3)\ly+84,12,bp(6))
  Circle(MA(3)\lx+33,MA(3)\ly+55,10,bp(6))
  Circle(MA(3)\lx+33,MA(3)\ly+33,7,bp(6))
  Circle(MA(3)\lx+33,MA(3)\ly+13,5,bp(6))
  
  Circle(MA(3)\lx+6,MA(3)\ly+dWid*4+6,4,bp(7))
  Circle(MA(3)\lx+60,MA(3)\ly+dWid*4+6,4,bp(7))
  
  DrawText(MA(3)\lx+4,MA(3)\ly-36,"Brush",bp(7))
  DrawText(MA(3)\lx+8,MA(3)\ly-20,"Size",bp(7))
  
  updateBrush()
  
  
  ; enable for debugging mouse area squares
;     For i=0 To maCount
;       drawBox(MA(i)\lx,MA(i)\ly,MA(i)\rx,MA(i)\ry,bp(i))
;     Next
    
  ; draw colour select boxes and double border
  drawColSel(dCol,7)
  For i=1 To 7
    Box(MA(1)\lx+4+i*32,MA(1)\ly+5,28,28,bp(i))
  Next  
  
  drawBox(MA(1)\lx,MA(1)\ly,MA(1)\rx,MA(1)\ry,bp(7))
  drawBox(MA(1)\lx+1,MA(1)\ly+1,MA(1)\rx-1,MA(1)\ry-1,bp(8))
  
  
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
      
      ; check if mouse is in palette area
      If EventType() = #PB_EventType_MouseMove
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
            StopDrawing()
          EndIf
          FreeImage(imgPAL)
        EndIf
      EndIf
      
      
      
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
              If ListSize(l(dLay)\lRedo())
                ClearList(l(dLay)\lRedo())
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
                If StartDrawing(ImageOutput(l(dLay)\IMG))
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
                  StopDrawing()
                EndIf
              Case 5 ; line tool
                If StartDrawing(ImageOutput(l(dLay)\IMG))
                  DrawingMode(#PB_2DDrawing_XOr)
                  LineXY(sx,sy,ox,oy,bp(7))
                  LineXY(sx,sy,mx-MA(0)\lx,my-MA(0)\ly,bp(7))
                  StopDrawing()
                EndIf
                ox=mx-MA(0)\lx
                oy=my-MA(0)\ly
                
              Case 6,7 ; polygon tool
                If StartDrawing(ImageOutput(l(dLay)\IMG))
                  DrawingMode(#PB_2DDrawing_XOr | #PB_2DDrawing_Outlined )
                  Circle(sx,sy,Abs(sx-ox),bp(7))
                  Circle(sx,sy,Abs(sx-(mx-MA(0)\lx)),bp(7))
                  DrawingMode(#PB_2DDrawing_Default)
                  StopDrawing()
                EndIf
                
                ox=mx-MA(0)\lx
                oy=my-MA(0)\ly
              Case 8,9,12,13  ; boxes, gradient
                If StartDrawing(ImageOutput(l(dLay)\IMG))
                  DrawingMode(#PB_2DDrawing_XOr)
                  drawBox(sx,sy,ox,oy,bp(7))
                  drawBox(sx,sy,mx-MA(0)\lx,my-MA(0)\ly,bp(7))
                  DrawingMode(#PB_2DDrawing_Default)
                  StopDrawing()
                EndIf
                ox=mx-MA(0)\lx
                oy=my-MA(0)\ly
              Case 10 ; flood fill
                If StartDrawing(CanvasOutput(0))
                  i=Point(mx,my)
                  If i<>dFil
                    dFil=i
                    Box(MA(7)\lx+4,MA(7)\ly+4,MA(7)\rx-MA(7)\lx-7,MA(7)\ry-MA(7)\ly-7,dFil)
                  EndIf
                  StopDrawing()
                EndIf
            EndSelect
            ; *** end drawing tool code
            
          Case 2 ; colour select
            i=(mx-MA(1)\lx) / 32
            If dCol<>i And i>-1 And i<8
              If StartDrawing(CanvasOutput(0))    
                drawColSel(dCol,0)
                dCol=i
                drawColSel(dcol,7)
                updateBrush()
                StopDrawing()
              EndIf
            EndIf
          Case 9 ; pattern select2 - old palette select is mact=3
            i=7-((my-MA(8)\ly) / 22)
            j=((mx-MA(8)\lx) / 32)
            If i<0: i=0: EndIf
            If i>7: i=7: EndIf
            If j<0: j=0: EndIf
            If j>17: j=17: EndIf
            ;If i%>-1 And i%<8 And J%>-1 And J%<18 THEN
            If pCol<>i Or pSel<>j
              If StartDrawing(CanvasOutput(0))              
                Box(MA(8)\lx+pSel*32+4,MA(8)\ly+174-pCol*22+4,32,2,bp(0))
                pCol=i
                pSel=j
                
                updateBrush()
                ; highlight selected pattern and update selected brush size pattern
                Box(MA(8)\lx+pSel*32+4,MA(8)\ly+174-pCol*22+4,32,2,bp(7))

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
        
        ; do any mouse up actions such as tools and pattern select
        Select mact
          Case 1 ; drawing area
                 ; determine tool being used
            Select tCur
              Case 5 ; line tool completion
                If StartDrawing(ImageOutput(l(dLay)\IMG))
                  DrawingMode(#PB_2DDrawing_XOr)
                  LineXY(sx,sy,ox,oy,bp(7))
                  LineXY(sx,sy,sx,sy,bp(7))
                  DrawingMode(#PB_2DDrawing_Default)
                  
                  dLine(sx+MA(0)\lx,sy+MA(0)\ly,ox+MA(0)\lx,oy+MA(0)\ly)
                  StopDrawing()
                EndIf
                
              Case 6,7 ; polygon completion
                If StartDrawing(ImageOutput(l(dLay)\IMG))
                  DrawingMode(#PB_2DDrawing_XOr | #PB_2DDrawing_Outlined )
                  Circle(sx,sy,Abs(sx-ox),bp(7))
                  DrawingMode(#PB_2DDrawing_Default)
                  
                  If tCur=6 
                    dCircOut(sx,sy,Abs(sx-ox))
                  Else
                    dCircle(sx,sy,Abs(sx-ox))
                  EndIf
                  
                  StopDrawing()
                EndIf
                
                
              Case 8,9,12,13  ; boxes, gradient
                If StartDrawing(ImageOutput(l(dLay)\IMG))
                  DrawingMode(#PB_2DDrawing_XOr)
                  drawBox(sx,sy,ox,oy,bp(7))
                  DrawingMode(#PB_2DDrawing_Default)
                  
                  If tCur>9
                    If sx<>ox And sy<>oy 
                      dBoxG(sx,sy,ox,oy,tCur-12)
                    EndIf
                  Else
                    dBox(sx,sy,ox,oy,tCur-8)
                  EndIf
                  
                  StopDrawing()
                EndIf
              Case 10 ; flood fill
                If StartDrawing(ImageOutput(l(dLay)\IMG))
                  floodfill(mx-MA(0)\lx,my-MA(0)\ly)
                  StopDrawing()
                EndIf
            EndSelect            
            
          Case 7 ; tool select
            
            ; get button of tool clicked And action
            tSel=(mx-MA(6)\lx) / 50+((my-MA(6)\ly) / 50)*2
            
            If tSel>-1 And tSel<28
              Select tSel
                Case 0 ; save
                  opensave(1)
                Case 1 ; load
                  opensave(0)
                Case 2 ; undo
                  undo()
                Case 3 ; redo
                  redo()
                Case 15; CLS
                  saveundo()
                  If StartDrawing(ImageOutput(l(dLay)\IMG))
                    Box(0,0,#drwW,#drwH,bp(0))
                    StopDrawing()
                  EndIf
                Case 14; toggle transparency
                  dTrn=(dTrn+1) % 2
                  addToggle(tSel,dTrn*2)
                  
                Case 16,17,18,19,20 ; brush style
                  If tSel<>dSel
                    addToggle(dSel,0)
                    dSel=tSel
                    addToggle(dSel,6)
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
        EndSelect
        
        ; reset mouse action
        mact=-1
      EndIf 
      
      ;-------- update screen
      
      If StartDrawing(CanvasOutput(0))
        
        ; update drawing area
        If mact<>9
          DrawImage(ImageID(l(dLay)\IMG),MA(0)\lx,MA(0)\ly)
          ; show stats
          showstats()
          
        EndIf
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
    
  Else
    Delay(1)
  EndIf
  
  
Until Event = #PB_Event_CloseWindow

End


;-------- Data Section --------

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
  Data.w 386,526,262,39
  Data.s "Pattern Select"
  Data.w 0,524,72,75
  Data.s "Brush Size"
  Data.w 750,60,204,140
  Data.s "Stats"
  Data.w 72,524,100,52
  Data.s "Selected Pattern"
  Data.w 750,446,44,44
  Data.s "Tool Strip"
  Data.w 648,24,100,550
  Data.s "Fill Colour"
  Data.w 750,526,44,44
  Data.s "Pattern Select2"
  Data.w 0,392,586,186
  
  
  ; inline toolstrip bmps
  ToolStripMain:       : IncludeBinary #PB_Compiler_FilePath + "/TOOLSTRIPEXA.BMP"
  ToolStripMain2:      : IncludeBinary #PB_Compiler_FilePath + "/TOOLSTRIP2EXA.BMP"
EndDataSection


; IDE Options = PureBasic 5.62 (Windows - x86)
; CursorPosition = 719
; FirstLine = 716
; Folding = -----
; EnableXP
; UseIcon = Art-icon.ico
; Executable = ART_EXA_PB_011_x86.exe
; DisableDebugger
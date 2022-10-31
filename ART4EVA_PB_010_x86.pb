;
; ------------------------------------------------------------
;
;   ART 4 Eva - Suits 800x600 screens ( - 24 for menu)
;
;    (c) Fourthstone 2020
;
; ------------------------------------------------------------
;

; TODO 29/11/2020

;* Directional Gradient Fills

;* Ellipses

;* Spray brush - would be nice To be able To alter the flow And maybe more paint centred in the middle / less around the edges

;* Tracing layer with variable transparency, ability to load a graphic file and scale as an overlay tracing guide
;  Partially implemented, need to experiment with it at different layer etc.

;* Add size indicator for brush size, added as test

;* Add ms indicator for flashing slide, added as test

;* gradients:  https://www.youtube.com/watch?v=973fiFaSXqw&t=1634s

;* autosave option, timer since last save, project folder similar to telepaint?

;* Implement line connecter tool

;* glitch effects, smear line of pixels on an axis, negate palette values

;* kaleidescope, mirror / flip corners of a circular region

; png file support
UsePNGImageEncoder()
UsePNGImageDecoder()

; constants
#scrW=800; 960
#scrH=800
#drwW=640
#drwWloop=#drwW-1
#drwH=512
#drwHloop=#drwH-1
#maxUndo=32
#SCRNsize=163839 ; screen buffer array size
#RAWsize=20480

; tool strip constants
#toolUndo=0
#toolRedo=1
#toolDraw=2
#toolLoad=3
#toolErase=4
#toolSave=5
#toolLine=6
#toolQSCur=7
#toolBox=8
#toolQSAll=9
#toolCircle=10
#toolCLS=11
#toolGradient=12
#toolCLSall=13
#toolFill=14
#ToolCopy=15
#toolTransform=16
#ToolPaste=17
#toolZoom=18
#toolDither=19
#toolTransparent=20
#toolBrushReflect=21
#toolBrushAnimate=22
#toolBrushNone=23


; menu image constants
#menuTools=0
#menuOptions=1
#menuAnimate=2
#menuAnimButtons=3
#menuCloseWindow=4
#menuHelp=5

#subDraw=0
#subLine=1
#subBox=2
#subCircle=3
#subGrad=4
#subFill=5
#subTran=6

; draw brush types 
#toolBrushBox=0
#toolBrushRound=1
#toolBrushLeft=2
#toolBrushRight=3
#toolBrushVert=4
#toolBrushHorz=5
#toolBrushFlash=6
#toolBrushSpare=7
#toolBrushRND=8
#toolBrushSPR=9

#toolBrush2x=3

; line tools
#toolLineLine=0
#toolLineConnect=1
#toolLineFlash=2

; box tools
#toolBoxOut=0
#toolBoxfill=1
#toolBoxEmpty=2
#toolBoxFlashLeft=3
#toolBoxFlashRight=4

; circle tools
#toolCirOut=0
#toolCirFill=1
#toolCirEmpty=2
#toolCirFlashLeft=3
#toolCirFlashRight=4

; gradient tools
#toolGradHor=0
#toolGradVer=1
#toolGradTL=2
#toolGradTR=3
#toolGradBR=4
#toolGradBL=5

; fill tools
#toolFillFill=0
#toolFillReplace=1

; animate menu select items
#toolAnimate=0
#toolAniCycle=1
#toolAniPing=2
#toolAniExport=3


; gadget area constants
#GA_MainCanvas=0
#GA_TSButtons=1
#GA_TSPalette=2

; mouse area list index constants
#MA_Drawing=3
#MA_ToolBar=4
#MA_ColSel=5
#MA_OptionShow=6
#MA_BrushSize=7
#MA_Stats=8
#MA_SelectedPat=9
#MA_FillCol=10
#MA_PatSel=11
#MA_AniSel=12
#MA_Layers=13
#MA_SpriteShow=14
#MA_AnimateShow=15
#MA_FlashSpeed=16
#MA_FlashDraw=17
#MA_FlashCycle=18
#MA_ToolAnimate=19
#MA_FlashLen=20
#MA_FlashGap=21

; mouse options index
#MO_OptThin=0
#MO_OptThick=1
#MO_OptPixel=2
#MO_OptOff1=3
#MO_OptGuide=4

#MO_OptMode0=5
#MO_OptMode1=6
#MO_OptMode2=7
#MO_OptMode3=8
#MO_OptMode4=9
#MO_OptMode5=10
#MO_OptMode6=11
#MO_OptMode7=12

#MO_OptGrid=13
#MO_OptTrace=14
#MO_OptTrnVal=15

#MO_OptFlashMin=16
; #MO_OptFlashGap=17


; structures
Structure MouseArea
  lx.w      ; left edge x (top left)
  ly.w      ; left edge y (top left)
  rx.w      ; right edge x
  ry.w      ; right edge y
  w.w       ; width rx-lx-1
  h.w       ; height ry-ly-1
  active.w  ; is mouse area active, used for dynamic menus
  gad.w
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
  d.a
EndStructure

Structure undoArray
  buf.a[#SCRNsize+1]
EndStructure

Structure copyArray
  buf.a[#SCRNsize+1]
  w.w
  h.w
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

Structure Submenu
  y.i
  b.i
  sel.i
  img.i
EndStructure

; globals
Global pCol.a=0 ; pattern colour selected
Global pSel.a=8 ; pattern selected
Global pGrd.a=0 ; diagonal gradient colour

Global dCol.a=1 ; drawing colour
Global dTrn.a=1 ; drawing transparency toggle
Global dDth.a=0 ; drawing dither toggle
Global dZom.a=0 ; drawing zoom toggle
Global dAni.a=0 ; drawing animate toggle
Global bAni.a=0 ; drawing animate current colour
Global dRef.a=0 ; drawing refelct
Global dWid.a=8 ; drawing width
Global dSel.a=#toolBrushBox; drawing brush selected
Global dCon.a=0            ; line connector
Global dFil.i=0            ; fill colour
Global dAir.a=0            ; air brush selector
Global tFont1.i, tFont2.i  ; font index

Global flashing.b=0           ; flashing colour toggle
Global flashMin.b=1           ; flash colour starting index
Global flashBak=8             ; flashing colour index background
Global flashFor=7             ; flashing colour index foreground
Global flashCol=8             ; flashing index for drawing
Global flashAnim=0            ; flash animation toggle
Global flashAniOld=0          ; flash animation toggle
Global flashCycle=8           ; flash cycle counter
Global flashDgap=0            ; flashing colour pixel gap for line and circle tools
Global flashDgapCount=0       ; flashing colour pixel gap counter for line And circle tools
Global flashDlen=1            ; flashing colour pixel length for line and circle tools
Global flashDlenCount=1       ; flashing colour pixel length counter for line And circle tools
Global fSpeed=540             ; flash speed in ms
Global fSpdOld=540            ; flash speed in ms
Global animExport=-1          ; export animation frames
Global animFile.s=""          ; export filename
Global animSave=0             ; frame save flag
Global animType=0             ; animation selector
Global animTypeOld=0          ; animation selector
Global animDir=1              ; animation direction
Global savePattern=0          ; save file pattern selector
Global loadPattern=0          ; load file pattern selector
Global fox,foy                ; flashing ox,oy

Global dMdx=160               ; current mode horizontal pixels
Global dMdy=256               ; current mode vertical pixels
Global dMpx=4                 ; current mode horizontal pixel size
Global dMpy=2                 ; current mode vertical pixel size
Global dBits=8                ; current mode colour divisor
Global dLay=0                 ; current drawing layer
Global dLayOld=0              ; old drawing layer
Global dAll=1                 ; all layers visible flag
Global dWire.a=0              ; draw wireframe ??? what is the function of this
Global dGRD.a=0               ; draw transparent grid
Global dDSP.a=0               ; main canvas screen display selector: 0=paint, 1=spr drawing, 2=options
Global dSED.a=0               ; editing spr
Global dOVL.a=0               ; palette overlay, 0=palette visible, 1=animate UI visible
Global dShift.a=0             ; Shift key flag
Global dCtrl.a=0              ; ctrl key flag
Global dMode.a=2              ; current mode

Global tCur.a=#toolDraw       ; tool selected
Global gCur=0                 ; current gadget event
Global tTog.a=3               ; tool toggle colour
Global tQSA.a=0               ; quick save all toggle
Global tQSC.a=0               ; quick save current toggle                  
Global tSel.a=0               ; new tool select
Global sSel=-1                ; new sub menu selector
Global tHi=-1                 ; tool highlight
Global tOld=-1                ; old tool highlight
Global tAni=-1                ; animate menu highlight
Global tSub=-1                ; sub menu visble
Global tGrd=0                 ; grid layer options
Global tGTR.a=127             ; trace layer transparency
Global tLIF.a=0               ; load image flag
Global copyState=0            ; copy start new flag 
Global copysx=-1              ; copy start x
Global copysy                 ; copy start y
Global copyex                 ; copy x size
Global copyey                 ; copy y size
Global copybx                 ; copy box x start
Global copyby                 ; copy box y start
Global tZoom = 1              ; drawing zoom value - binary stepped 1,2,4,8
Global scroll_x = 0           ; image scroll x for zoom
Global scroll_y = 0           ; image scroll y for zoom

Global oCrossHair.a=2         ; cross hair option flag
Global oMouseGuide.a=1        ; mouse guide option flag

Global mx,my,dx,dy,px,py,opx,opy,ox,oy,sx,sy,mact ; mouse x,y,action
Global imgFinal, imgGRD, imgTraceLayer    ; image handles
Global Dim imgHandle(10)                  ; menu and toolbar handle array
Global Dim subM.Submenu(6)                ; sub menu array

Global savePattern=0                                  ; save file pattern selector
Global loadPattern=0                                  ; load file pattern selector
Global imgOverlay.Overlay                             ; image menu overlay
Global SCRNcopy.copyArray                             ; copy paste buffer object
Global SCRNtemp.copyArray                             ; temporary buffer object

Global Dim dl.drawLayers(4) ; layers array
Global Dim SCRNout.a(#SCRNsize) ; final output buffer

Global Dim pat.a(17,15) ; drawing patterns
Global Dim bp(16)       ; beeb palette

Global Dim grad(7)      ; gradient colour indexes
Global grad_drag=-1     ; which gradient is dragging

Global Dim animTable(14); animation colour sequence table
Global anim_drag=-1     ; which animation col is dragging

Global Dim rgbT.rgbTable(16)     ; rgb lookup table
Global Dim ct(16)                ; colour table look for redrawing main canvas
Global Dim bpFlash(15)           ; flash palette 0-7 phase 1, 8-15 phase 2
Global Dim rawBBC.a(15)          ; raw bbc file format data
Global Dim revBBC.a(85)          ; reverse lookup bbc file format data
Global Dim curPat.a(15,15)       ; current drawing pattern colour matrix
Global Dim cusPat.a(17,63)       ; custom pattern array, 18 tiles of 8x8 colour data

; lists
Global NewList MA.MouseArea()
Global NewList MO.MouseArea()

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

; select mouse area by index
Procedure selectMA(i.i)
  If i<>ListIndex(MA())
    SelectElement(MA(),i)
  EndIf
EndProcedure

; select mouse area by index
Procedure selectMO(i.i)
  If i<>ListIndex(MO())
    SelectElement(MO(),i)
  EndIf
EndProcedure

; create standard gadget
Procedure CreateGadget(gIndex,gAttribute)
  selectMA(gIndex)
  CanvasGadget(gIndex,MA()\lx,MA()\ly,MA()\w,MA()\h)
  SetGadgetAttribute(gIndex, #PB_Canvas_Cursor , gAttribute)
EndProcedure

; set pixel coords For relative To draw window
Procedure setP(x,y)
  opx=px
  opy=py
  selectMA(#MA_Drawing)
  px=(x-MA()\lx) / (dMpx*tZoom)
  ;py=255-((y-MA()\ly) / dMpy)
  py=(#drwHloop-(y-MA()\ly)) / (dMpy*tZoom)
  ;py=(y-MA()\ly) / dMpy
EndProcedure

Procedure px1(x)
  ProcedureReturn (x-MA()\lx) / dMpx
EndProcedure
Procedure py1(y)
  ;ProcedureReturn 255-((y-MA()\ly) / dMpy)
  ProcedureReturn (#drwHloop-(y-MA()\ly)) / dMpy
EndProcedure

; display program stats
Procedure showstats()
  Protected x,y,g
  x=24
  y=12
  g=56
  setP(mx,my)
  
  selectMA(#MA_Stats)
  
  Box(MA()\lx+x,MA()\ly,32,54,bp(0))
  Box(MA()\lx+x+g,MA()\ly,32,54,bp(0))
  ;Box(MA()\lx+x+160,MA()\ly,32,54,bp(0))
  ;Box(MA()\lx+x+240,MA()\ly,32,54bp(0))
  
  DrawingFont(FontID(tFont1)) 
  
  DrawText(MA()\lx+x,MA()\ly,Str(mx))
  DrawText(MA()\lx+x,MA()\ly+y,Str(my))
  DrawText(MA()\lx+x,MA()\ly+y*2,Str(mact))
  DrawText(MA()\lx+x,MA()\ly+y*3,Str(fSpeed))
  
  DrawText(MA()\lx+x+g,MA()\ly,Str(px))
  DrawText(MA()\lx+x+g,MA()\ly+y,Str(py))
  DrawText(MA()\lx+x+g,MA()\ly+y*2,Str(dWid))
  DrawText(MA()\lx+x+g,MA()\ly+y*3,Str(SCRNcopy\w))
  
  
  ;DrawText(MA()\lx+x+160,MA()\ly,Str(gCur))
  ;DrawText(MA()\lx+x+160,MA()\ly+16,Str(dDSP))
  
EndProcedure

; draw box outline, assumes startdrawing is already active
Procedure drawBox(x1,y1,x2,y2,c)
  FrontColor(c)
  LineXY(x1,y1,x2,y1)
  LineXY(x2,y1,x2,y2)
  LineXY(x1,y2,x2,y2)
  LineXY(x1,y1,x1,y2)
EndProcedure

; draw dashed box outline, assumes startdrawing is already active
Procedure drawBoxDash(x1,y1,x2,y2,c,l,g)
  Protected x,y
  
  FrontColor(c)
  If x1>x2 : Swap x1,x2 : EndIf
  If y1>y2 : Swap y1,y2 : EndIf
  
  For x=x1 To x2 Step 2
    If x>-1 And x<#drwW
      If y1>-1 And y1<#drwH
        Plot(x,y1)
      EndIf
      If y2>-1 And y2<#drwH
        Plot(x,y2)
      EndIf
    EndIf
    
  Next
  
  For y=y1 To y2 Step 2
    If y>-1 And y<#drwH
      If x1>-1 And x1<#drwW
        Plot(x1,y)
      EndIf
      If x2>-1 And x2<#drwW
        Plot(x2,y)
      EndIf
      
    EndIf
    
  Next
  
EndProcedure


; check if mouse is in range of mouse area object, return '1' if in range
Procedure rangeApp(i)
  Protected r.a=0
  
  selectMA(i)
  If gCur=MA()\gad And mx>=MA()\lx And mx<=MA()\rx And my>=MA()\ly And my<=MA()\ry ;  gCur=MA()\gad And
    r=1
  EndIf
  ProcedureReturn r
EndProcedure

; check if mouse is in range of mouse area object, return '1' if in range
Procedure rangeOpt(i,x,y)
  Protected r.a=0
  
  selectMO(i)
  If x>=MO()\lx And x<=MO()\rx And y>=MO()\ly And y<=MO()\ry
    r=1
  EndIf
  ProcedureReturn r
EndProcedure

; drawing brush - generic routine - tesselated patterns
Procedure dBrush(dx,dy,w,d)
  Protected lx,ly,dc,s
  Protected r1,r2
  
  Select dDth
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
      
      ; range check
      If lx>-1 And lx<dMdx And ly>-1 And ly<dMdy
        
        ; set pattern colour And plot, 3 = erase
        If d<3
          dc=curPat(lx % 16,15-(ly % 16))
          If dAni=1 And dc>0 : dc=bAni : EndIf
        Else
          dc=0
        EndIf
        
        ; check For transparency
        If dc-dTrn>-1 Or d=3 
          dl(dLay)\SCRN[ly*#drwW+lx]=dc
          If dRef=1
            dl(dLay)\SCRN[ly*#drwW+(dMdx-lx-1)]=dc        
            dl(dLay)\SCRN[(dMdy-ly-1)*#drwW+lx]=dc
            dl(dLay)\SCRN[(dMdy-ly-1)*#drwW+(dMdx-lx-1)]=dc
          EndIf
          
        EndIf

      EndIf
    Next
  Next
  
EndProcedure

; drawing brush Zoom - single pixel editing with current colour selected
Procedure dBrushZoom(dx,dy)
  Protected lx,ly,dc,s
  Protected r1,r2
  
  dx=dx+scroll_x
  dy=dy+scroll_y
      
  dl(dLay)\SCRN[dy*#drwW+dx]=dCol
  
EndProcedure

; drawing brush - SPR at any loc
Procedure dBrushSPR(dx,dy,w,d,oP)
  Protected lx,ly,dc,s
  
  Select dDth
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
            ;Box(lx*dMpx,504-ly*dMpy,dMpx,dMpy,bp(dc))
            Box(lx*dMpx,(255-ly)*dMpy,dMpx,dMpy,bp(dc))
          Else
            dl(dLay)\SCRN[ly*#drwW+lx]=dc
          EndIf
        EndIf
      EndIf
    Next
  Next
  
EndProcedure

Procedure dBrushPaste(dx,dy,oP)
  Protected lx,ly,dc,s,dw,dh,x1
  
  dw=SCRNcopy\w
  dh=SCRNcopy\h
  ;dx=((dx-dw / 2) / 2)*2
  ;dy=((dy-dh) / 2)*2  
  x1=0
  
  ; draw pattern loop centered at pixel location dx,dy
  For ly=dy To dy+dh
    For lx=dx To dx+dw
      ; range check, set pattern colour And plot
      If lx>-1 And lx<dMdx And ly>-1 And ly<dMdy
        dc=SCRNcopy\buf[x1]
        
        ; check For transparency
        If dc-dTrn>-1
          If op 
            Box(lx*dMpx,(255-ly)*dMpy,dMpx,dMpy,bp(dc))
          Else
            dl(dLay)\SCRN[ly*#drwW+lx]=dc
          EndIf
        EndIf
      EndIf
      x1+1
    Next
  Next
  
EndProcedure

; update current pattern template
Procedure updateBrush()
  Protected px,py,lx,ly,dc,ps,s,h
  
  ; set skip counter  
  If dDth
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
  
  
  ;draw pattern loop 32,16,8,4
  selectMA(#MA_SelectedPat)
  s=32/dMpx-1 ;s=(4/dMpx)*9-1
  h=32/dMpy-1 ;(old value 17)
  
  
  ; update brush view on tools palette
  For lx=0 To s
    For ly=0 To h
      px=lx % 16
      py=ly % 16
      If curPat(px,py)=16 
        dc=8
      Else
        dc=curPat(px,py) % dBits
      EndIf
      Box(MA()\lx+lx*dMpx+6,MA()\ly+ly*dMpy+6,dMpx,dMpy,bp(dc))
    Next
  Next
  
EndProcedure

; update palette, assumes startdrawing is already active
Procedure updatePalette()
  Protected i,p,x,dc,p1,i1,xd,yd,s,h,lx,ly,px,py
  
  If dOVL=0
    selectMA(#MA_PatSel)
    dc=bp(1)
    s=32/dMpx-1
    h=16/dMpy-1
    py=MA()\ly
    
    Box(MA()\lx,MA()\ly,MA()\w,MA()\h,bp(0))
    drawBox(MA()\lx+1,MA()\ly+1,MA()\rx-1,MA()\ry-1,bp(7))
    
    For i=0 To 8          ; colour loop 0 = bottom - 7 = top
                          ; preset plot positions
      i1=MA()\ly+182-i*22
      For p=0 To 17       ; pattern number loop left to right
                          ; preset plot positions
        p1=MA()\lx+4+p*32
        If i<8            ; branch for custom patterns
          
          For ly=0 To h
            For lx=0 To s
              If pat(p,(lx % 4)+(ly % 4)*4) 
                dc=bp(i % dBits)
              Else
                If i=0 And dCol=0
                  dc=bp(8)
                Else
                  dc=bp(dCol % dBits)
                EndIf
              EndIf
              Box(p1+lx*dMpx,i1+ly*dMpy,dMpx,dMpy,dc)
            Next
          Next        
          
        Else
          ; custom pattern loop
          For ly=0 To h
            For lx=0 To s
              ;Box(p1+(x % 8)*dMpx,i1+(x / 8)*dMpy,dMpx,dMpy,bp(cusPat(p,(lx % 16)+(ly % 16)*16)))
              Box(p1+lx*dMpx,i1+ly*dMpy,dMpx,dMpy,bp(cusPat(p,(lx % 8)+(ly % 8)*8)))
            Next
          Next        
        EndIf
      Next      
    Next
    
    ; highlight selected pattern and update selected brush size pattern
    Box(MA()\lx+pSel*32+4,MA()\ry-22-pCol*22+4,32,2,bp(7))
  EndIf

  
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
        If dAni=1 And dc>0 : dc=bAni : EndIf
        
        ; Check For transparency
        If dc-dTrn>-1
          dl(dLay)\SCRN[ly*#drwW+x1]=dc
          If dRef=1
            dl(dLay)\SCRN[ly*#drwW+(dMdx-x1-1)]=dc        
            dl(dLay)\SCRN[(dMdy-ly-1)*#drwW+x1]=dc
            dl(dLay)\SCRN[(dMdy-ly-1)*#drwW+(dMdx-x1-1)]=dc
          EndIf
          
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
        If dAni=1 And dc>0 : dc=bAni : EndIf
        
        ; Check For transparency
        If dc-dTrn>-1 
          dl(dLay)\SCRN[y1*#drwW+lx]=dc
          If dRef=1
            dl(dLay)\SCRN[y1*#drwW+(dMdx-lx-1)]=dc        
            dl(dLay)\SCRN[(dMdy-y1-1)*#drwW+lx]=dc
            dl(dLay)\SCRN[(dMdy-y1-1)*#drwW+(dMdx-lx-1)]=dc
          EndIf
          
        EndIf
      EndIf
    Next
  EndIf
  
EndProcedure

; line drawing brush
Procedure dLine(x1,y1,x2,y2,dW)
  
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
  dWh=dW/2
  
  ; Draw starting segment
  dBrush(x1,y1,dW,0)
  
  ; draw line loop
  Repeat
    If x1=x2 And y1=y2 : Break: EndIf
    e2=2 * err
    If e2>-dy
      err-dy
      x1+sx
      Vline2(x1+dWh * sx,y1-dW,y1+dW)
    EndIf
    If e2<dx
      err+dx
      y1+sy
      Hline2(x1-dWh,x1+dWh,y1+dW * sy)
    EndIf
    If x1<-dW Or x1>dMdx+dW : Break : EndIf
    If y1<-dW Or y1>dMdy+dW : Break : EndIf
    ;PROCreadmouse
  Until 0
  
EndProcedure

; circle spray brush
Procedure dCircleSpray(x1,y1,r)
  Protected r2,dy,x,px1,px2,py1,py2
  
  r=Abs(r)
  r2=r*r
  
  For x=r To 0 Step -2
    dy=Sqr(r2-x*x)
    setP(x1-x,Int(y1-dy))
    px1=px : py1=py
    
    setP(x1+x,Int(y1+dy))
    px2=px : py2=py  
    
    For ly=py2 To py1
      If ly>-1 And ly<dMdy
        If px1>-1 And px1<dMdx
          If Random(1000)<15
            dc=curPat(px1 % 16,15-(ly % 16))
            
            ; Check For transparency
            If dc-dTrn>-1
              dl(dLay)\SCRN[ly*#drwW+px1]=dc
              If dRef=1
                dl(dLay)\SCRN[ly*#drwW+(dMdx-px1-1)]=dc        
                dl(dLay)\SCRN[(dMdy-ly-1)*#drwW+px1]=dc
                dl(dLay)\SCRN[(dMdy-ly-1)*#drwW+(dMdx-px1-1)]=dc
              EndIf
              
            EndIf
          EndIf
        EndIf
        If px2>-1 And px2<dMdx
          If Random(1000)<15
            dc=curPat(px2 % 16,15-(ly % 16))        
            ; Check For transparency
            If dc-dTrn>-1
              dl(dLay)\SCRN[ly*#drwW+px2]=dc
              If dRef=1
                dl(dLay)\SCRN[ly*#drwW+(dMdx-px2-1)]=dc        
                dl(dLay)\SCRN[(dMdy-ly-1)*#drwW+px2]=dc
                dl(dLay)\SCRN[(dMdy-ly-1)*#drwW+(dMdx-px2-1)]=dc
              EndIf
              
            EndIf
          EndIf
        EndIf
        
      EndIf
    Next
  Next
  
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
      Case 1 ; standard X2 brush
        setP(Int(x1+r*Cos(Radian(t))),Int(y1-r*Sin(Radian(t))))
        dBrush(px,py,dWid,1)
      Case 2 ; airbrush
        setP(Int(x1+r*Cos(Radian(t))),Int(y1-r*Sin(Radian(t))))        
        dBrush(px,py,dWid,2)
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
  
  If f=0
    ; filled
    For lx=x1-(dWid / 2) To x2+(dWid / 2)
      Vline2(lx,y1-dWid,y2+dWid)
    Next
  Else
    ; outline
    For lx=-(dWid / 2) To (dWid / 2)
      Vline2(lx+x1,y1-dWid,y2+dWid)
      Vline2(lx+x2,y1-dWid,y2+dWid)
    Next
    For lx=-dWid To dWid
      Hline2(x1-(dWid / 2),x2+(dWid / 2),lx+y1)
      Hline2(x1-(dWid / 2),x2+(dWid / 2),lx+y2)
    Next
    
    ; empty
    If f=2
      For lx=x1+(dWid / 2) To x2-(dWid / 2)
        Vline2(lx,y1-dWid,y2+dWid)
        For ly=y1+dwid To y2-dwid
          dl(dLay)\SCRN[ly*#drwW+lx]=0 
          If dRef=1
            dl(dLay)\SCRN[ly*#drwW+(dMdx-lx-1)]=0        
            dl(dLay)\SCRN[(dMdy-ly-1)*#drwW+lx]=0
            dl(dLay)\SCRN[(dMdy-ly-1)*#drwW+(dMdx-lx-1)]=0
          EndIf
          
        Next
      Next
    EndIf
    
  EndIf
  
EndProcedure

; box draw gradient, dWid=0 horizontal, dWid=1 vertical
Procedure dBoxG(x1,y1,x2,y2,d,op)
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
      ; range check, set pattern colour And plot (old 160x256)
      If lx>-1 And lx<dMdx And ly>-1 And ly<dMdy 
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
          If op
            Box(lx*dMpx,(255-ly)*dMpy,dMpx,dMpy,bp(dc))
          Else
            dl(dLay)\SCRN[ly*#drwW+lx]=dc
            If dRef=1
              dl(dLay)\SCRN[ly*#drwW+(dMdx-lx-1)]=dc        
              dl(dLay)\SCRN[(dMdy-ly-1)*#drwW+lx]=dc
              dl(dLay)\SCRN[(dMdy-ly-1)*#drwW+(dMdx-lx-1)]=dc
            EndIf
            
          EndIf
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

; box draw gradient 2, dWid=0 horizontal, dWid=1 vertical
; gradient steps 18 gradient slices per 8 steps = 144 
; create an array with 144 elements and calculate
; e.g. 0,x,x,x,x,x,x,1  - two colour gradient
; inc pattern every 8 steps
; 

; e.g. 0,x,x,x,1,x,x,2 - three colour gradient
; inc first patten every 5 steps, second pattern every 3 steps
; 

; second approach:
; each step count determine lenght of step
; step count = total length / total steps * step count
; e.g. 8 step :  160 / 8 * 8=160
; e.g. 2 step + 6 step :  160 / 8 * 2=40, 160 / 8 *6=120

; each step count needs to then be divided into 18 to attain the gradient step
; e.g. : 160 / 18 = 8.888889  : 1/8.888889 = .1125 : how many pixel steps to count for each gradient change
; e.g. : 40 / 18 = 2.22222 : 1/2.22222= .45  :  120 / 18 = 6.6666667  : 1/6.6666667=.15

Procedure dBoxG2(x1,y1,x2,y2,d,op)
  
  Protected lx,ly,dc,gR.f,gRd.f,gAdd.f,dv,cnt,c1,c2,tl,sl,sx,sy
  Protected Dim steps.f(7)
  Protected Dim gx(7,1)
  Protected Dim gs(7)
  Protected Dim gc(7,1)
  
  setP(x1,y1)
  x1=px : y1=py
  
  setP(x2,y2)
  x2=px : y2=py
  
  dv=1 : gR=0 : gRd=0
  If x1>x2
    Swap x1,x2
  EndIf
  If y1>y2
    Swap y1,y2
  EndIf
  
  For lx=0 To 7
    If grad(lx)>-1 : cnt+1 : EndIf
  Next
  
  ; ensure at least 2 elements exist
  If cnt>1 
    If d
      tl=y2-y1+1
    Else
      tl=x2-x1+1
    EndIf
        
    cnt=0
    For lx=0 To 7
      If grad(lx)>-1
        If cnt=0
          c1=lx
          cnt+1
        Else
          c2=lx
          
          ;gs(cnt-1)=c2-c1+1
          steps(cnt-1)=1/((tl / 7 * (c2-c1))/18)
          If d
            gx(cnt-1,0)=tl / 7 * c1 +y1
            gx(cnt-1,1)=tl / 7 * c2 +y1
          Else
            gx(cnt-1,0)=tl / 7 * c1 +x1
            gx(cnt-1,1)=tl / 7 * c2 +x1
          EndIf
          
          gc(cnt-1,0)=grad(c1)
          gc(cnt-1,1)=grad(c2)
          
          c1=c2
          cnt+1
          
        EndIf
      EndIf
    Next
    
    cnt-2
    
;       DrawText(10,20,"gx,gy   : "+Str(gx(0,0))+","+Str(gx(0,1))+"   ",bp(7),bp(1))
;       DrawText(10,50,"gc1,gc2 : "+Str(gc(0,0))+","+Str(gc(0,1))+"   ",bp(7),bp(1))
;       DrawText(10,80,"gAdd    : "+StrF(steps(0))+"   ",bp(7),bp(1))
;       DrawText(10,110,"tLen    : "+StrF(tl)+"   ",bp(7),bp(1))
;       DrawText(10,140,"cnt     : "+Str(cnt)+"   ",bp(7),bp(1))

    
    For sx=0 To cnt
      
      ; Calculate direction vector, gradient value And gradient Default values
      
      gAdd=steps(sx)
      gR=0
      
      If d
        y1=gx(sx,0)
        y2=gx(sx,1)
      Else
        x1=gx(sx,0)
        x2=gx(sx,1)
      EndIf
      
      For lx=x1 To x2
        If d : gR=gRd : EndIf
        For ly=y1 To y2
          ; range check, set pattern colour And plot (old 160x256)
          If lx>-1 And lx<dMdx And ly>-1 And ly<dMdy 
            pS=lx % 4+(ly % 4)*4
            If pat(Int(gR),pS)
              dc=gc(sx,1)
            Else
              dc=gc(sx,0)              
            EndIf
            
            ; Check For transparency
            If dc-dTrn>-1
              If op
                Box(lx*dMpx,(255-ly)*dMpy,dMpx,dMpy,bp(dc))
              Else
                dl(dLay)\SCRN[ly*#drwW+lx]=dc
                If dRef=1
                  dl(dLay)\SCRN[ly*#drwW+(dMdx-lx-1)]=dc        
                  dl(dLay)\SCRN[(dMdy-ly-1)*#drwW+lx]=dc
                  dl(dLay)\SCRN[(dMdy-ly-1)*#drwW+(dMdx-lx-1)]=dc
                EndIf
                
              EndIf
            EndIf
          EndIf
          If d=1
            gR+gAdd
            If gR>17.999 : gR=17.999 : EndIf
          EndIf
          
        Next
        If d=0 
          gR+gAdd
          If gR>17.999 : gR=17.999 : EndIf
        EndIf

      Next
    Next
    
  EndIf

EndProcedure

; draw a diagonal line using pattern 'p' and global var pGrd as index into pattern 'p'
Procedure dlinepat(x1,y1,x2,y2,p,c1,c2)
  Protected dx,dy,sx,sy,e,c
  
  dx=Abs(x2-x1)
  sx=Sign(x2-x1)
  
  dy=Abs(y2-y1)
  sy=Sign(y2-y1)
  
  If dx>dy
    e=dx / 2
  Else
    e=dy / 2
  EndIf
  
  Repeat
    t=0
    If Random(10)=1 : t=1 : EndIf
    
    pGrd=(pGrd+1-t) % 16
    If pat(p,pGrd)
      c=c2
    Else
      c=c1
    EndIf
    
    dl(dLay)\SCRN[y1*#drwW+x1]=c
    If dRef=1
      dl(dLay)\SCRN[y1*#drwW+(dMdx-x1-1)]=c        
      dl(dLay)\SCRN[(dMdy-y1-1)*#drwW+x1]=c
      dl(dLay)\SCRN[(dMdy-y1-1)*#drwW+(dMdx-x1-1)]=c
    EndIf
    
    
    If x1=x2 And y1=y2 : Break : EndIf
    If dx>dy
      x1+sx
      e-dy
      If e<0
        e+dx
        y1+sy
      EndIf
    Else
      y1+sy
      e-dx
      If e<0
        e+dy
        x1+sx
      EndIf
    EndIf
  Until 0
  
EndProcedure

; plot a diagonal gradient
Procedure dBoxG2_diag(x1,y1,x2,y2,d,op)
  
  ;       DEF PROCdiagonal_g(x1%,y1%,x2%,y2%,p%,d%)
  ;       LOCAL h%,v%,l%,gR,gAdd,min%,max%,L%
  
  Protected lx,ly,dc,gR.f,gRd.f,gAdd.f,dv,cnt,c1,c2,tl,sl,sx,sy
  Protected xmin,xmax,ymin,ymax,gstep,v,h
  Protected Dim steps.f(7)
  Protected Dim gx(7,1)
  Protected Dim gs(7)
  Protected Dim gc(7,1)
  
  
  setP(x1,y1)
  x1=px : y1=py
  
  setP(x2,y2)
  x2=px : y2=py
  
  dv=1 : gR=0 : gRd=0
  If x1>x2
    Swap x1,x2
  EndIf
  If y2>y1
    Swap y1,y2
  EndIf
  
  If x1<0 : x1=0 : EndIf
  If x1>159 : x1=159 : EndIf
  If y1<0 : y1=0 : EndIf
  If y1>255 : y1=255 : EndIf
  
  If x2<0 : x2=0 : EndIf
  If x2>159 : x2=159 : EndIf
  If y2<0 : y2=0 : EndIf
  If y2>255 : y2=255 : EndIf
  
  If x1<>x2 And y1<>y2
    
    xmin=x1
    xmax=x2
    ymin=y1
    ymax=y2
    
    Select d
      Case 0 : ; top left
        x2=x1
        y2=y1
        h=1
        v=-1
        
      Case 1 : ; top right
        x1=x2
        y2=y1
        h=-1
        v=-1
        
      Case 2 : ; bottom right
        x1=x2
        y1=y2
        h=-1
        v=1
        
      Case 3 ; bottom left
        x2=x1
        y1=y2
        h=1
        v=1
        
    EndSelect
    
    For lx=0 To 7
      If grad(lx)>-1 : cnt+1 : EndIf
    Next
    
    
    ; ensure at least 2 elements exist
    If cnt>1 
      
      tl=(xmax-xmin)+(ymin-ymax)+1
      
      cnt=0
      For lx=0 To 7
        If grad(lx)>-1
          If cnt=0
            c1=lx
            cnt+1
          Else
            c2=lx
            
            ;gs(cnt-1)=c2-c1+1
            steps(cnt-1)=1/((tl / 7 * (c2-c1))/18)
            gx(cnt-1,0)=tl / 7 * c1 +y1
            gx(cnt-1,1)=tl / 7 * c2 +y1
            
            gc(cnt-1,0)=grad(c1)
            gc(cnt-1,1)=grad(c2)
            
            c1=c2
            cnt+1
            
          EndIf
        EndIf
      Next
      
      cnt-2
      
      pGrd=0
      
      For sx=0 To cnt
        
        ; Calculate direction vector, gradient value And gradient Default values
        
        gAdd=steps(sx)
        gR=0
        
        Repeat
          Select d
            Case 0 : ; top left
              If y1>ymax
                y1+v
              Else
                x1+h
              EndIf
              
              If x2<xmax
                x2+h
              Else
                y2+v
              EndIf
              
            Case 1 : ; top right
              If y2>ymax
                y2+v
              Else
                x2+h
              EndIf
              
              If x1>xmin
                x1+h
              Else
                y1+v
              EndIf
              
            Case 2 : ; bottom right
              If y2<ymin
                y2+v
              Else
                x2+h
              EndIf
              
              If x1>xmin
                x1+h
              Else
                y1+v
              EndIf
              
            Case 3 : ; bottom left
              If y1<ymin
                y1+v
              Else
                x1+h
              EndIf
              
              If x2<xmax
                x2+h
              Else
                y2+v
              EndIf
              
          EndSelect
          gr+gAdd
          dlinepat(x1,y1,x2,y2,Int(gr),gc(sx,0),gc(sx,1))
          
          ;gstep ??
        Until gr+gAdd>17.999
        
        
      Next
      
    EndIf
    
    
  EndIf
  
EndProcedure


; draw highlight box for colour selector
Procedure drawColSel(oldDC,newDC)
  selectMA(#MA_ColSel)
  x=oldDC % 8*24+2
  y=oldDC / 8*24+2  
  drawBox(MA()\lx+x,MA()\ly+y,MA()\lx+x+24,MA()\ly+y+24,bp(0))
  dCol=newDC
  x=dCol % 8*24+2
  y=dCol / 8*24+2  
  
  drawBox(MA()\lx+x,MA()\ly+y,MA()\lx+x+24,MA()\ly+y+24,bp(7))
EndProcedure

; update colour select value
Procedure updateColSel(c.b)
  Protected oldC
  
  oldC=dCol
  If dcol<flashMin
    dCol-c
    If dcol=255:dCol=flashMin-1:EndIf
    If dcol=flashMin:dCol=0:EndIf
  Else
    dCol-c
    If dcol=(flashMin-1):dCol=15:EndIf
    If dcol=16:dCol=flashMin:EndIf
  EndIf
  
  If StartDrawing(CanvasOutput(#GA_TSPalette))     
    drawColSel(oldC,dCol)
    updatePalette()
    StopDrawing()
  EndIf
  If StartDrawing(CanvasOutput(#GA_TSButtons))
    updateBrush()
    StopDrawing()        
  EndIf
  
EndProcedure

; redraw colour select
Procedure resetColSel()
  selectMA(#MA_ColSel)
  
  Box(MA()\lx,MA()\ly,MA()\w,MA()\h,bp(0))
  
  ; first line colours 0-7, second line flashing colours 8-15
  For i=1 To 7
    Box(MA()\lx+4+i*24,MA()\ly+4,22,22,bp(i))
    Box(MA()\lx+4+i*24,MA()\ly+28,22,22,bp(i))
    LineXY(MA()\lx+4+i*24,MA()\ly+49,MA()\lx+25+i*24,MA()\ly+28,bp(7-i))
    FillArea(MA()\lx+14+i*24,MA()\ly+42,-1,bp(7-i))
  Next  
  Box(MA()\lx+4,MA()\ly+28,22,22,bp(7))
  LineXY(MA()\lx+4,MA()\ly+49,MA()\lx+25,MA()\ly+28,bp(0))
  FillArea(MA()\lx+8,MA()\ly+32,-1,bp(0))
  
  
  drawColSel(dCol,dCol)
EndProcedure

; update pattern colour 
Procedure updatepCol(c.b)
  Protected oldP
  oldP=pCol
  
  If StartDrawing(CanvasOutput(#GA_TSPalette))              
    pCol+c
    If pCol=8:pCol=0:EndIf
    If pCol=255:pCol=7:EndIf
    updatePalette()
    
    If mact=#MA_PatSel
      selectMA(#MA_PatSel)
      Box(MA()\lx+pSel*32+4,MA()\ry-22-oldP*22+4,32,2,bp(0))
      Box(MA()\lx+pSel*32+4,MA()\ry-22-pCol*22+4,32,2,bp(7))
    EndIf
    
    StopDrawing()
  EndIf  
  
  If StartDrawing(CanvasOutput(#GA_TSButtons))
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

; update flash colours
Procedure updateFlashColours(tool)
  
  If tool=#MA_FlashDraw
    c=flashBak
  Else
    c=flashFor
  EndIf  
  
  selectMA(tool)
  ;If fc<0:fc=8:EndIf
  
  Box(MA()\lx+2,MA()\ly+2,MA()\rx-MA()\lx-3,MA()\ry-MA()\ly-3,bp(c))
  
  If c=8 
    DrawingMode(#PB_2DDrawing_Transparent)
    DrawText(MA()\lx+6,MA()\ly+2,"D",bp(7))
    DrawingMode(#PB_2DDrawing_Default)
  EndIf
  
EndProcedure

; update highlight for flash colour selector
Procedure drawFlashSel(tool)
  selectMA(tool)
  i=(my+208) / 22
  If tool=#MA_FlashDraw
    c=flashBak
  Else
    c=flashFor
  EndIf
  
  If c<>i And i>-1 And i<9
    c=i
    If tool=#MA_FlashDraw
      flashBak=c
    Else
      flashFor=c
    EndIf
    
    If StartDrawing(CanvasOutput(#GA_TSPalette))    
      updateFlashColours(tool)
      StopDrawing()
    EndIf
  EndIf                    
  
EndProcedure

; update flash speed
Procedure updateFlashSpeed(t)
  
  x=t / 10
  If x>100
    x=100
  EndIf
  
  selectMA(#MA_FlashSpeed)
  Box(MA()\lx+1,MA()\ly+1,MA()\w-2,MA()\h-2,bp(0))
  Box(MA()\lx+1,MA()\ly+10,MA()\w-2,6,bp(1))
  LineXY(MA()\lx+2,MA()\ly+12,MA()\rx-2,MA()\ly+12,bp(7))
  Box(MA()\lx+x,MA()\ly+2,6,MA()\h-4,bp(7))
  Box(MA()\lx+x+2,MA()\ly+4,2,MA()\h-8,bp(8))
  DrawingFont(FontID(tFont1))
  DrawText(MA()\lx+28,MA()\ly+6,Right("00"+StrU(fSpeed),4)+"ms",bp(8))
EndProcedure

; draw flash colour picker menu
Procedure pickFlashColour(tool)
  ; draws to main canvas
  selectMA(#MA_Drawing)
  y=MA()\ry-208
  selectMA(tool)
  
  Box(MA()\lx-4,y,34,218,bp(0))
  drawbox(MA()\lx-3,y+1,MA()\lx+28,y+204,bp(15))
  For i=1 To 8
    Box(MA()\lx+1,y+5+i*22,24,18,bp(i))
  Next  
  
  If tool=#MA_FlashDraw
    c=flashBak
  Else
    c=flashFor
  EndIf  
  
  drawBox(MA()\lx-1,y+3+c*22,MA()\lx+26,y+24+c*22,bp(7))
  
EndProcedure

; show animate menu and button status, assumes start drawing active
; *NEW
; old proc name drawBrushtypeMenu
Procedure drawSubMenu(sm)
  Protected bx,by
  selectMA(#GA_MainCanvas)
  DrawingMode(#PB_2DDrawing_Outlined)
  
  y=subM(sm)\y
  
  Select sm
    Case 0 : ;double width draw menu
      x=MA()\rx-106
      bx=x+2+sSel % 2 * 50
      by=y+2+sSel / 2 * 50
      
    Default : ; everything else
      x=MA()\rx-56
      bx=x+2
      by=y+2+sSel*50
      
  EndSelect
  
  ; main canvas starts at y=4, toolbar starts at y=24
  
  
  DrawImage(ImageID(subM(sm)\img),x,y)
  If sSel>-1 
    Box(bx,by,46,46,bp(2)) 
  EndIf
   
EndProcedure


; drawing brush - flashing colour routine
Procedure flashBrush(dx,dy)
 
  ;range check, set pattern colour And plot
  If dx>-1 And dx<dMdx And dy>-1 And dy<dMdy
    If fox<>dx Or foy<>dy
      dl(dLay)\SCRN[dy*#drwW+dx]=flashCol
      If dRef=1
        dl(dLay)\SCRN[dy*#drwW+(dMdx-dx-1)]=flashCol        
        dl(dLay)\SCRN[(dMdy-dy-1)*#drwW+dx]=flashCol
        dl(dLay)\SCRN[(dMdy-dy-1)*#drwW+(dMdx-dx-1)]=flashCol
      EndIf
      
      flashCol+1
      If flashCol=16
        flashCol=flashMin
      EndIf
      fox=dx
      foy=dy
    EndIf
  EndIf
  
EndProcedure

; line drawing with flashing colours - single pixel
Procedure dLineFlash(x1,y1,x2,y2)
  
  Protected dx,dy,sx,sy
  Protected e2,err
  
  ; get pixel coords For line
  x1=px1(x1) : y1=py1(y1)
  x2=px1(x2) : y2=py1(y2)
  
  ; determine which vector To use For err
  dx=Abs(x2-x1)
  dy=Abs(y2-y1)
  If x1<x2 : sx=1 : Else : sx=-1 : EndIf
  If y1<y2 : sy=1 : Else : sy=-1 : EndIf
  err=dx-dy
  
  ; draw line loop
  Repeat
    ; check if flash gap is set to 0
    If flashDgapCount=0
      
      ; update pixel if flash gap=0
      If x1>-1 And x1<dMdx And y1>-1 And y1<dMdy
        dl(dLay)\SCRN[y1*#drwW+x1]=flashCol
        If dRef=1
          dl(dLay)\SCRN[y1*#drwW+(dMdx-x1-1)]=flashCol        
          dl(dLay)\SCRN[(dMdy-y1-1)*#drwW+x1]=flashcol
          dl(dLay)\SCRN[(dMdy-y1-1)*#drwW+(dMdx-x1-1)]=flashcol
        EndIf
        
      EndIf
      
      ; update flash length counter and reset gap, length and colour vars
      flashDlenCount-1
      If flashDlenCount=0
        flashDlenCount=flashDlen
        flashDgapCount=flashDgap
        flashCol+1
        If flashCol=16
          flashCol=flashMin
        EndIf
      EndIf
      
    Else 
      flashDgapCount-1
    EndIf
    
    
    If x1=x2 And y1=y2 : Break: EndIf
    e2=2 * err
    If e2>-dy
      err-dy
      x1+sx
    EndIf
    If e2<dx
      err+dx
      y1+sy
    EndIf
    If x1<-dWid Or x1>dMdx+dWid : Break : EndIf
    If y1<-dWid Or y1>dMdy+dWid : Break : EndIf
    
  Until 0
EndProcedure

; circle drawing with flashing colours - single pixel - Left
Procedure dCircOutFlash(x1,y1,r,d)
  Protected t,x2,y2,ox,oy
  
  r=Abs(r)
  For t=0 To 359
    x2=px1(x1+r*Cos(Radian(t)))
    y2=py1(y1-r*Sin(Radian(t)))
    
    If x2<>ox Or y2<>oy
      
      ;       If x2>-1 And x2<dMdx And y2>-1 And y2<dMdy
      ;         dl(dLay)\SCRN[y2*#drwW+x2]=flashCol
      ;       EndIf
      
      ; check if flash gap is set to 0
      If flashDgapCount=0
        
        ; update pixel if flash gap=0
        If x2>-1 And x2<dMdx And y2>-1 And y2<dMdy
          dl(dLay)\SCRN[y2*#drwW+x2]=flashCol
          If dRef=1
            dl(dLay)\SCRN[y2*#drwW+(dMdx-x2-1)]=flashCol        
            dl(dLay)\SCRN[(dMdy-y2-1)*#drwW+x2]=flashCol
            dl(dLay)\SCRN[(dMdy-y2-1)*#drwW+(dMdx-x2-1)]=flashCol
          EndIf
          
        EndIf
        
        ; update flash length counter and reset gap, length and colour vars
        flashDlenCount-1
        If flashDlenCount=0
          flashDlenCount=flashDlen
          flashDgapCount=flashDgap
          flashCol+d
          If flashCol=16
            flashCol=flashMin
          EndIf
          If flashCol<flashMin
            flashCol=15
          EndIf
          
        EndIf
        
      Else 
        flashDgapCount-1
      EndIf      
      
      ;       flashCol+1
      ;       If flashCol>15
      ;         flashCol=8
      ;       EndIf
    EndIf
    
    ox=x2
    oy=y2
    
  Next
  
EndProcedure

; box drawing with flashing colours - single pixel - rotate left
Procedure dBoxFlashLeft(x1,y1,x2,y2)
  If x1>x2:Swap x1,x2:EndIf
  If y1>y2:Swap y1,y2:EndIf
  
  flashDlenCount=flashDlen
  flashDgapCount=0
  
  dLineFlash(x1,y1,x1,y2)
  dLineFlash(x1+4,y2,x2,y2)
  dLineFlash(x2,y2-2,x2,y1)
  dLineFlash(x2-4,y1,x1,y1)
  
EndProcedure

; box drawing with flashing colours - single pixel - rotate right
Procedure dBoxFlashRight(x1,y1,x2,y2)
  If x1>x2:Swap x1,x2:EndIf
  If y1>y2:Swap y1,y2:EndIf
  
  flashDlenCount=flashDlen
  flashDgapCount=0
  
  dLineFlash(x1,y1,x2,y1)
  dLineFlash(x2,y1+2,x2,y2-2)
  dLineFlash(x2,y2,x1,y2)
  dLineFlash(x1,y2-2,x1,y1)
  
EndProcedure


; flood fill With current pattern
Procedure floodFill(sx,sy)
  setP(sx,sy)
  
  If (px)>-1 And (px)<dMdx And (py)>-1 And (py)<dMdy
    
    Protected uf,df,c,x,y,mc,dc,i,fp
    
    uf=0
    df=0
    dc=255
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
        
        If dl(dLay)\SCRN[y*#drwW+x]=mc 
          uf=1 : df=1
          
          ; scan left
          While x>0 And dl(dLay)\SCRN[y*#drwW+x-1]=mc
            x-1
          Wend
          
          ; scan right and plot fill colour
          While x<dMdx And dl(dLay)\SCRN[y*#drwW+x]=mc
            If i
              dc=curPat(x % 16,15-(y % 16))
            EndIf
            
            dl(dLay)\SCRN[y*#drwW+x]=dc
            
            ; detect colour changes above and add To List
            If y<(dMdy-1)
              c=dl(dLay)\SCRN[(y+1)*#drwW+x]
              If uf And c=mc
                AddElement(lFS())
                lFS()\x=x : lFS()\y=y+1
                uf=0
              EndIf
              If c<>mc : uf=1 : EndIf
            EndIf
            
            ; detect colour changes below and add To List
            If y>0
              c=dl(dLay)\SCRN[(y-1)*#drwW+x]
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
      
      mc=255
    Next
  EndIf
  
EndProcedure

; flood fill With current pattern
Procedure FillReplace(sx,sy)
  setP(sx,sy)
  
  If (px)>-1 And (px)<dMdx And (py)>-1 And (py)<dMdy
    
    Protected uf,df,c,x,y,mc,dc,i,fp
    
    uf=0
    df=0
    dc=15
    mc=dFil
    
    
    ; first iteration fills With mask colour (15) to replace fill colour
    ; second iteration replaces mask colour with current pattern
    For i=0 To #SCRNsize
      
      If dl(dLay)\SCRN[i]=mc 
        dl(dLay)\SCRN[i]=curPat(i % 16,15-((i / #drwW) % 16))
      EndIf
      
    Next
  EndIf
  
EndProcedure

; toggle button status, assumes startdrawing is already active
; button must be completely on visible window
; toolIndex:  button number
; hCol:       highlight colour
; toolSelect: Toolstrip to update
Procedure toolToggle(toolIndex,hCol,toolSelect)
  Protected x,y,lx,ly, sDrw
  
  ; default or erase button colour is 7
  If hCol=0: hCol=7: EndIf
  
  Select toolSelect
    Case 0 ; main toolbar
      selectMA(#MA_ToolBar)
      x=MA()\lx+(toolIndex % 2)*50+4
      y=MA()\ly+(toolIndex/2)*50+4
      sDrw=StartDrawing(CanvasOutput(#GA_TSButtons))
      
    Case 1 ; animate toolbar
      x=4
      y=4+toolIndex*50
      sDrw=StartDrawing(ImageOutput(imgHandle(#menuAnimButtons)))
      
    Case 2 To 8 ; submenu toolbars
      Select toolSelect
        Case 2 : ;double width draw menu
          x=4+toolIndex % 2 * 50
          y=4+toolIndex / 2 * 50
          
        Default : ; everything else
          x=4
          y=4+toolIndex*50
          
      EndSelect
      
      sDrw=StartDrawing(ImageOutput(subM(toolSelect-2)\img))
      
  EndSelect
  
  ; scan icon and update non black areas to new colour
  If sDrw 
    If x>-1 And (x+41)<#scrW And y>-1 And (y+41)<#scrH
      
      For lx=x To x+41
        For ly=y To y+41
          If Point(lx,ly)<>0: Plot(lx,ly,bp(hCol)): EndIf
        Next
      Next
    EndIf
    StopDrawing()
    
  EndIf
  
  ; drawbox(x,y,x+41,y+41,bp(2))
  
EndProcedure

; add a tool toggle item to the tool toggle draw list
; all items in list will be drawn at end of main loop
Procedure addToggle(toolIndex,hCol,canvasIndex)
  AddElement(lToggle())
  ltoggle()\b=toolIndex
  ltoggle()\c=hCol
  ltoggle()\d=canvasIndex
EndProcedure


; draw grid with grid dimensions of BX, BY, assumes startdrawing is initiated
Procedure drawGrid()
  Protected sx,sy,lx,ly,gs,dc,s,h,i1,p1
  
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
  
  ; show all sprites
  s=32/dMpx-1
  h=16/dMpy-1
  i1=512-64
  
  For p=0 To 17       ; pattern number loop left to right
    p1=12+p*34
    
    ; custom pattern loop
    For ly=0 To h
      For lx=0 To s
        Box(p1 + lx * dMpx,i1 + ly * dMpy,dMpx,dMpy,bp(cusPat(p,(lx % 8)+(ly % 8)*8)))
      Next
    Next        
  Next
  
  ;DrawingMode(#PB_2DDrawing_Outlined)
  Box(11+dSED*34,i1+18,34,2,bp(7))
EndProcedure

Procedure updateSPR()
  Protected x1,y1,gs
  
  ; determine smallest required box required to fit into drawing frame
  x1=Int(#drwW/SPR()\sw)
  y1=Int(#drwH/SPR()\sh)
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
      If StartDrawing(CanvasOutput(#GA_TSPalette))
        updatePalette()
        StopDrawing()
      EndIf
      If StartDrawing(CanvasOutput(#GA_TSButtons))
        updateBrush()
        StopDrawing()        
      EndIf                                      
      
    EndIf
  EndIf
EndProcedure

; update transparency value for entire image
Procedure updateTRN(imgTRN)
  Protected x,y,i,dc
  
  ; copy output buffer to final image
  If StartDrawing(ImageOutput(imgTRN))
    Buffer      = DrawingBuffer()             ; Get the start address of the screen buffer
    Pitch       = DrawingBufferPitch()        ; Get the length (in byte) took by one horizontal line
    PixelFormat = DrawingBufferPixelFormat()  ; Get the pixel format. 
    
    For y = 0 To #drwHloop 
      *Line.Pixel = Buffer+Pitch*y
      For x=0 To #drwWloop
        
        r=Red(*Line\Pixel)
        g=Green(*Line\Pixel)
        b=Blue(*Line\Pixel)
        
        If r+g+b>0
          If PixelFormat = #PB_PixelFormat_32Bits_RGB
            dc=RGBA(r,g,b,tGTR)
          Else ; Else it's 32bits_BGR
            dc=RGBA(b,g,r,tGTR)
          EndIf
        Else 
          dc=RGBA(0,0,0,0)
        EndIf
        
        *Line\Pixel=dc
        
        *line+4
        
      Next
    Next
    StopDrawing()
    
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
    Box(0,0,#drwW,#drwH,bp(0))          
    Buffer      = DrawingBuffer()             ; Get the start address of the screen buffer
    Pitch       = DrawingBufferPitch()        ; Get the length (in byte) took by one horizontal line
    PixelFormat = DrawingBufferPixelFormat()  ; Get the pixel format. 
    
    ; configure palette for RGB or BGR
    If animExport>-1
    Else
      
      For i=1 To 15
        If PixelFormat = #PB_PixelFormat_32Bits_RGB
          ct(i)=RGBA(rgbT(i)\r,rgbT(i)\g,rgbT(i)\b,255)
        Else
          ct(i)=RGBA(rgbT(i)\b,rgbT(i)\g,rgbT(i)\r,255)            
        EndIf
        
      Next
    EndIf
      
    If iTrans
      ct(0)=RGBA(0,0,0,255) ; ignore transparency
    Else
      ct(0)=RGBA(0,0,0,0) ; transparent black
    EndIf
    
    ct(16)=RGBA(0,0,0,255) ; true black
    
    For y = 0 To #drwHloop 
      *Line.Pixel = Buffer+Pitch*y
      yMul=(y/2)*#drwW
      
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
    
    ct(0)=bp(0)
    ct(16)=bp(16)
    
  EndIf  
EndProcedure

; load png into specific layer
Procedure loadPNG(f.s,l)
  Protected iTMP,iDepth,x,y,yMul
  
  iTMP=LoadImage(#PB_Any,f)
  If iTMP
    
    ; resize to 640x512 and get size of colour depth
    If ImageWidth(iTMP)<>640 Or ImageHeight(iTMP)<>512
      ResizeImage(iTMP,640,512,#PB_Image_Raw) 
    EndIf
    
    iDepth=ImageDepth(iTMP,#PB_Image_InternalDepth)
    
    ; convert imported image to beeb format and dump into draw buffer
    If StartDrawing(ImageOutput(iTMP))
      Buffer      = DrawingBuffer()             ; Get the start address of the screen buffer
      Pitch       = DrawingBufferPitch()        ; Get the length (in byte) took by one horizontal line
      PixelFormat = DrawingBufferPixelFormat()  ; Get the pixel format.           
      For y = 0 To #drwHloop 
        *Line.Pixel = Buffer + Pitch * y
        yMul=(y/2)*#drwW
        
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
          *line+iDepth/2  
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
      a=rawBBC(SCRNout(x+y*#drwW))<<1
      b=rawBBC(SCRNout(x+1+y*#drwW))
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
          dl(l)\SCRN[x+y*#drwW]=revBBC(a)
          dl(l)\SCRN[x+1+y*#drwW]=revBBC(b)
          
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
    ProcedureReturn 1
  EndIf
  
;   loop through layers
; dl()
; 
; SCRNout=dl()\SCRN
; 
; loop through animated colour index
; 
; update colour animated palette
; 
; save PNG
; 
; 
; -----------------------------------------
; 
;       ; colour cycle sequence table
;       ; e.g. 
;       ; [00] [01] [02] [03] [04] [05] [06] [07] [08] [09] [10] [11] [12] [13] [14] [15]
;       ;  3    1    1    1    1    1    1    1    1    1    1    1    1    1    1    1
;       ;  1    2    3    4    3    2    1    2    1    1    1    1    1    1    1    1
;       
;       
;       ; copy visible screen buffer to output buffer
;       For i=0 To ArraySize(dl())
; 
;           For x=0 To #SCRNsize
;             SCRNout(x)=0
;             If dl(i)\SCRN[x]
;               SCRNout(x)=dl(i)\SCRN[x]
;             EndIf
;           Next            
; 
;           For x=flashMin To 15
; 
; 
;           Next 
; 
;       Next
  
EndProcedure

; open save handler
Procedure openSave(mode)
  
  Protected filename.s, QSfolder.s, action.s, F.s, ff.s, N.w, iTMP, a.a,b.a
  
  ; load options:
  ; PNG to current layer
  ; RAW to current layer
  ; PNG all layers (requires 5 images named *01 to *05)
  ; RAW all layers (requires 5 images named *01 to *05)
  ; Tracing Layer (PNG, jpg Only)
  
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
      ff = "PNG file - Load Current Layer (*.PNG)|*.PNG|PNG file - Load All Layers (*.PNG)|*.PNG|BBC file - Load Current Layer (*.*)|*.*|BBC file - Load All Layers (*.*)|*.*|Tracing Layer (PNG Only) (*.PNG)|*.PNG"
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
            
          Case 4 ; load png to trace layer
                 ;f=Left(filename,Len(filename)-2)+"0"+Str(i+1)
            
            iTMP=LoadImage(#PB_Any,filename)
            If iTMP
              ; scale image and copy into trace layer
              xx.f=ImageWidth(iTMP)
              
              yy.f=ImageHeight(iTMP)
              
              If xx>#drwW
                fix.f=#drwW/xx
                xx*fix
                yy*fix
              EndIf
              If yy>#drwH
                fix.f=#drwH/yy
                xx*fix
                yy*fix
              EndIf
              
              ResizeImage(iTMP,xx,yy)
              fixx=(#drwW/2)-(ImageWidth(iTMP)/2)
              fixy=(#drwH/2)-(ImageHeight(iTMP)/2)
              
              
              If StartDrawing(ImageOutput(imgTraceLayer))
                DrawImage(ImageID(iTMP),fixx,fixy)
                StopDrawing()
              EndIf
              
              FreeImage(iTMP)            
              
              updateTRN(imgTraceLayer)
              
            Else
              MessageRequester(" File Error","ERROR: Cannot load file..." + #CRLF$ + #CRLF$ + f,#PB_MessageRequester_Error)
            EndIf            
            
        EndSelect
        
      Case 1 ; save drawing image to file
        
        If tQSA ; quick save all layers
          For i=0 To ArraySize(dl())
            ; erase output buffer
            For x=0 To #SCRNsize
              SCRNout(x)=0 ; clear output buffer
            Next
            
            LayerToOutput(i,1)
            
            f=filename+"0"+Str(i+1)
            savePNG(f+".PNG",0)
            
            ; erase output buffer
            For x=0 To #SCRNsize
              SCRNout(x)=0 ; clear output buffer
            Next
            
            LayerToOutput(i,0)
            
            saveRAW(f)
          Next
          MessageRequester(" Quick Save - All Layers","INFO: All layers saved to the following folder: " + #CRLF$ + #CRLF$ + QSfolder,#PB_MessageRequester_Info)
        ElseIf tQSC ; quick save current layer
          
          ; erase output buffer
          For x=0 To #SCRNsize
            SCRNout(x)=0 ; clear output buffer
          Next
          
          LayerToOutput(dLay,1)
          
          filename+"0"+Str(dLay+1)
          savePNG(filename+".PNG",0)
          
          ; erase output buffer
          For x=0 To #SCRNsize
            SCRNout(x)=0 ; clear output buffer
          Next
          
          LayerToOutput(dLay,0)
          
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
                
                LayerToOutput(i,1)
                
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
    addToggle(#toolUndo,7,0)
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
    addToggle(#toolRedo,7,0)
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
      addToggle(#toolUndo,8,0)
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
      addToggle(#toolRedo,8,0)
    EndIf
    
  EndIf
EndProcedure

; display debug text - start drawing before calling
Procedure DebugText(x.i,i.i,sText.s,tc.i,bc.i)
  Static yPos
  
  If i=0 : yPos=630 : EndIf
  DrawText(x,yPos,sText,tc,bc)
  yPos+20
  
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

If OpenWindow(0,0,0,#scrW, #scrH, "ART 4 EVA v0.3",#PB_Window_SystemMenu | #PB_Window_ScreenCentered) = 0 
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
  ;bp(i)=RGB(rgbT(i)\r,rgbT(i)\g,rgbT(i)\b)
  bp(i)=RGBA(rgbT(i)\r,rgbT(i)\g,rgbT(i)\b,255)
Next

; flashing palette - pointer to BP colour
For i=0 To 7
  bpFlash(i)=i  
  bpFlash(i+8)=7-i
Next

; animation sequence table
For i=0 To 14
  animTable(i)=1
Next
animTable(1)=7


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
    Read.w MA()\gad
    MA()\rx=MA()\lx+MA()\w-1
    MA()\ry=MA()\ly+MA()\h-1
  EndIf
Until tName="DATAEND" 


; mouse area / tool location list - Options page
Restore optionsData

Repeat
  Read.s tName.s
  If tName<>"DATAEND"
    AddElement(MO())
    MO()\name=tName
    Read.w MO()\lx
    Read.w MO()\ly
    Read.w MO()\img
    Select MO()\img
      Case 0 ; check box
        MO()\rx=MO()\lx+20
        MO()\ry=MO()\ly+20
      Case 1 ; slider vertical
        MO()\rx=MO()\lx+30
        MO()\ry=MO()\ly+132
      Case 2 ; +/-
        MO()\rx=MO()\lx+38
        MO()\ry=MO()\ly+18
        
    EndSelect
  EndIf
Until tName="DATAEND"

; raw data format for file save and open
Restore rawFileData
For i=0 To 15
  Read.a rawBBC(i)
  revBBC(rawBBC(i))=i
Next  

; toolstrip image
imgHandle(#menuTools)=CatchImage(#PB_Any,?ToolStripMain)
imgHandle(#menuOptions)=CatchImage(#PB_Any,?ToolStripMain2)
imgHandle(#menuAnimate)=CatchImage(#PB_Any,?ToolAnimate)
imgHandle(#menuAnimButtons)=CatchImage(#PB_Any,?ToolAnimate2)
imgHandle(#menuCloseWindow)=CatchImage(#PB_Any,?ToolCloseWindow)
imgHandle(#menuHelp)=CatchImage(#PB_Any,?HelpAbout)

; submenu image details
subM(#subDraw)\img=CatchImage(#PB_Any,?ToolStripSubDraw)
subM(#subLine)\img=CatchImage(#PB_Any,?ToolStripSubLine)
subM(#subBox)\img=CatchImage(#PB_Any,?ToolStripSubBox)
subM(#subCircle)\img=CatchImage(#PB_Any,?ToolStripSubCirc)
subM(#subGrad)\img=CatchImage(#PB_Any,?ToolStripSubGrad)
subM(#subFill)\img=CatchImage(#PB_Any,?ToolStripSubFill)
subM(#subTran)\img=CatchImage(#PB_Any,?ToolStripSubTran)

; submenu layout details - y=draw pos main canvas, b=max buttons, sel=currently selected button
subM(#subDraw)\y=70
subM(#subDraw)\b=10
subM(#subDraw)\sel=0

subM(#subLine)\y=170
subM(#subLine)\b=3
subM(#subLine)\sel=0

subM(#subBox)\y=220
subM(#subBox)\b=5
subM(#subBox)\sel=0

subM(#subCircle)\y=220
subM(#subCircle)\b=5
subM(#subCircle)\sel=0

subM(#subGrad)\y=70
subM(#subGrad)\b=7
subM(#subGrad)\sel=0

subM(#subFill)\y=320
subM(#subFill)\b=2
subM(#subFill)\sel=0

subM(#subTran)\y=110
subM(#subTran)\b=8
subM(#subTran)\sel=-1

; create drawing layers
For i=0 To ArraySize(dl())
  dl(i)\VIS=1
Next
imgFinal=CreateImage(#PB_Any,#drwW,#drwH,32)
imgGRD=CreateImage(#PB_Any,#drwW,#drwH,32)
imgTraceLayer=CreateImage(#PB_Any,#drwW,#drwH,32)

; buffered drawing area
selectMA(#MA_Drawing)
If OpenWindowedScreen(WindowID(0), MA()\lx,MA()\ly, #drwW, #drwH)=0
  Exit_ART("Cannot init main canvas windowed screen object!")
EndIf

; init screen gadgets and initial state
CreateGadget(#GA_MainCanvas,#PB_Cursor_Invisible) 
CreateGadget(#GA_TSButtons,#PB_Cursor_Cross) 
CreateGadget(#GA_TSPalette,#PB_Cursor_Cross) 

AddWindowTimer(0,0,fSpeed)

mact=-1

; load fonts
tFont1=LoadFont(#PB_Any,"Arial",8)
tFont2=LoadFont(#PB_Any,"Comic Sans MS",14,#PB_Font_Italic |#PB_Font_Bold)

; gradient indexes
For x=0 To 7
  grad(x)=-1  
Next
grad(0)=4
grad(3)=6
grad(4)=7
grad(5)=6
grad(6)=4
grad(7)=0

; create initial sprite objects for custom patterns
newSPR(8,8)


;
;-------- Draw controls --------
;
  
; create gridded transparent layer
If StartDrawing(ImageOutput(imgGRD))
  For x=0 To 79
    For y=0 To 63
      c=192-(~x!y&1)*100
      Box(x*8,y*8,8,8,RGBA(c,c,c,255))
    Next
  Next
  
  StopDrawing()
EndIf


; main drawing gadget
If StartDrawing(CanvasOutput(#GA_MainCanvas))
  selectMA(#GA_MainCanvas)
  
  ; clear canvas to black
  Box(0,0,MA()\w,MA()\h,bp(0))
  
  ; main canvas double border and grid pattern
  
  drawBox(MA()\lx,MA()\ly,MA()\rx,MA()\ry,bp(7))
  drawBox(MA()\lx+1,MA()\ly+1,MA()\rx-1,MA()\ry-1,bp(7))
  
  StopDrawing()
EndIf

; palette gadget
If StartDrawing(CanvasOutput(#GA_TSPalette))
  selectMA(#GA_TSPalette)
  Box(0,0,MA()\w,MA()\h,bp(0))
  
  ; pattern select border
  DrawImage(ImageID(imgHandle(#menuOptions)),0,2)
  
  ; draw colour select boxes and double border
  resetColSel()
  
  ; stats area
  ;   x=24
  ;   y=12
  ;   g=56
  ;   selectMA(#MA_Stats)
  ;   DrawingFont(FontID(tFont1)) 
  ;   DrawText(MA()\lx,MA()\ly,"mX:")
  ;   DrawText(MA()\lx,MA()\ly+y,"mY:")
  ;   DrawText(MA()\lx,MA()\ly+y*2,"mA:")
  ;   DrawText(MA()\lx,MA()\ly+y*3,"fS:")
  ;   DrawText(MA()\lx+g,MA()\ly,"pX:")
  ;   DrawText(MA()\lx+g,MA()\ly+y,"pY:")
  ;   DrawText(MA()\lx+g,MA()\ly+y*2,"dW:")
  ;DrawText(MA()\lx+g,MA()\ly+y*3,"TS:")
  ;DrawText(MA()\lx+g*2,MA()\ly,"gC:")
  ;DrawText(MA()\lx+g*2,MA()\ly+y,"mA:")
  
  ; flash speed
  updateFlashSpeed(fSpeed)
  selectMA(#MA_FlashSpeed)
  drawbox(MA()\lx,MA()\ly,MA()\rx,MA()\ry,bp(8))
  
  ; flash colour picker
  selectMA(#MA_FlashCycle)
  drawbox(MA()\lx,MA()\ly,MA()\rx,MA()\ry,bp(7))
  
  selectMA(#MA_FlashDraw)
  drawbox(MA()\lx,MA()\ly,MA()\rx,MA()\ry,bp(7))
  
  updateFlashColours(#MA_FlashCycle)
  updateFlashColours(#MA_FlashDraw)  
  
  ; animate tool button
  selectMA(#MA_ToolAnimate)
  DrawImage(ImageID(imgHandle(#menuAnimate)),MA()\lx,MA()\ly)
  
  selectMA(#MA_FlashLen)
  x=MA()\lx
  y=MA()\ly
  
  ; border
  ;drawbox(x-2,y-2,x+106,y+48,bp(8))
  
  ; length control
  drawbox(x+40,y,x+72,y+18,bp(8))
  DrawText(x+76,y+2,"Len",bp(3))
  DrawText(x+44,y+2,Str(flashDlen),bp(7))
  
  drawbox(x+2,y+2,x+18,y+16,bp(8))
  Line(x+4,y+9,12,1,bp(7))
  Line(x+10,y+4,1,11,bp(7))
  
  
  drawbox(x+20,y+2,x+36,y+16,bp(8))
  Line(x+22,y+9,12,1,bp(7))
  
  ; debug
  ;drawbox(x,y,MO()\rx,MO()\ry,bp(2))          
  
  ; gap control
  selectMA(#MA_FlashGap)
  x=MA()\lx
  y=MA()\ly
  drawbox(x+40,y,x+72,y+18,bp(8))
  DrawText(x+76,y+2,"Gap",bp(3))
  DrawText(x+44,y+2,Str(flashDgap),bp(7))
  
  drawbox(x+2,y+2,x+18,y+16,bp(8))
  Line(x+4,y+9,12,1,bp(7))
  Line(x+10,y+4,1,11,bp(7))
  
  
  drawbox(x+20,y+2,x+36,y+16,bp(8))
  Line(x+22,y+9,12,1,bp(7))
  
  ; debug
  ;drawbox(x,y,MO()\rx,MO()\ry,bp(3))  
  
  updatePalette()
  
  StopDrawing()
EndIf  

; tools gadget
If StartDrawing(CanvasOutput(#GA_TSButtons))
  selectMA(#GA_TSButtons)
  Box(0,0,MA()\w,MA()\h,bp(0))
  
  DrawingFont(FontID(tFont2))
  DrawText(4,0,"ART4EVA",bp(1))
  DrawingFont(#PB_Default)
  
  ; draw tools strip and toggle defaults
  DrawImage(ImageID(imgHandle(#menuTools)),0,24)
  
  ; brush size control
  selectMA(#MA_BrushSize)
  Circle(MA()\lx+33,MA()\ly+118,16,bp(6))
  Circle(MA()\lx+33,MA()\ly+84,12,bp(6))
  Circle(MA()\lx+33,MA()\ly+55,10,bp(6))
  Circle(MA()\lx+33,MA()\ly+33,7,bp(6))
  Circle(MA()\lx+33,MA()\ly+13,5,bp(6))
  Circle(MA()\lx+6,MA()\ly+dWid*4+6,4,bp(7))
  DrawingFont(FontID(tFont1)) 
  DrawText(MA()\lx-2,MA()\ly-36,"BrushSize",bp(7))
  DrawText(MA()\lx+4,MA()\ly-20,"09 x 17",bp(7))
  
  selectMA(#MA_SelectedPat)
  DrawingFont(#PB_Default)
  DrawText(MA()\lx,MA()\ly-20,"Pat",bp(7))
  drawBox(MA()\lx,MA()\ly,MA()\rx,MA()\ry,bp(7))
  
  selectMA(#MA_FillCol)
  DrawText(MA()\lx,MA()\ly-20,"Fill",bp(7))
  drawBox(MA()\lx,MA()\ly,MA()\rx,MA()\ry,bp(7))
  
  updateLayers()
  DrawText(MA()\lx+4,MA()\ly,"Layer",bp(7))
  
  updateBrush()
  
  StopDrawing()
EndIf

; update tool toggles
toolToggle(#toolUndo,8,0) ; undo
toolToggle(#toolRedo,8,0) ; redo
toolToggle(tCur,tTog,0)   ; standard
toolToggle(#toolTransparent,2,0)      ; transparency

;toolToggle(dSel,6,0)                  ; draw style  


If StartDrawing(ScreenOutput())
  ;DrawImage(ImageID(imgFinal),0,0)
  DrawImage(ImageID(imgGRD),0,0)
  StopDrawing()
EndIf


; animate menu
toolToggle(#toolAniCycle,(1-animType)*2,1)

; draw sub menus
toolToggle(subM(#subDraw)\sel,tTog,#subDraw+2) ; draw sub menu
toolToggle(subM(#subLine)\sel,tTog,#subLine+2) ; draw sub menu
toolToggle(subM(#subBox)\sel,tTog,#subBox+2) ; draw sub menu
toolToggle(subM(#subCircle)\sel,tTog,#subCircle+2) ; draw sub menu
toolToggle(subM(#subGrad)\sel,tTog,#subGrad+2)     ; draw sub menu
toolToggle(subM(#subFill)\sel,tTog,#subFill+2) ; draw sub menu

; enable For debugging mouse area squares
#MA_Debugging=0

If #MA_Debugging
  ResetList(MA())
  While NextElement(MA())
    If StartDrawing(CanvasOutput(MA()\gad))
      drawBox(MA()\lx,MA()\ly,MA()\rx,MA()\ry,bp((ListIndex(MA()) % 7)+1))
      StopDrawing()
    EndIf
  Wend
EndIf



FlipBuffers()

Delay(500)

;
;-------- MainLoop --------
;


Repeat
  
  ;Event = WaitWindowEvent()
  
  ; event loop
  someEvent=0
  Repeat
    
    Event = WindowEvent()
    
    ; if no events are detected small delay
    If Event
      someEvent=1 
      ; process all events
      
      Select event
          
        Case #PB_Event_Timer  ; handle flashing colours
          flashing=(flashing+1) & 1
          
          Select animType
            Case 1 ; ping style animate
              flashCycle+animDir
              If flashCycle=15:animDir=-1:EndIf
              If flashCycle=flashMin:animDir=1:EndIf
              
            Case 0 ; cycle style animate
              flashCycle+1
              If flashCycle=16:flashCycle=flashMin:EndIf
              
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
          mx = GetGadgetAttribute(gCur, #PB_Canvas_MouseX)
          my = GetGadgetAttribute(gCur, #PB_Canvas_MouseY)
          
          ; get relative drawing coords of mouse, check cursor for different screen actions
          If gCur=#GA_MainCanvas
            selectMA(#MA_Drawing)
            
            dx = mx - MA()\lx
            dy = my - MA()\ly
            
          EndIf
          
          ; set mouse action if none already set for left mouse click for all gadgets
          If EventType() = #PB_EventType_LeftButtonDown
            If mact=-1 And tLIF=0
              
              ;mx = GetGadgetAttribute(gCur, #PB_Canvas_MouseX)
              ;my = GetGadgetAttribute(gCur, #PB_Canvas_MouseY)
              
              ; set default action to none and check mouse area ranges to select current mouse area
              mact=99
              i=0
              ResetList(MA())
              
              ; range find MA
              If gCur=#GA_MainCanvas
                mact=#MA_Drawing
                selectMA(#MA_Drawing)
                If dx<0 : dx=0 : EndIf
                If dx>#drwWloop : dx=#drwWloop : EndIf
                If dy<0 : dy=0 : EndIf
                If dy>#drwHloop : dy=#drwWloop : EndIf
                
                
              Else
                
                While NextElement(MA())
                  If MA()\active And MA()\gad=gCur And mx>=MA()\lx And mx<=MA()\rx And my>=MA()\ly And my<=MA()\ry
                    mact=i
                    Break
                  EndIf
                  i+1
                Wend
              EndIf
              
              
              Select mact
                Case #MA_Drawing ; main drawing canvas - save undo
                                 ; clear redo when new drawing starts
                  If dDSP=0
                    If ListSize(dl(dLay)\lRedo())
                      ClearList(dl(dLay)\lRedo())
                      addToggle(#toolRedo,8,0)
                    EndIf
                    saveUndo()
                    sx=dx
                    sy=dy
                    ;sx=mx
                    ;sy=my
                    ox=sx
                    oy=sy
                    flashCol=flashMin
                    fox=px1(mx)
                    foy=py1(my)
                  EndIf
                  
              EndSelect
              
            EndIf
          EndIf
          
          
          
          ; determine which gadget has triggered an event
          Select gCur
              
              ;
              ;--- Drawing Gadget Mouse Down / Move ---
              ;              
              
            Case #GA_MainCanvas ; drawing area
              
              ; change cursor if needed
              x=GetGadgetAttribute(#GA_MainCanvas, #PB_Canvas_Cursor)
              
              If dDSP<>0
                If x<>#PB_Cursor_Cross
                  SetGadgetAttribute(#GA_MainCanvas, #PB_Canvas_Cursor , #PB_Cursor_Cross)
                EndIf
              Else
                If x<>#PB_Cursor_Invisible
                  SetGadgetAttribute(#GA_MainCanvas, #PB_Canvas_Cursor , #PB_Cursor_Invisible)
                EndIf
              EndIf        
              
              ; right mouse button event
              If EventType() = #PB_EventType_RightButtonDown Or (EventType() = #PB_EventType_MouseMove And GetGadgetAttribute(gCur, #PB_Canvas_Buttons) & #PB_Canvas_RightButton) And tZoom>1
                scroll_x=scroll_x-(px-opx)
                scroll_y=scroll_y-(py-opy)
                
                If scroll_x<0 : scroll_x=0 : EndIf
                If scroll_x>120 : scroll_x=120 : EndIf
                If scroll_y<0 : scroll_y=0 : EndIf
                If scroll_y>192 : scroll_y=192 : EndIf
              EndIf
              
              
              ; left mouse button down or mouse move with left button events
              If EventType() = #PB_EventType_LeftButtonDown Or (EventType() = #PB_EventType_MouseMove And GetGadgetAttribute(gCur, #PB_Canvas_Buttons) & #PB_Canvas_LeftButton)
                
                ; main canvas do any drawing actions For mouse move
                
                If tZoom=1
                  
                  Select mact
                    Case #MA_Drawing ; main drawing canvas
                      Select dDSP
                        Case 0 ; standard painting mode
                          
                          setP(mx,my)
                          
                          ; determine tool being used
                          Select tCur
                            Case #toolDraw ; brush tool
                              Select subM(#subDraw)\Sel
                                Case #toolBrushBox ; standard brush
                                  selectMA(#MA_Drawing)                                      
                                  dLine(mx,my,ox+MA()\lx,oy+MA()\ly,dWid)
                                  
                                Case #toolBrushRound ; circle brush
                                  dCircle(mx,my,dWid*2)
                                  
                                Case #toolBrushLeft ; left leaning brush
                                  dLine(mx-dwid,my-dwid,mx+dwid,my+dwid,2)
                                  
                                Case #toolBrushRight ; right leaning brush
                                  dLine(mx-dwid,my+dwid,mx+dwid,my-dwid,2)
                                  
                                Case #toolBrushVert ; vertical brush
                                  dLine(mx,my-dwid,mx,my+dwid,2)
                                  
                                Case #toolBrushHorz ; horizontal brush
                                  dLine(mx-dWid,my,mx+dWid,my,2)
                                  
                                Case #toolBrushFlash ; flasing pixels brush
                                  flashbrush(px1(mx),py1(my))
                                  
                                Case #toolBrushRND ; airbrush
                                                   ;dbrush(px,py,dWid,2)
                                  dCircleSpray(mx,my,dwid*2)
                                  
                                  dAir=1
                                  
                                  ;Case #toolBrush2x ; standard X2 brush
                                  ;dbrush(px,py,dWid,1)
                                  
                              EndSelect
                              
                            Case #toolErase ; erase tool
                              dbrush(px,py,dWid,3)
                            Case #toolLine,#toolCircle,#toolBox,#toolGradient
                              
                            Case #ToolCopy
                              
                              ; if new range
                              Select copyState
                                  
                                Case 0 ; new selection
                                  copysx=dx / dMpx
                                  copysy=dy / dMpy
                                  copybx=copysx * dMpx
                                  copyby=copysy * dMpy
                                  copyex=dMpx
                                  copyey=dMpy
                                  copyState=1
                                  
                                  
                                Case 1 ; drag outline for existing box
                                  x=dx / dMpx
                                  y=dy / dMpy
                                  
                                  If x<copysx
                                    copybx=copysx * dMpx + dMpx
                                    copyex=-((copysx - x) * dMpx + dMpx)
                                    If copybx+copyex<0 
                                      copyex-(copybx+copyex)
                                    EndIf
                                    
                                  Else
                                    copybx=copysx * dMpx 
                                    copyex=(x-copysx) * dMpx + dMpx
                                    
                                    If copybx+copyex>#drwW
                                      copyex=#drwW-copybx
                                    EndIf
                                    
                                  EndIf
                                  
                                  
                                  If y<copysy
                                    copyby=copysy * dMpy + dMpy
                                    copyey=-((copysy-y) * dMpy + dMpy)
                                    
                                    If copyby+copyey<0 
                                      copyey-(copyby+copyey)
                                    EndIf
                                    
                                  Else
                                    copyby=copysy * dMpy
                                    copyey=(y-copysy) * dMpy + dMpy
                                    
                                    If copyby+copyey>#drwHloop
                                      copyey=#drwhloop-copyby
                                    EndIf
                                    
                                  EndIf
                                  
                                  
                                Case 2 ; check if mouse in range and create moving selection
                                  cx1=copybx
                                  cx2=copybx+copyex
                                  cy1=copyby
                                  cy2=copyby+copyey
                                  If cx1>cx2 : Swap cx1,cx2 : EndIf
                                  If cy1>cy2 : Swap cy1,cy2 : EndIf
                                  
                                  If dx>=cx1 And dx<=cx2 And dy>=cy1 And dy<=cy2
                                    copyState=3
                                    
                                    ; omg this took ages to nut out!
                                    ; when dragging an existing box, add mouse pos offset so box and crosshair align
                                    copysx=copybx+dx % dMpx
                                    copysy=copyby+dy % dMpy
                                    
                                    
                                  Else
                                    ; finalise previous selection move
                                    x1=copybx / dMpx: x2=(copybx +copyex) / dMpx
                                    y1=(#drwH-copyby) / dMpy: y2=(#drwH-(copyby+copyey)) / dMpy
                                    
                                    If x1>x2 : Swap x1,x2 : EndIf
                                    If y1>y2 : Swap y1,y2 : EndIf
                                    
                                    dBrushPaste(x1,y1,0)
                                    
                                    ; new selection outside of previous selection
                                    copysx=dx / dMpx
                                    copysy=dy / dMpy
                                    copybx=copysx * dMpx
                                    copyby=copysy * dMpy
                                    copyex=dMpx
                                    copyey=dMpy
                                    copyState=1
                                    
                                  EndIf
                                  
                                Case 3 ; move existing box 
                                  copysx=copysx+dx-ox
                                  copysy=copysy+dy-oy
                                  
                                  copybx=(copysx / dMpx)*dMpx
                                  copyby=(copysy / dMpy)*dMpy
                                  
                              EndSelect
                              
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
                          ResetList(MO())
                          i=0
                          While NextElement(MO())
                            If rangeOpt(i,dx,dy)
                              Select i
                                Case #MO_OptThin,#MO_OptThick,#MO_OptPixel,#MO_OptOff1
                                  oCrossHair=i
                                  If i=#MO_OptOff1
                                    oMouseGuide=1
                                  EndIf
                                Case #MO_OptGuide
                                  oMouseGuide=1-oMouseGuide
                                  If oCrossHair=3
                                    oMouseGuide=1
                                  EndIf 
                                Case #MO_OptMode0 To #MO_OptMode7
                                  dMode=i-#MO_OptMode0
                                  
                                  Select dMode
                                    Case 0
                                      dMdx=640 ; current mode horizontal pixels
                                      dMdy=256 ; current mode vertical pixels
                                      dMpx=1   ; current mode horizontal pixel size
                                      dMpy=2   ; current mode vertical pixel size
                                      dBits=2
                                      
                                    Case 1
                                      dMdx=320 ; current mode horizontal pixels
                                      dMdy=256 ; current mode vertical pixels
                                      dMpx=2   ; current mode horizontal pixel size
                                      dMpy=2   ; current mode vertical pixel size
                                      dBits=4
                                      
                                    Case 2
                                      dMdx=160 ; current mode horizontal pixels
                                      dMdy=256 ; current mode vertical pixels
                                      dMpx=4   ; current mode horizontal pixel size
                                      dMpy=2   ; current mode vertical pixel size
                                      dBits=8
                                      
                                    Case 3
                                      dMdx=80 ; current mode horizontal pixels
                                      dMdy=128; current mode vertical pixels
                                      dMpx=8  ; current mode horizontal pixel size
                                      dMpy=4  ; current mode vertical pixel size
                                      dBits=8                                          
                                    Case 4
                                      
                                    Case 5
                                      
                                    Case 6
                                      
                                    Case 7
                                      dMdx=80 ; current mode horizontal pixels
                                      dMdy=64 ; current mode vertical pixels
                                      dMpx=8  ; current mode horizontal pixel size
                                      dMpy=8  ; current mode vertical pixel size
                                      dBits=8
                                      
                                  EndSelect
                                  If StartDrawing(CanvasOutput(#GA_TSPalette)) ; update brush on mode change
                                    resetColSel()
                                    updatePalette()
                                    StopDrawing()
                                  EndIf                        
                                  If StartDrawing(CanvasOutput(#GA_TSButtons))
                                    updateBrush()
                                    StopDrawing()        
                                  EndIf                                      
                                  
                                Case #MO_OptTrnVal
                                  y=(my-MO()\ly-4)*2
                                  If y>-1 And y<256
                                    tGTR=y
                                  EndIf
                                  
                              EndSelect
                              Break
                            EndIf
                            i+1
                          Wend
                          
                        Case 3 ; gradient options screen
                          
                          ; click on new colour
                          If dx>220 And dx<405 And dy>427 And dy<448 And grad_drag=-1
                            grad_drag=(dx-220) / 24
                            If grad_drag<0 Or grad_drag>7
                              grad_drag=-1  
                            EndIf
                          EndIf
                          
                          ;click on existing colour
                          If dx>100 And dx<520 And dy>370 And dy<400 And grad_drag=-1
                            x=(dx-100) / 54
                            If x<0 Or x>7
                              grad_drag=-1
                            Else
                              If grad(x)>-1
                                grad_drag=grad(x)
                                grad(x)=-1
                              Else
                                grad_drag=-1
                              EndIf
                              
                            EndIf
                            
                            ;120 drawbox(x*54+tx-8,ty-10,x*54+tx+11,ty+9,bp(7))
                          EndIf
                          
                          
                      EndSelect
                        
                    EndSelect
                  Else
                    ; if zoomed call brush zoomed proc
                    dbrushZoom(px,py)
                  EndIf
                  

                
              EndIf
              
              ;
              ;--- Drawing Gadget Mouse Up ---
              ; 
              
              If EventType()=#PB_EventType_LeftButtonUp
                ; do any mouse up actions such as tools and pattern select
                
                Select mact
                    
                  Case #MA_Drawing ; drawing area
                    
                    Select dDSP
                      Case 0 ; standard painting mode
                        If tZoom=1  
                          ; determine tool being used
                          Select tCur
                            Case #tooldraw
                              Select subM(#subDraw)\sel
                                Case #toolBrushSPR
                                  setP(mx,my)
                                  dBrushSPR(px,py,dWid,0,0)
                                  
                                Case #toolBrushRND
                                  dAir=0
                                Default
                                  
                              EndSelect
                              
                            Case #toolLine ; line tool completion
                              selectMA(#MA_Drawing)
                              Select subM(#subLine)\sel
                                  
                                Case #toolLineFlash
                                  flashDlenCount=flashDlen
                                  flashDgapCount=0
                                  If dShift ; horizontal
                                    dLineFlash(sx+MA()\lx,sy+MA()\ly,ox+MA()\lx,sy+MA()\ly)
                                  ElseIf dCtrl ; vertical
                                    dLineFlash(sx+MA()\lx,sy+MA()\ly,sx+MA()\lx,oy+MA()\ly)
                                  Else         ; any direction
                                    dLineFlash(sx+MA()\lx,sy+MA()\ly,ox+MA()\lx,oy+MA()\ly)  
                                  EndIf
                                  
                                Default
                                  If dShift ; horizontal
                                    dLine(sx+MA()\lx,sy+MA()\ly,ox+MA()\lx,sy+MA()\ly,dWid)
                                  ElseIf dCtrl ; vertical
                                    dLine(sx+MA()\lx,sy+MA()\ly,sx+MA()\lx,oy+MA()\ly,dWid)
                                  Else ; any direction
                                    dLine(sx+MA()\lx,sy+MA()\ly,ox+MA()\lx,oy+MA()\ly,dWid)
                                  EndIf
                              EndSelect                            
                              
                            Case #toolCircle ; polygon completion
                              Select subM(#subCircle)\sel
                                Case #toolCirFlashLeft
                                  flashDlenCount=flashDlen
                                  flashDgapCount=0
                                  dCircOutFlash(sx+MA()\lx,sy+MA()\ly,Abs(sx-ox),1)
                                Case #toolCirFlashRight
                                  flashDlenCount=flashDlen
                                  flashDgapCount=0
                                  dCircOutFlash(sx+MA()\lx,sy+MA()\ly,Abs(sx-ox),-1)
                                  
                                Case #toolCirFill ; polygon completion
                                  dCircle(sx+MA()\lx,sy+MA()\ly,Abs(sx-ox))
                                  
                                Default
                                  dSel=dDth
                                  dCircOut(sx+MA()\lx,sy+MA()\ly,Abs(sx-ox))
                              EndSelect                            
                              
                            Case #toolBox ; boxes out line
                              Select subM(#subBox)\sel
                                Case #toolBoxFlashLeft
                                  dBoxFlashLeft(sx+MA()\lx,sy+MA()\ly,ox+MA()\lx,oy+MA()\ly)
                                  
                                Case #toolBoxFlashRight
                                  dBoxFlashRight(sx+MA()\lx,sy+MA()\ly,ox+MA()\lx,oy+MA()\ly)
                                  
                                Case #toolBoxOut
                                  dBox(sx+MA()\lx,sy+MA()\ly,ox+MA()\lx,oy+MA()\ly,1)
                                  
                                Case #toolBoxfill 
                                  dBox(sx+MA()\lx,sy+MA()\ly,ox+MA()\lx,oy+MA()\ly,0)
                                  
                                Case #toolBoxEmpty
                                  dBox(sx+MA()\lx,sy+MA()\ly,ox+MA()\lx,oy+MA()\ly,2)
                                  
                              EndSelect                         
                              
                              
                            Case #toolGradient ; gradient
                              Select subM(#subGrad)\sel
                                Case #toolGradHor
                                  If sx<>ox And sy<>oy 
                                    ;dBoxG(sx+MA()\lx,sy+MA()\ly,ox+MA()\lx,oy+MA()\ly,0,0)
                                    dBoxG2(sx+MA()\lx,sy+MA()\ly,ox+MA()\lx,oy+MA()\ly,0,0)
                                  EndIf
                                Case #toolGradVer
                                  If sx<>ox And sy<>oy 
                                    ;dBoxG(sx+MA()\lx,sy+MA()\ly,ox+MA()\lx,oy+MA()\ly,1,0)
                                    dBoxG2(sx+MA()\lx,sy+MA()\ly,ox+MA()\lx,oy+MA()\ly,1,0)
                                  EndIf
                                  
                                Case #toolGradTL
                                  dBoxG2_diag(sx+MA()\lx,sy+MA()\ly,ox+MA()\lx,oy+MA()\ly,0,0)
                                  
                                Case #toolGradTR
                                  dBoxG2_diag(sx+MA()\lx,sy+MA()\ly,ox+MA()\lx,oy+MA()\ly,1,0)
                                  
                                Case #toolGradBR
                                  dBoxG2_diag(sx+MA()\lx,sy+MA()\ly,ox+MA()\lx,oy+MA()\ly,2,0)
                                  
                                Case #toolGradBL
                                  dBoxG2_diag(sx+MA()\lx,sy+MA()\ly,ox+MA()\lx,oy+MA()\ly,3,0)
                                  
                              EndSelect
                              
                            Case #toolFill ; fill tools
                              Select subM(#subFill)\sel  
                                Case #toolFillFill ; flood fill
                                  selectMA(#MA_Drawing)
                                  floodfill(dx,dy)
                                  
                                Case #toolFillReplace ; replace file
                                  selectMA(#MA_Drawing)
                                  FillReplace(dx,dy)
                              EndSelect
                              
                            Case #ToolCopy ; copy finalise
                              Select copystate
                                  
                                Case 1 ; get screen coords for start and end mouse pos
                                  
                                  ;x1=px1(copysx): x2=px1(copyex)
                                  ;y1=py1(copysy): y2=py1(copyey)
                                  
                                  x1=copybx / dMpx: x2=(copybx +copyex) / dMpx
                                  y1=(#drwH-copyby) / dMpy: y2=(#drwH-(copyby+copyey)) / dMpy
                                  
                                  If x1>x2 : Swap x1,x2 : EndIf
                                  If y1>y2 : Swap y1,y2 : EndIf
                                  
                                  x2-1
                                  y2-1
                                  
                                  ; check if new selection is valid
                                  If x1<>x2 Or y1<>y2
                                    If x1<0 : x1=0: EndIf
                                    If x2>159 : x2=159: EndIf
                                    If y1<0 : y1=0: EndIf
                                    If y2>255 : y2=255: EndIf
                                    x=0
                                    
                                    ;saveUndo()
                                    
                                    For ly=y1 To y2
                                      For lx=x1 To x2
                                        SCRNcopy\buf[x]=dl(dLay)\SCRN[ly*#drwW+lx]
                                        dl(dLay)\SCRN[ly*#drwW+lx]=0
                                        x+1
                                      Next
                                    Next
                                    
                                    SCRNcopy\w=x2-x1
                                    SCRNcopy\h=y2-y1
                                    
                                    copyState=2
                                  Else
                                    copyState=0
                                  EndIf
                                  
                                Case 2 ; ???
                                  
                                Case 3 ; finalise move selection
                                  
                                  ;                                     x1=copybx / dMpx: x2=(copybx +copyex) / dMpx
                                  ;                                     y1=(#drwH-copyby) / dMpy: y2=(#drwH-(copyby+copyey)) / dMpy
                                  ;                                     
                                  ;                                     If x1>x2 : Swap x1,x2 : EndIf
                                  ;                                     If y1>y2 : Swap y1,y2 : EndIf
                                  ;                                     
                                  ;                                     dBrushPaste(x1,y1,0)
                                  
                                  copyState=2
                                  
                              EndSelect
                              
                            Case #ToolPaste ; paste finalise
                              setP(mx,my)
                              px-(SCRNcopy\w / 2)
                              py-(SCRNcopy\h / 2)
                              
                              dBrushPaste(px,py,0)
                              
                          EndSelect
                          
                          ; update animate tool drawing colour on draw complete
                          If dAni
                            bAni=bAni+1
                            If bAni=16 : bAni= flashMin : EndIf
                          EndIf
                          
                        EndIf
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
                        
                        ; check for close button
                        If dx>603 And dx<637 And dy>475 And dy<509
                          dDSP=0
                        EndIf
                        
                        
                      Case 2 ; options screen
                        If rangeOpt(#MO_OptTrnVal,dx,dy)
                          updateTRN(imgTraceLayer)
                        EndIf
                        
                        If rangeOpt(#MO_OptGrid,dx,dy)
                          If tGrd=0
                            tGrd=-1
                          Else
                            tGrd=0 ; configure grid and trace layer
                          EndIf  
                        EndIf
                        
                        If rangeOpt(#MO_OptTrace,dx,dy)
                          If tGrd=1
                            tGrd=-1
                          Else
                            tGrd=1 ; configure grid and trace layer
                          EndIf  
                        EndIf
                        
                        ; check for close button
                        If dx>603 And dx<637 And dy>475 And dy<509
                          dDSP=0
                        EndIf
                        
                      Case 3 ; gradient options screen
                        
                        ; check for close button
                        If dx>603 And dx<637 And dy>475 And dy<509
                          dDSP=0
                        EndIf
                        
                        ; reverse gradient pattern
                        If dx>519 And dx<621 And dy>369 And dy<401
                          For x=0 To 3
                            Swap grad(x),grad(7-x)
                          Next
                          
                        EndIf
                        
                        If grad_drag>-1
                          If dx>100 And dx<520 And dy>370 And dy<400
                            x=(dx-100) / 54
                            If x>-1 And x<8
                              grad(x)=grad_drag
                            EndIf
                          EndIf
                          grad_drag=-1
                        EndIf
                        
                        
                    EndSelect
                    
                EndSelect
                
              EndIf
              
              ;
              ;--- Palette Gadget Mouse Down / Move ---
              ;  
              
            Case #GA_TSPalette
              
              ; left mouse button down or mouse move with left button events
              If EventType() = #PB_EventType_LeftButtonDown Or (EventType() = #PB_EventType_MouseMove And GetGadgetAttribute(gCur, #PB_Canvas_Buttons) & #PB_Canvas_LeftButton)
                
                Select mact 
                  Case #MA_ColSel ; colour select
                    selectMA(#MA_ColSel)
                    i=((mx-MA()\lx)/24)
                    j=((my-MA()\ly)/24*8)
                    If i<0 : i=0 : EndIf
                    If i>7 : i=7 : EndIf
                    If j<0 : j=0 : EndIf
                    If j>1 : j=1 : EndIf
                    i=i+j*8
                    
                    If dCol<>i And i>-1 And i<16
                      If StartDrawing(CanvasOutput(#GA_TSPalette))    
                        drawColSel(dCol,i)
                        updatePalette()
                        StopDrawing()
                      EndIf
                      If StartDrawing(CanvasOutput(#GA_TSButtons))
                        updateBrush()
                        StopDrawing()
                      EndIf
                      
                    EndIf
                    
                  Case #MA_FlashSpeed ; flash speed
                    selectMA(#MA_FlashSpeed)
                    s=(mx-MA()\lx)*10
                    If s<10: s=10: EndIf
                    If s>1000: s=1000: EndIf
                    If s<>fSpeed
                      fspeed=s
                      If StartDrawing(CanvasOutput(#GA_TSPalette))
                        updateflashspeed(fspeed)
                        StopDrawing()
                      EndIf
                    EndIf 
                    
                  Case #MA_FlashDraw ; flash draw colour (background)
                    drawFlashSel(#MA_FlashDraw)
                    
                  Case #MA_FlashCycle ; flash cycle colour (foreground)
                    drawFlashSel(#MA_FlashCycle)
                    
                  Case #MA_PatSel
                    If dOVL=0
                      If rangeApp(#MA_PatSel)
                        selectMA(#MA_PatSel)
                        i=8-((my-MA()\ly-8) / 22)
                        j=((mx-MA()\lx-8) / 32)
                        If i<0: i=0: EndIf
                        If i>8: i=8: EndIf
                        If j<0: j=0: EndIf
                        If j>17: j=17: EndIf
                        If pCol<>i Or pSel<>j
                          
                          ; highlight selected pattern and update selected brush size pattern
                          If StartDrawing(CanvasOutput(#GA_TSPalette))
                            Box(MA()\lx+pSel*32+4,MA()\ry-22-pCol*22+4,32,2,bp(0))
                            pCol=i
                            pSel=j
                            Box(MA()\lx+pSel*32+4,MA()\ry-22-pCol*22+4,32,2,bp(7))
                            StopDrawing()
                          EndIf
                          If StartDrawing(CanvasOutput(#GA_TSButtons))
                            updateBrush()
                            StopDrawing()
                          EndIf
                        EndIf
                      EndIf
                    Else
                      ; animate buttons
                      If rangeApp(#MA_AniSel)
                        selectMA(#MA_AniSel)
                        tAni=(my-MA()\ly) / 50
                      Else 
                        tAni=-1
                      EndIf
                      
                      ; click on new colour
                      If mx>127 And mx<317 And my>151 And my<173 And anim_drag=-1
                        anim_drag=(mx-128) / 24
                        If anim_drag<0 Or anim_drag>7
                          anim_drag=-1  
                        EndIf
                      EndIf
                      
                      ;click on existing colour
                      If mx>10 And mx<450 And my>105 And my<125 And anim_drag=-1
                        x=(mx-11) / 30
                        If x<0 Or x>14
                          anim_drag=-1
                        Else
                          If animTable(x)>-1
                            anim_drag=animTable(x)
                            animTable(x)=-1
                          Else
                            anim_drag=-1
                          EndIf
                          
                        EndIf
                        
                      EndIf                       
                      
                    EndIf
                    
                    
                EndSelect
                
              EndIf
              
              ;
              ;--- Palette Gadget Mouse Up ---
              ;  
              
              ; left button release events
              If EventType()=#PB_EventType_LeftButtonUp
                Select mact
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
                    
                  Case #MA_ToolAnimate ; animate menu
;                     If dDSP=0
;                       If dOVL=2
;                         dOVL=0
;                       Else
;                         dOVL=2
;                       EndIf
;                     EndIf 
                    
                  Case #MA_AnimateShow
                    If dOVL=1
                      dOVL=0
                      If StartDrawing(CanvasOutput(#GA_TSPalette))     
                        updatePalette()
                        StopDrawing()
                      EndIf
                    Else
                      dOVL=1
                    EndIf
                    
                    
                  Case #MA_FlashLen
                    selectMA(#MA_FlashLen)
                    If mx>MA()\lx+20
                      flashDlen-1
                      If flashDlen<1 : flashDlen=1 : EndIf
                      
                    Else
                      flashDlen+1
                      If flashDlen>32 : flashDlen=32 : EndIf
                      
                    EndIf
                    
                    If StartDrawing(CanvasOutput(#GA_TSPalette))
                      Box(MA()\lx+44,MA()\ly+1,28,16,bp(0))
                      DrawText(MA()\lx+44,MA()\ly+2,Str(flashDlen),bp(7))
                      StopDrawing()
                    EndIf
                    
                  Case #MA_FlashGap
                    selectMA(#MA_FlashGap)
                    
                    If mx>MA()\lx+20
                      flashDgap-1
                      If flashDgap<0 : flashDgap=0 : EndIf
                    Else
                      flashDgap+1
                      If flashDgap>32 : flashDgap=32 : EndIf
                    EndIf
                    
                    If StartDrawing(CanvasOutput(#GA_TSPalette))
                      Box(MA()\lx+44,MA()\ly+1,28,16,bp(0))
                      DrawText(MA()\lx+44,MA()\ly+2,Str(flashDgap),bp(7))
                      StopDrawing()
                    EndIf
                    
                  Case #MA_FlashSpeed ; flash speed slider
                    RemoveWindowTimer(0,0)
                    AddWindowTimer(0,0,fSpeed)                    
                    
                  Case #MA_PatSel
                    If dOVL=1
                      Select tAni
                          
                        Case #toolAnimate ; Animate on or off toggle
                          flashAnim=(flashAnim+1) % 2
                          addToggle(#toolAnimate,flashAnim*2,1)
                          
                        Case #toolAniCycle,#toolAniPing ; animate Ping = 1, animate cycle = 2
                          animType=tAni-1
                          addToggle(#toolAniCycle,(1-animType)*2,1)
                          addToggle(#toolAniPing,animType*2,1)
                          
                          If flashCycle=flashMin:animDir=1:EndIf
                          If flashCycle=15:animDir=-1:EndIf
                          
                        Case #toolAniExport ; export animation frames
                          If exportAnim()=1
                            dOVL=0
                            flashAniOld=flashAnim
                            animTypeOld=animType
                            fSpdOld=fSpeed
                            flashAnim=1
                            animExport=0
                            animType=0
                            addToggle(#toolAniCycle,(1-animType)*2,1)
                            addToggle(#toolAniPing,animType*2,1)
                            flashCycle=15
                            RemoveWindowTimer(0,0)
                            AddWindowTimer(0,0,10)
                          EndIf 
                      EndSelect
                      
                      selectMA(#MO_OptFlashMin)
;                     Case #MO_OptFlashMin
;                       x=(mx-MO()\lx-4)
;                       If x>19 And x<40 And flashMin<8
;                         flashMin=flashMin+1
;                         ;animCnt=flashMin
;                       EndIf
;                       If  x>-1 And x<20 And flashMin>1
;                         flashMin=flashMin-1
;                         ;animCnt=flashMin
;                       EndIf
                      
                      
                    EndIf
                    
                    
                EndSelect
                
                If dOVL=1
                  If anim_drag>-1
                    If mx>10 And mx<450 And my>105 And my<125
                      x=(mx-11) / 30
                      If x>-1 And x<15
                        animTable(x)=anim_drag
                      EndIf
                    EndIf
                    anim_drag=-1
                  EndIf
                EndIf
                
                  selectMA(#MA_Drawing)
                  
                  
                
              EndIf
              
              If EventType()=#PB_EventType_RightButtonUp
                If dOVL=1
                  If mx>127 And mx<317 And my>151 And my<173
                    c=(mx-128) / 24
                    If c>-1 And c<8
                      For x=0 To 14
                        animTable(x)=c
                      Next
                    EndIf
                  EndIf
                EndIf
              EndIf
              
              
              ;
              ;--- Tools Gadget Mouse Down / Move ---
              ;              
              
            Case #GA_TSButtons ; tools area
                               ; left mouse button down or mouse move with left button events
              If EventType() = #PB_EventType_LeftButtonDown Or (EventType() = #PB_EventType_MouseMove And GetGadgetAttribute(gCur, #PB_Canvas_Buttons) & #PB_Canvas_LeftButton)
                
                ; do any drawing actions for mouse move
                Select mact
                    
                  Case #MA_ToolBar ; tool area
                                   ; draw highlight box to indicate current menu option
                    selectMA(#MA_ToolBar)
                    
                    ; if mouse is in sub area check sub menu range, submenu is drawn in 'Update Screen' section
                    If mx-MA()\lx<0 And tSub>-1
                      sSel=-1
                      ;subM(tSub)\sel=-1
                      y=-1
                      If my>subM(tSub)\y+4
                        Select tSub
                          Case 0 : ; double width draw menu
                            If mx-MA()\lx>-101
                              y=((my-subM(tSub)\y-4) / 50)*2+(mx+100)/50
                            EndIf
                            
                          Default : ; single width menu
                            If mx-MA()\lx>-51
                              y=(my-subM(tSub)\y-4) / 50
                            EndIf
                        EndSelect
                        If y>-1 And y<subM(tSub)\b
                          ;subM(tSub)\sel=y
                          sSel=y
                        EndIf

                      EndIf
                      
                    Else
                      ; mouse is in toolbar area
                      x=(mx-MA()\lx) / 50
                      y=(my-MA()\ly) / 50
                      
                      If x>1: x=1: EndIf
                      If y<0: y=0: EndIf
                      If y>11: y=11: EndIf
                      
                      tHi=x+y*2
                      
                      If tOld<>tHi
                        If StartDrawing(CanvasOutput(#GA_TSButtons))
                          DrawingMode(#PB_2DDrawing_Outlined)
                          
                          x=MA()\lx+(tHi % 2)*50+2
                          y=MA()\ly+(tHi/2)*50+2
                          
                          Box(x,y,46,46,bp(2))
                          
                          If tOld>-1
                            x=MA()\lx+(tOld % 2)*50+2
                            y=MA()\ly+(tOld/2)*50+2
                            
                            Box(x,y,46,46,bp(0))
                            
                          EndIf
                          
                          tOld=tHi
                          
                          StopDrawing()
                          
                          ; update sub menu if selected
                          tSub=-1
                          
                          If tHi=#toolDraw : tSub=#subDraw : EndIf
                          If tHi=#toolLine : tSub=#subLine : EndIf
                          If tHi=#toolBox : tSub=#subBox : EndIf
                          If tHi=#toolCircle : tSub=#subCircle : EndIf
                          If tHi=#toolGradient : tSub=#subGrad : EndIf
                          If tHi=#toolFill : tSub=#subFill : EndIf
                          If tHi=#toolTransform : tSub=#subTran : EndIf
                          
                          If tSub>-1 : sSel=-1 : EndIf

                        EndIf           
                      EndIf
                      
                      
                    EndIf
                    
                    
                  Case #MA_BrushSize ; brush size
                    
                    SelectMA(#MA_BrushSize)
                    s=(my-MA()\ly-4) / 4
                    If s<>dWid And s>-1 And s<33
                      
                      ; update brush size indicator
                      If StartDrawing(CanvasOutput(#GA_TSButtons))
                        For i=0 To 1
                          Circle(MA()\lx+6,MA()\ly+dWid*4+6,4,bp(i*7))
                          dWid=s
                        Next
                        
                        DrawingFont(FontID(tFont1))
                        DrawText(MA()\lx+4,MA()\ly-20,Right("0"+Str(dWid+1),2)+" x "+Right("0"+Str(dWid*2+1),2),bp(7))
                        StopDrawing()
                      EndIf
                    EndIf 
                    
                EndSelect
              EndIf
              
              ;
              ;--- Tools Gadget Left Mouse Up
              ;              
              
              ; Tools Gadget left mouse up
              If EventType()=#PB_EventType_LeftButtonUp
                
                Select mact
                  Case #MA_ToolBar ; tool select
                    
                    selectMA(#MA_ToolBar)
                    
                    ; remove old highlight
                    If tOld<>-1
                      If StartDrawing(CanvasOutput(#GA_TSButtons))
                        DrawingMode(#PB_2DDrawing_Outlined)
                        
                        
                        x=MA()\lx+(tOld % 2)*50+2
                        y=MA()\ly+(tOld/2)*50+2
                        
                        Box(x,y,46,46,bp(0))
                        
                        tOld=-1
                        
                        StopDrawing()
                      EndIf           
                    EndIf
                    
                    ; only update tCur if tSel changes
                    tSel=tCur
                    
                    ; if mouse is in sub area check sub menu range, submenu is drawn in 'Update Screen' section
                    If mx-MA()\lx<0
                      ;subM(tSub)\sel=-1
                      sSel=-1
                      y=-1
                      If my>subM(tSub)\y+4
                        Select tSub
                          Case 0 : ; double width draw menu
                            If mx-MA()\lx>-101
                              y=((my-subM(tSub)\y-4) / 50)*2+(mx+100)/50
                            EndIf
                            
                          Default : ; single width menu
                            If mx-MA()\lx>-51
                              y=(my-subM(tSub)\y-4) / 50
                            EndIf
                        EndSelect
                        If y>-1 And y<subM(tSub)\b
                          sSel=y
                        EndIf

                      EndIf
                      
                      If sSel>-1
                        
                        Select tSub
                          Case #subDraw
                            
                            If subM(tSub)\sel<>sSel
                              toolToggle(subM(#subDraw)\sel,0,#subDraw+2) 
                              subM(tSub)\sel=sSel
                              toolToggle(subM(#subDraw)\sel,tTog,#subDraw+2)
                            EndIf
                            
                            tSel=#toolDraw
                            dSel=sSel
                            
                          Case #subLine
                            If subM(tSub)\sel<>sSel
                              toolToggle(subM(#subLine)\sel,0,#subLine+2) 
                              subM(tSub)\sel=sSel
                              toolToggle(subM(#subLine)\sel,tTog,#subLine+2)
                            EndIf
                            
                            tSel=#toolLine
                            dSel=sSel
                            
                          Case #subBox
                            If subM(tSub)\sel<>sSel
                              toolToggle(subM(#subBox)\sel,0,#subBox+2) 
                              subM(tSub)\sel=sSel
                              toolToggle(subM(#subBox)\sel,tTog,#subBox+2)
                            EndIf
                            
                            tSel=#toolBox
                            dSel=sSel
                            
                          Case #subCircle
                            If subM(tSub)\sel<>sSel
                              toolToggle(subM(#subCircle)\sel,0,#subCircle+2) 
                              subM(tSub)\sel=sSel
                              toolToggle(subM(#subCircle)\sel,tTog,#subCircle+2)
                            EndIf
                            
                            tSel=#toolCircle
                            dSel=sSel
                            
                          Case #subGrad
                            ; show gradient tool
                            If sSel=6
                              If dDSP<>3
                                dDSP=3
                              Else
                                dDSP=0
                              EndIf
                            Else
                              
                            If subM(tSub)\sel<>sSel
                              toolToggle(subM(#subGrad)\sel,0,#subGrad+2) 
                              subM(tSub)\sel=sSel
                              toolToggle(subM(#subGrad)\sel,tTog,#subGrad+2)
                            EndIf
                              
                              tSel=#toolGradient
                              dSel=subM(tSub)\sel
                              
                            EndIf
                            
                          Case #ToolPaste
                            tSel=#ToolPaste
                            
                          Case #subFill
                            If subM(tSub)\sel<>sSel
                              toolToggle(subM(#subFill)\sel,0,#subFill+2) 
                              subM(tSub)\sel=sSel
                              toolToggle(subM(#subFill)\sel,tTog,#subFill+2)
                            EndIf
                            
                            tSel=#toolFill
                            dSel=sSel
                            
                          Case #subTran
                            If copystate=2
                              x1=copybx / dMpx: x2=(copybx +copyex) / dMpx
                              y1=(#drwH-copyby) / dMpy: y2=(#drwH-(copyby+copyey)) / dMpy
                              
                              If x1>x2 : Swap x1,x2 : EndIf
                              If y1>y2 : Swap y1,y2 : EndIf
                              
                              x2-1
                              y2-1
                              
                              If x1<0 : x1=0: EndIf
                              If x2>159 : x2=159: EndIf
                              If y1<0 : y1=0: EndIf
                              If y2>255 : y2=255: EndIf
                              
                              ;saveUndo()
                              
                              
                              ;copyState=2
                              
                              Select y
                                Case 0 ; mirror
                                  
                                  dBrushPaste(x1,y1,0)
                                  saveUndo()
                                  copyState=0
                                  
                                  For ly=y1 To y2
                                    For lx=x1 To x2
                                      x=(x2+1)+(x2-x1)-(lx-x1)
                                      If x<dMdx 
                                        p=dl(dLay)\SCRN[ly*#drwW+lx]
                                        If p-dTrn>-1
                                          dl(dLay)\SCRN[ly*#drwW+x]=p
                                        EndIf
                                      EndIf
                                    Next
                                  Next
                                  
                                Case 1 ; reflect
                                  dBrushPaste(x1,y1,0)
                                  saveUndo()
                                  copyState=0
                                  
                                  For ly=y1 To y2
                                    y=(y1-1)-(ly-y1)
                                    If y>-1 
                                      For lx=x1 To x2
                                        p=dl(dLay)\SCRN[ly*#drwW+lx]
                                        If p-dTrn>-1
                                          dl(dLay)\SCRN[y*#drwW+lx]=p
                                        EndIf
                                      Next
                                    EndIf
                                  Next
                                  
                                Case 2 ; flip horizontal
                                  w=SCRNcopy\w+1
                                  h=SCRNcopy\h+1
                                  
                                  For x=0 To w*h-1
                                    SCRNtemp\buf[x]=SCRNcopy\buf[x]
                                  Next
                                  
                                  bx=0
                                  For y=0 To h-1
                                    For x=0 To w-1
                                      ;tx=(SCRNcopy\w-x)+y*SCRNcopy\w
                                      tx=(SCRNcopy\w-x)+y*w
                                      SCRNcopy\buf[bx]=SCRNtemp\buf[tx]
                                      bx+1
                                    Next
                                  Next
                                  
                                Case 3 ; flip vertical
                                  w=SCRNcopy\w+1
                                  h=SCRNcopy\h+1
                                  
                                  For x=0 To w*h-1
                                    SCRNtemp\buf[x]=SCRNcopy\buf[x]
                                  Next
                                  
                                  bx=0
                                  For y=0 To h-1
                                    For x=0 To w-1
                                      ;tx=(SCRNcopy\w-x)+y*SCRNcopy\w
                                      tx=x+(SCRNcopy\h-y)*w
                                      SCRNcopy\buf[bx]=SCRNtemp\buf[tx]
                                      bx+1
                                    Next
                                  Next
                                  
                                Case 4 ; negative
                                  w=SCRNcopy\w+1
                                  h=SCRNcopy\h+1
                                  
                                  For x=0 To w*h-1
                                    c=SCRNcopy\buf[x]
                                    If c<8 
                                      c=7-c
                                    Else
                                      c=15-(c-8)
                                    EndIf
                                    
                                    SCRNcopy\buf[x]=c
                                  Next
                                  
                                Case 5 ; glitch
                                  w=SCRNcopy\w+1
                                  h=SCRNcopy\h+1
                                  
                                  For x=0 To w*h-1
                                    SCRNtemp\buf[x]=SCRNcopy\buf[x]
                                  Next
                                  
                                  bx=0
                                  For y=0 To h-1
                                    For x=0 To w-1
                                      ;tx=(SCRNcopy\w-x)+y*SCRNcopy\w
                                      tx=x+y*w
                                      If Random(10)>8
                                        tx+(Random(11)-6)
                                      EndIf
                                      
                                      If tx>0 And tx<h*w
                                        SCRNcopy\buf[bx]=SCRNtemp\buf[tx]
                                      EndIf
                                      
                                      bx+1
                                    Next
                                  Next
                                  
                                Case 6 ; erase
                                  dBrushPaste(x1,y1,0)
                                  copyState=0
                                  
                                  For ly=y1 To y2
                                    For lx=x1 To x2
                                      dl(dLay)\SCRN[ly*#drwW+lx]=0
                                    Next
                                  Next
                                  
                                Case 7 ; options
                                  
                                  
                              EndSelect
                            EndIf
                            
                        EndSelect
                        
                        If tSel<>tCur
                          addToggle(tCur,0,0)
                          tCur=tSel
                          addToggle(tCur,tTog,0)
                        EndIf
                        
                      EndIf
                      
;                       If StartDrawing(CanvasOutput(#GA_TSButtons))
;                         DrawText(10,600,"OUT: "+Str(tSub)+"  "+Str(y)+"  "+Str(dDSP)+"   ")
;                         
;                         StopDrawing()
;                       EndIf           
                      
                      
                      
                    Else
                      
                      ; get button of tool clicked And action
                      x=(mx-MA()\lx) / 50
                      y=(my-MA()\ly) / 50
                      If x<0: x=0: EndIf
                      If x>1: x=1: EndIf
                      If y<0: y=0: EndIf
                      If y>11: y=11: EndIf
                      
                      tSel=x+y*2
                      
                      If tSel>-1 And tSel<24
                        Select tSel
                          Case #toolSave ; save
                            copyState=0
                            opensave(1)
                          Case #toolLoad ; load
                            copyState=0
                            saveUndo()                  
                            opensave(0)
                            tLIF=1
                          Case #toolQSAll ; quick save all
                            tQSA=1-tQSA
                            tQSC=0
                            addToggle(tSel,tQSA*2,0)
                            addToggle(#toolQSCur,0,0)
                            
                          Case #toolQSCur ; quick save current
                            tQSC=1-tQSC
                            tQSA=0
                            addToggle(tSel,tQSC*2,0)
                            addToggle(#toolQSAll,0,0)
                            
                          Case #toolUndo ; undo
                            copyState=0
                            undo()
                            
                          Case #toolRedo ; redo
                            copyState=0
                            redo()                  
                            
                          Case #toolCLS ; CLS #toolCLSall
                            saveundo()
                            copyState=0
                            For x=0 To #SCRNsize
                              dl(dLay)\SCRN[x]=0
                            Next                  
                            
                          Case #toolCLSall ; CLS #toolCLSall
                            dLayOld=dLay
                            copyState=0
                            For i=0 To ArraySize(dl())
                              dLay=i
                              saveundo()
                              For x=0 To #SCRNsize
                                dl(i)\SCRN[x]=0
                              Next
                            Next
                            dLay=dLayOld
                            
                          Case #toolTransparent; toggle transparency
                            dTrn=(dTrn+1) % 2
                            addToggle(tSel,dTrn*2,0)
                            
                            ;  Case #toolBrushType
                            ;  tBRT=0
                            ;  If StartDrawing(CanvasOutput(#GA_TSPalette))
                            ;    updateBrush()
                            ;    StopDrawing()
                            ;  EndIf
                            
                          Case #toolZoom
                            dZom=(dZom+1) % 2
                            tZoom=dZom*3+1
                            toolToggle(tSel,dZom*2,0)
                            
                          Case #toolDither ; toggle dither
                            dDth=(dDth+1) % 2
                            addToggle(tSel,dDth*2,0)
                            If StartDrawing(CanvasOutput(#GA_TSPalette))
                              updateBrush()
                              StopDrawing()
                            EndIf
                            
                          Case #toolBrushAnimate
                            dAni=(dAni+1) % 2
                            addToggle(tSel,dAni*2,0)
                            bAni= flashMin
                                                        
                          Case #toolFill
                            ; do nothing
;                               Case #toolFillFill, #toolFillReplace ; flood fill
;                                 If rangeApp(#MA_Drawing)
;                                   i=dl(dLay)\SCRN[px+#drwW*py];Point(mx,my) get pixel colour under cursor
;                                   If i<>dFil                  ; continue if fill colour is not the same as last fill
;                                     dFil=i
;                                     If StartDrawing(CanvasOutput(#GA_TSButtons)) ; update fill colour box in tool box
;                                       selectMA(#MA_FillCol)
;                                       Box(MA()\lx+4,MA()\ly+4,MA()\rx-MA()\lx-7,MA()\ry-MA()\ly-7,bp(dFil))
;                                       StopDrawing()
;                                     EndIf
;                                   EndIf
;                                 EndIf
                            
                            
                          Case #toolBrushReflect ; toggle reflect tool
                            dRef=(dRef+1) % 2
                            addToggle(tSel,dRef*2,0)
                            
                          Default ; all other tools - should not be needed now
                            If tSel<>tCur
                              addToggle(tCur,0,0)
                              tCur=tSel
                              addToggle(tCur,tTog,0)
                            EndIf
                            
                        EndSelect
                      EndIf
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
                    If StartDrawing(CanvasOutput(#GA_TSButtons))
                      updateLayers()
                      StopDrawing()
                    EndIf
                    ; update undo and redo
                    If ListSize(dl(dLay)\lRedo())=0
                      addToggle(#toolRedo,8,0)
                    Else
                      addToggle(#toolRedo,7,0)
                    EndIf
                    If ListSize(dl(dLay)\lUndo())=0
                      addToggle(#toolUndo,8,0)
                    Else
                      addToggle(#toolUndo,7,0)
                    EndIf
                EndSelect
                
                
              EndIf
              
          EndSelect
          
          
          
          ;
          ;-------- Mouse Up --------
          ;    
          
          ; left button release events
          If EventType()=#PB_EventType_LeftButtonUp
            
            ; reset load flag on mouse release
            If tLIF
              tLIF=0
            EndIf
            
            
            ; reset wire frame
            dWire=0           
            ; reset mouse action
            mact=-1
            
            
            tSub=-1
            
            If tCur<>#ToolCopy 
              If copystate=2
                ; finalise previous selection move
                x1=copybx / dMpx: x2=(copybx +copyex) / dMpx
                y1=(#drwH-copyby) / dMpy: y2=(#drwH-(copyby+copyey)) / dMpy
                
                If x1>x2 : Swap x1,x2 : EndIf
                If y1>y2 : Swap y1,y2 : EndIf
                
                dBrushPaste(x1,y1,0)
                
                copyState=0
              EndIf
              
            EndIf
            
            
          EndIf        
          
        Case #PB_Event_CloseWindow ; close application event  
          End        
          
          
      EndSelect
    EndIf
    
    x1=copybx / dMpx: x2=(copybx +copyex) / dMpx
    y1=(#drwH-copyby) / dMpy: y2=(#drwH-(copyby+copyey)) / dMpy
    
    If x1>x2 : Swap x1,x2 : EndIf
    If y1>y2 : Swap y1,y2 : EndIf
    
    x2-1
    y2-1
    
    If x1<0 : x1=0: EndIf
    If x2>159 : x2=159: EndIf
    If y1<0 : y1=0: EndIf
    If y2>255 : y2=255: EndIf

    If StartDrawing(CanvasOutput(#GA_TSButtons))
      DebugText(4,0," Debug Mode ",bp(7),bp(8))
      DebugText(4,1," mx,my : "+Str(mx)+","+Str(my)+"   ",bp(1),bp(3))      
      DebugText(4,1," dx,dy : "+Str(dx)+","+Str(dy)+"   ",bp(7),bp(4))
      DebugText(4,1," px,py : "+Str(px)+","+Str(py)+"   ",bp(7),bp(1))
      DebugText(4,1," mact  : "+Str(mact)+"   ",bp(7),bp(4))
      DebugText(4,1," tsel  : "+Str(tSel)+"   "+" tcur  : "+Str(tCur)+"   ",bp(7),bp(4))
      DebugText(4,1," ssel  : "+Str(sSel)+"   "+" dsel  : "+Str(dSel)+"   ",bp(7),bp(4))
      DebugText(4,1," tsub  : "+Str(tSub)+"   "+" bani  : "+Str(bAni)+"   ",bp(7),bp(4))
      
      StopDrawing()
    EndIf
    
    
;     If StartDrawing(CanvasOutput(#GA_TSButtons))
;       DrawText(10,640,Str(copyState)+"  "+Str(copysx)+","+Str(copysy)+"   ")
;       DrawText(10,660,Str(dx)+","+Str(dx)+"  "+Str(sx)+","+Str(sy)+"   ")
;       StopDrawing()
;     EndIf           
; 
    
  Until Event=0
  
  ; keep calling airbrush even if no other events trigger
  If someEvent=0
    If dAir ; airbrush
            ; dbrush(px,py,dWid,2)
        dCircleSpray(mx,my,dwid*2)
      someEvent=1
    EndIf
  EndIf
  
  ;-------- Keyboard ---------
  ExamineKeyboard() ;Keyboard
  
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
  
  ; zoom toggle
  If KeyboardReleased(#PB_Key_Z)
    dZom=(dZom+1) % 2
    tZoom=dZom*3+1
    toolToggle(#toolZoom,dZom*2,0)
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
  
  ; centre mouse
  If KeyboardPushed(#PB_Key_C)
      selectMA(#MA_Drawing)
      
      ;SetCursorPos_(WindowX(0)+MA()\lx+320,WindowY(0)+MA()\ly+278)
      SetCursorPos_(MA()\lx+318 + WindowX(0,#PB_Window_InnerCoordinate),MA()\ly+256 + WindowY(0,#PB_Window_InnerCoordinate))
      
      SetGadgetAttribute(#GA_MainCanvas, #PB_Canvas_MouseX,MA()\lx+318)
      SetGadgetAttribute(#GA_MainCanvas, #PB_Canvas_MouseY,MA()\ly+256)
      mx=MA()\lx+318
      my=MA()\ly+256
      ;ExamineMouse()
    
  EndIf
  
  ;
  ;-------- Update Screen --------
  ;         
  If someEvent ; reduce screen redraw unless event has occured
    
    ; update tool toggle highlights
    If ListSize(lToggle())
      ForEach lToggle()
        toolToggle(lToggle()\b,lToggle()\c,lToggle()\d)
      Next
      ClearList(lToggle())
    EndIf
    
    ; pre render process layers
    If dDSP=0
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

    EndIf
    
    ; build up final display image
    If StartDrawing(ImageOutput(imgFinal))
      
      Select dDSP
          ;-------- Update Painting Screen
        Case 0 ; main canvas picture drawing mode
          
          ; copy output buffer to final image
          Buffer      = DrawingBuffer()             ; Get the start address of the screen buffer
          Pitch       = DrawingBufferPitch()        ; Get the length (in byte) took by one horizontal line
          PixelFormat = DrawingBufferPixelFormat()  ; Get the pixel format. 
          
          ; configure palette for RGB or BGR
          c=flashCycle
          If PixelFormat = #PB_PixelFormat_32Bits_RGB
            For i=0 To 15
              ct(i)=bp(bpFlash(i))
              If flashAnim=1 And i>(flashMin-1)
                ct(i)=bp(animTable(c-1))
                c-1
                If c<flashMin:c=15:EndIf
              EndIf
            Next
          Else ; Else it's 32bits_BGR
            For i=0 To 15
              ct(i)=RGBA(rgbT(bpFlash(i))\b,rgbT(bpFlash(i))\g,rgbT(bpFlash(i))\r,255)
              If flashAnim=1 And i>(flashMin-1)
               ct(i)=RGBA(rgbT(animTable(c-1))\b,rgbT(animTable(c-1))\g,rgbT(animTable(c-1))\r,255)
               c-1
               If c<flashMin:c=15:EndIf
              EndIf
            Next
          EndIf              
          
          If dGRD And tGrd=0
            DrawImage(ImageID(imgGRD),0,0)
          Else
            Box(0,0,#drwW,#drwH,bp(0))
          EndIf        
                    
          ; fill screen buffer with data from output screen
          If tZoom=1
            For y = 0 To #drwHloop 
              *Line.Pixel = Buffer + Pitch * y
              yMul=(y / dMpy) * #drwW
              
              For x=0 To dMdx-1
                dc = ct(SCRNout(x+yMul))
                If dc<>bp(0)
                  For i=0 To dMpx-1
                    *Line\Pixel = dc ; Write the pixel directly to screen buffer 
                    *line+4
                  Next
                Else
                  *Line+(dMpx * 4)
                EndIf
              Next
            Next
            
          Else
            
            For y = 0 To #drwHloop 
              *Line.Pixel = Buffer + Pitch * y
              yMul=((y / (dMpy * tZoom))+ scroll_y) * #drwW
              
              For x=scroll_x To (dMdx / tZoom)+scroll_x-1
                dc = ct(SCRNout(x+yMul))
                If dc<>bp(0)
                  For i=0 To (dMpx * tZoom - 1)
                    *Line\Pixel = dc ; Write the pixel directly to screen buffer 
                    *line+4
                  Next
                Else
                  *Line+(dMpx * tZoom * 4)
                EndIf
              Next
            Next
          EndIf
        
          ; trace layer
          If dGRD And tGrd
            DrawAlphaImage(ImageID(imgTraceLayer),0,0)
          EndIf           
          
          ; set grid visible flag
          showGrid=0
          
          ; copy selection box and bitmap
          If copyState>0 And tZoom=1
            If copyState>1
              selectMA(#GA_MainCanvas)
              x1=copybx / dMpx: x2=(copybx +copyex) / dMpx
              y1=(#drwH-copyby) / dMpy: y2=(#drwH-(copyby+copyey)) / dMpy
              
              If x1>x2 : Swap x1,x2 : EndIf
              If y1>y2 : Swap y1,y2 : EndIf
              
              DrawingMode(#PB_2DDrawing_Default)
              dBrushPaste(x1,y1,1)
            EndIf
            If copyState<3
              DrawingMode(#PB_2DDrawing_Outlined|#PB_2DDrawing_XOr)
              ;Box(copybx,copyby,copyex,copyey,bp(7))
              drawBoxDash(copybx,copyby,copybx+copyex,copyby+copyey,bp(7),1,1)
            EndIf
            
          EndIf
          
          
          ; draw cross hair if mouse not in range of visible overlays
          If rangeApp(#GA_MainCanvas) Or gCur=#GA_MainCanvas
            
            If showGrid=0
              selectMA(#GA_MainCanvas)  
              
              DrawingMode(#PB_2DDrawing_Outlined|#PB_2DDrawing_XOr)          
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
                  x=dx / (dMpx * tZoom)
                  y=dy / (dMpy * tZoom)
                  
                  If x<0 : x=0 : EndIf
                  If x>dMdx-1 : x=dMdx-1 : EndIf
                  If y<0 : y=0 : EndIf
                  If y>dMdy-1 : y=dMdy-1 : EndIf
                  
                  x * (dMpx * tZoom)
                  y * (dMpy * tZoom)
                  
                  For i=0 To (dMpx * tZoom -1)
                    LineXY(x+i,MA()\ly,x+i,MA()\ry,RGB(63,63,63))
                    If i<(dMpy * tZoom)
                      LineXY(MA()\lx,y+i,MA()\rx,y+i,RGB(63,63,63))
                    EndIf
                  Next
              EndSelect
              
              If tZoom=1                             
              ; draw brush size guide or small circle
              If oMouseGuide
                Select tCur
                  Case #toolDraw,#toolLine,#toolErase ; brush and line draw
                    If dwid>3 And dSel<>#toolBrushFlash
                      ; drawsize guide
                      x=dx / dMpx-dWid / 2
                      y=dy / dMpy-dWid
                      x2=(x+dWid)*dMpx
                      y2=(y+dWid*2)*dMpy
                      x*dMpx
                      y*dMpy    
                      
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
                EndSelect
              EndIf
              
              ; draw selection area / sprite / paste if in range
              
              Select tCur
                Case #toolDraw
                  Select dSel
                    Case #toolBrushSPR
                      setP(mx,my)
                      DrawingMode(#PB_2DDrawing_Default)
                      dBrushSPR(px,py,dWid,0,1)
                  EndSelect
                Case #ToolPaste
                  setP(mx,my)
                  px-(SCRNcopy\w / 2)
                  py-(SCRNcopy\h / 2)
                  
                  DrawingMode(#PB_2DDrawing_Default)
                  dBrushPaste(px,py,1)
                  
              EndSelect                
              
            EndIf
          EndIf
          
            
          If tZoom=1            
            ; draw shape guides
            DrawingMode(#PB_2DDrawing_Outlined|#PB_2DDrawing_XOr)                
            Select dWire
              Case #toolLine   ; line tool
                If dShift      ; horizontal
                  LineXY(sx,sy,dx,sy,bp(7))                    
                ElseIf dCtrl ; vertical
                  LineXY(sx,sy,sx,dy,bp(7))
                Else ; any direction
                  LineXY(sx,sy,dx,dy,bp(7))
                EndIf
                
              Case #toolCircle ; polygon tool
                Circle(sx,sy,Abs(sx-dx),bp(7))
              Case #toolBox,#toolGradient  ; boxes, gradient
                If dShift
                  Box(sx,sy,dx-sx,dx-sx,bp(7))
                Else
                  Box(sx,sy,dx-sx,dy-sy,bp(7))
                EndIf
                
            EndSelect
          EndIf
          
            
            ; show pixel coords
            DrawingFont(FontID(tFont1))
            setP(mx,my)
            DrawingMode(#PB_2DDrawing_Default)
            Box(520,500,120,16,bp(0))
            If px<0 : px=0 : EndIf
            If px>dMdx-1 : px=dMdx-1 : EndIf
            If py<0 : py=0 : EndIf
            If py>dMdy-1 : py=dMdy-1 : EndIf
            
            DrawText(520,500,"W: " + Str(dWid),bp(8))
            DrawText(560,500,"X: " + Str(px),bp(8))
            DrawText(600,500,"Y: " + Str(py),bp(8))
            
          EndIf
          
          
          ; handle popups on main canvas
;           Select mact
;             Case #MA_FlashCycle,#MA_FlashDraw ; show animate colour select panels
;               DrawingMode(#PB_2DDrawing_Default)
;               pickFlashColour(mact)  
;             Case #MA_ToolAnimate ; show animate buttons
;               
;           EndSelect
          
          
          ;-------- Update Sprite Screen
        Case 1 ; sprite drawing mode
          Box(0,0,#drwW,#drwH,bp(0))
          ;Box(100,100,100,100,bp(1))
          
          DrawImage(ImageID(imgHandle(#menuCloseWindow)),604,476)
          
          drawGrid()
          drawbox(200,240,220,276,bp(7))
          drawbox(420,240,440,276,bp(7))
          LineXY(218,242,202,258,bp(8))
          LineXY(218,274,202,258,bp(8))
          LineXY(422,242,438,258,bp(8))
          LineXY(422,274,438,258,bp(8))
          
          DrawText(300,400,"SPR: "+Str(dSED))
          
          
          ;-------- Update Options Screen
        Case 2 ; options screen
          Box(0,0,#drwW,#drwH,bp(0))
          
          DrawImage(ImageID(imgHandle(#menuHelp)),2,2)
          
          DrawImage(ImageID(imgHandle(#menuCloseWindow)),604,476)
          
          ; help panel
          drawbox(500,128,636,304,bp(8))
          DrawText(508,120,"Hot Keys",bp(7))
          For i=0 To 7
            If i<6
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
          DrawText(532,200,"Pattern Down",bp(7))
          
          DrawText(508,220,"Z",bp(7))
          DrawText(532,220,"Toggle Zoom",bp(7))
          
          DrawText(508,240,"C",bp(7))
          DrawText(532,240,"Center Mouse",bp(7))
          
          DrawText(508,260,"Shift",bp(7))
          DrawText(552,260,"Horz Line",bp(7))
          
          DrawText(508,280,"Ctrl",bp(7))
          DrawText(552,280,"Vert Line",bp(7))
          
          ; cross hair options
          selectMO(#MO_OptThin)
          drawbox(MO()\lx-8,MO()\ly-12,MO()\lx+100,MO()\ly+124,bp(8))
          DrawText(MO()\lx,MO()\ly-20,"Cross Hair",bp(7))
          For i=#MO_OptThin To #MO_OptGuide
            selectMO(i)
            drawbox(MO()\lx,MO()\ly,MO()\rx,MO()\ry,bp(7))
            DrawText(MO()\lx+28,MO()\ly+2,MO()\name,bp(3))
            
            Select i
              Case #MO_OptThin,1,2,3
                If oCrossHair=i
                  Box(MO()\lx+2,MO()\ly+2,17,17,bp(2))
                EndIf
              Case 4
                If oMouseGuide
                  Box(MO()\lx+2,MO()\ly+2,17,17,bp(2))
                EndIf
                
            EndSelect
          Next
          
          ; mode options
          selectMO(#MO_OptMode0)
          drawbox(MO()\lx-8,MO()\ly-12,MO()\lx+100,MO()\ly+196,bp(8))
          DrawText(MO()\lx,MO()\ly-20,"Screen Mode",bp(7))              
          For i=#MO_OptMode0 To #MO_OptMode7
            selectMO(i)
            drawbox(MO()\lx,MO()\ly,MO()\rx,MO()\ry,bp(7))
            DrawText(MO()\lx+28,MO()\ly+2,MO()\name,bp(3))
            
            If dMode=(i-#MO_OptMode0)
              Box(MO()\lx+2,MO()\ly+2,17,17,bp(2))
            EndIf
          Next
          
          ; grid layer options
          selectMO(#MO_OptGrid)
          drawbox(MO()\lx-8,MO()\ly-12,MO()\lx+100,MO()\ly+196,bp(8))
          DrawText(MO()\lx,MO()\ly-20,"Grid Layer",bp(7))              
          For i=#MO_OptGrid To #MO_OptTrace
            selectMO(i)
            drawbox(MO()\lx,MO()\ly,MO()\rx,MO()\ry,bp(7))
            DrawText(MO()\lx+28,MO()\ly+2,MO()\name,bp(3))
            
            If tGrd=(i-#MO_OptGrid)
              Box(MO()\lx+2,MO()\ly+2,17,17,bp(2))
            EndIf
          Next
          y=tGTR / 2
          If y>127
            y=127
          EndIf
          
          ;selectMA(#MO_OptTrace)
          selectMO(#MO_OptTrnVal)
          Box(MO()\lx+13,MO()\ly,5,132,bp(1))
          LineXY(MO()\lx+15,MO()\ly+2,MO()\lx+15,MO()\ly+130,bp(7))
          Box(MO()\lx+1,MO()\ly+y,28,6,bp(7))
          Box(MO()\lx+3,MO()\ly+2+y,24,2,bp(8))
          DrawText(MO()\lx+40,MO()\ly,"000",bp(7))
          DrawText(MO()\lx+40,MO()\ly+116,"255",bp(7))
          
          
          ;-------- Update Gradient Screen
        Case 3 ; gradient options screen
          DrawingMode(#PB_2DDrawing_Default)
          
          Box(0,0,#drwW,#drwH,bp(0))
          
          DrawImage(ImageID(imgHandle(#menuCloseWindow)),604,476)
          
          ; title
          drawBox(20,300,620,301,bp(7))
          DrawingFont(FontID(tFont2))
          DrawText(232,284,"Gradient Editor")

          ; gradient adder line
          tx=120 : ty=386
          ;drawBox(tx120,ty386,tx+380500,ty+388,bp(7))
          drawBox(tx,ty,tx+380,ty+2,bp(7))
          
          For x=0 To 7
            ; gradient adder line
            drawBox(x*54+tx,ty-6,x*54+tx+2,ty+8,bp(7))
            
            ; gradient indexes
            If grad(x)>-1
              Box(x*54+tx-8,ty-10,20,20,bp(0))
              Box(x*54+tx-6,ty-8,16,16,bp(grad(x)))
              drawbox(x*54+tx-8,ty-10,x*54+tx+11,ty+9,bp(7))
            EndIf
            
            ; colour selection boxes
            Box(x*24+tx+99,ty+41,16,16,bp(x))
            drawbox(x*24+tx+96,ty+38,x*24+tx+117,ty+59,bp(7))
          Next
          
          ; gradient result box
          drawBox(tx,ty-46,tx+380,ty-26,bp(7))
          dboxg2(tx+392,ty-38,tx+8,ty-26,0,1)
          ;dBoxG2(4,ty-200,644,ty-120,0,1)
          
          If grad_drag>-1
            Box(dx-10,dy-10,20,20,bp(0))
            Box(dx-8,dy-8,16,16,bp(grad_drag))
            drawbox(dx-10,dy-10,dx+9,dy+9,bp(7))
          EndIf
          
          drawbox(tx+400,ty-16,tx+500,ty+14,bp(7))
          DrawText(tx+412,ty-14,"Reverse")
          
         
      EndSelect
      
      ; draw sub menu if active
      If tSub>-1
        drawSubMenu(tSub) 
      EndIf
      
      StopDrawing()
    EndIf

    ; paint main canvas
    If StartDrawing(ScreenOutput())
      DrawImage(ImageID(imgFinal),0,0)
      StopDrawing()
    EndIf
    
    ;-------- Update animation UI
            
    If dOVL=1
      If StartDrawing(CanvasOutput(#GA_TSPalette))
        ; anim start colour control
        ;selectMA(#MA_PatSel)
        ;tx=MO()\lx
        ;ty=MO()\ly
        selectMA(#MA_PatSel)
        tx=MA()\lx
        ty=MA()\ly
        
        ; erase area and redraw all controls
        Box(MA()\lx,MA()\ly,MA()\w,MA()\h,bp(0))
        drawBox(MA()\lx+1,MA()\ly+1,MA()\rx-1,MA()\ry-1,bp(7))
        
        ty+10
        tx+8
        
        ;drawbox(tx-8,ty-24,tx+448,ty+112,bp(8))
        ;DrawText(tx,ty-36,"Animation Colour Sequence",bp(7))              
        
        
        drawbox(tx+40,ty,tx+72,ty+18,bp(8))
        DrawText(tx+44,ty+2,Str(flashMin),bp(7))
        DrawText(tx+84,ty+2,"Start Colour",bp(3))
        
        drawbox(tx+2,ty+2,tx+18,ty+16,bp(8))
        Line(tx+4,ty+9,12,1,bp(7))
        
        
        drawbox(tx+20,ty+2,tx+36,ty+16,bp(8))
        Line(tx+22,ty+9,12,1,bp(7))
        Line(tx+28,ty+4,1,11,bp(7))
        
        
        ; animation sequence selector
        ty+35
        drawBox(tx,ty+9,tx+438,ty+11,bp(7))
        
        c=RGBA(63,63,63,0)
        For x=0 To 14
          If flashMin=x+1: c=bp(7): EndIf
          
          ; anim sequence adder line
          drawBox(x*30+tx+8,ty+3,x*30+tx+10,ty+17,c)
          
          ; anim sequence indexes
          If animTable(x)>-1 And flashMin<x+2
            Box(x*30+tx,ty,20,20,bp(0))
            Box(x*30+tx+2,ty+2,16,16,bp(animTable(x)))
            drawbox(x*30+tx,ty,x*30+tx+19,ty+19,bp(7))
          EndIf
          
          ; colour selection boxes
          If x<8
            Box(x*24+tx+120,ty+49,16,16,bp(x))
            drawbox(x*24+tx+117,ty+46,x*24+tx+138,ty+67,bp(7))
          EndIf  
        Next
        
        c=flashCycle
        
        For x=tx To tx+432 Step 8
          Box(x,ty+32,8,2,bp(animTable(c-1)))
          c+1
          If c=16:c=flashMin:EndIf
        Next
        
        selectMA(#MA_AniSel)
        DrawImage(ImageID(imgHandle(#menuAnimButtons)),MA()\lx,MA()\ly)
        
        If anim_drag>-1
          x=mx
          y=my
          If x<15:x=15:EndIf
          If x>575:x=575:EndIf
          If y<74:y=74:EndIf
          If y>266:y=266:EndIf
          
          Box(x-10,y-10,20,20,bp(0))
          Box(x-8,y-8,16,16,bp(anim_drag))
          drawbox(x-10,y-10,x+9,y+9,bp(7))
        EndIf
        StopDrawing()  
      EndIf
      
    EndIf
    
    FlipBuffers()
    
    ; save a frame of animation and clean up once complete
    If animSave=1
      f.s=animFile+"0"+Str(animExport)+".PNG"
      savePNG(f,1) ; discard transparency
      
      animSave=0
      animExport+1
      If animExport=flashMin
        animExport=-1
        flashAnim=flashAniOld
        animType=animTypeOld
        fSpeed=fSpdOld
        RemoveWindowTimer(0,0)
        AddWindowTimer(0,0,fspeed)
        ;toolAniToggle(#toolAnimate,0)
        If StartDrawing(CanvasOutput(#GA_TSPalette))
          updateFlashSpeed(fSpeed)
          StopDrawing()
        EndIf
        
        MessageRequester(" Export Complete","Animation export complete, check output folder for images.",#PB_MessageRequester_Info)
      EndIf
    EndIf      
    
  Else
    Delay(1) ; if no events then halt execution for minimum time to reduce cpu usage
  EndIf
  
  
Until 0

End


;-------- Data Section --------

DataSection
  
  ; mouse area Data (left x,y) (w,h) , active, parent gadget area
  ; ensure to update maCount index to match number of mouse areas
  
  mouseData:
  Data.s "GA_MainCanvas"         ; main draing canvas area
  Data.w 0,0,648,520,0,0
  Data.s "GA_TSButtons"       ; Toolstrip with button actions for save, load, drawing tools and effects etc.
  Data.w 648,0,152,800,0,1
  Data.s "GA_TSPalette"       ; Toolstrip with palette, sprites, options, colour selector panel and stats
  Data.w 0,520,648,280,0,2
  
  
  Data.s "MA_Drawing"         ; main draing canvas area
  Data.w 4,4,640,512,1,0
  Data.s "MA_Toolbar"         ; Toolstrip with button actions for save, load, drawing tools and effects etc.
  Data.w 0,24,100,600,1,1
  Data.s "MA_ColSel"          ; colour selector panel
  Data.w 446,2,200,52,1,2
  Data.s "MA_OptionShow"      ; select options screen
  Data.w 4,2,70,52,1,2
  Data.s "MA_BrushSize"       ; brush size slider
  Data.w 102,60,204,140,1,1
  Data.s "MA_Stats"           ; stats panel for showing mouse coords
  Data.w 214,0,122,54,0,2
  Data.s "MA_SelectedPat"     ; current drawing brush / pattern indicator
  Data.w 102,460,44,44,0,1
  Data.s "MA_FillCol"         ; fill colour indicator
  Data.w 102,526,44,44,0,1
  Data.s "MA_PatSel"          ; palette / pattern selector
  Data.w 2,60,586,220,1,2
  Data.s "MA_AniSel"          ; animate menu - only visible when activated via Tool Animate
  Data.w 525,70,50,200,0,2  
  Data.s "MA_Layers"          ; layers selector, needs redesign to allow other layers
  Data.w 102,200,50,238,1,1
  Data.s "MA_SpriteShow"      ; select sprite drawing mode
  Data.w 74,2,66,52,1,2
  Data.s "MA_AnimateShow"     ; spare button on palette toolbar
  Data.w 144,2,66,52,1,2
  Data.s "Flash Speed"
  Data.w 338,28,106,26,1,2
  Data.s "Flash Draw Colour"
  Data.w 338,2,32,26,1,2
  Data.s "Flash Cycle Colour"
  Data.w 374,2,32,26,1,2
  Data.s "Tool Animate"
  Data.w 410,2,32,26,1,2
  Data.s "Flash Length"
  Data.w 218,4,38,18,1,2  
  Data.s "Flash Gap"
  Data.w 218,28,38,18,1,2  
  
  ;   ; options screen
  ;   Data.s "MA_OptThin"
  ;   Data.w 20,140,20,20,0,0
  ;   Data.s "MA_OptThick"
  ;   Data.w 20,164,20,20,0,0
  ;   Data.s "MA_OptPixel"
  ;   Data.w 20,188,20,20,0,0
  ;   Data.s "MA_OptOff"
  ;   Data.w 20,212,20,20,0,0
  ;   Data.s "MA_OptGuide"
  ;   Data.w 20,236,20,20,0,0
  ;   
  ;   Data.s "MA_GridLayer"
  ;   Data.w 200,140,20,20,0,0
  ;   
  ;   Data.s "MA_OptFlash"
  ;   Data.w 400,140,20,20,0,0
  
  ; sprite screen
  ;...
  
  
  ; this entry flags the end of the mouse area data
  Data.s "DATAEND"
  
  ; options view mouse area data
  ; name, x, y, type
  ; type 0=button, 1=slider, 2=+/-
  optionsData:
  Data.s "Thin"
  Data.w 20,140,0
  Data.s "Thick"
  Data.w 20,164,0
  Data.s "Pixel"
  Data.w 20,188,0
  Data.s "Off"
  Data.w 20,212,0
  Data.s "Guide"
  Data.w 20,236,0
  Data.s "Mode 0"
  Data.w 20,290,0
  Data.s "Mode 1"
  Data.w 20,314,0
  Data.s "Mode 2"
  Data.w 20,338,0
  Data.s "Mode 3"
  Data.w 20,362,0
  Data.s "Mode 4"
  Data.w 20,386,0
  Data.s "Mode 5"
  Data.w 20,410,0
  Data.s "Mode 6"
  Data.w 20,434,0
  Data.s "Mode 7"
  Data.w 20,458,0
  Data.s "Grid"
  Data.w 150,140,0
  Data.s "Trace"
  Data.w 150,164,0
  Data.s "TRNValue"
  Data.w 150,188,1
  Data.s "FlashMin"
  Data.w 150,374,2
  
  Data.s "DATAEND"
  
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
  
; Patterns 0 - 17, format: 4x4 grid
  patternData_NEW:
  Data.a 0,0,0,0
  Data.a 0,0,0,0
  Data.a 0,0,0,0
  Data.a 0,0,0,0
  Data.a 0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0
  Data.a 0,0,0,0,0,0,1,0,0,0,0,0,0,1,0,0
  Data.a 0,0,0,0,0,1,0,0,0,0,0,0,1,0,1,0
  Data.a 0,0,0,0,1,0,1,0,0,0,0,1,0,1,0,0
  Data.a 0,0,1,1,0,0,0,1,0,1,0,0,1,0,1,0
  Data.a 0,1,0,0,1,0,1,0,1,0,0,1,0,0,0,1
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
  
  ; inline images
  ToolStripMain:       : IncludeBinary #PB_Compiler_FilePath + "Resources/ART_TOOL_Buttons_02.png"
  ToolStripMain2:      : IncludeBinary #PB_Compiler_FilePath + "Resources/ART_TOOL_Palette_02.png"
  
  ToolStripSubDraw:    : IncludeBinary #PB_Compiler_FilePath + "Resources/ART_TOOL_Sub_Draw_01.png"
  ToolStripSubLine:    : IncludeBinary #PB_Compiler_FilePath + "Resources/ART_TOOL_Sub_Line_01.png"
  ToolStripSubBox:     : IncludeBinary #PB_Compiler_FilePath + "Resources/ART_TOOL_Sub_Box_01.png"
  ToolStripSubCirc:    : IncludeBinary #PB_Compiler_FilePath + "Resources/ART_TOOL_Sub_Circ_01.png"
  ToolStripSubGrad:    : IncludeBinary #PB_Compiler_FilePath + "Resources/ART_TOOL_Sub_Grad_01.png"
  ToolStripSubFill:    : IncludeBinary #PB_Compiler_FilePath + "Resources/ART_TOOL_Sub_Fill_01.png"
  ToolStripSubTran:    : IncludeBinary #PB_Compiler_FilePath + "Resources/ART_TOOL_Sub_Tran_01.png"
  
  ToolAnimate:         : IncludeBinary #PB_Compiler_FilePath + "Resources/ART_TOOL_Animate_01.png"
  ToolAnimate2:        : IncludeBinary #PB_Compiler_FilePath + "Resources/ART_TOOL_Animate_Sub_01.png"
  ToolCloseWindow:     : IncludeBinary #PB_Compiler_FilePath + "Resources/ART_Close_01.png"
  
  HelpAbout:           : IncludeBinary #PB_Compiler_FilePath + "Resources/Art_About_01.PNG"
  
  
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

; IDE Options = PureBasic 6.00 LTS (Windows - x86)
; CursorPosition = 4344
; FirstLine = 4313
; Folding = ------------
; EnableXP
; UseIcon = Resources\Art-icon.ico
; Executable = ART4EVA_PB_009_x86.exe
; DisableDebugger
; Watchlist = pGrd;dlinepat()>x1;dBoxG2_diag()>gAdd;dlinepat()>y1;dlinepat()>y2;dBoxG2_diag()>gR
; Watchlist = dlinepat()>x2;dlinepat()>sx;dlinepat()>e;dLay;dlinepat()>dy;dlinepat()>sy
; Watchlist = dlinepat()>p;dlinepat()>dx
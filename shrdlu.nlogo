
extensions [ table sock2 ]

;------------------------------------
; globals
;------------------------------------

globals
[ 
  patch.pixels     ; effectively patch-size
  
  rail-height      ; rail ycor
  floor.height
  
  col.count        ; no. horisontal columns
  col.pixels
  col.size         ; size of a column - width & other uses

  
  cmd-stack        ; stack (kinda) of commands for execution
  cmd-rules        ; rules for cmd expansion & execution
  shapes-map       ; a map of shrdlu shapes to NL shapes
  
  arm0
  arm.pixels
  arm.size
  arm.retracted?
  arm.holds
  arm.base-height
  
  rail.color
  ratchet0
  lines0            ; this one is a set
  riggings
  
  output-indent
]


to globals.setup
  set patch.pixels  6
  set col.count     8    ; no. horisontal columns
  set col.pixels   54
  set arm.pixels   18
  set rail.color   black
  
  set col.size  (col.pixels / patch.pixels)    ; size of a column - width & other uses
  set arm.size   (arm.pixels / patch.pixels)
  set rail-height max-pycor - 2
  
  set floor.height -1 

  set cmd-stack ""
  
  ;; a bunch of singleton expansions
  let #expandR (gen "r" col.size)
  let #expandL (gen "l" col.size)
  
  set shapes-map table:from-list
  (list
    ["cube"     "blk-cube"    ]
    ["sphere"   "blk-sphere"  ]
    ["pyramid"  "blk-pyramid" ]
    )
  
  set cmd-rules table:from-list
  (list
    ;; expansions
    (list "R" (task [cmd-stack.push #expandR]) )
    (list "L" (task [cmd-stack.push #expandL]) )
    (list "D" (task [cmd-stack.push (gen "d" arm.dist-to-top-of-col)])  )
    (list "U" (task [arm.retract])    )
    
    ;; bbot controls
    (list "r" (task [arm.cmd-right])  )
    (list "l" (task [arm.cmd-left ])  )
    (list "d" (task [arm.cmd-down ])  )
    (list "u" (task [arm.cmd-up   ])  )
    
    ;; system controls
    
  )
end




;------------------------------------
; startup & setup
;------------------------------------


to startup
  globals.setup
  world.set-size
  setup
end


to setup
  clear-all
  globals.setup
  patches.setup
  rail.setup
  reset-ticks
end



;------------------------------------
; patches
;------------------------------------

to patches.setup
  ask patches [ set pcolor grey]
  
  ; table patches
  ask patches with [pycor = floor.height]
  [ set pcolor 2 ]
  
  ask patches with [(pycor = floor.height - 2) and (pxcor mod col.size) = 4]
  [ set plabel (floor (pxcor / col.size))
    set plabel-color 1
  ]
  
;  let #i 0
;  repeat col.count
;  [ crt 1
;    [ setxy (#i * col.size + 4) (floor.height - 2)
;      set size 0
;      set label floor (pxcor / col.size)
;      set #i (#i + 1)
;    ]
;  ]
end




;------------------------------------
; rail
;------------------------------------

breed [rail-bars rail-bar]
breed [rail-lines rail-line]
breed [arms arm]


to rail.setup
  ;; draw horizontal rail
  ask patches with [pycor = rail-height ]  ;; and pxcor mod 2 = 0
  [ ;set pcolor 41
    sprout-rail-bars 1
    [ set shape "rail3"
      set color black
      set size 1
    ]
  ]
  ;; draw the ratchet
  let #half-col int (col.size / 2) + 1
  crt 1 ;; the ratchet
  [ setxy #half-col (rail-height)
    set color rail.color
    set size 3
    set shape "rail-star"
    set heading 0
    set ratchet0 self
  ]
  ;; draw 2 elements between ratchet & arm
  set lines0 (turtle-set nobody)
  foreach [1 2]
  [ create-rail-lines 1
    [ setxy #half-col (rail-height - ?)
      init-rail-line-agent
      set lines0 (turtle-set lines0 self)
    ]
  ]
  ;; draw the arm
  set arm.base-height (rail-height - 3)
  create-arms 1
  [ setxy #half-col arm.base-height
    set color black
    set size  3
    set heading 0
    
    set arm0 self
    set arm.retracted? true
    set arm.holds nobody
  ]
  ;; arm + ratchet, etc are grouped as riggings
  ;; for coordinated horizontal movements
  set riggings (turtle-set arm0 lines0 ratchet0)
end


to init-rail-line-agent
  set color rail.color
  set size 1
  set shape "rail-line"
end



;------------------------------------
; arm procedures
;------------------------------------


to arm.cmd-right
  arm.move-horiz 1
end


to arm.cmd-left
  arm.move-horiz -1
end


to arm.cmd-down
  ask ratchet0 [ set color one-of [red black blue yellow]]
  ask arm0
  [ hatch-rail-lines 1 [init-rail-line-agent]
    setxy xcor (ycor - 1)
  ]
  ask (turtle-set arm.holds)
  [ setxy xcor (ycor - 1) ]
end


to arm.cmd-up
  ask ratchet0 [ set color one-of [red black blue yellow]]
  ask arm0
  [ setxy xcor (ycor + 1)
    ask rail-lines-here [die]
  ]
  ask (turtle-set arm.holds)
  [ setxy xcor (ycor + 1) ]
end


to arm.move-horiz [#dx]
  ask (turtle-set riggings arm.holds)
  [ setxy (xcor + #dx) ycor ]
  ask ratchet0 [ rt 30 ]
end


to-report arm.grab-ycor [#block]
  let #size ([size] of #block)
  report ([ycor] of arm0) - ((arm.size + #size) / 2) + 1
end


to arm.hold [#block]
  set arm.holds #block
end


to arm.unhold
  set arm.holds nobody
end


to-report arm.col
  report int (([xcor] of arm0) / col.size)
end


to-report arm.dist-to-top-of-col
  let #top block.at-top-of arm.col
  let #dist [ycor] of arm0
  ifelse (#top = nobody)  ; column is empty
  [ set #dist (#dist - floor.height) ]
  [ set #dist (#dist - ([ycor] of #top) - ([size] of #top) / 2) ]
  
  if (arm.holds != nobody)
  [ set #dist (#dist - ([size] of arm.holds)) ]
  
  set #dist (#dist - (arm.size / 2) + 1)
  report #dist
end




;======================================================
; CMD-STACK manipulation
;======================================================


to-report cmd-stack.pop
  let #dat (first cmd-stack)
  set cmd-stack (but-first cmd-stack)
  report #dat
end


to cmd-stack.push [#dat]
  set cmd-stack (word #dat cmd-stack)
end


to cmd-stack.queue [#dat]
  set cmd-stack (word cmd-stack #dat)
end


to cmd-stack.run
  while [not empty? cmd-stack]
  [ cmd-stack.run1 ]
end


to cmd-stack.run1
  let #m cmd-stack.pop
  ; print (word #m " => " cmd-stack)
  ifelse (table:has-key? cmd-rules #m)
  [ run (table:get cmd-rules #m)  ]
  [ ;; unknown command in use
    print (word "unknown command: " #m)
  ]
  tick
end


to-report gen [#str #n]
  ; print (list "gen:" #str #n)
  set #n (int #n)
  ifelse (#n = 0)
  [ report "" ]
  [ report reduce word n-values #n [#str] ]
end


;------------------------------------
; world control
;------------------------------------


to world.set-size
  let #min-px   0
  let #max-px   col.count * col.size     ;; +2 for block factory
  let #min-py   -1 * col.size
  let #max-py   (col.count - 2) * col.size
  
  set-patch-size 6
  resize-world  #min-px #max-px #min-py #max-py
end



;------------------------------------
; top level commands & execution
;------------------------------------

breed [blocks block]
breed [banners banner]

blocks-own
[block-name shape-name color-name]


;__ the repl _______________

to exec.repl
  let cmd-str sock2:read
  output-print (word "received: " cmd-str)
  run cmd-str
  tick
end

to flush-io
  let cmd-str sock2:read
  output-print (word "received: " cmd-str)
  if (cmd-str = "stop") [stop]
  tick
end


;__ block factories ________

to exec.make [#name #shape #size #color]
  ;; it will appear in the hand
  ;; assumptions
  ;     (i)  hand is empty
  ;     (ii) hand is retracted
  inform 1 (list "making" #name #color #shape "- size" #size)
  
  assert (arm.retracted? = true) "trying to make a block but arm is not retracted"
  assert (arm.holds = nobody) "trying to make a block but arm is holding a block"
  assert (#size <= col.size)  "trying to make a block but size is too big"
  
  let #b 0
  create-blocks 1
  [ set block-name #name
        
    set size  #size
    set shape-name #shape
    set color-name #color
    
    set shape (table:get shapes-map #shape)
    set color (runresult #color)

    setxy ([xcor] of arm0) (arm.grab-ycor self)
    arm.hold self
    set #b self  ;; for animation below
    
    hatch-banners 1
    [ set size  0
      set shape "circle"
      set color (runresult #color)
      set label #name
      set label-color black
      ; shift a little
      set heading 90 + 45
      fd 1.5
      create-link-from myself
      [ tie
        hide-link
      ]
    ]
  ]
  inform 0 (list "new block is" #b)
  block-flash #b
  inform -1 ["-making complete"]
  ask #b [st]
end


to exec.move-to [#col]
  assert (#col >= 0 and #col < col.count) "trying to move to a column that dosn't exist"
  inform 1 (list "moving to" #col)
  let #c (#col - arm.col)
  ifelse (#c >= 0)
  [ cmd-stack.queue (gen "R" #c) ]
  [ cmd-stack.queue (gen "L" (abs #c)) ]
  cmd-stack.run
  inform -1 ["-moving complete"]
end


to exec.pick-from [#col]
  assert (arm.holds = nobody) "trying to pick-up but already holding"
  let #b (block.at-top-of #col)
  assert (#b != nobody) "trying to pick-up but there is nothing there"
  
  inform 1 (list "picking" ([color-name] of #b) ([shape-name] of #b) ([block-name] of #b) "from stack" #col)
  
  exec.move-to #col
  let #dist arm.dist-to-top-of-col
  
  cmd-stack.queue (gen "d" #dist)
  cmd-stack.run
  
  inform 0 (list "grabbing" ([shape-name] of #b) ([block-name] of #b) )
  arm.hold #b
  arm.retract
  inform -1 ["-picking up complete"]
end


to exec.drop-at [#col]
  assert (arm.holds != nobody) "trying to drop an object but not holding anything"
  
  inform 1 (list "dropping"
                 ([color-name] of arm.holds)
                 ([shape-name] of arm.holds)
                 ([block-name] of arm.holds)
                 "at stack" #col)
  
  exec.move-to #col
  
  let #dist arm.dist-to-top-of-col
  
  cmd-stack.queue (gen "d" #dist)
  cmd-stack.run
  
  inform 0 (list "releasing" ([shape-name] of arm.holds) ([block-name] of arm.holds))
  
  arm.unhold
  arm.retract
  inform -1 ["-dropping complete"]
end


to exec.dispose
  inform 1 (list "disposing of" ([block-name] of arm.holds))
  block-flash arm.holds
  assert (arm.holds != nobody) "trying to dispose of an object but not holding anything"
  clear-links
  ask arm.holds [die]
  set arm.holds nobody
  inform -1 ["-dispose complete"]
end

to exec.move-from-to [#src #dst]
  inform 1 (list "moving from" #src "to" #dst)
  exec.pick-from #src
  exec.drop-at #dst
  inform -1 ["-move complete"]
end


to arm.retract
  inform 1 ["retracting arm to rail"]
  let #dist arm.base-height - ([ycor] of arm0)
  cmd-stack.queue (gen "u" #dist)
  cmd-stack.run
  ask ratchet0 [ set color black ]
  inform -1 ["-arm retract complete"]
end




;------------------------------------
; utils
;------------------------------------


to assert [#x #str]
  if not #x
  [ error (word "assert fails: " #str) ]
end


to inform [#i #m]
  let #ind-inc 2
  ifelse (#i = 0)   [inform- #m]
  [ ifelse (#i > 0)
    [ if (output-indent = 0) [output-print ""]
      inform- #m
      set output-indent (output-indent + #ind-inc)
    ]
    [ ; (#i < 0)
      set output-indent (output-indent - #ind-inc)
      inform- #m
    ]
  ]
end


to inform- [#m]
  repeat output-indent [output-type " "]
  foreach #m
  [ output-type ?
    output-type " "
  ]
  output-print ""
end
      

to-report block.at-top-of [#col]
  report max-one-of (blocks with [self != arm.holds and xcor = (col.xcor #col)])
             [ycor]
end


to-report col.xcor [#col]
  report (#col * col.size) + (int (col.size / 2)) + 1
end


to block-flash [#b]
  repeat 5
  [ tick
    ask #b [ ht ]
    tick
    ask #b [ st ]
  ]
end
@#$#@#$#@
GRAPHICS-WINDOW
7
10
455
425
-1
-1
6.0
1
11
1
1
1
0
0
0
1
0
72
-9
54
1
1
1
ticks
30.0

BUTTON
85
474
148
507
NIL
setup
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

BUTTON
8
474
80
507
NIL
startup
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

BUTTON
917
335
1003
368
quick-test
set cmd-stack \"RR\"\ncmd-stack.run\nexec.make \"B1\" \"cube\" 5 \"red\"\n
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

BUTTON
918
370
1008
403
up & down
set cmd-stack \"DURRLLddddLRuuuu\"\ncmd-stack.run
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

BUTTON
918
405
1011
438
drop & pick
exec.drop-at 5\nexec.move-to 3\nexec.pick-from 5
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

BUTTON
922
456
996
489
full-test
setup\nexec.make \"B1\" \"cube\" 9 \"green\"\nexec.drop-at 2\nexec.make \"B2\" \"cube\" 9 \"green\"\nexec.drop-at 3\nexec.make \"B3\" \"pyramid\" 6 \"green\"\nexec.drop-at 3\nexec.pick-from 3\nexec.drop-at 3\nexec.make \"B4\" \"sphere\" 2 \"red\"\nexec.drop-at 3\nexec.make \"B5\" \"cube\" 2 \"blue\"\nexec.drop-at 2\nexec.pick-from 2\nexec.drop-at 3\nexec.make \"B6\" \"cube\" 8 \"red\"\nexec.drop-at 3\nexec.make \"B7\" \"cube\" 8 \"yellow\"\nexec.drop-at 3
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

OUTPUT
454
10
835
425
12

MONITOR
7
425
455
470
command stack
cmd-stack
17
1
11

BUTTON
758
15
813
48
clear
clear-output
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

INPUTBOX
841
10
912
70
port-num
2222
1
0
Number

BUTTON
842
72
912
105
connect
print (word \"connecting on \" port-num)\nsock2:connect-local port-num\nprint \"socket connected\"
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

BUTTON
153
475
236
508
start-repl
exec.repl
T
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

BUTTON
845
117
917
150
NIL
flush-io
T
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

@#$#@#$#@
## WHAT IS IT?

(a general understanding of what the model is trying to show or explain)

## HOW IT WORKS

(what rules the agents use to create the overall behavior of the model)

## HOW TO USE IT

(how to use the model, including a description of each of the items in the Interface tab)

## THINGS TO NOTICE

(suggested things for the user to notice while running the model)

## THINGS TO TRY

(suggested things for the user to try to do (move sliders, switches, etc.) with the model)

## EXTENDING THE MODEL

(suggested things to add or change in the Code tab to make the model more complicated, detailed, accurate, etc.)

## NETLOGO FEATURES

(interesting or unusual features of NetLogo that the model uses, particularly in the Code tab; or where workarounds were needed for missing features)

## RELATED MODELS

(models in the NetLogo Models Library and elsewhere which are of related interest)

## CREDITS AND REFERENCES

(a reference to the model's URL on the web if it has one, as well as any other necessary credits, citations, and links)
@#$#@#$#@
default
true
0
Polygon -7500403 true true 150 5 40 250 150 205 260 250

airplane
true
0
Polygon -7500403 true true 150 0 135 15 120 60 120 105 15 165 15 195 120 180 135 240 105 270 120 285 150 270 180 285 210 270 165 240 180 180 285 195 285 165 180 105 180 60 165 15

arrow
true
0
Polygon -7500403 true true 150 0 0 150 105 150 105 293 195 293 195 150 300 150

blk-cube
false
0
Rectangle -16777216 true false 0 0 300 300
Rectangle -7500403 true true 15 15 285 285

blk-pyramid
false
0
Polygon -16777216 true false 150 0 300 300 0 300
Polygon -7500403 true true 150 30 30 285 270 285

blk-sphere
false
0
Circle -16777216 true false 0 0 300
Circle -7500403 true true 15 15 270

box
false
0
Polygon -7500403 true true 150 285 285 225 285 75 150 135
Polygon -7500403 true true 150 135 15 75 150 15 285 75
Polygon -7500403 true true 15 75 15 225 150 285 150 135
Line -16777216 false 150 285 150 135
Line -16777216 false 150 135 15 75
Line -16777216 false 150 135 285 75

bug
true
0
Circle -7500403 true true 96 182 108
Circle -7500403 true true 110 127 80
Circle -7500403 true true 110 75 80
Line -7500403 true 150 100 80 30
Line -7500403 true 150 100 220 30

butterfly
true
0
Polygon -7500403 true true 150 165 209 199 225 225 225 255 195 270 165 255 150 240
Polygon -7500403 true true 150 165 89 198 75 225 75 255 105 270 135 255 150 240
Polygon -7500403 true true 139 148 100 105 55 90 25 90 10 105 10 135 25 180 40 195 85 194 139 163
Polygon -7500403 true true 162 150 200 105 245 90 275 90 290 105 290 135 275 180 260 195 215 195 162 165
Polygon -16777216 true false 150 255 135 225 120 150 135 120 150 105 165 120 180 150 165 225
Circle -16777216 true false 135 90 30
Line -16777216 false 150 105 195 60
Line -16777216 false 150 105 105 60

car
false
0
Polygon -7500403 true true 300 180 279 164 261 144 240 135 226 132 213 106 203 84 185 63 159 50 135 50 75 60 0 150 0 165 0 225 300 225 300 180
Circle -16777216 true false 180 180 90
Circle -16777216 true false 30 180 90
Polygon -16777216 true false 162 80 132 78 134 135 209 135 194 105 189 96 180 89
Circle -7500403 true true 47 195 58
Circle -7500403 true true 195 195 58

circle
false
0
Circle -7500403 true true 0 0 300

circle 2
false
0
Circle -7500403 true true 0 0 300
Circle -16777216 true false 30 30 240

container
false
0
Rectangle -7500403 false true 0 75 300 225
Rectangle -7500403 true true 0 75 300 225
Line -16777216 false 0 210 300 210
Line -16777216 false 0 90 300 90
Line -16777216 false 150 90 150 210
Line -16777216 false 120 90 120 210
Line -16777216 false 90 90 90 210
Line -16777216 false 240 90 240 210
Line -16777216 false 270 90 270 210
Line -16777216 false 30 90 30 210
Line -16777216 false 60 90 60 210
Line -16777216 false 210 90 210 210
Line -16777216 false 180 90 180 210

cow
false
0
Polygon -7500403 true true 200 193 197 249 179 249 177 196 166 187 140 189 93 191 78 179 72 211 49 209 48 181 37 149 25 120 25 89 45 72 103 84 179 75 198 76 252 64 272 81 293 103 285 121 255 121 242 118 224 167
Polygon -7500403 true true 73 210 86 251 62 249 48 208
Polygon -7500403 true true 25 114 16 195 9 204 23 213 25 200 39 123

cylinder
false
0
Circle -7500403 true true 0 0 300

die 1
false
0
Rectangle -7500403 true true 45 45 255 255
Circle -16777216 true false 129 129 42

dot
false
0
Circle -7500403 true true 90 90 120

face happy
false
0
Circle -7500403 true true 8 8 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Polygon -16777216 true false 150 255 90 239 62 213 47 191 67 179 90 203 109 218 150 225 192 218 210 203 227 181 251 194 236 217 212 240

face neutral
false
0
Circle -7500403 true true 8 7 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Rectangle -16777216 true false 60 195 240 225

face sad
false
0
Circle -7500403 true true 8 8 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Polygon -16777216 true false 150 168 90 184 62 210 47 232 67 244 90 220 109 205 150 198 192 205 210 220 227 242 251 229 236 206 212 183

fish
false
0
Polygon -1 true false 44 131 21 87 15 86 0 120 15 150 0 180 13 214 20 212 45 166
Polygon -1 true false 135 195 119 235 95 218 76 210 46 204 60 165
Polygon -1 true false 75 45 83 77 71 103 86 114 166 78 135 60
Polygon -7500403 true true 30 136 151 77 226 81 280 119 292 146 292 160 287 170 270 195 195 210 151 212 30 166
Circle -16777216 true false 215 106 30

flag
false
0
Rectangle -7500403 true true 60 15 75 300
Polygon -7500403 true true 90 150 270 90 90 30
Line -7500403 true 75 135 90 135
Line -7500403 true 75 45 90 45

flower
false
0
Polygon -10899396 true false 135 120 165 165 180 210 180 240 150 300 165 300 195 240 195 195 165 135
Circle -7500403 true true 85 132 38
Circle -7500403 true true 130 147 38
Circle -7500403 true true 192 85 38
Circle -7500403 true true 85 40 38
Circle -7500403 true true 177 40 38
Circle -7500403 true true 177 132 38
Circle -7500403 true true 70 85 38
Circle -7500403 true true 130 25 38
Circle -7500403 true true 96 51 108
Circle -16777216 true false 113 68 74
Polygon -10899396 true false 189 233 219 188 249 173 279 188 234 218
Polygon -10899396 true false 180 255 150 210 105 210 75 240 135 240

house
false
0
Rectangle -7500403 true true 45 120 255 285
Rectangle -16777216 true false 120 210 180 285
Polygon -7500403 true true 15 120 150 15 285 120
Line -16777216 false 30 120 270 120

leaf
false
0
Polygon -7500403 true true 150 210 135 195 120 210 60 210 30 195 60 180 60 165 15 135 30 120 15 105 40 104 45 90 60 90 90 105 105 120 120 120 105 60 120 60 135 30 150 15 165 30 180 60 195 60 180 120 195 120 210 105 240 90 255 90 263 104 285 105 270 120 285 135 240 165 240 180 270 195 240 210 180 210 165 195
Polygon -7500403 true true 135 195 135 240 120 255 105 255 105 285 135 285 165 240 165 195

line
true
0
Line -7500403 true 150 0 150 300

line half
true
0
Line -7500403 true 150 0 150 150

pentagon
false
0
Polygon -7500403 true true 150 15 15 120 60 285 240 285 285 120

person
false
0
Circle -7500403 true true 110 5 80
Polygon -7500403 true true 105 90 120 195 90 285 105 300 135 300 150 225 165 300 195 300 210 285 180 195 195 90
Rectangle -7500403 true true 127 79 172 94
Polygon -7500403 true true 195 90 240 150 225 180 165 105
Polygon -7500403 true true 105 90 60 150 75 180 135 105

plant
false
0
Rectangle -7500403 true true 135 90 165 300
Polygon -7500403 true true 135 255 90 210 45 195 75 255 135 285
Polygon -7500403 true true 165 255 210 210 255 195 225 255 165 285
Polygon -7500403 true true 135 180 90 135 45 120 75 180 135 210
Polygon -7500403 true true 165 180 165 210 225 180 255 120 210 135
Polygon -7500403 true true 135 105 90 60 45 45 75 105 135 135
Polygon -7500403 true true 165 105 165 135 225 105 255 45 210 60
Polygon -7500403 true true 135 90 120 45 150 15 180 45 165 90

rail
false
0
Rectangle -7500403 false true 0 75 300 225
Rectangle -7500403 true true 0 60 300 240
Line -16777216 false 0 210 300 210
Line -16777216 false 0 90 300 90
Line -16777216 false 150 90 150 210
Line -16777216 false 120 90 120 210
Line -16777216 false 90 90 90 210
Line -16777216 false 240 90 240 210
Line -16777216 false 270 90 270 210
Line -16777216 false 30 90 30 210
Line -16777216 false 60 90 60 210
Line -16777216 false 210 90 210 210
Line -16777216 false 180 90 180 210
Rectangle -16777216 true false 60 210 30 90
Rectangle -16777216 true false 30 90 60 210
Rectangle -16777216 true false 90 90 120 210
Rectangle -16777216 true false 150 90 180 210
Rectangle -16777216 true false 210 90 240 210
Rectangle -16777216 true false 270 90 300 210

rail-line
false
1
Rectangle -7500403 false false 60 0 210 300
Rectangle -2674135 true true 210 0 240 300
Rectangle -2674135 true true 45 0 75 300
Rectangle -2674135 true true 75 0 210 75
Rectangle -2674135 true true 75 150 210 225

rail-star
true
0
Polygon -16777216 true false 151 1 185 108 298 108 207 175 242 282 151 216 59 282 94 175 3 108 116 108
Circle -7500403 true true 118 118 62

rail3
false
1
Rectangle -7500403 false false 0 90 300 240
Rectangle -2674135 true true 0 60 300 90
Rectangle -2674135 true true 0 225 300 255
Rectangle -2674135 true true 0 90 75 225
Rectangle -2674135 true true 150 90 225 225

sheep
false
15
Circle -1 true true 203 65 88
Circle -1 true true 70 65 162
Circle -1 true true 150 105 120
Polygon -7500403 true false 218 120 240 165 255 165 278 120
Circle -7500403 true false 214 72 67
Rectangle -1 true true 164 223 179 298
Polygon -1 true true 45 285 30 285 30 240 15 195 45 210
Circle -1 true true 3 83 150
Rectangle -1 true true 65 221 80 296
Polygon -1 true true 195 285 210 285 210 240 240 210 195 210
Polygon -7500403 true false 276 85 285 105 302 99 294 83
Polygon -7500403 true false 219 85 210 105 193 99 201 83

square
false
0
Rectangle -7500403 true true 30 30 270 270

square 2
false
0
Rectangle -7500403 true true 30 30 270 270
Rectangle -16777216 true false 60 60 240 240

star
true
0
Polygon -7500403 true true 151 1 185 108 298 108 207 175 242 282 151 216 59 282 94 175 3 108 116 108

target
false
0
Circle -7500403 true true 0 0 300
Circle -16777216 true false 30 30 240
Circle -7500403 true true 60 60 180
Circle -16777216 true false 90 90 120
Circle -7500403 true true 120 120 60

tree
false
0
Circle -7500403 true true 118 3 94
Rectangle -6459832 true false 120 195 180 300
Circle -7500403 true true 65 21 108
Circle -7500403 true true 116 41 127
Circle -7500403 true true 45 90 120
Circle -7500403 true true 104 74 152

triangle
false
0
Polygon -7500403 true true 150 30 15 255 285 255

triangle 2
false
0
Polygon -7500403 true true 150 30 15 255 285 255
Polygon -16777216 true false 151 99 225 223 75 224

truck
false
0
Rectangle -7500403 true true 4 45 195 187
Polygon -7500403 true true 296 193 296 150 259 134 244 104 208 104 207 194
Rectangle -1 true false 195 60 195 105
Polygon -16777216 true false 238 112 252 141 219 141 218 112
Circle -16777216 true false 234 174 42
Rectangle -7500403 true true 181 185 214 194
Circle -16777216 true false 144 174 42
Circle -16777216 true false 24 174 42
Circle -7500403 false true 24 174 42
Circle -7500403 false true 144 174 42
Circle -7500403 false true 234 174 42

turtle
true
0
Polygon -10899396 true false 215 204 240 233 246 254 228 266 215 252 193 210
Polygon -10899396 true false 195 90 225 75 245 75 260 89 269 108 261 124 240 105 225 105 210 105
Polygon -10899396 true false 105 90 75 75 55 75 40 89 31 108 39 124 60 105 75 105 90 105
Polygon -10899396 true false 132 85 134 64 107 51 108 17 150 2 192 18 192 52 169 65 172 87
Polygon -10899396 true false 85 204 60 233 54 254 72 266 85 252 107 210
Polygon -7500403 true true 119 75 179 75 209 101 224 135 220 225 175 261 128 261 81 224 74 135 88 99

wheel
false
0
Circle -7500403 true true 3 3 294
Circle -16777216 true false 30 30 240
Line -7500403 true 150 285 150 15
Line -7500403 true 15 150 285 150
Circle -7500403 true true 120 120 60
Line -7500403 true 216 40 79 269
Line -7500403 true 40 84 269 221
Line -7500403 true 40 216 269 79
Line -7500403 true 84 40 221 269

wolf
false
0
Polygon -16777216 true false 253 133 245 131 245 133
Polygon -7500403 true true 2 194 13 197 30 191 38 193 38 205 20 226 20 257 27 265 38 266 40 260 31 253 31 230 60 206 68 198 75 209 66 228 65 243 82 261 84 268 100 267 103 261 77 239 79 231 100 207 98 196 119 201 143 202 160 195 166 210 172 213 173 238 167 251 160 248 154 265 169 264 178 247 186 240 198 260 200 271 217 271 219 262 207 258 195 230 192 198 210 184 227 164 242 144 259 145 284 151 277 141 293 140 299 134 297 127 273 119 270 105
Polygon -7500403 true true -1 195 14 180 36 166 40 153 53 140 82 131 134 133 159 126 188 115 227 108 236 102 238 98 268 86 269 92 281 87 269 103 269 113

x
false
0
Polygon -7500403 true true 270 75 225 30 30 225 75 270
Polygon -7500403 true true 30 75 75 30 270 225 225 270

@#$#@#$#@
NetLogo 5.0.4
@#$#@#$#@
@#$#@#$#@
@#$#@#$#@
@#$#@#$#@
@#$#@#$#@
default
0.0
-0.2 0 0.0 1.0
0.0 1 1.0 0.0
0.2 0 0.0 1.0
link direction
true
0
Line -7500403 true 150 150 90 180
Line -7500403 true 150 150 210 180

@#$#@#$#@
0
@#$#@#$#@

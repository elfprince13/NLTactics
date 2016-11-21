extensions [bmpsprites burlap]
breed [ good-zealots good-zealot ]
breed [ bad-zealots bad-zealot ]


good-zealots-own [frame health team enemies enemy state]        ;; ranges from 1 to 9
bad-zealots-own [frame health team enemy state]        ;; ranges from 1 to 9
globals [walk-sequence sprites-table-w attack-sequence sprites-table-a death-sequence sprites-table-d HOLD MOVE ATTACK DEATH]

;;
;; SETUP PROCEDURES
;;

to-report name-agent
  report but-last but-first (word self)
end

to do-burlap
  let numguys 2
  let maxhealth 4
  setup-simple numguys maxhealth
  burlap:create-domain "sword"
  burlap:set-name-dependence "sword" true
  burlap:expose-agent-class "sword" "bad-zealot" false
  burlap:expose-agent-class "sword" "good-zealot" false
  burlap:expose-own-attr "sword" "bad-zealot" "enemy" (task enemy) (task [set enemy enemy]) burlap:attr:RELATIONAL
  burlap:expose-own-attr "sword" "bad-zealot" "health" (task [(WORD round health)]) (task [set health read-from-string ?]) burlap:attr:DISC
  burlap:expose-shared-attr "sword" "good-zealot" "health"
  burlap:expose-shared-attr "sword" "good-zealot" "enemy"
  burlap:expose-own-attr "sword" "good-zealot" "enemies" (task bad-zealots) (task [set enemies bad-zealots]) burlap:attr:MULTITARGETRELATIONAL
  burlap:set-attr-range:DISC "sword" "health" n-values (1 + maxhealth) [(WORD ?)]
  
  burlap:set-class-based-rewards "sword" (list "good-zealot" "bad-zealot") (list (task 0) (task [ifelse-value (state = DEATH) [10][0]]))
  let terminaltask (task [(0 = count (good-zealots with [health != 0])) or (0 = count (bad-zealots with [health != 0]))])
  burlap:set-terminal-function "sword" terminaltask
  let goodtask (task [simple-attack ?1])
  let badtask (task [simple-attack enemy])
  ;show (SENTENCE (sort ([name-agent] of good-zealots)) (sort ([name-agent] of bad-zealots)))
  burlap:add-deterministic-composite-action "sword" "attack" (SENTENCE (sort ([name-agent] of good-zealots)) (sort ([name-agent] of bad-zealots))) (SENTENCE (n-values numguys [goodtask]) (n-values numguys [badtask]) ) (SENTENCE (n-values numguys ["bad-zealot"]) (n-values numguys [""])) (SENTENCE (n-values numguys [sort ([name-agent] of bad-zealots)]) (n-values numguys [[""]]))
  
  ;; POINTERS DO NOT SURVIVE state-reloading
  ;let goodguy one-of good-zealots
  
  let startstate burlap:capture-current-state "sword"
  show (WORD (burlap:evaluate-attribute-in-state startstate ([name-agent] of good-zealot 0) "health") " " (burlap:evaluate-attribute-in-state startstate ([name-agent] of good-zealot 0) "enemies"))
  repeat 20 [
    ;show count good-zealots with [state != DEATH]
    ;show count bad-zealots with [state != DEATH]
    ;show "moo"
    if (count good-zealots with [state != DEATH]) > 0 and (count bad-zealots with [state != DEATH]) > 0[
      go-simple
      let thisstate burlap:capture-current-state "sword"
      show (WORD (burlap:evaluate-attribute-in-state thisstate ([name-agent] of good-zealot 0) "health") " " (burlap:evaluate-attribute-in-state thisstate ([name-agent] of good-zealot 0) "enemies"))
    ]
  ]
  burlap:return-to-state "sword" startstate
  let thisstate burlap:capture-current-state "sword"
  show (WORD (burlap:evaluate-attribute-in-state thisstate ([name-agent] of good-zealot 0) "health") " " (burlap:evaluate-attribute-in-state thisstate ([name-agent] of good-zealot 0) "enemies"))
  ;let qpolicy burlap:try-qlearning "sword" 0.95 0.001 1000 thisstate
  let qpolicy burlap:get-modeled-greedyq numguys 0 maxhealth
  while [not (runresult terminaltask)][
    show runresult terminaltask
    ask good-zealots [show (SENTENCE health)]
    ask bad-zealots [show (SENTENCE health)]
    set thisstate burlap:consult-policy "sword" qpolicy thisstate false
    ;show (WORD (burlap:evaluate-attribute-in-state thisstate ([name-agent] of good-zealot 0) "health") " " (burlap:evaluate-attribute-in-state thisstate ([name-agent] of good-zealot 0) "enemies"))
  ]
  ask good-zealots [show (SENTENCE health)]
  ask bad-zealots [show (SENTENCE health)]
    
  
  burlap:destroy-domain "sword"
end

to-report round-heading [theta]
  report ((round (theta / 32)) * 32) mod 360
end

to-report ani-heading [theta]
  report (round ((theta / 360) * 32)) mod 32
end

to-report hflip-heading [h]
  report (h > 16)
end

to-report index-offset-from-heading [h]
  report 16 - (abs (16 - h))
end

to draw-sprite-from-heading
  let h ani-heading heading
  if state = HOLD or state = MOVE [
    bmpsprites:draw-sprite (item ((frame * 17) + (index-offset-from-heading h)) sprites-table-w) [0.5 -0.5] (hflip-heading h) false
  ]
  
  if state = ATTACK [
    bmpsprites:draw-sprite (item ((frame * 17) + (index-offset-from-heading h)) sprites-table-a) [0.5 -0.5] (hflip-heading h) false
  ]
  
  if state = DEATH [
    bmpsprites:draw-sprite (item frame sprites-table-d) [0.5 -0.5] (hflip-heading h) false
  ]
end

to setup-simple [n h]               ;; executed when we press the SETUP button
  clear-all                       ;; clear all patches and turtles
  
  set HOLD 0
  set MOVE 1
  set ATTACK 2
  set DEATH 3
  
  create-good-zealots n [
    set xcor (max-pxcor - min-pxcor) / 2 + min-pxcor
    set ycor (max-pycor - min-pycor) / 2 + min-pycor
    set enemies bad-zealots
    set health h
    set state ATTACK
    set team 1
    update-color
  ]
  create-bad-zealots n [
    set xcor (max-pxcor - min-pxcor) / 2 + min-pxcor
    set ycor (max-pycor - min-pycor) / 2 + min-pycor
    set enemy one-of good-zealots with [0 = count (bad-zealots with [enemy = myself])]
    set health h
    set state ATTACK
    set team 2
    update-color
  ]
  
  ask good-zealots [
    set enemy one-of bad-zealots with [enemy = myself]
  ]
  
  ask patches [set pcolor green + 2]
  reset-ticks
end

to setup                          ;; executed when we press the SETUP button
  clear-all                       ;; clear all patches and turtles
  
  set HOLD 0
  set MOVE 1
  set ATTACK 2
  set DEATH 3
  
  bmpsprites:new-bitmap-spritesheet "zealot" "./zealot.png" [128 128] [0 0] [2176 1792] false
  set walk-sequence [85 102 119 136 153 170 187 204]
  set attack-sequence [0 17 34 51 68]
  set death-sequence [221 222 223 224 225 226 227 228]
  set sprites-table-w n-values ((length walk-sequence) * 17) [bmpsprites:get-indexed-sprite-with-chroma "zealot" (85 + ?) [1 0 1]]
  set sprites-table-a n-values ((length attack-sequence) * 17) [bmpsprites:get-indexed-sprite-with-chroma "zealot" (0 + ?) [1 0 1]]
  set sprites-table-d n-values (length death-sequence) [bmpsprites:get-indexed-sprite-with-chroma "zealot" (221 + ?) [1 0 1]]
  create-good-zealots (15 + (random 15)) [
    
    set heading round-heading (random 360)                ;; i.e. to the right
    set frame 0
    set state HOLD
    set health 10
    set team 1
    update-color
    set xcor (random (max-pxcor - min-pxcor)) + min-pxcor
    set ycor (random (max-pycor - min-pycor)) + min-pycor
    set enemy nobody
    set size 1
    set hidden? true
    draw-sprite-from-heading
  ]
  create-good-zealots (15 + (random 15)) [
    
    set heading round-heading (random 360)                ;; i.e. to the right
    set frame 0
    set state HOLD
    set health 10
    set team 2
    update-color
    set xcor (random (max-pxcor - min-pxcor)) + min-pxcor
    set ycor (random (max-pycor - min-pycor)) + min-pycor
    set enemy nobody
    set size 1
    set hidden? true
    draw-sprite-from-heading
  ]
  ask patches [set pcolor green + 2]
  reset-ticks
end

;;
;; GO PROCEDURES
;;

to go
  clear-drawing
  animate
  do-attacks
  tick
end

to go-simple
  animate-simple
  do-attacks
  tick
end

to animate
  ask good-zealots [
    move-zealot
    draw-sprite-from-heading
  ]
  ask bad-zealots [
    move-zealot
    draw-sprite-from-heading
  ]
end

to animate-simple
  ask good-zealots [
    move-zealot-simple
  ]
  ask bad-zealots [
    move-zealot-simple
  ]
end

to do-attacks
  ask bad-zealots [
   if (enemy != nobody) and (distance enemy) <= 2 and (frame = 0) and (state = ATTACK) and ([state] of enemy) != DEATH [
     ask enemy [
       set health max (list 0 (health - 1))
       update-color
       if health <= 0 [
         set state DEATH
         set frame -1
       ]
     ]
   ] 
  ]
  
  ; here we will need to process our policy
  ask good-zealots [
   if (enemy != nobody) and (distance enemy) <= 2 and (frame = 0) and (state = ATTACK) and ([state] of enemy) != DEATH [
     ask enemy [
       set health max (list 0 (health - 1))
       update-color
       if health <= 0 [
         set state DEATH
         set frame -1
       ]
     ]
   ] 
  ]
  
end

to update-color
  set color (10 + 90 * (team - 1)) + (health / 10) * 7
  ask my-out-links [set color ([color] of myself)]
end

to move-zealot
  ifelse state != DEATH [
    if enemy = nobody or ([state] of enemy) = DEATH [
      ask my-out-links [die]
      set enemy one-of turtles with [state != DEATH and team != ([team] of myself)]
      if (enemy != nobody) [create-link-to enemy [set color ([color] of myself)]]
    ]
    
    ifelse (enemy != nobody)  [
      set heading round-heading atan (([xcor] of enemy) - xcor) (([ycor] of enemy) - ycor)
      set frame frame + 1
      let state-sequence []
      ifelse ((distance enemy) > 2) and ((state = MOVE) or (state = ATTACK and frame = 4)) [
        set state MOVE
        forward 2 / (length walk-sequence) ;; try to match ground transform to animation
        set state-sequence walk-sequence
      ][
        set state ATTACK
        set state-sequence attack-sequence
      ]
      if frame >= length state-sequence
        [ set frame 0 ]             ;; go back to beginning of cycle of animation frames
        
    ] [
      set state HOLD
      set frame 0
      
    ]
  ][
    set frame frame + 1
    if frame > 7 [
      set frame 7
      ask my-out-links [die]
      ask my-in-links [die]
    ]
  ]
end

to simple-attack [target]
  if state != DEATH [
    ask target [
      set health max (list 0 (health - 1))
    ]
  ]
end

to move-zealot-simple
  ifelse state != DEATH [
    if enemy = nobody or ([state] of enemy) = DEATH [
      ask my-out-links [die]
      set enemy one-of turtles with [state != DEATH and team != ([team] of myself)]
      if (enemy != nobody) [create-link-to enemy [set color ([color] of myself)]]
    ]
    
    ifelse (enemy != nobody)  [
      set state ATTACK
      set frame 0          ;; go back to beginning of cycle of animation frames
        
    ] [
      set state HOLD
      set frame 0
      
    ]
  ][
    set frame -1
  ]
end

to vis-squares
  ask turtles [set hidden? true ask patch round xcor round ycor [set pcolor red]]
  ask turtle 0 [set hidden? true ask patch round xcor round ycor [set pcolor blue]]  
end
@#$#@#$#@
GRAPHICS-WINDOW
189
14
724
570
0
0
525.0
1
10
1
1
1
0
0
0
1
0
0
0
0
1
1
1
ticks
15.0

BUTTON
16
40
86
73
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
105
40
175
73
go
if (count bad-zealots) > 0 and (count good-zealots) > 0[\ngo\n]
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
29
214
170
247
NIL
reset-perspective
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
29
180
170
213
Follow Goodguy
follow one-of good-zealots
NIL
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

This code example shows how to use sprite sheets to created animated raster graphics

## HOW IT WORKS

For each zealot, a counter keeps track of the current animation frame and a state variable keeps track of the current animation. The headings are rounded to the nearest 32nd of a circle, and a sprite is selected based on that heading.

## NETLOGO FEATURES

The model has a frame rate setting of 15 frames per second, for smooth animation that isn't too fast.  The speed can be further adjusted by the user using the speed slider.
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
Polygon -10899396 true false 150 300 165 300 165 285 165 270

flower-1
false
0
Polygon -10899396 true false 150 300 165 300 165 285 165 270

flower-10
false
0
Polygon -10899396 true false 135 120 165 165 180 210 180 240 150 300 165 300 195 240 195 195 165 135
Polygon -10899396 true false 189 233 219 188 249 173 279 188 234 218
Polygon -10899396 true false 180 255 150 210 105 210 75 240 135 240
Polygon -10899396 true false 165 150 150 150 135 135 135 105 150 105 165 135
Polygon -7500403 true true 135 120 150 135 135 105

flower-11
false
0
Polygon -10899396 true false 135 120 165 165 180 210 180 240 150 300 165 300 195 240 195 195 165 135
Polygon -10899396 true false 189 233 219 188 249 173 279 188 234 218
Polygon -10899396 true false 180 255 150 210 105 210 75 240 135 240
Polygon -10899396 true false 150 135 120 120 120 105 135 90 150 90 165 120
Polygon -7500403 true true 150 90 150 120 135 90
Polygon -7500403 true true 120 105 120 120 150 120
Line -7500403 true 150 90 150 120

flower-12
false
0
Polygon -10899396 true false 135 120 165 165 180 210 180 240 150 300 165 300 195 240 195 195 165 135
Circle -10899396 true false 133 105 32
Circle -10899396 true false 152 90 28
Circle -10899396 true false 118 88 32
Circle -10899396 true false 133 73 32
Circle -7500403 true true 125 80 50
Polygon -10899396 true false 189 233 219 188 249 173 279 188 234 218
Polygon -10899396 true false 180 255 150 210 105 210 75 240 135 240
Circle -16777216 true false 147 102 6

flower-13
false
0
Polygon -10899396 true false 135 120 165 165 180 210 180 240 150 300 165 300 195 240 195 195 165 135
Circle -10899396 true false 133 120 32
Circle -10899396 true false 167 90 28
Circle -10899396 true false 103 88 32
Circle -10899396 true false 133 58 32
Circle -7500403 true true 120 75 60
Polygon -10899396 true false 189 233 219 188 249 173 279 188 234 218
Polygon -10899396 true false 180 255 150 210 105 210 75 240 135 240
Circle -16777216 true false 144 99 12

flower-14
false
0
Circle -10899396 true false 103 58 32
Polygon -10899396 true false 135 120 165 165 180 210 180 240 150 300 165 300 195 240 195 195 165 135
Circle -10899396 true false 103 120 32
Circle -7500403 true true 133 135 32
Circle -7500403 true true 182 90 28
Circle -10899396 true false 167 60 28
Circle -10899396 true false 167 122 28
Circle -7500403 true true 88 88 32
Circle -7500403 true true 133 43 32
Circle -7500403 true true 105 60 90
Polygon -10899396 true false 189 233 219 188 249 173 279 188 234 218
Polygon -10899396 true false 180 255 150 210 105 210 75 240 135 240
Circle -16777216 true false 135 90 30

flower-15
false
0
Polygon -10899396 true false 135 120 165 165 180 210 180 240 150 300 165 300 195 240 195 195 165 135
Circle -7500403 true true 90 137 28
Circle -7500403 true true 130 147 38
Circle -7500403 true true 192 85 38
Circle -7500403 true true 90 45 28
Circle -7500403 true true 182 45 28
Circle -7500403 true true 182 137 28
Circle -7500403 true true 70 85 38
Circle -7500403 true true 130 25 38
Circle -7500403 true true 96 51 108
Circle -16777216 true false 120 75 60
Polygon -10899396 true false 189 233 219 188 249 173 279 188 234 218
Polygon -10899396 true false 180 255 150 210 105 210 75 240 135 240

flower-16
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

flower-2
false
0
Polygon -10899396 true false 150 300 180 255 180 270 165 300

flower-3
false
0
Polygon -10899396 true false 150 300 180 240 195 225 195 240 165 300

flower-4
false
0
Polygon -10899396 true false 150 300 180 240 180 210 195 225 195 240 165 300
Polygon -10899396 true false 180 255 165 240 150 240

flower-5
false
0
Polygon -10899396 true false 150 300 180 240 180 210 180 165 195 195 195 240 165 300
Polygon -10899396 true false 180 255 135 225 105 240 135 240

flower-6
false
0
Polygon -10899396 true false 150 300 180 240 180 210 165 165 165 150 195 195 195 240 165 300
Polygon -10899396 true false 180 255 135 210 120 210 90 225 105 240 135 240
Polygon -10899396 true false 185 235 210 210 222 208 210 225

flower-7
false
0
Polygon -10899396 true false 180 255 150 210 105 210 83 241 135 240
Polygon -10899396 true false 150 300 180 240 180 210 165 150 150 135 165 135 195 195 195 240 165 300
Polygon -10899396 true false 189 230 217 200 235 195 255 195 232 210

flower-8
false
0
Polygon -10899396 true false 135 120 165 165 180 210 180 240 150 300 165 300 195 240 195 195 165 135
Polygon -10899396 true false 189 233 219 188 240 180 255 195 228 214
Polygon -10899396 true false 180 255 150 210 105 210 75 240 135 240
Polygon -10899396 true false 150 135 135 135 135 120 150 120

flower-9
false
0
Polygon -10899396 true false 135 120 165 165 180 210 180 240 150 300 165 300 195 240 195 195 165 135
Polygon -10899396 true false 189 233 219 188 240 180 270 195 234 218
Polygon -10899396 true false 180 255 150 210 105 210 75 240 135 240
Polygon -10899396 true false 150 135 135 120 135 105 150 105
Line -7500403 true 135 105 150 120

flower12
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

man standing
false
0
Circle -13345367 true false 112 23 75
Rectangle -13345367 true false 136 91 164 209
Polygon -13345367 true false 136 107 106 107 91 149 105 149 120 121 136 121 181 121 195 150 211 150 195 106
Polygon -13345367 true false 136 209 106 225 106 255 90 255 90 269 121 269 121 240 150 225 180 240 180 269 211 269 211 256 196 256 196 225 165 210

pentagon
false
0
Polygon -7500403 true true 150 15 15 120 60 285 240 285 285 120

person-1
false
0
Polygon -7500403 true true 120 195 120 105 135 90 195 90 195 105 195 120 180 180
Circle -7500403 true true 125 5 80
Rectangle -7500403 true true 147 73 183 90
Polygon -7500403 true true 165 195 120 180 96 228 60 255 75 300 133 242
Polygon -7500403 true true 180 180 120 180 156 232 159 299 199 299 195 223
Polygon -7500403 true true 135 90 95 125 60 180 90 195 127 137 150 120
Polygon -7500403 true true 180 136 180 151 206 202 234 190 210 136 195 91

person-2
false
0
Polygon -7500403 true true 180 136 165 166 192 203 220 191 202 147 191 98
Circle -7500403 true true 125 5 80
Rectangle -7500403 true true 147 73 183 90
Polygon -7500403 true true 168 176 117 160 105 225 75 255 90 300 135 255
Polygon -7500403 true true 174 164 120 180 150 225 150 300 195 300 195 225
Polygon -7500403 true true 105 180 120 105 135 90 187 89 195 105 195 120 180 180
Polygon -7500403 true true 131 91 96 135 75 180 120 195 135 150 150 120

person-3
false
0
Polygon -7500403 true true 171 123 181 162 165 200 196 201 198 150 194 104
Polygon -7500403 true true 165 180 120 180 120 225 90 285 135 300 165 225
Polygon -7500403 true true 180 180 121 180 158 239 135 300 180 300 180 225
Circle -7500403 true true 125 5 80
Rectangle -7500403 true true 147 73 183 90
Polygon -7500403 true true 116 192 120 105 135 90 180 90 195 105 195 120 180 195
Polygon -7500403 true true 135 90 120 105 93 193 128 199 135 150 150 120

person-4
false
0
Polygon -7500403 true true 180 135 180 165 175 205 186 203 190 135 183 89
Circle -7500403 true true 125 5 80
Rectangle -7500403 true true 147 73 183 90
Polygon -7500403 true true 165 180 120 180 120 225 120 300 165 300 165 225
Polygon -7500403 true true 181 182 151 182 136 227 136 302 181 302 181 212
Polygon -7500403 true true 120 190 120 105 130 90 180 90 195 105 195 120 180 195
Polygon -7500403 true true 145 90 130 102 120 135 120 165 120 210 150 210

person-5
false
0
Circle -7500403 true true 125 5 80
Rectangle -7500403 true true 147 73 183 90
Polygon -7500403 true true 180 180 135 180 135 225 120 300 180 300 180 225
Polygon -7500403 true true 195 180 120 180 135 225 150 300 195 300 201 222
Polygon -7500403 true true 120 195 120 105 135 90 180 90 195 105 195 120 180 195
Polygon -7500403 true true 180 135 180 165 180 195 195 195 195 120 183 89
Polygon -7500403 true true 136 128 136 158 136 203 166 203 165 120 142 95

person-6
false
0
Polygon -7500403 true true 165 180 120 180 122 222 105 300 150 300 165 225
Polygon -7500403 true true 182 174 120 178 165 240 173 298 214 294 210 225
Circle -7500403 true true 125 5 80
Rectangle -7500403 true true 147 73 183 90
Polygon -7500403 true true 116 192 120 105 135 90 180 90 195 105 195 120 180 195
Polygon -7500403 true true 135 90 105 120 93 193 120 195 120 135 150 120
Polygon -7500403 true true 150 124 150 154 161 201 195 199 193 133 183 93

person-7
false
0
Circle -7500403 true true 125 5 80
Rectangle -7500403 true true 147 73 183 90
Polygon -7500403 true true 165 180 120 180 105 240 90 300 135 300 150 240
Polygon -7500403 true true 165 166 120 181 177 229 180 297 221 289 213 210
Polygon -7500403 true true 120 195 120 105 135 90 180 90 195 105 195 120 189 193
Polygon -7500403 true true 122 103 100 133 79 195 111 202 124 165 154 120
Polygon -7500403 true true 180 133 165 163 180 210 210 195 198 146 195 105

person-8
false
0
Polygon -7500403 true true 183 168 138 183 178 239 203 300 240 280 213 226
Circle -7500403 true true 125 5 80
Rectangle -7500403 true true 147 73 183 90
Polygon -7500403 true true 120 180 120 105 135 90 180 90 195 105 195 120 195 180
Polygon -7500403 true true 135 90 99 127 66 183 92 197 118 158 150 120
Polygon -7500403 true true 180 133 165 163 210 195 232 179 203 141 183 87
Polygon -7500403 true true 165 180 120 180 105 240 75 300 120 300 150 240

person-9
false
0
Circle -7500403 true true 125 5 80
Rectangle -7500403 true true 147 73 183 90
Polygon -7500403 true true 165 180 120 180 105 225 68 289 105 300 150 225
Polygon -7500403 true true 188 185 135 180 163 240 171 300 217 301 200 229
Polygon -7500403 true true 121 180 121 105 136 90 181 90 195 97 196 120 187 185
Polygon -7500403 true true 135 90 90 135 60 180 90 195 120 150 150 120
Polygon -7500403 true true 185 137 185 154 222 199 246 179 215 139 185 92

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
false
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

x
false
0
Polygon -7500403 true true 270 75 225 30 30 225 75 270
Polygon -7500403 true true 30 75 75 30 270 225 225 270

@#$#@#$#@
NetLogo 5.0.4
@#$#@#$#@
setup
repeat 199 [ animate ]
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

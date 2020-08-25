;; New version of pjvegsim to include zone of influence equation

globals
[
  recruit-rate ;; chance of recruitment in next step
  c ;; constant for canopy size
  b ;; exponent for size asymmetric competition
    ;; 0 = complete symmetry (equal split)
    ;; 1 = perfect symmetry (proportional to biomass;
    ;; inf = complete size asymmetry (largest takes all)

]

patches-own
[
  pstress
  occupied?
  trees-on-me
  biomass-on-me
]

breed [people person]

people-own [
  energy ; people own energy which they harvest from patches
 ; will add more like socioeconomic status, etc.
]

breed [trees tree]

trees-own
[
  ;; Physical characteristics
  age
  height
  area
  stress

  ;; Biomass
  biomass-live
  biomass-dead
  biomass-max
  biomass-r
  dbiomass ;; biomass change this year

  ;; Mortality
  mort-alpha ;; low growth
  mort-beta  ;; decay
  mort-gamma ;; growth independent

  ;; Reproduction
  seed-min
  seed-mu
  seed-sigma

  my-patches

  dead?
]

to setup
  ca ;clear all

  set recruit-rate 0.01 ;maybe explore with slider later
  set c 1
  set b 1

  ask patches [
    set pstress random-float 1
    set pcolor scale-color gray pstress 0 1
    set occupied? false
    ;set trees-on-me []
    set trees-on-me no-turtles
    set biomass-on-me 0
  ]

  ask n-of patchno patches [
    ;if random-float 1 < pstress [
    make-a-tree
    set occupied? true
    ;]
  ]

  reset-ticks
end

to go
  if ticks = 500 [ ;; Allow a few hundred ticks to spin up
    create-people numberppl ; will vary later with slider
    [
      set shape "person"
      set energy random 10 ;give people a variable amount of energy to begin with
;    set energy 0 ; people start with no energy
;    setxy -10 0
    ]
  ]

  ask trees with [not dead?]
  [
    grow-tree
    survival
    if age > 25 [ reproduce ]
  ]

  ask people [
    move
    if any? trees-on patch-here
    [
      harvest-trees
    ]
  ]

  ask patches ;with [occupied?] ;; store biomass on patch for competition
  [
    set biomass-on-me sum [biomass-live] of trees-on-me
  ]

  tick
end

to move
    rt random 10
    fd 1
    set energy energy - .1 ;moving takes some energy
end

to harvest-trees ;; people procedure
  show "TREES?? OMG!!!" ;; people are excited
  ask [trees-on-me] of patch-here ;;patch procedure
  [
    if dead? [
      ask myself [set energy energy + 100]
;;      set [energy] of myself energy + 100
      ask patches in-radius calc-radius
      [
        set pcolor scale-color gray pstress 0 1
      ]
      die
    ]
  ]
end

to grow-tree
  set age age + 1
  set dbiomass biomass-r * (area - (biomass-live ^ 2 / biomass-max ^ (4 / 3))) * (1 - pstress)
  set biomass-live biomass-live + dbiomass
  set area calc-area
  set size calc-radius * 2
  set my-patches patches in-radius calc-radius
  ask patches in-radius calc-radius
  [
    set pcolor yellow
    ;if not member? myself trees-on-me [
      ;set trees-on-me lput myself trees-on-me ;; stores trees overlapping patch
      set trees-on-me (turtle-set myself trees-on-me) ;; stores trees overlapping patch
    ;]
  ]
  ;set stress calc-stress
end

to make-a-tree
  sprout-trees 1[
    set age 0
    set height 0.1

    set biomass-live 0.01
    set biomass-dead 0
    set biomass-max 500
    set biomass-r 0.1

    set stress [pstress] of patch-here
    set area calc-area

    set mort-alpha 0.001
    set mort-beta 0.6
    set mort-gamma 0.0005

    set seed-min 25
    set seed-mu 180
    set seed-sigma 50

    set size 1 ;height
    set color green
    set shape "tree pine"
    set dead? false
    ;set my-patches []
    ;set my-patches lput patch-here my-patches
  ]
end

to survival
  let mort-prob mortality
  if random-float 1 < mort-prob [
;    print "I'm dying here"
;    show ticks
;    show mort-prob
    set color black
    set dead? true
    ;ask patches in-radius calc-radius
    ;[
     ; set pcolor scale-color gray pstress 0 1
    ;]
  ]
end

to reproduce
  ;let reprod-rate random-normal seed-mu seed-sigma
  ;; Basic normal density
  let reprod-rate (1 / (seed-sigma * sqrt (2 * pi))) * exp(-0.5 * ((age - seed-mu) / seed-sigma)^ 2)

  if random-float 1 < reprod-rate [
    ask one-of patches with [not occupied?] [
      make-a-tree
      set occupied? true
    ]
  ]

end

to-report mortality
  report mort-gamma + mort-alpha * exp(-1 * mort-beta * dbiomass)
end

to-report calc-stress
  let npatch 0
  let prop 0
  let tmp-stress 0
  let tmp-stressf 0
  ask my-patches [
    set prop prop + [biomass-live] of myself / biomass-on-me
    set tmp-stress tmp-stress + (pstress * ([biomass-live] of myself / biomass-on-me))
    set tmp-stressf tmp-stressf + pstress
    set npatch npatch + 1
  ]
  show prop
  show npatch
  show prop / npatch
  show tmp-stress
  show tmp-stressf
  report tmp-stress / npatch
end

to-report calc-area-symm
end

to-report calc-area
  report c * biomass-live ^ (2 / 3)
end

to-report calc-radius
  report sqrt area / pi
end

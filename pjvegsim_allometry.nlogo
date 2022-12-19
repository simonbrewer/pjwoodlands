;; comment for whole team

;;-	Made Trees and Foragers their own breeds
;;-	Added a factor to punish the energy return from live trees as well as differentiate standing vs fallen dead tree returns (standing return slightly less energy than fallen which give full amount)
;;-	Added agents and agent behaviors

;;Needs remaining
;;- validate all working
;;- Put values on same unit scales & make them reasonable (truck space, wood per tree, energy from unit of wood per tree, annual energy need, etc.)

;; new updates with total biomass equations based on FIA data
;; from Cunliffe et al; McIntire et al

extensions [ csv profiler ]

breed [trees tree]
breed [foragers forager]

globals [
  ;; Parameters for deriving allometric equation (height)
  hgt-asym-mean
  hgt-lrc-mean
  hgt-asym-sd
  hgt-lrc-sd
  hgt-asym-wc
  hgt-lrc-wc
  hgt-corr

  ;; Parameters for deriving allometric equation (diameter)
  diam-asym-mean
  diam-lrc-mean
  diam-asym-sd
  diam-lrc-sd
  diam-asym-wc
  diam-lrc-wc
  diam-corr

  ;; Parameters for deriving allometric equation (crown area)
  carea-asym-mean
  carea-lrc-mean
  carea-asym-sd
  carea-lrc-sd
  carea-asym-wc
  carea-lrc-wc
  carea-corr

  ;; Parameters for deriving CMass from diameter
  cwood-mean
  cwood-sd

  ;; Parameters for deriving CMass from height and carea
  cwood-hgt-coef-a
  cwood-hgt-coef-b
  cwood-carea-coef-a
  cwood-carea-coef-b

  ;; Mean / SD of water content
  log-wc-mean
  log-wc-sd

  ;; Mortality coefficients
  max-age
  mort-asym
  mort-lrc

  ;; Trackers
  dead-trees
  new-trees
  removed-trees

  ;; Fire variables
  fire-front
  fire-size
  all-fire-sizes

  ;; Agent tracking variables
  all-agent-truckload-sum ;;running list of total truckload space used by each agent each turn
  all-trips-home ;;running list of the total number of foraging trips taken per turn per agent
  all-agent-wood-taken ;;running list of total wood taken by each agent each turn
]

trees-own [
  ;; Allometric coefficients
  hgt
  hgt-asym
  hgt-lrc
  diam
  diam-asym
  diam-lrc
  carea
  carea-asym
  carea-lrc

  ;; Cmass values
  cwood-coef
  cwood
  max-live-cwood

  ;; Other individual characteristics
  age
  species
  species-number
  reproductive-age
  live?
  standing?
  age-since-death
  decay-rate-standing
  decay-rate-fallen
  pfall ;;  prob of falling once dead
  avail-megajoules ;;total energy stored in tree - eventually will be kg of material in tree * megajoule conversion factor (21 for pine 16 for juniper)
  ;;right now this is c-wood * 21 or 16
  extra-vol-multiplier ;the excess space (volume) taken up by the wood without extra processing (i.e., due to cut wood's abnormal shapes, not all of the
  ;physical space in the truck will be used - there will be empty space - unless extra processing is done)
  dist-from-home-base ;;patch distance from the home-base patch (all agents have same home base)
  travel-cost-here-home ;;this is a temporary variable used within movement by individual agents that is the distance a patch is from where the agent is
  ;;at the moment plus the distance the patch is from home base (so total travel cost to get to the patch and home)
  RR ;;return rate obtained for an agent beginning at the home-base patch. RR is the energy available on the patch divided by the distance from home.
  temp-RR ;;return rate for an agent who has left home, harvested from a patch, and is now looking for another. So energy on a patch divided by distance from current patch.
  harvested-from? ;;this is a temporary state variable for use in developing the model just to make it easy to visualize which patches have been harvested
  max-load-energy ;;the total kilojoules an agent can extract from the patch on their harvest (capped at energy from a max-truckload)
  mj-energy-multiplier ;; this is the multiplier for determining how much energy is in the amount of available wood. This will be megajoules / kg^2 eventually. Right now abstract.

  ;; Fire variables
  flammability ;; Probability of igniting given burning neighbor
  burning?
]

foragers-own [
  max-truckload-empty ;;max amount of firewood an agent can haul in one trip with an empty truck
  truckload-space-taken ;;the amount of space taken up by firewood an agent is currently hauling
  yearly-need ;;amount of kilojoules agent needs for the year
  dist-travel-year ;;distance the agent travelled for the year
  lifetime-travel ;;a list of the distances travelled each year
  wood-taken-patch ;;amount of wood harvested from the single patch the agent is currently on
  wood-taken ;;amount of wood harvested in a year
  wood-taken-lifetime ;;amount of wood harvested over the full run
  finished ;;bivariate TRUE / FAlSE for if agent has acquired total energy needs on the turn
  energy-obtained ;;how many kilojoules of energy the agent has acquired within the year
  trips-home-counter ;;a counter for how many trips back home (i.e., foraging bouts) an agent engages in during the year
  truckload-taken ;;a list of how much space was used in the truck when the agent went home to unload
  space-taken ;;how much space has been taken up in the truck from harvesting on the particular patch
  wood-per-patch ;;list of how much wood was taken on a trip
  ;;sum of wood-per-patch list is the total wood taken in a year. sum of truckload-taken is the total space of the truck used.
  ;;If wood takes up no extra volume/space - sum of these two lists will be the same. But if wood takes up extra space (i.e., not post processed)
  ;;then the sum of truckload-taken will be larger than the sum of wood-per-patch.
  ;; travel-dist-per-bout ;; the distance (in patches) that an agent can travel on each of their foraging bouts
  travel-dist-per-year ;; the distance (in patches) that an agent can travel per tick (year) - emulates the amount of time they have for foraging
  ;;bout-travel ;;the distance the agent has travelled on the active foraging bout
  year-travel ;;the distance the agent has travelled on the active foraging bout
  poss-stands ;;the possible stands an agent can reach given their current travel limitations
  poss-trees ;;the set of trees it is possible for the agent to reach based on their travel distance limit
  no-place ;;a binary recording if an agent can't get their full energy quota this turn
  extra-year-travel ;;the additional time/distance an agent will spend if they are not solely time minimizing
  extra-energy-obtained;; the additional energy a forager has obtained from engaging in some percentage of energy maximizing
  total-extra-energy-obtained
  t-option-trees
  lifetime-pinyon
  lifetime-juniper
]

patches-own [
  occupied? ;; Tree present or not
  wc ;; Water content of soil (Guess units)
  suitability
  max-suitability
  n-fires
  stand-cwood-live
  stand-cwood-dead-standing
  stand-cwood-dead-fallen
  stand-mj-live ;the total megajoules of energy in the stand held in living trees
  stand-mj-dead-standing ;the total megajoules of energy in the stand held in standing dead trees
  stand-mj-dead-fallen ;the total megajoules of energy in the stand held in fallen dead trees
  stand-RR ;the return rate a forager may expect from foraging in a given stand (total energy available / distance)
  home-base? ;;Is this patch the start point for foragers or no
]

to setup
  ca
  reset-ticks


  ;; List of all fires sizes
  set all-fire-sizes []
  ;; Read in parameters
  set-params

  ;; Patch set up
  ask patches [
    set pcolor white
    set occupied? false
    set wc exp random-normal log-wc-mean log-wc-sd
    set n-fires 0

    ;; Patch suitability (not linked to wc atm)
    let tmp-suitability-0 random-float 1 * ( pycor / max-pycor )
    let tmp-suitability-1 random-float 1 * (( max-pycor - pycor ) / max-pycor)
    set max-suitability (list tmp-suitability-0 tmp-suitability-1 )
    set suitability max-suitability

  ]

  ;; ask patches [set pcolor scale-color red item 0 max-suitability 0 1 ]
  ;; Create initial trees (50/50 pine or juniper)
  ask n-of 50 patches [
    sprout-trees 1 [
      ifelse item 0 max-suitability > item 1 max-suitability
      [
        set species-number 0
        recruitment
      ] [
        set species-number 1
        recruitment
      ]

    ]
    set occupied? true
  ]

  ;; Opens a monitor for tree 0
  ;stop-inspecting-dead-agents
  ;inspect turtle 0

  ;; Establish a patch as the start point for all foragers
  ask patches [set home-base? FALSE]
  ask patch 0 0 [set home-base? TRUE]

  ;; Agent tracking lists
  set all-agent-truckload-sum [];;running list of total truckload space used by each agent each turn
  set all-trips-home [];;running list of the total number of foraging trips taken per turn per agent
  set all-agent-wood-taken [];;running list of total wood taken by each agent each turn

  ;; Create human foragers
  make-foragers
end

to go

  set dead-trees 0
  set new-trees 0
  set removed-trees 0

  if not any? trees with [live?] [stop]
  if ticks > n-time-steps [
    if fire? [
      ;;ask turtles with [not live?] [die] ;; test for fires with all dead trees removed
      repeat 50 [
        calc-flammability
        spark
        spread
        reset-trees
      ]
    ]
    stop
  ]

  ask trees with [live?] [
    grow
    if age > reproductive-age and any? neighbors with [not occupied?] [
      reproduce species-number
    ]
    death
  ]

  ;; Disturb trees
  ask trees with [not live? and standing?] [
    disturbance
    set age-since-death age-since-death + 1
  ]

  ;; Decay trees
  ask trees with [not live?] [
    decay
  ]

  ;; Remove some of the dead trees
  ask trees with [not live?] [
    remove-trees
  ]

  ask patches [
    ;; Update stand (have all stands update to accurate values)
    update-stand
  ]

  ;; Begin forager behavior
  if ticks > 49 [ ;; allow forest growth and death to happen before foragers begin operating
  reset-state-vars
  forage
  report-forager-vars
  ]

  tick
end

to grow
  ;; Increase age
  set age age + 1

  if age < reproductive-age [
    ;; Reduce suitability by ratio of age to reproductive age
    let age-ratio age / reproductive-age
    ask patch-here [
      set suitability replace-item 0 suitability (item 0 max-suitability * (1 - age-ratio))
      set suitability replace-item 1 suitability (item 1 max-suitability * (1 - age-ratio))
    ]
  ]

  if age = reproductive-age [
    set size 2
    ask patch-here [
      set suitability replace-item 0 suitability 0
      set suitability replace-item 1 suitability 0
    ]
  ]

  calc-diam ;; Only need to calculate this at death?
  calc-hgt
  calc-carea
  calc-cwood-carea

  if species = "pine" [set avail-megajoules ((cwood * mj-energy-multiplier) * Live_wood_energy)] ;; calculate the energy available, slight penalty for having to take down a standing dead tree
  if species = "juniper" [set avail-megajoules ((cwood * mj-energy-multiplier) * Live_wood_energy)];; calculate the energy available, slight penalty for having to take down a standing dead tree

end

to reproduce [tsn]
  ask one-of neighbors with [not occupied?] [
    if random-float 0.25 < item tsn suitability [ ;; NEEDS ADJUSTING
      sprout-trees 1 [
        set species-number tsn
        recruitment
      ]
      set occupied? true
      set new-trees new-trees + 1
    ]
  ]
end

to death
  ;;let pdeath (age / item species-number max-age) ^ 5

  let tmp-asym item species-number mort-asym
  let tmp-lrc item species-number mort-lrc
  set tmp-lrc tmp-lrc * 2
  let pdeath tmp-asym * ( 1 - exp(- exp( tmp-lrc ) * age ))
  ;print self
  ;print age
  ;print pdeath
  if random-float 1 < pdeath [
    set live? false
    set age-since-death 0
    set max-live-cwood cwood
    set color gray
    set dead-trees dead-trees + 1
    if species = "pine" [set avail-megajoules ((cwood * mj-energy-multiplier) * Standing_dead_energy)] ;; calculate the energy available, slight penalty for having to take down a standing dead tree
    if species = "juniper" [set avail-megajoules ((cwood * mj-energy-multiplier) * Standing_dead_energy)];; calculate the energy available, slight penalty for having to take down a standing dead tree
    ;; Uncomment these two lines to simulate immediate harvesting
    ;ask patch-here [ set occupied? false ] ;; Patches can be occupied following death of tree
    ;die
  ]


end

to disturbance
  ;; Tree fall - random chance to fall in each turn once dead
  ;; Can maybe be keyed to remaining biomass or time since death
  if random-float 1 < pfall [
    set standing? false
    set shape "logs"
  ]
end

to decay ;; combined decay function
  ;; Return rate (used to adjust suitability)
  let return-rate 0
  ifelse standing? [
    set return-rate (cwood * decay-rate-standing) / max-live-cwood
    set cwood cwood * (1 - decay-rate-standing)
    if species = "pine" [set avail-megajoules ((cwood * mj-energy-multiplier) * Standing_dead_energy)] ;; calculate the energy available, slight penalty for having to take down a standing dead tree
    if species = "juniper" [set avail-megajoules ((cwood * mj-energy-multiplier) * Standing_dead_energy)];; calculate the energy available, slight penalty for having to take down a standing dead tree
  ] [
    ifelse max-live-cwood > 0
    [set return-rate (cwood * decay-rate-fallen) / max-live-cwood]
    [set return-rate 0]
    set cwood cwood * (1 - decay-rate-fallen)
    if species = "pine" [set avail-megajoules ((cwood * mj-energy-multiplier) / 1)] ;; calculate the energy available, no penalty because tree is already fallen
    if species = "juniper" [set avail-megajoules ((cwood * mj-energy-multiplier) / 1)];; calculate the energy available, no penalty because tree is already fallen
  ]
  ask patch-here [
    set suitability replace-item 0 suitability ( item 0 suitability + ( return-rate * item 0 max-suitability ) )
    set suitability replace-item 1 suitability ( item 1 suitability + ( return-rate * item 1 max-suitability ) )
  ]

end

to remove-trees
  if max-live-cwood = 0 OR cwood / max-live-cwood < 0.1  [
    set removed-trees removed-trees + 1
    ask patch-here [ set occupied? false ]
    die
  ]

end

to calc-flammability
  ;; Modified from per Baak model
  ask trees with [live?] [
    set flammability 0.025 + 0.0003 * (age / 5) ^ 2
  ]
  ask trees with [ not live? and standing? ] [
    set flammability 0.25
  ]
  ask trees with [ not live? and not standing? ] [
    set flammability 0.5
  ]
end

to spark
  set fire-size 0
  ask one-of patches with [occupied?]
  [
    set pcolor red
    set n-fires n-fires + 1
    ask trees-here [
      set burning? true
      set color orange
      set fire-front turtle-set self
      set fire-size fire-size + 1
    ]
  ]
end

to spread
  while [ any? fire-front ] [
    ;; temp list to store new fire front
    let new-fire-front turtle-set nobody

    ask fire-front [
      ask ( trees-on neighbors ) with [ not burning? ] [
        if random-float 0.45 < flammability [
          ask patch-here [
            set pcolor red
            set n-fires n-fires + 1
          ]
          set burning? true
          set color orange
          set new-fire-front (turtle-set new-fire-front self) ;; extend the next round fron
          set fire-size fire-size + 1
        ]
      ]

      ;; Test here to see if original point is extinguished ;; NEEDS WAY TO RESET CWOOD POST-FIRE
      ;; if random-float 1 < 0.5 [
      ;if random-float 0.5 < cwood [
        ;;print self
        ;set new-fire-front (turtle-set new-fire-front self) ;; extend the next round fron
        ;set cwood cwood / 2
      ;]

      ;; Create new fire front for spread
      set fire-front new-fire-front
    ]
  ]
  set all-fire-sizes lput fire-size all-fire-sizes

end

to reset-trees
  ask trees [
    set burning? false
    set color green
  ]
end

to show-burn
  ask trees [ ht ]
  ask patches [
    set pcolor scale-color red n-fires 0 20
  ]
end

to make-foragers
  ask patch 0 0 [ ;make all foragers on the home-base patch
    sprout-foragers num_foragers ;create the chosen number of foragers
     [set shape "person" ;make agents person shape
      set finished FALSE ;upon creation, no forager has already acquired their annual energy need
      set max-truckload-empty Max_truck_capacity * 100 ;;cords * 100 to estimate kgs of wood a completely empty truck can haul (NEEDs TO BE PUT INTO APPROPRIATE UNIT VALUES)
      set yearly-need round (base_need + random-float need_variance) ;;set yearly energy need in megajoules - NEEDS TO BE EDITED FOR PROPER UNIT VALUES
      set wood-taken-lifetime 0 ;;start the agent having taken no wood
      set energy-obtained 0;;start having obtained no energy
      set dist-travel-year 0 ;;start with having no distance travelled
      set lifetime-travel [] ;make an empty list for the list of travel distances each year
      set trips-home-counter 0 ;;track number of times agents have gone home with a full truck (or finished harvest for year). (internal self-check)
      set truckload-taken [] ;start with an empty list of the amount of wood in the truck for each 'full' load
      set space-taken 0
      set truckload-space-taken 0
      set wood-per-patch []
      set travel-dist-per-year Max-travel
;        [set travel-dist-per-bout round (15 + random-float 50)];if running the model where agents have a limit to the distance they can travel, agents have a total number of patches they can move
;        ;per foraging bout. Right now this is between 25 and 100 patches away from home. Double this would be the max move they could make because it would be up to 100 out and 100 back.
;        [set travel-dist-per-bout 500];else if running the model with no distance limit, there is no need to worry about a travel distance limit so
;          ;;we will set it as 50,000. By which point agents will have had to have filled the truck
       ; set poss-trees trees in-radius (travel-dist-per-bout / 2) ;list of possible trees an agent can get to, divide by 2 to account for the fact they need to go out and back
       ; set poss-patches poss-patches with [home-base? = FALSE];drop home-base from possible foraging patches
      set no-place FALSE
      set total-extra-energy-obtained [] ;;make an empty list
      set extra-energy-obtained 0
      set lifetime-pinyon 0
      set lifetime-juniper 0
    ]
  ]

end

to reset-state-vars
  ;this is an agent and patch submodel to reset state variables to start fresh each tick (year)
  ask foragers [
   set dist-travel-year 0 ;;reset the distance travelled for each new year back to 0
   set truckload-space-taken 0 ;;start the year with an empty truckload
   set finished FALSE ;;start the year without having met the necessary firewood quota
   set wood-taken 0 ;start year having taken no wood
   set wood-taken-patch 0;start year having taken no wood from any patch
   set energy-obtained 0 ;start year having acquired no energy
   set trips-home-counter 0 ;start having taken no trips in the year
   set truckload-taken [] ;amount of truck space used per trip on each turn
   set space-taken 0
   set wood-per-patch []
   set year-travel travel-dist-per-year ;;year-travel is subtracted from, so start with the max distance an agent can go
   set no-place FALSE
   set extra-year-travel 1 ;;this starts as a positive value for implementation in code below, but agents who are purely time minimizing will zero this out before acting upon it
   set extra-energy-obtained 0
  ]

  ask trees [
    set temp-RR 0 ;;patches should reset the temp-RR at the start of the turn because no agent has gone
    set harvested-from? FALSE
    set max-load-energy 0
    set travel-cost-here-home 0
  ]

end

to forage
  ;this is an agent sub-model for foraging to acquire firewood for the year
  ask trees [set temp-RR 0]
  ask foragers [
    find-best-stand ;this submodel locates the best stand for the forager to go to
    harvest ;this submodel has agents go about acquiring their energy for the year via wood harvesting
  ]

end

to find-best-stand
  ;this is an agent sub-model for identifying the best initial stand to forage in
  ifelse year-travel <= 0 OR extra-year-travel <= 0
   [
    set finished TRUE
    if energy-obtained < yearly-need ;if the agent is ending their foraging without meeting their need
        [set no-place TRUE]
    go-home
   ]

  [; else, if they can still travel, find the best stands
    let start-patch patch-here
    ifelse energy-obtained < yearly-need
    [
      set poss-stands patches in-radius (year-travel / 2)
    ]
    [
      set poss-stands patches in-radius (extra-year-travel / 2)
    ]

    let best-stand max-one-of poss-stands [stand-RR]
    if best-stand = nobody or [stand-RR] of best-stand <= 0 [;if there is no wood left to go get, set finished true, set best-patch to home patch and go-home.
         set finished TRUE
         set best-stand patch 0 0
         go-home
       ]

    move-to best-stand
    ifelse energy-obtained < yearly-need
    [ ;if the yearly need in energy is not yet met
      set year-travel (year-travel - distance start-patch)
      set dist-travel-year (dist-travel-year + distance start-patch)
    ]

    [
      set extra-year-travel (extra-year-travel - distance start-patch)
      set dist-travel-year (dist-travel-year + distance start-patch)
    ]
    find-best-tree ;this submodel locates the best tree within the stand for the agent based on RR
  ]

end

to find-best-tree
  ;this is an agent sub-model for locating the best RR patch and moving there and recording distance travelled

  let start-patch patch-here ;remember temporarily the patch the agent begins on (for the first time they move in a turn, this will be the home-base patch)
  ask trees in-radius stand-size [set travel-cost-here-home (distance start-patch + dist-from-home-base)]
  ifelse energy-obtained < yearly-need
    [
      set poss-trees trees in-radius stand-size with [[travel-cost-here-home] of self <= [year-travel] of myself]
    ]
    [
      set poss-trees trees in-radius stand-size with [[travel-cost-here-home] of self <= [extra-year-travel] of myself]
    ]
  ask poss-trees [calc-temp-RR]
  let best-tree max-one-of poss-trees [temp-RR] ;;identify the patch that gives the best return rate (which is a function of the kilojoules acquired from the available biomass and distance from home)
  if  best-tree = nobody or [temp-RR] of best-tree <= 0 [;if there is no wood left to go get, set finished true, set best-patch to home patch and go-home. Best patch to home patch is because the below move code still tries to happen.
    set finished TRUE
    set best-tree patch 0 0
    set no-place TRUE
  ]
   move-to best-tree;;else, move to the best tree in the stand
      ifelse energy-obtained < yearly-need
      [  ;if the yearly need in energy is not yet met
        set year-travel (year-travel - distance start-patch) ;;record the distance the agent has gone to the new patch
        set dist-travel-year (dist-travel-year + distance start-patch) ;add this move distance (from current patch to new foraging patch) to the distance travelled for the year
      ]
      [ ;if the yearly need is met (i.e., agents are getting more firewood than min necessary
        set extra-year-travel (extra-year-travel - distance start-patch)
        set dist-travel-year (dist-travel-year + distance start-patch)
      ]

end

to calc-temp-RR
  ;this is a patch submodel (called by agents) that sets a return rate raster for foraging based on only the patches the agent can access resultant from their travel distance limit

;  ask poss-trees [ ;ask patches within a distance where the agent could get out to the patch and back home
      ifelse avail-megajoules = 0
      [set temp-RR 0];if there is no energy present, set this to 0
    [ifelse distance myself = 0 ;;cost distance. Megajoules divided by distance from home-base. This is the return rate the agent can get
      [set temp-RR (avail-megajoules)] ;; a tree on patch 0 0 has no distance factored in
      [set temp-RR (avail-megajoules / distance myself)]
    ]
;  ]

end

to harvest
  ;this is an agent submodel for harvesting wood. In here the agent needs to take biomass from the patch, ask the patch to lose the taken biomass, ask the patch to calculate its new RR
;and continue to harvest / forage until they meet their yearly need (including trips home to unload if necessary). this is called by an individual agent from the forage code above

ifelse finished = TRUE
  [];if finished = true, don't try to harvest
[  ;otherwise, harvest
  while [finished = FALSE] ;if the agent hasn't filled their quota for the year
  [
    let target-tree max-one-of trees-here [avail-megajoules]
    ;calculate how much space you have in the truck and how much energy you still need to get
    let max-truckload precision ((max-truckload-empty - truckload-space-taken) - ((max-truckload-empty - truckload-space-taken) * [extra-vol-multiplier] of target-tree)) 6
    ;the max wood an agent can load in the truck is the max amount their empty truck can haul minus any wood they have in the truck already and minus the extra space that will be taken up by
    ;harvested wood that is not post-processed (the empty space that is not able to be used)
    let energy-still-need (yearly-need - energy-obtained) ;;calculate how much energy agent still needs for the year as total needed minus total taken this year
    ask target-tree [ ;have the agent ask the tree they are currently targeting (and intend to harvest from)
    ifelse (cwood + (cwood * extra-vol-multiplier)) >= (max-truckload + (max-truckload * proportion_harvest_remain)) ;if the patch has equal to or more firewood than the agent's truck can haul
        ;plus the extra percent that is leftover as a result of harvest leaving smaller pieces behind and with unused space in the truck factored in
      [set max-load-energy (max-truckload * mj-energy-multiplier)] ;;calculate max-load-energy the agent can get as the kilojoules for the truckload of this wood type
      ;if the patch las less wood than the agent's truck can haul
      [set max-load-energy (avail-megajoules - (avail-megajoules * proportion_harvest_remain))] ;;set the max-load-energy to be the total kilojoules of energy on the patch minus the proportion of the harvest
        ;that will not be kept/used
  ]

    ifelse energy-still-need <= [max-load-energy] of target-tree ;if the agent's energy need is less than the maximum energy the agent can get from the load (i.e., the agent can fill their quota)
  [;start if
    ifelse Time_vs_Energy_max = 0 ;;if agents are time minimizing,
        [; just be done
          let wood-need (energy-still-need / [mj-energy-multiplier] of target-tree);calculate the kg wood yet needed to meet the yearly energy requirement
          set truckload-space-taken (truckload-space-taken + (wood-need + (wood-need * [extra-vol-multiplier] of target-tree)));add this wood to any already in the truck (or to an empty truck)
           ;accounting for the empty space
          set wood-per-patch lput wood-need wood-per-patch
          set wood-taken (wood-taken + wood-need) ;record how much wood has been taken this year
          set energy-obtained (energy-obtained + energy-still-need) ;;record how much energy the agent has obtained for the year
          ifelse [species] of target-tree = "juniper"
          [set lifetime-juniper lifetime-juniper + wood-need]
          [set lifetime-pinyon lifetime-pinyon + wood-need]
          ask target-tree [ ;have the patch remove the taken biomass and recalculate its RR
              set cwood (cwood - wood-need) ;;patch sets its new biomass as the biomass it began with minus the amount taken by the forager
              calc-new-energy
              set harvested-from? TRUE ;record that the patch has been harvested from
              ;color-patch ;run the color patch code which will only color this patch
          ]
          ask patches in-radius stand-size [update-stand] ;have all cells that include the harvested patch in their stand values update the stand values

          set finished TRUE ;agent records that they have finished harvesting for the year (i.e., met their quota)
          go-home ;agent runs the go-home procedure
        ]
        [;else, continue foraging until they are out of time

          ;first, record the agent will take enough to meet energy need
          let wood-need (energy-still-need / [mj-energy-multiplier] of target-tree);calculate the kg wood yet needed to meet the yearly energy requirement
          set truckload-space-taken (truckload-space-taken + (wood-need + (wood-need * [extra-vol-multiplier] of target-tree)));add this wood to any already in the truck (or to an empty truck)
           ;accounting for the empty space
          set wood-per-patch lput wood-need wood-per-patch
          set wood-taken (wood-taken + wood-need) ;record how much wood has been taken this year
          set energy-obtained (energy-obtained + energy-still-need) ;;record how much energy the agent has obtained for the year
          ifelse [species] of target-tree = "juniper"
            [set lifetime-juniper lifetime-juniper + wood-need]
            [set lifetime-pinyon lifetime-pinyon + wood-need]
          ask target-tree [ ;have the patch remove the taken biomass and recalculate its RR
              set cwood (cwood - wood-need) ;;patch sets its new biomass as the biomass it began with minus the amount taken by the forager
              calc-new-energy
              set harvested-from? TRUE ;record that the patch has been harvested from
              ;color-patch ;run the color patch code which will only color this patch
          ]
          ask patches in-radius stand-size [update-stand] ;have all cells that include the harvested patch in their stand values update the stand values

          ;then begin the process of taking extra (including from current patch)
          set extra-year-travel (((year-travel - distance patch 0 0) * Time_vs_Energy_max) + distance patch 0 0) ;;calcualte how much extra time (distance) the agent will use as the percent of remaining foraging time
          ;; this is total possible travel minus travel completed minus distance to home times how much % of that time agent will use with dist to home added back in since that dist isn't extra time
          ;but time already budgeted for and usable
          continue-foraging
        ]

  ]; end if

  [; start else if the yearly energy still needed is greater than the max energy they can get from the max load can take from the patch (i.e., agent cannot fill their quota)
      ifelse ([cwood] of target-tree + ([cwood] of target-tree * [extra-vol-multiplier] of target-tree)) >= (max-truckload + (max-truckload * proportion_harvest_remain))
      [;if the patch has equal to or more wood than the truck can carry, with the excess wood remaining after harvest factored in, fill the truck
        set truckload-space-taken (truckload-space-taken + max-truckload);fill the truck with wood it can take
        set space-taken max-truckload;;record how much space was taken up (wood + extra space)
        set wood-taken-patch max-truckload ;record how much wood was taken from this patch
        set wood-per-patch lput wood-taken-patch wood-per-patch
        set wood-taken (wood-taken + max-truckload) ;record how much wood has been taken for the year
        set energy-obtained (energy-obtained + [max-load-energy] of target-tree);;record how much energy (kilojoules) agent has obtained
        ifelse [species] of target-tree = "juniper"
          [set lifetime-juniper lifetime-juniper + wood-taken-patch]
          [set lifetime-pinyon lifetime-pinyon + wood-taken-patch]
      ]
      [;if the patch has less wood than a truck can carry,
        set truckload-space-taken (truckload-space-taken + ([cwood] of target-tree + ([cwood] of target-tree * [extra-vol-multiplier] of target-tree) - ([cwood] of target-tree * proportion_harvest_remain)));;take everything there
        ;(i.e., all harvestable wood from the patch) except the amount that won't be fully harvested and put it in the truck (truck not full)
        set space-taken ([cwood] of target-tree + ([cwood] of target-tree * [extra-vol-multiplier] of target-tree) - ([cwood] of target-tree * proportion_harvest_remain));
        set wood-taken-patch ([cwood] of target-tree - ([cwood] of target-tree * proportion_harvest_remain));record how much wood was taken from this patch (all but the proportion not harvested)
        set wood-per-patch lput wood-taken-patch wood-per-patch
        set wood-taken (wood-taken + wood-taken-patch);add to the amount of wood taken for the year
        set energy-obtained (energy-obtained + [max-load-energy] of target-tree * proportion_harvest_remain);;record how much energy (kilojoules) agent has obtained
        ifelse [species] of target-tree = "juniper"
          [set lifetime-juniper lifetime-juniper + wood-taken-patch]
          [set lifetime-pinyon lifetime-pinyon + wood-taken-patch]
      ]

      let cwood-loss wood-taken-patch ;remember how much wood was removed from this patch for patch-use below
      set wood-taken-patch 0 ;set back to 0 so it doesn't stack up over time

    ask target-tree [ ;have the patch remove the taken biomass and recalculate its RR
      set cwood (cwood - cwood-loss) ;;patch sets its new biomass as the biomass it began with minus the amount taken by the forager
      if cwood < 0.0001 [set cwood 0]
      calc-new-energy ;;once the patch has lost biomass, get the kilojoules of any remaining energy present on the patch based on its species
     ; calc-RR ;;calculate a new return-rate
      set harvested-from? TRUE ;record that the patch has been harvested from
      ;color-patch ;run the color patch code which will only color this patch
    ]
    ask patches in-radius stand-size [update-stand] ;have all cells that include the harvested patch in their stand values update the stand values


    ifelse (year-travel - distance patch 0 0) <= 0 ;;if the agent is out of travel time
      [
        set finished TRUE
        go-home
      ]
    [ ;otherwise
    ifelse max-truckload - space-taken <= 0
      [;; if the agent filled the truck, even though didn't fill the yearly-need, go home to empty the truck
        ;; or if the agent is out of moves (i.e., dist home is equal to amount of travel time left) go home
        go-home ;go home and empty truck
        find-best-stand;find new location to forage based on stand values from home-base
      ]
      [find-next-best-location];;else if the truck isn't full, find the next best location based on travelling from the current patch
        ]
  ]; end else


  ]; end while loop

  ];end else
end

to find-next-best-location
  ;this is an agent sub-model for locating the best RR patch from the current location if they have already harvested, haven't filled the truck, and need
  ;;more firewood

  let start-patch patch-here ;have the patch the agent is currently on become the patch for which distance is used for RR
  ask trees-here [set home-base? TRUE] ;;temporarily make the current patch home for purpose of recalculating new RR raster
  ask trees in-radius stand-size [
    calc-temp-RR ;have trees in the available stand area calculate foraging return rate
    set travel-cost-here-home (distance start-patch + dist-from-home-base) ;also get the total maximum travel cost
    ]

  ifelse energy-obtained < yearly-need ;;identify viable trees as those within the current stand and that the agent can get to while still being able to get home
    [;if agent is still working to meet the yearly need
      set t-option-trees trees in-radius stand-size with [[travel-cost-here-home] of self <= [year-travel] of myself]
    ]
    [;else if energy maximizing is happening
      set t-option-trees trees in-radius stand-size with [[travel-cost-here-home] of self <= [extra-year-travel] of myself]
    ]

  ask t-option-trees [;ask trees within the foraging radius
      if home-base? = TRUE
      [set temp-RR 0] ;;if the patch is the one the agent is currently on (i.e., already harvested from), give it a temp-RR of 0
    ]

  ask trees-here [set home-base? FALSE];;have current patch go back to not being a home patch
  let best-tree max-one-of t-option-trees [temp-RR]
  ifelse best-tree = nobody or [temp-RR] of best-tree <= 0
    [;if there are no patches that can be targeted (i.e., no patches with energy are in reach, go home - this could happen due to depletion or distance already traveled)
      go-home;go home to unload anything you have
      find-best-stand ;try to see if there is another stand you can reach with energy
    ]
    ;else, if there is a tree you can reach with energy
    [move-to best-tree;;move to the best tree
      ifelse energy-obtained < yearly-need
      [  ;if the yearly need in energy is not yet met
        set year-travel (year-travel - distance start-patch) ;;record the distance the agent has gone to the new patch
        set dist-travel-year (dist-travel-year + distance start-patch) ;add this move distance (from current patch to new foraging patch) to the distance travelled for the year
      ]
      [ ;if the yearly need is met (i.e., agents are getting more firewood than min necessary
        set extra-year-travel (extra-year-travel - distance start-patch)
        set dist-travel-year (dist-travel-year + distance start-patch)
      ]
  ]

end

to go-home
    ;this is an agent submodel for unloading wood from their truck and recording distance travelled and wood taken once annual needs are met
  let final-tree patch-here ;;remember temporarily the last patch the agent foraged in
  move-to patch 0 0 ;go back to the home patch
  ifelse energy-obtained < yearly-need
  [;if the agent has not yet hit the quota
  set year-travel (year-travel - distance final-tree)
  set dist-travel-year (dist-travel-year + distance final-tree) ;;add the distance travelled from the last patch to the home patch to the total distance travelled
  ]
  [;if the agent is pursuing at least some energy maximizing
   set extra-year-travel (extra-year-travel - distance final-tree)
   set dist-travel-year (dist-travel-year + distance final-tree)
  ]
  set truckload-taken lput truckload-space-taken truckload-taken ;add to the list of how much wood was in the truck when you went home
  set truckload-space-taken 0 ;;empty the truck
  set trips-home-counter (trips-home-counter + 1);;record how many times during a year a forager goes out foraging and returns home
  if year-travel <= 0 [set finished TRUE] ;;if the forager has no more travel time available, end their turn
  if extra-year-travel <= 0 [set finished TRUE] ;;if the forager has no more travel time available, end their turn
  ;set bout-travel travel-dist-per-bout;reset the distance traveled on the foraging bout since agent may begin a new bout
  if finished = TRUE  ;if they have met their quota
    [set lifetime-travel lput dist-travel-year lifetime-travel ;record lifetime travel by adding this year's distance travelled to a lifetime list
     set wood-taken-lifetime (wood-taken-lifetime + wood-taken) ;add the wood taken from this year to the lifetime wood taken record
     let tot-turn-truckloads sum truckload-taken
     set all-agent-truckload-sum lput tot-turn-truckloads all-agent-truckload-sum
     let wood-taken-in-truckloads sum wood-per-patch
     set all-agent-wood-taken lput wood-taken-in-truckloads all-agent-wood-taken
     set all-trips-home lput trips-home-counter all-trips-home
     if extra-energy-obtained > 0 [set total-extra-energy-obtained lput extra-energy-obtained total-extra-energy-obtained]
     if extra-energy-obtained <= 0 [set total-extra-energy-obtained lput 0 total-extra-energy-obtained]
     ]

end

to continue-foraging
  ;this is a foraging agent submodel to permit foraging beyond yearly need when agents have at least some energy maximizing emphasis
  while [finished = FALSE] [

    let target-tree max-one-of trees-here [avail-megajoules]
    ;calculate how much space you have in the truck and how much energy you still need to get
    let max-truckload ((max-truckload-empty - truckload-space-taken) - ((max-truckload-empty - truckload-space-taken) * [extra-vol-multiplier] of target-tree))
    ;the max wood an agent can load in the truck is the max amount their empty truck can haul minus any wood they have in the truck already and minus the extra space that will be taken up by
    ;harvested wood that is not post-processed (the empty space that is not able to be used)
    ask target-tree [ ;have the agent ask the tree they are currently targeting (and intend to harvest from)
    ifelse (cwood + (cwood * extra-vol-multiplier)) >= (max-truckload + (max-truckload * proportion_harvest_remain)) ;if the patch has equal to or more firewood than the agent's truck can haul
        ;plus the extra percent that is leftover as a result of harvest leaving smaller pieces behind and with unused space in the truck factored in
      [set max-load-energy (max-truckload * mj-energy-multiplier)] ;;calculate max-load-energy the agent can get as the kilojoules for the truckload of this wood type
      ;if the patch las less wood than the agent's truck can haul
      [set max-load-energy (avail-megajoules - (avail-megajoules * proportion_harvest_remain))] ;;set the max-load-energy to be the total kilojoules of energy on the patch minus the proportion of the harvest
        ;that will not be kept/used
      ]

      ifelse extra-year-travel > 0 ;if there is still time left being committed to harvesting more firewood
      [
        ifelse ([cwood] of target-tree + ([cwood] of target-tree * [extra-vol-multiplier] of target-tree)) >= (max-truckload + (max-truckload * proportion_harvest_remain))
      [;if the patch has equal to or more wood than the truck can carry, with the excess wood remaining after harvest factored in, fill the truck
        set truckload-space-taken (truckload-space-taken + max-truckload);fill the truck with wood it can take
        set space-taken max-truckload;;record how much space was taken up (wood + extra space)
        set wood-taken-patch max-truckload ;record how much wood was taken from this patch
        set wood-per-patch lput wood-taken-patch wood-per-patch
        set wood-taken (wood-taken + max-truckload) ;record how much wood has been taken for the year
        set extra-energy-obtained (extra-energy-obtained + [max-load-energy] of target-tree);;record how much energy (kilojoules) agent has obtained
        ifelse [species] of target-tree = "juniper"
          [set lifetime-juniper lifetime-juniper + wood-taken-patch]
          [set lifetime-pinyon lifetime-pinyon + wood-taken-patch]
      ]
      [;if the patch has less wood than a truck can carry,
        set truckload-space-taken precision (truckload-space-taken + ([cwood] of target-tree + ([cwood] of target-tree * [extra-vol-multiplier] of target-tree) - ([cwood] of target-tree * proportion_harvest_remain))) 6;;take everything there
        ;(i.e., all harvestable wood from the patch) except the amount that won't be fully harvested and put it in the truck (truck not full)
        set space-taken precision ([cwood] of target-tree + ([cwood] of target-tree * [extra-vol-multiplier] of target-tree) - ([cwood] of target-tree * proportion_harvest_remain)) 6;
        set wood-taken-patch precision ([cwood] of target-tree - ([cwood] of target-tree * proportion_harvest_remain)) 6;record how much wood was taken from this patch (all but the proportion not harvested)
        set wood-per-patch lput wood-taken-patch wood-per-patch
        set wood-taken (wood-taken + wood-taken-patch);add to the amount of wood taken for the year
        set extra-energy-obtained (extra-energy-obtained + [max-load-energy] of target-tree * proportion_harvest_remain);;record how much energy (kilojoules) agent has obtained
        ifelse [species] of target-tree = "juniper"
          [set lifetime-juniper lifetime-juniper + wood-taken-patch]
          [set lifetime-pinyon lifetime-pinyon + wood-taken-patch]
      ]

      let cwood-loss wood-taken-patch ;remember how much wood was removed from this patch for patch-use below
      set wood-taken-patch 0 ;set back to 0 so it doesn't stack up over time

    ask target-tree [ ;have the patch remove the taken biomass and recalculate its RR
      set cwood (cwood - cwood-loss) ;;patch sets its new biomass as the biomass it began with minus the amount taken by the forager
      if cwood < 0.0001 [set cwood 0]
      calc-new-energy ;;once the patch has lost biomass, get the kilojoules of any remaining energy present on the patch based on its species
     ; calc-RR ;;calculate a new return-rate
      set harvested-from? TRUE ;record that the patch has been harvested from
      ;color-patch ;run the color patch code which will only color this patch
    ]
    ask patches in-radius stand-size [update-stand] ;have all cells that include the harvested patch in their stand values update the stand values


    ifelse max-truckload - space-taken <= 0
      [;; if the agent filled the truck, even though didn't fill the yearly-need, go home to empty the truck
        ;; or if the agent is out of moves (i.e., dist home is equal to amount of travel time left) go home
        go-home ;go home and empty truck
        find-best-stand
      ]
      [find-next-best-location];;else if the truck isn't full, find the next best location based on travelling from the current patch

      ];; end if there is still time remaining committed to foraging


      [;else if there is no more time being committed to harvesting firewood
        set finished TRUE
        go-home
      ]

  ];end while loop




end

to calc-new-energy

  if live? [;if a living tree was harvested, it is now dead and has been felled
    set live? false
    set age-since-death 0
    set max-live-cwood cwood
    set color gray
    set dead-trees dead-trees + 1
    set standing? false
  ]
  ;all harvested trees are cut down, so standing? should be set to false and their energy is now the fallen dead energy, meaning no loss occurs as it does for live or fallen dead
  if standing? [set standing? false]
  if species = "pine" [set avail-megajoules ((cwood * mj-energy-multiplier) * 1)] ;; calculate the energy available, slight penalty for having to take down a standing dead tree
  if species = "juniper" [set avail-megajoules ((cwood * mj-energy-multiplier) * 1)];; calculate the energy available, slight penalty for having to take down a standing dead tree

end

to set-params
  ;; wc values from Guess
  set log-wc-mean -1.939
  set log-wc-sd 0.562

  ;; HEIGHT ;;
  ;; Parameters derived from NLME equations
  set hgt-asym-mean [ 6.554 3.604 ]
  set hgt-asym-sd [ 1.574 0.875 ]
  set hgt-asym-wc [ 0.151 -0.732 ]
  set hgt-lrc-mean [ -2.313 -1.564 ]
  set hgt-lrc-sd [ 0.720 0.577 ]
  set hgt-lrc-wc [ 0.191 0.504 ]
  ;; Parameter correlations (asym vs. lrc)
  set hgt-corr [ -0.593 -0.876 ]

  ;; DIAMETER ;;
  ;; Parameters derived from NLME equations
  set diam-asym-mean [ 0.145 0.031 ]
  set diam-asym-sd [ 0.078 0.01 ]
  set diam-asym-wc [ 0.029 0.006 ]
  set diam-lrc-mean [ -3.57 -1.018 ]
  set diam-lrc-sd [ 0.976 0.897 ]
  set diam-lrc-wc [ -0.215 -0.593 ]
  ;; Parameter correlations (asym vs. lrc)
  set diam-corr [ -0.886 -0.866 ]

  ;; CROWN AREA ;;
  ;; Parameters derived from NLME equations
  set carea-asym-mean [ 7.364 0.658 ]
  set carea-asym-sd [ 3.338 0.446 ]
  set carea-asym-wc [ 0.143 -0.071 ]
  set carea-lrc-mean [ -3.841 -2.035 ]
  set carea-lrc-sd [ 1.126 0.774 ]
  set carea-lrc-wc [ -0.088 0.041 ]
  ;; Parameter correlations (asym vs. lrc)
  set carea-corr [ -0.832 -0.805 ]

  ;; log cwood to log diameter
  set cwood-mean [ 1.123 1.381 ]
  set cwood-sd [ 0.395 0.352 ]
  set cwood-hgt-coef-a [ 0.971 0.773 ]
  set cwood-hgt-coef-b [ 2.563 4.347 ]
  set cwood-carea-coef-a [ -6.099 0 ] ;; Change first val to zero??
  set cwood-carea-coef-b [ 10.144 9.989 ]

  ;; Mortality
  set max-age [ 200 300 ]
  set mort-asym [ 1.0 1.0 ]
  set mort-lrc[ -3.669 -3.768 ]

end

to get-hgt-params
  ;; Uses bivariate random normal equation from
  ;; https://www.probabilitycourse.com/chapter5/5_3_2_bivariate_normal_dist.php

  set hgt-asym -9999
  set hgt-lrc 9999

  while [ (hgt-asym < 0) or (hgt-lrc > 0) ] [ ;; check for reasonable parameter values
    ;; 1. Generate z1 and z2
    let z1 random-normal 0 1
    let z2 random-normal 0 1

    ;; 2. Convert z2 to correlated version
    let tmp-corr item species-number hgt-corr
    set z2 tmp-corr * z1 + sqrt ( 1 - tmp-corr ^ 2 ) * z2

    ;; 3. Back transform to asym and lrc
    set hgt-asym z1 * item species-number hgt-asym-sd + item species-number hgt-asym-mean
    set hgt-asym hgt-asym + item species-number hgt-asym-wc * [wc] of patch-here
    set hgt-lrc z1 * item species-number hgt-lrc-sd + item species-number hgt-lrc-mean
    set hgt-lrc hgt-lrc + item species-number hgt-lrc-wc * [wc] of patch-here

  ]
end

to get-diam-params
  ;; Uses bivariate random normal equation from
  ;; https://www.probabilitycourse.com/chapter5/5_3_2_bivariate_normal_dist.php

  set diam-asym -9999
  set diam-lrc 9999

  while [ (diam-asym < 0) or (diam-lrc > 0) ] [ ;; check for reasonable parameter values
    ;; 1. Generate z1 and z2
    let z1 random-normal 0 1
    let z2 random-normal 0 1

    ;; 2. Convert z2 to correlated version
    let tmp-corr item species-number diam-corr
    set z2 tmp-corr * z1 + sqrt ( 1 - tmp-corr ^ 2 ) * z2

    ;; 3. Back transform to asym and lrc
    set diam-asym z1 * item species-number diam-asym-sd + item species-number diam-asym-mean
    set diam-asym diam-asym + item species-number diam-asym-wc * [wc] of patch-here
    set diam-lrc z1 * item species-number diam-lrc-sd + item species-number diam-lrc-mean
    set diam-lrc diam-lrc + item species-number diam-lrc-wc * [wc] of patch-here

  ]
end

to get-carea-params
  ;; Uses bivariate random normal equation from
  ;; https://www.probabilitycourse.com/chapter5/5_3_2_bivariate_normal_dist.php

  set carea-asym -9999
  set carea-lrc 9999

  while [ (carea-asym < 0) or (carea-lrc > 0) ] [ ;; check for reasonable parameter values
    ;; 1. Generate z1 and z2
    let z1 random-normal 0 1
    let z2 random-normal 0 1

    ;; 2. Convert z2 to correlated version
    let tmp-corr item species-number carea-corr
    set z2 tmp-corr * z1 + sqrt ( 1 - tmp-corr ^ 2 ) * z2

    ;; 3. Back transform to asym and lrc
    set carea-asym z1 * item species-number carea-asym-sd + item species-number carea-asym-mean
    set carea-asym carea-asym + item species-number carea-asym-wc * [wc] of patch-here
    set carea-lrc z1 * item species-number carea-lrc-sd + item species-number carea-lrc-mean
    set carea-lrc carea-lrc + item species-number carea-lrc-wc * [wc] of patch-here

  ]
end

to get-cwood-params
  ;; Set coefficient to relate diameter to c-wood
  set cwood-coef -9999
  while [ cwood-coef < 0 ] [
    set cwood-coef random-normal item species-number cwood-mean item species-number cwood-sd
  ]
end

to calc-diam
  ;; Equation from https://stat.ethz.ch/R-manual/R-devel/library/stats/html/SSasympOrig.html
  ;; Asym*(1 - exp(-exp(lrc)*input))
  set diam diam-asym * ( 1 - exp(- exp( diam-lrc ) * age ))
end

to calc-hgt
  ;; Equation from https://stat.ethz.ch/R-manual/R-devel/library/stats/html/SSasympOrig.html
  ;; Asym*(1 - exp(-exp(lrc)*input))
  set hgt hgt-asym * ( 1 - exp(- exp( hgt-lrc ) * age ))
end

to calc-carea
  ;; Equation from https://stat.ethz.ch/R-manual/R-devel/library/stats/html/SSasympOrig.html
  ;; Asym*(1 - exp(-exp(lrc)*input))
  set carea carea-asym * ( 1 - exp(- exp( carea-lrc ) * age ))
end

to calc-cwood ;; place holder
  ;; Could merge this into single statement
  let ldiam ln diam
  let lcwood ldiam * cwood-coef
  set cwood exp lcwood
  set avail-megajoules ((cwood * mj-energy-multiplier) * Live_wood_energy) ;; calculate the energy available in the tree - but lower that energy total greatly since tree is alive
end

to calc-cwood-carea
  ;; Could merge this into single statement
  let a item species-number cwood-carea-coef-a
  let b item species-number cwood-carea-coef-b
  set cwood a + b * carea
end

to calc-cwood-hgt
  ;; Could merge this into single statement
  let a item species-number cwood-hgt-coef-a
  let b item species-number cwood-hgt-coef-b
  set cwood a * carea ^ b
end

;to calc-cwood
;  OLD VERSION BASED ON GUESS OUTPUT/NLME
;  ;; Could merge this into single statement
;  let ldiam ln diam
;  let lcwood ldiam * cwood-coef
;  set cwood exp lcwood
;end

to recruitment

  set age 0
  ;set age random 20
  set live? true
  set standing? true
  set burning? false
  set cwood 0
  set pfall 0.25
  set dist-from-home-base distance patch 0 0 ;;know how far the patch is from home base - this is used by agents
  set travel-cost-here-home 0;; no agents have moved so this value should be zero

  ifelse species-number = 0
  [
    set species "pine"
    set shape "tree pine"
    set color 57
    set reproductive-age 20
    if ticks = 0 [ set age random ( reproductive-age - 1 ) ] ;; Only for initialization
    set decay-rate-standing 0.01
    set decay-rate-fallen 0.1
    set pfall 0.25
    set mj-energy-multiplier 21
    set avail-megajoules ((cwood * mj-energy-multiplier) * Live_wood_energy) ;; adjust this once we have translated cwood into a volume estimate so we can do kg * 21 (~21 megajoules / kg)
    set extra-vol-multiplier Excess_volume_taken_pinyon ;set the multiplier for how much extra space in the truck unprocessed pinyon takes up
  ]
  [
    set species "juniper"
    set shape "tree"
    set color 53
    set reproductive-age 30
    if ticks = 0 [ set age random ( reproductive-age - 1 ) ] ;; Only for initialization
    set decay-rate-standing 0.01
    set decay-rate-fallen 0.1
    set pfall 0.25
    set mj-energy-multiplier 16
    set avail-megajoules ((cwood * mj-energy-multiplier) * Live_wood_energy) ;; adjust this once we have translated cwood into a volume estimate so we can do kg * 16 (~16 megajoules / kg)
    set extra-vol-multiplier Excess_volume_taken_juniper ;set the multiplier for how much extra space in the truck unprocessed pinyon takes up
  ]

  ;; Assign allometric coefficients
  get-diam-params
  get-hgt-params
  get-carea-params
  get-cwood-params

end

to update-stand
  ;ask patches [
  ;  set stand-cwood-live sum [cwood] of trees in-radius 10 with [ live? ]
  ;  set stand-cwood-dead-standing sum [cwood] of trees in-radius 10 with [ not live? and standing? ]
  ;  set stand-cwood-dead-fallen sum [cwood] of trees in-radius 10 with [ not live? and not standing? ]
  ;  set pcolor scale-color green stand-cwood-dead-standing 0 1000
  ;]
  ;here we use available megajoules of energy rather than cwood for stand values b/c avail-megajoules takes into consideration the type of wood (pinyon vs juniper)
  ;whereas cwood does not

    set stand-mj-live sum [avail-megajoules] of trees in-radius stand-size with [live?]
    set stand-mj-dead-standing sum [avail-megajoules] of trees in-radius stand-size with [not live? and standing?]
    set stand-mj-dead-fallen sum [avail-megajoules] of trees in-radius stand-size with [ not live? and not standing? ]
   ; set pcolor scale-color green stand-cwood-dead-standing 0 1000
    let stand-megajoules (stand-mj-live + stand-mj-dead-standing + stand-mj-dead-fallen)
    ifelse distance patch 0 0 = 0 ;;cost distance. Megajoules divided by distance from home-base. This is the return rate the agent can get
      [set stand-RR (stand-megajoules)] ;; a tree on patch 0 0 has no distance factored in
      [set stand-RR (stand-megajoules / distance patch 0 0)]


end

to-report mean-suitability-pine
  ifelse ticks = 0 [
    report 0
  ] [
    report mean [item 0 suitability] of patches
  ]
end

to-report mean-suitability-juniper
  ifelse ticks = 0 [
    report 0
  ] [
    report mean [item 1 suitability] of patches
  ]
end

to report-forager-vars
  ;this is an agent sub-model for forager agents to record annual harvest and travel data
  file-open "forager_dat.csv" ;open the csv file
  file-print csv:to-string [(list who ticks yearly-need energy-obtained wood-taken dist-travel-year trips-home-counter no-place extra-energy-obtained lifetime-pinyon lifetime-juniper Max-travel Time_vs_Energy_max Live_wood_energy Standing_dead_energy Excess_volume_taken_pinyon Excess_volume_taken_juniper Max_truck_capacity proportion_harvest_remain) ] of foragers ;put the data into the csv as one row with multiple columns per forager agent
  file-close ;close the csv file

end

to profile
  setup                                          ;; set up the model
  profiler:start                                 ;; start profiling
  repeat 500 [ go ]                               ;; run something you want to measure
  profiler:stop                                  ;; stop profiling
  csv:to-file "profiler_data.csv" profiler:data  ;; save the results
  profiler:reset                                 ;; clear the data
end
@#$#@#$#@
GRAPHICS-WINDOW
5
10
442
448
-1
-1
13.0
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
32
0
32
1
1
1
ticks
30.0

BUTTON
10
460
76
493
NIL
setup\n
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

PLOT
940
460
1140
580
Soil WC
NIL
NIL
0.0
1.0
0.0
10.0
true
false
"set-plot-x-range 0 1\nset-plot-y-range 0 count patches\nset-histogram-num-bars 10" ""
PENS
"default" 1.0 1 -16777216 true "" "histogram [wc] of patches"

BUTTON
10
500
73
533
NIL
go
T
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

PLOT
450
10
650
160
Tree diameter (0)
NIL
NIL
0.0
10.0
0.0
0.25
true
false
"" "ask trees with [ species-number = 0 ][\n  create-temporary-plot-pen (word who)\n  set-plot-pen-color color\n  plotxy ticks diam\n]"
PENS
"default" 1.0 0 -16777216 true "" ""

PLOT
450
160
650
310
Tree diameter (1)
NIL
NIL
0.0
10.0
0.0
0.1
true
false
"" "ask trees with [ species-number = 1 ][\n  create-temporary-plot-pen (word who)\n  set-plot-pen-color color\n  plotxy ticks diam\n]"
PENS
"default" 1.0 0 -16777216 true "" ""

PLOT
1050
310
1250
460
CWood coefs
NIL
NIL
0.0
2.0
0.0
10.0
true
false
"set-histogram-num-bars 10" ""
PENS
"default" 1.0 1 -16777216 true "" "histogram [cwood-coef] of trees with [species-number = 0]"
"pen-1" 0.2 1 -2674135 true "" "histogram [cwood-coef] of trees with [species-number = 1]"

PLOT
1050
10
1250
160
CWood (0)
NIL
NIL
0.0
10.0
0.0
0.5
true
false
"" "ask trees with [ species-number = 0 ][\n  create-temporary-plot-pen (word who)\n  set-plot-pen-color color\n  plotxy ticks cwood\n]"
PENS
"default" 1.0 0 -16777216 true "" ""

PLOT
1050
160
1250
310
CWood (1)
NIL
NIL
0.0
10.0
0.0
0.1
true
false
"" "ask trees with [ species-number = 1 ][\n  create-temporary-plot-pen (word who)\n  set-plot-pen-color color\n  plotxy ticks cwood\n]"
PENS
"default" 1.0 0 -16777216 true "" ""

PLOT
450
310
650
460
Dead trees / tick
NIL
NIL
0.0
10.0
0.0
10.0
true
false
"" ""
PENS
"dead" 1.0 0 -16777216 true "" "plot dead-trees"

PLOT
650
310
850
460
New trees / tick
NIL
NIL
0.0
10.0
0.0
10.0
true
false
"" ""
PENS
"default" 1.0 0 -16777216 true "" "plot new-trees"

PLOT
850
310
1050
460
Removed trees / tick
NIL
NIL
0.0
10.0
0.0
10.0
true
false
"" ""
PENS
"default" 1.0 0 -16777216 true "" "plot removed-trees"

PLOT
450
460
945
580
Age distribution
NIL
NIL
0.0
200.0
0.0
10.0
true
false
"" ""
PENS
"pine" 1.0 1 -16777216 true "" "histogram [age] of trees with [species-number = 0]"
"juniper" 1.0 1 -2674135 true "" "histogram [age] of trees with [species-number = 1]"

BUTTON
85
460
182
493
NIL
show-burn
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

SWITCH
85
500
188
533
fire?
fire?
0
1
-1000

PLOT
205
460
445
580
plot 1
NIL
NIL
0.0
10.0
0.0
0.5
true
false
"" ""
PENS
"pine" 1.0 0 -6565750 true "" "plot mean-suitability-pine"
"juniper" 1.0 0 -13210332 true "" "plot mean-suitability-juniper"

SLIDER
15
545
187
578
n-time-steps
n-time-steps
10
1000
150.0
10
1
NIL
HORIZONTAL

SLIDER
15
625
300
658
Excess_volume_taken_pinyon
Excess_volume_taken_pinyon
0
0.5
0.1
0.01
1
percent truck bed
HORIZONTAL

SLIDER
15
665
300
698
Excess_volume_taken_juniper
Excess_volume_taken_juniper
0
0.5
0.3
0.01
1
percent truck bed
HORIZONTAL

SLIDER
15
585
187
618
num_foragers
num_foragers
0
30
5.0
1
1
NIL
HORIZONTAL

SLIDER
15
705
247
738
Max_truck_capacity
Max_truck_capacity
0.20
1.5
0.5
.010
1
cords (wood)
HORIZONTAL

SLIDER
645
655
842
688
proportion_harvest_remain
proportion_harvest_remain
0
0.3
0.1
0.01
1
NIL
HORIZONTAL

SLIDER
305
625
567
658
Live_wood_energy
Live_wood_energy
0
1
0.0
0.0001
1
% of ideal max
HORIZONTAL

PLOT
1450
10
1760
160
Dist. Travelled per Year
Tick
Dist. travelled
0.0
10.0
0.0
100.0
true
true
"" ""
PENS
"Mean" 1.0 0 -16777216 true "" "if ticks > 49 [plot mean [dist-travel-year] of foragers]"
"Max" 1.0 0 -10141563 true "" "if ticks > 49 [plot max [dist-travel-year] of foragers]"
"Min" 1.0 0 -8990512 true "" "if ticks > 49 [plot min [dist-travel-year] of foragers]"
"Max-travel" 1.0 0 -7500403 true "" "if ticks > 49 [plot Max-travel]"

PLOT
1250
310
1450
460
Foraging Trips per Turn (Histogram)
No. Trips
Frequency
1.0
10.0
0.0
10.0
true
false
"" ""
PENS
"default" 1.0 1 -16777216 true "" "if ticks > 49 [histogram all-trips-home]"

PLOT
650
10
850
160
Tree height (0)
NIL
NIL
0.0
10.0
0.0
1.0
true
false
"" "ask trees with [ species-number = 0 ][\n  create-temporary-plot-pen (word who)\n  set-plot-pen-color color\n  plotxy ticks hgt\n]"
PENS
"default" 1.0 0 -16777216 true "" ""

PLOT
650
160
850
310
Tree height (1)
NIL
NIL
0.0
10.0
0.0
1.0
true
false
"" "ask trees with [ species-number = 1 ][\n  create-temporary-plot-pen (word who)\n  set-plot-pen-color color\n  plotxy ticks hgt\n]"
PENS
"default" 1.0 0 -16777216 true "" ""

PLOT
850
10
1050
160
Tree crown area (0)
NIL
NIL
0.0
10.0
0.0
1.0
true
false
"" "ask trees with [ species-number = 0 ][\n  create-temporary-plot-pen (word who)\n  set-plot-pen-color color\n  plotxy ticks carea\n]"
PENS
"default" 1.0 0 -16777216 true "" ""

PLOT
850
160
1050
310
Tree crown area (1)
NIL
NIL
0.0
10.0
0.0
1.0
true
false
"" "ask trees with [ species-number = 1 ][\n  create-temporary-plot-pen (word who)\n  set-plot-pen-color color\n  plotxy ticks carea\n]"
PENS
"default" 1.0 0 -16777216 true "" ""

PLOT
1250
10
1450
160
Proportion all Foragers not meeting energy need
Tick
Proportion
0.0
10.0
0.0
1.0
true
false
"" ""
PENS
"default" 1.0 0 -16777216 true "" "if ticks > 49 [plot count foragers with [energy-obtained < yearly-need] / num_foragers]"

SLIDER
305
665
587
698
Standing_dead_energy
Standing_dead_energy
0
1
0.825
0.025
1
% of ideal max
HORIZONTAL

SLIDER
15
745
187
778
Max-travel
Max-travel
25
1000
400.0
25
1
NIL
HORIZONTAL

SLIDER
255
705
427
738
Time_vs_Energy_max
Time_vs_Energy_max
0
1
0.0
0.1
1
NIL
HORIZONTAL

PLOT
1250
160
1450
310
Extra energy (mj) obtained
NIL
NIL
0.0
10.0
0.0
10.0
true
true
"" ""
PENS
"Mean" 1.0 0 -16777216 true "" "if ticks > 49 [plot mean [extra-energy-obtained] of foragers]"
"Max" 1.0 0 -10141563 true "" "if ticks > 49 [plot max [extra-energy-obtained] of foragers]"
"Min" 1.0 0 -8990512 true "" "if ticks > 49 [plot min [extra-energy-obtained] of foragers]"

PLOT
1450
160
1650
310
Wood Harvested by Species
NIL
NIL
0.0
10.0
0.0
10.0
true
true
"" ""
PENS
"Pinyon" 1.0 0 -8330359 true "" "if ticks > 49 [plot sum [lifetime-pinyon] of foragers]"
"Juniper" 1.0 0 -14333415 true "" "if ticks > 49 [plot sum [lifetime-juniper] of foragers]"

SLIDER
205
745
392
778
stand-size
stand-size
1
5
3.0
1
1
grid-cell radius
HORIZONTAL

SLIDER
195
585
367
618
base_need
base_need
500
1500
750.0
250
1
NIL
HORIZONTAL

SLIDER
375
585
547
618
need_variance
need_variance
0
1000
500.0
100
1
NIL
HORIZONTAL

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

logs
false
0
Polygon -7500403 true true 15 241 75 271 89 245 135 271 150 246 195 271 285 121 235 96 255 61 195 31 181 55 135 31 45 181 49 183
Circle -1 true false 132 222 66
Circle -16777216 false false 132 222 66
Circle -1 true false 72 222 66
Circle -1 true false 102 162 66
Circle -7500403 true true 222 72 66
Circle -7500403 true true 192 12 66
Circle -7500403 true true 132 12 66
Circle -16777216 false false 102 162 66
Circle -16777216 false false 72 222 66
Circle -1 true false 12 222 66
Circle -16777216 false false 30 240 30
Circle -1 true false 42 162 66
Circle -16777216 false false 42 162 66
Line -16777216 false 195 30 105 180
Line -16777216 false 255 60 165 210
Circle -16777216 false false 12 222 66
Circle -16777216 false false 90 240 30
Circle -16777216 false false 150 240 30
Circle -16777216 false false 120 180 30
Circle -16777216 false false 60 180 30
Line -16777216 false 195 270 285 120
Line -16777216 false 15 240 45 180
Line -16777216 false 45 180 135 30

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

tree pine
false
0
Rectangle -6459832 true false 120 225 180 300
Polygon -7500403 true true 150 240 240 270 150 135 60 270
Polygon -7500403 true true 150 75 75 210 150 195 225 210
Polygon -7500403 true true 150 7 90 157 150 142 210 157 150 7

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
NetLogo 6.2.2
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
1
@#$#@#$#@

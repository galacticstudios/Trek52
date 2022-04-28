
! TREK52 - A version of Star Trek for the VT52 terminal
! Written by Bob Alexander - bob@GalacticStudios.org
! Implemented in DEC BASIC-PLUS.
! Inspired by the original Star Trek game written by
! Mike Mayfield.
! Written in 2022. Released to the public domain. No rights reserved.

!   Set up the environment
110 extend
120 randomize

!   Use coords [0..7]. Enterprise pos is 0..63
!   The galaxy is an 8x8 grid of (unfortunately named) quadrants.
!   A quadrant consists of 8x8 sectors
130 dim quadrant%(7%, 7%), galaxy%(7%, 7%)
!   localKlingons is a list of Klingons' row, column, and energy level
!   for Klingons in the current quadrant
140 dim localKlingons%(3%, 3%)

!   Define the damage array
145 dim damage%(7%), damageNames$(7%)
150 dWarp% = 1% : dShields% = 2% : dPhasers% = 3% : &
    dTorpedoes% = 4% : dSRScan% = 5% : dLRScan% = 6% : dComputer% = 7%
160 DATA "Warp Engines", "Shields", "Phasers", "Torpedoes", "SR Scanner", &
         "LR Scanner", "Computer"
170 read damageNames$(i%) for i% = 1% to 7%

180 messageCount% = 0%

!   Screen coordinates for where we draw the "Command" prompt and the
!   damage report
185 commandX% = 25% : commandY% = 14% : damageX% = 0% : damageY% = 14%

!   Initialize the VT52 escape codes
190 q = fnInitializeEscapeCodes

!   Use RSTS' input trick for getting a single character from the
!   user, without requiring him to hit Return
200 open "KB:" for input as file #1%
210 field #1%, 1 as getChar$

!   Define an array for holding warp and torpedo tracks through a quadrant
220 dim track(9, 2)

!   Welcome the user and print instructions
260 q = fnWelcome()

!   Main game loop
510 for mainLoop% = 1 until exitGame%
520    q = fnInitializeGame()
530    q = fnRefreshScreen()
550    q = fnPlayGame()
560    if fnOfferReplay%() = 0 then exitGame% = 1%
570 next mainLoop%

999 print clearScreen$

!    Initialize game
1010 def fnInitializeGame
1020 klingons% = 0
1030 bases% = 0

!    Put stars, bases, and Klingons in the galaxy
!    Go through each quadrant
1040 for row% = 0% to 7%
1050    for col% = 0% to 7%
1060       r = rnd(1)
1070       k% = 0% : b% = 0% : s% = 0%

!          Different probablilities for 1, 2, or 3 Klingons in the quadrant
1080       if r > 0.8 then k% = 1% : if r > 0.95 then k% = 2% : &
           if r > 0.98 then k% = 3%

!          Probablility of a starbase
1090       if rnd(1) > 0.96 then b% = 1

!          1 to 8 stars in the quadrant
1100       s% = rnd(1) * 8 + 1

!          Set the quadrant's contents. Make them negative since
!          the quadrant hasn't been scanned yet.
1110       galaxy%(row%, col%) = -(k% * 100% + b% * 10% + s%)
1120       klingons% = klingons% + k%
1130       bases% = bases% + b%
1140    next col%
1150 next row%

!    If no starbase has been placed, create one
1160 if bases% = 0 then &
        bases% = 1% : &
        row% = rnd * 8 : &
        col% = rnd * 8 : &
        galaxy%(row%, col%) = galaxy%(row%, col%) - 10%

!    Place the Enterprise. Make its quadrant positive, i.e. scanned
1170 enterpriseX% = rnd(1) * 64 : enterpriseY% = rnd(1) * 64
1172 x% = enterpriseX% / 8% : y% = enterpriseY% / 8%
1175 galaxy%(y%, x%) = -galaxy%(y%, x%) + 1000%

1180 stardates% = 30%
1190 shields% = 0
1200 shields% = 0% : energy% = 3000% : torpedoes% = 10% : mat damage% = zer
1210 q = fnInitializeQuadrant()
1220 exitGame% = 0%
1290 fnend

!    Recharge Enterprise
1310 def fnRechargeEnterprise()
1320 shields% = 0% : energy% = 3000% : torpedoes% = 10%
1330 q = fnAdjustDamage(di%, -1000%) for di% = 1% to 7%
1340 fnend

!    Return the string for whetever is occupying a sector
1370 def fnSectorText$(s%)
1380 if damage%(dSRScan%) then fnSectorText$ = " " &
     else if s% = 0% then fnSectorText$ = "." &
     else if s% = 1% then fnSectorText$ = "*" &
     else if s% = 10% then fnSectorText$ = "B" &
     else if s% = 100% then fnSectorText$ = "K" &
     else if s% = 1000% then fnSectorText$ = "E"
1390 fnend

!    Refresh the entire screen
1500 def fnRefreshScreen
1510 print clearScreen$;

!    Print horizontal lines over the quadrant and galaxy maps
1520 print string$(24%, 45%); string$(7%, 32%); string$(49%, 61%)

!    Set the screen coordinates of the maps
1530 quadrantChartX% = 0% : quadrantChartY% = 0%
1540 galaxyChartX% = quadrantChartX% + 31% : galaxyChartY% = quadrantChartY%

!    Print the maps
1550 q = fnPrintQuadrant() : q = fnPrintGalaxy()

!    Print the bottom lines
1560 print fnCursor$(0%, 9%); string$(24%, 45%);
1570 print fnCursor$(31%, 9%); string$(49%, 61%)
1580 print fnStatusReport$();
1590 print fnHintLine$;
1600 print fnDirectionGuide$(25%, 3%);
1610 print fnDamageReport$(damageX%, damageY%);
1620 fnend

!    Print the galaxy map
1700 def fnPrintGalaxy()
1710 for row% = 0% to 7%
1720    print fnCursor$(31%, row% + 1%); "|";
1730    print fnGalaxyQuadrant$(row%, col%); "|"; &
           for col% = 0% to 7%
1740 next row%
1750 fnend

!    Print the quadrant map
1800 def fnPrintQuadrant()
1810 for row% = 0% to 7%
1820    ! Print one row of the quadrant scan
1830    print fnCursor$(0%, row% + 1%);
1840    for col% = 0% to 7%
1850      print " "; fnSectorText$(quadrant%(row%, col%)); " ";
1860    next col%
1870 next row%
1880 fnend

!    Format a quadrant for printing in the galaxy chart
!    (i.e. three stars, three digits, or blanks if the computer is damaged)
1920 def fnGalaxyQuadrant$(row%, col%)
1925 erow% = enterpriseY% / 8% : ecol% = enterpriseX% / 8%
1930 if damage%(dComputer%) and &
        (row% <> erow% or col% <> ecol%) then &
        fnGalaxyQuadrant$ = "     " : goto 1980
1935 q% = galaxy%(row%, col%)
1940 if q% < 0 then fnGalaxyQuadrant$ = " *** " : goto 1980
1950 qq$ = mid(num$(q% + 1000%), 3%, 3%)
1960 if q% >= 1000% then qq$ = "(" + qq$ + ")" else qq$ = " " + qq$ + " "
1970 fnGalaxyQuadrant$ = qq$
1980 fnend

     ! Place objects in the quadrant
2010 def fnInitializeQuadrant()
2020 quadrant%(qy%, qx%) = 0% for qy% = 0% to 7% for qx% = 0% to 7%
2030 quadrantX% = enterpriseX% / 8% : quadrantY% = enterpriseY% / 8%
!    Place the Enterprise
2032 x% = fnmod%(enterpriseX%, 8%) : y% = fnmod%(enterpriseY%, 8%)
2033 quadrant%(y%, x%) = 1000%
2035 contents% = abs(galaxy%(quadrantY%, quadrantX%))
!    Place stars
2050 for s% = 1% to fnmod%(contents%, 10%)
2060    x% = rnd * 8% : y% = rnd * 8%
2065    ! If the space is unoccupied, put a star there. Else repeat loop
2070    if quadrant%(y%, x%) = 0% then &
           quadrant%(y%, x%) = 1% &
        else &
           s% = s% - 1%
2080 next s%

!    Place bases
2100 for s% = 1% to fnmod%(contents% / 10%, 10%)
2110    x% = rnd * 8% : y% = rnd * 8%
2115    ! If the space is unoccupied, put a base there. Else repeat loop
2120    if quadrant%(y%, x%) = 0% then &
           quadrant%(y%, x%) = 10% &
        else &
           s% = s% - 1%
2130 next s%

!    Place Klingons
2145 mat localKlingons% = zer
2150 for s% = 1 to fnmod%(contents% / 100%, 10%)
2160    x% = rnd * 8% : y% = rnd * 8%

!       If the space is occupied, repeat loop; else put a K there
2170    if quadrant%(y%, x%) <> 0% then &
           s% = s% - 1% &
        else &
           quadrant%(y%, x%) = 100% : &
           localKlingons%(s%, 0%) = y% : localKlingons%(s%, 1%) = x% : &
           localKlingons%(s%, 2%) = 200%
2180 next s%
2190 fnend

!    Modulo function
2210 def fnmod%(n%, d%) = n% - n% / d% * d%

!    Return a string that will position cursor at x, y
2310 def fnCursor$(x%, y%) = esc$ + "Y" + chr$(y% + 32%) + chr$(x% + 32%)

!    Return  a right justified NUM$
2410 def fnRNum$(n%, width%)
2420 rn$ = num$(n%) : rn$ = mid(rn$, 2%, len(rn$) - 2%)
2430 width% = width% - len(rn$)
2440 if width% > 0 then rn$ = space$(width%) + rn$ 
2450 fnRNum$ = rn$
2460 fnend

!    Change what's in a sector, and update the screen
2510 def fnChangeSector(row%, col%, newVal%)
2520 qcsrow% = fnMod%(row%, 8%) : qcscol% = fnMod%(col%, 8%)
2530 quadrant%(qcsrow%, qcscol%) = newVal%
2540 print fnUpdateSector$(qcsrow%, qcscol%)
2550 fnend

!    Return string to update a sector
2610 def fnUpdateSector$(row%, col%) = &
        fnCursor$(col% * 3% + 1%, row% + 1%) + &
        fnSectorText$(quadrant%(row%, col%))

!    Distance from Klingon to Enterprise
2710 def fnDist(k%) = sqr( &
        (fnMod%(enterpriseY%, 8%) - localKlingons%(k%, 0%)) ^ 2 + &
        (fnMod%(enterpriseX%, 8%) - localKlingons%(k%, 1%)) ^ 2)

!    Return the formatted status report
3005 def fnStatusReport$
3010 sr$ = ""
3020 sr$ = sr$ + fnCursor$(0%, 10%) + "ENERGY:" + fnUpdateEnergy$()
3030 sr$ = sr$ + fnCursor$(31%, 10%) + "CONDITION:" + fnUpdateCondition$()
3060 sr$ = sr$ + fnCursor$(0%, 11%) + "SHIELDS:" + fnUpdateShields$()
3090 sr$ = sr$ + fnCursor$(31%, 11%) + "STARDATES REMAINING: " + &
        fnRNum$(stardates%, 2%)
3100 sr$ = sr$ + fnCursor$(0%, 12%) + "TORPEDOES:   " + &
        fnRNum$(torpedoes%, 11%)
3110 sr$ = sr$ + fnCursor$(31%, 12%) + "KLINGONS REMAINING:  " + &
        fnRNum$(klingons%, 2%)
3290 fnStatusReport$ = sr$
3295 sr$ = ""
3300 fnend

!    Return a string for the Hint line
3410 def fnHintLine$() = fnCursor$(0%, 23%) + &
        "   (W)arp  (P)hasers  (T)orpedoes  (S)hields  " + &
        "(L).R. Scan  (A)bandon Ship"

!    Return a string that displays the direction guide
3510 def fnDirectionGuide$(x%, y%) = &
        fnCursor$(x%, y%) +      "4 3 2" + &
        fnCursor$(x%, y% + 1%) + " \|/" + &
        fnCursor$(x%, y% + 2%) + "5---1" + &
        fnCursor$(x%, y% + 3%) + " /|\" + &
        fnCursor$(x%, y% + 4%) + "6 7 8"

!    Return a string with the damage report
3610 def fnDamageReport$(x%, y%)
3620 qd$ = fnCursor$(x%, y%) + " == DAMAGE REPORT =="
3630 for qi% = 1% to 7%
3640    qd$ = qd$ + fnCursor$(x%, y% + qi%) + damageNames$(qi%) + &
           ": " + fnUpdateDamage$(qi%)
3650 next qi%
3660 fnDamageReport$ = qd$
3670 fnend

!    Display a message after the "Command" input line
3710 def fnDisplayMessage(msg$)

!    If there's no room for more messages, have the user hit a
!    key to clear the current messages and make room for more
3720 if (commandY% + messageCount% + 1% >= 23%) then &
        print fnCursor$(commandX%, messageCount% + commandY% + 1%); &
              "-- More -- (hit any key)"; : &
        a$ = fnGetChar$() : &
        q = fnClearMessages()
!    Display the new message
3730 print fnCursor$(commandX%, messageCount% + commandY% + 1%); msg$; : &
     messageCount% = messageCount% + 1%
3740 fnend 

!    Clear all the messages
3800 def fnClearMessages()
3810 print fnCursor$(commandX%, qmi% + commandY% + 1%); clearLine$; &
        for qmi% = 0% to messageCount%
3820 messageCount% = 0%
3830 fnend

!    Add or subtract damage
4060 def fnAdjustDamage(di%, amt%)
4065 oldDamage% = damage%(di%)
4070 damage%(di%) = fnMax%(damage%(di%) + amt%, 0%)
4080 print fnUpdateDamage$(di%)

!    If a system went from damaged to fixed, or from fixed to damaged,
!    we might need to refresh a part of the screen (specifically, redraw
!    the quadrant map or galaxy map)
4085 if sgn(oldDamage%) <> sgn(damage%(di%)) then &
        if di% = dSRScan% then &
           q = fnPrintQuadrant() &
        else if di% = dComputer% then &
           q = fnPrintGalaxy()
4090 fnend

!    Return a string to display Time To Repair
4110 def fnUpdateDamage$(di%)
4120 q$ = fnCursor$(damageX% + 14%, damageY% + di%)
4130 if damage%(di%) then q$ = q$ + "TTR " + fnRNum$(damage%(di%), 3%) &
     else q$ = q$ + "Working"
4140 fnUpdateDamage$ = q$
4150 fnend

!    Randomly wreak damage
4200 def fnWreakDamage(shields%)

!    Choose a system at random
4220 qsys% = rnd(1) * 7% + 1%

!    Inflict damage
4230 q = fnAdjustDamage(qsys%, rnd(1) * 5 * (3000% - shields%) / 3000. + 2) : &
     q = fnDisplayMessage(damageNames$(qsys%) + " damaged.")
4240 fnend

!    Randomly repair damage
4250 def fnRepairDamage()
!    Choose a damaged system
4260 for qdi% = rnd(1) * 6% to 13%
4270    if damage%(fnMod%(qdi%, 7%) + 1%) goto 4300
4280 next qdi%
4290 goto 4330

!    If we're here, we found a damaged system
4300 qdi% = fnMod%(qdi%, 7%) + 1%
4310 q = fnAdjustDamage(qdi%, -1000%) : &
     quip% = rnd(1) * 3 : &
     if quip% = 0 then &
        q = fnDisplayMessage("Spock repaired " + &
           damageNames$(qdi%) + " using a new technique.") &
     else if quip% = 1% then &
        q = fnDisplayMessage("Scotty exaggerated the time to repair") + &
            fnDisplayMessage("   the " + damageNames$(qdi%) + ".") &
     else &
        q = fnDisplayMessage("McCoy repaired the " + &
                             damageNames$(qdi%) + ".") + &
            fnDisplayMessage("It turns out he's not just a doctor.")
4320 print fnUpdateDamage$(qsys%)
4330 fnend

!    Repair each system by 1 unit
4400 def fnNormalRepair()
4410 for qdi% = 1% to 7%
4420    if damage%(qdi%) then &
           qq = fnAdjustDamage(qdi%, -1%) : &
           if damage%(qdi%) = 0% then &
              qq = fnDisplayMessage(damageNames$(qdi%) + " repaired.")
4430 next qdi%
4440 fnend

!    Fire phasers
4510 def fnPhasers%()
4520 print "Phasers"

!    Check whether the phasers are damaged
4530 if damage%(dPhasers%) then &
        q = fnDisplayMessage("Phasers inoperable.") : &
        goto 4680

!    Get the Enterprise's location in the quadrant
4532 eqY% = enterpriseY% / 8% : eqX% = enterpriseX% / 8%

!    Get the number of Klingons in the quadrant
4535 k% = (galaxy%(eqY%, eqX%) - 1000%) / 100%

!    If there are no Klingons, don't fire
4537 if k% = 0% then &
        q = fnDisplayMessage("No Klingons in this quadrant.") : &
        goto 4690

!    Get the number of units to fire
4540 q = fnDisplayMessage("Phasers locked on target.") 
4545 q = fnDisplayMessage("Units to fire? " + clearLine$)
4550 on error goto 4680
4560 input line #1, qin$
4570 phasers% = val(cvt$$(qin$, 4%))

!    Check the user's entry
4580 if phasers% <= 0% goto 4680 &
     else if phasers% > energy% then &
        q = fnClearMessages() : &
        q = fnDisplayMessage("Only" + num$(energy%) + "units available") : &
        goto 4540

!    Deduct the energy from our total
4590 energy% = energy% - phasers% : destroyed% = 0% 
4595 print fnUpdateEnergy$() : q = fnClearMessages()

!    Go through all the Klingons
4600 for qi% = 1% to 3%

!       If the Klingon is still alive
4610    pwr% = localKlingons%(qi%, 2%)
4620    if pwr% = 0% goto 4660

!       Shoot him! Energy is attenuated by distance and randomness
4625    hit% = phasers% / fnDist(qi%) * rnd(1)
4630    pwr% = fnMax%(0%, pwr% - hit%) : &
        localKlingons%(qi%, 2%) = pwr%

!       Report the results
4640    msg$ =cvt$$(num$(hit%), 2%) + " unit hit on Klingon." : &
        if pwr% then msg$ = msg$ + num$(pwr%) + "left." &
        else msg$ = msg$ + " Klingon destroyed!"
4645    q = fnDisplayMessage(msg$)
4650    if pwr% = 0% then q = fnKlingonDestroyed(qi%)
4660 next qi%

!    The Klingons fight back!
4665 q = fnKlingonAttack()
4670 goto 4700

!    If an error occurred, cancel
4680 q = fnClearMessages() + fnDisplayMessage("Phaser command canceled.")
4690 resume 4700
4700 fnend

!    Process the destruction of a Klingon, based on his index in localKlingons
4900 def fnKlingonDestroyed(ki%)

!    Get our current quadrant
4910 eqY% = enterpriseY% / 8% : eqX% = enterpriseX% / 8%

!    Subtract the Klingon from the galaxy map and redraw it.
!    Also set his power to zero in the localKlingon array
4920 galaxy%(eqY%, eqX%) = galaxy%(eqY%, eqX%) - 100% : &
     print fnUpdateQuadrant$(eqY%, eqX%) : &
     quadrant%(localKlingons%(ki%, 0%), localKlingons%(ki%, 1%)) = 0% : &
     print fnUpdateSector$(localKlingons%(ki%, 0%), &
                           localKlingons%(ki%, 1%)) : &
     localKlingons%(ki%, 2%) = 0% : &
     if fnMod%(galaxy%(eqY%, eqX%), 1000%) < 100% then &
        print fnUpdateCondition$()

!    Decrease the total Klingon count
4930 klingons% = klingons% - 1%
4940 print fnCursor$(52%, 12%); fnRNum$(klingons%, 2%)
4950 fnend

!    Process the destruction of a Klingon by coordinates
5000 def fnKlingonDestroyedRC(row%, col%)
5010 for qi% = 1% to 3%
5020    if localKlingons%(qi%, 0%) = row% and &
           localKlingons%(qi%, 1%) = col% and &
           localKlingons%(qi%, 2%) then &
           fnKlingonDestroyedRC = fnKlingonDestroyed(qi%) 
5030 next qi%
5040 fnend

!    Play the game!
6010 def fnPlayGame

!    Accept commands until we're game over
6020 gameOver% = 0%
6030 for round% = 0% until gameOver%

!       Get the user's command
6045    if shields% <= 200% then &
           prompt$ = "Command: (shields up, Captain?) " &
        else &
           prompt$ = "Command: "
6050    print fnCursor$(commandX%, commandY%); prompt$; clearLine$;

!       Accept a single character without hitting Return
6060    cmd$ = cvt$$(fnGetChar$(), 32%)

6065    q = fnClearMessages() : &
        print fnCursor$(commandX% + len(prompt$), commandY%);

!       Dispatch the command
6070    if cmd$ = "W" then q% = fnWarp%() &
        else if cmd$ = "P" then q% = fnPhasers%() &
        else if cmd$ = "T" then q% = fnTorpedoes%() &
        else if cmd$ = "S" then q% = fnShields%() &
        else if cmd$ = "L" then q% = fnLRScan%() &
        else if cmd$ = "A" then q% = fnAbandon%() &
        else if cmd$ = "R" then q = fnRefreshScreen()
        
!       Check for game over
6080    if shields% < 0% or energy% + shields% <= 0% or klingons% = 0% or &
           stardates% = 0% then &
           gameOver% = 1%   

6090 next round%

!    Announce the end of the game if all the Klingons are dead
6100 if klingons% = 0% then &
        if energy% + shields% >= 0% then &
           q = fnDisplayMessage("The Federation has been saved!") + &
               fnDisplayMessage("You are promoted to Admiral... until you") + &
               fnDisplayMessage("steal a starship or something.") &
        else &
           q = fnDisplayMessage("The Federation has been saved!") + &
               fnDisplayMessage("But the Enterprise was destroyed.") + &
               fnDisplayMessage("You are promoted to Admiral posthumously.")
               
!    If there are still Klingons, maybe the Enterprise is destroyed or
!    we ran out of time
6110 if klingons% then &
        if shields% < 0 then &
           q = fnDisplayMessage("The Enterprise has been destroyed!") + &
               fnDisplayMessage("The Federation will be conquered.") &
        else if energy% + shields% = 0% then &
           q = fnDisplayMessage("The Enterprise is dead in space!") + &
               fnDisplayMessage("The Federation will be conquered.") &
        else &
           q = fnDisplayMessage("Time has run out!") + &
               fnDisplayMessage("The Federation has been conquered.") + &
               fnDisplayMessage("You will be imprisoned on Rura Penthe.")
        
6120 fnend

6200 def fnOfferReplay%()
6210 print fnCursor$(commandX%, commandY%); &
        "Would you like to play again? "; clearLine$;
!    Accept a single character without hitting Return
6220 cmd$ = cvt$$(fnGetChar$(), 32%)
6230 if cmd$ <> "Y" and cmd$ <> "N" goto 6210
6240 fnOfferReplay% = (cmd$ = "Y")
6250 fnend

!    Implement a max() function
6400 def fnMax%(a%, b%)
6410 if a% < b% then fnMax% = b% else fnMax% = a%
6420 fnend
!    And a min() function
6430 def fnMin%(a%, b%)
6440 if a% < b% then fnMin% = a% else fnMin% = b%
6450 fnend

!    Long Range Scan
6510 def fnLRScan%()
6520 print "Long Range Scan";
6525 sleep 1% ! Let the "Long Range Scan" stay up for a moment

!    Make sure the scanner isn't damaged
6530 if damage%(dLRScan%) then &
        q = fnDisplayMessage("Long range scanner inoperable.") : &
        goto 6600

!    Get our quadrant
6540 qx% = enterpriseX% / 8% : qy% = enterpriseY% / 8%

!    Even if the comupter is down, show the quadrants around us
6545 compDamage% = damage%(dComputer%) : damage%(dComputer%) = 0%

!    Loop through the quadrants around us
6550 for qrow% = fnMax%(qy% - 1%, 0%) to fnMin%(qy% + 1%, 7%)
6560    for qcol% = fnMax%(qx% - 1%, 0%) to fnMin%(qx% + 1%, 7%)

!          A negative value means the quadrant hasn't been scanned yet.
!          So scan it. But if the computer is damaged, don't store the
!          the fact that it's been scanned
6565       qquad% = galaxy%(qrow%, qcol%)
6570       if qquad% < 0% or compDamage% then &
              galaxy%(qrow%, qcol%) = abs(qquad%) : &
              print fnUpdateQuadrant$(qrow%, qcol%); : &
              if compDamage% then galaxy%(qrow%, qcol%) = qquad%
6580    next qcol%
6590 next qrow%
6600 damage%(dComputer%) = compDamage%
6610 fnend

!    Redraw a quadrant's info in the galactic chart
6710 def fnUpdateQuadrant$(row%, col%)
6720 quqx% = galaxyChartX% + 1% + col% * 6%
6730 quqy% = galaxyChartY% + 1% + row%
6740 fnUpdateQuadrant$ = fnCursor$(quqx%, quqy%) + &
        fnGalaxyQuadrant$(row%, col%)
6750 fnend

!    Set shield strength
7010 def fnShields%()
7020 print "Shields"; 

!    Make sure shield control isn't damaged
7023 if damage%(dShields%) then &
        q = fnDisplayMessage("Shield control inoperable.") : &
        goto 7110

!    Get the new energy setting
7030 q = fnDisplayMessage("Energy available:" + num$(energy% + shields%))
7040 q = fnDisplayMessage("Shield level? ")
7045 on error goto 7090
7050 input line #1, qin$
7053 q = fnClearMessages()
7055 newShields% = val(cvt$$(qin$, 4%))

!    Check the input value
7057 if newShields% < 0 or newShields% > energy% + shields% goto 7090
7060 energy% = energy% + shields% - newShields% : shields% = newShields%
7070 print fnUpdateEnergy$(); fnUpdateShields$();
7080 goto 7110

!    If there was a problem, don't change the shield value
7090 q = fnDisplayMessage("Shield setting unchanged.")
7100 resume 7110
7110 on error goto 0
7120 fnend

!    Display the current energy level
7200 def fnUpdateEnergy$() = fnCursor$(20%, 10%) + &
        fnRNum$(energy%, 4%)

!    Display the current shield level
7210 def fnUpdateShields$()
7220 qus$ = fnCursor$(20%, 11%) + fnRNum$(fnMax%(shields%, 0%), 4%)
7230 if shields% <= 200% then qus$ = qus$ + " LOW" else qus$ = qus$ + "    "
7240 fnUpdateShields$ = qus$
7250 fnend

!    Display the remaining stardates
7260 def fnUpdateStardates$() = fnCursor$(52%, 11%) + fnRNum$(stardates%, 2%)

!    Display the remaining torpedoes
7270 def fnUpdateTorpedoes$() = fnCursor$(22%, 12%) + fnRNum$(torpedoes%, 2%)

!    End the game
7410 def fnAbandon%()
7420 print "Abandon Ship";
7430 q = fnDisplayMessage("Do you want to exit the game? ")
7440 input #1, qa$
7450 if cvt$$(left(qa$, 1%), 32%) = "Y" then print clearScreen$ : goto 32767
7460 fnend

!     Warp speed!
10010 def fnWarp%()
10020 print "Warp"; : q = fnClearMessages()

!     Make sure the engines aren't damaged
10030 if damage%(dWarp%) then &
         q = fnDisplayMessage("Engines damaged. Max. speed warp 0.2!")

!     Get course and distance, checking the input values
10040 q = fnDisplayMessage("Course? ")
10050 on error goto 10310
10060 input line #1, qc$ : qc = val(cvt$$(qc$, 4%))
10070 if qc < 1 or qc >= 9 then goto 10310
10080 q = fnDisplayMessage("Warp factor? ")
10090 input line #1, qw$ : qw = val(cvt$$(qw$, 4%))
10095 q = fnClearMessages()
10100 if qw < 0.125 or (damage%(dWarp%) and qw > 0.2) goto 10310

!     If we don't have enough energy, shorten the trip
10110 qmsg$ = ""
10120 if qw * 8 + 5 > energy% then &
         qw = fnMax%(energy% - 5%, 0%) / 8. : &
         qmsg$ = "Engines shut down due to low energy."

!     Calculate the course through this quadrant
10130 qi% = fnCourseTrack%(qc, qw) ! Fill the track array

!     If we encounter an obstruction, shorten the distance
10150 if qi% < 0% then &
         qw = sqr((track(0%, 1%) - track(-qi% - 1%, 1%)) ^ 2 + &
                  (track(0%, 2%) - track(-qi% - 1%, 2%)) ^ 2) / 8.0 : &
         qmsg$ = "Engines shut down to avoid collision."

!     Calculate the E's new location
10165 theta = (qc - 1) * pi / 4
10170 newX% = enterpriseX% + cos(theta) * qw * 8 + 0.5
10180 newY% = enterpriseY% - sin(theta) * qw * 8 + 0.5

!     If it takes us outside the galaxy, cut it short
10195 if newX% < 0% or newY% > 63% or newY% < 0% or newY% > 63% then &
         qmsg$ = "Engines shut down at galactic barrier."
10200 if newX% < 0% then  &
         qw = qw * enterpriseX% / (enterpriseX% - newX%) : &
         goto 10170
10210 if newX% > 63% then &
         qw = qw * (63% - enterpriseX%) / (newX% - enterpriseX%) : &
         goto 10170
10220 if newY% < 0% then &
         qw = qw * enterpriseY% / (enterpriseY% - newY%) : &
         goto 10170
10230 if newY% > 63% then &
         qw = qw * (63% - enterpriseY%) / (newY% - enterpriseY%) : &
         goto 10170
         
10235 if qmsg$ <> "" then q = fnDisplayMessage(qmsg$) : qmsg$ = ""

!     Calculate what quadrant we'll be in after moving
10240 qX% = enterpriseX% / 8% : qY% = enterpriseY% / 8% : &
      qnX% = newX% / 8% : qnY% = newY% / 8%

!     If we're staying in the same quadrant, redraw the sectors
10250 newQuadrant% = (qX% <> qnX%) or (qY% <> qnY%)
10260 if not newQuadrant% then &
         q = fnChangeSector(enterpriseY%, enterpriseX%, 0%) : &
         q = fnChangeSector(newY%, newX%, 1000%)

!     Set our new location
10265 enterpriseX% = newX% : enterpriseY% = newY%

!     If we're in a new quadrant, update the galaxy map,
!     redraw our old quadrant and new quadrant in the galaxy map,
!     initialize our new quadrant and redraw the quadrant map.
10280 if newQuadrant% then &
         galaxy%(qY%, qX%) = galaxy%(qY%, qX%) - 1000% : &
         print fnUpdateQuadrant$(qY%, qX%); : &
         galaxy%(qnY%, qnX%) = abs(galaxy%(qnY%, qnX%)) + 1000% : &
         print fnUpdateQuadrant$(qnY%, qnX%); : &
         q = fnInitializeQuadrant() : &
         q = fnPrintQuadrant() : &
         if damage%(dComputer%) then q = fnPrintGalaxy()

!     Update the energy consumed by warping
10290 energy% = energy% - int(qw * 8 + 5) : print fnUpdateEnergy$()

!     A stardate has passed
10295 stardates% = stardates% - 1% : &
      print fnUpdateStardates$();

!     Do normal, turn-based repair plus random repair
10296 q = fnNormalRepair() : &
      if rnd(1) < 0.2 then qq = fnRepairDamage()

!     Randomly wreak damage, but the stronger the shields, the lower the risk
10297 if rnd(1) < 0.1 then &
         qq = fnDisplayMessage("** SPACE STORM **") : &
         qq = fnWreakDamage(shields%) 
         

10298 print fnUpdateCondition$();

!     If there are Klingons here, they're gonna shoot at us!
10299 qq = fnKlingonAttack()
10300 goto 10320

!     If there was an error, cancel
10310 qmsg$ = "Warp command canceled." : q = fnClearMessages()
10320 if qmsg$ <> "" then q = fnDisplayMessage(qmsg$)
10330 resume 10340
10340 fnend

!     Figure out what the new alert condition is, and update the display
10510 def fnUpdateCondition$()
10520 if galaxy%(enterpriseY% / 8%, enterpriseX% / 8%) - 1000% >= 100% then &
         cond$ = "   RED" &
      else &
         cond$ = " GREEN"
!     See if we're docked
10540 qsX% = fnMod%(enterpriseX%, 8%) : qsY% = fnMod%(enterpriseY%, 8%)
10550 for qrow% = fnMax%(qsY% - 1%, 0%) to fnMin%(qsY% + 1%, 7%)
10560    for qcol% = fnMax%(qsX% - 1%, 0%) to fnMin%(qsX% + 1%, 7%)
10570       if quadrant%(qrow%, qcol%) = 10% then &
               cond$ = "DOCKED" : &
               q = fnRechargeEnterprise() : &
               print fnUpdateEnergy$(); fnUpdateShields$(); : &
               print fnUpdateTorpedoes$() : &
               q = fnDisplayMessage("Shields lowered for docking.") : &
               goto 10600
10580    next qcol%
10590 next qrow%
10600 fnUpdateCondition$ = fnCursor$(48%, 10%) + cond$
10610 fnend

!     Klingons attack! They're mean like that
11010 def fnKlingonAttack()
11015 hit% = 0% : cnt% = 0%

!     Go through each Klingon in the quadrant
11020 for qi% = 1% to 3%
11030    if localKlingons%(qi%, 2%) = 0% goto 11100
11040    hit% = hit% + int(localKlingons%(qi%, 2%) / fnDist(qi%) * (2+rnd(1)))
11050    cnt% = cnt% + 1%
11100 next qi%

!     If we're docked at a starbase, we're protected
11105 if cnt% <> 0% and cond$ = "DOCKED" then &
         q = fnDisplayMessage("Starbase shields protect Enterprise.") : &
         goto 11130

!     Report hits, using proper singular or plural cases
11110 if cnt% = 1% then &
         q = fnDisplayMessage(cvt$$(num$(hit%), 2%) + &
                       " unit hit on Enterprise from Klingon!") &
      else if cnt% > 1% then &
         q = fnDisplayMessage(cvt$$(num$(hit%), 2%) + &
                       " unit hits on Enterprise from Klingons!")

!     Strong hits, relative to shields, might cause damage
11120 if int(rnd(1) * shields%) < hit% then q = fnWreakDamage(shields%)

11125 shields% = shields% - hit% : print fnUpdateShields$()

11130 fnend

!     Fire torpedo
11510 def fnTorpedoes%()
11520 print "Torpedo";

!     Check whether torpedoes are damaged
11530 if damage%(dTorpedoes%) then &
         q = fnDisplayMessage("Torpedo tubes not operational.") : &
         goto 11810

!     Check whether we have any torpedoes
11540 if torpedoes% = 0 then &
         q = fnDisplayMessage("All torpedoes expended.") : &
         goto 11810

!     Get the torpedo course
11550 q = fnDisplayMessage("Course? ")
11560 on error goto 11760
11570 input line #1, qc$ : qc = val(cvt$$(qc$, 4%))

!     Check the value
11580 if qc < 1 or qc >= 9 then q = fnClearMessages() : goto 11760

!     One less torpedo
11585 torpedoes% = torpedoes% - 1% : print fnUpdateTorpedoes$();

!     Fill in the torpedo track
11590 qi% = fnCourseTrack%(qc, 1) 

!     Display the torpedo's track
11610 t$ = ""
11615 if qi% > 0% then qlast% = qi% - 1% else qlast% = -qi%
11620 for ti% = 1% to qlast%
11630    t$ = t$ + fnFormatCoord$(track(ti%, 2%) + 1) + "-" + &
                   fnFormatCoord$(track(ti%, 1%) + 1) + "  "
11650 next ti%
11670 q = fnDisplayMessage(t$)

!     Check if we missed
11680 if qi% >= 0% then &
         q = fnDisplayMessage("Torpedo missed.") : &
         goto 11745

!     The torpedo hit something
11700 qi% = -qi% : trow% = track(qi%, 1%) + 0.5 : tcol% = track(qi%, 2%) + 0.5

!     Get the object that was hit
11710 h% = quadrant%(trow%, tcol%)

!     If it was a star...
11720 if h% = 1% then &
         q = fnDisplayMessage("Torpedo vaporized by star") : &
         goto 11745
11725 qrow% = enterpriseY% / 8% : qcol% = enterpriseX% / 8%

!     If it was a starbase
11730 if h% = 10% then &
         q = fnDisplayMessage("Starbase destroyed! " + &
                              "Court martial assured!") : &
         q = fnChangeSector(trow%, tcol%, 0%) : &
         galaxy%(qrow%, qcol%) = galaxy%(qrow%, qcol%) - 10% : &
         print fnUpdateQuadrant$(qrow%, qcol%); : &
         goto 11745
         
!     If it was a Klingon...
11740 if h% = 100% then &
         q = fnDisplayMessage("Klingon destroyed!") : &
         q = fnKlingonDestroyedRC(trow%, tcol%) 
11745 q = fnKlingonAttack()
11750 goto 11800

!     If there was an error, torpedo canceled
11760 q = fnClearMessages() + fnDisplayMessage("Torpedo command canceled.")
11770 resume 11800
11800 on error goto 0
11810 fnend

!     Round a coordinate and return it as a string
12000 def fnFormatCoord$(n) = cvt$$(num$(int(n + 0.5)), 2%)

!     Calculate the track of an object, starting at the Enterprise
!     Return the negative index where an obstruction is found, or a positive
!     index if the the track leaves the quadrant
31010 def fnCourseTrack%(course, warp)
31020 mat track = zer
31040 track(0%, 2%) = fnMod%(enterpriseX%, 8%) : &
      track(0%, 1%) = fnMod%(enterpriseY%, 8%)
31050 course = (course - 1) / 4 * pi
31060 deltaX = cos(course) : deltaY = sin(course)

!     scale deltaX and deltaX so that instead of reaching a cicle with
!     radius 1, they reach a square's edges
31063 if abs(deltaX) <= abs(deltaY) then &
         deltaX = deltaX / abs(deltaY) : &
         deltaY = sgn(deltaY)
31064 if abs(deltaY) < abs(deltaX) then &
         deltaY = deltaY / abs(deltaX) : &
         deltaX = sgn(deltaX)
31070 for qi% = 1% to warp * 8
31080    track(qi%, 1%) = track(qi% - 1%, 1%) - deltaY
31090    track(qi%, 2%) = track(qi% - 1%, 2%) + deltaX
31100    if (track(qi%, 1%) < 0 or track(qi%, 1%) > 7) or &
            (track(qi%, 2%) < 0 or track(qi%, 2%) > 7) then &
            fnCourseTrack% = qi% : &
            goto 31140
31110    if quadrant%(track(qi%, 1%) + 0.5, track(qi%, 2%) + 0.5) then &
            fnCourseTrack% = -qi% : &
            goto 31140
31120 next qi%
31130 fnCourseTrack% = qi%
31140 fnend

!     Welcome the user and print instructions
32010 def fnWelcome
32015 print clearScreen$;
32020 print "                  * * *  VT52 STAR TREK  * * *" : print
32030 print "Do you want instructions (Y, [N])? ";
32035 a$ = fnGetChar$() : a$ = cvt$$(left(a$, 1), 32%) : print clearScreen$;
32040 for dummy% = 1% while a$ <> "N"
32050    for ln% = 1% to 23%
32060       on error goto 32130
32070       read l$ : print l$
32075       on error goto 0
32080    next ln%
32090    print "More ([Y], N)? "; : a$ = cvt$$(left(fnGetChar$(), 1), 32%)
32110    print startOfLine$; clearLine$;
32120 next dummy%
32130 resume 32140
32140 if a$ <> "N" then print "Done. Hit any key..."; : a$ = fnGetChar$()
32150 fnend

!     Read a single character from the keyboard
32210 def fnGetChar$()
32215 q$ = sys(chr$(3%)) ! Disable echoing
32220 on error goto 32230 : &
      q$ = sys(chr$(4%) + chr$(1%)) : &
      get #1, record 8192% : &
      goto 32250
32230 if err = 13 then resume 32240 else resume 32250
32240 sleep 1% : goto 32220
32250 fnGetChar$ = getChar$
32255 q$ = sys(chr$(2%)) ! Re-enable echoing
32260 fnend

!     Initialize VT52 escape codes
32410 def fnInitializeEscapeCodes
32420 esc$ = chr$(27% + 128%)
32430 startOfLine$ = chr$(13%)
32440 clearLine$ = esc$ + "K"
32450 clearScreen$ = esc$ + "H" + esc$ + "J"
32490 fnend

32500 data "     Instructions", &
    " ", &
    "The galaxy is divided into an 8,8 quadrant grid", &
    "which is in turn divided into an 8,8 sector grid.", &
    " ", &
    "The cast of characters is as follows:", &
    " E = Enterprise", &
    " K = Klingon", &
    " B = Starbase", &
    " *  = star", &
    "Command W = Warp engine control:", &
    "  Course is in a circular numerical       4  3  2", &
    "  vector arrangement as shown.             \ | /", &
    "  Integer and real values may be            \|/", &
    "  used.  Therefore course 1.5 is         5 ----- 1", &
    "  half way between 1 and 2.                 /|\", &
    "                                           / | \", &
    "  A vector of 9 is undefined, but         6  7  8", &
    "  values may approach 9.", &
    "                                          Course", &
    "  One warp factor is the size of", &
    "  one quadrant.  Therefore to get from", &
    "  quadrant 6,5 to 5,5 you would use course 3, warp factor 1.", &
    " ", &
    "Command L = Long range sensor scan", &
    "  shows conditions in space for one quadrant on each side", &
    "  of the Enterprise in the middle of the scan. The scan", &
    "  is coded in the form xxx, where the units digit is the ", &
    "  number of stars, the tens digit is the number of star-", &
    "  bases.  The hundreds digit is the number of Klingons.", &
    " ", &
    "Command P = Phaser control", &
    "  Allows you to destroy the Klingons by hitting them with", &
    "  suitably large numbers of energy units to deplete their ", &
    "  shield power.  Keep in mind that when you shoot at them,", &
    "  they gonna shoot at you, too!", &
    " ", &
    "Command T = Photon Torpedo control", &
    "  Course is the same as used in warp engine control.", &
    "  If you hit the Klingon, he is destroyed  If you miss,", &
    "  you are subject to his phaser fire.", &
    " ", &
    "Command S = Shield control", &
    "  Defines number of energy units to be assigned to shields.", &
    "  Energy is taken from total ship's energy.", &
    "  Note that total energy includes shield energy."

32767 end

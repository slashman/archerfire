DECLARE SUB pre.options ()
DECLARE FUNCTION sys.randoms% (min%, max%)
DECLARE SUB game.inter ()
DECLARE SUB game.chkspecial ()
DECLARE SUB sys.Comp (which%)
DECLARE FUNCTION sys.currentchain% ()
DECLARE SUB sys.macros (cut%, scrn%)
DECLARE SUB sys.macro (a$)
DECLARE SUB init.Macros (which%)
DECLARE SUB init.Continue ()
DECLARE FUNCTION sys.inp$ (a$, x%, y%)
DECLARE SUB game.chkobj ()
DECLARE SUB game.maneuver ()
DECLARE SUB sys.fade2 ()
OPTION BASE 0
DEFINT A-Z
'$INCLUDE: 'directqb.bi'

DECLARE SUB InitImageData (FileName$, ImageArray%())

DECLARE SUB sys.Brief (which%)
DECLARE SUB sys.color (i%)
DECLARE SUB sys.Dialog2 (s$, w$, w2$, w3$, piture%)
DECLARE SUB sys.Finish ()
DECLARE SUB sys.InAndOut ()
DECLARE SUB sys.initthings (sq%, lsq%, stq%)
DECLARE SUB sys.Maneuver1 ()
DECLARE SUB sys.Maneuver2 ()
DECLARE SUB sys.Script ()
DECLARE FUNCTION sys.Menu2% (x%, y%, wide%, lng%, hsqr%, vsqr%)
DECLARE FUNCTION sys.Menu% (x1%, y1%, wid%, lenght%, num%, po%)

DECLARE SUB init.ClearVars ()
DECLARE SUB init.compose ()
DECLARE SUB init.Create ()
DECLARE SUB init.Cuts (which%)
DECLARE SUB init.Gauntlet ()
DECLARE SUB init.Level1 (which%)
DECLARE SUB init.NewHistory (which%)
DECLARE SUB init.NewGauntlet ()
DECLARE SUB init.Pilot ()
DECLARE SUB init.Ship ()
DECLARE SUB init.spawn ()
DECLARE SUB init.Weapon ()

DECLARE SUB pre.scene1 (which%)
DECLARE SUB pre.Brief1 (which%)
DECLARE SUB pre.IntroHistory (which%)
DECLARE SUB pre.GoodBye ()
DECLARE SUB pre.GauntletIntro ()
DECLARE SUB pre.intro ()
DECLARE SUB pre.title ()

DECLARE SUB env.menu1 ()
DECLARE SUB env.fore ()
DECLARE SUB env.menu ()
DECLARE SUB env.draw ()

DECLARE FUNCTION game.chkblast (x%, y%)
DECLARE SUB game ()
DECLARE SUB game.divine ()
DECLARE SUB game.complete (which%)
DECLARE SUB game.chkspecials ()
DECLARE SUB game.chkcs ()
DECLARE SUB game.gameover ()
DECLARE SUB gauntlet ()
DECLARE SUB game.setspecial (mission%, cut%, scrn%)
DECLARE SUB game.alienmove ()
DECLARE SUB game.chkupgrade ()
DECLARE SUB game.msgs ()
DECLARE SUB game.gameover ()
DECLARE SUB game.explode (x%, y%)
DECLARE SUB game.alienshoot ()
DECLARE SUB game.chkali ()
DECLARE SUB game.spawn (i%)
DECLARE SUB game.plotarmy ()
DECLARE SUB game.shoot ()
DECLARE SUB game.plotship ()
DECLARE SUB game.getkeys ()

RANDOMIZE TIMER
CONST Size = ((16 * 9) / 2) + 4
CONST GameSpeed = 0, MaxChain = 16, EnemyGat = 5
CLEAR
CONST debug = 0
'CONST path$ = "\dqb\andcher\"
CONST path$ = ".\"

'EnemyGat = VELOCIDAD DE DISPARO,
'MaxChain=NO DE DISPAROS

TYPE CUTchn
 scrno AS INTEGER
 light AS INTEGER
 active AS INTEGER
 done AS INTEGER
END TYPE

TYPE cut
 time AS INTEGER
 portrait AS INTEGER
 who AS STRING * 15
 s1 AS STRING * 45
 s2 AS STRING * 45
 s3 AS STRING * 45
END TYPE

TYPE level
     maxene AS INTEGER
     curene AS INTEGER
     enespd AS INTEGER
END TYPE

TYPE sho
     act AS INTEGER
     px AS INTEGER
     py AS INTEGER
     spd AS INTEGER
     face AS INTEGER
END TYPE

TYPE alie
     act AS INTEGER
     py AS INTEGER
     px AS INTEGER
     spd AS INTEGER
     shooting AS INTEGER
     face AS INTEGER
     dir AS INTEGER
     sx AS INTEGER
     sy AS INTEGER
     Value AS INTEGER
     ai AS INTEGER
     state AS INTEGER
END TYPE

TYPE shi
     py AS INTEGER
     px AS INTEGER
     spd AS INTEGER
     shooting AS INTEGER
     weapon AS INTEGER
     ammo AS INTEGER
END TYPE

TYPE pla
     score AS LONG
     energy AS INTEGER
     shots AS LONG
     hits AS INTEGER
     avrg AS INTEGER
     pilot AS INTEGER
     chasis AS INTEGER
     gat AS INTEGER
     spd AS INTEGER
     per AS INTEGER
     eva AS INTEGER
     abr AS INTEGER
     Sshi AS INTEGER
     Shspd AS INTEGER
     Svspd AS INTEGER
     Sreg AS INTEGER
     Sgat AS INTEGER
     sweap AS INTEGER
     Schain AS INTEGER
     chan AS INTEGER
     mode AS INTEGER
     divine AS INTEGER
     visto AS INTEGER
END TYPE

TYPE enviro
     fore AS INTEGER
     back1 AS INTEGER
     back2 AS INTEGER
     spd AS INTEGER
     starsq AS INTEGER
     lstarsq AS INTEGER
     stonesq AS INTEGER
END TYPE

TYPE gauge
 active AS INTEGER
 x AS INTEGER
 wide AS INTEGER
 dir AS INTEGER
 spd AS INTEGER
 power AS INTEGER
 pass AS INTEGER
 position AS INTEGER
END TYPE

DIM SHARED stars(1 TO 20) AS sho
DIM SHARED lilstars(1 TO 20) AS sho
DIM SHARED rocks(1 TO 15) AS sho
DIM SHARED msgcnt%, msgactive%, msgmiss%, msggat%, msgspd%, msgper%, msgeva%
DIM SHARED msgabs%, envirom AS enviro, mes%
DIM SHARED special, screens
DIM SHARED cuts(5, 10) AS cut
DIM SHARED cutchain(5) AS CUTchn
'DIM SHARED macros(1 TO 5, 1 TO 6, 1 TO 10) AS STRING * 10
DIM SHARED macros(1 TO 5, 1 TO 6, 1 TO 10) AS STRING

DIM SHARED scene AS level, lys, outtahere, ct, onmsg, maneuver

DIM SHARED specials(1 TO 10) AS sho

DIM SHARED army(1 TO 20) AS alie
DIM SHARED alienst(1 TO 20) AS sho

DIM SHARED ship AS shi, shootx, shooty, shootspeed
DIM SHARED ss(1 TO MaxChain) AS sho
DIM SHARED player AS pla
DIM SHARED face, chasis, t, lang$

DIM SHARED palet AS STRING * 768

DIM SHARED interactive AS gauge
'-----
SCREEN 0
CLS
PRINT "Initializing DQB!"

SELECT CASE DQBinit(1, 0)
CASE 1
     PRINT "DQB Initialization error...."
     PRINT
     PRINT "You got to do something to put EMS to work..."
     PRINT
     PRINT "Read helpme.txt for assistance, for further aid"
     PRINT "visit http://szdev.tripod.com."
     PRINT
     PRINT "Cya!"
     DO: LOOP UNTIL INKEY$ <> ""
     DQBclose
     END
CASE 0
     PRINT "DQB has been succesfully loaded."
     DO: LOOP UNTIL INKEY$ <> ""
END SELECT

SCREEN 13
DQBinitVGA

'pp256
REDIM SHARED texture(1 TO 1)
REDIM SHARED ships(1 TO 1)
REDIM SHARED backs(1 TO 1)
REDIM SHARED aliships(1 TO 1)
REDIM SHARED weaps(1 TO 1)
REDIM SHARED logos(1 TO 1)
REDIM SHARED weps(1 TO 1)
REDIM SHARED faces(1 TO 1)
REDIM SHARED dudes(1 TO 1)
REDIM SHARED messes(1 TO 1)
REDIM SHARED face(1 TO 1)
REDIM SHARED planes(1 TO 1)
InitImageData path$ + "imgs\text.szg", texture()
InitImageData path$ + "imgs\ships.szg", ships()
InitImageData path$ + "imgs\backs.szg", backs()
InitImageData path$ + "imgs\alships.szg", aliships()
InitImageData path$ + "imgs\weaps.szg", weaps()
InitImageData path$ + "imgs\logos.szg", logos()
InitImageData path$ + "imgs\weapmen.szg", weps()
InitImageData path$ + "imgs\faces.szg", faces()
InitImageData path$ + "imgs\dudes.szg", dudes()
InitImageData path$ + "imgs\mess.szg", messes()
InitImageData path$ + "imgs\port.szg", face()
InitImageData path$ + "imgs\planes.szg", planes()

DEF SEG = VARSEG(palet)
BLOAD path$ + "res\rainbow.szp", VARPTR(palet)
DEF SEG
DQBsetPal palet

'---
RESTART:

sys.color 1
DQBsetTextStyle TEXTURED
IF DQBloadFont(path$ + "res\archie.fnt") THEN
 DQBsetBIOSfont
 DQBprint 0, "Boooing! Font Loading Error.... mail SZ!", 5, 5, 9
 DQBwaitKey (KEYANY)
 sys.Finish
END IF

DQBinstallKeyboard
'pre.intro
lang$ = "English"
pre.title
sys.Finish

SUB env.draw
SELECT CASE envirom.back1
CASE 1
 FOR i = 1 TO envirom.starsq
  stars(i).px = stars(i).px - stars(i).spd - envirom.spd
  IF stars(i).px < 330 THEN
   DQBput 1, stars(i).px, stars(i).py, VARSEG(backs(1)), VARPTR(backs(1))
  END IF
  IF stars(i).px < -16 THEN
   stars(i).px = 320 + INT(RND * 140) + 1
   stars(i).py = INT(RND * 195) + 1
   stars(i).spd = (INT(RND * 5) + 3) + GameSpeed
  END IF
 NEXT
END SELECT

SELECT CASE envirom.back2
CASE 1
 FOR i = 1 TO envirom.lstarsq
  lilstars(i).px = lilstars(i).px - lilstars(i).spd - envirom.spd
  IF lilstars(i).px < 330 THEN
   DQBpset 1, lilstars(i).px, lilstars(i).py, 9
  END IF
  IF lilstars(i).px < 1 THEN
   lilstars(i).px = 320 + INT(RND * 140) + 1
   lilstars(i).py = INT(RND * 195) + 1
   lilstars(i).spd = (INT(RND * 3) + 1) + GameSpeed
  END IF
 NEXT
END SELECT

END SUB

SUB env.fore
SELECT CASE envirom.fore
CASE 1
 FOR i = 1 TO envirom.stonesq
  rocks(i).px = rocks(i).px - rocks(i).spd - envirom.spd
  IF rocks(i).px < 340 THEN
   DQBput 1, rocks(i).px, rocks(i).py, VARSEG(backs(1)), VARPTR(backs(rocks(i).face))
  END IF
  IF rocks(i).px < -16 THEN
   rocks(i).px = 320 + INT(RND * 140) + 1
   rocks(i).py = INT(RND * 195) + 1
   rocks(i).spd = (INT(RND * 7) + 5) + GameSpeed
   SELECT CASE INT(RND * 3) + 1
   CASE 1
    rocks(i).face = 16
   CASE 2
    rocks(i).face = 31
   CASE 3
    rocks(i).face = 65
   END SELECT
  END IF
 NEXT
END SELECT

END SUB

SUB env.menu
DQBput 1, 0, 170, VARSEG(logos(1)), VARPTR(logos(1))
DQBput 1, 60, 175, VARSEG(logos(1)), VARPTR(logos(764))
DQBput 1, 160, 175, VARSEG(logos(1)), VARPTR(logos(1000))
DQBput 1, 280, 170, VARSEG(logos(1)), VARPTR(logos(417))
DQBprint 1, STR$(player.score), 100, 177, 15
DQBprint 1, STR$(player.energy) + "%", 210, 177, 15
DQBprint 1, "Beta Version", 200, 10, 15

END SUB

SUB env.menu1
DQBput 1, 0, 170, VARSEG(logos(1)), VARPTR(logos(1))
DQBput 1, 60, 175, VARSEG(logos(1)), VARPTR(logos(764))
DQBput 1, 160, 175, VARSEG(logos(1)), VARPTR(logos(1000))
DQBput 1, 280, 170, VARSEG(logos(1)), VARPTR(logos(417))
DQBprint 1, STR$(ship.ammo), 100, 177, 15
DQBprint 1, STR$(player.energy) + "%", 210, 177, 15

IF debug THEN
 DQBprint 1, "Enemies down:" + STR$(player.hits), 200, 20, 15
 DQBprint 1, "DEBIG INFO:", 200, 10, 15
 DQBprint 1, "LYS:" + STR$(lys) + "t" + STR$(t), 200, 30, 15
 DQBprint 1, "Screens:" + STR$(screens), 200, 40, 15
 DQBprint 1, "LYS, Act, Done", 200, 50, 15
 DQBprint 1, STR$(cutchain(1).light) + STR$(cutchain(1).active) + STR$(cutchain(1).done), 200, 60, 15
 DQBprint 1, STR$(cutchain(2).light) + STR$(cutchain(2).active) + STR$(cutchain(2).done), 200, 70, 15
 DQBprint 1, STR$(cutchain(3).light) + STR$(cutchain(3).active) + STR$(cutchain(3).done), 200, 80, 15
 DQBprint 1, STR$(cutchain(4).light) + STR$(cutchain(4).active) + STR$(cutchain(4).done), 200, 90, 15
 DQBprint 1, STR$(cutchain(5).light) + STR$(cutchain(5).active) + STR$(cutchain(5).done), 200, 100, 15
 curr = 1
 IF cutchain(1).active = 1 THEN curr = 1
 IF cutchain(2).active = 1 THEN curr = 2
 IF cutchain(3).active = 1 THEN curr = 3
 IF cutchain(4).active = 1 THEN curr = 4
 IF cutchain(5).active = 1 THEN curr = 5
 DQBprint 1, macros(curr, screens + 1, 1), 200, 110, 15
 DQBprint 1, macros(curr, screens + 1, 2), 200, 120, 15
 DQBprint 1, macros(curr, screens + 1, 3), 200, 130, 15
 DQBprint 1, macros(curr, screens + 1, 4), 200, 140, 15
 DQBprint 1, macros(curr, screens + 1, 5), 200, 150, 15
 DQBprint 1, macros(curr, screens + 1, 6), 200, 160, 15
END IF

END SUB

SUB game
'Erase old things
DQBclearLayer 1
'Get Keys
game.getkeys
'Alien AI
game.chkali
game.alienmove
'Draw new things
env.draw       'Stars
game.shoot     'Weapons
game.alienshoot'Alien Weaps
game.plotarmy  'Alien Ships
game.plotship  'Ship
env.fore       'Foreground
game.msgs      'Mensajitos
env.menu1       'Menu
'Checks
game.chkcs
game.chkspecial
game.chkobj
game.inter
game.maneuver
'DQBwait (1)
DQBcopyLayer 1, VIDEO
'Regen
t = t + 1
IF t MOD (30 - player.Sreg) * 30 = 0 AND player.energy < 100 THEN player.energy = player.energy + 1

IF t = 1000 THEN
 ct = 0
 lys = lys + 1
 t = 0
END IF

END SUB

SUB game.alienmove

FOR i = 1 TO scene.curene
 SELECT CASE army(i).ai
 CASE 1
  army(i).px = army(i).px - army(i).spd
 CASE 2
  army(i).px = army(i).px - army(i).spd
  SELECT CASE army(i).state
  CASE 0
   army(i).py = army(i).py + 1
  CASE 1
   army(i).py = army(i).py - 1
  END SELECT
  IF army(i).py < 10 THEN army(i).state = 0
  IF army(i).py > 160 THEN army(i).state = 1
 END SELECT
 IF army(i).px < -32 THEN
  game.spawn i
 END IF
 
NEXT

END SUB

SUB game.alienshoot

FOR i = 1 TO scene.curene
 IF alienst(i).act = 1 THEN
  alienst(i).px = alienst(i).px - shootspeed + 1
  DQBput 1, alienst(i).px, alienst(i).py, VARSEG(weaps(1)), VARPTR(weaps(4 * 74 + 1))
  IF alienst(i).px < -16 THEN
   alienst(i).act = 0
   alienst(i).px = 320
  END IF
  IF alienst(i).px > ship.px AND alienst(i).px < ship.px + 32 THEN
   IF alienst(i).py > ship.py AND alienst(i).py < ship.py + 32 THEN
    IF (INT(RND * 100) + 1) > player.eva THEN
     alienst(i).act = 0
     game.explode ship.px, ship.py
     player.energy = player.energy - (INT(RND * 15) + 12) + (INT(RND * (player.abr \ 2))) + player.Sshi \ 3
     IF player.energy > 100 THEN player.energy = 100
     IF player.energy < 0 THEN
      SELECT CASE player.mode
      CASE IS > 0
       IF player.divine > 0 THEN
        game.divine
        player.divine = player.divine - 1
       ELSE
        game.gameover
       END IF
      CASE 0
       game.gameover
      END SELECT
     END IF
    ELSE
     msgcnt = 0: msgactive = 0
     msggat = 0: msgspd = 0: msgper = 0: msgeva = 0: msgabs = 0: msgmiss = 0
     msgmiss = 1
     msgactive = 1
    END IF
   END IF
  END IF
 END IF
NEXT

END SUB

SUB game.chkali
IF player.visto THEN
FOR i = 1 TO scene.curene
 IF t MOD EnemyGat = 0 THEN
  IF army(i).px < 320 THEN
   IF army(i).py + 16 > ship.py AND army(i).py + 16 < ship.py + 32 THEN
    IF alienst(i).act = 0 THEN
     alienst(i).act = 1
     alienst(i).px = army(i).px
     alienst(i).py = army(i).py + 16
    END IF
   END IF
  END IF
 END IF
NEXT
END IF
END SUB

FUNCTION game.chkblast (x, y)
FOR i = 1 TO scene.curene
 IF x > army(i).px AND x < army(i).px + 32 THEN
  IF y > army(i).py - 9 AND y < army(i).py + 32 THEN
   player.score = player.score + army(i).Value
   game.explode army(i).px, army(i).py
   game.spawn i
   player.hits = player.hits + 1
   player.shots = player.shots + 1
   game.chkblast = 1
  END IF
 END IF
NEXT
END FUNCTION

SUB game.chkcs

FOR i = 1 TO 5
 IF lys = cutchain(i).light AND cutchain(i).done = 0 THEN cutchain(i).active = 1: onmsg = 1
NEXT

quien = 99

FOR i = 1 TO 5
IF cutchain(i).active = 1 THEN
     a$ = cuts(i, screens + 1).s1
     b$ = cuts(i, screens + 1).s2
     c$ = cuts(i, screens + 1).s3
     w$ = cuts(i, screens + 1).who
     quien = cuts(i, screens + 1).portrait
     ct = ct + 1
     IF ct = 150 THEN screens = screens + 1: ct = 1
     IF screens = cutchain(i).scrno THEN
      ct = 0
      cutchain(i).active = 0
      cutchain(i).done = 1
      onmsg = 0
      screens = 0
     END IF
     IF t MOD 150 = 2 THEN game.setspecial player.mode, i, screens + 1
END IF
NEXT

IF onmsg THEN sys.Dialog2 w$, a$, b$, c$, quien
END SUB

SUB game.chkobj
SELECT CASE player.mode
 CASE 2
     IF player.hits = 40 THEN lys = 3: t = 1: player.hits = 41
 END SELECT
END SUB

SUB game.chkspecial
SELECT CASE special
CASE 1
 FOR i = 1 TO 10
  specials(i).px = -INT(RND * 100) + 1
  specials(i).py = INT(RND * 200) + 1
 NEXT
 special = 2
CASE 2
 FOR i = 1 TO 10
  IF t MOD 4 = 1 THEN specials(i).spd = specials(i).spd + 1
  specials(i).px = specials(i).px + specials(i).spd
  IF specials(i).px > -5 THEN DQBput 1, specials(i).px, specials(i).py, VARSEG(weaps(1)), VARPTR(weaps((ship.weapon - 1) * 74 + 1))
  IF game.chkblast(specials(i).px, specials(i).py) THEN
   specials(i).px = -INT(RND * 200) + 1
   specials(i).py = INT(RND * 200) + 1
   specials(i).spd = 0
  END IF
  IF specials(i).px > 320 THEN
   specials(i).px = -INT(RND * 200) + 1
   specials(i).py = INT(RND * 200) + 1
   specials(i).spd = 0
  END IF
 NEXT
CASE 3
 DQBboxf 1, 0, 0, 31, 31, 0
 DQBget 1, 0, 0, 31, 31, VARSEG(ships(1)), VARPTR(ships(1))
 player.visto = 0
 special = 4
 interactive.active = 1
 interactive.wide = 30
 interactive.spd = 5
 interactive.dir = 1
 interactive.x = 30
 interactive.pass = 1.5
 interactive.power = 500
 interactive.position = 150
CASE 4
 DQBprint 1, "Stealth Device power " + STR$(interactive.power), 160, 100, 15
 DQBellipse 1, ship.px, ship.py, 40, 40, t MOD 10
CASE 5
 init.compose
 player.visto = 1
 special = 0
CASE 6
 interactive.active = 1
 interactive.wide = 5
 interactive.spd = 15
 interactive.pass = 8
 special = 4
END SELECT

END SUB

SUB game.chkupgrade
IF player.score MOD 5000 = 0 THEN
 scene.curene = scene.curene + 1
 player.chan = player.chan + 1
 game.spawn scene.curene
 SELECT CASE scene.curene - 2
 CASE 3
  envirom.fore = 1
 CASE 4
  ship.weapon = ship.weapon + 1
 CASE 7
  ship.weapon = ship.weapon + 1
 END SELECT
 player.score = player.score + 100
END IF

IF player.score MOD 2000 = 0 AND player.score <> 0 THEN
     which = INT(RND * 5) + 1
     much = INT(RND * 2) + 1
     msgcnt = 0: msgactive = 0
     msggat = 0: msgspd = 0: msgper = 0: msgeva = 0: msgabs = 0: msgmiss = 0
     SELECT CASE which
     CASE 1
          player.gat = player.gat + much
          msggat = 1
     CASE 2
          player.spd = player.spd + much
          msgspd = 1
     CASE 3
          player.per = player.per + much
          msgper = 1
     CASE 4
          player.eva = player.eva + much
          msgeva = 1
     CASE 5
          player.abr = player.abr + much
          msgabs = 1
     END SELECT
     player.score = player.score + 100
     msgactive = 1
     msgcnt = 0
END IF

END SUB

SUB game.complete (which%)

DQBfadeTo 0, 0, 0
DQBclearLayer 0
DQBclearLayer 1

SELECT CASE lang$
CASE "English"
 OPEN path$ + "comp.szs" FOR INPUT AS #1
CASE "Espanol"
 OPEN path$ + "ecomp.szs" FOR INPUT AS #1
END SELECT

sys.Comp (which)
CLOSE #1
DQBfadeIn palet
DQBwaitKey (KEYSPACE)

init.NewHistory player.mode + 1

END SUB

SUB game.divine
DQBfadeTo 0, 0, 0
DQBclearLayer 0
DQBclearLayer 1
player.energy = player.energy + (3 * player.per)
DQBfadeTo 63, 63, 63
DQBprint 0, "... Your time hasn't come yet warrior ...", 40, 50, 15
sys.InAndOut
DQBprint 0, "... I'll grant you life again... but ...", 50, 70, 15
sys.InAndOut
DQBprint 0, "... You should now go back and Fight! ...", 60, 90, 15
sys.InAndOut
DQBfadeIn palet

END SUB

SUB game.explode (xr, yr)
DQBboxf 0, 0, 0, 319, 199, 15
FOR i = 1 TO 10
     DQBellipse VIDEO, xr + 16, yr + 16, i + 5, i, 250
NEXT
DQBwait (1)
FOR i = 1 TO 20
     DQBellipse VIDEO, xr + 16, yr + 16, i + 5, i, 9
NEXT
DQBwait (1)
'DQBboxf 0, 0, 0, 319, 199, 0
END SUB

SUB game.gameover

DQBclearLayer 1
IF DQBloadFont(path$ + "res\archie.fnt") THEN DQBsetBIOSfont

SELECT CASE player.mode
CASE 0
     DQBfadeTo 63, 0, 0
     DQBclearLayer 0
     IF player.hits = 0 THEN player.hits = 1
     IF player.shots = 0 THEN player.shots = 1
     a# = player.score
     IF a# < 5000 THEN
          rank$ = "Rookie"
          thing$ = "Well... that's not really good man..."
     END IF
     IF a# >= 5000 AND a# < 15000 THEN
          rank$ = "Pilot"
          thing$ = "Good, but you can do it better."
     END IF
     IF a# >= 15000 AND a# < 25000 THEN
          rank$ = "Veteran"
          thing$ = "Well done, now go for the next challenge!"
     END IF
     IF a# >= 25000 AND a# < 40000 THEN
          rank$ = "Legionary"
          thing$ = "Now you're good enough to protect your king!"
     END IF
     IF a# >= 40000 THEN
          rank$ = "Rocker"
          thing$ = "Haha!, isn't it good to be an expert?"
     END IF

     DQBprint 0, "Your Gauntlet Quest is Over...", CENTERED, 20, 15
     DQBprint 0, "You Scored " + STR$(player.score) + "pts.", CENTERED, 40, 9
     DQBprint 0, "and advanced to level" + STR$((player.score \ 5000) + 1), CENTERED, 50, 9
     DQBprint 0, "That gives you the rank of " + rank$, CENTERED, 60, 9
     DQBprint 0, thing$, CENTERED, 70, 9
     DQBprint 0, "Average:" + STR$(INT(player.score / player.shots)) + "pts.", CENTERED, 80, 9
     DQBprint 0, "Final Stats:", CENTERED, 100, 9
     DQBprint 0, "Gattling: " + STR$(player.gat), CENTERED, 120, 9
     DQBprint 0, "Speed: " + STR$(player.spd), CENTERED, 130, 9
     DQBprint 0, "Perception: " + STR$(player.per), CENTERED, 140, 9
     DQBprint 0, "Evade: " + STR$(player.eva), CENTERED, 150, 9
     DQBprint 0, "Absorb: " + STR$(player.abr), CENTERED, 160, 9
CASE ELSE
     DQBfadeTo 0, 0, 63
     DQBclearLayer 0
     DQBprint 0, "Game Over...", CENTERED, 20, 15
END SELECT
DQBprint 0, "Press ESC to continue...", CENTERED, 180, 15
DQBfadeIn palet
DQBwaitKey (KEYESC)
'sys.Finish
'END
DQBclearLayer 0
DQBclearLayer 1
pre.title
END SUB

SUB game.getkeys
IF DQBkey(KEYUP) THEN
 IF ship.py > 1 THEN ship.py = ship.py - ((player.spd \ 5) \ 2) - ((player.Svspd \ 5) \ 2)
END IF

IF DQBkey(KEYDOWN) THEN
 IF ship.py < 165 THEN ship.py = ship.py + ((player.spd \ 5) \ 2) + ((player.Svspd \ 5) \ 2)
END IF

IF DQBkey(KEYRIGHT) THEN
 IF ship.px < 200 THEN ship.px = ship.px + ((player.spd \ 5) \ 2) + ((player.Shspd \ 5) \ 2)
END IF

IF DQBkey(KEYLEFT) THEN
 IF ship.px > 1 THEN ship.px = ship.px - ((player.spd \ 5) \ 2) - ((player.Shspd \ 5) \ 2)
END IF

IF DQBkey(29) THEN
 IF ((25 - player.gat) \ 2 + (25 - player.Sgat) \ 2) > 0 THEN
  rate = ((25 - player.gat) \ 2 + (25 - player.Sgat) \ 2)
 ELSE
  rate = 1
 END IF

 IF t MOD rate = 0 THEN
  i = 1
  WHILE i < player.chan AND F = 0
   IF ship.ammo > 0 AND ss(i).act = 0 THEN
    IF player.mode >= 1 THEN ship.ammo = ship.ammo - 1
    ss(i).act = 1
    ss(i).py = ship.py + 20
    ss(i).px = ship.px + 10
    IF ship.weapon = 1 THEN ss(i).spd = 0 ELSE ss(i).spd = 8
    F = 1
   END IF
   i = i + 1
  WEND
 END IF
END IF

IF DQBkey(63) THEN
     player.gat = 23
     player.spd = 50
     player.per = 99
     player.eva = 99
     player.abr = 99
     player.Shspd = 50
     player.Svspd = 50
     player.Sreg = 99
     player.Sgat = 23
     player.chan = 16

     DQBprint 0, "CHEAT!", CENTERED, 100, 15
     DQBwaitKey (KEYESC)
END IF

IF DQBkey(25) THEN
     DQBwait (15)
     DQBprint 0, "PAUSE", CENTERED, 100, 15
     DQBwaitKey (KEYANY)
END IF
IF DQBkey(KEYESC) THEN
     sys.Finish
END IF
END SUB

SUB game.inter
IF interactive.active THEN
 DQBline 1, 10, 150, 290, 150, 9
 DQBbox 1, interactive.position - (interactive.wide / 2), 135, interactive.position + (interactive.wide / 2), 165, 9
 DQBellipse 1, interactive.x, 150, 10, 10, t MOD 30
 interactive.x = interactive.x + (interactive.spd * interactive.dir)
 IF interactive.x < 20 OR interactive.x > 300 THEN interactive.dir = interactive.dir * -1
 IF DQBkey(KEYENTER) THEN
   IF (interactive.x > interactive.position - (interactive.wide / 2) AND interactive.x < interactive.position + (interactive.wide / 2)) THEN
    interactive.power = interactive.power + 15 + INT(RND * 5) + 1
   ELSE
    interactive.power = interactive.power - 2
   END IF
 END IF
 IF (INT(RND * 100) + 1) > 75 THEN interactive.power = interactive.power - interactive.pass
 IF interactive.power > 500 THEN interactive.power = 500
 IF interactive.power < 0 THEN interactive.active = 0: special = 5
 IF t MOD 100 = 0 THEN interactive.position = sys.randoms(120, 180)
END IF
END SUB

SUB game.maneuver
IF maneuver = 2 THEN
 outtahere = 0
 t = 0
 DO
  sys.Maneuver2
 LOOP WHILE outtahere = 0
 game.complete player.mode
END IF

END SUB

SUB game.msgs
IF msgactive = 1 THEN
     IF msgcnt = 50 THEN
          msgcnt = 0: msgactive = 0
          msggat = 0: msgspd = 0: msgper = 0: msgeva = 0: msgabs = 0: msgmiss = 0
     END IF
     msgcnt = msgcnt + 1
     IF msgmiss = 1 THEN mes = 1
     IF msggat = 1 THEN mes = 243
     IF msgspd = 1 THEN mes = 485
     IF msgper = 1 THEN mes = 727
     IF msgeva = 1 THEN mes = 969
     IF msgabs = 1 THEN mes = 1211
     DQBput 1, ship.px, ship.py - 5, VARSEG(messes(1)), VARPTR(messes(mes))
END IF
END SUB

SUB game.plotarmy

FOR i = 1 TO scene.curene
     IF army(i).py < 350 THEN
          DQBput 1, army(i).px, army(i).py, VARSEG(aliships(1)), VARPTR(aliships((army(i).face - 1) * 514 + 1))
     END IF
NEXT

END SUB

SUB game.plotship
DQBput 1, ship.px, ship.py, VARSEG(ships(1)), VARPTR(ships(1))
END SUB

SUB game.setspecial (mission, cut, scrn)
sys.macros cut, scrn

END SUB

SUB game.shoot
FOR i = 1 TO player.chan
     IF ss(i).act = 1 THEN
          SELECT CASE ship.weapon
          CASE 1, 2, 3
               IF t MOD (8 - (ship.weapon * 2)) = 1 THEN
                    ss(i).spd = ss(i).spd + 1
               END IF
               ss(i).px = ss(i).px + ss(i).spd
          CASE 4, 5, 6, 7, 8, 9
               ss(i).px = ss(i).px + ss(i).spd
          END SELECT
          DQBput 1, ss(i).px, ss(i).py, VARSEG(weaps(1)), VARPTR(weaps((ship.weapon - 1) * 74 + 1))
          IF game.chkblast(ss(i).px, ss(i).py) THEN
               ss(i).act = 0
          END IF
          IF ss(i).px > 320 THEN
               player.shots = player.shots + 1
               ss(i).act = 0
               ss(i).px = 0
          END IF
     END IF
NEXT
END SUB

SUB game.spawn (i)
     army(i).px = INT(RND * 320) + 1
     army(i).px = army(i).px + 500
     army(i).py = INT(RND * 165) + 1
     army(i).dir = 3   'INT(RND * 3) + 1
     army(i).spd = INT(RND * 4) + 3 + scene.enespd
     army(i).shooting = 0
     army(i).sx = 320
     army(i).sy = 0
     army(i).Value = 100
     army(i).ai = INT(RND * 2) + 1
     army(i).face = INT(RND * scene.maxene) + 1
END SUB

SUB gauntlet

'Get Keys
game.getkeys

'Alien AI
game.chkali
game.alienmove

'Erase old things
DQBclearLayer 1

'Draw new things
env.draw       'Stars
game.shoot     'Weapons
game.alienshoot'Alien Weaps
game.plotarmy  'Alien Ships
game.plotship  'Ship
env.fore       'Foreground
game.msgs      'Mensajitos
env.menu       'Menu

'IF t MOD 2 = 0 THEN
'DQBwait (1)
DQBcopyLayer 1, VIDEO

'Checks
game.chkupgrade

'Regen
t = t + 1
IF t MOD (30 - player.Sreg) * 30 = 0 AND player.energy < 100 THEN player.energy = player.energy + 1
IF t = 10000 THEN t = 0

END SUB

SUB init.ClearVars

special = 0
maneuver = 0
screens = 0

FOR i = 1 TO 5
     FOR j = 1 TO 5
          cuts(i, j).s1 = ""
          cuts(i, j).s2 = ""
          cuts(i, j).s3 = ""
          cuts(i, j).who = ""
          cuts(i, j).portrait = 99
     NEXT
     cutchain(i).done = 0
     cutchain(i).active = 0
NEXT
  
FOR i = 1 TO 5
 FOR j = 1 TO 6
  FOR k = 1 TO 10
   macros(i, j, k) = ""
  NEXT
 NEXT
NEXT

FOR i = 1 TO 10
     alienst(i).act = 0
NEXT

FOR i = 1 TO player.chan
 ss(i).act = 0
 ss(i).px = -30
 ss(i).py = -30
NEXT
         
msgcnt = 0: msgactive = 0
msggat = 0: msgspd = 0: msgper = 0: msgeva = 0: msgabs = 0: msgmiss = 0

player.hits = 0
lys = 0
t = 0
ct = 0
END SUB

SUB init.compose
DQBclearLayer 1
DQBput 1, 0, 0, VARSEG(ships(1)), VARPTR(ships(chasis))
DQBput 1, 0, 0, VARSEG(faces(1)), VARPTR(faces(face))
DQBget 1, 0, 0, 31, 31, VARSEG(ships(1)), VARPTR(ships(1))
DQBclearLayer 1
END SUB

SUB init.Continue
DQBclearLayer 0
a$ = sys.inp$(a$, 50, 50)
END SUB

SUB init.Create
init.Pilot
init.Ship
init.Weapon
init.compose
END SUB

SUB init.Cuts (which)
cont = 0
SELECT CASE lang$
CASE "English"
 OPEN path$ + "cuts.szs" FOR INPUT AS #1
CASE "Espanol"
 OPEN path$ + "ecuts.szs" FOR INPUT AS #1
END SELECT

DO
 LINE INPUT #1, check$
 IF check$ = "&" THEN cont = cont + 1
LOOP UNTIL cont = which

FOR i = 1 TO 5
     INPUT #1, cutchain(i).scrno
     FOR j = 1 TO cutchain(i).scrno
          INPUT #1, cuts(i, j).time, cuts(i, j).portrait, cuts(i, j).who
          LINE INPUT #1, cuts(i, j).s1
          LINE INPUT #1, cuts(i, j).s2
          LINE INPUT #1, cuts(i, j).s3
     NEXT
NEXT
CLOSE #1

END SUB

SUB init.Gauntlet
scene.curene = 1
envirom.starsq = 10
envirom.lstarsq = 10
envirom.stonesq = 3
ship.spd = 3 + GameSpeed
shootspeed = 8 + GameSpeed
ship.px = 30
ship.py = 80
player.energy = 100
player.chan = 2
scene.maxene = 9
player.mode = 0
envirom.fore = 0
envirom.back1 = 1
envirom.back2 = 1
player.visto = 1

sys.initthings envirom.starsq, envirom.starsq, envirom.stonesq
END SUB

SUB init.Level1 (which)

player.mode = which
player.visto = 1
cont = 0
OPEN path$ + "mission.szs" FOR INPUT AS #1
DO
 LINE INPUT #1, check$
 IF check$ = "&" THEN cont = cont + 1
LOOP UNTIL cont = which

INPUT #1, piloto, nave, ship.weapon, player.chan, player.energy, player.divine
INPUT #1, ship.ammo, ship.px, ship.py, envirom.fore, envirom.back1, envirom.back2
INPUT #1, envirom.starsq, envirom.lstarsq, envirom.stonesq
INPUT #1, scene.maxene, scene.curene, shootspeed, scene.enespd
FOR i = 1 TO 5
INPUT #1, cutchain(i).scrno, cutchain(i).light
NEXT
CLOSE #1

'Pilot
OPEN path$ + "pilots.szs" FOR INPUT AS #1
cont = 0
DO
 LINE INPUT #1, check$
 IF check$ = "&" THEN cont = cont + 1
LOOP UNTIL cont = piloto
INPUT #1, player.gat, player.spd, player.per, player.eva, player.abr, verb1$, verb2$, job$, hehehe$, birth$, dir, which, face
CLOSE #1

OPEN path$ + "ships.szs" FOR INPUT AS #1
cont = 0
DO
 LINE INPUT #1, check$
 IF check$ = "&" THEN cont = cont + 1
LOOP UNTIL cont = nave
INPUT #1, player.Sshi, player.Shspd, player.Sreg, player.Svspd, player.Sgat, verb1$, verb2$, hehehe$, dir, chasis
CLOSE #1

sys.initthings envirom.starsq, envirom.lstarsq, envirom.stonesq
init.compose
init.spawn
player.divine = 0
END SUB

SUB init.Macros (which%)
cont = 0
OPEN path$ + "macros.szs" FOR INPUT AS #1

DO
      LINE INPUT #1, check$
 IF check$ = "&" THEN cont = cont + 1
LOOP UNTIL cont = which

FOR i = 1 TO 5
     LINE INPUT #1, a$
     FOR j = 1 TO 6
          LINE INPUT #1, a$
          me = VAL(RIGHT$(a$, 1))
          FOR k = 1 TO me
           LINE INPUT #1, dd$
           macros(i, j, k) = LTRIM$(dd$)
          NEXT
     NEXT
NEXT
CLOSE #1
 
END SUB

SUB init.NewGauntlet

init.Create
init.Gauntlet
pre.GauntletIntro
init.spawn
ship.ammo = 1

IF DQBloadFont(path$ + "res\digits.fnt") THEN DQBsetBIOSfont
    
DO
     IF sto = 0 THEN t = t + 1
     IF t <= 128 THEN
          DQBfadeStepIn palet
     ELSE
          sto = 1
     END IF
     gauntlet
LOOP WHILE sto = 0

DO
gauntlet
LOOP

END SUB

SUB init.NewHistory (which%)

init.ClearVars
pre.IntroHistory which
pre.scene1 which
init.Macros which
init.Cuts which
init.Level1 which
IF DQBloadFont(path$ + "res\digits.fnt") THEN DQBsetBIOSfont
DQBfadeTo 0, 0, 0

IF player.mode = 1 THEN
 sys.Maneuver1
 DQBfadeIn palet
 DO: sys.Maneuver1: LOOP WHILE outtahere = 0
ELSE
 DQBfadeIn palet
END IF
t = 0
DO: game: LOOP

END SUB

SUB init.Pilot
DO
backagain = 0
DQBfadeTo 0, 0, 0
DQBclearLayer 0
DQBput VIDEO, 115, 10, VARSEG(logos(1)), VARPTR(logos(6292))
SELECT CASE lang$
CASE "English"
 DQBprint 0, "Choose your pilot", CENTERED, 70, 15
CASE "Espanol"
 DQBprint 0, "Elije tu piloto", CENTERED, 70, 15
END SELECT


DQBput 0, 144, 90, VARSEG(face(1)), VARPTR(face(0 * 514 + 1))
DQBput 0, 107, 90, VARSEG(face(1)), VARPTR(face(1 * 514 + 1))
DQBput 0, 181, 90, VARSEG(face(1)), VARPTR(face(2 * 514 + 1))
DQBput 0, 144, 127, VARSEG(face(1)), VARPTR(face(3 * 514 + 1))
DQBput 0, 107, 127, VARSEG(face(1)), VARPTR(face(4 * 514 + 1))
DQBput 0, 181, 127, VARSEG(face(1)), VARPTR(face(5 * 514 + 1))
DQBput 0, 144, 164, VARSEG(face(1)), VARPTR(face(8 * 514 + 1))
DQBput 0, 181, 164, VARSEG(face(1)), VARPTR(face(6 * 514 + 1))
DQBput 0, 107, 164, VARSEG(face(1)), VARPTR(face(7 * 514 + 1))

DQBfadeIn palet
player.pilot = sys.Menu2(105, 89, 36, 36, 3, 3)
'----

DQBfadeTo 0, 0, 0
DQBclearLayer 0
SELECT CASE lang$
CASE "English"
 OPEN path$ + "pilots.szs" FOR INPUT AS #1
CASE "Espanol"
 OPEN path$ + "epilots.szs" FOR INPUT AS #1
END SELECT
cont = 0
DO
 LINE INPUT #1, check$
 IF check$ = "&" THEN cont = cont + 1
LOOP UNTIL cont = player.pilot

INPUT #1, gat, spd, per, eva, abr, verb1$, verb2$, job$, hehehe$, birth$, dir, which, face
CLOSE #1
sys.color 3
SELECT CASE lang$
CASE "English"
 DQBprint 0, "PILOT FILE", CENTERED, 10, 15
 DQBput 0, 20, 30, VARSEG(face(1)), VARPTR(face((which - 1) * 514 + 1))
 DQBprint 0, "Yes", CENTERED, 180, 9
 DQBprint 0, "No", CENTERED, 190, 9
 sys.color 1
 DQBprint 0, "Name: " + hehehe$, 60, 30, 12
 DQBprint 0, "Job: " + job$, 60, 40, 9
 DQBprint 0, "Birthplace: " + birth$, 60, 50, 9
 DQBprint 0, verb1$, 10, 70, 9
 DQBprint 0, verb2$, 10, 80, 9
 DQBprint 0, "Speed:", 80, 100, 9
 DQBprint 0, "Perception:", 80, 110, 9
 DQBprint 0, "Gattling:", 80, 120, 9
 DQBprint 0, "Evade:", 80, 130, 9
 DQBprint 0, "Absorb:", 80, 140, 9
CASE "Espanol"
 DQBprint 0, "ARCHIVO DEL PILOTO", CENTERED, 10, 15
 DQBput 0, 20, 30, VARSEG(face(1)), VARPTR(face((which - 1) * 514 + 1))
 DQBprint 0, "Si", CENTERED, 180, 9
 DQBprint 0, "No", CENTERED, 190, 9
 sys.color 1
 DQBprint 0, "Nombre: " + hehehe$, 60, 30, 12
 DQBprint 0, "Ocupacion: " + job$, 60, 40, 9
 DQBprint 0, "Nacimiento: " + birth$, 60, 50, 9
 DQBprint 0, verb1$, 10, 70, 9
 DQBprint 0, verb2$, 10, 80, 9
 DQBprint 0, "Velocidad:", 80, 100, 9
 DQBprint 0, "Percepcion:", 80, 110, 9
 DQBprint 0, "Disparo:", 80, 120, 9
 DQBprint 0, "Evasion:", 80, 130, 9
 DQBprint 0, "Absorcion:", 80, 140, 9
END SELECT
          sys.color 2
          DQBprint 0, STR$(spd), 150, 100, 9
          DQBprint 0, STR$(per), 150, 110, 9
          DQBprint 0, STR$(gat), 150, 120, 9
          DQBprint 0, STR$(eva), 150, 130, 9
          DQBprint 0, STR$(abr), 150, 140, 9
          DQBput 0, 25, 105, VARSEG(dudes(1)), VARPTR(dudes(dir))

DQBfadeIn palet
IF sys.Menu(120, 178, 80, 10, 2, po) = 2 THEN backagain = 1

LOOP WHILE backagain = 1
player.gat = gat
player.spd = spd
player.per = per
player.eva = eva
player.abr = abr

END SUB

SUB init.Ship
DO
 backagain = 0
 DQBfadeTo 0, 0, 0
 DQBclearLayer 0
 DQBput VIDEO, 115, 10, VARSEG(logos(1)), VARPTR(logos(6292))
 SELECT CASE lang$
 CASE "English"
  DQBprint 0, "Choose your Ship", CENTERED, 70, 15
 CASE "Espanol"
  DQBprint 0, "Elije tu nave", CENTERED, 70, 15
 END SELECT
 DQBput 0, 95, 90, VARSEG(planes(1)), VARPTR(planes(0 * 1202 + 1))
 DQBput 0, 165, 90, VARSEG(planes(1)), VARPTR(planes(1 * 1202 + 1))
 DQBput 0, 95, 140, VARSEG(planes(1)), VARPTR(planes(2 * 1202 + 1))
 DQBput 0, 165, 140, VARSEG(planes(1)), VARPTR(planes(3 * 1202 + 1))
 DQBfadeIn palet
 player.chasis = sys.Menu2(90, 85, 70, 50, 2, 2)
 '----
 DQBfadeTo 0, 0, 0
 DQBclearLayer 0
 SELECT CASE lang$
 CASE "English"
  OPEN path$ + "ships.szs" FOR INPUT AS #1
 CASE "Espanol"
  OPEN path$ + "eships.szs" FOR INPUT AS #1
 END SELECT
 cont = 0
 DO
  LINE INPUT #1, check$
  IF check$ = "&" THEN cont = cont + 1
 LOOP UNTIL cont = player.chasis
 INPUT #1, escudo, hvelo, regenera, vvelo, repeti, verb1$, verb2$, hehehe$, dir, chasis
 CLOSE #1
 sys.color 3
 SELECT CASE lang$
 CASE "English"
  DQBprint 0, "Ship File", CENTERED, 30, 15
  DQBprint 0, "Yes", CENTERED, 180, 9
  DQBprint 0, "No", CENTERED, 190, 9
  sys.color 1
  DQBprint 0, "Horizontal Speed:", 100, 90, 9
  DQBprint 0, "Vertical Speed:", 100, 100, 9
  DQBprint 0, "Shield Force:", 100, 110, 9
  DQBprint 0, "Shield Regen R.:", 100, 120, 9
  DQBprint 0, "Repetition Rate:", 100, 130, 9
 CASE "Espanol"
  DQBprint 0, "Archivo de la Nave", CENTERED, 30, 15
  DQBprint 0, "Si", CENTERED, 180, 9
  DQBprint 0, "No", CENTERED, 190, 9
  sys.color 1
  DQBprint 0, "Velocidad Horizontal:", 100, 90, 9
  DQBprint 0, "Velocidad Vertical:", 100, 100, 9
  DQBprint 0, "Fuerza del Escudo:", 100, 110, 9
  DQBprint 0, "Tasa de Regeneracion:", 100, 120, 9
  DQBprint 0, "Tasa de Repeticion:", 100, 130, 9
 END SELECT
 sys.color 2
 DQBprint 0, hehehe$, 20, 60, 12
 DQBprint 0, verb1$, 105, 60, 9
 DQBprint 0, verb2$, 105, 70, 9
 DQBprint 0, STR$(hvelo), 210, 90, 9
 DQBprint 0, STR$(vvelo), 210, 100, 9
 DQBprint 0, STR$(escudo), 210, 110, 9
 DQBprint 0, STR$(regenera), 210, 120, 9
 DQBprint 0, STR$(repeti), 210, 130, 9
 DQBput 0, 25, 90, VARSEG(logos(1)), VARPTR(logos(dir))
 DQBfadeIn palet
 IF sys.Menu(120, 178, 80, 10, 2, po) = 2 THEN backagain = 1
LOOP WHILE backagain = 1

player.Sshi = escudo
player.Shspd = hvelo
player.Svspd = vvelo
player.Sreg = regenera
player.Sgat = repeti
DQBfadeTo 0, 0, 0
END SUB

SUB init.spawn
FOR i = 1 TO scene.maxene
     game.spawn i
NEXT
END SUB

SUB init.Weapon
DO
backagain = 0
DQBfadeTo 0, 0, 0
DQBclearLayer 0
DQBput VIDEO, 115, 10, VARSEG(logos(1)), VARPTR(logos(6292))
DQBprint 0, "Choose your Technology", CENTERED, 70, 15

DQBput 0, 110, 110, VARSEG(weps(1)), VARPTR(weps(0 * 302 + 1))
DQBput 0, 165, 110, VARSEG(weps(1)), VARPTR(weps(2 * 302 + 1))
DQBput 0, 110, 160, VARSEG(weps(1)), VARPTR(weps(1 * 302 + 1))
'DQBput 0, 165, 140, VARSEG(weps(1)), VARPTR(weps(0 * 302 + 1))

DQBfadeIn palet
player.sweap = sys.Menu2(110, 88, 45, 48, 2, 2)
'----
DQBfadeTo 0, 0, 0
DQBclearLayer 0
sys.color 3
SELECT CASE lang$
CASE "English"
DQBprint 0, "Weapon Technology File", CENTERED, 30, 15

DQBprint 0, "Yes", CENTERED, 180, 9
DQBprint 0, "No", CENTERED, 190, 9

SELECT CASE player.sweap
CASE 1
 verb1$ = "Explosive Missiles Propelled by gas, they have"
 verb2$ = "been used by Terrans for ages."
 hehehe$ = "Hi-Speed Missiles"
 ship.weapon = 1
 player.chan = 5
CASE 2
 verb1$ = "This Weapon shots lit gases that melt away almost"
 verb2$ = "everything, prefered by expert mercenaries."
 hehehe$ = "Fire Shots"
 ship.weapon = 7
 player.chan = 7
CASE 3
 verb1$ = "These devices are able to transform any kind of"
 verb2$ = "gas into solid projectiles, called Ice Balls."
 hehehe$ = "Condensed Ice Balls."
 ship.weapon = 4
 player.chan = 5
CASE 4
 verb1$ = "Play History Mode to unlock this Secret Weapon..."
 verb2$ = "Boo..."
 hehehe$ = "???"
 ship.weapon = 10
 player.chan = 5
CASE 5
 verb1$ = "These Heavy Weapons are very effective, but"
 verb2$ = "they can be very dangerous too."
 hehehe$ = "Explosives"
 ship.weapon = 10
 player.chan = 5
END SELECT
CASE "Espanol"
DQBprint 0, "Archivo de Tecnologia Militar", CENTERED, 30, 15

DQBprint 0, "Si", CENTERED, 180, 9
DQBprint 0, "No", CENTERED, 190, 9

SELECT CASE player.sweap
CASE 1
 verb1$ = "Misiles Explosivos impulsados por gas, han sido"
 verb2$ = "usados por los Terrans por anos."
 hehehe$ = "Misiles de Alta Velocidad"
 ship.weapon = 1
 player.chan = 5
CASE 2
 verb1$ = "Este arma dispara tiros de gas caliente que"
 verb2$ = "derriten todo, preferida por mercenarios."
 hehehe$ = "Tiros de Fuego"
 ship.weapon = 7
 player.chan = 7
CASE 3
 verb1$ = "Estos dispositivos transforman cualquier clase"
 verb2$ = "de gas en proyectiles solidos, llamados hielos."
 hehehe$ = "Hielo Condensado."
 ship.weapon = 4
 player.chan = 5
CASE 4
 verb1$ = "Juega en Modo de Historia para escoger esta arma..."
 verb2$ = "Boo..."
 hehehe$ = "???"
 ship.weapon = 10
 player.chan = 5
CASE 5
 verb1$ = "These Heavy Weapons are very effective, but"
 verb2$ = "they can be very dangerous too."
 hehehe$ = "Explosives"
 ship.weapon = 10
 player.chan = 5
END SELECT
END SELECT

sys.color 2
DQBprint 0, hehehe$, 20, 60, 12
sys.color 1
DQBprint 0, verb1$, 30, 70, 9
DQBprint 0, verb2$, 30, 80, 9

DQBfadeIn palet

IF player.sweap = 4 THEN backagain = 1
SELECT CASE sys.Menu(120, 178, 80, 10, 2, po)
CASE 1
CASE 2
     backagain = 1
END SELECT

LOOP WHILE backagain = 1

DQBfadeTo 0, 0, 0


END SUB

SUB InitImageData (FileName$, ImageArray())
REM &DYNAMIC
IF FileName$ <> "" THEN
        FileNo = FREEFILE
        OPEN FileName$ FOR BINARY AS #FileNo
        Ints = (LOF(FileNo) - 7) \ 2
        CLOSE #FileNo
        REDIM ImageArray(1 TO Ints)
        DEF SEG = VARSEG(ImageArray(1))
        BLOAD FileName$, 0
        DEF SEG
    ELSE
        READ IntCount
        REDIM ImageArray(1 TO IntCount)
        FOR n = 1 TO IntCount
            READ x
            ImageArray(n) = x
        NEXT n
    END IF

END SUB

SUB pre.Brief1 (which)
DQBfadeTo 0, 0, 0
DQBclearLayer 0
DQBclearLayer 1
SELECT CASE lang$
CASE "English"
 OPEN path$ + "brief.szs" FOR INPUT AS #1
CASE "Espanol"
 OPEN path$ + "ebrief.szs" FOR INPUT AS #1
END SELECT
sys.Brief (which)
CLOSE #1
DQBfadeIn palet
DQBwaitKey (KEYSPACE)
END SUB

SUB pre.GauntletIntro
DQBfadeTo 0, 0, 0
DQBclearLayer 0
DQBprint 0, "GAUNTLET MODE", CENTERED, 70, 12
DQBprint 0, "Fight to the death and strive", CENTERED, 90, 9
DQBprint 0, "to become a legend in the sector!", CENTERED, 100, 9
DQBprint 0, "Level 1.... Get Ready!!!", CENTERED, 120, 15

DQBfadeIn palet
DQBwaitKey (KEYANY)
DQBfadeTo 0, 0, 0

END SUB

SUB pre.GoodBye
DQBfadeTo 0, 0, 0
DQBclearLayer 0
SELECT CASE lang$
CASE "English"
 DQBprint 0, "Now leaving ArcherFire", CENTERED, 30, 15
 sys.color 2
 DQBprint 0, "ArcherFire, 2003", CENTERED, 80, 9
 sys.color 2
 DQBprint 0, "Designed and Programmed by", CENTERED, 90, 9
 DQBprint 0, "Santiago Zapata - SZDev", CENTERED, 100, 9
 DQBprint 0, "visit http://szdev.tripod.com", CENTERED, 110, 9
 sys.color 3
 DQBprint 0, "This game is freeware, please", CENTERED, 130, 9
 DQBprint 0, "mail me with a comment to:", CENTERED, 140, 9
 DQBprint 0, "ragnar300@hotmail.com...", CENTERED, 150, 9
 DQBprint 0, "Cya!", CENTERED, 170, 9
CASE "Espanol"
 DQBprint 0, "Saliendo de ArcherFire", CENTERED, 30, 15
 sys.color 2
 DQBprint 0, "ArcherFire, 2003", CENTERED, 80, 9
 sys.color 2
 DQBprint 0, "Disenado y Programado por", CENTERED, 90, 9
 DQBprint 0, "Santiago Zapata - SZDev", CENTERED, 100, 9
 DQBprint 0, "visita http://szdev.tripod.com", CENTERED, 110, 9
 sys.color 3
 DQBprint 0, "Este juego es freeware, por favor", CENTERED, 130, 9
 DQBprint 0, "mandame un comentario a:", CENTERED, 140, 9
 DQBprint 0, "ragnar300@hotmail.com...", CENTERED, 150, 9
 DQBprint 0, "Suerte!", CENTERED, 170, 9
END SELECT
DQBfadeIn palet
DQBwaitKey KEYANY
END SUB

SUB pre.intro
DQBfadeTo 0, 0, 0
DQBput VIDEO, 145, 80, VARSEG(logos(1)), VARPTR(logos(417))

sys.fade2
DQBprint 0, "Proudly Presents", CENTERED, 100, 15
sys.fade2

DQBprint 0, "The mankind has lived in relative peace", CENTERED, 20, 15
DQBprint 0, "for 2000 years, humans have mastered", CENTERED, 30, 15
DQBprint 0, "the sciences of space and time, centuries of", CENTERED, 40, 15
DQBprint 0, "peace and joy have lightened the path for", CENTERED, 50, 15
DQBprint 0, "humans to conquer the wisdom...", CENTERED, 60, 15
DQBprint 0, "The mankind has grown from a single planet", CENTERED, 70, 15
DQBprint 0, "civilization to a hundred systems empire...", CENTERED, 80, 15
DQBprint 0, "The Terran Empire...", CENTERED, 90, 15
DQBprint 0, "peace and joy have lightened the path for", CENTERED, 100, 15
sys.fade2
DQBwait (400)
DQBput VIDEO, 115, 80, VARSEG(logos(1)), VARPTR(logos(6292))
DQBprint 0, "The Return of Chaos", CENTERED, 150, 15
sys.fade2

END SUB

SUB pre.IntroHistory (which)
SELECT CASE which
CASE 1
envirom.starsq = 5: envirom.lstarsq = 10
envirom.back1 = 1: envirom.back2 = 1
sys.color 1
sys.initthings envirom.starsq, envirom.lstarsq, 0
DQBfadeTo 0, 0, 0
DQBclearLayer 0
DQBclearLayer 1
DQBfadeIn palet
DO
     DQBclearLayer 1
     SELECT CASE lang$
      CASE "English"
      IF t < 400 THEN
      DQBprint 1, "There was a time when universe was balanced...", 20, 20, 15
      ELSEIF t < 800 THEN
       DQBprint 1, "But when life appeared, battle for power did too.", 20, 60, 15
       ELSEIF t < 1200 THEN
        DQBprint 1, "Seeking power, some races took forbidden powers.", 20, 80, 15
        ELSEIF t < 1600 THEN
        sys.color 2
        DQBprint 1, "Dark powers, that should not be...", 20, 100, 15
      END IF
      CASE "Espanol"
      IF t < 400 THEN
      DQBprint 1, "En un tiempo el universo era balance...", 20, 20, 15
      ELSEIF t < 800 THEN
       DQBprint 1, "Pero con la vida, aparecio la pelea por el poder.", 20, 60, 15
       ELSEIF t < 1200 THEN
        DQBprint 1, "Buscando poder, algunas razas llamaron fuerzas.", 20, 80, 15
        ELSEIF t < 1600 THEN
        sys.color 2
        DQBprint 1, "Fuerzas oscuras... que no deben existir..", 20, 100, 15
      END IF
     END SELECT
     env.draw
     DQBwait (1)
     DQBcopyLayer 1, VIDEO
     t = t + 1
     IF DQBkey(KEYESC) THEN
          sys.Finish
     END IF
     IF DQBkey(KEYSPACE) THEN
          t = 1999
     END IF


LOOP WHILE t < 2000
t = 0
END SELECT

END SUB

SUB pre.options
DO
DQBfadeTo 0, 0, 0
DQBclearLayer 1
DQBclearLayer 0
DQBput VIDEO, 115, 10, VARSEG(logos(1)), VARPTR(logos(6292))
sys.color 2
SELECT CASE lang$
CASE "English"
 DQBprint 0, "The Return of Chaos", CENTERED, 70, 15
 sys.color 1
 DQBprint 0, "OPTIONS", CENTERED, 90, 9
 DQBprint 0, "Language: " + lang$, CENTERED, 110, 9
 DQBprint 0, "Sound: No", CENTERED, 120, 9
 DQBprint 0, "Return to Title", CENTERED, 130, 9
 sys.color 3
 DQBprint 0, "ArcherFire, Designed and Programmed", CENTERED, 160, 9
 DQBprint 0, "by SZ, v1. 0. 0 01/2003", CENTERED, 170, 9
CASE "Espanol"
 DQBprint 0, "El Regreso del Caos", CENTERED, 70, 15
 sys.color 1
 DQBprint 0, "OPCIONES", CENTERED, 90, 9
 DQBprint 0, "Lenguaje: " + lang$, CENTERED, 110, 9
 DQBprint 0, "Sonido: No", CENTERED, 120, 9
 DQBprint 0, "Volver al Titulo", CENTERED, 130, 9
 sys.color 3
 DQBprint 0, "ArcherFire, Disenado y Programado", CENTERED, 160, 9
 DQBprint 0, "por SZ, v1. 0. 0 01/2003", CENTERED, 170, 9
END SELECT
sys.color 2
DQBfadeIn palet
SELECT CASE sys.Menu(75, 108, 160, 10, 3, po)
CASE 1
     IF lang$ = "English" THEN lang$ = "Espanol"
CASE 3
     pre.title
END SELECT

DQBwaitKey (KEYANY)
LOOP
END SUB

SUB pre.scene1 (which)

sys.color 1
a = DQBloadFont(path$ + "\res\archie.fnt")

SELECT CASE lang$
CASE "English"
 OPEN path$ + "scripts.szs" FOR INPUT AS #1
CASE "Espanol"
 OPEN path$ + "escripts.szs" FOR INPUT AS #1
END SELECT
cont = 0
DO
 LINE INPUT #1, check$
 IF LEFT$(check$, 1) = "&" THEN cont = cont + 1
LOOP UNTIL cont = which
sys.Script
CLOSE #1
pre.Brief1 which
END SUB

SUB pre.title

DQBfadeTo 0, 0, 0
DQBclearLayer 0
DQBput VIDEO, 115, 10, VARSEG(logos(1)), VARPTR(logos(6292))
sys.color 2

SELECT CASE lang$
CASE "English"
 DQBprint 0, "The Return of Chaos", CENTERED, 70, 15
 sys.color 1
 DQBprint 0, "History Mode", CENTERED, 90, 9
 DQBprint 0, "Arcade Mode", CENTERED, 100, 9
 DQBprint 0, "Continue", CENTERED, 110, 9
 DQBprint 0, "Options", CENTERED, 120, 9
 DQBprint 0, "Quit", CENTERED, 130, 9
 sys.color 3
 DQBprint 0, "ArcherFire, Designed and Programmed", CENTERED, 160, 9
 DQBprint 0, "by SZ, v1. 0. 0 01/2003", CENTERED, 170, 9
CASE "Espanol"
 DQBprint 0, "El Regreso del Caos", CENTERED, 70, 15
 sys.color 1
 DQBprint 0, "Modo Historia", CENTERED, 90, 9
 DQBprint 0, "Modo Arcade", CENTERED, 100, 9
 DQBprint 0, "Continuar", CENTERED, 110, 9
 DQBprint 0, "Opciones", CENTERED, 120, 9
 DQBprint 0, "Salir", CENTERED, 130, 9
 sys.color 3
 DQBprint 0, "ArcherFire, Disenado y Programado", CENTERED, 160, 9
 DQBprint 0, "por SZ, v1. 0. 0 01/2003", CENTERED, 170, 9
END SELECT
sys.color 2
DQBfadeIn palet
SELECT CASE sys.Menu(90, 88, 140, 10, 5, po)
CASE 1
     init.NewHistory 1
CASE 2
     init.NewGauntlet
CASE 3
     init.Continue
CASE 4
     pre.options
CASE 5
     pre.GoodBye
     sys.Finish
     END
END SELECT

DQBwaitKey (KEYANY)
END SUB

SUB sys.Brief (which)

FOR i = 1 TO which
LINE INPUT #1, m$
LINE INPUT #1, mname$
LINE INPUT #1, t1$
LINE INPUT #1, t2$
LINE INPUT #1, t3$
LINE INPUT #1, t4$
LINE INPUT #1, t5$
LINE INPUT #1, o1$
LINE INPUT #1, o2$
LINE INPUT #1, o3$
LINE INPUT #1, o4$
LINE INPUT #1, o5$
NEXT

sys.color 3
DQBprint 0, m$, 20, 20, 12
DQBprint 0, "Press Space.", 200, 140, 15
sys.color 2
DQBprint 0, mname$, 60, 30, 15
DQBprint 0, "Objetives:", 20, 110, 12
sys.color 1
DQBprint 0, t1$, 20, 50, 9
DQBprint 0, t2$, 20, 60, 9
DQBprint 0, t3$, 20, 70, 9
DQBprint 0, t4$, 20, 80, 9
DQBprint 0, t5$, 20, 90, 9
DQBprint 0, o1$, 20, 120, 15
DQBprint 0, o2$, 20, 130, 15
DQBprint 0, o3$, 20, 140, 15
DQBprint 0, o4$, 20, 150, 15
DQBprint 0, o5$, 20, 160, 15

END SUB

SUB sys.color (i)
DQBsetFontTexture VARSEG(texture(1)), VARPTR(texture((i - 1) * 34 + 1))
END SUB

SUB sys.Comp (which%)

FOR i = 1 TO which
LINE INPUT #1, m$
LINE INPUT #1, mname$
LINE INPUT #1, t1$
LINE INPUT #1, t2$
LINE INPUT #1, t3$
LINE INPUT #1, t4$
LINE INPUT #1, pass$

NEXT

sys.color 3
DQBprint 0, m$, 20, 20, 12
DQBprint 0, "Press Space.", 200, 140, 15
sys.color 2
DQBprint 0, mname$, 60, 30, 15
sys.color 1
DQBprint 0, t1$, 20, 50, 9
DQBprint 0, t2$, 20, 60, 9
DQBprint 0, t3$, 20, 70, 9
DQBprint 0, t4$, 20, 80, 9
DQBprint 0, "Password " + pass$, 20, 120, 15

END SUB

FUNCTION sys.currentchain
END FUNCTION

SUB sys.Dialog2 (s$, w$, w2$, w3$, piture)
     IF piture <> 99 THEN DQBput 1, 5, 10, VARSEG(face(1)), VARPTR(face((piture) * 514 + 1))
     sys.color 2
     DQBprint 1, s$, 40, 10, 12
     sys.color 1
     DQBprint 1, w$, 40, 20, 9
     DQBprint 1, w2$, 40, 30, 9
     DQBprint 1, w3$, 40, 40, 9
END SUB

SUB sys.fade2
DQBfadeIn palet
DQBwait (200)
DQBfadeTo 0, 0, 0
DQBclearLayer 0

END SUB

SUB sys.Finish
DQBremoveKeyboard
DQBclose
END
END SUB

SUB sys.InAndOut
DQBfadeIn palet
DQBfadeTo 0, 0, 0
DQBfadeTo 63, 63, 63
DQBclearLayer 0
END SUB

SUB sys.initthings (sq%, lsq%, stq%)
FOR i = 1 TO sq
     stars(i).px = INT(RND * 320) + 1
     stars(i).py = INT(RND * 195) + 1
     stars(i).spd = (INT(RND * 5) + 3) + GameSpeed
NEXT
FOR i = 1 TO lsq
     lilstars(i).px = INT(RND * 320) + 1
     lilstars(i).py = INT(RND * 195) + 1
     lilstars(i).spd = (INT(RND * 3) + 1) + GameSpeed
NEXT
FOR i = 1 TO stq
     rocks(i).px = INT(RND * 320) + 321
     rocks(i).py = INT(RND * 195) + 1
     rocks(i).spd = (INT(RND * 7) + 5) + GameSpeed
     SELECT CASE INT(RND * 3) + 1
     CASE 1
     rocks(i).face = 16
     CASE 2
     rocks(i).face = 31
     CASE 3
     rocks(i).face = 65
     END SELECT
NEXT

END SUB

FUNCTION sys.inp$ (a$, x, y)
'
buf$ = ""
DO
'DO: buf$ = CHR$(DQBasc(DQBreadKey, 0))
': LOOP WHILE buf$ = ""
FOR i = 10 TO 80
     IF DQBkey(i) THEN
          a$ = CHR$(DQBasc(i, 0))
          back$ = back$ + a$
          DQBprint 0, back$, x, y, 0
          DQBwait 8
     END IF
NEXT
LOOP WHILE a$ <> " "
sys.inp = back$

END FUNCTION

SUB sys.macro (a$)
'DQBprint 0, a$, 100, 150, 15
SELECT CASE LEFT$(a$, 5)
CASE "chenf"
 envirom.fore = VAL(RIGHT$(a$, 1))
CASE "chcue"
 scene.curene = VAL(RIGHT$(a$, 1))
CASE "spawn"
 game.spawn (VAL(RIGHT$(a$, 1)))
CASE "ifsps"
 IF special = VAL(MID$(a$, 7, 1)) THEN special = VAL(RIGHT$(a$, 1))
CASE "setma"
 maneuver = VAL(RIGHT$(a$, 1))
CASE "setsp"
 special = VAL(RIGHT$(a$, 1))
CASE "inspa"
 init.spawn
CASE "chspd"
 envirom.spd = VAL(RIGHT$(a$, 1))
CASE "chlys"
 lys = VAL(RIGHT$(a$, 1))
CASE "setcd"
 cutchain(VAL(MID$(a$, 7, 1))).done = VAL(RIGHT$(a$, 1))
CASE "setca"
 cutchain(VAL(MID$(a$, 7, 1))).active = VAL(RIGHT$(a$, 1))
CASE "chtim"
 t = VAL(RIGHT$(a$, 1))
END SELECT
END SUB

SUB sys.macros (cut, scrn)
 FOR i = 1 TO 10
  sys.macro macros(cut, scrn, i)
 NEXT
 
END SUB

SUB sys.Maneuver1

DQBclearLayer 1
env.draw       'Stars
game.plotship  'Ship
env.fore       'Foreground
SELECT CASE lang$
CASE "English"
 IF t > 50 AND t < 400 THEN
     a$ = "What kind of artifact should it be?"
     b$ = "My father doesn't call me too often,"
     c$ = "I has to be really strange..."
     sys.Dialog2 "Juran", a$, b$, c$, 0
 END IF
 IF t > 600 AND t < 800 THEN
     a$ = "What's this!!! Lot's of Alien Ships ahead!!"
     b$ = "Heck! What will I do now!"
     c$ = ""
     sys.Dialog2 "Juran", a$, b$, c$, 0
 END IF
CASE "Espanol"
 IF t > 50 AND t < 400 THEN
     a$ = "Que clase de artefacto sera?"
     b$ = "Mi padre no me llama muy seguido, "
     c$ = "tiene que ser en verdad extrano!"
     sys.Dialog2 "Juran", a$, b$, c$, 0
 END IF
 IF t > 600 AND t < 800 THEN
     a$ = "Que es esto!!! Naves Aliens me atacan!!!"
     b$ = "Rayos! Que voy a hacer!"
     c$ = ""
     sys.Dialog2 "Juran", a$, b$, c$, 0
 END IF
END SELECT
DQBwait (1)
DQBcopyLayer 1, VIDEO
t = t + 1
IF DQBkey(KEYESC) THEN
    sys.Finish
END IF

IF t = 900 THEN
     outtahere = 1
END IF
END SUB

SUB sys.Maneuver2
  DQBclearLayer 1
     env.draw
     game.plotship
     game.plotarmy
     game.alienmove
     game.shoot
     game.chkspecial
     env.fore
     DQBwait (1)
     DQBcopyLayer 1, VIDEO
     IF t > 50 AND t < 200 THEN
      IF ship.py > 100 THEN ship.py = ship.py - 1
      IF ship.py < 100 THEN ship.py = ship.py + 1
     END IF
     IF t > 250 AND t < 350 THEN
      ship.px = ship.px + 5
     END IF
     t = t + 1
     IF DQBkey(KEYESC) THEN
          sys.Finish
     END IF

     IF t = 400 THEN
      outtahere = 1
     END IF

END SUB

FUNCTION sys.Menu (x1, y1, wid, lenght, num, po)
DQBellipse 0, x1 + (wid / 2), y1 + (lenght / 2), wid / 2, lenght / 2, 12
po = 1
DO
 IF DQBkey(KEYENTER) THEN
  sys.Menu = po
  b = 1
 END IF
 IF DQBkey(KEYUP) THEN
  IF po > 1 THEN
   DQBellipse 0, x1 + (wid / 2), y1 + ((po - 1) * lenght) + (lenght / 2), wid / 2, lenght / 2, 16
   po = po - 1
   DQBellipse 0, x1 + (wid / 2), y1 + ((po - 1) * lenght) + (lenght / 2), wid / 2, lenght / 2, 12
   DQBwait (10)
  END IF
 END IF
 IF DQBkey(KEYDOWN) THEN
  IF po < num THEN
   DQBellipse 0, x1 + (wid / 2), y1 + ((po - 1) * lenght) + (lenght / 2), wid / 2, lenght / 2, 16
   po = po + 1
   DQBellipse 0, x1 + (wid / 2), y1 + ((po - 1) * lenght) + (lenght / 2), wid / 2, lenght / 2, 12
   DQBwait (10)
  END IF
 END IF
 IF DQBkey(KEYESC) THEN
     sys.Finish
 END IF

LOOP WHILE b = 0
END FUNCTION

FUNCTION sys.Menu2 (px, py, wide, lng, hsqr, vsqr)
DQBbox 0, px, py, px + wide, py + lng, 12
xpo = 1
ypo = 1
DO
 IF DQBkey(KEYENTER) THEN
  sys.Menu2 = ((ypo - 1) * hsqr) + xpo
  b = 1
 END IF
 IF DQBkey(KEYUP) THEN
  IF ypo > 1 THEN
   DQBbox 0, px + ((xpo - 1) * wide), py + ((ypo - 1) * lng), px + ((xpo - 1) * wide) + wide, py + ((ypo - 1) * lng) + lng, 16
   ypo = ypo - 1
   DQBbox 0, px + ((xpo - 1) * wide), py + ((ypo - 1) * lng), px + ((xpo - 1) * wide) + wide, py + ((ypo - 1) * lng) + lng, 12
   DQBwait (10)
  END IF
 END IF
 IF DQBkey(KEYDOWN) THEN
  IF ypo < vsqr THEN
   DQBbox 0, px + ((xpo - 1) * wide), py + ((ypo - 1) * lng), px + ((xpo - 1) * wide) + wide, py + ((ypo - 1) * lng) + lng, 16
   ypo = ypo + 1
   DQBbox 0, px + ((xpo - 1) * wide), py + ((ypo - 1) * lng), px + ((xpo - 1) * wide) + wide, py + ((ypo - 1) * lng) + lng, 12
   DQBwait (10)
  END IF
 END IF
 IF DQBkey(KEYLEFT) THEN
  IF xpo > 1 THEN
   DQBbox 0, px + ((xpo - 1) * wide), py + ((ypo - 1) * lng), px + ((xpo - 1) * wide) + wide, py + ((ypo - 1) * lng) + lng, 16
   xpo = xpo - 1
   DQBbox 0, px + ((xpo - 1) * wide), py + ((ypo - 1) * lng), px + ((xpo - 1) * wide) + wide, py + ((ypo - 1) * lng) + lng, 12
   DQBwait (10)
  END IF
 END IF
 IF DQBkey(KEYRIGHT) THEN
  IF xpo < hsqr THEN
   DQBbox 0, px + ((xpo - 1) * wide), py + ((ypo - 1) * lng), px + ((xpo - 1) * wide) + wide, py + ((ypo - 1) * lng) + lng, 16
   xpo = xpo + 1
   DQBbox 0, px + ((xpo - 1) * wide), py + ((ypo - 1) * lng), px + ((xpo - 1) * wide) + wide, py + ((ypo - 1) * lng) + lng, 12
   DQBwait (10)
  END IF
 END IF
 IF DQBkey(KEYESC) THEN
     sys.Finish
 END IF

LOOP WHILE b = 0

END FUNCTION

FUNCTION sys.randoms (min, max)
 DO
 a = INT(RND * max) + 1
 LOOP WHILE a < min
 sys.randoms = a
END FUNCTION

SUB sys.Script
DO: LINE INPUT #1, z$: LOOP WHILE LEFT$(z$, 1) = "%"
FOR k = 1 TO VAL(z$)
     ypos = 10
     DQBfadeTo 0, 0, 0
     DQBclearLayer 0
     DQBclearLayer 1
     DQBfadeIn palet
     DO: LINE INPUT #1, w$: LOOP WHILE LEFT$(w$, 1) = "%"
     FOR j = 1 TO VAL(w$)
          DO: INPUT #1, rt$: LOOP WHILE LEFT$(rt$, 1) = "%"
          INPUT #1, Style, param
          rt = VAL(rt$)
          SELECT CASE Style
          CASE 1
               xpos = 50
          CASE 2
               xpos = 10
          END SELECT
          IF param <> -1 THEN DQBput 0, 5, ypos, VARSEG(face(1)), VARPTR(face(param * 514 + 1))
          FOR i = 1 TO rt
               DO: LINE INPUT #1, s$: LOOP WHILE LEFT$(s$, 1) = "%"
               IF Style = 1 AND i = 1 THEN sys.color 2
               DQBprint 0, s$, xpos, ypos, 15
               ypos = ypos + 10
               sys.color 1
          NEXT
          DO: resp$ = DQBinkey$: LOOP WHILE resp$ = ""
          IF UCASE$(resp$) = "Q" THEN GOTO leave
     NEXT
NEXT
leave:
END SUB


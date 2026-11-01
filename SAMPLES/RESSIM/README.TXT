RESSIM - Exemples de projets / Sample projects
==============================================

Fichiers / Files (Fichier > Ouvrir / File > Open) :

  BALDEAGL.RSM  Crue de reference de novembre 1993 (charge par defaut).
                Reference flood of November 1993 (loaded by default).
  CRUE2X.RSM    Crue 2,5 fois plus forte : zones de crue, MaxCC(4531).
                Flood 2.5 times larger: flood zones, MaxCC(4531).
  CRUE5X.RSM    Crue extreme x5 : capacite maximale des vannes, sommet du barrage.
                Extreme flood x5: maximum gate capacity, top of dam.
  PETITE.RSM    Petite crue : le pool reste en conservation.
                Small flood: the pool stays in conservation.
  EVACUAT.RSM   Pool initial eleve (650 ft) : evacuation du stockage de crue.
                High initial pool (650 ft): flood storage evacuation.
  AVALSTRI.RSM  Regulation aval stricte (MaxCC 2500/3500, RROC 200 cfs/h).
                Strict downstream regulation (MaxCC 2500/3500, RROC 200 cfs/hr).
  PERIODE.RSM   Periode de simulation courte (27 nov 14h - 1er dec 06h).
                Short simulation period (27 Nov 14h - 1 Dec 06h).

Format (texte, une instruction par ligne ; ';' = commentaire) :
Format (text, one statement per line; ';' = comment):

  RESSIM 1                       En-tete obligatoire / required header
  TITLE texte                    Titre en francais (\e=e aigu, \g=e grave, \E=E aigu...)
  TITLE_EN text                  English title
  FACTOR f                       Multiplicateur de crue / flood multiplier (0.1 a 5)
  INITELEV e                     Elevation initiale du pool / initial pool elevation (ft)
  SIM debut fin                  Indices horaires depuis le 25 nov 00h (1 .. 252)
                                 Hourly indexes from 25 Nov 00h (1 .. 252)
  LAG sm mb bm mt lissage        Delais de propagation (h) / routing lags (hr)
  HYD i base pointe t0 tp        Hydrogramme / hydrograph : 0 Sayers, 1 Marsh Creek,
                                 2 Beech Creek, 3 Fishing Creek (cfs, heures / hours)
  ALT i jeu inclus               Alternative i : jeu d'operations, 1 = calculee
  TOPDAM e                       Sommet du barrage / top of dam (ft)
  POOL i elev stock surface      Table elevation-stockage-surface (9 lignes / rows)
  GATE i elev capacite           Capacite des vannes / gate capacity (19 lignes / rows)
  OPNAME o nom                   Nom du jeu d'operations / operation set name
  ISCNT o n / ISP o i elev debit Enveloppe de surcharge induite / induced surcharge
  ZONE o z sommet n              Zone z (0 inactive .. 5 sommet du barrage / top of dam)
  RULE o z r type valeur         Regle r de la zone z : type 0 MinWQ, 1 MaxRel,
                                 2 MaxCC, 3 Incr RROC, 4 Dec RROC, 5 IndSurch,
                                 6 Maintain Peak

Les lignes absentes gardent les valeurs par defaut.
Missing statements keep the default values.

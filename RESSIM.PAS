{ @author: Sylvain Maltais (support@gladir.com)
  @created: 2026
  @version: 1.00
  @website(https://www.gladir.com/electrc)
  @abstract(Target: Turbo Pascal 7, Free Pascal)
  @description: Clone de l'application HEC-ResSim (simulation de la gestion
                des reservoirs, des lachers d'eau et des contraintes
                d'exploitation).

  Interface graphique (VGA 640x480 par defaut, -maxscreen pour le plein
  ecran) avec menus deroulants, carte du reseau (bassin Bald Eagle),
  panneau de controle de simulation, editeur de reservoir (physique,
  zones et regles d'exploitation), rapport de decision de relache,
  graphiques, resume des resultats et journal de calcul.

  Le moteur de simulation est integre : bilan de stockage horaire du
  reservoir Sayers, zones d'exploitation (Inactive, Conservation, Controle
  des crues inferieur, Controle des crues, Pool maximal, Sommet du barrage),
  regles (debit minimal, debit maximal, controle aval MaxCC au point de
  controle Beech Ck Station, taux de changement, surcharge induite, maintien
  du pic), propagation dans le reseau avec delais et attenuation.

  Syntaxe : RESSIM [-maxscreen] [-c] [-h] [fichier.rsm]

  Options : -maxscreen  resolution maximale (Free Pascal seulement)
            -c          calcule et affiche un resume sans interface graphique
            -h, /?      aide

  La variable d'environnement LANGUAGE=EN affiche l'interface en anglais
  (francais par defaut).

  Note : developpe et teste avec Free Pascal. La syntaxe reste compatible
  avec Turbo Pascal 7, mais la taille du programme n'a pas ete verifiee
  sous Turbo Pascal (limites de 64 Ko de code et de donnees).
}

Program RESSIM;

{$IFDEF FPC}
 {$MODE TP}
 {$Q-}
 {$R-}
 {$OVERFLOWCHECKS OFF}
{$ELSE}
 {$N+,E+}
 {$M 49152,0,400000}
{$ENDIF}

Uses {$IFDEF FPC}
      DOS,Crt,PtcGraph,PtcCrt,PtcMouse
     {$ELSE}
      DOS,Crt,Graph
     {$ENDIF};

Const
 AppVer='1.00';
 MaxT=260;          { taille des series horaires }
 NT=253;            { 25 nov 1993 0000 a 5 dec 1993 1200, pas d'une heure }
 NNodes=6;          { noeuds du reseau }
 NZones=6;          { zones d'exploitation }
 MaxRules=8;        { regles par zone }
 PoolN=9;           { points de la courbe elevation-stockage }
 GateN=19;          { points de la courbe de capacite des vannes }
 ISMax=12;          { points de la courbe de surcharge induite }
 NOps=2;            { jeux d'operations }
 NAlts=2;           { alternatives }
 {$IFDEF UNIX}DirSep='/';{$ELSE}DirSep='\';{$ENDIF}   { separateur de repertoire }
 Hr2Ac=0.0826446;   { 1 cfs pendant 1 heure en acres-pieds }

 { Modules }
 modSetup=0; modNet=1; modSim=2;
 { Fenetres }
 wRes=0; wRep=1; wPlot=2; wTxt=3; NWin=4;
 { Types de regles }
 rkMin=0; rkMax=1; rkCC=2; rkIncr=3; rkDecr=4; rkInd=5; rkPeak=6;
 rkUnc=7; rkCap=8; rkHold=9;
 { Menus }
 NMenus=8;
 MaxItems=14;
 { Disposition de l'ecran }
 TitleH=14; MenuH=16; ModH=22; ToolW=26; PanelW=160; StatH=14;

Type
 TSeries=Array[0..MaxT-1] of Single;
 TRule=Record Kind:Byte; P1:Double; End;
 TZone=Record
  Top:Double;
  N:Byte;
  R:Array[0..MaxRules-1] of TRule;
 End;
 TOpSet=Record
  Name:String[22];
  Descr:String[62];
  Z:Array[0..NZones-1] of TZone;
  ISCnt:Byte;
  ISE,ISR:Array[0..ISMax-1] of Double;
 End;
 TModel=Record
  Ops:Array[0..NOps-1] of TOpSet;
  PE,PS,PA:Array[0..PoolN-1] of Double;
  GE,GT:Array[0..GateN-1] of Double;
  TopDam:Double;
 End;
 TAlt=Record
  Name:String[12];
  OpSet:Byte;
  Incl,Valid:Boolean;
 End;
 TAltArr=Array[0..NAlts-1] of TAlt;
 TH4=Array[0..3] of Double;
 TResult=Record
  Elev,Stor,Rel:TSeries;
  Flow:Array[0..NNodes-1] of TSeries;
  CumLoc:TSeries;
  Zone,RK:Array[0..MaxT-1] of Byte;
  RV:Array[0..MaxT-1] of Integer;
 End;
 TMItem=Record
  Cap:String[34];
  Sc:String[10];
  Sep,Chk,En:Boolean;
 End;
 TMenu=Record
  Title:String[14];
  N:Integer;
  X,W:Integer;
  It:Array[0..MaxItems-1] of TMItem;
 End;

Var
 Eng:Boolean;
 TmpLang:String;
 MaxScreen,ConsoleMode:Boolean;
 SW,SH:Integer;
 { Souris et clavier }
 MX,MY,MB,OMX,OMY,OMB:Integer;
 Pressed,Released,RPress,DblClick:Boolean;
 Frame,LastClk:LongInt;
 LastCX,LastCY:Integer;
 HaveKey:Boolean;
 KeyCh:Char;
 KeyExt:Byte;
 Running,Dirty:Boolean;
 { Modele }
 M,WM:TModel;
 Alts:TAltArr;
 ActAlt:Integer;
 Res:Array[0..NAlts-1] of TResult;
 InfS,MarsS,BeecS,FishS:TSeries;
 InflowFactor,InitElev:Double;
 SimStart,SimEnd:Integer;
 DispT,PlayCnt:Integer;
 Playing:Boolean;
 LogL:Array[0..47] of String[64];
 LogN:Integer;
 FileName,CurName:String;
 PrjFr,PrjEn:String[60];
 { Parametres charges depuis le fichier projet }
 LagSM,LagMB,LagBM,LagMT,SmoothBM:Integer;  { delais de propagation (heures) }
 HBase,HPeak,HT0,HTp:TH4; { hydrogrammes : Sayers, Marsh, Beech, Fishing }
 { Interface }
 Module:Integer;
 ModDrop:Boolean;
 VScale,VOX,VOY:Double;
 Tool,SelNode:Integer;
 ShowLbl,ShowGrid,ShowNames:Boolean;
 PanDrag,SliderDrag:Boolean;
 StatMsg:String;
 Menus:Array[0..NMenus-1] of TMenu;
 MenuOn:Boolean;
 MenuAct,MenuItm:Integer;
 { Fenetres }
 WOpen:Array[0..NWin-1] of Boolean;
 WX,WY,WW,WH:Array[0..NWin-1] of Integer;
 ZList:Array[0..NWin-1] of Byte;
 ZCnt,DragW,DragDX,DragDY:Integer;
 TxtTitle:String[40];
 TxtL:Array[0..79] of String[72];
 TxtN,TxtTop:Integer;
 RepTop:Integer;
 RepChg:Boolean;
 PlotNode,PlotVar:Integer;
 ResTab,ResPhy,ResOp,ResTree,ResSelZ,ResSelR,ResTbl,ResObs:Integer;

{ ------------------------------------------------------------------ }
{ Utilitaires                                                         }
{ ------------------------------------------------------------------ }

{ Convertit les marqueurs d'accents du francais (\e=e aigu, \g=e grave,
  \x=e circonflexe, \a=a grave, \q=a circonflexe, \c=c cedille, \o=o
  circonflexe, \u=u grave, \E=E aigu, \i=i circonflexe) en code CP437. }
Function Acc(S:String):String;
Var I:Integer;
    R:String;
Begin
 R:='';I:=1;
 While I<=Length(S)do Begin
  If(S[I]='\')and(I<Length(S))Then Begin
   Case S[I+1] of
    'e':R:=R+#130;
    'g':R:=R+#138;
    'x':R:=R+#136;
    'a':R:=R+#133;
    'q':R:=R+#131;
    'c':R:=R+#135;
    'o':R:=R+#147;
    'u':R:=R+#151;
    'E':R:=R+#144;
    'i':R:=R+#140;
    Else R:=R+S[I+1];
   End;
   Inc(I,2);
  End
   Else Begin R:=R+S[I];Inc(I);End;
 End;
 Acc:=R;
End;

Function Tr(Fr,En:String):String;Begin
 If Eng Then Tr:=En Else Tr:=Acc(Fr);
End;

Function IntStr(N:LongInt):String;
Var S:String;
Begin
 Str(N,S);
 IntStr:=S;
End;

Function RStr(X:Double;D:Integer):String;
Var S:String;
Begin
 Str(X:0:D,S);
 RStr:=S;
End;

Function StrUp(S:String):String;
Var I:Integer;
Begin
 For I:=1 to Length(S)do If S[I] in['a'..'z']Then S[I]:=Chr(Ord(S[I])-32);
 StrUp:=S;
End;

Function MinI(A,B:Integer):Integer;Begin
 If A<B Then MinI:=A Else MinI:=B;
End;

Function MaxI(A,B:Integer):Integer;Begin
 If A>B Then MaxI:=A Else MaxI:=B;
End;

Function Clamp(V,A,B:Integer):Integer;Begin
 If V<A Then V:=A;
 If V>B Then V:=B;
 Clamp:=V;
End;

Function PadR(S:String;N:Integer):String;Begin
 While Length(S)<N do S:=S+' ';
 PadR:=Copy(S,1,N);
End;

Function PadL(S:String;N:Integer):String;Begin
 While Length(S)<N do S:=' '+S;
 PadL:=S;
End;

Function Inside(X,Y,X1,Y1,X2,Y2:Integer):Boolean;Begin
 Inside:=(X>=X1)and(X<=X2)and(Y>=Y1)and(Y<=Y2);
End;

Function ValStr(S:String;Var V:Double):Boolean;
Var Code:Integer;
    I:Integer;
Begin
 For I:=1 to Length(S)do If S[I]=','Then S[I]:='.';
 While(Length(S)>0)and(S[1]=' ')do Delete(S,1,1);
 While(Length(S)>0)and(S[Length(S)]=' ')do Delete(S,Length(S),1);
 Val(S,V,Code);
 ValStr:=(Code=0)and(S<>'');
End;

Function FlowLbl(V:Double):String;Begin
 If V>=1000 Then FlowLbl:=RStr(V/1000,1)+'K'
            Else FlowLbl:=IntStr(Round(V));
End;

{ Dates : l'indice 0 correspond au 25 novembre 1993 a 00:00 }
Function DIM(Mo:Integer):Integer;Begin
 Case Mo of
  4,6,9,11:DIM:=30;
  2:DIM:=28;
  Else DIM:=31;
 End;
End;

Procedure IdxToDate(T:Integer;Var Y,Mo,D,H:Integer);
Var Days:Integer;
Begin
 Y:=1993;Mo:=11;D:=25;
 H:=T mod 24;
 Days:=T div 24;
 While Days>0 do Begin
  Inc(D);
  If D>DIM(Mo)Then Begin
   D:=1;Inc(Mo);
   If Mo>12 Then Begin Mo:=1;Inc(Y);End;
  End;
  Dec(Days);
 End;
End;

Function MonName(Mo:Integer):String;
Var S:String;
Begin
 If Eng Then S:=Copy('JanFebMarAprMayJunJulAugSepOctNovDec',Mo*3-2,3)
  Else
 Begin
  S:=Copy('JanFevMarAvrMaiJuiJulAouSepOctNovDec',Mo*3-2,3);
  If Mo=2 Then S[2]:=#130;
  If Mo=8 Then S[3]:=#150;
  If Mo=12 Then S[2]:=#130;
 End;
 MonName:=S;
End;

Function Pad2(N:Integer):String;
Var S:String;
Begin
 S:=IntStr(N);
 If Length(S)<2 Then S:='0'+S;
 Pad2:=S;
End;

Function FmtDT(T:Integer):String;
Var Y,Mo,D,H:Integer;
Begin
 IdxToDate(T,Y,Mo,D,H);
 FmtDT:=Pad2(D)+MonName(Mo)+IntStr(Y)+', '+Pad2(H)+':00';
End;

Function FmtDT2(T:Integer):String;
Var Y,Mo,D,H:Integer;
Begin
 IdxToDate(T,Y,Mo,D,H);
 FmtDT2:=IntStr(D)+' '+MonName(Mo)+' '+IntStr(Y)+', '+Pad2(H)+'00';
End;

Function DayHourToIdx(D,H:Integer):Integer;Begin
 If D>=25 Then DayHourToIdx:=(D-25)*24+H
          Else DayHourToIdx:=(D+5)*24+H;
End;

{ ------------------------------------------------------------------ }
{ Modele physique et operationnel                                     }
{ ------------------------------------------------------------------ }

Function Interp(Var X,Y:Array of Double;N:Integer;V:Double):Double;
Var I:Integer;
Begin
 If N<1 Then Begin Interp:=0;Exit;End;
 If V<=X[0]Then Begin Interp:=Y[0];Exit;End;
 If V>=X[N-1]Then Begin Interp:=Y[N-1];Exit;End;
 For I:=1 to N-1 do If V<=X[I]Then Begin
  If X[I]=X[I-1]Then Interp:=Y[I]
   Else Interp:=Y[I-1]+(Y[I]-Y[I-1])*(V-X[I-1])/(X[I]-X[I-1]);
  Exit;
 End;
 Interp:=Y[N-1];
End;

Procedure SetRule(Var O:TOpSet;Z,I:Integer;K:Byte;P:Double);Begin
 O.Z[Z].R[I].Kind:=K;
 O.Z[Z].R[I].P1:=P;
 If O.Z[Z].N<I+1 Then O.Z[Z].N:=I+1;
End;

Procedure InitModel;
Const
 PE:Array[0..PoolN-1] of Double=(570,590,610,620,645,662,675,680,690);
 PS:Array[0..PoolN-1] of Double=(0,4000,15000,22750,46500,66500,84700,92400,109400);
 PA:Array[0..PoolN-1] of Double=(0,400,700,850,1050,1300,1500,1600,1800);
 GT:Array[0..GateN-1] of Double=(0,500,1350,2500,3560,4460,5250,5920,6480,
                                  6980,7470,7910,8340,8730,9120,9480,9830,
                                  10160,10500);
 IE:Array[0..ISMax-1] of Double=(657,657.5,658,658.5,659,659.5,660,660.5,
                                  661,661.25,665,680);
 IR:Array[0..ISMax-1] of Double=(0,100,220,380,590,860,1230,1680,2330,2800,
                                  5000,10500);
 ZT:Array[0..NZones-1] of Double=(590,620,645,662,680,690);
Var
 I,O,Z:Integer;
Begin
 FillChar(M,SizeOf(M),0);
 For I:=0 to PoolN-1 do Begin
  M.PE[I]:=PE[I];M.PS[I]:=PS[I];M.PA[I]:=PA[I];
 End;
 For I:=0 to GateN-1 do Begin
  M.GE[I]:=590+I*5;
  M.GT[I]:=GT[I];
 End;
 M.TopDam:=690;
 For O:=0 to NOps-1 do Begin
  M.Ops[O].ISCnt:=ISMax;
  For I:=0 to ISMax-1 do Begin
   M.Ops[O].ISE[I]:=IE[I];
   M.Ops[O].ISR[I]:=IR[I];
  End;
  For Z:=0 to NZones-1 do Begin
   M.Ops[O].Z[Z].Top:=ZT[Z];
   M.Ops[O].Z[Z].N:=0;
  End;
 End;
 { Jeu 0 : Base (sans operations en aval) }
 With M.Ops[0]do Begin
  Name:='Base';
  Descr:=Tr('Sans contraintes de d\ebit en aval','No downstream flow constraints');
  SetRule(M.Ops[0],1,0,rkMin,250);
  SetRule(M.Ops[0],2,0,rkMin,250);
  SetRule(M.Ops[0],2,1,rkMax,2000);
  SetRule(M.Ops[0],3,0,rkIncr,500);
  SetRule(M.Ops[0],3,1,rkDecr,400);
  SetRule(M.Ops[0],3,2,rkInd,0);
  SetRule(M.Ops[0],3,3,rkMin,250);
  SetRule(M.Ops[0],3,4,rkMax,3000);
  SetRule(M.Ops[0],4,0,rkInd,0);
  SetRule(M.Ops[0],4,1,rkMin,250);
  SetRule(M.Ops[0],5,0,rkInd,0);
  SetRule(M.Ops[0],5,1,rkMin,250);
 End;
 { Jeu 1 : Downstream Regulation }
 With M.Ops[1]do Begin
  Name:='Downstream Regulation';
  Descr:=Tr('Repr\esentation du r\gglement de r\egulation aval',
            'Representation of the downstream regulation plan');
  SetRule(M.Ops[1],1,0,rkMin,250);
  SetRule(M.Ops[1],1,1,rkDecr,400);
  SetRule(M.Ops[1],1,2,rkIncr,500);
  SetRule(M.Ops[1],1,3,rkCC,3248);
  SetRule(M.Ops[1],2,0,rkIncr,500);
  SetRule(M.Ops[1],2,1,rkDecr,400);
  SetRule(M.Ops[1],2,2,rkMin,250);
  SetRule(M.Ops[1],2,3,rkCC,3248);
  SetRule(M.Ops[1],3,0,rkIncr,500);
  SetRule(M.Ops[1],3,1,rkDecr,400);
  SetRule(M.Ops[1],3,2,rkInd,0);
  SetRule(M.Ops[1],3,3,rkMin,250);
  SetRule(M.Ops[1],3,4,rkCC,4531);
  SetRule(M.Ops[1],4,0,rkPeak,0);
  SetRule(M.Ops[1],4,1,rkInd,0);
  SetRule(M.Ops[1],4,2,rkMin,250);
  SetRule(M.Ops[1],5,0,rkInd,0);
  SetRule(M.Ops[1],5,1,rkMin,250);
 End;
End;

Procedure InitDefaults;Begin
 InitModel;
 Alts[0].Name:='NoDSOps';Alts[0].OpSet:=0;Alts[0].Incl:=True;Alts[0].Valid:=False;
 Alts[1].Name:='WithDSOps';Alts[1].OpSet:=1;Alts[1].Incl:=True;Alts[1].Valid:=False;
 ActAlt:=1;
 InflowFactor:=1.0;
 InitElev:=610.09;
 SimStart:=62;       { 27 nov 1993, 14:00 }
 SimEnd:=NT-1;       { 5 dec 1993, 12:00 }
 DispT:=SimStart;
 LogN:=0;
 LagSM:=2;LagMB:=2;LagBM:=5;SmoothBM:=2;LagMT:=3;
 HBase[0]:=360;HPeak[0]:=9500;HT0[0]:=64;HTp[0]:=16;
 HBase[1]:=60;HPeak[1]:=2400;HT0[1]:=62;HTp[1]:=9;
 HBase[2]:=90;HPeak[2]:=3600;HT0[2]:=62;HTp[2]:=11;
 HBase[3]:=70;HPeak[3]:=2000;HT0[3]:=64;HTp[3]:=13;
 PrjFr:='';PrjEn:='';CurName:='';
End;

Function Hyd(T,T0,Tp,Base,Peak:Double):Double;
Var X:Double;
Begin
 If T<=T0 Then Begin Hyd:=Base;Exit;End;
 X:=(T-T0)/Tp;
 Hyd:=Base+(Peak-Base)*Exp(2*(Ln(X)+1-X));
End;

Procedure BuildInputs;
Var T:Integer;
Begin
 For T:=0 to MaxT-1 do Begin
  InfS[T]:=Hyd(T,HT0[0],HTp[0],HBase[0],HPeak[0])*InflowFactor+HBase[0]*(1-InflowFactor);
  MarsS[T]:=Hyd(T,HT0[1],HTp[1],HBase[1],HPeak[1])*InflowFactor+HBase[1]*(1-InflowFactor);
  BeecS[T]:=Hyd(T,HT0[2],HTp[2],HBase[2],HPeak[2])*InflowFactor+HBase[2]*(1-InflowFactor);
  FishS[T]:=Hyd(T,HT0[3],HTp[3],HBase[3],HPeak[3])*InflowFactor+HBase[3]*(1-InflowFactor);
 End;
End;

Function ZoneOf(Var O:TOpSet;E:Double):Integer;
Var Z:Integer;
Begin
 For Z:=0 to NZones-2 do If E<=O.Z[Z].Top Then Begin ZoneOf:=Z;Exit;End;
 ZoneOf:=NZones-1;
End;

Function OutCap(E:Double):Double;
Var C:Double;
Begin
 If E<=M.GE[0]Then Begin OutCap:=0;Exit;End;
 If E>M.GE[GateN-1]Then
  C:=M.GT[GateN-1]+(M.GT[GateN-1]-M.GT[GateN-2])/(M.GE[GateN-1]-M.GE[GateN-2])
    *(E-M.GE[GateN-1])*2
 Else C:=Interp(M.GE,M.GT,GateN,E);
 OutCap:=C;
End;

Function ZoneName(Z:Integer):String;Begin
 Case Z of
  0:ZoneName:=Tr('Inactive','Inactive');
  1:ZoneName:='Conservation';
  2:ZoneName:=Tr('Contr. crues inf.','Lower Flood Control');
  3:ZoneName:=Tr('Contr\ole des crues','Flood Control');
  4:ZoneName:=Tr('Pool maximal','Maximum Pool');
  Else ZoneName:=Tr('Sommet du barrage','Top of Dam');
 End;
End;

Function ZoneAbbr(Z:Integer):String;Begin
 Case Z of
  0:ZoneAbbr:='INA';
  1:ZoneAbbr:='CON';
  2:ZoneAbbr:='LFC';
  3:ZoneAbbr:='FC';
  4:ZoneAbbr:='MXP';
  Else ZoneAbbr:='TOD';
 End;
End;

Function RuleTypeName(K:Integer):String;Begin
 Case K of
  rkMin:RuleTypeName:=Tr('D\ebit minimal','Minimum release');
  rkMax:RuleTypeName:=Tr('D\ebit maximal','Maximum release');
  rkCC:RuleTypeName:=Tr('Contr\ole aval (MaxCC)','Downstream control (MaxCC)');
  rkIncr:RuleTypeName:=Tr('Taux de hausse (RROC)','Rate of increase (RROC)');
  rkDecr:RuleTypeName:=Tr('Taux de baisse (RROC)','Rate of decrease (RROC)');
  rkInd:RuleTypeName:=Tr('Surcharge induite','Induced surcharge');
  rkPeak:RuleTypeName:=Tr('Maintien du pic','Maintain peak release');
  rkUnc:RuleTypeName:=Tr('Non contr\ole','Uncontrolled');
  rkCap:RuleTypeName:=Tr('Capacit\e des vannes','Gate capacity');
  Else RuleTypeName:=Tr('Maintien du niveau','Hold top of conservation');
 End;
End;

Function RuleLabel(K:Integer;V:LongInt):String;Begin
 Case K of
  rkMin:RuleLabel:='MinWQ('+IntStr(V)+')';
  rkMax:RuleLabel:='MaxRel('+IntStr(V)+')';
  rkCC:RuleLabel:='MaxCC('+IntStr(V)+')_BeechCkSta';
  rkIncr:RuleLabel:='Incr RROC';
  rkDecr:RuleLabel:='Dec RROC';
  rkInd:RuleLabel:='IndSurch-EmergencyReg';
  rkPeak:RuleLabel:='Maintain Peak Release';
  rkUnc:RuleLabel:='Unctrl';
  rkCap:RuleLabel:='MaxCapacity';
  Else RuleLabel:='HoldTopCons';
 End;
End;

{ Decide le debit relache d'apres les regles de la zone, de la plus
  prioritaire (haut de la liste) a la moins prioritaire. }
Procedure Decide(Var O:TOpSet;Z:Integer;E,Prev,CapOut,Avail,Others,Peak,
                 Iavg:Double;Evac:Boolean;Var R:Double;Var KD:Byte;
                 Var KV:Integer);
Var
 I:Integer;
 Lo,Hi,V:Double;
 LoK,HiK:Byte;
 LoV,HiV:LongInt;
Begin
 Lo:=0;Hi:=CapOut;LoK:=rkUnc;HiK:=rkCap;LoV:=0;HiV:=Round(CapOut);
 If Avail<Hi Then Begin Hi:=Avail;HiK:=rkUnc;HiV:=0;End;
 If Hi<0 Then Hi:=0;
 For I:=0 to O.Z[Z].N-1 do With O.Z[Z].R[I]do Begin
  Case Kind of
   rkMin,rkInd,rkPeak,rkDecr:Begin
    Case Kind of
     rkMin:V:=P1;
     rkInd:V:=Interp(O.ISE,O.ISR,O.ISCnt,E);
     rkPeak:V:=Peak;
     Else Begin V:=Prev-P1;If V<0 Then V:=0;End;
    End;
    If V>Hi Then V:=Hi;
    If V>Lo Then Begin
     Lo:=V;LoK:=Kind;
     If Kind in[rkMin,rkDecr]Then LoV:=Round(P1)Else LoV:=Round(V);
    End;
   End;
   rkMax,rkCC,rkIncr:Begin
    Case Kind of
     rkMax:V:=P1;
     rkCC:Begin V:=P1-Others;If V<0 Then V:=0;End;
     Else V:=Prev+P1;
    End;
    If V<Lo Then V:=Lo;
    If V<Hi Then Begin Hi:=V;HiK:=Kind;HiV:=Round(P1);End;
   End;
  End;
 End;
 If Z=0 Then Begin R:=0;KD:=rkUnc;KV:=0;Exit;End;
 If Z>=2 Then Begin R:=Hi;KD:=HiK;KV:=Integer(HiV);End
  Else
 Begin
  R:=Lo;KD:=LoK;KV:=Integer(LoV);
  { Apres une evacuation de crue, le niveau est maintenu en haut de la
    zone de conservation : debit sortant = debit entrant }
  If Evac and(E>=O.Z[1].Top-0.25)Then Begin
   V:=Iavg;
   If V<Lo Then V:=Lo;
   If V>Hi Then V:=Hi;
   R:=V;KD:=rkHold;KV:=0;
  End;
 End;
End;

Function LagIdx(T,L:Integer):Integer;Begin
 If T-L<0 Then LagIdx:=0 Else LagIdx:=T-L;
End;

Procedure RunAlt(A:Integer);
Var
 T,Z,K,J,Last:Integer;
 S,E,Iavg,CapOut,Avail,Oth,R,Prev,Peak,InactS,Tmp:Double;
 KD:Byte;
 KV:Integer;
 Evac:Boolean;
 O:Integer;
Begin
 O:=Alts[A].OpSet;
 FillChar(Res[A],SizeOf(Res[A]),0);
 S:=Interp(M.PE,M.PS,PoolN,InitElev);
 InactS:=Interp(M.PE,M.PS,PoolN,M.Ops[O].Z[0].Top);
 Peak:=0;Prev:=InfS[0];Evac:=False;
 Last:=SimEnd;
 If Last>NT-1 Then Last:=NT-1;
 With Res[A]do Begin
  For T:=0 to NT-1 do Begin
   If T>Last Then Begin
    Elev[T]:=Elev[Last];Stor[T]:=Stor[Last];Rel[T]:=Rel[Last];
    Zone[T]:=Zone[Last];RK[T]:=RK[Last];RV[T]:=RV[Last];
   End
    Else
   Begin
    E:=Interp(M.PS,M.PE,PoolN,S);
    Elev[T]:=E;Stor[T]:=S;
    Z:=ZoneOf(M.Ops[O],E);
    Zone[T]:=Z;
    If T<SimStart Then Begin
     Rel[T]:=InfS[T];RK[T]:=rkUnc;RV[T]:=0;Prev:=InfS[T];
    End
     Else
    Begin
     If T<NT-1 Then Iavg:=(InfS[T]+InfS[T+1])/2 Else Iavg:=InfS[T];
     CapOut:=OutCap(E);
     Avail:=(S-InactS)/Hr2Ac+Iavg;
     J:=MinI(T+LagSM,NT-1);K:=MinI(T+LagSM+LagMB,NT-1);
     Oth:=MarsS[J]+BeecS[K];
     If Z>=2 Then Evac:=True;
     Decide(M.Ops[O],Z,E,Prev,CapOut,Avail,Oth,Peak,Iavg,Evac,R,KD,KV);
     Rel[T]:=R;RK[T]:=KD;RV[T]:=KV;
     If R>Peak Then Peak:=R;
     Prev:=R;
     S:=S+(Iavg-R)*Hr2Ac;
     If S<0 Then S:=0;
    End;
   End;
  End;
  { Propagation dans le reseau }
  For T:=0 to NT-1 do Begin
   Flow[0][T]:=InfS[T];
   Flow[1][T]:=Rel[T];
   Flow[2][T]:=Rel[LagIdx(T,LagSM)]+MarsS[T];
  End;
  For T:=0 to NT-1 do Flow[3][T]:=Flow[2][LagIdx(T,LagMB)]+BeecS[T];
  For T:=0 to NT-1 do Begin
   Tmp:=0;
   For J:=-SmoothBM to SmoothBM do Tmp:=Tmp+Flow[3][Clamp(T-LagBM+J,0,NT-1)];
   Flow[4][T]:=Tmp/(2*SmoothBM+1)+FishS[T];
  End;
  For T:=0 to NT-1 do Begin
   Flow[5][T]:=Flow[4][LagIdx(T,LagMT)];
   CumLoc[T]:=MarsS[LagIdx(T,LagMB)]+BeecS[T];
  End;
 End;
 Alts[A].Valid:=True;
End;

Procedure BuildLog;
Var
 A,T,Z,Chg:Integer;
 Cnt:Array[0..NZones-1] of Integer;
 MaxE,MaxR:Double;
 TmE,TmR:Integer;
Begin
 LogN:=0;
 For A:=0 to NAlts-1 do If Alts[A].Valid Then With Res[A]do Begin
  FillChar(Cnt,SizeOf(Cnt),0);
  MaxE:=0;MaxR:=0;TmE:=SimStart;TmR:=SimStart;Chg:=0;
  For T:=SimStart to SimEnd do Begin
   Inc(Cnt[Zone[T]]);
   If Elev[T]>MaxE Then Begin MaxE:=Elev[T];TmE:=T;End;
   If Rel[T]>MaxR Then Begin MaxR:=Rel[T];TmR:=T;End;
   If(T>SimStart)and((RK[T]<>RK[T-1])or(Zone[T]<>Zone[T-1]))Then Inc(Chg);
  End;
  LogL[LogN]:=Tr('Alternative ','Alternative ')+Alts[A].Name+' ('+
              M.Ops[Alts[A].OpSet].Name+')';Inc(LogN);
  LogL[LogN]:=Tr('  Pas de calcul : ','  Time steps: ')+IntStr(SimEnd-SimStart+1)+
              Tr(' (1 heure)',' (1 hour)');Inc(LogN);
  For Z:=NZones-1 downto 0 do If Cnt[Z]>0 Then Begin
   LogL[LogN]:='  '+PadR(ZoneName(Z),20)+PadL(IntStr(Cnt[Z]),4)+Tr(' h',' hr');
   Inc(LogN);
  End;
  LogL[LogN]:=Tr('  Changements de r\ggle active : ','  Active rule changes: ')+
              IntStr(Chg);Inc(LogN);
  LogL[LogN]:=Tr('  \El\evation maximale : ','  Maximum elevation: ')+
              RStr(MaxE,2)+' ft  ('+FmtDT(TmE)+')';Inc(LogN);
  LogL[LogN]:=Tr('  D\ebit rel\qche maximal : ','  Peak release: ')+
              IntStr(Round(MaxR))+' cfs  ('+FmtDT(TmR)+')';Inc(LogN);
  If MaxE>M.TopDam Then Begin
   LogL[LogN]:=Tr('  ATTENTION : d\ebordement du barrage !',
                  '  WARNING: dam overtopped!');Inc(LogN);
  End;
 End;
End;

Procedure DoCompute;
Var A:Integer;
Begin
 BuildInputs;
 For A:=0 to NAlts-1 do
  If Alts[A].Incl Then RunAlt(A)
   Else Alts[A].Valid:=False;
 BuildLog;
 If DispT<SimStart Then DispT:=SimStart;
 If DispT>SimEnd Then DispT:=SimEnd;
End;

{ ------------------------------------------------------------------ }
{ Souris et mode graphique                                            }
{ ------------------------------------------------------------------ }

Procedure InitializeGraphics;
Var
 Driver,Mode:Integer;
 ErrCode:Integer;
 {$IFDEF FPC}LoMode,HiMode:SmallInt;{$ENDIF}
Begin
 {$IFDEF FPC}
  If MaxScreen Then Begin
   Driver:=D8bit;
   GetModeRange(Driver,LoMode,HiMode);
   If HiMode>=LoMode Then Mode:=HiMode Else Mode:=m640x480;
  End
   Else
  Begin
   Driver:=VGA;
   Mode:=VGAHi;
  End;
 {$ELSE}
  Driver:=Detect;
  Mode:=VGAHi;
 {$ENDIF}
 InitGraph(Driver,Mode,'');
 ErrCode:=GraphResult;
 If ErrCode=grOk Then Begin
  SetColor(White);
  SetLineStyle(0,0,1);
  SW:=GetMaxX+1;
  SH:=GetMaxY+1;
 End
  Else
 Begin
  WriteLn(Tr('Erreur graphique : ','Graphics error: '),GraphErrorMsg(ErrCode));
  Halt(1);
 End;
End;

{$IFNDEF FPC}
 Function MouseDriverFound:Boolean;
 Var
  Regs:Registers;
 Begin
  Regs.AX:=0;
  Intr($33,Regs);
  MouseDriverFound:=Regs.AX=$FFFF;
 End;

 Procedure MouseInit;Begin
  If MouseDriverFound Then Begin End;
 End;

 Procedure ShowMouse;
 Var
  Regs:Registers;
 Begin
  Regs.AX:=$0001;
  Intr($33,Regs);
 End;

 Procedure HideMouse;
 Var
  Regs:Registers;
 Begin
  Regs.AX:=$0002;
  Intr($33,Regs);
 End;

 Procedure GetMouseState(Var X,Y,Button:LongInt);
 Var
  Regs:Registers;
 Begin
  Regs.AX:=$0003;
  Intr($33,Regs);
  Button:=Regs.BX;
  X:=Regs.CX;
  Y:=Regs.DX;
 End;
{$ELSE}
 Function MouseDriverFound:Boolean;Begin
  MouseDriverFound:=True;
 End;

 Procedure MouseInit;Begin
  PtcMouse.InitMouse;
 End;

 Procedure ShowMouse;Begin
  PtcMouse.ShowMouse;
 End;

 Procedure HideMouse;Begin
  PtcMouse.HideMouse;
 End;

 Procedure GetMouseState(Var X,Y,Button:LongInt);Begin
  PtcMouse.GetMouseState(X,Y,Button);
 End;
{$ENDIF}

{ Lecture de la souris et du clavier (une fois par image) }
Procedure PollInput;
Var LX,LY,LB:LongInt;
Begin
 Inc(Frame);
 OMX:=MX;OMY:=MY;OMB:=MB;
 GetMouseState(LX,LY,LB);
 MX:=LX;MY:=LY;MB:=LB;
 Pressed:=((MB and 1)<>0)and((OMB and 1)=0);
 Released:=((MB and 1)=0)and((OMB and 1)<>0);
 RPress:=((MB and 2)<>0)and((OMB and 2)=0);
 DblClick:=False;
 If Pressed Then Begin
  If(Frame-LastClk<40)and(Abs(MX-LastCX)<5)and(Abs(MY-LastCY)<5)Then Begin
   DblClick:=True;
   LastClk:=-1000;
  End
   Else
  Begin
   LastClk:=Frame;LastCX:=MX;LastCY:=MY;
  End;
 End;
 HaveKey:=False;KeyExt:=0;KeyCh:=#0;
 If KeyPressed Then Begin
  KeyCh:=ReadKey;
  HaveKey:=True;
  If KeyCh=#0 Then Begin
   KeyCh:=ReadKey;
   KeyExt:=Ord(KeyCh);
   KeyCh:=#0;
  End;
 End;
End;

{ ------------------------------------------------------------------ }
{ Primitives de dessin                                                }
{ ------------------------------------------------------------------ }

Procedure FillR(X1,Y1,X2,Y2,C:Integer);Begin
 SetFillStyle(SolidFill,C);
 Bar(X1,Y1,X2,Y2);
End;

Procedure Bevel(X1,Y1,X2,Y2:Integer;Raised:Boolean);Begin
 If Raised Then Begin
  SetColor(White);Line(X1,Y1,X2-1,Y1);Line(X1,Y1,X1,Y2-1);
  SetColor(Black);Line(X2,Y1,X2,Y2);Line(X1,Y2,X2,Y2);
  SetColor(DarkGray);Line(X2-1,Y1+1,X2-1,Y2-1);Line(X1+1,Y2-1,X2-1,Y2-1);
 End
  Else
 Begin
  SetColor(DarkGray);Line(X1,Y1,X2-1,Y1);Line(X1,Y1,X1,Y2-1);
  SetColor(White);Line(X2,Y1,X2,Y2);Line(X1,Y2,X2,Y2);
  SetColor(Black);Line(X1+1,Y1+1,X2-2,Y1+1);Line(X1+1,Y1+1,X1+1,Y2-2);
 End;
End;

Procedure PanelBox(X1,Y1,X2,Y2:Integer;Raised:Boolean);Begin
 FillR(X1,Y1,X2,Y2,LightGray);
 Bevel(X1,Y1,X2,Y2,Raised);
End;

Procedure FieldBox(X1,Y1,X2,Y2:Integer);Begin
 FillR(X1,Y1,X2,Y2,White);
 Bevel(X1,Y1,X2,Y2,False);
End;

Procedure TxtAt(X,Y:Integer;S:String;C:Integer);Begin
 SetColor(C);
 OutTextXY(X,Y,S);
End;

Procedure TxtC(X1,X2,Y:Integer;S:String;C:Integer);Begin
 SetColor(C);
 OutTextXY(X1+((X2-X1+1)-TextWidth(S))div 2,Y,S);
End;

Procedure TxtW(X,Y,W:Integer;S:String;C:Integer);Begin
 If Length(S)*8>W Then S:=Copy(S,1,W div 8);
 SetColor(C);
 OutTextXY(X,Y,S);
End;

Procedure Button(X1,Y1,X2,Y2:Integer;Cap:String;En:Boolean);Begin
 PanelBox(X1,Y1,X2,Y2,True);
 If En Then TxtC(X1,X2,Y1+((Y2-Y1)-7)div 2,Cap,Black)
       Else TxtC(X1,X2,Y1+((Y2-Y1)-7)div 2,Cap,DarkGray);
End;

Procedure Tri(X,Y,Dir,C:Integer);
Var I:Integer;
Begin
 SetColor(C);
 Case Dir of
  0:For I:=0 to 3 do Line(X-I,Y-2+I,X+I,Y-2+I);
  1:For I:=0 to 3 do Line(X-I,Y+2-I,X+I,Y+2-I);
  2:For I:=0 to 3 do Line(X-2+I,Y-I,X-2+I,Y+I);
  Else For I:=0 to 3 do Line(X+2-I,Y-I,X+2-I,Y+I);
 End;
End;

Procedure CheckBox(X,Y:Integer;On:Boolean);Begin
 FillR(X,Y,X+10,Y+10,White);
 Bevel(X,Y,X+10,Y+10,False);
 If On Then Begin
  SetColor(Black);
  Line(X+3,Y+5,X+4,Y+7);Line(X+4,Y+7,X+8,Y+3);
  Line(X+3,Y+4,X+4,Y+6);Line(X+4,Y+6,X+8,Y+2);
 End;
End;

{ Barre de defilement verticale (largeur 12) }
Procedure VScrollDraw(X,Y1,Y2,Pos,Total,Vis:Integer);
Var Tk,Th,Ty,Rng:LongInt;
Begin
 FillR(X,Y1,X+11,Y2,LightGray);
 SetColor(DarkGray);Rectangle(X,Y1,X+11,Y2);
 Button(X,Y1,X+11,Y1+11,'',True);Tri(X+6,Y1+6,0,Black);
 Button(X,Y2-11,X+11,Y2,'',True);Tri(X+6,Y2-5,1,Black);
 Tk:=Y2-Y1-24;
 If(Total>Vis)and(Tk>4)Then Begin
  Th:=Tk*Vis div Total;If Th<6 Then Th:=6;
  Rng:=Total-Vis;
  Ty:=Y1+12+(Tk-Th)*Pos div Rng;
  Button(X,Ty,X+11,Ty+Th,'',True);
 End;
End;

{ Retourne la nouvelle position si le clic est dans la barre }
Function VScrollClick(X,Y1,Y2,Pos,Total,Vis:Integer):Integer;
Var Tk,Th,Ty,Rng:LongInt;
    P:Integer;
Begin
 P:=Pos;
 If Inside(MX,MY,X,Y1,X+11,Y2)Then Begin
  If MY<=Y1+11 Then Dec(P)
   Else
  If MY>=Y2-11 Then Inc(P)
   Else
  Begin
   Tk:=Y2-Y1-24;
   If(Total>Vis)and(Tk>4)Then Begin
    Th:=Tk*Vis div Total;If Th<6 Then Th:=6;
    Rng:=Total-Vis;
    Ty:=Y1+12+(Tk-Th)*Pos div Rng;
    If MY<Ty Then Dec(P,MaxI(1,Vis-1))
     Else
    If MY>Ty+Th Then Inc(P,MaxI(1,Vis-1));
   End;
  End;
 End;
 If P>Total-Vis Then P:=Total-Vis;
 If P<0 Then P:=0;
 VScrollClick:=P;
End;

{ ------------------------------------------------------------------ }
{ Fenetres : ordre et gestion                                         }
{ ------------------------------------------------------------------ }

Procedure ZRemove(W:Integer);
Var I,J:Integer;
Begin
 J:=0;
 For I:=0 to ZCnt-1 do If ZList[I]<>W Then Begin ZList[J]:=ZList[I];Inc(J);End;
 ZCnt:=J;
End;

Procedure WinFront(W:Integer);Begin
 ZRemove(W);
 ZList[ZCnt]:=W;Inc(ZCnt);
End;

Procedure WinShow(W:Integer);Begin
 WOpen[W]:=True;
 WinFront(W);
 Dirty:=True;
End;

Procedure WinClose(W:Integer);Begin
 WOpen[W]:=False;
 ZRemove(W);
 Dirty:=True;
End;

Function TopWin:Integer;Begin
 If ZCnt>0 Then TopWin:=ZList[ZCnt-1] Else TopWin:=-1;
End;

Procedure WinFrame(W:Integer;Title:String);
Var X,Y,Wd,Ht:Integer;
    C:Integer;
Begin
 X:=WX[W];Y:=WY[W];Wd:=WW[W];Ht:=WH[W];
 PanelBox(X,Y,X+Wd-1,Y+Ht-1,True);
 If TopWin=W Then C:=Blue Else C:=DarkGray;
 FillR(X+3,Y+3,X+Wd-4,Y+16,C);
 TxtW(X+7,Y+6,Wd-34,Title,White);
 PanelBox(X+Wd-18,Y+4,X+Wd-6,Y+15,True);
 SetColor(Black);
 Line(X+Wd-15,Y+7,X+Wd-9,Y+13);Line(X+Wd-15,Y+13,X+Wd-9,Y+7);
End;

{ ------------------------------------------------------------------ }
{ Boites de dialogue modales                                          }
{ ------------------------------------------------------------------ }

Procedure RedrawAll;Forward;

{ Vide les evenements en attente a la fermeture d'une boite modale pour que
  la boucle principale ne rejoue pas la touche ou le clic qui l'a fermee. }
Procedure FlushInput;Begin
 HaveKey:=False;KeyExt:=0;KeyCh:=#0;
 Pressed:=False;DblClick:=False;RPress:=False;Released:=False;
End;

Procedure DlgFrame(X,Y,W,H:Integer;Title:String);Begin
 PanelBox(X,Y,X+W-1,Y+H-1,True);
 FillR(X+3,Y+3,X+W-4,Y+16,Blue);
 TxtW(X+7,Y+6,W-12,Title,White);
End;

Function InputBox(Title,Prompt:String;Var S:String;MaxLen:Integer):Boolean;
Var
 X,Y,W,H:Integer;
 Done,OK,Redraw,Fresh:Boolean;
 T:String;
Begin
 W:=336;H:=94;
 X:=(SW-W)div 2;Y:=(SH-H)div 3;
 T:=S;Fresh:=True;Redraw:=True;Done:=False;OK:=False;
 RedrawAll;
 Repeat
  If Redraw Then Begin
   HideMouse;
   DlgFrame(X,Y,W,H,Title);
   TxtW(X+10,Y+24,W-20,Prompt,Black);
   FieldBox(X+10,Y+38,X+W-11,Y+52);
   If Fresh Then FillR(X+12,Y+40,X+12+TextWidth(T)+1,Y+50,Blue);
   If Fresh Then TxtW(X+13,Y+42,W-30,T,White)
            Else TxtW(X+13,Y+42,W-30,T+'_',Black);
   Button(X+W-166,Y+64,X+W-92,Y+82,'OK',True);
   Button(X+W-86,Y+64,X+W-12,Y+82,Tr('Annuler','Cancel'),True);
   ShowMouse;
   Redraw:=False;
  End;
  PollInput;
  If HaveKey Then Begin
   If KeyExt=0 Then Begin
    Case KeyCh of
     #13:Begin OK:=True;Done:=True;End;
     #27:Done:=True;
     #8:Begin
      If Fresh Then Begin T:='';Fresh:=False;End
       Else If Length(T)>0 Then Delete(T,Length(T),1);
      Redraw:=True;
     End;
     Else If KeyCh>=' 'Then Begin
      If Fresh Then Begin T:='';Fresh:=False;End;
      If Length(T)<MaxLen Then T:=T+KeyCh;
      Redraw:=True;
     End;
    End;
   End;
  End;
  If Pressed Then Begin
   If Inside(MX,MY,X+W-166,Y+64,X+W-92,Y+82)Then Begin OK:=True;Done:=True;End
    Else
   If Inside(MX,MY,X+W-86,Y+64,X+W-12,Y+82)Then Done:=True;
  End;
  Delay(10);
 Until Done;
 If OK Then S:=T;
 InputBox:=OK;
 FlushInput;
 Dirty:=True;
End;

Function InputNum(Title,Prompt:String;Var V:Double;Dec:Integer):Boolean;
Var S:String;
    R:Double;
Begin
 S:=RStr(V,Dec);
 InputNum:=False;
 If InputBox(Title,Prompt,S,20)Then Begin
  If ValStr(S,R)Then Begin V:=R;InputNum:=True;End
   Else StatMsg:=Tr('Valeur num\erique invalide','Invalid numeric value');
 End;
End;

Var
 ChItems:Array[0..11] of String[70];
 ChN:Integer;

Function ChoiceBox(Title:String;Sel:Integer):Integer;
Var
 X,Y,W,H,I,Hov,MW:Integer;
 Done,Redraw:Boolean;
 R:Integer;
Begin
 MW:=0;
 For I:=0 to ChN-1 do If Length(ChItems[I])>MW Then MW:=Length(ChItems[I]);
 W:=MW*8+40;
 If W<300 Then W:=300;
 If W>SW-20 Then W:=SW-20;
 H:=44+ChN*14;
 X:=(SW-W)div 2;Y:=(SH-H)div 3;
 Redraw:=True;Done:=False;R:=-1;
 If Sel<0 Then Sel:=0;
 RedrawAll;
 Repeat
  If Redraw Then Begin
   HideMouse;
   DlgFrame(X,Y,W,H,Title);
   FieldBox(X+8,Y+22,X+W-9,Y+22+ChN*14+2);
   For I:=0 to ChN-1 do Begin
    If I=Sel Then Begin
     FillR(X+10,Y+24+I*14,X+W-11,Y+24+I*14+12,Blue);
     TxtW(X+14,Y+27+I*14,W-28,ChItems[I],White);
    End
     Else TxtW(X+14,Y+27+I*14,W-28,ChItems[I],Black);
   End;
   ShowMouse;
   Redraw:=False;
  End;
  PollInput;
  If HaveKey Then Begin
   If KeyExt=72 Then Begin If Sel>0 Then Dec(Sel);Redraw:=True;End;
   If KeyExt=80 Then Begin If Sel<ChN-1 Then Inc(Sel);Redraw:=True;End;
   If(KeyExt=0)and(KeyCh=#13)Then Begin R:=Sel;Done:=True;End;
   If(KeyExt=0)and(KeyCh=#27)Then Done:=True;
  End;
  If(MX<>OMX)or(MY<>OMY)Then Begin
   If Inside(MX,MY,X+10,Y+24,X+W-11,Y+24+ChN*14-1)Then Begin
    Hov:=(MY-Y-24)div 14;
    If Hov<>Sel Then Begin Sel:=Hov;Redraw:=True;End;
   End;
  End;
  If Pressed Then Begin
   If Inside(MX,MY,X+10,Y+24,X+W-11,Y+24+ChN*14-1)Then Begin
    R:=(MY-Y-24)div 14;Done:=True;
   End
    Else If Not Inside(MX,MY,X,Y,X+W-1,Y+H-1)Then Done:=True;
  End;
  Delay(10);
 Until Done;
 ChoiceBox:=R;
 FlushInput;
 Dirty:=True;
End;

{ Boite de message : lignes separees par '|' }
Procedure MsgBox(Title,Msg:String);
Var
 X,Y,W,H,I,NL,P,MaxW:Integer;
 Ls:Array[0..15] of String[70];
 Done:Boolean;
Begin
 NL:=0;MaxW:=120;
 While(Msg<>'')and(NL<16)do Begin
  P:=Pos('|',Msg);
  If P=0 Then Begin Ls[NL]:=Copy(Msg,1,70);Msg:='';End
   Else Begin Ls[NL]:=Copy(Copy(Msg,1,P-1),1,70);Delete(Msg,1,P);End;
  If Length(Ls[NL])*8>MaxW Then MaxW:=Length(Ls[NL])*8;
  Inc(NL);
 End;
 W:=MaxW+32;H:=62+NL*10;
 X:=(SW-W)div 2;Y:=(SH-H)div 3;
 RedrawAll;
 HideMouse;
 DlgFrame(X,Y,W,H,Title);
 For I:=0 to NL-1 do TxtAt(X+16,Y+26+I*10,Ls[I],Black);
 Button(X+(W div 2)-34,Y+H-28,X+(W div 2)+34,Y+H-10,'OK',True);
 ShowMouse;
 Done:=False;
 Repeat
  PollInput;
  If HaveKey Then If(KeyCh=#13)or(KeyCh=#27)or(KeyCh=' ')Then Done:=True;
  If Pressed Then
   If Inside(MX,MY,X+(W div 2)-34,Y+H-28,X+(W div 2)+34,Y+H-10)Then Done:=True;
  Delay(10);
 Until Done;
 FlushInput;
 Dirty:=True;
End;

Function YesNo(Title,Msg:String):Boolean;
Var
 X,Y,W,H:Integer;
 Done,R:Boolean;
Begin
 W:=Length(Msg)*8+32;
 If W<260 Then W:=260;
 If W>SW-20 Then W:=SW-20;
 H:=84;
 X:=(SW-W)div 2;Y:=(SH-H)div 3;
 RedrawAll;
 HideMouse;
 DlgFrame(X,Y,W,H,Title);
 TxtW(X+16,Y+26,W-24,Msg,Black);
 Button(X+(W div 2)-80,Y+H-30,X+(W div 2)-8,Y+H-10,Tr('Oui','Yes'),True);
 Button(X+(W div 2)+8,Y+H-30,X+(W div 2)+80,Y+H-10,Tr('Non','No'),True);
 ShowMouse;
 Done:=False;R:=False;
 Repeat
  PollInput;
  If HaveKey Then Begin
   If(KeyCh='o')or(KeyCh='O')or(KeyCh='y')or(KeyCh='Y')or(KeyCh=#13)Then Begin R:=True;Done:=True;End;
   If(KeyCh='n')or(KeyCh='N')or(KeyCh=#27)Then Done:=True;
  End;
  If Pressed Then Begin
   If Inside(MX,MY,X+(W div 2)-80,Y+H-30,X+(W div 2)-8,Y+H-10)Then Begin R:=True;Done:=True;End;
   If Inside(MX,MY,X+(W div 2)+8,Y+H-30,X+(W div 2)+80,Y+H-10)Then Done:=True;
  End;
  Delay(10);
 Until Done;
 YesNo:=R;
 FlushInput;
 Dirty:=True;
End;

{ ------------------------------------------------------------------ }
{ Fichiers : projet (.RSM) et export CSV                              }
{ ------------------------------------------------------------------ }

Const
 WTitle=19;   { decalage du contenu sous la barre de titre d'une fenetre }

Var
 RepIdx:Array[0..MaxT-1] of Integer;
 RepN:Integer;
 TreeZ,TreeR:Array[0..63] of Integer;
 TreeN:Integer;
 PlotRO:String;

Procedure Stat(S:String);Begin
 StatMsg:=S;
 Dirty:=True;
End;

Function NextTok(Var S:String):String;
Var P:Integer;
Begin
 While(S<>'')and(S[1]=' ')do Delete(S,1,1);
 P:=Pos(' ',S);
 If P=0 Then Begin NextTok:=S;S:='';End
  Else Begin NextTok:=Copy(S,1,P-1);Delete(S,1,P);End;
End;

Function TokNum(Var S:String):Double;
Var V:Double;
Begin
 If Not ValStr(NextTok(S),V)Then V:=0;
 TokNum:=V;
End;

Function SaveProject(Fn:String):Boolean;
Var
 F:Text;
 I,O,Zn,Rn:Integer;
Begin
 SaveProject:=False;
 {$I-}
 Assign(F,Fn);
 Rewrite(F);
 {$I+}
 If IOResult<>0 Then Exit;
 WriteLn(F,'RESSIM 1');
 WriteLn(F,'; Projet RESSIM : hydrogrammes, reseau, reservoir et operations');
 WriteLn(F,'TITLE ',PrjFr);
 WriteLn(F,'TITLE_EN ',PrjEn);
 WriteLn(F,'FACTOR ',InflowFactor:0:3);
 WriteLn(F,'INITELEV ',InitElev:0:2);
 WriteLn(F,'SIM ',SimStart,' ',SimEnd);
 WriteLn(F,'LAG ',LagSM,' ',LagMB,' ',LagBM,' ',LagMT,' ',SmoothBM);
 For I:=0 to 3 do
  WriteLn(F,'HYD ',I,' ',HBase[I]:0:1,' ',HPeak[I]:0:1,' ',HT0[I]:0:1,' ',HTp[I]:0:1);
 For I:=0 to NAlts-1 do
  WriteLn(F,'ALT ',I,' ',Alts[I].OpSet,' ',Ord(Alts[I].Incl));
 WriteLn(F,'TOPDAM ',M.TopDam:0:2);
 For I:=0 to PoolN-1 do
  WriteLn(F,'POOL ',I,' ',M.PE[I]:0:2,' ',M.PS[I]:0:1,' ',M.PA[I]:0:1);
 For I:=0 to GateN-1 do
  WriteLn(F,'GATE ',I,' ',M.GE[I]:0:2,' ',M.GT[I]:0:1);
 For O:=0 to NOps-1 do Begin
  WriteLn(F,'OPNAME ',O,' ',M.Ops[O].Name);
  WriteLn(F,'ISCNT ',O,' ',M.Ops[O].ISCnt);
  For I:=0 to M.Ops[O].ISCnt-1 do
   WriteLn(F,'ISP ',O,' ',I,' ',M.Ops[O].ISE[I]:0:2,' ',M.Ops[O].ISR[I]:0:1);
  For Zn:=0 to NZones-1 do Begin
   WriteLn(F,'ZONE ',O,' ',Zn,' ',M.Ops[O].Z[Zn].Top:0:2,' ',M.Ops[O].Z[Zn].N);
   For Rn:=0 to M.Ops[O].Z[Zn].N-1 do
    WriteLn(F,'RULE ',O,' ',Zn,' ',Rn,' ',M.Ops[O].Z[Zn].R[Rn].Kind,' ',
            M.Ops[O].Z[Zn].R[Rn].P1:0:2);
  End;
 End;
 Close(F);
 SaveProject:=True;
End;

Procedure SetDescr;Begin
 M.Ops[0].Descr:=Tr('Sans contraintes de d\ebit en aval','No downstream flow constraints');
 M.Ops[1].Descr:=Tr('Repr\esentation du r\gglement de r\egulation aval',
                    'Representation of the downstream regulation plan');
End;

Type
 TState=Record
  Mdl:TModel;
  Al:TAltArr;
  AA,SS,SE,L1,L2,L3,L4,L5:Integer;
  Fct,IEl:Double;
  HB,HP,H0,HT:TH4;
  T1,T2:String[60];
 End;

Procedure GetState(Var S:TState);Begin
 S.Mdl:=M;S.Al:=Alts;S.AA:=ActAlt;S.SS:=SimStart;S.SE:=SimEnd;
 S.L1:=LagSM;S.L2:=LagMB;S.L3:=LagBM;S.L4:=LagMT;S.L5:=SmoothBM;
 S.Fct:=InflowFactor;S.IEl:=InitElev;
 S.HB:=HBase;S.HP:=HPeak;S.H0:=HT0;S.HT:=HTp;S.T1:=PrjFr;S.T2:=PrjEn;
End;

Procedure SetState(Var S:TState);Begin
 M:=S.Mdl;Alts:=S.Al;ActAlt:=S.AA;SimStart:=S.SS;SimEnd:=S.SE;
 LagSM:=S.L1;LagMB:=S.L2;LagBM:=S.L3;LagMT:=S.L4;SmoothBM:=S.L5;
 InflowFactor:=S.Fct;InitElev:=S.IEl;
 HBase:=S.HB;HPeak:=S.HP;HT0:=S.H0;HTp:=S.HT;PrjFr:=S.T1;PrjEn:=S.T2;
End;

Function BaseName(Fn:String):String;
Var D:DirStr;N:NameStr;E:ExtStr;
Begin
 FSplit(Fn,D,N,E);
 BaseName:=N+E;
End;

{ Charge un fichier projet. En cas d'erreur, l'etat courant est conserve. }
Function LoadProject(Fn:String):Boolean;
Var
 F:Text;
 L,Key:String;
 I,O,Zn,Rn:Integer;
 A:Double;
 Sv:TState;
 Bad:Boolean;
Begin
 LoadProject:=False;
 {$I-}
 Assign(F,Fn);
 Reset(F);
 {$I+}
 If IOResult<>0 Then Exit;
 ReadLn(F,L);
 If Copy(L,1,6)<>'RESSIM'Then Begin Close(F);Exit;End;
 GetState(Sv);
 InitDefaults;
 SetDescr;
 While Not Eof(F)do Begin
  ReadLn(F,L);
  Key:=StrUp(NextTok(L));
  If Key='TITLE'Then PrjFr:=Copy(Acc(L),1,60)
  Else If Key='TITLE_EN'Then PrjEn:=Copy(Acc(L),1,60)
  Else If Key='FACTOR'Then InflowFactor:=TokNum(L)
  Else If Key='INITELEV'Then InitElev:=TokNum(L)
  Else If Key='SIM'Then Begin
   SimStart:=Clamp(Round(TokNum(L)),1,NT-2);
   SimEnd:=Clamp(Round(TokNum(L)),SimStart+1,NT-1);
  End
  Else If Key='LAG'Then Begin
   LagSM:=Clamp(Round(TokNum(L)),0,24);LagMB:=Clamp(Round(TokNum(L)),0,24);
   LagBM:=Clamp(Round(TokNum(L)),0,24);LagMT:=Clamp(Round(TokNum(L)),0,24);
   SmoothBM:=Clamp(Round(TokNum(L)),0,6);
  End
  Else If Key='HYD'Then Begin
   I:=Clamp(Round(TokNum(L)),0,3);
   HBase[I]:=TokNum(L);HPeak[I]:=TokNum(L);HT0[I]:=TokNum(L);HTp[I]:=TokNum(L);
  End
  Else If Key='ALT'Then Begin
   I:=Clamp(Round(TokNum(L)),0,NAlts-1);
   Alts[I].OpSet:=Clamp(Round(TokNum(L)),0,NOps-1);
   Alts[I].Incl:=TokNum(L)<>0;
  End
  Else If Key='TOPDAM'Then M.TopDam:=TokNum(L)
  Else If Key='POOL'Then Begin
   I:=Clamp(Round(TokNum(L)),0,PoolN-1);
   M.PE[I]:=TokNum(L);M.PS[I]:=TokNum(L);M.PA[I]:=TokNum(L);
  End
  Else If Key='GATE'Then Begin
   I:=Clamp(Round(TokNum(L)),0,GateN-1);
   M.GE[I]:=TokNum(L);M.GT[I]:=TokNum(L);
  End
  Else If Key='OPNAME'Then Begin
   O:=Clamp(Round(TokNum(L)),0,NOps-1);
   While(L<>'')and(L[1]=' ')do Delete(L,1,1);
   M.Ops[O].Name:=Copy(L,1,22);
  End
  Else If Key='ISCNT'Then Begin
   O:=Clamp(Round(TokNum(L)),0,NOps-1);
   M.Ops[O].ISCnt:=Clamp(Round(TokNum(L)),0,ISMax);
  End
  Else If Key='ISP'Then Begin
   O:=Clamp(Round(TokNum(L)),0,NOps-1);
   I:=Clamp(Round(TokNum(L)),0,ISMax-1);
   M.Ops[O].ISE[I]:=TokNum(L);M.Ops[O].ISR[I]:=TokNum(L);
  End
  Else If Key='ZONE'Then Begin
   O:=Clamp(Round(TokNum(L)),0,NOps-1);
   Zn:=Clamp(Round(TokNum(L)),0,NZones-1);
   M.Ops[O].Z[Zn].Top:=TokNum(L);
   M.Ops[O].Z[Zn].N:=0;
  End
  Else If Key='RULE'Then Begin
   O:=Clamp(Round(TokNum(L)),0,NOps-1);
   Zn:=Clamp(Round(TokNum(L)),0,NZones-1);
   Rn:=Clamp(Round(TokNum(L)),0,MaxRules-1);
   A:=TokNum(L);
   M.Ops[O].Z[Zn].R[Rn].Kind:=Clamp(Round(A),0,6);
   M.Ops[O].Z[Zn].R[Rn].P1:=TokNum(L);
   If M.Ops[O].Z[Zn].N<Rn+1 Then M.Ops[O].Z[Zn].N:=Rn+1;
  End;
 End;
 Close(F);
 { Validation : tables croissantes, hydrogrammes coherents }
 Bad:=False;
 For I:=1 to PoolN-1 do If M.PE[I]<=M.PE[I-1]Then Bad:=True;
 For I:=1 to GateN-1 do If M.GE[I]<=M.GE[I-1]Then Bad:=True;
 For O:=0 to NOps-1 do
  For Zn:=1 to NZones-1 do
   If M.Ops[O].Z[Zn].Top<=M.Ops[O].Z[Zn-1].Top Then Bad:=True;
 For I:=0 to 3 do
  If(HTp[I]<=0)or(HBase[I]<0)or(HPeak[I]<HBase[I])Then Bad:=True;
 If(InflowFactor<0.1)or(InflowFactor>5)Then Bad:=True;
 If(InitElev<M.PE[0])or(InitElev>M.PE[PoolN-1])Then Bad:=True;
 If Bad Then Begin SetState(Sv);Exit;End;
 SetDescr;
 DispT:=SimStart;
 CurName:=BaseName(Fn);
 LoadProject:=True;
End;

{ Titre d'un fichier projet (pour la liste d'ouverture) }
Function PeekTitle(Fn:String):String;
Var
 F:Text;
 L,Key,Fr,En:String;
 N:Integer;
Begin
 Fr:='';En:='';N:=0;
 {$I-}
 Assign(F,Fn);
 Reset(F);
 {$I+}
 If IOResult<>0 Then Begin PeekTitle:='';Exit;End;
 While(Not Eof(F))and(N<16)do Begin
  ReadLn(F,L);Inc(N);
  Key:=StrUp(NextTok(L));
  If Key='TITLE'Then Fr:=Copy(Acc(L),1,56);
  If Key='TITLE_EN'Then En:=Copy(L,1,56);
 End;
 Close(F);
 If Eng and(En<>'')Then PeekTitle:=En
  Else If Fr<>''Then PeekTitle:=Fr
  Else PeekTitle:=En;
End;

{ Liste des projets disponibles : SAMPLES\RESSIM\ puis repertoire courant }
Var
 PickPath:Array[0..11] of String[100];
 PickN:Integer;

Function SampleDir(Which:Integer):String;
Var D:DirStr;N:NameStr;E:ExtStr;
Begin
 If Which=0 Then SampleDir:='SAMPLES'+DirSep+'RESSIM'+DirSep
  Else Begin
   FSplit(ParamStr(0),D,N,E);
   SampleDir:=D+'SAMPLES'+DirSep+'RESSIM'+DirSep;
  End;
End;

Procedure ScanDir(Dir:String);
Var SR:SearchRec;
Begin
 FindFirst(Dir+'*.RSM',Archive,SR);
 While(DosError=0)and(PickN<11)do Begin
  PickPath[PickN]:=Dir+SR.Name;
  ChItems[PickN]:=PadR(SR.Name,13)+PeekTitle(Dir+SR.Name);
  Inc(PickN);
  FindNext(SR);
 End;
 {$IFDEF FPC}FindClose(SR);{$ENDIF}
End;

Procedure ScanProjects;
Var I,J:Integer;
    P:String[100];
    C:String[70];
Begin
 PickN:=0;
 ScanDir(SampleDir(0));
 If FExpand(SampleDir(1))<>FExpand(SampleDir(0))Then ScanDir(SampleDir(1));
 ScanDir('');
 For I:=0 to PickN-2 do
  For J:=I+1 to PickN-1 do
   If StrUp(BaseName(PickPath[J]))<StrUp(BaseName(PickPath[I]))Then Begin
    P:=PickPath[I];PickPath[I]:=PickPath[J];PickPath[J]:=P;
    C:=ChItems[I];ChItems[I]:=ChItems[J];ChItems[J]:=C;
   End;
End;

{ Charge l'exemple par defaut s'il existe (SAMPLES\RESSIM\BALDEAGL.RSM) }
Function LoadDefaultSample:Boolean;Begin
 LoadDefaultSample:=LoadProject(SampleDir(0)+'BALDEAGL.RSM')or
                    LoadProject(SampleDir(1)+'BALDEAGL.RSM');
End;

Function ExportCSV(Fn:String):Boolean;
Var
 F:Text;
 T:Integer;
Begin
 ExportCSV:=False;
 If Not Alts[ActAlt].Valid Then Exit;
 {$I-}
 Assign(F,Fn);
 Rewrite(F);
 {$I+}
 If IOResult<>0 Then Exit;
 WriteLn(F,'DateTime,Elevation_ft,Storage_acft,Inflow_cfs,Release_cfs,',
         'MarshCkJct_cfs,BeechCkStation_cfs,MillHall_cfs,BaldEagleTotal_cfs,Zone,ActiveRule');
 With Res[ActAlt]do
  For T:=0 to NT-1 do
   WriteLn(F,FmtDT(T),',',Elev[T]:0:2,',',Stor[T]:0:1,',',Flow[0][T]:0:1,',',
           Rel[T]:0:1,',',Flow[2][T]:0:1,',',Flow[3][T]:0:1,',',Flow[4][T]:0:1,
           ',',Flow[5][T]:0:1,',',ZoneAbbr(Zone[T]),',',RuleLabel(RK[T],RV[T]));
 Close(F);
 ExportCSV:=True;
End;

Function AskFile(Title,Prompt,Def:String;Var Fn:String):Boolean;Begin
 Fn:=Def;
 AskFile:=InputBox(Title,Prompt,Fn,40)and(Fn<>'');
End;

{ ------------------------------------------------------------------ }
{ Fenetre de texte (aide, a propos, journal, resume, verification)    }
{ ------------------------------------------------------------------ }

Procedure TxtAdd(S:String);Begin
 If TxtN<80 Then Begin TxtL[TxtN]:=Copy(S,1,72);Inc(TxtN);End;
End;

Procedure TxtOpen(Title:String);Begin
 TxtTitle:=Copy(Title,1,40);
 TxtTop:=0;
 WW[wTxt]:=600;WH[wTxt]:=300;
 WX[wTxt]:=(SW-WW[wTxt])div 2;
 WY[wTxt]:=TitleH+MenuH+20;
 WinShow(wTxt);
End;

Function TxtVis:Integer;Begin
 TxtVis:=(WH[wTxt]-WTitle-46)div 10;
End;

Procedure DrawTxt;
Var X,Y,I:Integer;
Begin
 X:=WX[wTxt];Y:=WY[wTxt];
 WinFrame(wTxt,TxtTitle);
 FieldBox(X+8,Y+WTitle,X+WW[wTxt]-23,Y+WH[wTxt]-34);
 For I:=0 to TxtVis-1 do
  If TxtTop+I<TxtN Then TxtAt(X+14,Y+WTitle+5+I*10,TxtL[TxtTop+I],Black);
 VScrollDraw(X+WW[wTxt]-21,Y+WTitle,Y+WH[wTxt]-34,TxtTop,TxtN,TxtVis);
 Button(X+(WW[wTxt]div 2)-36,Y+WH[wTxt]-28,X+(WW[wTxt]div 2)+36,Y+WH[wTxt]-10,'OK',True);
End;

Procedure TxtClick;
Var X,Y:Integer;
Begin
 X:=WX[wTxt];Y:=WY[wTxt];
 TxtTop:=VScrollClick(X+WW[wTxt]-21,Y+WTitle,Y+WH[wTxt]-34,TxtTop,TxtN,TxtVis);
 If Inside(MX,MY,X+(WW[wTxt]div 2)-36,Y+WH[wTxt]-28,X+(WW[wTxt]div 2)+36,Y+WH[wTxt]-10)Then
  WinClose(wTxt);
 Dirty:=True;
End;

Procedure TxtKey;
Var Mx2:Integer;
Begin
 Mx2:=MaxI(0,TxtN-TxtVis);
 If KeyExt=72 Then TxtTop:=MaxI(0,TxtTop-1);
 If KeyExt=80 Then TxtTop:=MinI(Mx2,TxtTop+1);
 If KeyExt=73 Then TxtTop:=MaxI(0,TxtTop-TxtVis+1);
 If KeyExt=81 Then TxtTop:=MinI(Mx2,TxtTop+TxtVis-1);
 If KeyExt=71 Then TxtTop:=0;
 If KeyExt=79 Then TxtTop:=Mx2;
 If(KeyExt=0)and((KeyCh=#27)or(KeyCh=#13))Then WinClose(wTxt);
 Dirty:=True;
End;

{ ------------------------------------------------------------------ }
{ Rapport de decision de relache                                      }
{ ------------------------------------------------------------------ }

Function RepVis:Integer;Begin
 RepVis:=(WH[wRep]-WTitle-84-40)div 10;
End;

Procedure RepBuild;
Var T:Integer;
Begin
 RepN:=0;
 If Not Alts[ActAlt].Valid Then Begin RepTop:=0;Exit;End;
 With Res[ActAlt]do
  For T:=SimStart to SimEnd do
   If(Not RepChg)or(T=SimStart)or(RK[T]<>RK[T-1])or(Zone[T]<>Zone[T-1])or
     (RV[T]<>RV[T-1])Then Begin
    RepIdx[RepN]:=T;Inc(RepN);
   End;
 If RepTop>RepN-RepVis Then RepTop:=RepN-RepVis;
 If RepTop<0 Then RepTop:=0;
End;

Procedure RepOpen;Begin
 RepBuild;
 WinShow(wRep);
End;

Procedure DrawRep;
Var
 X,Y,I,T,RY:Integer;
 S:String;
 C:Integer;
Begin
 X:=WX[wRep];Y:=WY[wRep];
 WinFrame(wRep,Tr('Rapport de d\ecision de rel\qche : Sayers','Release Decision Report: Sayers'));
 FieldBox(X+8,Y+WTitle,X+WW[wRep]-9,Y+WH[wRep]-34);
 TxtAt(X+14,Y+WTitle+5,'Alternative: '+Alts[ActAlt].Name+'-:'+Alts[ActAlt].Name+'    Run: '+
       Alts[ActAlt].Name+'-0',Black);
 TxtAt(X+14,Y+WTitle+15,Tr('Jeu d''op\erations : ','Operation set: ')+M.Ops[Alts[ActAlt].OpSet].Name,Black);
 TxtAt(X+14,Y+WTitle+25,Tr('Retour : ','Lookback: ')+FmtDT(0)+Tr('   D\ebut : ','   Start: ')+FmtDT(SimStart),Black);
 TxtAt(X+14,Y+WTitle+35,Tr('Fin : ','End: ')+FmtDT(SimEnd)+Tr('   Zones : INA CON LFC FC MXP TOD','   Zones: INA CON LFC FC MXP TOD'),Black);
 TxtAt(X+14,Y+WTitle+45,Tr('Cl\e : MinWQ=min MaxCC=ctrl aval IndSurch=surcharge RROC=taux','Key: MinWQ=min MaxCC=downstream IndSurch=surcharge RROC=rate'),DarkGray);
 RY:=Y+WTitle+58;
 PanelBox(X+10,RY,X+WW[wRep]-26,RY+12,True);
 TxtAt(X+14,RY+3,Tr('Date-Heure','Date-Time'),Black);
 TxtAt(X+14+136,RY+3,'Elev ft',Black);
 TxtAt(X+14+200,RY+3,Tr('Entr.cfs','Inflow'),Black);
 TxtAt(X+14+268,RY+3,Tr('L\qcher','Release'),Black);
 TxtAt(X+14+336,RY+3,Tr('Zone:R\ggle active','Zone:Active rule'),Black);
 If(Not Alts[ActAlt].Valid)or(RepN=0)Then
  TxtAt(X+20,RY+22,Tr('Aucun r\esultat : lancez le calcul (F5).','No results: run the compute (F5).'),Red)
 Else
  For I:=0 to RepVis-1 do If RepTop+I<RepN Then Begin
   T:=RepIdx[RepTop+I];
   If T=DispT Then FillR(X+10,RY+14+I*10,X+WW[wRep]-26,RY+14+I*10+9,LightCyan);
   With Res[ActAlt]do Begin
    TxtAt(X+14,RY+15+I*10,FmtDT(T),Black);
    TxtAt(X+14+136,RY+15+I*10,PadL(RStr(Elev[T],2),7),Black);
    TxtAt(X+14+200,RY+15+I*10,PadL(RStr(Flow[0][T],1),7),Black);
    TxtAt(X+14+268,RY+15+I*10,PadL(RStr(Rel[T],1),7),Black);
    S:=ZoneAbbr(Zone[T])+':'+RuleLabel(RK[T],RV[T]);
    If RK[T]=rkCC Then C:=Red Else If RK[T]=rkInd Then C:=Magenta
     Else If RK[T]=rkMin Then C:=Blue Else C:=Black;
    TxtAt(X+14+336,RY+15+I*10,S,C);
   End;
  End;
 VScrollDraw(X+WW[wRep]-24,RY,Y+WH[wRep]-36,RepTop,RepN,RepVis);
 CheckBox(X+12,Y+WH[wRep]-27,RepChg);
 TxtAt(X+28,Y+WH[wRep]-26,Tr('Seulement les changements de r\ggle','Only rule changes'),Black);
 Button(X+WW[wRep]-250,Y+WH[wRep]-29,X+WW[wRep]-140,Y+WH[wRep]-9,Tr('Exporter CSV','Export CSV'),True);
 Button(X+WW[wRep]-130,Y+WH[wRep]-29,X+WW[wRep]-14,Y+WH[wRep]-9,Tr('Fermer','Close'),True);
End;

Procedure RepClick;
Var
 X,Y,RY,I,T:Integer;
 Fn:String;
Begin
 X:=WX[wRep];Y:=WY[wRep];
 RY:=Y+WTitle+58;
 RepTop:=VScrollClick(X+WW[wRep]-24,RY,Y+WH[wRep]-36,RepTop,RepN,RepVis);
 If Inside(MX,MY,X+12,Y+WH[wRep]-27,X+250,Y+WH[wRep]-17)Then Begin
  RepChg:=Not RepChg;
  RepBuild;
 End;
 If Inside(MX,MY,X+WW[wRep]-130,Y+WH[wRep]-29,X+WW[wRep]-14,Y+WH[wRep]-9)Then WinClose(wRep);
 If Inside(MX,MY,X+WW[wRep]-250,Y+WH[wRep]-29,X+WW[wRep]-140,Y+WH[wRep]-9)Then Begin
  If AskFile(Tr('Exporter','Export'),Tr('Nom du fichier CSV :','CSV file name:'),'RESSIM.CSV',Fn)Then Begin
   If ExportCSV(Fn)Then Stat(Tr('R\esultats export\es : ','Results exported: ')+Fn)
    Else Stat(Tr('\Echec de l''exportation','Export failed'));
  End;
 End;
 If Inside(MX,MY,X+10,RY+14,X+WW[wRep]-26,RY+13+RepVis*10)Then Begin
  I:=(MY-RY-14)div 10;
  If RepTop+I<RepN Then Begin
   T:=RepIdx[RepTop+I];
   DispT:=T;
  End;
 End;
 Dirty:=True;
End;

Procedure RepKey;Begin
 If KeyExt=72 Then RepTop:=MaxI(0,RepTop-1);
 If KeyExt=80 Then RepTop:=MaxI(0,MinI(RepN-RepVis,RepTop+1));
 If KeyExt=73 Then RepTop:=MaxI(0,RepTop-RepVis+1);
 If KeyExt=81 Then RepTop:=MaxI(0,MinI(RepN-RepVis,RepTop+RepVis-1));
 If KeyExt=71 Then RepTop:=0;
 If KeyExt=79 Then RepTop:=MaxI(0,RepN-RepVis);
 If(KeyExt=0)and(KeyCh=#27)Then WinClose(wRep);
 Dirty:=True;
End;

{ ------------------------------------------------------------------ }
{ Fenetre de graphique                                                }
{ ------------------------------------------------------------------ }

Function NodeName(N:Integer):String;Begin
 Case N of
  0:NodeName:='Sayers Inflow Jct';
  1:NodeName:='Sayers';
  2:NodeName:='Marsh Ck Jct';
  3:NodeName:='Beech Ck Station';
  4:NodeName:='Mill Hall';
  Else NodeName:='Bald Eagle Total';
 End;
End;

Function AltColor(A:Integer):Integer;Begin
 If A=0 Then AltColor:=Red Else AltColor:=Green;
End;

Function NiceStep(Rng:Double;Cnt:Integer):Double;
Var R,P,F:Double;
Begin
 R:=Rng/Cnt;
 If R<=0 Then R:=1;
 P:=1;
 While R>=10*P do P:=P*10;
 While R<P do P:=P/10;
 F:=R/P;
 If F<=1 Then F:=1 Else If F<=2 Then F:=2 Else If F<=5 Then F:=5 Else F:=10;
 NiceStep:=F*P;
End;

Function PlotVal(A,Kind,T:Integer):Double;Begin
 Case Kind of
  0:PlotVal:=Res[A].Flow[PlotNode][T];
  1:PlotVal:=Res[A].Elev[T];
  2:PlotVal:=Res[A].Stor[T];
  Else PlotVal:=Res[A].CumLoc[T];
 End;
End;

Procedure PlotArea(Var X0,Y0,X1,Y1:Integer);Begin
 X0:=WX[wPlot]+62;
 Y0:=WY[wPlot]+WTitle+22;
 X1:=WX[wPlot]+WW[wPlot]-14;
 Y1:=WY[wPlot]+WH[wPlot]-100;
End;

Procedure HatchRect(X1,Y1,X2,Y2,C:Integer);
Var K,W,H:Integer;
Begin
 If(X2<=X1)or(Y2<=Y1)Then Exit;
 W:=X2-X1;H:=Y2-Y1;
 SetViewPort(X1,Y1,X2,Y2,True);
 SetColor(C);
 K:=-H;
 While K<=W do Begin
  Line(K,H,K+H,0);
  Inc(K,5);
 End;
 SetViewPort(0,0,SW-1,SH-1,True);
End;

Procedure PlotOpen(Node,V:Integer);Begin
 PlotNode:=Node;
 PlotVar:=V;
 If(Node<>1)and(V>0)Then PlotVar:=0;
 PlotRO:='';
 WinShow(wPlot);
End;

Procedure DrawPlotRO;
Var
 X0,Y0,X1,Y1,T,A:Integer;
 S:String;
Begin
 PlotArea(X0,Y0,X1,Y1);
 FillR(WX[wPlot]+8,WY[wPlot]+WH[wPlot]-24,WX[wPlot]+WW[wPlot]-9,WY[wPlot]+WH[wPlot]-9,LightGray);
 If Inside(MX,MY,X0,Y0,X1,Y1)Then Begin
  T:=Round((MX-X0)*(NT-1)/(X1-X0));
  T:=Clamp(T,0,NT-1);
  S:=FmtDT(T);
  For A:=0 to NAlts-1 do If Alts[A].Valid Then
   S:=S+'  '+Copy(Alts[A].Name,1,4)+'='+RStr(PlotVal(A,PlotVar,T),1);
  TxtW(WX[wPlot]+10,WY[wPlot]+WH[wPlot]-21,WW[wPlot]-20,S,Black);
 End
  Else TxtW(WX[wPlot]+10,WY[wPlot]+WH[wPlot]-21,WW[wPlot]-20,PlotRO,DarkGray);
End;

Procedure DrawPlot;
Var
 X,Y,X0,Y0,X1,Y1,A,T,N,Ly,Px,Py,Xp,Yp,Y2,Y3:Integer;
 YMin,YMax,Step,V:Double;
 Any:Boolean;
 S,Title:String;
 Yo,Mo,D,H:Integer;
Function PltY(V:Double):Integer;Begin
 PltY:=Y1-Round((V-YMin)/(YMax-YMin)*(Y1-Y0));
End;
Function PltX(T:Double):Integer;Begin
 PltX:=X0+Round(T*(X1-X0)/(NT-1));
End;
Begin
 X:=WX[wPlot];Y:=WY[wPlot];
 Case PlotVar of
  1:Title:=Tr('\El\evation','Elevation');
  2:Title:=Tr('Stockage','Storage');
  Else Title:=Tr('D\ebit','Flow');
 End;
 WinFrame(wPlot,Tr('Graphique par d\efaut - ','Default Plot - ')+NodeName(PlotNode)+' ('+Title+')');
 PlotArea(X0,Y0,X1,Y1);
 { Barre d'outils }
 If PlotNode=1 Then Begin
  Button(X+8,Y+WTitle+2,X+88,Y+WTitle+17,Tr('D\ebits','Flows'),True);
  Button(X+92,Y+WTitle+2,X+172,Y+WTitle+17,Tr('\El\evation','Elevation'),True);
  Button(X+176,Y+WTitle+2,X+256,Y+WTitle+17,Tr('Stockage','Storage'),True);
  FillR(X+8+PlotVar*84,Y+WTitle+17,X+88+PlotVar*84,Y+WTitle+18,Blue);
 End
  Else TxtAt(X+10,Y+WTitle+6,Tr('D\ebit (cfs) aux points du r\eseau','Flow (cfs) at network locations'),Black);
 Button(X+WW[wPlot]-60,Y+WTitle+2,X+WW[wPlot]-36,Y+WTitle+17,'<',True);
 Button(X+WW[wPlot]-32,Y+WTitle+2,X+WW[wPlot]-8,Y+WTitle+17,'>',True);
 { Echelle verticale }
 Any:=False;YMin:=1E30;YMax:=-1E30;
 For A:=0 to NAlts-1 do If Alts[A].Valid Then
  For T:=0 to NT-1 do Begin
   V:=PlotVal(A,PlotVar,T);
   If V<YMin Then YMin:=V;
   If V>YMax Then YMax:=V;
   Any:=True;
  End;
 If(PlotNode=1)and(PlotVar=0)and Any Then
  For T:=0 to NT-1 do Begin
   V:=InfS[T];
   If V>YMax Then YMax:=V;
  End;
 If(PlotNode=3)and(PlotVar=0)and Any Then Begin
  If YMax<4900 Then YMax:=4900;
 End;
 FieldBox(X0-1,Y0-1,X1+1,Y1+1);
 If Not Any Then Begin
  TxtC(X0,X1,(Y0+Y1)div 2,Tr('Aucun r\esultat : lancez le calcul (F5)','No results: run the compute (F5)'),Red);
  Exit;
 End;
 If PlotVar<>1 Then YMin:=0;
 If YMax-YMin<1E-6 Then YMax:=YMin+1;
 Step:=NiceStep(YMax-YMin,6);
 YMax:=Int((YMax-YMin)/Step+0.999)*Step+YMin;
 YMin:=Int(YMin/Step)*Step;
 If YMax<=YMin Then YMax:=YMin+Step;
 { Grille horizontale }
 V:=YMin;
 While V<=YMax+Step*0.01 do Begin
  Py:=PltY(V);
  SetColor(LightGray);Line(X0,Py,X1,Py);
  If Step>=1 Then S:=IntStr(Round(V)) Else S:=RStr(V,1);
  TxtAt(X0-4-Length(S)*8,Py-3,S,Black);
  V:=V+Step;
 End;
 { Grille verticale : une ligne par jour }
 For T:=0 to NT-1 do Begin
  IdxToDate(T,Yo,Mo,D,H);
  If H=0 Then Begin
   Px:=PltX(T);
   SetColor(LightGray);Line(Px,Y0,Px,Y1);
   SetColor(Black);Line(Px,Y1,Px,Y1+3);
   S:=IntStr(D);
   TxtAt(Px-Length(S)*4+((X1-X0)div(NT-1))*12,Y1+5,S,Black);
   If(D=1)or(T<24)Then Begin
    SetColor(DarkGray);Line(Px,Y1+3,Px,Y1+18);
    TxtAt(Px+3,Y1+14,MonName(Mo)+IntStr(Yo),Black);
   End;
  End;
 End;
 { Bandes de capacite MaxCC au point de controle Beech Ck Station }
 If(PlotNode=3)and(PlotVar=0)Then Begin
  Y2:=PltY(3248);Y3:=PltY(4531);
  HatchRect(X0,Y3,X1,Y2,LightRed);
  HatchRect(X0,Y0,X1,Y3,Brown);
  SetColor(Red);Line(X0,Y2,X1,Y2);
 End;
 { Courbes }
 For A:=0 to NAlts-1 do If Alts[A].Valid Then Begin
  SetColor(AltColor(A));
  SetLineStyle(SolidLn,0,NormWidth);
  Px:=PltX(0);Py:=PltY(PlotVal(A,PlotVar,0));
  For T:=1 to NT-1 do Begin
   Xp:=PltX(T);Yp:=PltY(PlotVal(A,PlotVar,T));
   Line(Px,Py,Xp,Yp);
   If((PlotNode=3)and(PlotVar=0)and(A=1)and(T mod 24=0))Then Begin
    SetColor(Green);
    Line(Xp,Yp-3,Xp-3,Yp+2);Line(Xp-3,Yp+2,Xp+3,Yp+2);Line(Xp+3,Yp+2,Xp,Yp-3);
   End;
   Px:=Xp;Py:=Yp;
  End;
 End;
 If(PlotNode=3)and(PlotVar=0)Then Begin
  For A:=0 to NAlts-1 do If Alts[A].Valid Then Begin
   SetColor(DarkGray);SetLineStyle(DashedLn,0,NormWidth);
   Px:=PltX(0);Py:=PltY(Res[A].CumLoc[0]);
   For T:=1 to NT-1 do Begin
    Xp:=PltX(T);Yp:=PltY(Res[A].CumLoc[T]);Line(Px,Py,Xp,Yp);Px:=Xp;Py:=Yp;
   End;
   Break;
  End;
  SetLineStyle(SolidLn,0,NormWidth);
 End;
 If(PlotNode=1)and(PlotVar=0)Then Begin
  SetColor(Blue);
  Px:=PltX(0);Py:=PltY(InfS[0]);
  For T:=1 to NT-1 do Begin
   Xp:=PltX(T);Yp:=PltY(InfS[T]);Line(Px,Py,Xp,Yp);Px:=Xp;Py:=Yp;
  End;
 End;
 { Temps de simulation et temps affiche }
 SetColor(Black);SetLineStyle(DashedLn,0,NormWidth);
 Line(PltX(SimStart),Y0,PltX(SimStart),Y1);
 SetLineStyle(SolidLn,0,NormWidth);
 SetColor(Blue);Line(PltX(DispT),Y0,PltX(DispT),Y1);
 { Legende }
 Ly:=Y1+28;
 N:=0;
 For A:=0 to NAlts-1 do If Alts[A].Valid Then Begin
  If PlotVar=1 Then S:=Tr('Elev','Elev') Else If PlotVar=2 Then S:='Stor' Else S:='Flow';
  S:=NodeName(PlotNode)+'.'+Alts[A].Name+'-0.'+S;
  If(PlotNode=1)and(PlotVar=0)Then S:=NodeName(PlotNode)+'.'+Alts[A].Name+'-0.'+Tr('L\qcher','Release');
  SetColor(AltColor(A));Line(X+14,Ly+3,X+44,Ly+3);
  TxtAt(X+52,Ly,S,Black);
  Inc(Ly,10);Inc(N);
 End;
 If(PlotNode=1)and(PlotVar=0)Then Begin
  SetColor(Blue);Line(X+14,Ly+3,X+44,Ly+3);
  TxtAt(X+52,Ly,'Sayers.'+Tr('D\ebit entrant','Inflow'),Black);Inc(Ly,10);
 End;
 If(PlotNode=3)and(PlotVar=0)Then Begin
  SetColor(DarkGray);SetLineStyle(DashedLn,0,NormWidth);Line(X+14,Ly+3,X+44,Ly+3);
  SetLineStyle(SolidLn,0,NormWidth);
  TxtAt(X+52,Ly,'Beech Ck Station.CUMLOC '+Tr('(d\ebits locaux)','(local flows)'),Black);Inc(Ly,10);
 End;
 SetColor(Black);SetLineStyle(DashedLn,0,NormWidth);Line(X+14,Ly+3,X+44,Ly+3);
 SetLineStyle(SolidLn,0,NormWidth);
 TxtAt(X+52,Ly,Tr('D\ebut de la simulation','Time of Simulation'),Black);
 Inc(Ly,10);
 SetColor(Blue);Line(X+14,Ly+3,X+44,Ly+3);
 TxtAt(X+52,Ly,Tr('Temps affich\e','Display time'),Black);
 DrawPlotRO;
End;

Procedure PlotClick;
Var X,Y:Integer;
Begin
 X:=WX[wPlot];Y:=WY[wPlot];
 If PlotNode=1 Then Begin
  If Inside(MX,MY,X+8,Y+WTitle+2,X+88,Y+WTitle+17)Then PlotVar:=0;
  If Inside(MX,MY,X+92,Y+WTitle+2,X+172,Y+WTitle+17)Then PlotVar:=1;
  If Inside(MX,MY,X+176,Y+WTitle+2,X+256,Y+WTitle+17)Then PlotVar:=2;
 End;
 If Inside(MX,MY,X+WW[wPlot]-60,Y+WTitle+2,X+WW[wPlot]-36,Y+WTitle+17)Then Begin
  PlotNode:=(PlotNode+NNodes-1)mod NNodes;
  If PlotNode<>1 Then PlotVar:=0;
 End;
 If Inside(MX,MY,X+WW[wPlot]-32,Y+WTitle+2,X+WW[wPlot]-8,Y+WTitle+17)Then Begin
  PlotNode:=(PlotNode+1)mod NNodes;
  If PlotNode<>1 Then PlotVar:=0;
 End;
 Dirty:=True;
End;

Procedure PlotKey;Begin
 If(KeyExt=0)and(KeyCh=#27)Then WinClose(wPlot);
 If KeyExt=75 Then Begin PlotNode:=(PlotNode+NNodes-1)mod NNodes;If PlotNode<>1 Then PlotVar:=0;End;
 If KeyExt=77 Then Begin PlotNode:=(PlotNode+1)mod NNodes;If PlotNode<>1 Then PlotVar:=0;End;
 Dirty:=True;
End;

{ ------------------------------------------------------------------ }
{ Editeur de reservoir                                                }
{ ------------------------------------------------------------------ }

Function RuleDefault(K:Integer):Double;Begin
 Case K of
  rkMin:RuleDefault:=250;
  rkMax:RuleDefault:=2000;
  rkCC:RuleDefault:=3248;
  rkIncr:RuleDefault:=500;
  rkDecr:RuleDefault:=400;
  Else RuleDefault:=0;
 End;
End;

Function GateAt(E,Tot:Double;Setting:Double):Double;Begin
 If(Tot<=0)or(Setting<=0)Then GateAt:=0
  Else GateAt:=Tot*Exp(0.6*Ln(Setting/15));
End;

Procedure BuildTree;
Var Zn,Rn:Integer;
Begin
 TreeN:=0;
 For Zn:=NZones-1 downto 0 do Begin
  TreeZ[TreeN]:=Zn;TreeR[TreeN]:=-1;Inc(TreeN);
  For Rn:=0 to WM.Ops[ResOp].Z[Zn].N-1 do Begin
   TreeZ[TreeN]:=Zn;TreeR[TreeN]:=Rn;Inc(TreeN);
  End;
 End;
End;

Function TreeVis:Integer;Begin
 TreeVis:=(WH[wRes]-66-110-4)div 12;
End;

Procedure ResOpen(Tab:Integer);Begin
 If Not WOpen[wRes]Then Begin
  WM:=M;
  ResOp:=Alts[ActAlt].OpSet;
  ResPhy:=1;ResSelZ:=2;ResSelR:=-1;ResTree:=0;ResTbl:=0;ResObs:=0;
 End;
 ResTab:=Tab;
 WinShow(wRes);
End;

Procedure ResApply;Begin
 M:=WM;
 Alts[ActAlt].OpSet:=ResOp;
 DoCompute;
 RepBuild;
 Stat(Tr('Modifications appliqu\ees et simulation recalcul\ee',
         'Changes applied and simulation recomputed'));
End;

Procedure RuleIcon(X,Y,K:Integer);
Var C:Integer;
Begin
 Case K of
  rkMin:C:=Blue;
  rkMax:C:=Red;
  rkCC:C:=Red;
  rkIncr,rkDecr:C:=Green;
  rkInd:C:=Magenta;
  Else C:=Brown;
 End;
 FillR(X,Y,X+6,Y+6,C);
 SetColor(Black);Rectangle(X,Y,X+6,Y+6);
End;

Procedure DrawResPhys(X,Y:Integer);
Const Nm:Array[0..4] of String[24]=('Sayers','Pool','Dam at Bald Eagle Creek','Main Gates','Spillway Diversion');
      Ind:Array[0..4] of Integer=(0,1,1,2,1);
Var
 DX,DY,I,J,Ht:Integer;
 S:String;
 E,St:Double;
Begin
 Ht:=WH[wRes];
 FieldBox(X+12,Y+63,X+190,Y+Ht-42);
 For I:=0 to 4 do Begin
  If I=ResPhy Then Begin
   FillR(X+14,Y+66+I*12,X+188,Y+66+I*12+11,Blue);
   TxtAt(X+36+Ind[I]*12,Y+68+I*12,Nm[I],White);
  End
   Else TxtAt(X+36+Ind[I]*12,Y+68+I*12,Nm[I],Black);
  SetColor(Blue);
  If I=0 Then Begin Line(X+22,Y+68+I*12,X+27,Y+76+I*12);Line(X+27,Y+76+I*12,X+17,Y+76+I*12);Line(X+17,Y+76+I*12,X+22,Y+68+I*12);End
  Else FillR(X+20+Ind[I]*12,Y+69+I*12,X+26+Ind[I]*12,Y+75+I*12,Cyan);
 End;
 DX:=X+198;DY:=Y+63;
 Case ResPhy of
  0:Begin
   TxtAt(DX,DY+2,Tr('R\eservoir Sayers','Reservoir Sayers'),Blue);
   E:=InitElev;St:=Interp(WM.PE,WM.PS,PoolN,E);
   TxtAt(DX,DY+20,Tr('\El\evation initiale : ','Initial elevation: ')+RStr(E,2)+' ft',Black);
   TxtAt(DX,DY+32,Tr('Stockage initial   : ','Initial storage  : ')+RStr(St,0)+' ac-ft',Black);
   TxtAt(DX,DY+44,Tr('Capacit\e des vannes : ','Gate capacity   : ')+RStr(OutCap(E),0)+' cfs',Black);
   TxtAt(DX,DY+56,Tr('Sommet du barrage   : ','Top of dam       : ')+RStr(WM.TopDam,1)+' ft',Black);
   TxtAt(DX,DY+72,Tr('Stockage par zone (jeu ','Storage by zone (set ')+WM.Ops[ResOp].Name+') :',Black);
   For I:=NZones-1 downto 0 do Begin
    St:=Interp(WM.PE,WM.PS,PoolN,WM.Ops[ResOp].Z[I].Top);
    TxtAt(DX+8,DY+84+(NZones-1-I)*12,PadR(ZoneName(I),19)+PadL(RStr(St,0),8)+' ac-ft',Black);
   End;
  End;
  1:Begin
   TxtAt(DX,DY+2,Tr('Sayers-Pool : \El\evation-Stockage-Surface','Sayers-Pool: Elevation-Storage-Area'),Blue);
   PanelBox(DX,DY+16,DX+99,DY+28,True);TxtC(DX,DX+99,DY+19,'Elev. (ft)',Black);
   PanelBox(DX+100,DY+16,DX+203,DY+28,True);TxtC(DX+100,DX+203,DY+19,Tr('Stock. ac-ft','Stor. ac-ft'),Black);
   PanelBox(DX+204,DY+16,DX+299,DY+28,True);TxtC(DX+204,DX+299,DY+19,Tr('Surf. acre','Area acre'),Black);
   For I:=0 to PoolN-1 do Begin
    FieldBox(DX,DY+29+I*12,DX+99,DY+29+I*12+12);
    FieldBox(DX+100,DY+29+I*12,DX+203,DY+29+I*12+12);
    FieldBox(DX+204,DY+29+I*12,DX+299,DY+29+I*12+12);
    TxtAt(DX+92-Length(RStr(WM.PE[I],2))*8+8,DY+32+I*12,RStr(WM.PE[I],2),Black);
    TxtAt(DX+196-Length(RStr(WM.PS[I],1))*8+8,DY+32+I*12,RStr(WM.PS[I],1),Black);
    TxtAt(DX+292-Length(RStr(WM.PA[I],1))*8+8,DY+32+I*12,RStr(WM.PA[I],1),Black);
   End;
   TxtAt(DX,DY+146,Tr('Cliquez une cellule pour la modifier.','Click a cell to edit it.'),DarkGray);
  End;
  2:Begin
   TxtAt(DX,DY+2,Tr('Sayers-Barrage de Bald Eagle Creek','Sayers-Dam at Bald Eagle Creek'),Blue);
   TxtAt(DX,DY+22,Tr('\El\evation du sommet du barrage (ft) :','Top of dam elevation (ft):'),Black);
   FieldBox(DX,DY+34,DX+99,DY+47);
   TxtAt(DX+92-Length(RStr(WM.TopDam,2))*8+8,DY+37,RStr(WM.TopDam,2),Black);
   TxtAt(DX,DY+58,Tr('Sorties : Main Gates (1 type)','Outlets: Main Gates (1 type)'),Black);
   TxtAt(DX,DY+70,Tr('D\eversoir : Spillway Diversion','Spillway: Spillway Diversion'),Black);
   TxtAt(DX,DY+92,Tr('Un d\ebordement du barrage est signal\e','A dam overtopping is reported'),DarkGray);
   TxtAt(DX,DY+102,Tr('lorsque l''\el\evation d\epasse le sommet.','when the pool exceeds the top of dam.'),DarkGray);
  End;
  3:Begin
   TxtAt(DX,DY+2,'Sayers-Dam at Bald Eagle Creek-Main Gates',Blue);
   TxtAt(DX,DY+16,Tr('Nombre de vannes de ce type : 1','Number of gates of this type: 1'),Black);
   PanelBox(DX,DY+30,DX+47,DY+42,True);TxtC(DX,DX+47,DY+33,'Elev',Black);
   For J:=0 to 3 do Begin
    PanelBox(DX+48+J*48,DY+30,DX+95+J*48,DY+42,True);
    Case J of 0:S:='1.00';1:S:='5.00';2:S:='9.00';Else S:='11.00';End;
    TxtC(DX+48+J*48,DX+95+J*48,DY+33,S,Black);
   End;
   PanelBox(DX+240,DY+30,DX+303,DY+42,True);TxtC(DX+240,DX+303,DY+33,'Total',Black);
   For I:=0 to 10 do If ResTbl+I<GateN Then Begin
    E:=WM.GE[ResTbl+I];
    FieldBox(DX,DY+43+I*12,DX+47,DY+43+I*12+12);
    TxtAt(DX+40-Length(RStr(E,0))*8+8,DY+46+I*12,RStr(E,0),Black);
    For J:=0 to 3 do Begin
     FieldBox(DX+48+J*48,DY+43+I*12,DX+95+J*48,DY+43+I*12+12);
     Case J of 0:St:=1;1:St:=5;2:St:=9;Else St:=11;End;
     S:=RStr(GateAt(E,WM.GT[ResTbl+I],St),0);
     TxtAt(DX+88+J*48-Length(S)*8+8,DY+46+I*12,S,DarkGray);
    End;
    FieldBox(DX+240,DY+43+I*12,DX+303,DY+43+I*12+12);
    S:=RStr(WM.GT[ResTbl+I],0);
    TxtAt(DX+296-Length(S)*8+8,DY+46+I*12,S,Black);
   End;
   VScrollDraw(DX+306,DY+30,DY+43+11*12,ResTbl,GateN,11);
   TxtAt(DX,DY+182,Tr('Cliquez Elev ou Total pour modifier.','Click Elev or Total to edit.'),DarkGray);
  End;
  Else Begin
   TxtAt(DX,DY+2,'Sayers-Spillway Diversion',Blue);
   TxtAt(DX,DY+22,Tr('D\eversoir de secours : la capacit\e de','Emergency spillway: the capacity above'),Black);
   TxtAt(DX,DY+34,Tr('sortie est extrapol\ee au-dessus de','the last tabulated elevation is'),Black);
   TxtAt(DX,DY+46,Tr('la derni\gre \el\evation de la table.','extrapolated from the gate table.'),Black);
  End;
 End;
End;

Procedure DrawResOps(X,Y:Integer);
Const SubT:Array[0..5] of String[12]=('Zone-Rules','Rel. Alloc.','Outages','Stor. Credit','Dec. Sched.','Proj. Elev');
Var
 DX,DY,I,Wd,Ht,Tx,Z,R,Vis,Right:Integer;
 S:String;
 Sel:Boolean;
Begin
 Wd:=WW[wRes];Ht:=WH[wRes];Right:=X+Wd-14;
 TxtAt(X+12,Y+65,Tr('Jeu d''op\erations','Operation Set'),Black);
 FieldBox(X+150,Y+62,X+350,Y+76);
 TxtW(X+154,Y+66,176,WM.Ops[ResOp].Name,Black);
 Button(X+338,Y+63,X+349,Y+75,'',True);Tri(X+344,Y+69,1,Black);
 TxtAt(X+12,Y+81,'Description',Black);
 FieldBox(X+100,Y+78,Right,Y+92);
 TxtW(X+104,Y+82,Right-X-108,WM.Ops[ResOp].Descr,Black);
 Tx:=X+12;
 For I:=0 to 5 do Begin
  Wd:=Length(SubT[I])*8+8;
  If I=0 Then Begin
   PanelBox(Tx,Y+96,Tx+Wd,Y+110,True);
   TxtC(Tx,Tx+Wd,Y+100,SubT[I],Black);
  End
   Else Begin
   FillR(Tx,Y+98,Tx+Wd,Y+109,LightGray);
   TxtC(Tx,Tx+Wd,Y+100,SubT[I],DarkGray);
  End;
  Tx:=Tx+Wd+2;
 End;
 Ht:=WH[wRes];Wd:=WW[wRes];
 BuildTree;
 Vis:=TreeVis;
 FieldBox(X+12,Y+112,X+248,Y+Ht-66);
 For I:=0 to Vis-1 do If ResTree+I<TreeN Then Begin
  Z:=TreeZ[ResTree+I];R:=TreeR[ResTree+I];
  Sel:=(Z=ResSelZ)and(R=ResSelR);
  If Sel Then FillR(X+14,Y+114+I*12,X+246,Y+114+I*12+11,Blue);
  If R<0 Then Begin
   SetColor(Cyan);FillR(X+18,Y+117+I*12,X+26,Y+122+I*12,Cyan);
   If Sel Then TxtW(X+32,Y+116+I*12,208,ZoneName(Z),White)
          Else TxtW(X+32,Y+116+I*12,208,ZoneName(Z),Black);
  End
   Else Begin
   RuleIcon(X+32,Y+117+I*12,WM.Ops[ResOp].Z[Z].R[R].Kind);
   S:=RuleLabel(WM.Ops[ResOp].Z[Z].R[R].Kind,Round(WM.Ops[ResOp].Z[Z].R[R].P1));
   If Sel Then TxtW(X+44,Y+116+I*12,196,S,White)
          Else TxtW(X+44,Y+116+I*12,196,S,Black);
  End;
 End;
 VScrollDraw(X+250,Y+112,Y+Ht-66,ResTree,TreeN,Vis);
 Button(X+12,Y+Ht-62,X+72,Y+Ht-44,Tr('Ajouter','Add'),True);
 Button(X+76,Y+Ht-62,X+136,Y+Ht-44,Tr('Retirer','Remove'),ResSelR>=0);
 Button(X+140,Y+Ht-62,X+192,Y+Ht-44,Tr('Haut','Up'),ResSelR>0);
 Button(X+196,Y+Ht-62,X+248,Y+Ht-44,Tr('Bas','Down'),(ResSelR>=0)and(ResSelR<WM.Ops[ResOp].Z[ResSelZ].N-1));
 { Detail }
 DX:=X+272;DY:=Y+112;
 If ResSelR<0 Then Begin
  TxtAt(DX,DY+2,Tr('Zone : ','Zone: ')+ZoneName(ResSelZ),Blue);
  TxtAt(DX,DY+16,Tr('Op\gre le d\ebit de : Sayers','Operates release from: Sayers'),Black);
  TxtAt(DX,DY+34,Tr('\El\evation du sommet (ft) :','Top elevation (ft):'),Black);
  FieldBox(DX,DY+46,DX+99,DY+59);
  S:=RStr(WM.Ops[ResOp].Z[ResSelZ].Top,2);
  TxtAt(DX+92-Length(S)*8+8,DY+49,S,Black);
  If ResSelZ>0 Then S:=RStr(WM.Ops[ResOp].Z[ResSelZ-1].Top,2) Else S:='-';
  TxtAt(DX,DY+68,Tr('\El\evation de base (ft)  : ','Base elevation (ft): ')+S,Black);
  S:=RStr(Interp(WM.PE,WM.PS,PoolN,WM.Ops[ResOp].Z[ResSelZ].Top),0);
  TxtAt(DX,DY+80,Tr('Stock. au sommet : ','Storage at top : ')+S+' ac-ft',Black);
  TxtAt(DX,DY+92,Tr('R\ggles dans zone  : ','Rules in zone : ')+IntStr(WM.Ops[ResOp].Z[ResSelZ].N),Black);
  TxtAt(DX,DY+116,Tr('Les r\ggles du haut de la liste sont','Rules at the top of the list have'),DarkGray);
  TxtAt(DX,DY+126,Tr('les plus prioritaires.','the highest priority.'),DarkGray);
 End
  Else
 Begin
  With WM.Ops[ResOp].Z[ResSelZ].R[ResSelR]do Begin
   TxtAt(DX,DY+2,Tr('R\ggle : ','Rule: ')+RuleLabel(Kind,Round(P1)),Blue);
   TxtW(DX,DY+14,Right-DX,Tr('Type : ','Type: ')+RuleTypeName(Kind),Black);
   TxtAt(DX,DY+26,Tr('Zone : ','Zone: ')+ZoneName(ResSelZ),Black);
   Case Kind of
    rkMin,rkMax,rkCC,rkIncr,rkDecr:Begin
     If Kind=rkCC Then TxtAt(DX,DY+42,Tr('Point de contr\ole : Beech Ck Station','Control point: Beech Ck Station'),Black);
     Case Kind of
      rkMin:S:=Tr('D\ebit minimal (cfs)','Minimum release (cfs)');
      rkMax:S:=Tr('D\ebit maximal (cfs)','Maximum release (cfs)');
      rkCC:S:=Tr('D\ebit max. au point (cfs)','Max flow at point (cfs)');
      rkIncr:S:=Tr('Taux de hausse (cfs/h)','Rate of increase (cfs/hr)');
      Else S:=Tr('Taux de baisse (cfs/h)','Rate of decrease (cfs/hr)');
     End;
     TxtAt(DX,DY+60,S,Black);
     FieldBox(DX+8,DY+72,DX+107,DY+85);
     S:=RStr(P1,1);
     TxtAt(DX+100-Length(S)*8+8,DY+75,S,Black);
     TxtAt(DX,DY+96,Tr('Cliquez la valeur pour la modifier.','Click the value to edit it.'),DarkGray);
    End;
    rkInd:Begin
     TxtAt(DX,DY+40,Tr('Enveloppe de surcharge induite','Induced surcharge envelope'),Black);
     PanelBox(DX+8,DY+52,DX+107,DY+64,True);TxtC(DX+8,DX+107,DY+55,'Elev. (ft)',Black);
     PanelBox(DX+108,DY+52,DX+207,DY+64,True);TxtC(DX+108,DX+207,DY+55,Tr('D\ebit (cfs)','Release (cfs)'),Black);
     For I:=0 to WM.Ops[ResOp].ISCnt-1 do Begin
      FieldBox(DX+8,DY+65+I*12,DX+107,DY+65+I*12+12);
      FieldBox(DX+108,DY+65+I*12,DX+207,DY+65+I*12+12);
      S:=RStr(WM.Ops[ResOp].ISE[I],2);
      TxtAt(DX+100-Length(S)*8+8,DY+68+I*12,S,Black);
      S:=RStr(WM.Ops[ResOp].ISR[I],1);
      TxtAt(DX+200-Length(S)*8+8,DY+68+I*12,S,Black);
     End;
    End;
    rkPeak:Begin
     TxtAt(DX,DY+46,Tr('Maintient le plus fort d\ebit rel\qche','Keeps the highest release reached'),Black);
     TxtAt(DX,DY+58,Tr('tant que le niveau reste \elev\e.','while the pool remains high.'),Black);
    End;
   End;
  End;
 End;
End;

Procedure DrawResObs(X,Y:Integer);
Var
 I,T,Ht,Vis:Integer;
 S:String;
Begin
 Ht:=WH[wRes];
 Vis:=(Ht-36-6-82)div 12;
 TxtAt(X+12,Y+63,Tr('Donn\ees observ\ees (p\eriode de retour)','Observed data (lookback period)'),Blue);
 PanelBox(X+12,Y+75,X+148,Y+87,True);TxtC(X+12,X+148,Y+78,Tr('Date-Heure','Date-Time'),Black);
 PanelBox(X+149,Y+75,X+265,Y+87,True);TxtC(X+149,X+265,Y+78,Tr('Entrant (cfs)','Inflow (cfs)'),Black);
 PanelBox(X+266,Y+75,X+382,Y+87,True);TxtC(X+266,X+382,Y+78,Tr('Local (cfs)','Local (cfs)'),Black);
 PanelBox(X+383,Y+75,X+499,Y+87,True);TxtC(X+383,X+499,Y+78,Tr('Elev. (ft)','Elev. (ft)'),Black);
 For I:=0 to Vis-1 do Begin
  T:=ResObs+I;
  If T<=SimStart Then Begin
   FieldBox(X+12,Y+88+I*12,X+148,Y+88+I*12+12);
   FieldBox(X+149,Y+88+I*12,X+265,Y+88+I*12+12);
   FieldBox(X+266,Y+88+I*12,X+382,Y+88+I*12+12);
   FieldBox(X+383,Y+88+I*12,X+499,Y+88+I*12+12);
   TxtAt(X+16,Y+91+I*12,FmtDT(T),Black);
   S:=RStr(InfS[T],1);TxtAt(X+260-Length(S)*8+8,Y+91+I*12,S,Black);
   S:=RStr(MarsS[T]+BeecS[T],1);TxtAt(X+377-Length(S)*8+8,Y+91+I*12,S,Black);
   S:=RStr(InitElev,2);TxtAt(X+494-Length(S)*8+8,Y+91+I*12,S,Black);
  End;
 End;
 VScrollDraw(X+501,Y+75,Y+88+Vis*12,ResObs,SimStart+1,Vis);
End;

Procedure DrawRes;
Const TabT:Array[0..2] of String[16]=('Physical','Operations','Observed Data');
      TabW:Array[0..2] of Integer=(84,100,128);
Var
 X,Y,Wd,Ht,I,Tx:Integer;
 TabF:Array[0..2] of String[16];
Begin
 X:=WX[wRes];Y:=WY[wRes];Wd:=WW[wRes];Ht:=WH[wRes];
 TabF[0]:='Physique';TabF[1]:=Acc('Op\erations');TabF[2]:=Acc('Donn\ees obs.');
 WinFrame(wRes,Tr('\Editeur de r\eservoir - R\eseau : ','Reservoir Editor - Network: ')+Alts[ActAlt].Name+'-:Base2003');
 TxtAt(X+10,Y+25,Tr('R\eservoir','Reservoir'),Black);
 FieldBox(X+86,Y+22,X+206,Y+36);
 TxtAt(X+90,Y+26,'Sayers',Black);
 Button(X+194,Y+23,X+205,Y+35,'',True);Tri(X+200,Y+29,1,Black);
 TxtAt(X+216,Y+25,'Description',Black);
 FieldBox(X+304,Y+22,X+Wd-86,Y+36);
 TxtW(X+308,Y+26,Wd-402,Tr('Barrage Sayers, Bald Eagle Creek','Sayers Dam, Bald Eagle Creek'),Black);
 TxtAt(X+Wd-76,Y+26,Tr('1 de 1','1 of 1'),Black);
 PanelBox(X+6,Y+57,X+Wd-7,Y+Ht-36,True);
 Tx:=X+8;
 For I:=0 to 2 do Begin
  If I<>ResTab Then Begin
   PanelBox(Tx,Y+42,Tx+TabW[I],Y+58,True);
   If Eng Then TxtC(Tx,Tx+TabW[I],Y+47,TabT[I],Black) Else TxtC(Tx,Tx+TabW[I],Y+47,TabF[I],Black);
  End;
  Inc(Tx,TabW[I]+2);
 End;
 Tx:=X+8;
 For I:=0 to 2 do Begin
  If I=ResTab Then Begin
   PanelBox(Tx-2,Y+39,Tx+TabW[I]+2,Y+60,True);
   FillR(Tx,Y+57,Tx+TabW[I],Y+60,LightGray);
   If Eng Then TxtC(Tx,Tx+TabW[I],Y+45,TabT[I],Black) Else TxtC(Tx,Tx+TabW[I],Y+45,TabF[I],Black);
  End;
  Inc(Tx,TabW[I]+2);
 End;
 Case ResTab of
  0:DrawResPhys(X,Y);
  1:DrawResOps(X,Y);
  Else DrawResObs(X,Y);
 End;
 Button(X+Wd-246,Y+Ht-30,X+Wd-180,Y+Ht-10,'OK',True);
 Button(X+Wd-172,Y+Ht-30,X+Wd-106,Y+Ht-10,Tr('Annuler','Cancel'),True);
 Button(X+Wd-98,Y+Ht-30,X+Wd-14,Y+Ht-10,Tr('Appliquer','Apply'),True);
End;

Function CellEdit(Title,Prompt:String;Var V:Double;Lo,Hi:Double;Dec:Integer):Boolean;
Var T:Double;
Begin
 CellEdit:=False;
 T:=V;
 If InputNum(Title,Prompt,T,Dec)Then Begin
  If(T<Lo)or(T>Hi)Then
   Stat(Tr('Valeur hors limites (','Value out of range (')+RStr(Lo,1)+' .. '+RStr(Hi,1)+')')
  Else Begin V:=T;CellEdit:=True;End;
 End;
End;

Procedure ResClickPhys(X,Y:Integer);
Var
 DX,DY,I,J:Integer;
 Lo,Hi:Double;
Begin
 If Inside(MX,MY,X+12,Y+66,X+190,Y+66+5*12-1)Then Begin
  ResPhy:=(MY-Y-66)div 12;
  ResTbl:=0;
  Exit;
 End;
 DX:=X+198;DY:=Y+63;
 Case ResPhy of
  1:For I:=0 to PoolN-1 do Begin
   If I>0 Then Lo:=WM.PE[I-1]+0.01 Else Lo:=0;
   If I<PoolN-1 Then Hi:=WM.PE[I+1]-0.01 Else Hi:=2000;
   If Inside(MX,MY,DX,DY+29+I*12,DX+99,DY+29+I*12+12)Then
    CellEdit('Elevation','Elevation (ft) :',WM.PE[I],Lo,Hi,2);
   If I>0 Then Lo:=WM.PS[I-1]+0.1 Else Lo:=0;
   If I<PoolN-1 Then Hi:=WM.PS[I+1]-0.1 Else Hi:=100000000;
   If Inside(MX,MY,DX+100,DY+29+I*12,DX+203,DY+29+I*12+12)Then
    CellEdit(Tr('Stockage','Storage'),Tr('Stockage (ac-ft) :','Storage (ac-ft):'),WM.PS[I],Lo,Hi,1);
   If Inside(MX,MY,DX+204,DY+29+I*12,DX+299,DY+29+I*12+12)Then
    CellEdit(Tr('Surface','Area'),Tr('Surface (acre) :','Area (acre):'),WM.PA[I],0,1000000,1);
  End;
  2:If Inside(MX,MY,DX,DY+34,DX+99,DY+47)Then
     CellEdit(Tr('Barrage','Dam'),Tr('Sommet du barrage (ft) :','Top of dam (ft):'),WM.TopDam,0,2000,2);
  3:Begin
   ResTbl:=VScrollClick(DX+306,DY+30,DY+43+11*12,ResTbl,GateN,11);
   For I:=0 to 10 do If ResTbl+I<GateN Then Begin
    J:=ResTbl+I;
    If J>0 Then Lo:=WM.GE[J-1]+0.01 Else Lo:=0;
    If J<GateN-1 Then Hi:=WM.GE[J+1]-0.01 Else Hi:=2000;
    If Inside(MX,MY,DX,DY+43+I*12,DX+47,DY+43+I*12+12)Then
     CellEdit('Elevation','Elevation (ft) :',WM.GE[J],Lo,Hi,1);
    If Inside(MX,MY,DX+240,DY+43+I*12,DX+303,DY+43+I*12+12)Then
     CellEdit(Tr('Capacit\e','Capacity'),Tr('Capacit\e totale (cfs) :','Total capacity (cfs):'),WM.GT[J],0,1000000,0);
   End;
  End;
 End;
End;

Procedure ResClickOps(X,Y:Integer);
Var
 DX,DY,I,Z,R,K,Vis,N:Integer;
 Lo,Hi:Double;
 TmpR:TRule;
Begin
 Vis:=TreeVis;
 BuildTree;
 ResTree:=VScrollClick(X+250,Y+112,Y+WH[wRes]-66,ResTree,TreeN,Vis);
 If Inside(MX,MY,X+150,Y+62,X+350,Y+76)Then Begin
  For I:=0 to NOps-1 do ChItems[I]:=WM.Ops[I].Name;
  ChN:=NOps;
  I:=ChoiceBox(Tr('Jeu d''op\erations','Operation set'),ResOp);
  If I>=0 Then Begin ResOp:=I;ResSelR:=-1;ResTree:=0;End;
  Exit;
 End;
 If Inside(MX,MY,X+14,Y+114,X+246,Y+113+Vis*12)Then Begin
  I:=(MY-Y-114)div 12;
  If ResTree+I<TreeN Then Begin
   ResSelZ:=TreeZ[ResTree+I];
   ResSelR:=TreeR[ResTree+I];
  End;
  Exit;
 End;
 { Boutons sous l'arbre }
 If Inside(MX,MY,X+12,Y+WH[wRes]-62,X+72,Y+WH[wRes]-44)Then Begin
  If WM.Ops[ResOp].Z[ResSelZ].N>=MaxRules Then Begin
   Stat(Tr('Nombre maximal de r\ggles atteint dans cette zone','Maximum number of rules reached in this zone'));
   Exit;
  End;
  For I:=0 to 6 do ChItems[I]:=RuleTypeName(I);
  ChN:=7;
  K:=ChoiceBox(Tr('Ajouter une r\ggle \a ','Add a rule to ')+ZoneName(ResSelZ),0);
  If K>=0 Then Begin
   N:=WM.Ops[ResOp].Z[ResSelZ].N;
   WM.Ops[ResOp].Z[ResSelZ].R[N].Kind:=K;
   WM.Ops[ResOp].Z[ResSelZ].R[N].P1:=RuleDefault(K);
   WM.Ops[ResOp].Z[ResSelZ].N:=N+1;
   ResSelR:=N;
  End;
  Exit;
 End;
 If(ResSelR>=0)and Inside(MX,MY,X+76,Y+WH[wRes]-62,X+136,Y+WH[wRes]-44)Then Begin
  N:=WM.Ops[ResOp].Z[ResSelZ].N;
  For I:=ResSelR to N-2 do WM.Ops[ResOp].Z[ResSelZ].R[I]:=WM.Ops[ResOp].Z[ResSelZ].R[I+1];
  WM.Ops[ResOp].Z[ResSelZ].N:=N-1;
  ResSelR:=-1;
  Exit;
 End;
 If(ResSelR>0)and Inside(MX,MY,X+140,Y+WH[wRes]-62,X+192,Y+WH[wRes]-44)Then Begin
  TmpR:=WM.Ops[ResOp].Z[ResSelZ].R[ResSelR];
  WM.Ops[ResOp].Z[ResSelZ].R[ResSelR]:=WM.Ops[ResOp].Z[ResSelZ].R[ResSelR-1];
  WM.Ops[ResOp].Z[ResSelZ].R[ResSelR-1]:=TmpR;
  Dec(ResSelR);
  Exit;
 End;
 If(ResSelR>=0)and(ResSelR<WM.Ops[ResOp].Z[ResSelZ].N-1)and
   Inside(MX,MY,X+196,Y+WH[wRes]-62,X+248,Y+WH[wRes]-44)Then Begin
  TmpR:=WM.Ops[ResOp].Z[ResSelZ].R[ResSelR];
  WM.Ops[ResOp].Z[ResSelZ].R[ResSelR]:=WM.Ops[ResOp].Z[ResSelZ].R[ResSelR+1];
  WM.Ops[ResOp].Z[ResSelZ].R[ResSelR+1]:=TmpR;
  Inc(ResSelR);
  Exit;
 End;
 { Detail }
 DX:=X+272;DY:=Y+112;
 Z:=ResSelZ;R:=ResSelR;
 If R<0 Then Begin
  If Inside(MX,MY,DX,DY+46,DX+99,DY+59)Then Begin
   If Z>0 Then Lo:=WM.Ops[ResOp].Z[Z-1].Top+0.01 Else Lo:=0;
   If Z<NZones-1 Then Hi:=WM.Ops[ResOp].Z[Z+1].Top-0.01 Else Hi:=2000;
   CellEdit(ZoneName(Z),Tr('\El\evation du sommet (ft) :','Top elevation (ft):'),WM.Ops[ResOp].Z[Z].Top,Lo,Hi,2);
  End;
 End
  Else
 Begin
  Case WM.Ops[ResOp].Z[Z].R[R].Kind of
   rkMin,rkMax,rkCC,rkIncr,rkDecr:
    If Inside(MX,MY,DX+8,DY+72,DX+107,DY+85)Then
     CellEdit(RuleTypeName(WM.Ops[ResOp].Z[Z].R[R].Kind),Tr('Valeur :','Value:'),
              WM.Ops[ResOp].Z[Z].R[R].P1,0,30000,1);
   rkInd:For I:=0 to WM.Ops[ResOp].ISCnt-1 do Begin
    If I>0 Then Lo:=WM.Ops[ResOp].ISE[I-1]+0.01 Else Lo:=0;
    If I<WM.Ops[ResOp].ISCnt-1 Then Hi:=WM.Ops[ResOp].ISE[I+1]-0.01 Else Hi:=2000;
    If Inside(MX,MY,DX+8,DY+65+I*12,DX+107,DY+65+I*12+12)Then
     CellEdit('Elevation','Elevation (ft) :',WM.Ops[ResOp].ISE[I],Lo,Hi,2);
    If Inside(MX,MY,DX+108,DY+65+I*12,DX+207,DY+65+I*12+12)Then
     CellEdit(Tr('D\ebit','Release'),Tr('D\ebit (cfs) :','Release (cfs):'),WM.Ops[ResOp].ISR[I],0,30000,1);
   End;
  End;
 End;
End;

Procedure ResClick;
Var
 X,Y,Wd,Ht,I,Tx:Integer;
 Const TabW:Array[0..2] of Integer=(84,100,128);
Begin
 X:=WX[wRes];Y:=WY[wRes];Wd:=WW[wRes];Ht:=WH[wRes];
 Tx:=X+8;
 For I:=0 to 2 do Begin
  If Inside(MX,MY,Tx,Y+40,Tx+TabW[I],Y+57)Then Begin ResTab:=I;Dirty:=True;Exit;End;
  Inc(Tx,TabW[I]+2);
 End;
 If Inside(MX,MY,X+Wd-246,Y+Ht-30,X+Wd-180,Y+Ht-10)Then Begin ResApply;WinClose(wRes);Exit;End;
 If Inside(MX,MY,X+Wd-172,Y+Ht-30,X+Wd-106,Y+Ht-10)Then Begin WinClose(wRes);Exit;End;
 If Inside(MX,MY,X+Wd-98,Y+Ht-30,X+Wd-14,Y+Ht-10)Then Begin ResApply;Exit;End;
 Case ResTab of
  0:ResClickPhys(X,Y);
  1:ResClickOps(X,Y);
  Else ResObs:=VScrollClick(X+501,Y+75,Y+88+((Ht-36-6-82)div 12)*12,ResObs,SimStart+1,(Ht-36-6-82)div 12);
 End;
 Dirty:=True;
End;

Procedure ResKey;Begin
 If(KeyExt=0)and(KeyCh=#27)Then WinClose(wRes);
 If(KeyExt=0)and(KeyCh=#9)Then ResTab:=(ResTab+1)mod 3;
 Dirty:=True;
End;

{ ------------------------------------------------------------------ }
{ Carte du reseau                                                     }
{ ------------------------------------------------------------------ }

Const
 NodeX:Array[0..NNodes-1] of Integer=(190,470,540,620,800,940);
 NodeY:Array[0..NNodes-1] of Integer=(720,450,400,350,170,140);
 LblDX:Array[0..NNodes-1] of Integer=(10,-62,8,-70,-84,-124);
 LblDY:Array[0..NNodes-1] of Integer=(8,-18,8,-20,-6,-20);
 PoolPX:Array[0..16] of Integer=(190,200,240,290,330,380,420,455,475,488,472,440,395,350,300,250,210);
 PoolPY:Array[0..16] of Integer=(722,700,665,625,600,560,515,470,450,462,490,535,585,625,665,700,725);

Var
 LnK:Array[0..31] of Byte;
 LnS,LnC:Array[0..31] of Integer;
 PtX,PtY:Array[0..255] of Integer;
 NLn,NPt:Integer;

Procedure LB(K:Integer);Begin
 LnK[NLn]:=K;LnS[NLn]:=NPt;LnC[NLn]:=0;Inc(NLn);
End;

Procedure LP(X,Y:Integer);Begin
 PtX[NPt]:=X;PtY[NPt]:=Y;Inc(NPt);Inc(LnC[NLn-1]);
End;

Procedure InitMapData;Begin
 NLn:=0;NPt:=0;
 { 0..3 : affluents ; 4..7 : troncons du reseau }
 LB(0);LP(40,70);LP(150,120);LP(260,110);LP(330,200);LP(430,290);LP(540,320);LP(620,350);
 LB(0);LP(30,540);LP(110,470);LP(220,400);LP(330,380);LP(430,400);LP(540,400);
 LB(0);LP(960,600);LP(930,480);LP(880,380);LP(850,270);LP(800,170);
 LB(0);LP(30,640);LP(90,700);LP(190,720);
 LB(1);LP(470,450);LP(505,430);LP(540,400);
 LB(1);LP(540,400);LP(620,350);
 LB(1);LP(620,350);LP(690,330);LP(730,260);LP(760,215);LP(800,170);
 LB(1);LP(800,170);LP(860,175);LP(900,150);LP(940,140);
 { ruisseaux secondaires }
 LB(2);LP(100,300);LP(180,330);LP(250,380);
 LB(2);LP(350,100);LP(380,160);LP(420,220);
 LB(2);LP(700,500);LP(660,440);LP(600,420);
 LB(2);LP(880,420);LP(780,340);LP(700,330);
 LB(2);LP(300,520);LP(380,480);LP(440,470);
 LB(2);LP(520,200);LP(580,260);LP(620,300);
 LB(2);LP(700,640);LP(640,560);LP(580,500);
 LB(2);LP(120,170);LP(200,230);LP(240,300);
 { limites de sous-bassins }
 LB(3);LP(60,500);LP(150,380);LP(330,330);LP(440,320);LP(500,400);LP(470,450);LP(300,640);LP(190,740);LP(60,640);
 LB(3);LP(330,330);LP(380,200);LP(520,150);LP(640,230);LP(700,320);LP(620,350);LP(540,400);LP(500,400);LP(440,320);
 LB(3);LP(640,230);LP(800,90);LP(960,100);LP(980,300);LP(900,420);LP(760,330);LP(700,320);
End;

Function MapX1:Integer;Begin MapX1:=ToolW;End;
Function MapY1:Integer;Begin MapY1:=TitleH+MenuH+ModH;End;
Function MapX2:Integer;Begin MapX2:=SW-PanelW-1;End;
Function MapY2:Integer;Begin MapY2:=SH-StatH-1;End;

Procedure FitView;
Var W,H:Double;
Begin
 W:=MapX2-MapX1-4;H:=MapY2-MapY1-4;
 VScale:=W/1000;
 If H/800<VScale Then VScale:=H/800;
 VOX:=500-(W/2)/VScale;
 VOY:=410-(H/2)/VScale;
End;

Function WSX(Wx:Integer):Integer;
Var V:Double;
Begin
 V:=(Wx-VOX)*VScale;
 If V>20000 Then V:=20000;
 If V<-20000 Then V:=-20000;
 WSX:=Round(V);
End;

Function WSY(Wy:Integer):Integer;
Var V:Double;
Begin
 V:=(Wy-VOY)*VScale;
 If V>20000 Then V:=20000;
 If V<-20000 Then V:=-20000;
 WSY:=Round(V);
End;

Procedure ZoomAt(F:Double;Sx,Sy:Integer);
Var Wx,Wy:Double;
Begin
 Wx:=VOX+(Sx-MapX1)/VScale;
 Wy:=VOY+(Sy-MapY1)/VScale;
 VScale:=VScale*F;
 If VScale<0.15 Then VScale:=0.15;
 If VScale>6 Then VScale:=6;
 VOX:=Wx-(Sx-MapX1)/VScale;
 VOY:=Wy-(Sy-MapY1)/VScale;
 Dirty:=True;
End;

Function LineFlow(I:Integer):Double;
Var A:Integer;
Begin
 LineFlow:=0;
 If I<=3 Then Begin
  Case I of
   0:LineFlow:=BeecS[DispT];
   1:LineFlow:=MarsS[DispT];
   2:LineFlow:=FishS[DispT];
   Else LineFlow:=InfS[DispT];
  End;
 End
  Else If(I>=4)and(I<=7)Then Begin
  A:=ActAlt;
  If Not Alts[A].Valid Then Begin LineFlow:=-1;Exit;End;
  LineFlow:=Res[A].Flow[I-3][DispT];
 End;
End;

Function NodeFlow(N:Integer):Double;Begin
 If Not Alts[ActAlt].Valid Then NodeFlow:=-1
  Else NodeFlow:=Res[ActAlt].Flow[N][DispT];
End;

Procedure DrawLineSet(Kind:Integer);
Var
 I,J,S,C,Mid:Integer;
 F:Double;
 Lbl:String;
 Col:Integer;
Begin
 For I:=0 to NLn-1 do If LnK[I]=Kind Then Begin
  S:=LnS[I];C:=LnC[I];
  Case Kind of
   0:Begin Col:=Brown;SetLineStyle(SolidLn,0,NormWidth);End;
   1:Begin Col:=Blue;
    If Module=modSetup Then SetLineStyle(SolidLn,0,NormWidth)
     Else SetLineStyle(SolidLn,0,ThickWidth);End;
   2:Begin Col:=LightCyan;SetLineStyle(SolidLn,0,NormWidth);End;
   Else Begin Col:=LightGray;SetLineStyle(DashedLn,0,NormWidth);End;
  End;
  If(Kind=1)and(Module=modSim)Then Begin
   F:=LineFlow(I);
   If F<0 Then Col:=Blue
    Else If F>=4500 Then Col:=Red
    Else If F>=3248 Then Col:=Magenta
    Else Col:=Blue;
  End;
  SetColor(Col);
  For J:=0 to C-2 do
   Line(WSX(PtX[S+J]),WSY(PtY[S+J]),WSX(PtX[S+J+1]),WSY(PtY[S+J+1]));
  If Kind=3 Then
   Line(WSX(PtX[S+C-1]),WSY(PtY[S+C-1]),WSX(PtX[S]),WSY(PtY[S]));
  SetLineStyle(SolidLn,0,NormWidth);
  If(Kind=0)and ShowLbl and(Module=modSim)Then Begin
   F:=LineFlow(I);
   If F>=0 Then Begin
    Mid:=S+C div 2;
    Lbl:=FlowLbl(F);
    TxtAt(WSX(PtX[Mid])+4,WSY(PtY[Mid])+3,Lbl,Brown);
   End;
  End;
 End;
End;

Procedure DrawMap;
Var
 I,X,Y,W,H,Wx,Wy:Integer;
 Pts:Array[0..16] of PointType;
 F:Double;
Begin
 W:=MapX2-MapX1;H:=MapY2-MapY1;
 FillR(MapX1-1,MapY1-1,MapX2+1,MapY2+1,White);
 Bevel(MapX1-2,MapY1-2,MapX2+1,MapY2+1,False);
 SetViewPort(MapX1,MapY1,MapX2,MapY2,True);
 FillR(0,0,W,H,White);
 If ShowGrid Then Begin
  SetColor(LightGray);
  Wx:=0;
  While Wx<=1000 do Begin Line(WSX(Wx),0,WSX(Wx),H);Inc(Wx,100);End;
  Wy:=0;
  While Wy<=800 do Begin Line(0,WSY(Wy),W,WSY(Wy));Inc(Wy,100);End;
 End;
 DrawLineSet(3);
 DrawLineSet(2);
 { Retenue du reservoir Sayers }
 For I:=0 to 16 do Begin
  Pts[I].X:=WSX(PoolPX[I]);Pts[I].Y:=WSY(PoolPY[I]);
 End;
 SetFillStyle(SolidFill,LightCyan);
 SetColor(Blue);
 FillPoly(17,Pts);
 DrawLineSet(0);
 DrawLineSet(1);
 { Noeuds }
 For I:=0 to NNodes-1 do Begin
  X:=WSX(NodeX[I]);Y:=WSY(NodeY[I]);
  If I=SelNode Then Begin
   SetColor(Blue);SetLineStyle(SolidLn,0,NormWidth);
   Rectangle(X-8,Y-8,X+8,Y+8);
  End;
  SetFillStyle(SolidFill,Red);
  SetColor(Black);
  FillEllipse(X,Y,5,5);
  If I=1 Then Begin
   SetColor(Black);Line(X-6,Y+6,X+6,Y-6);Line(X-6,Y+7,X+6,Y-5);
  End;
  If ShowNames Then Begin
   TxtAt(X+LblDX[I],Y+LblDY[I],NodeName(I),Red);
   If(Module=modSim)and ShowLbl Then Begin
    F:=NodeFlow(I);
    If F>=0 Then TxtAt(X+LblDX[I],Y+LblDY[I]+9,FlowLbl(F)+' cfs',Blue);
   End;
  End;
 End;
 SetViewPort(0,0,SW-1,SH-1,True);
End;

{ ------------------------------------------------------------------ }
{ Titre, menus, rangee de module, outils, panneau, barre d'etat       }
{ ------------------------------------------------------------------ }

Var CurM,NMn:Integer;

Procedure MBegin(Title:String);Begin
 CurM:=NMn;
 Menus[CurM].Title:=Copy(Title,1,14);
 Menus[CurM].N:=0;
 Inc(NMn);
End;

Procedure MAdd(Cap,Sc:String;Chk,En:Boolean);Begin
 With Menus[CurM]do Begin
  It[N].Cap:=Copy(Cap,1,34);
  It[N].Sc:=Copy(Sc,1,10);
  It[N].Sep:=False;It[N].Chk:=Chk;It[N].En:=En;
  Inc(N);
 End;
End;

Procedure MSep;Begin
 With Menus[CurM]do Begin
  It[N].Cap:='';It[N].Sc:='';It[N].Sep:=True;It[N].Chk:=False;It[N].En:=False;
  Inc(N);
 End;
End;

Function ModuleName(I:Integer):String;Begin
 Case I of
  0:ModuleName:=Tr('Bassin versant','Watershed Setup');
  1:ModuleName:=Tr('R\eseau de r\eservoirs','Reservoir Network');
  Else ModuleName:='Simulation';
 End;
End;

Procedure BuildMenus;
Var I,J,W,X,Sw2:Integer;
Begin
 NMn:=0;
 MBegin(Tr('Fichier','File'));
 MAdd(Tr('Nouveau','New'),'Ctrl+N',False,True);
 MAdd(Tr('Ouvrir...','Open...'),'Ctrl+O',False,True);
 MAdd(Tr('Enregistrer','Save'),'Ctrl+S',False,True);
 MAdd(Tr('Enregistrer sous...','Save as...'),'',False,True);
 MSep;
 MAdd(Tr('Exporter r\esultats (CSV)...','Export results (CSV)...'),'',False,Alts[ActAlt].Valid);
 MSep;
 MAdd(Tr('Quitter','Exit'),'Alt+X',False,True);
 MBegin(#144+Tr('dition','dit'));
 If Eng Then Menus[CurM].Title:='Edit';
 MAdd(Tr('\Editeur de r\eservoir...','Reservoir Editor...'),'F2',False,True);
 MAdd(Tr('Facteur de d\ebit entrant...','Inflow factor...'),'',False,True);
 MAdd(Tr('Niveau initial du pool...','Initial pool elevation...'),'',False,True);
 MSep;
 MAdd(Tr('Valeurs par d\efaut','Restore defaults'),'',False,True);
 MBegin(Tr('Affichage','View'));
 For I:=0 to 2 do MAdd(ModuleName(I),'F6',Module=I,True);
 Menus[CurM].It[0].Sc:='';Menus[CurM].It[1].Sc:='';
 MSep;
 MAdd(Tr('\Etiquettes de d\ebit','Flow labels'),'',ShowLbl,True);
 MAdd(Tr('Noms des \el\ements','Element names'),'',ShowNames,True);
 MAdd(Tr('Grille','Grid'),'',ShowGrid,True);
 MSep;
 MAdd(Tr('Zoom avant','Zoom in'),'+',False,True);
 MAdd(Tr('Zoom arriere','Zoom out'),'-',False,True);
 MAdd(Tr('Vue compl\gte','Full view'),'',False,True);
 MBegin('Simulation');
 MAdd(Tr('Contr\ole de simulation...','Simulation control...'),'',False,True);
 MAdd(Tr('Calculer','Compute'),'F5',False,True);
 MSep;
 MAdd(Tr('Lecture / Pause','Play / Pause'),Tr('Espace','Space'),Playing,True);
 MAdd(Tr('D\ebut','Start'),'Home',False,True);
 MAdd(Tr('Fin','End'),'End',False,True);
 MAdd(Tr('Pas pr\ec\edent','Previous step'),'Left',False,True);
 MAdd(Tr('Pas suivant','Next step'),'Right',False,True);
 MBegin('Alternative');
 MAdd(Alts[0].Name,'',ActAlt=0,True);
 MAdd(Alts[1].Name,'',ActAlt=1,True);
 MSep;
 MAdd(Tr('Inclure dans le calcul','Include in compute'),'',Alts[ActAlt].Incl,True);
 MAdd(Tr('Jeu d''ops : ','Op set: ')+Copy(M.Ops[Alts[ActAlt].OpSet].Name,1,18),'',False,True);
 MAdd(Tr('\Editer les op\erations...','Edit operations...'),'',False,True);
 MBegin(Tr('Rapports','Reports'));
 MAdd(Tr('D\ecision de rel\qche...','Release decision...'),'F8',False,True);
 MAdd(Tr('Graphique Beech Ck Station','Plot Beech Ck Station'),'F7',False,True);
 MAdd(Tr('Graphique du r\eservoir','Reservoir plot'),'',False,True);
 MAdd(Tr('Graphique du point choisi','Selected location plot'),'',False,True);
 MSep;
 MAdd(Tr('R\esum\e des r\esultats','Results summary'),'',False,True);
 MAdd(Tr('Journal de calcul','Compute log'),'',False,True);
 MBegin(Tr('Outils','Tools'));
 MAdd(Tr('V\erifier le r\eseau','Check network'),'',False,True);
 MAdd(Tr('Centrer sur Sayers','Center on Sayers'),'',False,True);
 MAdd(Tr('Langue : English','Language: Francais'),'',False,True);
 MBegin(Tr('Aide','Help'));
 MAdd(Tr('Aide','Help'),'F1',False,True);
 MAdd(Tr('\a propos...','About...'),'',False,True);
 X:=6;
 For I:=0 to NMenus-1 do Begin
  Menus[I].X:=X;
  X:=X+Length(Menus[I].Title)*8+16;
  W:=0;Sw2:=0;
  For J:=0 to Menus[I].N-1 do Begin
   If Length(Menus[I].It[J].Cap)*8>W Then W:=Length(Menus[I].It[J].Cap)*8;
   If Length(Menus[I].It[J].Sc)*8>Sw2 Then Sw2:=Length(Menus[I].It[J].Sc)*8;
  End;
  Menus[I].W:=24+W+Sw2+16;
  If Sw2>0 Then Menus[I].W:=Menus[I].W+16;
 End;
End;

Procedure DrawMenuBar;
Var I:Integer;
Begin
 FillR(0,TitleH,SW-1,TitleH+MenuH-1,LightGray);
 SetColor(DarkGray);Line(0,TitleH+MenuH-1,SW-1,TitleH+MenuH-1);
 For I:=0 to NMenus-1 do Begin
  If MenuOn and(MenuAct=I)Then Begin
   FillR(Menus[I].X-4,TitleH+2,Menus[I].X+Length(Menus[I].Title)*8+4,TitleH+MenuH-2,Blue);
   TxtAt(Menus[I].X,TitleH+5,Menus[I].Title,White);
  End
   Else TxtAt(Menus[I].X,TitleH+5,Menus[I].Title,Black);
 End;
End;

Procedure DrawMenuDrop;
Var
 I,X,Y,W,H:Integer;
Begin
 If(MenuAct<0)or(MenuAct>=NMenus)Then Exit;
 X:=Menus[MenuAct].X-4;Y:=TitleH+MenuH;W:=Menus[MenuAct].W;
 H:=Menus[MenuAct].N*14+4;
 If X+W>SW-1 Then X:=SW-1-W;
 PanelBox(X,Y,X+W,Y+H,True);
 For I:=0 to Menus[MenuAct].N-1 do With Menus[MenuAct].It[I]do Begin
  If Sep Then Begin
   SetColor(DarkGray);Line(X+3,Y+2+I*14+7,X+W-3,Y+2+I*14+7);
   SetColor(White);Line(X+3,Y+2+I*14+8,X+W-3,Y+2+I*14+8);
  End
   Else
  Begin
   If MenuItm=I Then Begin
    FillR(X+3,Y+2+I*14,X+W-3,Y+2+I*14+13,Blue);
    SetColor(White);
   End
    Else If En Then SetColor(Black) Else SetColor(DarkGray);
   If Chk Then OutTextXY(X+7,Y+2+I*14+3,#251);
   OutTextXY(X+20,Y+2+I*14+3,Cap);
   If Sc<>'' Then OutTextXY(X+W-8-Length(Sc)*8,Y+2+I*14+3,Sc);
  End;
 End;
End;

Function MenuBarHit(X,Y:Integer):Integer;
Var I:Integer;
Begin
 MenuBarHit:=-1;
 If(Y<TitleH)or(Y>=TitleH+MenuH)Then Exit;
 For I:=0 to NMenus-1 do
  If(X>=Menus[I].X-4)and(X<=Menus[I].X+Length(Menus[I].Title)*8+4)Then Begin
   MenuBarHit:=I;Exit;
  End;
End;

Function MenuItemHit(X,Y:Integer):Integer;
Var I,X0,W:Integer;
Begin
 MenuItemHit:=-1;
 If(MenuAct<0)or(MenuAct>=NMenus)Then Exit;
 X0:=Menus[MenuAct].X-4;W:=Menus[MenuAct].W;
 If X0+W>SW-1 Then X0:=SW-1-W;
 If(X<X0)or(X>X0+W)or(Y<TitleH+MenuH+2)Then Exit;
 I:=(Y-TitleH-MenuH-2)div 14;
 If(I>=0)and(I<Menus[MenuAct].N)Then Begin
  If Not Menus[MenuAct].It[I].Sep Then MenuItemHit:=I;
 End;
End;

Procedure DrawTitle;Begin
 FillR(0,0,SW-1,TitleH-1,DarkGray);
 FillR(4,3,10,9,LightBlue);
 SetColor(White);Rectangle(4,3,10,9);
 If CurName<>'' Then TxtAt(18,3,'RESSIM '+AppVer+' - BaldEagle_V3.5  ['+CurName+']',White)
  Else TxtAt(18,3,'RESSIM '+AppVer+' - BaldEagle_V3.5',White);
End;

Procedure DrawModuleRow;
Var Y0:Integer;
Begin
 Y0:=TitleH+MenuH;
 FillR(0,Y0,SW-1,Y0+ModH-1,LightGray);
 TxtAt(ToolW+4,Y0+7,'Module:',Black);
 FieldBox(ToolW+66,Y0+3,ToolW+210,Y0+18);
 TxtW(ToolW+70,Y0+7,138,ModuleName(Module),Black);
 Button(ToolW+196,Y0+4,ToolW+209,Y0+17,'',True);Tri(ToolW+203,Y0+10,1,Black);
 TxtAt(ToolW+226,Y0+7,Tr('R\eseau :','Network:'),Black);
 FieldBox(ToolW+290,Y0+3,SW-PanelW-6,Y0+18);
 TxtW(ToolW+294,Y0+7,SW-PanelW-ToolW-304,Alts[ActAlt].Name+'-:Base2003',Black);
End;

Procedure DrawModDrop;
Var I,Y0:Integer;
Begin
 Y0:=TitleH+MenuH+ModH-3;
 PanelBox(ToolW+66,Y0,ToolW+210,Y0+3*14+3,True);
 For I:=0 to 2 do Begin
  If I=Module Then Begin
   FillR(ToolW+69,Y0+2+I*14,ToolW+207,Y0+2+I*14+13,Blue);
   TxtAt(ToolW+72,Y0+5+I*14,ModuleName(I),White);
  End
   Else TxtAt(ToolW+72,Y0+5+I*14,ModuleName(I),Black);
 End;
End;

Procedure DrawToolStrip;
Var I,X,Y:Integer;
Begin
 FillR(0,MapY1-2,ToolW-1,SH-StatH-1,LightGray);
 For I:=0 to 2 do Begin
  X:=2;Y:=MapY1+2+I*25;
  PanelBox(X,Y,X+22,Y+22,Tool<>I);
  If Tool=I Then FillR(X+2,Y+2,X+20,Y+20,White);
  SetColor(Black);
  Case I of
   0:Begin
    Line(X+7,Y+4,X+7,Y+17);Line(X+7,Y+4,X+16,Y+13);Line(X+7,Y+17,X+10,Y+13);
    Line(X+10,Y+13,X+16,Y+13);Line(X+10,Y+13,X+13,Y+19);
   End;
   1:Begin Circle(X+10,Y+9,5);Line(X+14,Y+13,X+19,Y+18);Line(X+10,Y+7,X+10,Y+11);Line(X+8,Y+9,X+12,Y+9);End;
   Else Begin
    Line(X+8,Y+17,X+6,Y+10);Line(X+8,Y+17,X+15,Y+17);Line(X+15,Y+17,X+17,Y+11);
    Line(X+8,Y+10,X+8,Y+5);Line(X+11,Y+10,X+11,Y+4);Line(X+14,Y+10,X+14,Y+5);Line(X+17,Y+11,X+17,Y+7);
   End;
  End;
 End;
End;

Function TransportX(I:Integer):Integer;Begin
 TransportX:=SW-PanelW+8+I*28;
End;

Function SliderT(MouseX:Integer):Integer;
Var X1,X2,T:Integer;
Begin
 X1:=SW-PanelW+10;X2:=SW-11;
 T:=SimStart+Round((MouseX-X1)/(X2-X1)*(SimEnd-SimStart));
 If T<SimStart Then T:=SimStart;
 If T>SimEnd Then T:=SimEnd;
 SliderT:=T;
End;

Procedure DrawPanel;
Var
 PX,PY,I,Y,Tx:Integer;
 Ok:Boolean;
Begin
 PX:=SW-PanelW;PY:=MapY1;
 PanelBox(PX,PY-2,SW-1,SH-StatH-1,True);
 FillR(PX+4,PY+2,SW-5,PY+16,Cyan);
 Case Module of
  modSim:Begin
   TxtC(PX+4,SW-5,PY+6,'Simulation Control',White);
   TxtAt(PX+8,PY+22,'Simulation:',DarkGray);TxtAt(PX+8,PY+32,FmtDT2(SimStart),Black);
   TxtAt(PX+8,PY+44,Tr('Retour (lookback):','Lookback:'),DarkGray);TxtAt(PX+8,PY+54,FmtDT2(0),Black);
   TxtAt(PX+8,PY+66,Tr('Fin :','End:'),DarkGray);TxtAt(PX+8,PY+76,FmtDT2(SimEnd),Black);
   FieldBox(PX+4,PY+90,SW-5,PY+144);
   FillR(PX+8,PY+95,PX+18,PY+103,Yellow);SetColor(Black);Rectangle(PX+8,PY+95,PX+18,PY+103);
   TxtAt(PX+24,PY+95,'1993.11.27-1400',Black);
   For I:=0 to NAlts-1 do Begin
    Y:=PY+110+I*16;
    CheckBox(PX+16,Y,Alts[I].Incl);
    If I=ActAlt Then Begin
     FillR(PX+32,Y-2,SW-8,Y+11,Blue);
     TxtAt(PX+36,Y+1,Alts[I].Name,White);
    End
     Else TxtAt(PX+36,Y+1,Alts[I].Name,Black);
   End;
   Button(PX+8,PY+150,SW-9,PY+166,Tr('Calculer (F5)','Compute (F5)'),True);
   Ok:=Alts[0].Valid or Alts[1].Valid;
   If Ok Then TxtC(PX+4,SW-5,PY+171,Tr('R\esultats : OK','Results: OK'),Green)
         Else TxtC(PX+4,SW-5,PY+171,Tr('Non calcul\e','Not computed'),Red);
   TxtAt(PX+8,PY+186,Tr('Temps affich\e :','Display time:'),DarkGray);
   TxtAt(PX+8,PY+196,FmtDT2(DispT),Blue);
   FieldBox(PX+10,PY+210,SW-11,PY+216);
   Tx:=PX+10+Round((DispT-SimStart)/(SimEnd-SimStart)*(SW-11-PX-10));
   Button(Tx-3,PY+206,Tx+3,PY+220,'',True);
   Button(TransportX(0),PY+226,TransportX(0)+24,PY+242,'|<',True);
   Button(TransportX(1),PY+226,TransportX(1)+24,PY+242,'<',True);
   If Playing Then Button(TransportX(2),PY+226,TransportX(2)+24,PY+242,'||',True)
              Else Button(TransportX(2),PY+226,TransportX(2)+24,PY+242,#16,True);
   Button(TransportX(3),PY+226,TransportX(3)+24,PY+242,'>',True);
   Button(TransportX(4),PY+226,TransportX(4)+24,PY+242,'>|',True);
   If Alts[ActAlt].Valid Then With Res[ActAlt]do Begin
    TxtAt(PX+8,PY+252,'Sayers',Red);
    TxtAt(PX+8,PY+264,'Elev : '+RStr(Elev[DispT],2)+' ft',Black);
    TxtAt(PX+8,PY+274,Tr('Entr.: ','Inflow: ')+IntStr(Round(Flow[0][DispT]))+' cfs',Black);
    TxtAt(PX+8,PY+284,Tr('L\qcher: ','Release: ')+IntStr(Round(Rel[DispT]))+' cfs',Black);
    TxtW(PX+8,PY+294,PanelW-12,Tr('Zone: ','Zone: ')+ZoneName(Zone[DispT]),Black);
    TxtW(PX+8,PY+304,PanelW-12,RuleLabel(RK[DispT],RV[DispT]),Blue);
    If(SelNode>=0)and(SelNode<>1)Then Begin
     TxtW(PX+8,PY+320,PanelW-12,NodeName(SelNode),Red);
     TxtAt(PX+8,PY+330,IntStr(Round(Flow[SelNode][DispT]))+' cfs',Black);
    End;
   End;
  End;
  modNet:Begin
   TxtC(PX+4,SW-5,PY+6,Tr('R\eseau de r\eservoirs','Reservoir Network'),White);
   TxtAt(PX+8,PY+22,Tr('\El\ements','Elements'),DarkGray);
   For I:=0 to NNodes-1 do Begin
    Y:=PY+34+I*14;
    If I=SelNode Then Begin FillR(PX+8,Y-2,SW-8,Y+10,Blue);TxtAt(PX+14,Y+1,NodeName(I),White);End
     Else TxtAt(PX+14,Y+1,NodeName(I),Black);
   End;
   TxtAt(PX+8,PY+124,Tr('Tron\cons','Reaches'),DarkGray);
   TxtAt(PX+14,PY+136,'Sayers-Marsh',Black);
   TxtAt(PX+14,PY+148,'Marsh-Beech',Black);
   TxtAt(PX+14,PY+160,'Beech-Mill Hall',Black);
   TxtAt(PX+14,PY+172,'Mill Hall-Total',Black);
   Button(PX+8,PY+196,SW-9,PY+214,Tr('\Editer le r\eservoir','Edit reservoir'),True);
   TxtAt(PX+8,PY+226,Tr('Double-clic : ouvrir','Double-click: open'),DarkGray);
  End;
  Else Begin
   TxtC(PX+4,SW-5,PY+6,Tr('Bassin versant','Watershed Setup'),White);
   TxtAt(PX+8,PY+22,Tr('Bassin :','Watershed:'),DarkGray);
   TxtAt(PX+8,PY+32,'BaldEagle_V3.5',Black);
   TxtAt(PX+8,PY+48,Tr('R\eseaux :','Networks:'),DarkGray);
   For I:=0 to NAlts-1 do Begin
    Y:=PY+60+I*14;
    If I=ActAlt Then Begin FillR(PX+8,Y-2,SW-8,Y+10,Blue);TxtW(PX+10,Y+1,PanelW-16,Alts[I].Name+'-:Base2003',White);End
     Else TxtW(PX+10,Y+1,PanelW-16,Alts[I].Name+'-:Base2003',Black);
   End;
   TxtAt(PX+8,PY+100,Tr('\El\ements :','Elements:'),DarkGray);
   TxtAt(PX+14,PY+112,'1 '+Tr('r\eservoir','reservoir'),Black);
   TxtAt(PX+14,PY+122,'4 '+Tr('jonctions','junctions'),Black);
   TxtAt(PX+14,PY+132,'4 '+Tr('tron\cons','reaches'),Black);
   TxtAt(PX+14,PY+142,'4 '+Tr('affluents','tributaries'),Black);
   TxtAt(PX+8,PY+162,Tr('Unit\es : US (cfs, ft,','Units: US (cfs, ft,'),DarkGray);
   TxtAt(PX+8,PY+172,'ac-ft)',DarkGray);
  End;
 End;
End;

Procedure DrawStatus;
Var S:String;
Begin
 FillR(0,SH-StatH,SW-1,SH-1,LightGray);
 Bevel(0,SH-StatH,SW-1,SH-1,False);
 If StatMsg<>'' Then S:=StatMsg
  Else If(Eng and(PrjEn<>''))Then S:=PrjEn
  Else If(Not Eng)and(PrjFr<>'')Then S:=PrjFr
  Else S:=Tr('Local Workspace BaldEagle_V3.5 ouvert','Local Workspace BaldEagle_V3.5 opened');
 TxtW(6,SH-StatH+4,SW-220,S,Black);
 TxtAt(SW-210,SH-StatH+4,FmtDT2(DispT),DarkGray);
End;

Procedure DrawWindow(W:Integer);Begin
 Case W of
  wRes:DrawRes;
  wRep:DrawRep;
  wPlot:DrawPlot;
  wTxt:DrawTxt;
 End;
End;

Procedure RedrawAll;
Var I:Integer;
Begin
 HideMouse;
 BuildMenus;
 FillR(0,0,SW-1,SH-1,LightGray);
 DrawTitle;
 DrawModuleRow;
 DrawToolStrip;
 DrawMap;
 DrawPanel;
 DrawStatus;
 DrawMenuBar;
 For I:=0 to ZCnt-1 do DrawWindow(ZList[I]);
 If ModDrop Then DrawModDrop;
 If MenuOn Then DrawMenuDrop;
 ShowMouse;
 Dirty:=False;
End;

{ ------------------------------------------------------------------ }
{ Textes : aide, a propos, resume, verification                       }
{ ------------------------------------------------------------------ }

Procedure ShowHelp;Begin
 TxtN:=0;
 TxtAdd(Tr('RESSIM - Clone de HEC-ResSim (simulation de r\eservoirs)','RESSIM - HEC-ResSim clone (reservoir system simulation)'));
 TxtAdd('');
 TxtAdd(Tr('Modules (menu Affichage ou F6) :','Modules (View menu or F6):'));
 TxtAdd(Tr('  Configuration du bassin : carte et \el\ements du bassin.','  Watershed Setup  : map and watershed elements.'));
 TxtAdd(Tr('  R\eseau de r\eservoirs    : \el\ements, \editeur de r\eservoir.','  Reservoir Network: elements, reservoir editor.'));
 TxtAdd(Tr('  Simulation              : calcul, d\ebits et r\esultats.','  Simulation       : compute, flows and results.'));
 TxtAdd('');
 TxtAdd(Tr('Carte (outils \a gauche) :','Map (tools on the left):'));
 TxtAdd(Tr('  Fl\gche : clic = s\election ; double-clic = graphique du point','  Arrow: click = select ; double-click = location plot'));
 TxtAdd(Tr('           (Sayers = \editeur de r\eservoir).','         (Sayers = reservoir editor).'));
 TxtAdd(Tr('  Loupe  : clic gauche = zoom avant, clic droit = zoom arriere.','  Zoom   : left click = zoom in, right click = zoom out.'));
 TxtAdd(Tr('  Main   : glisser pour d\eplacer la carte.','  Pan    : drag to move the map.'));
 TxtAdd('');
 TxtAdd(Tr('Simulation :','Simulation:'));
 TxtAdd(Tr('  Les alternatives coch\ees sont calcul\ees (F5). Cliquez le nom','  Checked alternatives are computed (F5). Click the name to'));
 TxtAdd(Tr('  d''une alternative pour l''afficher. Le curseur de temps, les','  display it. The time slider, the transport buttons and the'));
 TxtAdd(Tr('  boutons de lecture et les fl\gches Gauche/Droite parcourent','  Left/Right arrows move through the simulation period.'));
 TxtAdd(Tr('  la p\eriode simul\ee. Espace = lecture / pause.','  Space = play / pause.'));
 TxtAdd('');
 TxtAdd(Tr('R\ggles d''exploitation (\Editeur de r\eservoir, onglet Op\erations) :','Operating rules (Reservoir Editor, Operations tab):'));
 TxtAdd(Tr('  MinWQ    : d\ebit minimal                MaxRel : d\ebit maximal','  MinWQ    : minimum release              MaxRel : maximum release'));
 TxtAdd(Tr('  MaxCC    : d\ebit maximal au point de contr\ole aval (Beech Ck)','  MaxCC    : maximum flow at downstream control point (Beech Ck)'));
 TxtAdd(Tr('  RROC     : taux maximal de hausse / baisse du d\ebit (cfs/h)','  RROC     : maximum rate of increase / decrease of release (cfs/hr)'));
 TxtAdd(Tr('  IndSurch : surcharge induite (d\ebit minimal selon l''\el\evation)','  IndSurch : induced surcharge (minimum release by elevation)'));
 TxtAdd(Tr('  Maintain Peak Release : maintient le plus fort d\ebit atteint','  Maintain Peak Release: keeps the highest release reached'));
 TxtAdd(Tr('  Les r\ggles du haut de la liste sont les plus prioritaires.','  Rules at the top of the list have the highest priority.'));
 TxtAdd(Tr('  Zones de conservation : d\ebit = minimum ; zones de crue :','  Conservation zone: release = minimum ; flood zones:'));
 TxtAdd(Tr('  d\ebit = maximum permis par les r\ggles.','  release = maximum allowed by the rules.'));
 TxtAdd('');
 TxtAdd(Tr('Exemples : Fichier > Ouvrir liste les projets de SAMPLES\\RESSIM\\*.RSM.','Samples: File > Open lists the projects in SAMPLES\\RESSIM\\*.RSM.'));
 TxtAdd('');
 TxtAdd(Tr('Clavier : F1 aide, F2 \editeur, F5 calcul, F6 module, F7 graphique,','Keys: F1 help, F2 editor, F5 compute, F6 module, F7 plot,'));
 TxtAdd(Tr('  F8 rapport, F10 menus, +/- zoom, \Echap ferme, Alt+X quitte.','  F8 report, F10 menus, +/- zoom, Esc closes, Alt+X exits.'));
 TxtOpen(Tr('Aide','Help'));
End;

Procedure ShowAbout;Begin
 TxtN:=0;
 TxtAdd('');
 TxtAdd('   RESSIM '+AppVer+Tr(' - Clone de HEC-ResSim',' - HEC-ResSim clone'));
 TxtAdd('');
 TxtAdd(Tr('   Simulation de la gestion des r\eservoirs, des l\qchers d''eau','   Reservoir system simulation: release decisions and'));
 TxtAdd(Tr('   et des contraintes d''exploitation (bassin Bald Eagle Creek).','   operating constraints (Bald Eagle Creek watershed).'));
 TxtAdd('');
 TxtAdd(Tr('   Auteur : Sylvain Maltais (support@gladir.com)','   Author: Sylvain Maltais (support@gladir.com)'));
 TxtAdd('   https://www.gladir.com/electrc');
 TxtAdd('');
 TxtAdd(Tr('   Inspir\e du logiciel HEC-ResSim du U.S. Army Corps of Engineers','   Inspired by HEC-ResSim from the U.S. Army Corps of Engineers'));
 TxtAdd(Tr('   (Hydrologic Engineering Center). Ce programme n''est pas affili\e.','   (Hydrologic Engineering Center). This program is not affiliated.'));
 TxtAdd(Tr('   Les donn\ees du bassin sont fictives.','   The watershed data are fictitious.'));
 TxtOpen(Tr('A propos de RESSIM','About RESSIM'));
End;

Procedure ShowLog;
Var I:Integer;
Begin
 TxtN:=0;
 If LogN=0 Then TxtAdd(Tr('Aucun calcul effectu\e (F5).','No compute done yet (F5).'))
  Else For I:=0 to LogN-1 do TxtAdd(LogL[I]);
 TxtOpen(Tr('Journal de calcul','Compute log'));
End;

Procedure ShowSummary;
Const NM2=10;
Var
 A,T,Hrs,I:Integer;
 Vv:Array[0..NAlts-1,0..NM2-1] of Double;
 MaxZ:Array[0..NAlts-1] of Integer;
 Ok:Boolean;
 S:String;
Function Lbl(I:Integer):String;Begin
 Case I of
  0:Lbl:=Tr('D\ebit entrant max. (cfs)','Peak inflow (cfs)');
  1:Lbl:=Tr('D\ebit rel\qche max. (cfs)','Peak release (cfs)');
  2:Lbl:=Tr('\El\evation max. (ft)','Maximum elevation (ft)');
  3:Lbl:=Tr('Stockage max. (ac-ft)','Maximum storage (ac-ft)');
  4:Lbl:=Tr('Stockage de crue (ac-ft)','Flood storage (ac-ft)');
  5:Lbl:=Tr('Pointe Marsh Ck Jct (cfs)','Peak Marsh Ck Jct (cfs)');
  6:Lbl:=Tr('Pointe Beech Ck Stn (cfs)','Peak Beech Ck Stn (cfs)');
  7:Lbl:=Tr('Heures > 3248 cfs Beech','Hours > 3248 cfs Beech');
  8:Lbl:=Tr('Pointe Mill Hall (cfs)','Peak Mill Hall (cfs)');
  Else Lbl:=Tr('Pointe Bald Eagle Tot.','Peak Bald Eagle Total');
 End;
End;
Begin
 TxtN:=0;
 Ok:=False;
 FillChar(Vv,SizeOf(Vv),0);
 For A:=0 to NAlts-1 do If Alts[A].Valid Then Begin
  Ok:=True;MaxZ[A]:=0;Hrs:=0;
  For T:=SimStart to SimEnd do With Res[A]do Begin
   If Flow[0][T]>Vv[A,0]Then Vv[A,0]:=Flow[0][T];
   If Rel[T]>Vv[A,1]Then Vv[A,1]:=Rel[T];
   If Elev[T]>Vv[A,2]Then Vv[A,2]:=Elev[T];
   If Stor[T]>Vv[A,3]Then Vv[A,3]:=Stor[T];
   If Flow[2][T]>Vv[A,5]Then Vv[A,5]:=Flow[2][T];
   If Flow[3][T]>Vv[A,6]Then Vv[A,6]:=Flow[3][T];
   If Flow[3][T]>3249 Then Inc(Hrs);
   If Flow[4][T]>Vv[A,8]Then Vv[A,8]:=Flow[4][T];
   If Flow[5][T]>Vv[A,9]Then Vv[A,9]:=Flow[5][T];
   If Zone[T]>MaxZ[A]Then MaxZ[A]:=Zone[T];
  End;
  Vv[A,7]:=Hrs;
  Vv[A,4]:=Vv[A,3]-Interp(M.PE,M.PS,PoolN,M.Ops[Alts[A].OpSet].Z[1].Top);
  If Vv[A,4]<0 Then Vv[A,4]:=0;
 End;
 If Not Ok Then TxtAdd(Tr('Aucun r\esultat : lancez le calcul (F5).','No results: run the compute (F5).'))
  Else Begin
   TxtAdd(Tr('R\esum\e : ','Summary: ')+FmtDT(SimStart)+' - '+FmtDT(SimEnd));
   TxtAdd('');
   TxtAdd(PadR('',27)+PadL(Alts[0].Name,16)+PadL(Alts[1].Name,16));
   TxtAdd(PadR(Tr('Jeu d''op\erations','Operation set'),27)+PadL(Copy(M.Ops[Alts[0].OpSet].Name,1,15),16)+
          PadL(Copy(M.Ops[Alts[1].OpSet].Name,1,15),16));
   TxtAdd(PadR('',59));
 TxtL[TxtN-1]:='-----------------------------------------------------------';
   For I:=0 to NM2-1 do Begin
    S:=PadR(Lbl(I),27);
    For A:=0 to NAlts-1 do
     If Not Alts[A].Valid Then S:=S+PadL('-',16)
      Else If I=2 Then S:=S+PadL(RStr(Vv[A,I],2),16)
      Else S:=S+PadL(IntStr(Round(Vv[A,I])),16);
    TxtAdd(S);
   End;
   S:=PadR(Tr('Zone maximale atteinte','Highest zone reached'),27);
   For A:=0 to NAlts-1 do
    If Alts[A].Valid Then S:=S+PadL(ZoneAbbr(MaxZ[A]),16) Else S:=S+PadL('-',16);
   TxtAdd(S);
   TxtAdd('');
   TxtAdd(Tr('Facteur de d\ebit entrant : ','Inflow factor: ')+RStr(InflowFactor,2)+
          Tr('   \El\evation initiale : ','   Initial elevation: ')+RStr(InitElev,2)+' ft');
  End;
 TxtOpen(Tr('R\esum\e des r\esultats','Results summary'));
End;

Procedure ShowCheck;
Var
 O,Z,R,I,Warn:Integer;
 Ok:Boolean;
Begin
 TxtN:=0;Warn:=0;
 TxtAdd(Tr('V\erification du r\eseau','Network check'));
 TxtAdd('');
 TxtAdd('Sayers Inflow Jct -> Sayers -> Marsh Ck Jct -> Beech Ck Station');
 TxtAdd('  -> Mill Hall -> Bald Eagle Total  ('+Tr('6 \el\ements, 4 tron\cons : OK','6 elements, 4 reaches: OK')+')');
 TxtAdd(Tr('Affluents : Upstream, Marsh Creek, Beech Creek, Fishing Creek : OK','Tributaries: Upstream, Marsh Creek, Beech Creek, Fishing Creek: OK'));
 Ok:=True;
 For I:=1 to PoolN-1 do If(M.PE[I]<=M.PE[I-1])or(M.PS[I]<=M.PS[I-1])Then Ok:=False;
 If Ok Then TxtAdd(Tr('Table \el\evation-stockage-surface : OK','Elevation-storage-area table: OK'))
  Else Begin TxtAdd(Tr('ATTENTION : table du pool non croissante','WARNING: pool table is not increasing'));Inc(Warn);End;
 Ok:=True;
 For I:=1 to GateN-1 do If M.GE[I]<=M.GE[I-1]Then Ok:=False;
 If Ok Then TxtAdd(Tr('Table de capacit\e des vannes : OK','Gate capacity table: OK'))
  Else Begin TxtAdd(Tr('ATTENTION : table des vannes non croissante','WARNING: gate table is not increasing'));Inc(Warn);End;
 If(InitElev<M.PE[0])or(InitElev>M.PE[PoolN-1])Then Begin
  TxtAdd(Tr('ATTENTION : \el\evation initiale hors de la table','WARNING: initial elevation is outside the pool table'));Inc(Warn);
 End;
 For O:=0 to NOps-1 do Begin
  TxtAdd(Tr('Jeu d''op\erations : ','Operation set: ')+M.Ops[O].Name);
  Ok:=True;
  For Z:=1 to NZones-1 do If M.Ops[O].Z[Z].Top<=M.Ops[O].Z[Z-1].Top Then Ok:=False;
  If Not Ok Then Begin TxtAdd(Tr('  ATTENTION : sommets de zone non croissants','  WARNING: zone tops are not increasing'));Inc(Warn);End;
  For Z:=1 to NZones-1 do Begin
   If M.Ops[O].Z[Z].N=0 Then Begin
    TxtAdd(Tr('  ATTENTION : aucune r\ggle dans ','  WARNING: no rules in ')+ZoneName(Z));Inc(Warn);
   End;
   For R:=0 to M.Ops[O].Z[Z].N-1 do
    If(M.Ops[O].Z[Z].R[R].Kind=rkCC)and(M.Ops[O].Z[Z].R[R].P1<250)Then Begin
     TxtAdd(Tr('  ATTENTION : MaxCC tr\gs bas dans ','  WARNING: very low MaxCC in ')+ZoneName(Z));Inc(Warn);
    End;
  End;
  If Warn=0 Then TxtAdd(Tr('  zones et r\ggles : OK','  zones and rules: OK'));
 End;
 TxtAdd('');
 If Warn=0 Then TxtAdd(Tr('Aucun probl\gme d\etect\e.','No problem detected.'))
  Else TxtAdd(IntStr(Warn)+Tr(' avertissement(s).',' warning(s).'));
 TxtOpen(Tr('V\erification du r\eseau','Network check'));
End;

{ ------------------------------------------------------------------ }
{ Actions                                                             }
{ ------------------------------------------------------------------ }

Procedure SetModule(I:Integer);Begin
 Module:=I;
 ModDrop:=False;
 Dirty:=True;
End;

Procedure Recalc(Msg:String);Begin
 DoCompute;
 RepBuild;
 Stat(Msg);
End;

Procedure NewProject;
Var I:Integer;
Begin
 InitDefaults;
 SetDescr;
 BuildInputs;
 DoCompute;
 FileName:='';
 Playing:=False;
 For I:=0 to NWin-1 do If WOpen[I]Then WinClose(I);
 RepTop:=0;RepBuild;
 Stat(Tr('Nouveau projet (valeurs par d\efaut)','New project (default values)'));
End;

Procedure DoSave(AsNew:Boolean);
Var Fn:String;
Begin
 Fn:=FileName;
 If AsNew or(Fn='')Then
  If Not AskFile(Tr('Enregistrer','Save'),Tr('Nom du fichier projet :','Project file name:'),'RESSIM.RSM',Fn)Then Exit;
 If SaveProject(Fn)Then Begin FileName:=Fn;Stat(Tr('Projet enregistr\e : ','Project saved: ')+Fn);End
  Else Stat(Tr('\Echec de l''enregistrement','Save failed'));
End;

Procedure DoOpen;
Var
 Fn:String;
 K:Integer;
Begin
 ScanProjects;
 ChItems[PickN]:=Tr('Autre fichier...','Other file...');
 ChN:=PickN+1;
 K:=ChoiceBox(Tr('Ouvrir un projet ou un exemple','Open a project or a sample'),0);
 If K<0 Then Exit;
 If K<PickN Then Fn:=PickPath[K]
  Else If Not AskFile(Tr('Ouvrir','Open'),Tr('Nom du fichier projet :','Project file name:'),'RESSIM.RSM',Fn)Then Exit;
 If WOpen[wRes]Then WinClose(wRes);
 If LoadProject(Fn)Then Begin
  FileName:=Fn;
  DoCompute;RepBuild;
  Stat(Tr('Projet ouvert : ','Project opened: ')+Fn);
 End
  Else Stat(Tr('Fichier invalide ou introuvable : ','Invalid or missing file: ')+Fn);
End;

Procedure DoExport;
Var Fn:String;
Begin
 If AskFile(Tr('Exporter','Export'),Tr('Nom du fichier CSV :','CSV file name:'),'RESSIM.CSV',Fn)Then Begin
  If ExportCSV(Fn)Then Stat(Tr('R\esultats export\es : ','Results exported: ')+Fn)
   Else Stat(Tr('\Echec de l''exportation','Export failed'));
 End;
End;

Function ParseDH(S:String;Var Idx:Integer):Boolean;
Var D,H:Double;
Begin
 ParseDH:=False;
 If Not ValStr(NextTok(S),D)Then Exit;
 If Not ValStr(NextTok(S),H)Then H:=0;
 If(D<1)or(D>30)or(H<0)or(H>23)Then Exit;
 Idx:=DayHourToIdx(Round(D),Round(H));
 ParseDH:=True;
End;

Procedure SimControl;
Var
 S:String;
 Y,Mo,D,H,NS,NE:Integer;
Begin
 IdxToDate(SimStart,Y,Mo,D,H);
 S:=IntStr(D)+' '+IntStr(H);
 If Not InputBox(Tr('Contr\ole de simulation','Simulation control'),
                 Tr('D\ebut (jour heure, ex. 27 14) :','Start (day hour, e.g. 27 14):'),S,12)Then Exit;
 If Not ParseDH(S,NS)Then Begin Stat(Tr('Date invalide','Invalid date'));Exit;End;
 IdxToDate(SimEnd,Y,Mo,D,H);
 S:=IntStr(D)+' '+IntStr(H);
 If Not InputBox(Tr('Contr\ole de simulation','Simulation control'),
                 Tr('Fin (jour heure, ex. 5 12) :','End (day hour, e.g. 5 12):'),S,12)Then Exit;
 If Not ParseDH(S,NE)Then Begin Stat(Tr('Date invalide','Invalid date'));Exit;End;
 If(NS<1)or(NS>=NE)or(NE>NT-1)Then Begin
  Stat(Tr('P\eriode invalide (25 nov 01h \a 5 d\ec 12h, d\ebut < fin)','Invalid period (25 Nov 01h to 5 Dec 12h, start < end)'));
  Exit;
 End;
 SimStart:=NS;SimEnd:=NE;DispT:=NS;
 Recalc(Tr('P\eriode de simulation modifi\ee','Simulation period changed'));
End;

Procedure ExecMenu(Mn,It:Integer);
Var V:Double;
Begin
 Case Mn of
  0:Case It of
   0:If YesNo(Tr('Nouveau','New'),Tr('Abandonner le projet courant ?','Discard the current project?'))Then NewProject;
   1:DoOpen;
   2:DoSave(False);
   3:DoSave(True);
   5:DoExport;
   7:Running:=False;
  End;
  1:Case It of
   0:ResOpen(0);
   1:Begin
    V:=InflowFactor;
    If InputNum(Tr('Facteur de d\ebit entrant','Inflow factor'),
                Tr('Multiplicateur de crue (0.1 \a 5) :','Flood multiplier (0.1 to 5):'),V,2)Then Begin
     If(V>=0.1)and(V<=5)Then Begin
      InflowFactor:=V;
      Recalc(Tr('Facteur de d\ebit entrant : ','Inflow factor: ')+RStr(V,2));
     End
      Else Stat(Tr('Valeur hors limites (0.1 \a 5)','Value out of range (0.1 to 5)'));
    End;
   End;
   2:Begin
    V:=InitElev;
    If InputNum(Tr('Niveau initial','Initial elevation'),Tr('\El\evation initiale du pool (ft) :','Initial pool elevation (ft):'),V,2)Then Begin
     If(V>=M.PE[0])and(V<=M.PE[PoolN-1])Then Begin
      InitElev:=V;
      Recalc(Tr('\El\evation initiale : ','Initial elevation: ')+RStr(V,2)+' ft');
     End
      Else Stat(Tr('\El\evation hors de la table du pool','Elevation outside the pool table'));
    End;
   End;
   4:If YesNo(Tr('Valeurs par d\efaut','Defaults'),Tr('R\etablir les valeurs par d\efaut du mod\gle ?','Restore the model defaults?'))Then Begin
    InitModel;SetDescr;InflowFactor:=1;InitElev:=610.09;
    If WOpen[wRes]Then WinClose(wRes);
    Recalc(Tr('Valeurs par d\efaut r\etablies','Defaults restored'));
   End;
  End;
  2:Case It of
   0,1,2:SetModule(It);
   4:Begin ShowLbl:=Not ShowLbl;Dirty:=True;End;
   5:Begin ShowNames:=Not ShowNames;Dirty:=True;End;
   6:Begin ShowGrid:=Not ShowGrid;Dirty:=True;End;
   8:ZoomAt(1.4,(MapX1+MapX2)div 2,(MapY1+MapY2)div 2);
   9:ZoomAt(1/1.4,(MapX1+MapX2)div 2,(MapY1+MapY2)div 2);
   10:Begin FitView;Dirty:=True;End;
  End;
  3:Case It of
   0:SimControl;
   1:Recalc(Tr('Simulation calcul\ee','Simulation computed'));
   3:Begin Playing:=Not Playing;Dirty:=True;End;
   4:Begin DispT:=SimStart;Dirty:=True;End;
   5:Begin DispT:=SimEnd;Dirty:=True;End;
   6:Begin If DispT>SimStart Then Dec(DispT);Dirty:=True;End;
   7:Begin If DispT<SimEnd Then Inc(DispT);Dirty:=True;End;
  End;
  4:Case It of
   0,1:Begin ActAlt:=It;RepBuild;Dirty:=True;End;
   3:Begin
    If Alts[ActAlt].Incl and(Not Alts[1-ActAlt].Incl)Then
     Stat(Tr('Au moins une alternative doit \qtre incluse','At least one alternative must be included'))
    Else Begin Alts[ActAlt].Incl:=Not Alts[ActAlt].Incl;Recalc(Tr('Alternative modifi\ee','Alternative changed'));End;
   End;
   4:Begin
    Alts[ActAlt].OpSet:=(Alts[ActAlt].OpSet+1)mod NOps;
    If WOpen[wRes]Then WinClose(wRes);
    Recalc(Tr('Jeu d''op\erations : ','Operation set: ')+M.Ops[Alts[ActAlt].OpSet].Name);
   End;
   5:ResOpen(1);
  End;
  5:Case It of
   0:RepOpen;
   1:PlotOpen(3,0);
   2:PlotOpen(1,0);
   3:Begin If SelNode<0 Then PlotOpen(3,0) Else PlotOpen(SelNode,0);End;
   5:ShowSummary;
   6:ShowLog;
  End;
  6:Case It of
   0:ShowCheck;
   1:Begin SelNode:=1;FitView;VOX:=NodeX[1]-(MapX2-MapX1)/2/VScale;VOY:=NodeY[1]-(MapY2-MapY1)/2/VScale;Dirty:=True;End;
   2:Begin Eng:=Not Eng;SetDescr;Dirty:=True;End;
  End;
  7:Case It of
   0:ShowHelp;
   1:ShowAbout;
  End;
 End;
End;

{ ------------------------------------------------------------------ }
{ Souris                                                              }
{ ------------------------------------------------------------------ }

Function NodeAt(X,Y:Integer):Integer;
Var I:Integer;
Begin
 NodeAt:=-1;
 For I:=0 to NNodes-1 do
  If(Abs(MapX1+WSX(NodeX[I])-X)<9)and(Abs(MapY1+WSY(NodeY[I])-Y)<9)Then Begin
   NodeAt:=I;Exit;
  End;
End;

Procedure OpenNode(N:Integer);Begin
 If N=1 Then ResOpen(1)
  Else If Alts[ActAlt].Valid Then PlotOpen(N,0)
  Else Stat(Tr('Aucun r\esultat : lancez le calcul (F5)','No results: run the compute (F5)'));
End;

Procedure PanelClick;
Var PX,PY,I,Y:Integer;
Begin
 PX:=SW-PanelW;PY:=MapY1;
 Case Module of
  modSim:Begin
   For I:=0 to NAlts-1 do Begin
    Y:=PY+110+I*16;
    If Inside(MX,MY,PX+16,Y,PX+26,Y+10)Then Begin
     If Alts[I].Incl and(Not Alts[1-I].Incl)Then
      Stat(Tr('Au moins une alternative doit \qtre incluse','At least one alternative must be included'))
     Else Begin Alts[I].Incl:=Not Alts[I].Incl;Recalc(Tr('Alternative modifi\ee','Alternative changed'));End;
    End
     Else If Inside(MX,MY,PX+32,Y-2,SW-8,Y+11)Then Begin
      ActAlt:=I;RepBuild;Dirty:=True;
     End;
   End;
   If Inside(MX,MY,PX+8,PY+150,SW-9,PY+166)Then Recalc(Tr('Simulation calcul\ee','Simulation computed'));
   If Inside(MX,MY,PX+8,PY+204,SW-9,PY+222)Then Begin
    SliderDrag:=True;
    DispT:=SliderT(MX);Dirty:=True;
   End;
   If Inside(MX,MY,TransportX(0),PY+226,TransportX(0)+24,PY+242)Then Begin DispT:=SimStart;Dirty:=True;End;
   If Inside(MX,MY,TransportX(1),PY+226,TransportX(1)+24,PY+242)Then Begin If DispT>SimStart Then Dec(DispT);Dirty:=True;End;
   If Inside(MX,MY,TransportX(2),PY+226,TransportX(2)+24,PY+242)Then Begin Playing:=Not Playing;Dirty:=True;End;
   If Inside(MX,MY,TransportX(3),PY+226,TransportX(3)+24,PY+242)Then Begin If DispT<SimEnd Then Inc(DispT);Dirty:=True;End;
   If Inside(MX,MY,TransportX(4),PY+226,TransportX(4)+24,PY+242)Then Begin DispT:=SimEnd;Dirty:=True;End;
  End;
  modNet:Begin
   For I:=0 to NNodes-1 do Begin
    Y:=PY+34+I*14;
    If Inside(MX,MY,PX+8,Y-2,SW-8,Y+10)Then Begin
     SelNode:=I;Dirty:=True;
     If DblClick Then OpenNode(I);
    End;
   End;
   If Inside(MX,MY,PX+8,PY+196,SW-9,PY+214)Then ResOpen(1);
  End;
  Else For I:=0 to NAlts-1 do Begin
   Y:=PY+60+I*14;
   If Inside(MX,MY,PX+8,Y-2,SW-8,Y+10)Then Begin ActAlt:=I;RepBuild;Dirty:=True;End;
  End;
 End;
End;

Procedure MapClick;
Var N:Integer;
Begin
 Case Tool of
  0:Begin
   N:=NodeAt(MX,MY);
   SelNode:=N;
   Dirty:=True;
   If N>=0 Then Begin
    Stat(NodeName(N));
    If DblClick Then OpenNode(N);
   End;
  End;
  1:ZoomAt(1.5,MX,MY);
  Else Begin PanDrag:=True;End;
 End;
End;

Procedure HandleMouse;
Var
 B,It,I,W,Y0:Integer;
Begin
 { Survol des menus ouverts }
 If MenuOn and((MX<>OMX)or(MY<>OMY))Then Begin
  B:=MenuBarHit(MX,MY);
  If(B>=0)and(B<>MenuAct)Then Begin MenuAct:=B;MenuItm:=-1;Dirty:=True;End
   Else Begin
   It:=MenuItemHit(MX,MY);
   If It<>MenuItm Then Begin MenuItm:=It;Dirty:=True;End;
  End;
 End;
 { Deplacement d'une fenetre }
 If DragW>=0 Then Begin
  If(MB and 1)<>0 Then Begin
   If(MX<>OMX)or(MY<>OMY)Then Begin
    WX[DragW]:=Clamp(MX-DragDX,0,SW-60);
    WY[DragW]:=Clamp(MY-DragDY,TitleH+MenuH,SH-40);
    Dirty:=True;
   End;
  End
   Else DragW:=-1;
  Exit;
 End;
 If SliderDrag Then Begin
  If(MB and 1)<>0 Then Begin
   If SliderT(MX)<>DispT Then Begin DispT:=SliderT(MX);Dirty:=True;End;
  End
   Else SliderDrag:=False;
  Exit;
 End;
 If PanDrag Then Begin
  If(MB and 1)<>0 Then Begin
   If(MX<>OMX)or(MY<>OMY)Then Begin
    VOX:=VOX-(MX-OMX)/VScale;
    VOY:=VOY-(MY-OMY)/VScale;
    Dirty:=True;
   End;
  End
   Else PanDrag:=False;
  Exit;
 End;
 { Lecture des coordonnees du graphique }
 If(TopWin=wPlot)and((MX<>OMX)or(MY<>OMY))and Not Dirty Then Begin
  HideMouse;DrawPlotRO;ShowMouse;
 End;
 { Clic droit : zoom arriere }
 If RPress and(MenuOn=False)Then Begin
  If(Tool=1)and Inside(MX,MY,MapX1,MapY1,MapX2,MapY2)Then ZoomAt(1/1.5,MX,MY);
 End;
 If Not Pressed Then Exit;
 { Menus }
 If MenuOn Then Begin
  B:=MenuBarHit(MX,MY);
  If B>=0 Then Begin
   If B=MenuAct Then MenuOn:=False Else Begin MenuAct:=B;MenuItm:=-1;End;
   Dirty:=True;Exit;
  End;
  It:=MenuItemHit(MX,MY);
  MenuOn:=False;Dirty:=True;
  If It>=0 Then If Menus[MenuAct].It[It].En Then ExecMenu(MenuAct,It);
  Exit;
 End;
 B:=MenuBarHit(MX,MY);
 If B>=0 Then Begin
  MenuOn:=True;MenuAct:=B;MenuItm:=-1;ModDrop:=False;Dirty:=True;Exit;
 End;
 { Liste deroulante des modules }
 Y0:=TitleH+MenuH;
 If ModDrop Then Begin
  If Inside(MX,MY,ToolW+66,Y0+ModH-3,ToolW+210,Y0+ModH-3+3*14+3)Then Begin
   I:=(MY-(Y0+ModH-3)-2)div 14;
   If(I>=0)and(I<=2)Then SetModule(I);
   Exit;
  End;
  ModDrop:=False;Dirty:=True;
 End;
 { Fenetres }
 For I:=ZCnt-1 downto 0 do Begin
  W:=ZList[I];
  If Inside(MX,MY,WX[W],WY[W],WX[W]+WW[W]-1,WY[W]+WH[W]-1)Then Begin
   WinFront(W);
   Dirty:=True;
   If Inside(MX,MY,WX[W]+WW[W]-18,WY[W]+4,WX[W]+WW[W]-6,WY[W]+15)Then WinClose(W)
    Else If Inside(MX,MY,WX[W]+3,WY[W]+3,WX[W]+WW[W]-22,WY[W]+16)Then Begin
    DragW:=W;DragDX:=MX-WX[W];DragDY:=MY-WY[W];
   End
    Else Case W of
    wRes:ResClick;
    wRep:RepClick;
    wPlot:PlotClick;
    wTxt:TxtClick;
   End;
   Exit;
  End;
 End;
 { Rangee du module }
 If Inside(MX,MY,ToolW+66,Y0+3,ToolW+210,Y0+18)Then Begin
  ModDrop:=Not ModDrop;Dirty:=True;Exit;
 End;
 { Outils }
 For I:=0 to 2 do
  If Inside(MX,MY,2,MapY1+2+I*25,24,MapY1+2+I*25+22)Then Begin Tool:=I;Dirty:=True;Exit;End;
 If Inside(MX,MY,MapX1,MapY1,MapX2,MapY2)Then Begin MapClick;Exit;End;
 If Inside(MX,MY,SW-PanelW,MapY1,SW-1,MapY2)Then PanelClick;
End;

{ ------------------------------------------------------------------ }
{ Clavier                                                             }
{ ------------------------------------------------------------------ }

Function HotMenu(Scan:Integer):Integer;
Const
 FrScan:Array[0..7] of Integer=(33,18,30,31,38,19,24,23);
 EnScan:Array[0..7] of Integer=(33,18,47,31,30,19,20,35);
Var I:Integer;
Begin
 HotMenu:=-1;
 For I:=0 to 7 do
  If((Not Eng)and(FrScan[I]=Scan))or(Eng and(EnScan[I]=Scan))Then HotMenu:=I;
End;

Procedure MenuNext(Step:Integer);
Var I,N:Integer;
Begin
 N:=Menus[MenuAct].N;
 If N<=0 Then Exit;
 If MenuItm<0 Then Begin
  If Step>0 Then MenuItm:=-1 Else MenuItm:=N;
 End;
 For I:=1 to N do Begin
  MenuItm:=MenuItm+Step;
  If MenuItm<0 Then MenuItm:=N-1;
  If MenuItm>=N Then MenuItm:=0;
  If Not Menus[MenuAct].It[MenuItm].Sep Then Exit;
 End;
End;

Procedure HandleKey;
Var
 H,W:Integer;
Begin
 If Not HaveKey Then Exit;
 If KeyExt<>0 Then Begin
  Case KeyExt of
   59:Begin ShowHelp;Exit;End;
   60:Begin ResOpen(0);Exit;End;
   63:Begin Recalc(Tr('Simulation calcul\ee','Simulation computed'));Exit;End;
   64:Begin SetModule((Module+1)mod 3);Exit;End;
   65:Begin PlotOpen(3,0);Exit;End;
   66:Begin RepOpen;Exit;End;
   68:Begin MenuOn:=Not MenuOn;MenuAct:=0;MenuItm:=-1;Dirty:=True;Exit;End;
   107,45:Begin Running:=False;Exit;End;
  End;
  H:=HotMenu(KeyExt);
  If H>=0 Then Begin MenuOn:=True;MenuAct:=H;MenuItm:=-1;Dirty:=True;Exit;End;
 End
  Else Begin
  Case KeyCh of
   #14:Begin ExecMenu(0,0);Exit;End;
   #15:Begin ExecMenu(0,1);Exit;End;
   #19:Begin ExecMenu(0,2);Exit;End;
   #17:Begin Running:=False;Exit;End;
  End;
 End;
 If MenuOn Then Begin
  If KeyExt=72 Then Begin MenuNext(-1);Dirty:=True;End;
  If KeyExt=80 Then Begin MenuNext(1);Dirty:=True;End;
  If KeyExt=75 Then Begin MenuAct:=(MenuAct+NMenus-1)mod NMenus;MenuItm:=-1;Dirty:=True;End;
  If KeyExt=77 Then Begin MenuAct:=(MenuAct+1)mod NMenus;MenuItm:=-1;Dirty:=True;End;
  If(KeyExt=0)and(KeyCh=#27)Then Begin MenuOn:=False;Dirty:=True;End;
  If(KeyExt=0)and(KeyCh=#13)Then Begin
   MenuOn:=False;Dirty:=True;
   If(MenuItm>=0)and Menus[MenuAct].It[MenuItm].En Then ExecMenu(MenuAct,MenuItm);
  End;
  Exit;
 End;
 If ModDrop and(KeyExt=0)and(KeyCh=#27)Then Begin ModDrop:=False;Dirty:=True;Exit;End;
 W:=TopWin;
 If W>=0 Then Begin
  Case W of
   wRes:ResKey;
   wRep:RepKey;
   wPlot:PlotKey;
   wTxt:TxtKey;
  End;
  If(KeyExt<>0)or(KeyCh=#27)Then Exit;
 End;
 { Touches de la carte et de la simulation }
 If KeyExt=0 Then Begin
  Case KeyCh of
   '+','=':ZoomAt(1.4,(MapX1+MapX2)div 2,(MapY1+MapY2)div 2);
   '-','_':ZoomAt(1/1.4,(MapX1+MapX2)div 2,(MapY1+MapY2)div 2);
   ' ':Begin Playing:=Not Playing;Dirty:=True;End;
  End;
 End
  Else Begin
  If KeyExt=75 Then Begin If DispT>SimStart Then Dec(DispT);Dirty:=True;End;
  If KeyExt=77 Then Begin If DispT<SimEnd Then Inc(DispT);Dirty:=True;End;
  If KeyExt=73 Then Begin DispT:=MaxI(SimStart,DispT-24);Dirty:=True;End;
  If KeyExt=81 Then Begin DispT:=MinI(SimEnd,DispT+24);Dirty:=True;End;
  If KeyExt=71 Then Begin DispT:=SimStart;Dirty:=True;End;
  If KeyExt=79 Then Begin DispT:=SimEnd;Dirty:=True;End;
 End;
End;

Procedure InitUI;Begin
 FillChar(WOpen,SizeOf(WOpen),0);
 ZCnt:=0;DragW:=-1;
 WW[wRes]:=580;WH[wRes]:=360;
 WW[wRep]:=600;WH[wRep]:=300;
 WW[wPlot]:=480;WH[wPlot]:=340;
 WW[wTxt]:=600;WH[wTxt]:=300;
 WX[wRes]:=(SW-WW[wRes])div 2;WY[wRes]:=TitleH+MenuH+6;
 WX[wRep]:=(SW-WW[wRep])div 2;WY[wRep]:=TitleH+MenuH+30;
 WX[wPlot]:=(SW-WW[wPlot])div 2;WY[wPlot]:=TitleH+MenuH+20;
 WX[wTxt]:=(SW-WW[wTxt])div 2;WY[wTxt]:=TitleH+MenuH+20;
 Module:=modSim;
 ModDrop:=False;
 Tool:=0;SelNode:=3;
 ShowLbl:=True;ShowNames:=True;ShowGrid:=False;
 PanDrag:=False;SliderDrag:=False;Playing:=False;PlayCnt:=0;
 MenuOn:=False;MenuAct:=0;MenuItm:=-1;
 RepChg:=False;RepTop:=0;RepN:=0;
 PlotNode:=3;PlotVar:=0;PlotRO:='';
 ResTab:=0;ResPhy:=1;ResOp:=0;ResTree:=0;ResSelZ:=2;ResSelR:=-1;ResTbl:=0;ResObs:=0;
 MX:=SW div 2;MY:=SH div 2;OMX:=MX;OMY:=MY;OMB:=0;MB:=0;
 Frame:=0;LastClk:=-1000;
 StatMsg:='';
 InitMapData;
 FitView;
End;

Procedure MainLoop;Begin
 Dirty:=True;
 Running:=True;
 While Running do Begin
  PollInput;
  HandleMouse;
  HandleKey;
  If Playing Then Begin
   Inc(PlayCnt);
   If PlayCnt>=6 Then Begin
    PlayCnt:=0;
    If DispT<SimEnd Then Inc(DispT) Else Playing:=False;
    Dirty:=True;
   End;
  End;
  If Dirty Then RedrawAll;
  Delay(10);
 End;
End;

{ ------------------------------------------------------------------ }
{ Mode console (-c) et aide en ligne de commande                      }
{ ------------------------------------------------------------------ }

Procedure ConsoleRun;
Var A,T:Integer;
Begin
 DoCompute;
 WriteLn('RESSIM ',AppVer,' - ',Tr('Simulation sans interface graphique','Simulation without graphical interface'));
 For A:=0 to NAlts-1 do If Alts[A].Valid Then Begin
  WriteLn;
  WriteLn(Alts[A].Name,' (',M.Ops[Alts[A].OpSet].Name,')');
  WriteLn(PadR(Tr('Date-Heure','Date-Time'),18),PadL('Elev ft',9),PadL(Tr('Entr.','Inflow'),9),
          PadL(Tr('L\qcher','Release'),9),PadL('Beech',8),'  ',Tr('Zone:R\ggle','Zone:Rule'));
  T:=SimStart;
  While T<=SimEnd do Begin
   WriteLn(PadR(FmtDT(T),18),PadL(RStr(Res[A].Elev[T],2),9),PadL(IntStr(Round(Res[A].Flow[0][T])),9),
           PadL(IntStr(Round(Res[A].Rel[T])),9),PadL(IntStr(Round(Res[A].Flow[3][T])),8),'  ',
           ZoneAbbr(Res[A].Zone[T]),':',RuleLabel(Res[A].RK[T],Res[A].RV[T]));
   Inc(T,12);
  End;
 End;
 WriteLn;
 For T:=0 to LogN-1 do WriteLn(LogL[T]);
End;

Procedure ShowCmdHelp;Begin
 WriteLn('RESSIM: ',Tr('Clone de HEC-ResSim, simulation de la gestion des r\eservoirs.',
                       'HEC-ResSim clone, reservoir system simulation.'));
 WriteLn;
 WriteLn(Tr('Syntaxe : RESSIM [-maxscreen] [-c] [fichier.rsm]','Syntax: RESSIM [-maxscreen] [-c] [file.rsm]'));
 WriteLn;
 WriteLn(Tr('Options :','Options:'));
 WriteLn(Tr('  -maxscreen   Utilise la r\esolution maximale (Free Pascal).','  -maxscreen   Use the maximum resolution (Free Pascal).'));
 WriteLn(Tr('  -c           Calcule et affiche un r\esum\e sans interface graphique.','  -c           Compute and print a summary without graphics.'));
 WriteLn(Tr('  -s fichier   \Ecrit le projet courant dans un fichier .RSM et quitte.','  -s file      Write the current project to an .RSM file and exit.'));
 WriteLn(Tr('  -h, /?       Affiche cette aide.','  -h, /?       Show this help.'));
 WriteLn;
 WriteLn(Tr('Exemples : SAMPLES\\RESSIM\\*.RSM (BALDEAGL.RSM est charg\e par d\efaut).','Samples: SAMPLES\\RESSIM\\*.RSM (BALDEAGL.RSM is loaded by default).'));
 WriteLn(Tr('Par d\efaut : mode 640x480. LANGUAGE=EN pour l''interface en anglais.',
            'Default: 640x480 mode. LANGUAGE=EN for the English interface.'));
End;

Var
 ArgI:Integer;
 Arg,FileArg,SaveArg:String;

BEGIN
 Eng:=False;
 TmpLang:=GetEnv('LANGUAGE');
 If TmpLang<>''Then Begin
  If TmpLang[1]='"'Then TmpLang:=Copy(TmpLang,2,255);
  If StrUp(Copy(TmpLang,1,2))='EN'Then Eng:=True;
 End;
 MaxScreen:=False;ConsoleMode:=False;FileArg:='';SaveArg:='';
 If(ParamStr(1)='/?')or(ParamStr(1)='--help')or(ParamStr(1)='-h')Then Begin
  ShowCmdHelp;
  Halt;
 End;
 ArgI:=1;
 While ArgI<=ParamCount do Begin
  Arg:=StrUp(ParamStr(ArgI));
  If(Arg='-MAXSCREEN')or(Arg='/MAXSCREEN')Then MaxScreen:=True
  Else If(Arg='-C')or(Arg='/C')Then ConsoleMode:=True
  Else If(Arg='-S')or(Arg='/S')Then Begin
   Inc(ArgI);
   If ArgI<=ParamCount Then SaveArg:=ParamStr(ArgI);
  End
  Else If(FileArg='')and(Copy(Arg,1,1)<>'-')and(Copy(Arg,1,1)<>'/')Then FileArg:=ParamStr(ArgI);
  Inc(ArgI);
 End;
 InitDefaults;
 SetDescr;
 FileName:='';
 If FileArg<>''Then Begin
  If LoadProject(FileArg)Then FileName:=FileArg
   Else Begin
    WriteLn(Tr('Fichier invalide ou introuvable : ','Invalid or missing file: '),FileArg);
    Halt(1);
   End;
 End
  Else LoadDefaultSample;
 BuildInputs;
 DoCompute;
 If SaveArg<>''Then Begin
  If SaveProject(SaveArg)Then WriteLn(Tr('Projet \ecrit : ','Project written: '),SaveArg)
   Else Begin WriteLn(Tr('\Echec de l''\ecriture : ','Write failed: '),SaveArg);Halt(1);End;
  Halt;
 End;
 If ConsoleMode Then ConsoleRun
  Else Begin
  If Not MouseDriverFound Then Begin
   WriteLn(Tr('Pilote de souris introuvable.','Mouse driver not found.'));
   Halt(1);
  End;
  InitializeGraphics;
  MouseInit;
  InitUI;
  ShowMouse;
  MainLoop;
  HideMouse;
  CloseGraph;
 End;
END.

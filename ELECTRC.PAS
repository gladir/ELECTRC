{ @author: Sylvain Maltais (support@gladir.com)
  @created: 2024
  @website(https://www.gladir.com/electrc)
  @abstract(Target: Turbo Pascal 7, Free Pascal 3.2)
}

Program ELECTRC;

Uses Crt,DOS;

Const
  { Touche clavier }
 kbNoKey=0;{Pas de touche}
 kbDn=$5000;{Fleche Bas}
 kbEnter=$1C0D;{Enter}
 kbEsc=$011B;{Escape}
 kbShiftTab=$0F00;{Shift+Tabulation}
 kbTab=$0F09;{Tabulation}
 kbUp=$4800;{Fleche Haut}

Var
 CurrDialog:Byte;
 ShowASCII128:Boolean;
 C:Char;
 I:Integer;

Function StrToUpper(S:String):String;
Var
 I:Byte;
Begin
 For I:=1 to Length(S)do Begin
  If S[I] in['a'..'z']Then S[I]:=Chr(Ord(S[I])-32);
 End;
 StrToUpper:=S;
End;

Function PadRight(S:String;Space:Byte):String;
Var
 I:Byte;
Begin
 If Length(S)<Space Then For I:=Length(S)+1 to Space do S:=S+' ';
 PadRight:=S;
End;


{$IFNDEF FPC}
 Procedure CursorOff;
 Var
  Regs:Registers;
 Begin
  Regs.AH:=1;
  Regs.CH:=32;
  Regs.CL:=0;
  Intr($10,Regs);
 End;

 Procedure CursorOn;
 Var
  Regs:Registers;
 Begin
  Regs.AX:=$0100;
  Regs.CX:=(7 shl 8)+9;
  Intr($10,Regs);
 End;
{$ENDIF}

Function DuplicateString(C:Char;Num:Integer):String;
Var
 I:Byte;
 S:String;
Begin
 S:='';
 For I:=1 to Num do S:=S+C;
 DuplicateString:=S;
End;

Procedure SetASCII128(X:Boolean);Begin
 ShowASCII128:=X;
End;

Function GetASCII128:Boolean;Begin
 GetASCII128:=ShowASCII128;
End;

Procedure Box(X1,Y1,X2,Y2,Couleur:Byte);Begin
 Window(X1,Y1,X2,Y2);
 TextBackground((Couleur shr 4)and 15);
 {$IFDEF FPC}
  If Couleur and $80=$80 Then Begin
   TextColor((Couleur and 15)+BLINK);
  End
   Else
  TextColor(Couleur and 15);
 {$ELSE}
  TextColor(Couleur and 15);
 {$ENDIF}
 ClrScr;
 Window(1,1,80,25);
End;

Procedure FrameEmpty(X1,Y1,X2,Y2,Couleur:Byte);
Var
 I:Byte;
 ChrHori,ChrVert:Char;
 Chr218,Chr192,Chr191,Chr217:Char;
Begin
 TextBackground((Couleur shr 4)and 15);
 {$IFDEF FPC}
  If Couleur and $80=$80 Then Begin
   TextColor((Couleur and 15)+BLINK);
  End
   Else
  TextColor(Couleur and 15);
 {$ELSE}
  TextColor(Couleur and 15);
 {$ENDIF}
 If(ShowASCII128)Then Begin
  ChrHori:='-';
  ChrVert:='|';
  Chr218:='+';
  Chr192:='+';
  Chr191:='+';
  Chr217:='+';
 End
  Else
 Begin
  ChrHori:=#205;
  ChrVert:=#186;
  Chr218:=#201;
  Chr192:=#200;
  Chr191:=#187;
  Chr217:=#188;
  End;
 For I:=Y1+1 to Y2-1 do Begin
  GotoXY(X1,I);
  Write(ChrVert);
  GotoXY(X2,I);
  Write(ChrVert);
 End;
 GotoXY(X1+1,Y1);
 Write(DuplicateString(ChrHori,X2-X1-1));
 GotoXY(X1+1,Y2);
 Write(DuplicateString(ChrHori,X2-X1-1));
 GotoXY(X1,Y1);
 Write(Chr218);
 GotoXY(X1,Y2);
 Write(Chr192);
 GotoXY(X2,Y1);
 Write(Chr191);
 GotoxY(X2,Y2);
 Write(Chr217);
End;

Procedure DialogBox(X1,Y1,X2,Y2,Couleur:Byte);Begin
 Box(X1,Y1,X2,Y2,Couleur);
 FrameEmpty(X1,Y1,X2,Y2,Couleur);
 TextAttr:=Couleur;
End;

Function ReadLine(Var S:String):Word;
Var
 K:Char;
 X,Y,I:Byte;
 CurrHistory:Integer;
Begin
 ReadLine:=0;
 X:=WhereX;
 Y:=WhereY;
 Repeat
  GotoXY(X,Y);
  Write(S);
  ClrEol;
  K:=ReadKey;
  Case K of
   #0:Case ReadKey of
    #72:Begin
     ReadLine:=kbUp;
     Exit;
    End;
    #80:Begin
     ReadLine:=kbDn;
     Exit;
    End;
   End;
   #8:Begin
    If S<>''Then S:=Copy(S,1,Length(S)-1);
   End;
   #9:Begin
    ReadLine:=kbTab;
    Exit;
   End;
   #13:Begin
    ReadLine:=kbEnter;
   End;
   #27:Begin
    ReadLine:=kbEsc;
    Exit;
   End;
   Else S:=S+K;
  End;
  If S[Length(S)]=#13Then Begin
   S:=Copy(S,1,Length(S)-1);
   K:=#13;
  End;
 Until K=#13;
 GotoXY(X,Y);
 WriteLn(S);
End;

Function ReadLineInBox(X1,Y1,X2,Y2:Byte;Var S:String):Word;Begin
 Window(X1,Y1,X2,Y2);
 ReadLineInBox:=ReadLine(S);
 Window(1,1,80,25);
End;

Procedure WriteCenter(Y:Byte;S:String;Color:Byte);Begin
 If S=''Then Exit;
 TextColor(Color and $F);
 TextBackground(Color shr 4);
 GotoXY(40-(Length(S) shr 1),Y);
 Write(S);
End;

Procedure SetDialogTextColor;Begin
 TextBackground(CurrDialog shr 4);
 TextColor(CurrDialog and$F);
End;

Procedure Init;Begin
 DialogBox(1,1,80,24,$1B);
 TextColor(LightCyan);
 GotoXY(1,3);
 Write(#204,DuplicateString(#205,78),#185);
 GotoXY(1,22);
 Write(#204,DuplicateString(#205,78),#185);
End;

Procedure DCPowerVoltageCurrentResistanceCalcs;
Var
 K:Word;
 PowerValue,VoltageValue,CurrentValue,ResistanceValue:Real;
 PowerStr,VoltageStr,CurrentStr,ResistanceStr:String;

 Procedure Compute;
 Var
  Err:Word;
 Begin
  If(PowerStr<>'')and(VoltageStr<>'')Then Begin
   Val(PowerStr,PowerValue,Err);
   If Err>0 Then Exit;
   Val(VoltageStr,VoltageValue,Err);
   If Err>0 Then Exit;
   If VoltageValue<>0 Then Begin
    CurrentValue:=PowerValue/VoltageValue;
    Str(CurrentValue:0:3,CurrentStr);
    GotoXY(50,14);
    Write(CurrentStr);
    ResistanceValue:=(Sqr(VoltageValue)/PowerValue);
    Str(ResistanceValue:0:3,ResistanceStr);
    GotoXY(50,15);
    Write(ResistanceStr);
   End;
  End;
  If(CurrentStr<>'')and(ResistanceStr<>'')Then Begin
   Val(CurrentStr,CurrentValue,Err);
   If Err>0 Then Exit;
   Val(ResistanceStr,ResistanceValue,Err);
   If Err>0 Then Exit;
   PowerValue:=Sqr(CurrentValue)*ResistanceValue;
   Str(PowerValue:0:3,PowerStr);
   GotoxY(50,12);
   Write(PowerStr);
   VoltageValue:=CurrentValue*ResistanceValue;
   Str(VoltageValue:0:3,VoltageStr);
   GotoXY(50,13);
   Write(VoltageStr);
  End;
 End;

Begin
 PowerStr:='';
 VoltageStr:='';
 CurrentStr:='';
 ResistanceStr:='';
 Box(2,2,79,2,$17);
 WriteCenter(2,'PUISSANCE DC / VOLTAGE / COURANT / CALCULS DE RSISTANCE',$1F);
 Box(2,5,79,21,$17);
 WriteCenter(5,'Vous serez invit‚ … saisir des valeurs de puissance, '+
               'de tension, de courant ',$17);
 WriteCenter(6,'et de r‚sistance. Entrez seulement 2 de 4 des valeurs, le',$17);
 WriteCenter(7,'programme calculera les autres.',$17);
 WriteCenter(9,'- ENTRER UNE VALEUR OU PRESSE ENTER POUR SAUTER -',$17);
 GotoXY(30,12);
 Write('PUISSANCE (watts) =');
 GotoXY(30,13);
 Write('VOLTAGE (volts)   =');
 GotoXY(30,14);
 Write('COURANT (amps)    =');
 GotoXY(30,15);
 Write('RESISTANCE (ohms) =');
 Box(2,23,79,23,$17);
 WriteCenter(23,'',$17);
 Repeat
  Repeat
   TextColor(White);
   TextBackground(Blue);
   K:=ReadLineInBox(50,12,79,12,PowerStr);
   If(K=kbEsc)Then Exit;
  Until(K=kbTab)or(K=kbEnter)or(K=kbDn);
  Compute;
  Repeat
   TextColor(White);
   TextBackground(Blue);
   K:=ReadLineInBox(50,13,79,13,VoltageStr);
   If(K=kbEsc)Then Exit;
  Until(K=kbTab)or(K=kbEnter)or(K=kbDn);
  Compute;
  Repeat
   TextColor(White);
   TextBackground(Blue);
   K:=ReadLineInBox(50,14,79,14,CurrentStr);
   If(K=kbEsc)Then Exit;
  Until(K=kbTab)or(K=kbEnter)or(K=kbDn);
  Compute;
  Repeat
   TextColor(White);
   TextBackground(Blue);
   K:=ReadLineInBox(50,15,79,15,ResistanceStr);
   If(K=kbEsc)Then Exit;
  Until(K=kbTab)or(K=kbEnter)or(K=kbDn);
  Compute;
 Until K=kbEsc;
End;

Procedure CalculateCostToOperateVariousLoads;
Var
 PowerOfLoadValue,DailyUseValue,NumberOfDaysValue,PriceKwHValue,Cost:Real;
 Err,K:Word;
 PowerOfLoadStr,DailyUseStr,NumberOfDaysStr,PriceKwHStr:String;
Begin
 PowerOfLoadStr:='';
 DailyUseStr:='';
 NumberOfDaysStr:='';
 PriceKwHStr:='';
 Box(2,2,79,2,$17);
 WriteCenter(2,'COUT D''OPRATION',$1F);
 Box(2,5,79,21,$17);
 GotoXY(14,5);
 Write('Entrez la puissance de charge (watts) .......');
 GotoXY(14,7);
 Write('Entrez l''utilisation quotidienne (heures) ...');
 GotoXY(14,9);
 Write('Entrez le nombre de jours ...................');
 GotoXY(14,11);
 Write('Entrer le price/KwH .........................');
 Box(2,23,79,23,$17);
 WriteCenter(23,'',$17);
 Repeat
  TextColor(White);
  TextBackground(Blue);
  K:=ReadLineInBox(60,5,79,5,PowerOfLoadStr);
  If(K=kbEsc)Then Exit;
 Until(K=kbTab)or(K=kbEnter)or(K=kbDn);
 Repeat
  TextColor(White);
  TextBackground(Blue);
  K:=ReadLineInBox(60,7,79,7,DailyUseStr);
  If(K=kbEsc)Then Exit;
 Until(K=kbTab)or(K=kbEnter)or(K=kbDn);
 Repeat
  TextColor(White);
  TextBackground(Blue);
  K:=ReadLineInBox(60,9,79,9,NumberOfDaysStr);
  If(K=kbEsc)Then Exit;
 Until(K=kbTab)or(K=kbEnter)or(K=kbDn);
 Repeat
  TextColor(White);
  TextBackground(Blue);
  K:=ReadLineInBox(60,11,79,11,PriceKwHStr);
  If(K=kbEsc)Then Exit;
 Until(K=kbTab)or(K=kbEnter)or(K=kbDn);
 TextColor(LightGray);
 Val(PowerOfLoadStr,PowerOfLoadValue,Err);
 If Err>0 Then Exit;
 Val(DailyUseStr,DailyUseValue,Err);
 If Err>0 Then Exit;
 Val(NumberOfDaysStr,NumberOfDaysValue,Err);
 If Err>0 Then Exit;
 Val(PriceKwHStr,PriceKwHValue,Err);
 If Err>0 Then Exit;
 Cost:=(PowerOfLoadValue/1000)*DailyUseValue*NumberOfDaysValue*PriceKwHValue;
 GotoXY(25,18);
 Write('Le co–t d''op‚ration … ',PowerOfLoadValue:1:0,' Watts');
 GotoXY(25,19);
 Write(DailyUseValue:1:0,' par jours pour ',NumberOfDaysValue:1:0);
 GotoXY(25,20);
 Write('A ',PriceKwHValue:1:2,' cents/kwh doit ˆtre $ ',Cost:1:2);
 ReadKey;
 If(Keypressed)Then ReadKey;
End;

Procedure GroundingElectrodeConductorTable;Begin
 Box(2,2,79,2,$17);
 WriteCenter(2,'CONDUCTEUR D''LECTRODE DE MISE · LA TERRE POUR '+
               'SYSTÔMES AC',$1F);
 Box(2,5,79,21,$17);
 GotoXY(3,4);
 Write('Taille du plus grand conducteur d''entr‚e ');
 GotoXY(3,5);
 Write('de service ou zone ‚quivalente pour les ');
 GotoXY(3,6);
 Write('conducteurs parallŠles');
 GotoXY(55,4);
 Write('Taille du conducteur de');
 GotoXY(55,5);
 Write('l''‚lectrode de mise … ');
 GotoXY(55,6);
 Write('la terre');
 GotoXY(2,7);
 Write(DuplicateString('-',78));
 GotoXY(5,10);
 Write('Cuivre');
 GotoXY(22,8);
 Write('Aluminium ou ');
 GotoXY(22,9);
 Write('aluminium plaqu‚ ');
 GotoXY(22,10);
 Write('cuivre');
 GotoXY(45,10);
 Write('Cuivre');
 GotoXY(60,8);
 Write('Aluminium ou ');
 GotoXY(60,9);
 Write('aluminium plaqu‚ ');
 GotoXY(60,10);
 Write('cuivre');
 GotoXY(2,11);
 Write(DuplicateString('-',78));
 GotoXY(3,12);
 Write('2 ou moins');
 GotoXY(3,13);
 Write('1 ou 0');
 GotoXY(3,14);
 Write('2/0 ou 3/0');
 GotoXY(3,15);
 Write('Plus de 3/0 …');
 GotoXY(3,16);
 Write(' 350 mcm');
 GotoXY(3,17);
 Write('Plus de 350 mcm ');
 GotoXY(3,18);
 Write(' … 600 mcm');
 GotoXY(3,19);
 Write('Plus de 600 mcm ');
 GotoXY(3,20);
 Write(' … 1100 mcm');
 GotoXY(3,21);
 Write('Plus de 1100 mcm ');
 GotoXY(22,12);
 Write('0 ou moins');
 GotoXY(22,13);
 Write('2/0 ou 3/0');
 GotoXY(22,14);
 Write('4/0 ou 250 mcm');
 GotoXY(22,15);
 Write('Plus de 250 mcm ');
 GotoXY(22,16);
 Write(' … 500 mcm');
 GotoXY(22,17);
 Write('Plus de 500 mcm');
 GotoXY(22,18);
 Write(' … 900 mcm');
 GotoXY(22,19);
 Write('Plus de 900 mcm');
 GotoXY(22,20);
 Write(' … 1750 mcm');
 GotoXY(22,21);
 Write('Plus de 1750 mcm');
 GotoXY(48,12);
 Write(8);
 GotoXY(48,13);
 Write(6);
 GotoXY(48,14);
 Write(4);
 GotoXY(48,15);
 Write(2);
 GotoXY(48,17);
 Write(0);
 GotoXY(48,19);
 Write('2/0');
 GotoxY(48,21);
 Write('3/0');
 Box(2,23,79,23,$17);
 WriteCenter(23,'Presse n''importe quel touche pour le menu principal',$17);
 ReadKey;
 If(Keypressed)Then ReadKey;
End;

Procedure EquipmentGroundConductorTable;Begin
 Box(2,2,79,2,$17);
 WriteCenter(2,'CONDUCTEUR DE MISE · LA TERRE DE L''QUIPEMENT DE '+
               'TAILLE MINIMALE',$1F);
 Box(2,5,79,21,$17);
 GotoXY(4,5);
 Write('valuation du dispositif de');
 GotoXY(4,6);
 Write('surintensit‚ de circuit. Ne');
 GotoXY(4,7);
 Write('d‚passant pas (ampŠres).');
 GotoXY(50,5);
 Write('Taille');
 GotoXY(35,6);
 Write(DuplicateString('-',40));
 GotoXY(35,7);
 Write('Fil de cuivre #');
 GotoXY(58,7);
 Write('Fil d''aluminium #');
 GotoXY(2,8);
 Write(DuplicateString('-',78));
 GotoXY(15,10);
 Write(15);
 GotoXY(15,11);
 Write(20);
 GotoXY(15,12);
 Write(30);
 GotoXY(15,13);
 Write(40);
 GotoXY(15,14);
 Write(60);
 GotoXY(14,15);
 Write(100);
 GotoXY(14,16);
 Write(200);
 GotoXY(14,17);
 Write(300);
 GotoXY(14,18);
 Write(400);
 GotoXY(14,19);
 Write(500);
 GotoXY(14,20);
 Write(600);
 GotoXY(14,21);
 Write(800);
 GotoXY(40,10);
 Write(14);
 GotoXY(40,11);
 Write(12);
 GotoXY(40,12);
 Write(10);
 GotoXY(40,13);
 Write(10);
 GotoXY(40,14);
 Write(10);
 GotoXY(40,15);
 Write(8);
 GotoXY(40,16);
 Write(6);
 GotoXY(40,17);
 Write(4);
 GotoXY(40,18);
 Write(3);
 GotoXY(40,19);
 Write(2);
 GotoXY(40,20);
 Write(1);
 GotoXY(40,21);
 Write('1/0');
 GotoXY(62,10);
 Write(12);
 GotoXY(62,11);
 Write(10);
 GotoXY(62,12);
 Write(8);
 GotoXY(62,13);
 Write(8);
 GotoXY(62,14);
 Write(8);
 GotoXY(62,15);
 Write(6);
 GotoXY(62,16);
 Write(4);
 GotoXY(62,17);
 Write(2);
 GotoXY(62,18);
 Write(1);
 GotoXY(62,19);
 Write('1/0');
 GotoXY(62,20);
 Write('2/0');
 GotoXY(62,21);
 Write('3/0');
 Box(2,23,79,23,$17);
 WriteCenter(23,'Presse n''importe quel touche pour le menu principal',$17);
 ReadKey;
 If(Keypressed)Then ReadKey;
End;

Procedure UndergroundCoverRequirements;Begin
 Box(2,2,79,2,$17);
 WriteCenter(2,'EXIGENCES MINIMALES DE COUVERTURE',$1F);
 Box(2,5,79,21,$17);
 GotoXY(5,7);
 Write('M‚thode de cƒblage');
 GotoXY(58,5);
 Write('Profondeur ');
 GotoXY(51,6);
 Write('d''enfouissement minimale');
 GotoXY(59,7);
 Write('(pouces)');
 GotoXY(2,8);
 Write(DuplicateString('-',78));
 GotoXY(5,9);
 Write('Cƒble enterr‚ directement',DuplicateString('.',34));
 GotoXY(5,11);
 Write('Conduit m‚tallique rigide',DuplicateString('.',34));
 GotoXY(5,13);
 Write('Conduit m‚tallique interm‚diaire',DuplicateString('.',27));
 GotoXY(5,15);
 Write('Conduit rigide non m‚tallique approuv‚');
 GotoXY(5,16);
 Write(' pour un enfouissement direct sans');
 GotoXY(5,17);
 Write('  enrobage de b‚ton',DuplicateString('.',40));
 GotoXY(5,19);
 Write('Autres chemins de roulement approuv‚s',DuplicateString('.',22));
 GotoXY(65,9);
 Write(24);
 GotoXY(65,11);
 Write(' 6');
 GotoXY(65,13);
 Write(' 6');
 GotoXY(65,17);
 Write(18);
 GotoXY(65,19);
 Write(18);
 Box(2,23,79,23,$17);
 WriteCenter(23,'Presse n''importe quel touche pour le menu principal',$17);
 ReadKey;
 If(Keypressed)Then ReadKey;
End;

Procedure KWToAmpereTables;
Var
 Page:Byte;
Begin
 Page:=1;
 Repeat
  Box(2,2,79,2,$17);
  WriteCenter(2,'TABLEAU KW EN AMPÔRE',$1F);
  GotoXY(60,2);
  Write('Page ',Page,' de 2');
  Box(2,5,79,21,$17);
  Case Page of
   1:Begin
    WriteCenter(5,'AmpŠres nominaux, charges r‚sistives',$17);
    WriteCenter(6,'Monophas‚',$17);
    GotoXY(2,7);
    Write(DuplicateString('_',77));
    GotoXY(5,8);
    Write('KW');
    GotoXY(17,8);
    Write('120v');
    GotoXY(34,8);
    Write('208v');
    GotoXY(51,8);
    Write('240v');
    GotoXY(68,8);
    Write('277v');
    GotoXY(2,9);
    Write(DuplicateString('-',78));
    GotoXY(7,10);
    Write('.5');
    GotoXY(7,11);
    Write('.75');
    GotoXY(6,12);
    Write('1');
    GotoXY(6,13);
    Write('2');
    GotoXY(6,14);
    Write('3');
    GotoXY(6,15);
    Write('5');
    GotoXY(6,16);
    Write('7.5');
    GotoXY(5,17);
    Write('10');
    GotoXY(5,18);
    Write('25');
    GotoXY(5,19);
    Write('50');
    GotoXY(4,20);
    Write('100');
    GotoXY(18,10);
    Write('4.2');
    GotoXY(18,11);
    Write('6.3');
    GotoXY(18,12);
    Write('8.4');
    GotoXY(17,13);
    Write('16.7');
    GotoXY(17,14);
    Write('25.0');
    GotoXY(17,15);
    Write('41.7');
    GotoXY(17,16);
    Write('62.5');
    GotoXY(17,17);
    Write('83.4');
    GotoXY(16,18);
    Write('208.4');
    GotoXY(16,19);
    Write('416.7');
    GotoXY(16,20);
    Write('833.4');
    GotoXY(35,10);
    Write('2.5');
    GotoXY(35,11);
    Write('3.7');
    GotoXY(35,12);
    Write('4.9');
    GotoXY(35,13);
    Write('9.7');
    GotoXY(34,14);
    Write('14.5');
    GotoXY(34,15);
    Write('24.1');
    GotoXY(34,16);
    Write('36.1');
    GotoXY(34,17);
    Write('48.1');
    GotoXY(33,18);
    Write('120.2');
    GotoXY(33,19);
    Write('240.4');
    GotoXY(33,20);
    Write('480.8');
    GotoXY(52,10);
    Write('2.1');
    GotoXY(52,11);
    Write('3.2');
    GotoXY(52,12);
    Write('4.2');
    GotoXY(52,13);
    Write('8.4');
    GotoXY(51,14);
    Write('12.5');
    GotoXY(51,15);
    Write('21.0');
    GotoXY(51,16);
    Write('31.3');
    GotoXY(51,17);
    Write('41.7');
    GotoXY(50,18);
    Write('104.2');
    GotoXY(50,19);
    Write('208.4');
    GotoXY(50,20);
    Write('416.7');
    GotoXY(69,10);
    Write('1.9');
    GotoXY(69,11);
    Write('2.8');
    GotoXY(69,12);
    Write('3.7');
    GotoXY(69,13);
    Write('7.3');
    GotoXY(69,14);
    Write('10.9');
    GotoXY(69,15);
    Write('18.1');
    GotoXY(69,16);
    Write('27.1');
    GotoXY(69,17);
    Write('36.2');
    GotoXY(69,18);
    Write('90.3');
    GotoXY(68,19);
    Write('180.6');
    GotoXY(68,20);
    Write('361.1');
   End;
   2:Begin
    WriteCenter(5,'AmpŠres nominaux, charges r‚sistives',$17);
    WriteCenter(6,'Trois phase',$17);
    GotoXY(2,7);
    Write(DuplicateString('_',77));
    GotoXY(5,8);
    Write('KW');
    GotoXY(23,8);
    Write('208v');
    GotoXY(42,8);
    Write('240v');
    GotoXY(60,8);
    Write('480v');
    GotoXY(2,9);
    Write(DuplicateString('-',78));
    GotoXY(5,10);
    Write('1');
    GotoXY(5,11);
    Write('2');
    GotoXY(5,12);
    Write('3');
    GotoXY(5,13);
    Write('4');
    GotoXY(5,14);
    Write('5');
    GotoXY(5,15);
    Write('7');
    GotoXY(5,16);
    Write('10');
    GotoXY(5,17);
    Write('25');
    GotoXY(5,18);
    Write('50');
    GotoXY(5,19);
    Write('100');
    GotoXY(23,10);
    Write('2.8');
    GotoXY(23,11);
    Write('5.6');
    GotoXY(23,12);
    Write('8.4');
    GotoXY(22,13);
    Write('11.2');
    GotoXY(22,14);
    Write('13.9');
    GotoXY(22,15);
    Write('19.6');
    GotoXY(22,16);
    Write('27.8');
    GotoXY(22,17);
    Write('69.5');
    GotoXY(21,18);
    Write('139');
    GotoXY(21,19);
    Write('278');
    GotoXY(42,10);
    Write('2.5');
    GotoXY(42,11);
    Write('4.9');
    GotoXY(42,12);
    Write('7.3');
    GotoXY(42,13);
    Write('9.7');
    GotoXY(41,14);
    Write('12.1');
    GotoXY(41,15);
    Write('17');
    GotoXY(41,16);
    Write('24.1');
    GotoXY(41,17);
    Write('60.3');
    GotoXY(40,18);
    Write('120.5');
    GotoXY(40,19);
    Write('240.9');
    GotoXY(60,10);
    Write('1.3');
    GotoXY(60,11);
    Write('2.5');
    GotoXY(60,12);
    Write('3.7');
    GotoXY(60,13);
    Write('4.9');
    GotoXY(60,14);
    Write('6.1');
    GotoXY(60,15);
    Write('8.5');
    GotoXY(60,16);
    Write('12.1');
    GotoXY(60,17);
    Write('30.2');
    GotoXY(60,18);
    Write('60.3');
    GotoXY(59,19);
    Write('120.5');
   End;
  End;
  Box(2,23,79,23,$17);
  WriteCenter(23,'Presse ESC pour le menu principal ou la barre d''espace pour d‚filer',$17);
  If Page=1 Then Page:=2
            Else Page:=1;
 Until ReadKey=#27;
 If(Keypressed)Then ReadKey;
End;

Function Menu:Char;
Begin
 Box(2,2,79,2,$17);
 WriteCenter(2,'CALCULATEUR ELECTRIQUE - ELECTRC',$1F);
 Box(2,4,79,21,$17);
 Box(2,23,79,23,$17);
 TextColor(7);
 GotoXY(3,5);
 Write('(A) Puissance DC / Voltage / Courant / Calculs de r‚sistance');
 GotoXY(3,6);
 Write('(B) Calculs d''‚clairage (m‚thode de la cavit‚ zonale)');
 GotoXY(3,7);
 Write('(C) Calculateur de chute de tension');
 GotoXY(3,8);
 Write('(D) S‚lecte la taille du fil en fonction du pourcentage de chute de tension');
 GotoXY(3,9);
 Write('(E) Calculer le co–t d''exploitation de diverses charges');
 GotoXY(3,10);
 Write('(F) S‚lecteur de conduits');
 GotoXY(3,11);
 Write('(G) Calculateur de remplissage de boŒte d''appareil');
 GotoXY(3,12);
 Write('(H) Tableau des conducteurs d''‚lectrode de mise … la terre');
 GotoXY(3,13);
 Write('(I) Tableau des conducteurs de terre de l''‚quipement');
 GotoXY(3,14);
 Write('(J) Exigences de couverture souterraine');
 GotoXY(3,15);
 Write('(K) Tableau de KW en AmpŠre');
 WriteCenter(23,'ENTRER LA LETTRE DE SELECTION [Q pour Quitter]',$17);
 Menu:=UpCase(ReadKey);
End;

BEGIN
 {$IFDEF FPC}
  {$IFDEF WINDOWS}
   SetUseACP(False);
  {$ENDIF}
 {$ENDIF}
 If(ParamStr(1)='/?')or(ParamStr(1)='--help')or(ParamStr(1)='-h')Then Begin
  WriteLn('ELECTRC : Cette commande permet de lancer l''application Electrc.');
  WriteLn;
  WriteLn('Syntaxe : ELECTRC');
  WriteLn;
  WriteLn(' /ASCII128   Ce parametre permet de lancer en mode compatibilite 128 caracteres seulement.');
 End
  Else
 Begin
  ShowASCII128:=False;
  If ParamCount>0Then For I:=1 to ParamCount do Begin
   If StrToUpper(ParamStr(I))='/ASCII128'Then ShowASCII128:=True
    Else
   Begin
    WriteLn('Parametre inconnu : ',ParamStr(I));
    Halt;
   End;
  End;
  Init;
  Repeat
   C:=Menu;
   Case(C)of
    'A':DCPowerVoltageCurrentResistanceCalcs;
    'E':CalculateCostToOperateVariousLoads;
    'H':GroundingElectrodeConductorTable;
    'I':EquipmentGroundConductorTable;
    'J':UndergroundCoverRequirements;
    'K':KWToAmpereTables;
   End;
  Until C='Q';
  TextBackground(Black);
  TextColor(LightGray);
  ClrScr;
 End;
END.
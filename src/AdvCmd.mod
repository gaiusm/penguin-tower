IMPLEMENTATION MODULE AdvCmd ;


FROM ASCII IMPORT ff, esc ;
FROM Storage IMPORT ALLOCATE ;

FROM AdvMap IMPORT IncPosition ;
FROM DrawG IMPORT DrawMan, EraseMan ;
FROM AdvTreasure IMPORT GetTreasure, DropTreasure, UseTreasure, Inventory ;
FROM ProcArgs IMPORT SetArgs ;

FROM AdvSystem IMPORT ArrowArgs,
                      Player, TypeOfDeath, PlayerNo,
                      GetWriteAccessToPlayer,
                      ReleaseWriteAccessToPlayer,
                      GetReadAccessToPlayer,
                      ReleaseReadAccessToPlayer,
                      GetAccessToScreenNo,
                      ReleaseAccessToScreenNo ;

FROM AdvMath IMPORT UpDateWoundsAndFatigue,
                    StrengthToFireArrow,
                    StrengthToFireMagic ;

FROM AdvUtil IMPORT MoveMan, Exit, InitialDisplay,
                    OpenDoor, CloseDoor, ExamineDoor,
                    Attack, Thrust, Parry,
                    Speak, Alive ;

FROM Screen IMPORT Height, Width, WriteTime, ClearScreen, Pause,
                   WriteString,
                   WriteCommentLine1,
                   DelCommentLine1,
                   WriteArrows, WriteMagicArrows ;


CONST
   CtrlL   =    ff ;


(*
   CheckTilde - we check for ~ ! to toggle the anim.
*)

PROCEDURE CheckTilde (ch: CHAR) ;
VAR
   p: CARDINAL ;
BEGIN
   p := PlayerNo() ;
   WITH Player[p] DO
      IF tilde AND (ch = '!')
      THEN
         anim := NOT anim ;
         tilde := FALSE ;

      ELSIF ch = '~'
      THEN
         tilde := TRUE
      ELSIF ch = '#'
      THEN
         (* (internal) Shell *)
         tilde := FALSE
      ELSE
         tilde := FALSE
      END
   END
END CheckTilde ;


(* Adventure Commands! - Interpreted *)


PROCEDURE ExecuteCommand (ch: CHAR ; VAR Dead: BOOLEAN) ;
VAR
   p: CARDINAL ;
BEGIN
   p := PlayerNo() ;
   WITH Player[p] DO
      GetWriteAccessToPlayer ;
      IF Alive ()
      THEN
         WriteTime (p) ;  (* always start by displaying the current time.  *)

         ReleaseWriteAccessToPlayer ;
         GetAccessToScreenNo(p) ;
         UpDateWoundsAndFatigue(p) ;
         ReleaseAccessToScreenNo(p) ;

         CheckTilde (ch) ;
         CASE ch OF

         'h'      : Help |
         'i'      : Inventory |
         'v'      : ValtTurn |
         'r'      : RightTurn |
         'l'      : LeftTurn |
         '0'..'9' : MoveMan (ORD (ch)-ORD ('0')) |
         'f'      : FireNormalArrow |
         'm'      : FireMagicArrow |
         'p'      : Parry |
         't'      : Thrust |
         'a'      : Attack |
         'o'      : OpenDoor |
         'c'      : CloseDoor |
         'e'      : ExamineDoor |
         'g'      : GetTreasure |
         'd'      : DropTreasure |
         'u'      : UseTreasure |
         's'      : Speak |
         'w'      : |

         CtrlL    : RedrawScreen |

         esc      : Exit ;
                    Entity.DeathType := exitdungeon

         ELSE
         END ;
         GetWriteAccessToPlayer ;
         IF Alive ()
         THEN
         END
      END ;
      ReleaseWriteAccessToPlayer
   END
END ExecuteCommand ;


PROCEDURE Help ;
VAR
   p: CARDINAL ;
BEGIN
   p := PlayerNo() ;
   GetAccessToScreenNo(p) ;
   ClearScreen(p) ;
   WriteString(p, 'Key commands are:') ;
   WriteString(p, '') ;
   WriteString(p, "'1'..'9'   move 1..9 squares forward") ;
   WriteString(p, "'r'        turn right") ;
   WriteString(p, "'l'        turn left") ;
   WriteString(p, "'v'        vault turn") ;
   WriteString(p, "'f'        fire normal arrow") ;
   WriteString(p, "'m'        fire magic arrow") ;
   WriteString(p, "'o'        open door in front of you") ;
   WriteString(p, "'c'        close door in front of you") ;
   WriteString(p, "'e'        examine wall for secret door in front of you") ;
   WriteString(p, "'p'        parry with sword") ;
   WriteString(p, "'a'        attack with sword") ;
   WriteString(p, "'t'        thrust with sword") ;
   WriteString(p, "'g'        get treasure in front of you") ;
   WriteString(p, "'d' <no>   drop treasure in front of you") ;
   WriteString(p, "'u' <no>   use treasure") ;
   WriteString(p, "'F11       toggle full screen") ;
   WriteString(p, "'F12       toggle animation mode") ;
   ReleaseAccessToScreenNo(p) ;
   Pause(p) ;
   RedrawScreen
END Help ;


PROCEDURE ValtTurn ;
VAR
   p: CARDINAL ;
BEGIN
   p := PlayerNo() ;
   GetWriteAccessToPlayer ;
   EraseMan(p) ;
   WITH Player[p] DO
      Direction := (Direction+2) MOD 4 ;
   END ;
   DrawMan( p ) ;
   ReleaseWriteAccessToPlayer
END ValtTurn ;


PROCEDURE RightTurn ;
VAR
   p: CARDINAL ;
BEGIN
   p := PlayerNo() ;
   GetWriteAccessToPlayer ;
   EraseMan(p) ;
   WITH Player[p] DO
      Direction := (Direction+3) MOD 4 ;
   END ;
   DrawMan(p) ;
   ReleaseWriteAccessToPlayer
END RightTurn ;


PROCEDURE LeftTurn ;
VAR
   p: CARDINAL ;
BEGIN
   p := PlayerNo() ;
   GetWriteAccessToPlayer ;
   EraseMan(p) ;
   WITH Player[p] DO
      Direction := (Direction+1) MOD 4 ;
   END ;
   DrawMan( p ) ;
   ReleaseWriteAccessToPlayer
END LeftTurn ;


PROCEDURE SendFireToProcess (p, r, x, y, d: CARDINAL; magic: BOOLEAN) ;
VAR
   aa: ArrowArgs ;
BEGIN
   NEW(aa) ;
   WITH aa^ DO
      ArrowPlayer := p ;
      ArrowRoom := r ;
      ArrowX := x ;
      ArrowY := y ;
      ArrowDir := d ;
      IsMagic := FALSE
   END ;
   WITH Player[p] DO
      IF magic
      THEN
         aa := SetArgs(MagicProcArgs, aa)
      ELSE
         aa := SetArgs(NormalProcArgs, aa)
      END
   END
END SendFireToProcess ;


PROCEDURE FireMagicArrow ;
VAR
   r,
   x, y, p,
   Dir    : CARDINAL ;
BEGIN
   p := PlayerNo() ;
   WITH Player[p] DO
      GetWriteAccessToPlayer ;
      IF StrengthToFireMagic ()
      THEN
         IF NoOfMagic>0
         THEN
            DEC(NoOfMagic) ;
            Dir := Direction ;
            x := Xman ;
            y := Yman ;
            r := RoomOfMan ;
            ReleaseWriteAccessToPlayer ;
            IncPosition(x, y, Dir) ;
            SendFireToProcess(p, r, x, y, Dir, TRUE) ;
            GetAccessToScreenNo(p) ;
            WriteMagicArrows(p, NoOfMagic) ;
            ReleaseAccessToScreenNo(p)
         ELSE
            ReleaseWriteAccessToPlayer ;
            GetAccessToScreenNo(p) ;
            DelCommentLine1(p) ;
            WriteCommentLine1(p, 'None left') ;
            ReleaseAccessToScreenNo(p)
         END
      ELSE
         ReleaseWriteAccessToPlayer
      END
   END
END FireMagicArrow ;


PROCEDURE FireNormalArrow ;
VAR
   r,
   x, y, p,
   Dir    : CARDINAL ;
BEGIN
   p := PlayerNo() ;
   WITH Player[p] DO
      GetWriteAccessToPlayer ;
      IF StrengthToFireArrow ()
      THEN
         IF NoOfNormal>0
         THEN
            DEC(NoOfNormal) ;
            Dir := Direction ;
            x := Xman ;
            y := Yman ;
            r := RoomOfMan ;
            ReleaseWriteAccessToPlayer ;
            IncPosition(x, y, Dir) ;
            SendFireToProcess(p, r, x, y, Dir, FALSE) ;
            GetAccessToScreenNo(p) ;
            DelCommentLine1(p) ;
            WriteArrows(p, NoOfNormal) ;
            ReleaseAccessToScreenNo(p)
         ELSE
            ReleaseWriteAccessToPlayer ;
            GetAccessToScreenNo(p) ;
            WriteCommentLine1(p, 'None left') ;
            ReleaseAccessToScreenNo(p)
         END
      ELSE
         ReleaseWriteAccessToPlayer
      END
   END
END FireNormalArrow ;


PROCEDURE RedrawScreen ;
BEGIN
   InitialDisplay
END RedrawScreen ;


END AdvCmd.

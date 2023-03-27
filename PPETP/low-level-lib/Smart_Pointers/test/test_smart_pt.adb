with Ada.Text_Io, Smart_Integer_Pt;
use  Ada.Text_Io, Smart_Integer_Pt;

procedure Test_Smart_Pt is
   X : S_Pt.Smart_Pt;
begin
   declare
      Y : S_Pt.Smart_Pt := S_Pt.New_Pt (new Integer'(5));
      Z : S_Pt.Smart_Pt := S_Pt.New_Pt (new Integer'(42));
   begin
      X := Y;
      Put_Line ("x = " & Integer'Image(S_Pt.Pt(X).all));
      Put_Line ("y = " & Integer'Image(S_Pt.Pt(Y).all));
      Put_Line ("z = " & Integer'Image(S_Pt.Pt(Z).all));
   end;

   Put_Line ("x = " & Integer'Image(S_Pt.Pt(X).all));
end  Test_Smart_Pt;

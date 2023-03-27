with Text_Io;
use Text_Io;

with Ada.Exceptions;
use  Ada.Exceptions;

package body Check_Field is
   Failed : exception;

   function Check_Division_By_Zero
     return Boolean is
      Ok : Boolean;
      A  : Field;
   begin
      Put ("Checking division by zero...");

      Ok := False;
      begin
         A := Inv(Zero);
      exception
         when Numeric_Error =>
            Ok := True;
         when others =>
            raise;
      end;

      if (Ok) then
         Put_Line ("Ok");
         return True;
      else
         Put_Line ("Division by zero not raised");
         return False;
      end if;
   end Check_Division_By_Zero;

   function Check_Inverses return Boolean is
      A : Field;
      G : Generator;
   begin
      Put ("Checking inverses...");
      Reset (G);
      while Again(G) loop
         A := Value (G); Next (G);

         begin
            if (A /= Zero and then A * Inv(A) /= One) then
               raise Failed with "Inv";
            end if;

            if (A + (-A) /= Zero) then
               raise Failed with "Unary minus";
            end if;
         exception
            when E : Failed =>
               Put_Line (Exception_Message(E) & " failed for A=" & Image(A));
               return false;
            when others =>
               Put_Line("A=" & Image(A));
               raise;
         end;
      end loop;

      Put_Line ("OK");
      return True;
   end Check_Inverses;

   function Check_Commutativity return Boolean is
      A, B : Field;
      G, H : Generator;
   begin
      Put ("Checking commutativity...");

      Reset (G);
      while (Again (G)) loop
         A := Value(G); Next (G);

         Reset (H);
         while (Again (H)) loop
            B := Value (H) ; Next (H);

            begin
               if (A+B /= B+A) then
                  raise Failed with "sum";
               end if;

               if (A*B /= B*A) then
                  raise Failed with "product";
               end if;
            exception
               when E : Failed =>
                  Put_Line (Exception_Message(E)
                              & " failed for A=" & Image(A)
                              & " B="            & Image(B));
                  return False;
               when others =>
                  raise;
            end;
         end loop;
      end loop;

      Put_Line ("OK");
      return True;
   end Check_Commutativity;

   function Check_Distributivity return Boolean is
      A, B, C, D, F : Field;
      G, H, K : Generator;
   begin
      Put_Line ("Checking distributuvity....");

      Reset (G);
      while (Again (G)) loop
         A := Value(G); Next (G);
         Put (Image(A) & " ");

         Reset (H);
         while (Again (H)) loop
            B := Value (H) ; Next (H);

            Reset (K);
            while (Again (K)) loop
               C := Value (K) ; Next (K);

               begin
                  D := A*(B+C);
                  F := A*B+A*C;

                  if (D /= F) then
                     raise Failed;
                  end if;
               exception
                  when Failed =>
                     Put_Line ("failed for A=" & Image(A) & ", "
                                 & "B="        & Image(B) & ", "
                                 & "C="        & Image(C) & ", "
                                 & "A*(B+C)="  & Image(D) & ", "
                                 & "A*B+A*C="  & Image(F));
                     return False;
                  when others =>
                     raise;
               end;
            end loop;
         end loop;
      end loop;
      Put_Line ("OK");
      return True;
   end Check_Distributivity;

   --------------
   -- Do_Check --
   --------------

   function Check
     return Boolean is
   begin
      Put_Line ("Checking field " & Name & "...");
      return Check_Division_By_Zero
        and Check_Inverses
        and Check_Commutativity
        and Check_Distributivity;
   end Check;

end Check_Field;

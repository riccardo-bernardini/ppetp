with Byte_Arrays;               use Byte_Arrays;
package body Auth.Checkers.Tokens is

   -----------
   -- Check --
   -----------

   procedure Check
     (Checker    : in out Token_Checker;
      Credential : in     Credential_Type;
      Result     :    out Boolean)
   is
      Payload : Byte_Array := Credential.Credential.Data;
   begin
      pragma Assert (Checker.N_Used <= Checker.N_Tokens);

      if (Checker.N_Used = Checker.N_Tokens or else
            Payload'Size /= Token_Type'Size) then
         Result := False;
         return;
      end if;

      declare
         Received : Token_Type := Token_Type (Payload);
      begin
         for I in Checker.Valid_Tokens'Range loop
            if (not Checker.Valid_Tokens (I).Used and then
                  Checker.Valid_Tokens (I).token = Received) then
               Checker.Valid_Tokens (I).Used := True;
               Result := True;
               return;
            end if;
         end loop;
      end;

      Result := False;
   end Check;

   -----------------
   -- New_Checker --
   -----------------

   function New_Checker
     (List : Token_List)
      return Token_Checker_Pt
   is
      Result : Token_Checker_Pt := new Token_Checker(List'Size);                                                      --Valid_Tokens => <>);
      Pos : Positive;
   begin

      Pos := Result.Valid_Tokens'First;
      for I in List'Range loop
         Result.Valid_Tokens (Pos) := (Token => List (I),
                                       Used  => False);
         Pos := Pos + 1;
      end loop;

      Result.N_Used := 0;
      return Result;
   end New_Checker;

end Auth.Checkers.Tokens;

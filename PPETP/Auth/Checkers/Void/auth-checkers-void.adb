package body Auth.Checkers.Void is

   -----------
   -- Check --
   -----------

   procedure Check
     (Checker    : in out Void_Checker;
      Credential : in     Credential_Type;
      Result     :    out Boolean)
   is
   begin
      Result := True;
   end Check;

   -----------------
   -- New_Checker --
   -----------------

   function New_Checker
     return Void_Checker_Pt
   is
   begin
      return new Void_Checker;
   end New_Checker;


end Auth.Checkers.Void;

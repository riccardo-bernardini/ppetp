with Profiles.Entanglers.Basic;
with Profiles.Entanglers.Vandermonde;

package body profiles.entanglers is
   -------------------
   -- New_Entangler --
   -------------------

   function New_Entangler
     (Profile    : Profile_Type;
      Parameters : Parameters_Class_Pt := null)
      return       Entangler_Class_Pt
   is
      Result : Entangler_Class_Pt;
   begin
      case Profile is
         when Basic_Profile =>
            Result := Entangler_Class_Pt (Basic.New_Entangler);
         when Vandermonde_Profile =>
            -- return
            Result := Entangler_Class_Pt(Vandermonde.New_Entangler);

            -- when Vandermonde_Profile =>
            -- return
            --   Entangler_Class_Pt(Vandermonde.New_Entangler(Parameters));
      end case;

      if Parameters /= null then
         Result.Set_Default(Parameters);
      end if ;


      return Result;
   end New_Entangler;


   -----------------
   -- Set_Default --
   -----------------

   procedure Set_Default
     (Handler    : in out Root_Entangler;
      Parameters : in Parameters_Class_Pt)
   is
   begin
      Handler.Default := Parameters;
   end Set_Default;

   -----------------
   -- Get_Default --
   -----------------

   function Get_Default
     (Handler : Root_Entangler)
      return    Parameters_Class_Pt
   is
   begin
      return Handler.Default;
   end Get_Default;


end profiles.entanglers;

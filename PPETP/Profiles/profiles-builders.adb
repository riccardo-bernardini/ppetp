with Profiles.Builders.Basic;
with Profiles.Builders.Vandermonde;
with Ada.Unchecked_Deallocation;

package body profiles.builders is

   -----------------
   -- New_Builder --
   -----------------

   function New_Builder (Profile : Profile_Type) return Builder_Class_Pt is
      Result : Builder_Class_Pt;
   begin
      case Profile is
         when Basic_Profile =>
            Result := Builder_Class_Pt (Basic.New_Builder);
         when Vandermonde_Profile =>
            Result := Builder_Class_Pt (Vandermonde.New_Builder);
      end case;


      return Result;
   end New_Builder;



   ------------------------
   -- New_Builder_Table --
   -----------------------

   function New_Builder_Table return Builder_Table is
      Result : Builder_Table;
   begin
      for I in Result'Range loop
         Result (I) := New_Builder (I);
      end loop;

      return Result;
   end New_Builder_Table;

   --------------
   -- Finalize --
   --------------

   procedure Finalize (Object : in out Builder_Table) is
   begin
      for I in Profile_Type loop
         Free (Object (I));
      end loop;
   end Finalize;

end profiles.builders;

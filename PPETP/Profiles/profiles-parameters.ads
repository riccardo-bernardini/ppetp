with Ada.Unchecked_Deallocation;

package Profiles.Parameters is
   -- ================ --
   -- == Parameters == --
   -- ================ --

   type Root_Parameters is abstract new Root_Profile_Handler with null record;
   type Parameters_Class_Pt is access all Root_Parameters'Class;

   type Root_Parameters_Pt is access Root_Parameters;

   -- type Parameters_Table is array (Profile_Type) of Parameters_Class_Pt;

   function Image(X : Root_Parameters) return String is abstract;

   function "=" (X, Y: Root_Parameters) return Boolean is abstract;

   procedure Free is
     new Ada.Unchecked_Deallocation(Object => Root_Parameters'Class,
                                    Name   => Parameters_Class_Pt);

end Profiles.Parameters;

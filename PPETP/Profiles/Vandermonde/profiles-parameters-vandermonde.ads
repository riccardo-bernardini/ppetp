with Galois_Field; use Galois_Field;
with Ada.Unchecked_Deallocation;

package  Profiles.Parameters.Vandermonde is

   type Vandermonde_Par is new  Root_Parameters with
      record
         Reduction_Factor : Natural;  --> è R
         RF_Valid         : Boolean := True;
         Galois_Element   : Galois   ;   --> è b!
         GE_Valid         : Boolean := True;
         GF_Size          : Natural;
         GF_Valid         : Boolean := True;
      end record;

   type Vandermonde_Par_Pt is access Vandermonde_Par;


   function New_Parameter(Reduction_Factor : in Natural;  --> è R
                          Galois_Element   : in Galois   ;   --> è b!
                          GF_Size	   : in Natural)  return Parameters_Class_Pt;


   function Image(X : Vandermonde_Par) return String;

   function "=" (X, Y: Vandermonde_Par) return Boolean;

   procedure Free is
      new Ada.Unchecked_Deallocation(Object => Vandermonde_Par,
                                     Name   => Vandermonde_Par_Pt);

private



end Profiles.Parameters.Vandermonde;

-- COME è DEFINITO IL ROOT_PARAMETERS ?
--
--  package Profiles.Parameters is
--     -- ================ --
--     -- == Parameters == --
--     -- ================ --
--
--     type Root_Parameters is abstract new Root_Profile_Handler with null record;
--     type Parameters_Class_Pt is access all Root_Parameters'Class;
--
--     -- type Parameters_Table is array (Profile_Type) of Parameters_Class_Pt;
--
--     function Image(X : Root_Parameters) return String is abstract;
--     function "=" (X, Y: Root_Parameters) return Boolean is abstract;
--  end Profiles.Parameters;

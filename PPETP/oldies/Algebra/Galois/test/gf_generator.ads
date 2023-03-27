with Interfaces;
use  Interfaces;

with Gf_2p_Varsize;
generic
   with package GF is new GF_2p_Varsize(<>);
   Max_Exponent_For_Full : Natural := 16;
package Gf_Generator is
   type Generator is private;
   type Generator_Class is (Full, Random, Default);

   procedure Reset (G : in out Generator);
   procedure Set_Class (G     : in out Generator;
                        Class :        Generator_Class);

   procedure Next  (G : in out Generator);
   function  Again (G : in     Generator) return Boolean;
   function  Value (G : in     Generator) return GF.Galois;

   Overflow : exception;
private
   type Generator is record
      Class : Generator_Class := Default;
      Current   : GF.Galois;
      Contatore : Natural;
      Massimo   : Natural;
   end record;
end Gf_Generator;

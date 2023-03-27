with PPETP;
with Profiles.Parameters;    use Profiles.Parameters;

package Profiles.Entangled is
   -- =============== --
   -- == Entangled == --
   -- =============== --

   type Root_Entangled_Payload is abstract new Root_Profile_Handler with null record;
   type Entangled_Payload_Pt is access all Root_Entangled_Payload'Class;

   type Entangled_Data is
      record
         Sequence_Num : PPETP.Data_Sequence_Number;
         Payload      : Entangled_Payload_Pt;
      end record;

   function "=" (X, Y: Root_Entangled_Payload) return Boolean is abstract;

end Profiles.Entangled;

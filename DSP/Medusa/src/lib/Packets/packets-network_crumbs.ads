with Timestamps;
use  Timestamps;

package Packets.Network_Crumbs is
   type Network_Crumb (Size : Natural; Explicit_RV : Boolean) is record
      Component_Info : Component_Info_Type;
      Payload   : Stream_Element_Array (1..Size);

      case Explicit_Rv is
         when True =>
            Reduction_Vector : Reduction_IDX;
         when False =>
            null;
      end case;
   end record;

   type Network_Crumb_Access is
     access Network_Crumb;

   type Network_Crumb_Array is
     array(Positive range <>) of Network_Crumb_Access;

   function Crumb_To_Bytes (Input : Network_Crumb)
                     return Stream_Element_Array;

   function Converted_Length (Input : Network_Crumb)
                             return Stream_Element_Offset;

   procedure Bytes_To_crumb (Input  : in     Stream_Element_Array;
                             Crumb  :    out Network_Crumb_Accessb;
                             cursor : in out Stream_Element_Offset);
end Packets.Network_Crumbs;

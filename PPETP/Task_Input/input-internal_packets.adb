with Byte_Arrays;                    use Byte_Arrays;

package body Input.Internal_Packets is

   procedure Finalize(Object: in out Internal_Packet_Pt) is
   begin
      Free(Object.Data);
      Free(Object);
   end Finalize;


end Input.Internal_Packets;

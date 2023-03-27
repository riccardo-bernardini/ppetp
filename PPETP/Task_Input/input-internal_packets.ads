with Ada.Unchecked_Deallocation;
with Byte_Arrays;                    use Byte_Arrays;
with Network;                        use Network;

package Input.Internal_Packets is
   type Internal_Class is (Received_Data, Timeout_Event);

   type Internal_Packet (Class : Internal_Class) is record
      case Class is
         when Received_Data =>
            Data : Byte_Array_Pt;
            Peer : Sock_Addr_Type;
         when Timeout_Event =>
            null;
      end case;
   end record;

   type Internal_Packet_Pt is access Internal_Packet;

   procedure Free is
     new Ada.Unchecked_Deallocation (Object => Internal_Packet,
                                     Name   => Internal_Packet_Pt);

   No_Packet  : constant Internal_Packet := (Class => Received_Data,
                                             Data  => null,
                                             Peer  =>
                                               (Family_Inet,
                                                No_Inet_Addr,
                                                Any_Port));

   procedure Finalize(Object:  in out Internal_Packet_Pt);

end Input.Internal_Packets;

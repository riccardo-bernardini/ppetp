with Packets.Protocol.Utilities;   use Packets.Protocol.Utilities;
with Network;                      use Network;
with Profiles;                     use Profiles;
with Interfaces;                   use Interfaces;
with Ada.Streams;                  use Ada.Streams;
with Ada.Text_IO;                  use Ada.Text_IO;
with PPETP;
with Auth.Credentials;
-- with Common_Types;                 use Common_Types;
-- with System;

package body Packets.Protocol.Command is
   procedure Print (A : Control_Packet) is
   begin
      Put_Line ("Type = Control");
      Print (Protocol_Packet (A));
      New_Line;
      Put_Line ("Command : " & Request_Type'Image (A.Command));


      case A.Command is
         when Set_Default =>
            Put_Line ("Set_Default");
         when Acknowledge =>
            Put_Line ("ACK to   : " & A.ACKed_Number'img &
                      " : [sub: " & PPETP.Sub_Sequence_Number'Image (A.ACKed_Sub_Number) & " ] " &
		      " : " & ACK_Reason_Type'Image(A.ACK_Reason));
         when Hello =>
            Put("Hello : ");

         when Data_Control =>
            Put_Line ("Data Control");
         when Forward =>
            Put_Line ("Forward");
      end case;
   end;

   -- There is not dynamic memory to free in Command packets
   procedure Free(Object: in out Control_Packet) is
   begin
      null;
   end Free;

  function Is_Command (Packet : Network_Packet)
                        return Boolean is
      Command_Mask    : constant Unsigned_8 := 2#0010_0000#;
--      n: Unsigned_8;
   begin
--        n := Unsigned_8(Packet.Get(1));
--        Put_Line("pkt :" & n'img);
--        n := Unsigned_8(Packet.Get(2));
--        Put_Line("pkt :" & n'img);
--        n := Unsigned_8(Packet.Get(3));
--        Put_Line("pkt :" & n'img);
--        n := Unsigned_8(Packet.Get(4));
--        Put_Line("pkt :" & n'img);
      return
        ((Unsigned_8 (Packet.Get(1)) and Command_Mask) /= 0);
   end Is_Command;
   pragma Inline (Is_Command);
end Packets.Protocol.Command;

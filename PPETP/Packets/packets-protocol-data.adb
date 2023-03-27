with Ada.Text_IO;                 use Ada.Text_IO;
with System;
with Ada.Unchecked_Conversion;
with Ada.Unchecked_Deallocation;

package body Packets.Protocol.Data is
   procedure Print (A : Data_Packet) is
   begin
      Put_Line ("Type = Data");
      Print (Protocol_Packet (A));
      New_Line;
      --      Put_Line ("Length : " & Integer'Image(A.Payload'Length));
   end Print;

   procedure Free(Object: in out Data_Packet) is
   begin
      Free(Object.Payload.Data);
   end Free;

end Packets.Protocol.Data;

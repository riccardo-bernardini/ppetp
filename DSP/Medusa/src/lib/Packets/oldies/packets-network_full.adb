with Timestamps;
use  Timestamps;

with Packets.Network_Crumbs;

package body Packets.Network is
   type Reserved_Type is mod 2**3;

   type Network_Packet_Header is record
      Data_Packet  : Boolean;
      Reserved     : Reserved_Type;
      N_Components : Component_Counter;

      Constituent  : Constituent_ID;
      Stream       : Stream_ID;

      Class        : Packet_Class;
      Priority     : Packet_Priority;

      Timestamp    : Multimedia_Timestamp;
   end record;

   for Network_Packet_Header use
      record
         Data_Packet  at 0 range 0..0;  -- Always 1 for data
         Reserved     at 0 range 1..3;  -- Always 2#000#
         N_Components at 0 range 4..7;

         Constituent  at 1 range 0..3;
         Stream       at 1 range 4..7;

         class        at 2 range 0..3;
         Priority     at 2 range 4..7;

         Timestamp    at 3 range 0..31;
      end record;


   Elements_Per_Header : constant Integer :=
     Network_Packet_Header'Size / Stream_Element'Size;

   type Header_Buffer is Stream_Element_Array(1..Elements_Per_Header);

   function To_Header_Buffer is new
     Ada.Unchecked_Conversion (Source => Network_Packet_Header,
                               Target => Header_Buffer);

   function To_Network_Header is new
     Ada.Unchecked_Conversion (Source => Header_Buffer,
                               Target => Network_Packet_Header);


   function Packet_To_Bytes (Input : Network_Packet)
                            return Byte_Array is
      Packet_Length : Natural;

      Header : Network_Packet_Header :=
        (Data_Packet  => True,
         Reserved     => 2#000#,
         N_Components => Input.Payload'Length,
         Constituent  => Input.Id.Constituent,
         Stream       => Input.Id.Stream,
         Class        => Input.Id.Class,
         Priority     => Input.Id.Priority,
         Timestamp    => Input.Id.Timestamp);
   begin
      -- Compute the total packet length
      Packet_Length := Elements_Per_Header;
      for I in Input.Payload'Range loop
         Total_Payload_Length := Total_Payload_Length +
           Converted_Length (Input.Payload(I));
      end loop;

      declare
         Result : Stream_Element_Array(1..Packet_Length);
         Cursor : Stream_Element_Offset;
         Length : Stream_Element_Offset;
      begin
         Cursor := Result'First;

         Result(Cursor..Cursor+Elements_Per_Header-1) :=
           To_Header_Buffer(Header);
         Cursor := Cursor+Elements_Per_Header;

         for I in Input.Payload'Range loop
            Length := Converted_Length(Input.Payload(I));
            Result(Cursor..Cursor+Length-1)
              := Crumb_To_Bytes (Input.Payload(I));
            Cursor := Cursor + Length;
         end loop;
      end;
   end Packet_To_Bytes;


   function Bytes_To_Packet (Input : Stream_Element_Array)
                            return Network_Packet is
      Header : Network_Packet_Header;
      Cursor : Stream_Element_Offset;
   begin
      Cursor := Input'First;

      Header := To_Network_Header(Input(Cursor..Cursor+Elements_Per_Header-1));
      Cursor := Cursor+Elements_Per_Header;

      declare
         Result : Network_Packet (Header.N_Components);
      begin
         Result.Id := (Class       => Header.Class,
                       Priority    => Header.Priority,
                       Constituent => Header.Constituent,
                       Stream_ID   => Header.Stream,
                       Timestamp   => Header.Timestamp);

         for I in Result.Payload'Range loop
            Bytes_To_Crumb (Input  => Input,
                            Cursor => Cursor,
                            Crumb  => Result.Payload(I));
         end loop;

         return Result;
      end;
   end Bytes_To_Packet;
end Packets.Network;

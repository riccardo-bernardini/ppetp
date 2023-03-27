with Timestamps;
use  Timestamps;

with Packets.Network_Crumbs;
with From_To_Bytes;

package body Packets.Network is
   type Component_Counter is new Integer range 1..2**4;
   type Reserved_Type     is mod 2**3;

   type Network_Packet_Header is record
      Data_Packet  : Boolean;
      Reserved     : Reserved_Type;
      N_Components : Component_Counter;

      Class        : Packet_Class;
      Priority     : Packet_Priority;

      Timestamp    : Multimedia_Timestamp;
   end record;

   for Network_Packet_Header use
      record
         Data_Packet  at 0 range 0..0;  -- Always 1 for data
         Reserved     at 0 range 1..3;  -- Always 2#000#
         N_Components at 0 range 4..7;

         Class        at 1 range 0..3;
         Priority     at 1 range 4..7;

         Timestamp    at 2 range 0..31;
      end record;

   package Header_Conversion is
      new From_To_Bytes (Target_Type => Network_Packet_Header);


   function Packet_To_Bytes (Input : Network_Packet)
                            return Stream_Element_Array is
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
      Packet_Length := Header_Conversion.Elements_Per_Item;
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

         Result(Cursor..Cursor + Header_Conversion.Elements_Per_Item - 1) :=
           Header_Conversion.To_Byte(Header);
         Cursor := Cursor+Header_Conversion.Elements_Per_Item;

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

      Header := Header_Conversion.From_Byte (Input(Cursor..Cursor + Header_Conversion.Elements_Per_Item - 1));
      Cursor := Cursor + Header_Conversion.Elements_Per_Item;

      return Result : Network_Packet (Header.N_Components) do
        Result.Class     := Header.Class;
        Result.Priority  := Header.Priority;
        Result.Id        := (Timestamp => Header.Timestamp);

        for I in Result.Payload'Range loop
           Bytes_To_Crumb (Input  => Input,
                           Cursor => Cursor,
                           Crumb  => Result.Payload(I));
        end loop;
      end return;

      -- declare
      --    Result :
      -- begin
      --    Result.Class     := Header.Class;
      --    Result.Priority  := Header.Priority;
      --    Result.Id        := (Timestamp => Header.Timestamp);
      --
      --    for I in Result.Payload'Range loop
      --       Bytes_To_Crumb (Input  => Input,
      --                       Cursor => Cursor,
      --                       Crumb  => Result.Payload(I));
      --    end loop;
      --
      --    return Result;
      -- end;
   end Bytes_To_Packet;
end Packets.Network;

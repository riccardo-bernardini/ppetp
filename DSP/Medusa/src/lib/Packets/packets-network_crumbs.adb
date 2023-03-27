with Ada.Unchecked_Conversion;

package body Packets.Network_Crumbs is
   type Payload_Length_Type is mod 2**15;
   type Network_Crumb_Header is
      record
         Explicit_RV    : Boolean;
         Payload_Length : Payload_Length_Type;
      end record;

   for Network_Crumb_Header use
      record
         Explicit_RV     at 0 range 0..0;
         Payload_Length  at 0 range 1..15;
      end record;

   Header_Size : constant Stream_Element_Offset :=
     Network_Crumb_Header'Size / Stream_Element'Size;

   RV_Size : constant Stream_Element_Offset :=
     Reduction_IDX'Size / Stream_Element'Size;

   type Header_Buffer   is Stream_Element_Array(1..2);
   type RV_Index_Buffer is Stream_Element_Array(1..4);

   function To_Header_Buffer is
      new Ada.Unchecked_Conversion (Source => Network_Crumb_Header,
                                    Target => Header_Buffer);

   function To_Crumb_Header is
     new Ada.Unchecked_Conversion (Source => Stream_Element_Array,
                                   Target => Network_Crumb_Header);

   function To_RV_Index is
      new Ada.Unchecked_Conversion (Source => Stream_Element_Array,
                                    Target => Reduction_IDX);

   function RV_To_Bytes is
      new Ada.Unchecked_Conversion (Source => Reduction_IDX,
                                    Target => RV_Index_Buffer);

   function Crumb_To_Bytes (Input : Network_Crumb)
                     return Stream_Element_Array is
      Header : Network_Crumb_Header
        (Explicit_RV    => Input.Explicit_RV,
         Payload_Length => Input.Payload'Length);
   begin
      if (Header.Explicit_RV) then
         return  To_Header_Buffer (Header)
           & Stream_Element_Array(RV_To_Bytes(Input.Reduction_Vector))
           & Stream_Element_Array(Input.Payload);
      else
         return  To_Header_Buffer (Header)
           & Stream_Element_Array(Input.Payload);
      end if;
   end To_Bytes;

   function Converted_Length (Input : Network_Crumb)
                             return Stream_Element_Offset is
   begin
      if (Input.Explicit_RV) then
         return Input.Payload'Length + Header_Size + RV_Size;
      else
         return Input.Payload'Length + Header_Size;
      end if;
   end Converted_Length;

   procedure Bytes_To_Crumb (Input  : in     Stream_Element_Array;
                             Crumb  :    out Network_Crumb_Access;
                             Cursor : in out Stream_Element_Offset);
   is
     Header : Network_Crumb_Header;
   begin
      Header := To_Crumb_Header (Input(Cursor..Cursor + Header_Size-1));
      Cursor := Cursor+Header_Size;

      Crumb := new Network_Crumb(Size        => Header.Payload_Length,
                                 Explicit_RV => Header.Explicit_RV);

      if (Crumb.Explicit_RV)
        Crumb.Reduction_Vector := To_RV_Index(Input(Cursor..Cursor+RV_Size-1));
        Cursor := Cursor+RV_Size;
      end if;

      Crumb.Payload := Input(Cursor..Cursor+Header.Payload_Length-1);
      Cursor := Cursor+Header.Payload_Length;
   end Bytes_To_Crumb;
end Packets.Network_Crumbs;


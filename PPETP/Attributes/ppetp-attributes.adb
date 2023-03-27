with Ada.Exceptions;  use Ada.Exceptions;


with PPETP.Attributes;			use PPETP.Attributes;
with Packets.Protocol.Utilities;	use Packets.Protocol.Utilities;
with Parsing_Buffers;			use Parsing_Buffers;

with Ada.Text_IO;			use Ada.Text_IO;

package body PPETP.Attributes is

   Callback_Array : array (Attributes_Index_Type'Range) of Handler_Processing;

   procedure Extract_Attributes_Index is
     new Extract (Attributes_Index_Type);

   procedure Extract_Bit_Field_8 is
     new Extract (Bit_Field_8);



   --------------
   -- Register --
   --------------
   procedure Register(Index    : Attributes_Index_Type;
                      Callback : Handler_Processing) is
   begin
      if Callback_Array(Index) /= null then
         raise Yet_Registered_Index;
      else
         Callback_Array(Index) := Callback;
      end if;

   end Register;



   ------------------------
   -- Get_Next_Attribute --
   ------------------------
   procedure Get_Next_Attribute(Buffer : in out Parsing_Buffer;
                                Result :    out Access_Attribute_Class) is
      Index  : Attributes_Index_Type;
      Bt : Bit_Field_8;
      Length : Bit_Field_16;
   begin
      -- Extract the Attribute Index
      --      Extract_Attributes_Index(Buffer, Index);
      Extract_Bit_Field_8(Buffer, Bt);
      Index := Attributes_Index_Type(Bt);

     -- Put_Line("Attr : " & Index'img);
      -- Extract the length of the payload
      Extract_Bit_Field_8(Buffer, Bt);
      Length := Bit_Field_16(Bt) * 2**8;
      Extract_Bit_Field_8(Buffer, Bt);
      Length := Length + Bit_Field_16(Bt);
     -- Put_Line("Attr Length: " & Length'img);

      -- Extract the payload and process it
      declare
         type Payload_Type is array (1..Length) of Byte;

         procedure Extract_Payload is
           new Extract (Payload_Type);

         Payload: Payload_Type;
      begin

         Extract_Payload(Buffer, Payload);

         if Callback_Array(Index) = null then
            raise Unknown_Attributes;
         else
            -- Parsing of the Attribute
            Result := Callback_Array(Index)(Byte_Array(Payload));
         end if;
      end;

   end Get_Next_Attribute;


   -------------------
   -- Building_Data --
   -------------------
--     function Building_Data(Object: Access_Attribute_Class) return byte_array is
--        ret: Byte_Array(0..0);
--     begin
--        -- this function should never be called
--        Raise_Exception(E       => Program_Error'Identity,
--                        Message => "Trying to build Base PPETP Attribute");
--        return ret;
--     end Building_Data;


   --------------------------
   -- Get_Attribute_Packet --
   --------------------------
   function Get_Attribute_Packet(Object: Access_Attribute_Class) return byte_array is
      TLV_Header: Byte_Array(1..3);
      Data_Tmp : Byte_Array := Building_Data(Object.all);
      Length : Bit_Field_16 := Data_Tmp'Length ;
   begin

      Put_Line("Attr Length : " & Length'img);

      TLV_Header(1) := byte_arrays.Byte(Object.Attr_Type);
      TLV_Header(2) := byte_arrays.Byte( (Length and 16#FF_00#) / 2**8);
      TLV_Header(3) := byte_arrays.Byte(Length and 16#00_FF#);

      Put_Line("Type: " & TLV_Header(1)'img);
      Put_Line("MSB: " & TLV_Header(2)'img);
      Put_Line("LSB: " & TLV_Header(3)'img);

      return TLV_Header & Data_Tmp;


   end Get_Attribute_Packet;

   --------------
   -- Get_Type --
   --------------
   function Get_Type(Object: Access_Attribute_Class) return Attributes_Index_Type is
   begin
      return Object.Attr_Type;
   end Get_Type;


   ----------
   -- Free --
   ----------
   procedure Free(X: in out Access_Attribute_Class) is
      procedure Free_Obj is
        new Ada.Unchecked_Deallocation(Object => Root_Attributes'Class,
                                       Name   => Access_Attribute_Class);
   begin
   	Free_Obj(X);
   end Free;

end PPETP.Attributes;


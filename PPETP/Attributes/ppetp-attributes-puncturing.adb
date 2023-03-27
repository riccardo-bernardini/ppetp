with Ada.Streams;			use Ada.Streams;
with PPETP.Attributes;			use PPETP.Attributes;
with Network_Utilities;			use Network_Utilities;
with Packets.Protocol.Utilities;	use Packets.Protocol.Utilities;
with byte_arrays;			use byte_arrays;
with Interfaces;
with Ada.Unchecked_Conversion;

package body PPETP.Attributes.Puncturing is


   function Byte_To_Mode is
     new Ada.Unchecked_Conversion (Source => byte_arrays.Byte,
                                   Target => Puncturing_Mode);

   function Mode_To_Byte is
     new Ada.Unchecked_Conversion (Source => Puncturing_Mode,
                                   Target => byte_arrays.Byte);
   ------------------
   -- Parsing_Data --
   ------------------
   function Parsing_Data(Data: byte_Array) return Access_Attribute_Class is
      Mode : Puncturing_Mode;
   begin


      Mode := Byte_To_Mode(Data(1));

      case Mode is
         when Probabilistic =>
            declare
               Punct_Data : Puncturing_Data(Mode);
            begin
               Punct_Data.Num := Data(2);
               Punct_Data.Den := Data(3);

               return Access_Attribute_Class'(new PUNCTURING_Attribute'(Mode      => Mode,
                                                                        Attr_Type => PUNCTURING_Attribute_Index,
                                                                        Data      => Punct_Data));
            end;

         when Deterministic =>
            declare
               Punct_Data : Puncturing_Data(Mode);
            begin
               Punct_Data.M := Data(2);
               Punct_Data.Size := Data'Length - 2; -- 1 byte of Punt. Type and 1 byte for M, the remaining bytes are values

               for i in Stream_Element_Offset range 1..Data'Length - 2 loop
                  Punct_Data.Val(i) := Data(2+i);
               end loop;

               return Access_Attribute_Class'(new PUNCTURING_Attribute'(Mode      => Mode,
                                                                        Attr_Type => PUNCTURING_Attribute_Index,
                                                                        Data      => Punct_Data));
            end;

         when others =>
            raise Unknown_Puncturing_Mode;

      end case;

   end Parsing_Data;


   -------------------
   -- Building_Data --
   -------------------
   function Building_Data(Object: PUNCTURING_Attribute) return byte_array is
   begin
      case  Object.Mode is
         when Probabilistic =>
            declare
               Payload: Byte_Array(1..3);
            begin
               Payload(1) := Mode_To_Byte(Object.Mode);
               Payload(2) := Object.Data.Num;
               Payload(3) := Object.Data.Den;

               return Payload;
            end;

         when Deterministic =>
            declare
               Payload_Size : Stream_Element_Offset := 2 + Stream_Element_Offset(Object.Data.Size);
               Payload: Byte_Array(1..Payload_Size);
            begin
               Payload(1) := Mode_To_Byte(Object.Mode);
               Payload(2) := Object.Data.M;

               for i in 1 .. Payload_Size -2 loop
                  Payload(2+i) := Object.Data.Val(i);
               end loop;

               return Payload;
            end;
         when Null_Puncturing =>
            -- Null_Puncturing Mode should never be Built
            raise Program_Error;

      end case;

   end Building_Data;



   -------------------------
   -- Get_Puncturing_Mode --
   -------------------------
   function Get_Puncturing_Mode(Object : PUNCTURING_Attribute) return Puncturing_Mode is
   begin
      return Object.Mode;
   end Get_Puncturing_Mode;

   --------------------
   -- Get_Attributes --
   --------------------
  procedure Get_Attribute(Object :   in     PUNCTURING_Attribute;
                          Data:         out Puncturing_Data) is
   begin
      Data       := Object.Data;
   end Get_Attribute;


   -------------------
   -- Set_Attribute --
   -------------------
   procedure Set_Attribute(Object :    in out    PUNCTURING_Attribute;
                           Data:       in        Puncturing_Data) is
   begin
      Object.Data := Data;
   end;






begin

   PPETP.Attributes.Register(Index    => PUNCTURING_Attribute_Index,
                             Callback => Parsing_Data'Access);

end PPETP.Attributes.Puncturing;

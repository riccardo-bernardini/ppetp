with Network;		use Network;
with byte_arrays;	use byte_arrays;
with Ada.Streams;	use Ada.Streams;

package PPETP.Attributes.Puncturing is

   PUNCTURING_Attribute_Index : constant Attributes_Index_Type := 3;

   type Puncturing_Mode is (Probabilistic,
                            Deterministic,
                            Null_Puncturing);

   for Puncturing_Mode'Size use 8;
   for Puncturing_Mode use (Probabilistic   => 0,
                            Deterministic   => 1,
                            Null_Puncturing => 255);

   Unknown_Puncturing_Mode : exception;

   type Puncturing_Data(Mode: Puncturing_Mode) is
      record
         case Mode is
            when Probabilistic =>
               Num : Byte;
               Den : Byte;
            when Deterministic =>
               M    : Byte;
               Size : Byte;
               Val  : Byte_Array(1..256);
            when Null_Puncturing =>
               null;
         end case;
      end record;


   type PUNCTURING_Attribute (Mode: Puncturing_Mode) is
     new Root_Attributes with private;

   function Parsing_Data(Data: byte_Array) return Access_Attribute_Class;
   function Building_Data(Object: PUNCTURING_Attribute) return byte_array;


   function Get_Puncturing_Mode(Object : PUNCTURING_Attribute) return Puncturing_Mode;

   procedure Get_Attribute(Object :    in     PUNCTURING_Attribute;
                           Data:          out Puncturing_Data);

   procedure Set_Attribute(Object :    in out    PUNCTURING_Attribute;
                           Data:       in        Puncturing_Data);

private

   type PUNCTURING_Attribute (Mode: Puncturing_Mode) is new Root_Attributes with
      record
         Data      : Puncturing_Data(Mode);
      end record;

end PPETP.Attributes.Puncturing;

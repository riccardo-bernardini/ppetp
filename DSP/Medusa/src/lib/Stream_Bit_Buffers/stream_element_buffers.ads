--                              -*- Mode: Ada -*-
--  Filename        : bit_streams.ads
--  Description     : Bit Stream Package
--  Author          : Finta Tartaruga
--  Created On      : Mon Aug 20 22:53:40 2007
--  Last Modified By: R. Bernardini
--  Last Modified On: Mon Aug 20 22:53:40 2007
--  Update Count    : 0
--  Status          : Tested. OK.
with Text_Io;
use  Text_Io;

with Ada.Streams;
use  Ada.Streams;

package Stream_element_Buffers is
   type Bit_Stream is new Controlled with private;

   type Value_Length_Pair is
      record
         Value : Natural;
         Nbit  : Positive;
      end record;

   type Value_Length_Pair_Array is
     array (Positive range <>) of Value_Length_Pair;


   -- Allocate an initial buffer of Size bytes.  The buffer will
   -- be automatically enlarged when needed, unless Rigid=True
   procedure Set_Buffer (Stream : in out Bit_Stream;
                         Size   : in     Positive;
                         Rigid  : in     Boolean := False);

   -- Use as buffer the array whose access is Buffer. If
   -- this procedure is called, Rigid=True is implied.
   procedure Set_buffer (Stream : in out Bit_Stream;
                         Buffer : in     Stream_Element_Array);

   type Seek_Reference is (First_Pos, Current_Pos, Last_Pos);

   procedure Seek (Stream : in out Bit_Stream;
                   Offset : in     Integer;
                   Start  : in     Seek_Reference := First_Pos);

   -- Write the Nbit-bit Value to the output bit stream. Value must
   -- satisfy 0 <= Vale < 2**Nbit
   procedure Write (Stream : in out Bit_Stream;
                    Value  : in     Natural;
                    Nbit   : in     Positive := 1);

   procedure Write (Stream : in out Bit_Stream;
                    Data   : in     Value_Length_Pair_Array);

   procedure Write_Variable_Length_Field(Stream : in out Bit_Stream;
                                         Value  : in     Natural;
                                         Pre_Field_Size : Positive);

   procedure Unary_Encoding (Stream : in out Bit_Stream;
                             Value  : in     Natural);

   -- Get the portion of the buffer written so far
   function Get_Buffer (Stream : in Bit_Stream) return Byte_Array;

   function Buffer_Length (Stream : in Bit_Stream) return Natural;

   procedure Get_Buffer (Stream : in     Bit_Stream;
                         Buffer :    out Byte_Array);


   function Padding (Stream : in Bit_Stream) return Natural;

   -- Initialize the buffer of the Bit_Stream
   procedure Set_Buffer (Stream  : in out Bit_Stream;
                         Data    : in     Byte_Array);

   -- Read Nbit bit and return them as Value
   procedure Read (Stream : in out Bit_Stream;
                   Value  :    out Natural;
                   Nbit   : in     Positive := 1);

   procedure Read_Variable_Length_Field(Stream : in out Bit_Stream;
                                        Value  :    out Natural;
                                        Pre_Field_Size : Positive);

   procedure Unary_Decoding (Stream : in out Bit_Stream;
                             Value  :    out Natural);

   Buffer_Overflow : exception;  -- A Rigid buffer would need enlargment
   Double_Init     : exception;  -- Set_Buffer called twice
   Uninitialized   : exception;  -- Set_buffer never called
private
   subtype Padding_Type is Integer range 0..8;

   type Bit_Stream is new Controlled with record
      Rigid   : Boolean;
      Padding : Padding_Type;
      Pos     : Positive;
      Buffer  : access Stream_Element_Array := null;
   end record;
end Stream_Element_Buffers;

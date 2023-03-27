with Log2;

with Ada.Unchecked_Deallocation;
with Text_Io; use Text_Io;

package body Stream_Element_Buffers is
   package Bio is new Integer_Io(Integer);

   procedure Delete_Buffer is
      new Ada.Unchecked_Deallocation(Stream_Element_Array, Stream_Element_Array_Pt);

   -- This package implements the Bit_Stream class, useful to
   -- write/read packets of data.
   --
   -- A Bit_Stream initialized for writing stores its data
   -- in a buffer of unsigned 8-bit integers.  The wrote portion
   -- is the part Buffer(1..pos-1) with the Cursor less significative
   -- bits of buffer(pos).  Padding = 8-cursor

   ----------------
   -- Init_write --
   ----------------

   procedure Dump(Stream: in Bit_Stream)
   is
   begin
      for I in 1..Stream.Pos loop
         Put("B(" & Integer'Image(I) & ")=");
         Bio.Put(Item => Integer(Stream.Buffer(I)), Base=>16);
         Put_Line("");
      end loop;

      Put_Line ("Padding: " & Integer'Image(Stream.Padding));
   end Dump;

   procedure Set_Buffer (Stream : in out Bit_Stream;
                         Size   : in     Positive;
                         Rigid  : in     Boolean := False)
   is
   begin
      if (Stream.Buffer /= null) then
         raise Double_Init;
      end if;

      Stream := (Buffer  => new Stream_Element_Array(1..Size),
                 Padding => Stream_Element'Size,
                 Pos     => 1,
                 Rigid   => Rigid);

      Stream.Buffer.all := (others => 0);
   end Set_Buffer;

   procedure Set_buffer (Stream : in out Bit_Stream;
                         Buffer : in     Stream_Element_Array_Pt)
   is
   begin
      if (Stream.Buffer /= null) then
         raise Double_Init;
      end if;

      Stream := (Buffer  => Buffer,
                 Padding => Stream_Element'Size,
                 Pos     => 1,
                 Rigid   => True);

      Stream.Buffer.all := (others => 0);
   end Set_Buffer;


   procedure Enlarge_Up_To(Stream : in out Bit_Stream;
                           Pos    : in     Positive) is

   begin
      if (Pos <= Stream.Buffer'Last) then
         return;
      end if;

      -- If we are here, Pos is outside the current buffer:
      -- reallocate a larger one (whose size is 1.5 times the
      -- current buffer size) unless the buffer was declared
      -- "Rigid"
      if (Stream.Rigid) then
         raise Buffer_Overflow;
      end if;


      declare
         Old_Buffer : access Stream_Element_Array := Stream.Buffer;
         Old_Length : Positive := Stream.Buffer'Length;
         New_Length : Positive := (3*Stream.Buffer'Length)/2;
      begin
         while (New_Length < Pos) loop
            New_Length := (3*New_Length)/2;
         end loop;

         Stream.Buffer := new Stream_Element_Array(1..New_Length);
         Stream.Buffer(1..Old_Length) := Old_Buffer.all;
         Stream.Buffer(Old_Length+1..New_Length) := (others => 0);

         Delete_Buffer(Old_Buffer);
      end;
   end Enlarge_Up_To;

   -- Select the next writable entry in Stream, i.e., increase
   -- the buffer pointer by 1 and, if necessary, reallocates
   -- the buffer
   procedure Next_Writable_Entry (Stream : in out Bit_Stream)
   is
   begin
      Stream.Pos := Stream.Pos+1;

      Enlarge_Up_To (Stream, Stream.Pos);
   end Next_Writable_Entry;


   procedure Seek (Stream : in out Bit_Stream;
                   Offset : in     Integer;
                   Start  : in     Seek_Reference := First_Pos) is
   begin
      case Start is
         when First_Pos =>
            Stream.Pos := Offset;
         when Current =>
            Stream.Pos := Stream.Pos + Offset;
         when Last_Pos =>
            Stream.Pos := Stream.Buffer'Last + Offset;
      end case;

      Enlarge_Up_To (Stream, Stream.Pos);
   end Seek;

   -----------
   -- Write --
   -----------


   procedure Write (Stream : in out Bit_Stream;
                    Value  : in     Natural;
                    Nbit   : in     Positive := 1)
   is
   begin
      if (Stream.Buffer = null) then
         Set_Buffer(Stream, 16);
      end if;

      if (Value >= 2**Nbit) then
         raise Constraint_Error;
      end if;

      if (Nbit <= Stream.Padding) then
         -- This is the simplest case: Value will fit in the remaining
         -- part of stream.buffer(pos);

         -- The first Stream.Cursor of Stream.Buffer(pos) are used
         -- The first bit to be written is bit Stream.cursor

         Stream.Buffer(Stream.Pos) :=
           Stream.Buffer(Stream.Pos) or
           Shift_Left(Stream_Element(Value),
                      Stream_Element'Size-Stream.Padding);

         Stream.Padding := Stream.Padding - Nbit;
      else
         -- Here Value does not fit in stream.padding bits.
         declare
            Mini_Buffer : Stream_Element;
            Bits_To_Go  : Natural := Nbit;
            Value_To_Go : Natural := Value;
         begin
            -- First use whatever (if any) is left, i.e.,
            -- the size-stream.padding most significative bits
            -- of stream.buffer(stream.pos)
            if (Stream.Padding > 0) then
              Mini_Buffer := Stream_Element(Value_To_Go mod 2**Stream.Padding);

              Stream.Buffer(Stream.Pos) :=
                Stream.Buffer(Stream.Pos) or
                Shift_Left(Mini_Buffer, Stream_Element'Size-Stream.Padding);

              Value_To_Go := Value_To_Go/2**Stream.Padding;
              Bits_To_Go := Bits_To_Go-Stream.Padding;
            end if;

            -- Go to the next buffer entry
            Next_Writable_Entry(Stream);

            -- Now write value in buffer Stream_Element'Size bits at time
            while (Bits_To_Go >= Stream_Element'Size) loop
               Stream.Buffer(Stream.Pos) :=
                 Stream_Element(Value_To_Go mod 2**Stream_Element'Size)

               Value_To_Go := Value_To_Go/2**Stream_Element'Size;
               Bits_To_Go  := Bits_To_Go-Stream_Element'Size;

               -- Go to the next buffer entry
               Next_Writable_Entry(Stream);
            end loop;

            -- Now write the remaining (if any) bits of value
            -- in the current entry
            if (Bits_To_Go > 0) then
               Stream.Buffer(Stream.Pos) := Stream_Element(Value_To_Go);
            end if;

            Stream.Padding := Stream_Element'Size-Bits_To_Go;
         end;
      end if;
   end Write;

   procedure Write (Stream : in out Bit_Stream;
                    Data   : in     Value_Length_Pair_Array) is
   begin
      for I in Data'Range loop
         Write(Stream, Data(I).Value, Data(I).Nbit);
      end loop;
   end Write;

   procedure Write_Variable_Length_Field(Stream : in out Bit_Stream;
                                         Value  : in     Natural;
                                         Pre_Field_Size : Positive) is
      Field_Size : Positive := Log2(Value+1);
   begin
      Write(Stream => Stream,
            Value  => Field_Size,
            Nbit   => Pre_Field_Size);

      Write(Stream => Stream,
            Value  => Value,
            Nbit   => Field_Size);
   end Write_Variable_Length_Field;

   procedure Unary_Encoding (Stream : in out Bit_Stream;
                             Value  : in     Natural) is
   begin
      for I in 1..Value loop
         Write(Stream, 1);
      end loop;

      Write(Stream, 0);
   end Unary_Encoding;


   procedure Unary_Decoding (Stream : in out Bit_Stream;
                             Value  :    out Natural) is
      Buf : Natural;
   begin
      Value := 0;

  Decoding_Loop:
      loop
         Read (Stream => Stream,
               Value  => Buf,
               Nbit   => 1);

         exit Decoding_Loop when Buf=0;
         Value := Value + 1;
      end loop Decoding_Loop;
   end Unary_Decoding;




   ----------------
   -- Get_Buffer --
   ----------------

   function Get_Buffer (Stream : in Bit_Stream) return Stream_Element_Array is
   begin
      return Stream.Buffer(1..Stream.Pos);
   end Get_Buffer;

   procedure Get_Buffer (Stream : in     Bit_Stream;
                         Buffer :    out Stream_Element_Array) is
   begin
      Buffer := Stream.Buffer(1..Stream.Pos);
   end Get_Buffer;

   function Buffer_Length (Stream : in Bit_Stream) return Natural is
   begin
      return Stream.Pos;
   end Buffer_Length;

   -------------
   -- Padding --
   -------------

   function Padding (Stream : in Bit_Stream) return Natural is
   begin
      return Stream.Padding;
   end Padding;

   ---------------
   -- Init_Read --
   ---------------

   procedure Set_Buffer (Stream  : in out Bit_Stream;
                         Data    : in     Stream_Element_Array)
   is
   begin
      if (Stream.Buffer /= null) then
         raise Double_Init;
      end if;

      Stream := (Buffer  => new Stream_Element_Array(1..Data'Length),
                 Padding => Stream_Element'Size,
                 Pos     => 1);

      Stream.Buffer.all := Data;
   end Set_Buffer;

   ----------
   -- Read --
   ----------
   procedure Next_Readable_Entry (Stream : in out Bit_Stream)
   is
   begin
      Stream.Pos := Stream.Pos + 1;
      if (Stream.Pos > Stream.Buffer'Last) then
         raise Constraint_Error;
      end if;
   end Next_Readable_Entry;
   pragma Inline(Next_Readable_Entry);


   procedure Extract (Source : in out Stream_Element;
                      Result :    out Stream_Element;
                      Nbit   : in     Padding_type)
   is
   begin
      if (Nbit < Stream_Element'Size) then
         Result := Source mod 2**Nbit;
         Source := Source / 2**Nbit;
      else
         Result := Source;
         Source := 0;
      end if;
   end Extract;

   pragma Inline(Extract);


   procedure Read (Stream : in out Bit_Stream;
                   Value  :    out Natural;
                   Nbit   : in     Positive := 1)
   is
      Mini_Buf : Byte;
   begin
      if (Stream.Buffer = null) then
         raise Uninitialized;
      end if;

      if (Nbit <= Stream.Padding) then
         Extract(Source => Stream.Buffer(Stream.Pos),
                 Result => Mini_Buf,
                 Nbit   => Nbit);

         Value := Integer(Mini_Buf);
         Stream.Padding := Stream.Padding - Nbit;
         return;
      end if;

      declare
         Bits_To_Get : Natural := Nbit - Stream.Padding;
         K : Positive;
      begin
         Value := Integer(Stream.Buffer(Stream.Pos));
         K := 2**Stream.Padding;
         Next_Readable_Entry(Stream);

         while Bits_To_Get >= Stream_Element'Size loop
            Value := Value + K*Integer(Stream.Buffer(Stream.Pos));
            K := K*(2**Stream_Element'Size);
            Bits_To_Get := Bits_To_Get-Stream_Element'Size;

            Next_Readable_Entry(Stream);
         end loop;

         if (Bits_To_Get > 0) then
            Extract(Source => Stream.Buffer(Stream.Pos),
                    Result => Mini_Buf,
                    Nbit   => Bits_To_Get);

            Value := Value + K*Integer(Mini_Buf);
            Stream.Padding := Stream_Element'Size-Bits_To_Get;
         else
            Stream.Padding := Stream_Element'Size;
         end if;


      end;
   end Read;

   procedure Read_Variable_Length_Field(Stream : in out Bit_Stream;
                                        Value  :    out Natural;
                                        Pre_Field_Size : Positive) is
      Field_Size : Positive;
   begin
      Read(Stream => Stream,
           Nbit   => Pre_Field_Size,
           Value  => Field_Value);


      Read(Stream => Stream,
           Nbit   => Field_Size,
           Value  => Value);

   end Read_Variable_Length_Field;

end Stream_Element_Buffers;

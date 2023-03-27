with Profiles.Entangled.Vandermonde;     use Profiles.Entangled.Vandermonde;
with Profiles.Parameters.Vandermonde;    use Profiles.Parameters.Vandermonde;

with Byte_Arrays;       use Byte_Arrays;
with Galois_Matrices;   use Galois_Matrices;
with Galois_Field;	use Galois_Field;

with Interfaces;                         use Interfaces;
with Ada.Streams;			 use Ada.Streams;
with Ada.Unchecked_Conversion;


package body Profiles.Parsers.Vandermonde is
   -- ================ --
   -- ==== PARSER ==== --
   -- ================ --


   ----------------
   -- New_Parser --
   ----------------

   function New_Parser return Parser_Pt is
   begin
      return new Parser;
   end New_Parser;

   -----------------
   -- Set_Default --
   -----------------

   procedure Set_Default (Handler : in out Parser;
                          Default : in     Parameters_Class_Pt)
   is
   begin
      Handler.Parser_Param := Vandermonde_Par'(Vandermonde_Par(Default.all));
   end Set_Default;


   ----------------
   -- Parse_Data --
   ----------------

   procedure Parse_Data
     (Handler   : in     Parser;
      Flags     : in     Profile_Flags;
      Inline    : in     Boolean;
      Source    : in out Parsing_Buffer;
      Result    :    out Entangled_Payload_Pt)
   is
      Buffer : Byte_Array_Pt;

      -- GET in PARSE_DATA
      procedure Get is
        new Get_Remaining (Target => byte_array_Pt);

      -- CONVERT_BYTE_ARRAY_TO_MATRIX_VECTOR in PARSE_dATA
      function Convert_Byte_Array_To_Matrix_Vector (Input : Byte_Array_Pt) return Matrix
      is
         Temp : Ada.Streams.Stream_Element_Array (1 .. Input'Length);
         Count : Ada.Streams.Stream_Element_Offset := Input'First;

         function To_Stream_Offset is
                new Ada.Unchecked_Conversion (Source => Integer ,
                                              Target => Ada.Streams.Stream_Element_Offset);

      begin
         for I in 1 .. Input'Length
         loop
            Temp(To_Stream_Offset(I)) := Input(Count);
            Count := To_Stream_Offset(Integer(Count) + 1);
         end loop;

         declare
            Array_Of_Galois : Galois_Array := Stream_To_Galois_Array(Temp);
            Vector          : Matrix;
         begin
            Vector := Create(Array_Of_Galois'Length , 1);
            for J in 1 .. Array_Of_Galois'Length
            loop
               Set(Vector, J , 1, Array_Of_Galois(J));
            end loop;
            return Vector;
         end;
      end Convert_Byte_Array_To_Matrix_Vector;


      Pad    : Boolean;
      F : Profile_Flags := Flags;

   begin
   -- Extraction of Padding Flag --
      if (F mod 4)/2 = 1 then
         Pad := True;
      else
         Pad := False;
      end if;

      Get(Source, Buffer);
      Result := Entangled_Payload_Pt'
        (new Vandermonde_Ent'(Galois_Data   => Convert_Byte_Array_To_Matrix_Vector(Buffer),
                              Param => Handler.Parser_Param,
                              Padding => Pad));
      Free(Buffer);
   end Parse_Data;

   ----------------------
   -- Parse_Parameters --
   ----------------------

   procedure Parse_Parameters
     (Handler    : in     Parser;
      Source     : in out Parsing_Buffer;
      Parameters :    out Parameters_Class_Pt)
   is
      subtype Stream_Word is Byte_Array (1 .. 4);

      Byte_Word : Byte_Array(1 .. 4);


      ----------------------------
      --- CONVERSION FUNCTIONS ---

      function To_Stream_Offset is
        new Ada.Unchecked_Conversion (Source => Integer ,
                                      Target => Ada.Streams.Stream_Element_Offset);

      function Unsigned_8_To_Stream is
        new Ada.Unchecked_Conversion (Source => Unsigned_8,
                                      Target => Ada.Streams.Stream_Element);

       function Stream_To_Unsigned_8 is
        new Ada.Unchecked_Conversion (Source => Ada.Streams.Stream_Element,
                                      Target => Unsigned_8);

      function Stream_To_Integer (Src : Ada.Streams.Stream_Element) return Integer
      is
         Res : Integer := Integer(Stream_To_Unsigned_8(Src));
      begin
         return Res;
      end Stream_To_Integer;



      function Word_To_Galois (Word : Stream_Word) return Galois is
         A : Galois;
         B : Unsigned_64 := 0;
      begin

         B := Unsigned_64(Word(4))
              + Unsigned_64(Word(3)) * 256
              + Unsigned_64(Word(2)) * (256 ** 2)
              + Unsigned_64(Word(1)) * (256 ** 3);
          A := To_Galois(B);
         return A;
      end Word_To_Galois;

   begin
      pragma Assert(Remaining(Source) >= 8);

      Skip(Source, 2);
      Parameters := new Vandermonde_Par;

      Vandermonde_Par(Parameters.all).GF_Size :=
        (Integer(Stream_To_Unsigned_8(Get_Actual(Source)) or 16#03#) + 1) * 8;

      Skip(Source, 1);

      Vandermonde_Par(Parameters.all).Reduction_Factor :=
        Stream_To_Integer(Get_Actual(Source));

      for Current_Byte in  1 .. 4
      loop
         Skip(Source, 1);
         Byte_Word(To_Stream_Offset(Current_Byte)) := Get_Actual(Source);
      end loop;

      Vandermonde_Par(Parameters.all).Galois_Element := Word_To_Galois(Byte_Word);

   end Parse_Parameters;


   procedure Free (Object: in out Parser_Pt) is
      procedure Free_In is
        new Ada.Unchecked_Deallocation(Object => Parser,
                                       Name   => Parser_Pt);
   begin
      Free_In(Object);
   end Free;


end Profiles.Parsers.Vandermonde;

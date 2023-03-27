with Profiles.Entangled.Vandermonde;     use Profiles.Entangled.Vandermonde;
with Profiles.Parameters.Vandermonde;    use Profiles.Parameters.Vandermonde;

with Galois_Arrays;     use Galois_Arrays;
with Galois_Field;	use Galois_Field;


with Interfaces;        use Interfaces;
with Ada.Streams;
with Ada.Unchecked_Conversion;

with Ada.Text_IO;	use ada.Text_IO;

package body profiles.builders.vandermonde is
   -- ================= --
   -- ==== BUILDER ==== --
   -- ================= --

   -----------------
   -- New_Builder --
   -----------------

   function New_Builder return Builder_Pt is
   begin
      return new Builder;
   end New_Builder;

   -----------------
   -- Set_Default --
   -----------------


   procedure Set_Default (Handler : in out Builder;
                          Default : in     Parameters_Class_Pt)
   is
   begin
      Handler.Builder_Parameters := Vandermonde_Par'(Vandermonde_Par(Default.all));

   end Set_Default;


   -----------------
   -- Format_Data --
   -----------------

   procedure Format_Data
     (Handler : in     Builder;
      Source  : in     Entangled_Payload_Pt;
      Inline  :    out Boolean;
      Flags   :    out Profile_Flags;
      Result  :    out Byte_Array_Pt)
   is
      Reduced_Vector : Matrix := Vandermonde_Ent(Source.all).Galois_Data;


      ----------------------------
      --- CONVERSION FUNCTIONS ---

      function To_Stream_Offset is
        new Ada.Unchecked_Conversion (Source => Integer ,
                                   Target => Ada.Streams.Stream_Element_Offset);

      function To_Stream is
        new Ada.Unchecked_Conversion (Source => Unsigned_8,
                                      Target => Ada.Streams.Stream_Element);



      function Convert_Vector_To_Byte_Array (Input : Matrix) return Byte_Array_Pt
      is


         Temp :   Galois_Array (1 .. Size_Col(Input));
      begin
      -- Input must be a column matrix --
         pragma Assert(Size_Row(Input) = 1);

--           Put_Line("righe inpu : " & Size_Row(Inpu)'img);
--           Put_Line("colonne inpu: " & Size_Col(Inpu)'img);
--           Put_Line("Temp length: " & Temp'Length'img);

         for I in 1 .. Size_Col(Input)
         loop

            Temp(I) := Get(Input,1,I);
         end loop;

         declare
            Array_Of_Stream : Ada.Streams.Stream_Element_Array :=
                                                  Galois_Array_To_Stream (Temp);
            Output : Byte_Array_Pt := new Byte_Array
              (To_Stream_Offset(1) .. To_Stream_Offset(Array_Of_Stream'Length));
            Counter : Integer := 1;
         begin
            for J in  Array_Of_Stream'First .. Array_Of_Stream'Last
            loop
               Output(To_Stream_Offset(Counter)) := Array_Of_Stream((J));
               Counter := Counter + 1;
            end loop;
            return Output;
         end;
      end Convert_Vector_To_Byte_Array;


   begin
      pragma Assert (Source /= null);

      if Builder(Handler).Builder_Parameters =
        Vandermonde_Par(Vandermonde_Ent(Source.all).Param) then
         Inline := False;
      else
         Inline := True;
      end if;

      if Vandermonde_Ent(Source.all).Padding then
         Flags := 2;
      else
         Flags := 0;
      end if;

     -- Put_line("Righe: "   & Size_Row(Reduced_Vector)'img);
     -- Put_line("Colonne: " & Size_Col(Reduced_Vector)'img);

      Result := Convert_Vector_To_Byte_Array (Reduced_Vector);

   end Format_Data;

   -----------------------
   -- Format_Parameters --
   -----------------------

   procedure Format_Parameters
     (Handler    : in     Builder;
      Parameters : in     Parameters_Class_Pt;
      Result     :    out Byte_Array_Pt)
   is
       type Unsigned_8_Word is array(1 .. 4) of Unsigned_8;

       Reduction_Vector_Word : Unsigned_8_Word;


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

       function Galois_To_Word (Value : Galois) return Unsigned_8_Word is
         A : Unsigned_64 := To_Int(Value);
         B : Unsigned_8_word;
      begin
         B(1) := Unsigned_8(A / (256 ** 3)); A := A mod (256 ** 3);
         B(2) := Unsigned_8(A / (256 ** 2)); A := A mod (256 ** 2);
         B(3) := Unsigned_8(A / 256); A := A mod 256;
         B(4) := Unsigned_8(A);
         -- Note: this above is the correct way to order bytes in parameters
         -- byte packet

         return B;
       end Galois_To_Word;

   begin
      Result := new Byte_Array(1 .. 8);
      for I in 1 .. 8
      loop
        Result(To_Stream_Offset(I)) := 0;
      end loop;

      Result(3) := Unsigned_8_To_Stream
                      (Unsigned_8
                         (Vandermonde_Par(Parameters.all).GF_Size / 8 - 1));

      Result(4) := Unsigned_8_To_Stream
                      (Unsigned_8
                         (Vandermonde_Par(Parameters.all).Reduction_Factor));

      Reduction_Vector_Word := Galois_To_Word
                               (Vandermonde_Par(Parameters.all).Galois_Element);

      Result(5) := Unsigned_8_To_Stream(Reduction_Vector_Word(1));
      Result(6) := Unsigned_8_To_Stream(Reduction_Vector_Word(2));
      Result(7) := Unsigned_8_To_Stream(Reduction_Vector_Word(3));
      Result(8) := Unsigned_8_To_Stream(Reduction_Vector_Word(4));

   end Format_Parameters;

end profiles.builders.vandermonde;

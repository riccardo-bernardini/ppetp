with Profiles.Parameters.Vandermonde;   use Profiles.Parameters.Vandermonde;
with Profiles.Entangled.Vandermonde;    use Profiles.Entangled.Vandermonde;
with Profiles.Builders.Vandermonde;   use Profiles.Builders.Vandermonde;


with Packets;           use Packets;
with Byte_Arrays;       use Byte_Arrays;
with Galois_Matrices;   use Galois_Matrices;
with Galois_Field;	use Galois_Field;
with Galois_Arrays; 	use Galois_Arrays;
with Parsing_Buffers;   use Parsing_Buffers;

with Interfaces;        use Interfaces;
with Ada.Unchecked_Conversion;
with Ada.Streams;
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with Ada.Unchecked_Deallocation;

with Create;  use Create;
with View;    use View;
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;



procedure Profiles.Parsers.Vandermonde.Test is

   Object_Pt : Parser_Pt;
   Init_Pt : Parameters_Class_Pt := Create_Parameter(8,19);
   -- Param_Pt : Parameters_Class_Pt;
   Matrix_Of_Galois : Matrix:= Create_Matrix_Cell(8);
   Data_Pt : Entangled_Payload_Pt;
   Inline : Boolean := False;
   Flags : Profile_Flags := 0;
   Header : Byte_Array(1 .. 8) := (0, 0, 3, 15, 0, 0, 0, 27);
      Data : Byte_Array(1 .. 8) :=(others => 1);  -- non deve essere minore di 4!
   A : Byte_Array_Pt := Create_Byte_Array(8, 5);
   Param_Source : Parsing_Buffer := Make_Parsing_Buffer(Header);
   Data_Source : Parsing_Buffer := Make_Parsing_Buffer(Data);




begin

   Object_Pt := New_Parser;
   Set_Default(Object_Pt.all, Init_Pt);
   -- Parse_Parameters(Object_Pt.all, Param_Source, Param_Pt);
   -- View_Parameters(Param_Pt);
--     Put(Integer(A(1)));
--     Put(Integer(A'length));
--     View_Byte_Array(A);
   Parse_Data(Object_Pt.all , Flags, Inline, Data_Source, Data_Pt);
   View_Entangled(Data_Pt);




   New_Line; Put(" Processo terminato! "); New_Line;New_Line;
end Profiles.Parsers.Vandermonde.Test;

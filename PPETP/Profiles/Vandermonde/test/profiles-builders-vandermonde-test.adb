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



procedure Profiles.Builders.Vandermonde.Test is

   Object_Pt : Builder_Pt;
   Init_Pt : Parameters_Class_Pt := Create_Parameter(Reduction_Factor =>8,
                                                     Galois_Element => 19);
   Init_Pt2 : Parameters_Class_Pt := Create_Parameter(Reduction_Factor =>8,
                                                      Galois_Element => 19);
   Source : Matrix:= Create_Matrix_Cell(8);
   Data_Pt : Entangled_Payload_Pt := Create_Entangled(Source, Init_Pt2);
   Header : Byte_Array_Pt;
   Inline : Boolean;
   Flags : Profile_Flags;
   Formated_Data : Byte_Array_Pt;
begin

   Object_Pt := New_Builder;
   Set_Default(Object_Pt.all, Init_Pt);
   Format_Parameters(Object_Pt.all , Init_Pt , Header);
   -- View_Byte_Array(Header);
   Format_Data(Object_Pt.all , Data_Pt , Inline , Flags , Formated_Data);

   View_Byte_Array(Formated_Data);
   View_Boolean (Inline);
   View_Profile_Flags(Flags);






   New_Line; Put(" Processo terminato! "); New_Line;New_Line;
               end Profiles.Builders.Vandermonde.Test;




--     -- Liberare la memoria allocata: serve?
--      procedure Free_Parameters is
--       new Ada.Unchecked_Deallocation (Byte_Array, Byte_Array_Pt);
--
--           procedure Free is new Ada.Unchecked_Deallocation (
--           Object => Builder,
--           Name => Builder_Pt);
--
--           procedure Free is new Ada.Unchecked_Deallocation (
--           Object => Root_Parameters'Class,
--           Name => Parameters_Class_Pt);
--
--           procedure Free is new Ada.Unchecked_Deallocation (
--           Object => Root_Entangled_Payload'Class,
--           Name => Entangled_Payload_Pt);

with Profiles.Parameters.Vandermonde;   use Profiles.Parameters.Vandermonde;
with Profiles.Entangled.Vandermonde;    use Profiles.Entangled.Vandermonde;
with Profiles.Builders.Vandermonde;     use Profiles.Builders.Vandermonde;
with Profiles.Parsers.Vandermonde;      use Profiles.Parsers.Vandermonde;
with Profiles.Parameters;	        use Profiles.Parameters;
with Profiles.Parsers;		        use Profiles.Parsers;
with Profiles. Entangled;	        use Profiles.Entangled;
with Profiles.Entanglers;		use Profiles.Entanglers;
with Profiles;				use Profiles;


with Packets;           use Packets;
with Byte_Arrays;       use Byte_Arrays;
with Galois_Matrices;   use Galois_Matrices;
with Galois_Field;	use Galois_Field;
with Galois_Arrays; 	use Galois_Arrays;
with Parsing_Buffers;   use Parsing_Buffers;
with Generic_Matrix;

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



procedure CrossTest is

   -- PARSER --
   Parser_Obj: Parser_Pt;
   Param_Pt : Parameters_Class_Pt;
   Result : Entangled_Payload_Pt;

   -- BUILDER --
   Builder_Obj : Builder_Pt;
   Init_Pt : Parameters_Class_Pt := Create_Parameter(Reduction_Factor =>15,
                                                     Galois_Element => 253400000);

   -- PACKETS --
   Source : Matrix := Galois_Matrices.Create(13 , 1);
   Data_Pt : Entangled_Payload_Pt;
   Header : Byte_Array_Pt;
   Formated_Data : Byte_Array_Pt;

   -- BOOLEANS --
   Inline : Boolean;
   Flags : Profile_Flags;


begin
   Put("1");
   Galois_Matrices.Set(Source, 1 , 1 , To_Galois(Integer(1280)));
   Galois_Matrices.Set(Source, 2 , 1 , To_Galois(Integer(47890)));
   Galois_Matrices.Set(Source, 3 , 1 , To_Galois(Integer(3456)));
   Galois_Matrices.Set(Source, 4 , 1 , To_Galois(Integer(4)));
   Galois_Matrices.Set(Source, 5 , 1 , To_Galois(Integer(253647)));
   Galois_Matrices.Set(Source, 6 , 1 , To_Galois(Integer(4748)));
   Galois_Matrices.Set(Source, 7 , 1 , To_Galois(Integer(900)));
   Galois_Matrices.Set(Source, 8 , 1 , To_Galois(Integer(47890)));
   Galois_Matrices.Set(Source, 9 , 1 , To_Galois(Integer(3456)));
   Galois_Matrices.Set(Source, 10 , 1 , To_Galois(Integer(4)));
   Galois_Matrices.Set(Source, 11 , 1 , To_Galois(Integer(253647)));
   Galois_Matrices.Set(Source, 12 , 1 , To_Galois(Integer(4748)));
   Galois_Matrices.Set(Source, 13 , 1 , To_Galois(Integer(900)));
Put("2");
   Data_Pt := Create_Entangled(Source, Init_Pt);


   ---------------------
   -- BUILD --> PARSE --

   Parser_Obj := New_Parser;
   Builder_Obj := New_Builder;

   Set_Default(Parser_Obj.all, Init_Pt);
   Set_Default(Builder_Obj.all, Init_Pt);


   New_Line;Put_Line("RESOCONTO :");
   Put_Line("Parametri in ingresso al BUILDER : ");
   View_Parameters(Init_Pt);


   -- Parameters --
   New_Line;Put_Line("Parametri letti dal PARSER: ");
   Format_Parameters(Builder_Obj.all , Init_Pt , Header);
   View_Byte_Array(Header);
   declare
      Param_Source : Parsing_Buffer := Make_Parsing_Buffer(Header.all);
   begin
      Parse_Parameters(Parser_Obj.all, Param_Source, Param_Pt);
      View_Parameters(Param_Pt);
   end;



   -- Data --
   New_Line;Put_Line("Entangled_Data in ingresso al BUILDER: ");
   Format_Data(Builder_Obj.all , Data_Pt , Inline , Flags , Formated_Data);
   View_Entangled(Data_Pt);
   New_Line;Put_Line("Byte_Array in uscita da  Builder: ");
   View_Byte_Array(Formated_Data);
   New_Line;Put_Line("Entanlged_Data ricostruito dal PARSER: ");
   declare
      Data_Source : Parsing_Buffer := Make_Parsing_Buffer(Formated_Data.all);
   begin
      Parse_Data(Parser_Obj.all , Flags, Inline, Data_Source, Result);
      View_Entangled(Result);
   end;


end CrossTest;

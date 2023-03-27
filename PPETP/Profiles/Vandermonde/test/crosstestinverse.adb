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



procedure CrossTestinverse is

   -- PARSER --
   Parser_Obj: Parser_Pt;
   Param_Pt : Parameters_Class_Pt;
   -- Result : Entangled_Payload_Pt;

   -- BUILDER --
   Builder_Obj : Builder_Pt;
   Init_Pt : Parameters_Class_Pt := Create_Parameter(Reduction_Factor =>13,
                                                     Galois_Element => 5);

   -- PACKETS --
   Source : Matrix := Galois_Matrices.Create(7 , 1);
   Data_Pt : Entangled_Payload_Pt;
   -- Header : Byte_Array(1 .. 8) := (0, 0, 3, 15, 15, 26, 147, 192);
   Header : Byte_Array_Pt := new Byte_Array' (0, 0, 3, 15, 15, 26, 147, 192);
   Formated_Data : Byte_Array_Pt;
   Res : Byte_Array_Pt;
   Data_Input : Byte_Array(1 .. 12) := (0, 0, 0, 0,
                                        255, 255, 255 ,255,
                                        1, 1, 1, 1);


   -- BOOLEANS --
   Inline : Boolean := false;
   Flags : Profile_Flags := 0;

   Data_Pt2 : Entangled_Payload_Pt;


begin
   Galois_Matrices.Set(Source, 1 , 1 , To_Galois(Integer(1280)));
   Galois_Matrices.Set(Source, 2 , 1 , To_Galois(Integer(47890)));
   Galois_Matrices.Set(Source, 3 , 1 , To_Galois(Integer(3456)));
   Galois_Matrices.Set(Source, 4 , 1 , To_Galois(Integer(4)));
      Galois_Matrices.Set(Source, 5 , 1 , To_Galois(Integer(253647)));
   Galois_Matrices.Set(Source, 6 , 1 , To_Galois(Integer(4748)));
      Galois_Matrices.Set(Source, 7 , 1 , To_Galois(Integer(900)));

   Data_Pt := Create_Entangled(Source, Init_Pt);
   Data_Pt2 := Create_Entangled(Source, Init_Pt);

   if Data_Pt.all = Data_Pt2.all then
      Put_Line("Uguali!");
   else
      Put_Line("Diversi!");
   end if;


   ---------------------
   -- PARSE--> BUILD --

   Parser_Obj := New_Parser;
   Builder_Obj := New_Builder;

   Set_Default(Parser_Obj.all, Init_Pt);
   Set_Default(Builder_Obj.all, Init_Pt);


   New_Line;Put_Line("RESOCONTO :");
   Put_Line("Contenuto del Parsing_Buffer in ingresso al PARSER : ");
   View_Byte_Array(Header);


   -- Parameters --
   declare
      Param_Source : Parsing_Buffer := Make_Parsing_Buffer(Header.all);
      -- Header1 : Byte_Array := Byte_Array(Header.all);
   begin
      ---New_Line;
      --Put(Integer(Header'Last)); New_Line;
      --Put(Integer(Last(Param_source))); New_Line;
      Parse_Parameters(Parser_Obj.all, Param_Source, Param_Pt);
      --View_Parameters(Param_Pt);
      Format_Parameters (Builder_Obj.all, Param_Pt, Res);
   end;
   --View_Byte_Array(Res);


   -- Data --
   New_Line;Put_Line("Byte_Array in ingresso al PARSER: ");
   -- View_Byte_Array(Data_Input);
--     declare
--        Data_Source : Parsing_Buffer := Make_Parsing_Buffer(Data_Input);
--     begin
     -- Parse_Data(Parser_Obj.all , Flags, Inline, Data_Source, Result);
      -- View_Entangled(Result);
      Format_Data(Builder_Obj.all , Data_Pt , Inline , Flags , Formated_Data);
   declare
      Data_Source : Parsing_Buffer := Make_Parsing_Buffer(Formated_Data.all);
      begin
      Parse_Data(Parser_Obj.all , Flags, Inline, Data_Source, Data_Pt2);
   if Data_Pt.all = Data_Pt2.all then
      Put_Line("Uguali!");
   else
      Put_Line("Diversi!");
   end if;
end;


   --View_Byte_Array(Formated_Data);



end CrossTestinverse;



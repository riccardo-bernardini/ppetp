-- This is a complete test for package Profiles.Builders.Vandermonde.
-- Running this procedure a series of 10 tests will rised, for improveing
-- the correctness of object Builder, and related functions.
-- Tests are comprhensive of cross test, the method of build some
-- parameters/data, parse them and re-build another time.
-- Test is positive if the final result match the attended result.

with Profiles.Parameters.Vandermonde;   use Profiles.Parameters.Vandermonde;
with Profiles.Parsers.Vandermonde;      use Profiles.Parsers.Vandermonde;
with Profiles.Entangled.Vandermonde;    use Profiles.Entangled.Vandermonde;
with Profiles.Builders.Vandermonde;     use Profiles.Builders.Vandermonde;
with Profiles.Parameters;               use Profiles.Parameters;
with Profiles.Entangled;                use Profiles.Entangled;
with Profiles;                          use Profiles;


with Byte_Arrays;       use Byte_Arrays;
with Galois_Matrices;   use Galois_Matrices;
with Galois_Field;	use Galois_Field;
with Parsing_Buffers;   use Parsing_Buffers;
with Test_Report;       use Test_Report;



procedure Test_Builder_Vandermonde is


   -- "CREATE" FUNCTIONS --

   function Create_Parameter (Reduction_Factor : Natural;
                              Galois_Element : Integer;
                              GF_Size          : Natural := 32)
                              return Parameters_Class_Pt is
      P : Parameters_Class_Pt := new Vandermonde_Par;
   begin
      Vandermonde_Par(P.all).Reduction_Factor := Reduction_Factor;
      Vandermonde_Par(P.all).Galois_Element :=  To_Galois(Galois_Element);
      Vandermonde_Par(P.all).GF_Size := GF_Size;
      -- Root_Profile_Handler(P.all).My_Profile := Vandermonde_Profile;

      return P;
   end Create_Parameter;



   function Create_Entangled (Galois_Data : Matrix;
                              Param       : Parameters_Class_Pt;
                              Padding      : Boolean := False)
                              return Entangled_Payload_Pt is
      E : Entangled_Payload_Pt := new Vandermonde_Ent;
   begin

      Vandermonde_Ent(E.all).Galois_Data := Galois_Data;
      Vandermonde_Ent(E.all).Param := Param;
      Vandermonde_Ent(E.all).Padding := Padding;

      return E;

   end Create_Entangled;




   Reporter :Reporter_Type;

   Object_Pt1 : Builder_Pt := New_Builder;
   Object_Pt2 : Builder_Pt := New_Builder;

   Init_Pt1 : Parameters_Class_Pt := Create_Parameter(Reduction_Factor =>8,
                                                       Galois_Element => 19);
   Init_Pt2 : Parameters_Class_Pt := Create_Parameter(Reduction_Factor =>15,
                                                       Galois_Element => 253400000);

   Source1 : Matrix := Galois_Matrices.Create(4 , 1);
   Source2 : Matrix := Galois_Matrices.Create(8 , 1);
   Data_Pt1 : Entangled_Payload_Pt;
   Data_Pt2 : Entangled_Payload_Pt;
   Data_Pt3 : Entangled_Payload_Pt;

   Header1 : Byte_Array_Pt := new Byte_Array'(0, 0, 3, 8, 0, 0, 0, 19);
   Header2 : Byte_Array_Pt := new Byte_Array'(0, 0, 3, 15, 15, 26, 147, 192);

   Formated_Data1 : Byte_Array_Pt := new Byte_Array'(1, 0, 0, 0,
                                                     2, 0, 0, 0,
                                                     3, 0, 0, 0,
                                                     4, 0, 0, 0);

   Formated_Data2 : Byte_Array_Pt := new Byte_Array'(0,      5,      0,      0,
                                                    18,    187,      0,      0,
                                                   128,     13,      0,      0,
                                                     4,      0,      0,      0,
                                                   207,    222,      3,      0,
                                                   140,     18,      0,      0,
                                                   132,      3,      0,      0,
                                                    18,    187,      0,      0);


   -- TEST BUILDER PARAMETERS --
   type Builder_Param_Case is
      record
         Hand : Builder;
         Def : Parameters_Class_Pt;
         Res  : Byte_Array_Pt;
      end record;

   A : Builder_Param_Case := (Hand => Object_Pt1.all, Def => Init_Pt1, Res => Header1);
   B : Builder_Param_Case := (Hand => Object_Pt2.all, Def => Init_Pt2, Res => Header2);


   type Builder_Param_Case_Array is array (Positive  range <>) of Builder_Param_Case;

   Cases_Param : Builder_Param_Case_Array := (A, B);



   function Builder_Param_Test (X : Builder_Param_Case) return Boolean is
      Result : Byte_Array_Pt;
   begin
      Format_Parameters(X.Hand , X.Def , Result);
      return
        Result.all = X.Res.all;
   end Builder_Param_Test;




   function Builder_Param_Test_Cross (X : Builder_Param_Case) return Boolean is
      Temp : Byte_Array_Pt;
      Result : Byte_Array_Pt;
      Temp2 : Parameters_Class_Pt;

   begin
      Format_Parameters(X.Hand , X.Def , Temp);
   declare
         Param_Source : Parsing_Buffer := Make_Parsing_Buffer(Temp.all);
         Parser_Obj : Parser_Pt := New_Parser;
      begin
         Set_Default(Parser_Obj.all, X.Def);
         Parse_Parameters(Parser_Obj.all, Param_Source, Temp2);
         Format_Parameters(X.Hand , Temp2 , Result);
         return
           Result.all = X.Res.all;
      end;
   end Builder_Param_Test_Cross;



   procedure Test_Param is
     new Do_Suite(Test_Case => Builder_Param_Case,
                  Test_Case_Array => Builder_Param_Case_Array,
                  Check => Builder_Param_Test);

      procedure Test_Param_Cross is
     new Do_Suite(Test_Case => Builder_Param_Case,
                  Test_Case_Array => Builder_Param_Case_Array,
                  Check => Builder_Param_Test_Cross);


   -- TEST BUILDER DATA --
   type Builder_Data_Case is
      record
         Hand : Builder;
         Sou  : Entangled_Payload_Pt;
         Inl  : Boolean;
         Fla  : Profile_Flags;
         Res  : Byte_Array_Pt;
         Par : Parameters_Class_Pt;
      end record;




begin

   Set_Default(Object_Pt1.all, Init_Pt1);
   Set_Default(Object_Pt2.all, Init_Pt2);

   -- TEST BUILDER PARAMETERS --
   Test_Param(Reporter , Cases_Param);
   Test_Param_Cross(Reporter , Cases_Param);


   -- TEST BUILDER DATA --
   Galois_Matrices.Set(Source1, 1 , 1 , To_Galois(Integer(1)));
   Galois_Matrices.Set(Source1, 2 , 1 , To_Galois(Integer(2)));
   Galois_Matrices.Set(Source1, 3 , 1 , To_Galois(Integer(3)));
   Galois_Matrices.Set(Source1, 4 , 1 , To_Galois(Integer(4)));

   Galois_Matrices.Set(Source2, 1 , 1 , To_Galois(Integer(1280)));
   Galois_Matrices.Set(Source2, 2 , 1 , To_Galois(Integer(47890)));
   Galois_Matrices.Set(Source2, 3 , 1 , To_Galois(Integer(3456)));
   Galois_Matrices.Set(Source2, 4 , 1 , To_Galois(Integer(4)));
   Galois_Matrices.Set(Source2, 5 , 1 , To_Galois(Integer(253647)));
   Galois_Matrices.Set(Source2, 6 , 1 , To_Galois(Integer(4748)));
   Galois_Matrices.Set(Source2, 7 , 1 , To_Galois(Integer(900)));
   Galois_Matrices.Set(Source2, 8 , 1 , To_Galois(Integer(47890)));


   Data_Pt1 := Create_Entangled(Source1, Init_Pt1);
   Data_Pt2 := Create_Entangled(Source2, Init_Pt2);
   Data_Pt3 := Create_Entangled(Source2, Init_Pt2, True);

   declare
   C : Builder_Data_Case := (Hand => Object_Pt1.all, Sou => Data_Pt1, Inl => False, Fla => 0, Res => Formated_Data1, Par => Init_Pt1);
   D : Builder_Data_Case := (Hand => Object_Pt2.all, Sou => Data_Pt1, Inl => True,  Fla => 0, Res => Formated_Data1, Par => Init_Pt1);
   E : Builder_Data_Case := (Hand => Object_Pt2.all, Sou => Data_Pt3, Inl => False, Fla => 2, Res => Formated_Data2, Par => Init_Pt2);


   type Builder_Data_Case_Array is array (Positive  range <>) of Builder_Data_Case;

   Cases_Data : Builder_Data_Case_Array := (C, D, E);




   function Builder_Data_Test (X : Builder_Data_Case) return Boolean is
      Result : Byte_Array_Pt;
      I      : Boolean;
      F      : Profile_Flags;
   begin
      Format_Data(X.Hand , X.Sou, I, F, Result);
      return
           Result.all = X.Res.all and then I = X.Inl and then F = X.Fla;
      end Builder_Data_Test;




   function Builder_Data_Test_Cross (X : Builder_Data_Case) return Boolean is
         Temp : Byte_Array_Pt;
         Result : Byte_Array_Pt;
         Temp2 : Entangled_Payload_Pt;
      I      : Boolean;
      F      : Profile_Flags;
   begin
         Format_Data(X.Hand , X.Sou, I, F, Temp);

   declare
            Data_Source : Parsing_Buffer := Make_Parsing_Buffer(Temp.all);
      Parser_Obj : Parser_Pt := New_Parser;
         begin
            Set_Default(Parser_Obj.all, X.Par);
            Parse_Data(Parser_Obj.all , F, I, Data_Source, Temp2);
            Format_Data(X.Hand , Temp2, I, F, Result);
      return
              Result.all = X.Res.all and then I = X.Inl and then F = X.Fla;
            end;
   end Builder_Data_Test_Cross;



   procedure Test_Data is
     new Do_Suite(Test_Case => Builder_Data_Case,
                  Test_Case_Array => Builder_Data_Case_Array,
                  Check => Builder_Data_Test);

   procedure Test_Data_Cross is
     new Do_Suite(Test_Case => Builder_Data_Case,
                  Test_Case_Array => Builder_Data_Case_Array,
                  Check => Builder_Data_Test_Cross);

   begin
      Test_Data(Reporter , Cases_Data);
      Test_Data_Cross(Reporter , Cases_Data);
   end;


   Final(Reporter);

end Test_Builder_Vandermonde;


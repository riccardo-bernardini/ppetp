-- THIS IS A TEST PROCEDURE FOR PACKAGE PROFILES.PARSERS.VANDERMONDE
-- There is a direct test for improve the input-output behaviour of
-- all functions in package Profiles.Parsers.Vandermonde and
-- there is a "cross test" for checking consistance of behaviour
-- with the corrispondent functions of package Profiles .Builders.Vandermonde.

with Profiles.Parameters.Vandermonde;   use Profiles.Parameters.Vandermonde;
with Profiles.Entangled.Vandermonde;    use Profiles.Entangled.Vandermonde;
with Profiles.Builders.Vandermonde;     use Profiles.Builders.Vandermonde;
with Profiles.Parsers.Vandermonde;      use Profiles.Parsers.Vandermonde;
with Profiles.Parameters;               use Profiles.Parameters;
with Profiles.Entangled;                use Profiles.Entangled;
with Profiles;                          use Profiles;

with Byte_Arrays;       use Byte_Arrays;
with Galois_Matrices;   use Galois_Matrices;
with Galois_Field;	use Galois_Field;
with Parsing_Buffers;   use Parsing_Buffers;

with Interfaces;        use Interfaces;
with Test_Report;       use Test_Report;



procedure Test_Parsers_Vandermonde is


   Reporter : Reporter_Type;

   Object_Pt1 : Parser_Pt := New_Parser;
   Object_Pt2 : Parser_Pt := New_Parser;

   ------------------------
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
   ------------------------



   Init_Pt1 : Parameters_Class_Pt := Create_Parameter(Reduction_Factor =>8,
                                                       Galois_Element => 19);
   Init_Pt2 : Parameters_Class_Pt := Create_Parameter(Reduction_Factor =>15,
                                                      Galois_Element => 253400000);

   Header1 : Byte_Array_Pt := new Byte_Array'(0, 0, 3, 8, 0, 0, 0, 19);
   Header2 : Byte_Array_Pt := new Byte_Array'(0, 0, 3, 15, 15, 26, 147, 192);





   -- TEST PARSER PARAMETERS --
   type Parser_Param_Case is
      record
         Hand : Parser;
         Sou :  Byte_Array_Pt;
         Res  : Parameters_Class_Pt;
      end record;

   A : Parser_Param_Case := (Hand => Object_Pt1.all, Sou => Header1, Res => Init_Pt1);
   B : Parser_Param_Case := (Hand => Object_Pt2.all, Sou => Header2, Res => Init_Pt2);


   type Parser_Param_Case_Array is array (Positive  range <>) of Parser_Param_Case;

   Cases_Param : Parser_Param_Case_Array := (A, B);



   function Parser_Param_Test (X : Parser_Param_Case) return Boolean is
      Result : Parameters_Class_Pt;
      Param_Source : Parsing_Buffer := Make_Parsing_Buffer(X.Sou.all);
   begin
      Parse_Parameters(X.Hand , Param_Source , Result);
      return
        Result.all = X.Res.all;
   end Parser_Param_Test;




   function Parser_Param_Test_Cross (X : Parser_Param_Case) return Boolean is
      Temp : Parameters_Class_Pt;
      Result : Parameters_Class_Pt;
      Temp2  : Byte_Array_Pt;
      Builder_Obj : Builder_Pt := New_Builder;
      Param_Source : Parsing_Buffer := Make_Parsing_Buffer(X.Sou.all);

   begin
      Parse_Parameters(X.Hand , Param_Source , Temp);
      Set_Default(Builder_Obj.all, X.Res);
      Format_Parameters(Builder_Obj.all, X.Res, Temp2);
   declare
         Param_Source2 : Parsing_Buffer := Make_Parsing_Buffer(Temp2.all);
      begin
         Parse_Parameters(X.Hand , Param_Source2 , Result);
         return
           Result.all = X.Res.all;
      end;
   end Parser_Param_Test_Cross;



   procedure Test_Param is
     new Do_Suite(Test_Case => Parser_Param_Case,
                  Test_Case_Array => Parser_Param_Case_Array,
                  Check => Parser_Param_Test);

      procedure Test_Param_Cross is
     new Do_Suite(Test_Case => Parser_Param_Case,
                  Test_Case_Array => Parser_Param_Case_Array,
                  Check => Parser_Param_Test_Cross);



   -- TEST PARSER DATA --

      Data1 : Byte_Array_Pt := new Byte_Array '(1, 2, 3, 4, 5, 6, 7, 8,
                                            1, 2, 3, 4, 5, 6, 7, 8,
                                            1, 2, 3, 4, 5, 6, 7, 8,
                                            1, 2, 3, 4, 5, 6, 7, 8);

      Data2 : Byte_Array_Pt := new Byte_Array '(    0,   0,   0,   0,
                                                      255, 255, 255, 255,
                                                        1,   1,   1,   1);

   type Parser_Data_Case is
      record
         Hand : Parser;
         Sou  : Byte_Array_Pt;
         Inl  : Boolean;
         Fla  : Profile_Flags;
         Res  : Entangled_Payload_Pt;
         Par : Parameters_Class_Pt;
      end record;

   Source1 : Matrix := Galois_Matrices.Create(8 , 1);
   Source2 : Matrix := Galois_Matrices.Create(3 , 1);

   Result1 : Entangled_Payload_Pt;
   Result2 : Entangled_Payload_Pt;




begin

   Set_Default(Object_Pt1.all, Init_Pt1);
   Set_Default(Object_Pt2.all, Init_Pt2);

   -- TEST PARSERS PARAMETERS --
   Test_Param(Reporter , Cases_Param);
   Test_Param_Cross(Reporter , Cases_Param);


   -- TEST DATA --

   Galois_Matrices.Set(Source1, 1 , 1 , To_Galois(Integer(67305985)));
   Galois_Matrices.Set(Source1, 2 , 1 , To_Galois(Integer(134678021)));
   Galois_Matrices.Set(Source1, 3 , 1 , To_Galois(Integer(67305985)));
   Galois_Matrices.Set(Source1, 4 , 1 , To_Galois(Integer(134678021)));
   Galois_Matrices.Set(Source1, 5 , 1 , To_Galois(Integer(67305985)));
   Galois_Matrices.Set(Source1, 6 , 1 , To_Galois(Integer(134678021)));
   Galois_Matrices.Set(Source1, 7 , 1 , To_Galois(Integer(67305985)));
   Galois_Matrices.Set(Source1, 8 , 1 , To_Galois(Integer(134678021)));

   Result1 := Create_Entangled(Source1, Init_Pt1);


   Galois_Matrices.Set(Source2, 1 , 1 , To_Galois(Unsigned_64(0)));
   Galois_Matrices.Set(Source2, 2 , 1 , To_Galois(Unsigned_64(4294967295)));
   Galois_Matrices.Set(Source2, 3 , 1 , To_Galois(Unsigned_64(16843009)));

   Result2 := Create_Entangled(Source2, Init_Pt2);


   declare
      C : Parser_Data_Case := (Hand => Object_Pt1.all,
                                Sou => Data1,
                                Inl => False,
                                Fla => 0,
                                Res => Result1,
                                Par => Init_Pt1);


      D : Parser_Data_Case := (Hand => Object_Pt2.all,
                                Sou => Data2,
                                Inl => False,
                                Fla => 0,
                                Res => Result2,
                                Par => Init_Pt2);

      E : Parser_Data_Case := (Hand => Object_Pt2.all,
                                Sou => Data2,
                                Inl => True,
                                Fla => 0,
                                Res => Result2,
                                Par => Init_Pt1);



   type Parser_Data_Case_Array is array (Positive  range <>) of Parser_Data_Case;

   Cases_Data : Parser_Data_Case_Array := (C, D, E);



   function Parser_Data_Test (X : Parser_Data_Case) return Boolean is
      Result : Entangled_Payload_Pt;
      Data_Source : Parsing_Buffer := Make_Parsing_Buffer(X.Sou.all);
   begin
      Parse_Data(X.Hand , X.Fla, X.Inl, Data_Source , Result);
      return
        Result.all = X.Res.all;
   end Parser_Data_Test;




   function Parser_Data_Test_Cross (X : Parser_Data_Case) return Boolean is
         Temp : Entangled_Payload_Pt;
         Result : Entangled_Payload_Pt;
         Temp2  : Byte_Array_Pt;
         Builder_Obj : Builder_Pt := New_Builder;
         Data_Source : Parsing_Buffer := Make_Parsing_Buffer(X.Sou.all);
         I           : Boolean;
         F           : Profile_Flags;
   begin
      Parse_Data(X.Hand , X.Fla, X.Inl,  Data_Source , Temp);
      Set_Default(Builder_Obj.all, X.Par);
      Format_Data(Builder_Obj.all, Temp, I, F,  Temp2);
   declare
         Data_Source2 : Parsing_Buffer := Make_Parsing_Buffer(Temp2.all);
      begin
            Parse_Data(X.Hand ,F, I, Data_Source2 , Result);
         return
           Result.all = X.Res.all and then X.Fla = F and then X.Inl = I;
      end;
   end Parser_Data_Test_Cross;


   procedure Test_Data is
     new Do_Suite(Test_Case => Parser_Data_Case,
                  Test_Case_Array => Parser_Data_Case_Array,
                  Check => Parser_Data_Test);

      procedure Test_Data_Cross is
     new Do_Suite(Test_Case => Parser_Data_Case,
                  Test_Case_Array => Parser_Data_Case_Array,
                  Check => Parser_Data_Test_Cross);
   begin
      Test_Data(Reporter , Cases_Data);
      Test_Data_Cross(Reporter , Cases_Data);
   end;

   Final(Reporter);

end Test_Parsers_Vandermonde;

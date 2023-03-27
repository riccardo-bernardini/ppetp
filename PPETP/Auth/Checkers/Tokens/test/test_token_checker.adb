with Auth.Profiles.Tokens;   use Auth.Profiles.Tokens;
with Test_Report;            use Test_Report;
with Ada.Streams;            use Ada.Streams;

procedure Test_Token_Checker is
   Reporter : Reporter_Type;

   Token_A : Token_Type := ( 12,  33,  45,  71,  24, 128,  99,  42);
   Token_B : Token_Type := ( 33,  90, 121, 115, 201,  90,  23,  16);
   Token_C : Token_Type := (111, 223, 109,   7, 124, 255,   0,   8);
   Good_Tokens : Token_List :=
     (1 => Token_A,
      2 => Token_B,
      3 => Token_A,
      4 => Token_C);

   type Single_Case is
      record
         Token  : Token_Type;
         Result : Boolean;
      end record;

   type Case_Array is array (Positive range <>) of Single_Case;

   Token_X : Token_Type := ( 33,  88, 121, 121, 201,  90,  23,  16);
   Token_Y : Token_Type := ( 21, 111, 111, 106,   3,   1,   0, 255);

   Test_Cases : Case_Array :=
     ((Token  => Token_C, Result => True),   -- Valid token
      (Token  => Token_B, Result => True),   -- Valid token
      (Token  => Token_X, Result => False),  -- Unknown token
      (Token  => Token_B, Result => False),  -- Valid token used twice
      (Token  => Token_A, Result => True),   -- Valid token
      (Token  => Token_Y, Result => False),  -- Unknown token
      (Token  => Token_A, Result => True),   -- Used twice, but declared twice
      (Token  => Token_A, Result => False)); -- Used too many times


   Checker  : Token_Checker_Pt := New_Checker(Good_Tokens);

   function Single_Test (X : Single_Case) return Boolean is
      Result : Boolean;
   begin
      Checker.Check(Stream_Element_Array(X.Token), Result);
      return Result = X.Result;
   end Single_Test;

   procedure Many_Tests is
      new Do_Suite (Test_Case       => Single_Case,
                    Test_Case_Array => Case_Array,
                    Check           => Single_Test);
begin
   Reporter.Be_Verbose;

   Many_Tests(Reporter, Test_Cases);
   Reporter.Final;
end Test_Token_Checker;

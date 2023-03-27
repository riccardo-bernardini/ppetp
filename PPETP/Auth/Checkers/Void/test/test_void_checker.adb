with Auth.Profiles.Void;   use Auth.Profiles;
with Test_Report;          use Test_Report;
with Ada.Streams;          use Ada.Streams;

procedure Test_Void_Checker is
   Reporter : Reporter_Type;
   Checker  : Void.Void_Checker_Pt := Void.New_Checker;
   Data     : Stream_Element_Array(1..1) := (1 => 1);
begin
   -- Well, this is quite trivial...
   Reporter.New_Result(Checker.Check(Data));
   Reporter.Final;
end Test_Void_Checker;

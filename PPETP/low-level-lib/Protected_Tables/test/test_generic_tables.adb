with Ada.Text_Io;
use  Ada.Text_Io;

with Test_Status;
use  Test_Status;

with Generic_Tables;
with Generic_Tables.Two_Params;

procedure Test_Generic_Tables is
   package TBL is
      new Generic_Tables (Element => Integer,
                          Cursor  => Positive);

   package TBL_2_Para is
      new TBL.Two_Params (Second_Type => Float);

   Accum : Integer := 0;

   procedure Accumulate(X : Integer) is
   begin
      Accum := Accum + X;
   end Accumulate;

   function Duplicate(X : Integer) return Integer is
   begin
      return 2*X;
   end Duplicate;

   procedure Compute_Sum (T      : in out Tbl.Table;
                          Result :    out Integer) is

   begin
      Accum := 0;
      T.Iterate(Accumulate'Access);
      Result := Accum;
   end Compute_Sum;

   procedure Accumulate_Bis (X : Integer; Y : Float) is
   begin
      Accum := Accum + Integer(Float(X)*Y);
   end Accumulate_Bis;

   T : TBL.Table;
   Status : Status_Type;
   Exception_Raised : Boolean;
   Index, Index2 : Positive;
   Tot, Total, Total_Bis : Integer;
begin
   Status.New_Test;
   Status.Fail_If (not T.Is_Full, "Table not full");

   Status.New_Test;
   T.Resize(10);
   Status.Fail_If(T.Is_Full, "Table full");

   Status.New_Test;
   Exception_Raised := False;
   begin
      T.Delete(1);
   exception
      when TBL.Invalid_Cursor =>
         Exception_Raised := True;
   end;
   Status.Fail_If(not Exception_Raised, "Exception not raised on Delete");

   Status.New_Test;
   Exception_Raised := False;
   declare
      Tmp : Integer;
   begin
      Tmp := T.Get(1);
   exception
      when TBL.Invalid_Cursor =>
         Exception_Raised := True;
   end;
   Status.Fail_If(not Exception_Raised, "Exception not raised on Get");

   Status.New_Test;
   Exception_Raised := False;
   begin
      T.Replace(1, 4);
   exception
      when TBL.Invalid_Cursor =>
         Exception_Raised := True;
   end;
   Status.Fail_If(not Exception_Raised, "Exception not raised on Replace");

   T.Reserve(Index);

   Status.New_Test;
   Exception_Raised := False;
   begin
      T.Replace(Index, 4);
   exception
      when TBL.Invalid_Cursor =>
         Exception_Raised := True;
   end;
   Status.Fail_If(Exception_Raised, "Exception raised on Replace");

   Status.New_Test;
   Status.Fail_If (not T.Exists(Index), "Not existent");

   Status.New_Test;
   Exception_Raised := False;
   declare
      Tmp : Integer;
   begin
      Tmp := T.Get(Index);
   exception
      when TBL.Invalid_Cursor =>
         Exception_Raised := True;
   end;
   Status.Fail_If(Exception_Raised, "Exception raised on Get");

   Status.New_Test;
   T.Insert(42, Index2);
   Status.Fail_If (Index = Index2, "Twice the same index");

   Status.New_Test;
   Exception_Raised := False;
   begin
      T.Delete(Index);
      T.Delete(Index2);
   exception
      when TBL.Invalid_Cursor =>
         Exception_Raised := True;
   end;
   Status.Fail_If(Exception_Raised, "Exception raised on Delete");

   Total := 0;
   Total_Bis := 0;
   for I in 1..10 loop
      Total := Total + I;
      Total_Bis := Total_Bis + Integer(1.5*Float(I));
      T.Insert(I, Index);
   end loop;

   Status.New_Test;
   Compute_Sum(T, tot);
   Status.Fail_If(Tot /= Total, "Processing");

   Status.New_Test;
   Accum := 0;
   TBL_2_Para.Iterate(T, Accumulate_Bis'Access, 1.5);
   Status.Fail_If(Accum /= Total_Bis, "Double param");

   Status.New_Test;
   T.Iterate(Duplicate'Access);
   Compute_Sum(T, tot);
   Status.Fail_If(Tot /= 2*Total, "Duplicate Processing");


   Status.New_Test;
   Status.Fail_If(not T.Is_Full, "Table not full");

   Status.New_Test;
   Exception_Raised := False;
   begin
      T.Insert(42, Index);
   exception
      when TBL.Table_Full =>
         Exception_Raised := True;
   end;
   Status.Fail_If(not Exception_Raised, "Exception not raised on Insert");

   Final_Report(Status);
end Test_Generic_Tables;

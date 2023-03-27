with Test_Status, Interfaces, Int_Lst, Ada.Text_Io;
use  Test_Status, Interfaces, Int_Lst, Ada.Text_Io;

with Ada.Numerics.Discrete_Random;

procedure Test_Integer_Lists is
   package Rnd_Bool is
      new Ada.Numerics.Discrete_Random(Boolean);


   use Rnd_Bool;

   G     : Generator;
   Max_N : constant Unsigned_32 := 30000;
   Flags : array (0..Max_N) of Boolean := (others => False);
   List  : Int_Lst.Integer_List;

   Ok : Boolean;

   Status : Status_Type;
begin
   Reset(G);
   declare
      S : State;
   begin
      Save(G, S);
      Put_Line(Image(S));
   end;

   for I in Flags'Range loop
      if (Random(G)) then
         Insert(List, I);
         Flags(I) := True;
      end if;
   end loop;

   New_Test(Status);

   Ok := True;
   for I in Flags'Range loop
     if (Flags(I) /= Contains(List, I)) then
        Ok := False;
        exit;
     end if;
   end loop;

   Fail_If(Status, not Ok, "Inserted values");

   New_Test(Status);
   if (Ok) then
      for I in Flags'Last+1..2*Flags'Last loop
         if Contains(List, I) then
            Ok := False;
            exit;
         end if;
      end loop;
   end if;
   Fail_If(Status, not Ok, "Default values");

   Final_Report(Status);
end Test_Integer_Lists;

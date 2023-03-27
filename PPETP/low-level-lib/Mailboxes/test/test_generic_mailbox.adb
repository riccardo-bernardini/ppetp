with Ada.Numerics.Float_Random;    use Ada.Numerics.Float_Random;
with Ada.Text_Io;                  use Ada.Text_Io;
with Ada.Command_Line;             use Ada.Command_Line;

with Generic_Mailboxes;

procedure Test_Generic_Mailbox is
   Gen : Generator;

   function Random_Delta(From, To : Float) return Duration is
   begin
      return Duration(From + (To-From)*Random(Gen));
   end Random_Delta;

   package MBOX is
      new Generic_Mailboxes(Integer);

   type Mbox_Access is access MBOX.Mailbox;

   task Oracolo is
      entry Double_Of (Input     : Integer;
                       Result_To : Mbox_Access);
      entry Stop;
   end Oracolo;

   task body Oracolo is
      Again      : Boolean := True;
      In_Buffer  : Integer;
      Out_Buffer : Mbox_Access;
   begin
      while Again loop
         select
            accept  Double_Of (Input     : Integer;
                               Result_To : Mbox_Access) do
               In_Buffer  := Input;
               Out_Buffer := Result_To;
            end Double_Of;
         or
            accept Stop do
               Again := False;
            end Stop;
         end select;

         if (Again) then
            delay Random_Delta (1.0, 5.0);
            Put (" done ");
            Out_Buffer.Done(In_Buffer*2);
         end if;
      end loop;
   end Oracolo;

   Box    : Mbox_Access := new MBOX.Mailbox;
   Result : Integer;
   Ok     : Boolean := True;
begin
   for I in -5..5 loop
      New_Line;
      Put(Integer'Image(I) & ": ");
      Oracolo.Double_Of(I, Box);
      delay Random_Delta(0.5, 2.5);
      Put (" waiting ");
      Box.Wait(Result);

      if (Result /= 2*I) then
         Put_Line ("Test failed for inp="
                   & Integer'Image(I)
                   & " out="
                   & Integer'Image(Result));
         Ok := False;
      end if;
   end loop;

   if (not Ok) then
      Put_Line ("FAILED");
      Set_Exit_Status (Failure);
   else
      Put_Line ("PASSED");
      Set_Exit_Status (Success);
   end if;

   Oracolo.Stop;
end Test_Generic_Mailbox;

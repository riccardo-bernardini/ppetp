with Text_Io; use  Text_Io;
with Generic_Shared_Buffer;
with Ada.Exceptions;
use  Ada.Exceptions;

procedure Test_Shared_Buffer is
   package Int_Buf is
      new Generic_Shared_Buffer (Integer);

   use Int_Buf;

   Buf : Shared_Buffer(10);

   N_Iter : constant Integer := 12;

   task Reader is
      entry Result (R : out Boolean);
   end Reader;

   task Writer;

   task body Reader is
      Index : Natural;
      OK : Boolean := True;
   begin
      for I in 1..N_Iter loop
         Buf.Index.Begin_Reading(Index);
         Put (Integer'Image(I)
              & " => "
              & Integer'Image(Buf.Buffer(Index)));

         if (2*I = Buf.Buffer(Index)) then
            Put_Line(" OK");
         else
            Put_Line(" *** BAD *** ");
            OK := False;
         end if;

         Buf.Index.Done_Reading(Index);

         delay 0.3;
      end loop;

      accept Result(R : out Boolean) do
         R := Ok;
      end Result;
   exception
      when  E : others =>
         Put_Line("Bum reader!");
         Put_Line(Exception_Message(E));
   end Reader;

   task body Writer is
      Index : Natural;
   begin
      for I in 1..N_Iter loop
         Buf.Index.Begin_Writing(Index);
         Put_Line("W" & Integer'Image(I) & "," & Integer'Image(Index));

         Buf.Buffer(Index) := 2*I;
         Buf.Index.Done_Writing(Index);
      end loop;
   exception
      when E: others =>
         Put_Line("Bum writer!");
         Put_Line(Exception_Message(E));
   end Writer;

   Ok : Boolean;
begin
   Reader.Result(Ok);
   if (Ok) then
      Put_Line ("PASSED");
   else
      Put_Line ("FAILED");
   end if;
end Test_Shared_Buffer;

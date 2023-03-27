with Generic_Shared_Queue;

with Ada.Numerics.Float_Random, Ada.Text_Io, Ada.Command_Line;
use  Ada.Numerics.Float_Random, Ada.Text_Io, Ada.Command_Line;

procedure Test_Shared_Queue is
   Gen : Generator;

   function Random_Delta(From, To : Float) return Duration is
   begin
      return Duration(From + (To-From)*Random(Gen));
   end Random_Delta;

   type Data is
      record
         Source : Integer;
         Stamp  : Integer;
      end record;

   package Q is
      new Generic_Shared_Queue (Data);

   type Queue_Access is access Q.Queue;

   task type Producer(Id : Integer) is
      entry Start(X : Queue_Access);
   end Producer;

   task body Producer is
      Queue : Queue_Access;
      Buf   : Data;
   begin
      accept Start(X : Queue_Access) do
         Queue := X;
      end Start;

      for I in -5..5 loop
         Buf := (Source => Id,
                 Stamp  => I);
         delay Random_Delta(0.1, 0.5);
         Queue.Insert(Buf);
      end loop;
   end Producer;

   type Int_Array is array (Positive range <>) of Integer;
   type Producer_Pt is access Producer;
   type Task_Array is array (Positive range <>) of Producer_Pt;

   N_Producer : constant Integer := 4;

   Last_Read : Int_Array(1..N_Producer) := (others => -6);
   Jobs      : Task_Array(Last_Read'Range);
   N_Done    : Natural := 0;

   Exception_Raised : Boolean;
   Buf : Data;
   Queue : Queue_Access := new Q.Queue;
   Ok : Boolean := True;
begin
   -- First check that Peek on an empty queue raises an exception
   Exception_Raised := False;
   begin
      Buf := Queue.Peek;
   exception
      when Q.Empty_Queue =>
         Exception_Raised := True;
   end;

   if (not Exception_Raised) then
      Put_Line ("FAILURE: Exception not raised on empty queue");
      Ok := False;
   end if;

   -- Now check that Is_Empty return True
   if (not Queue.Is_Empty) then
      Put_Line ("FAILURE: Is_Empty return False");
      Ok := False;
   end if;

   -- Now start the producers
   for I in Jobs'Range loop
      Jobs(I) := new Producer(I);
      Jobs(I).Start(Queue);
   end loop;

   -- Read the produced data and check that they are in the
   -- correct order
   while N_Done < N_Producer loop
      Put("+");
      Queue.Extract (Buf);
      if (Buf.Stamp /= Last_Read(Buf.Source) + 1) then
         Put_Line ("Wrong read on job "
                   & Integer'Image(Buf.Source));
         Ok := False;
      end if;

      Last_Read(Buf.Source) := Buf.Stamp;
      if (Buf.Stamp = 5) then
         N_Done := N_Done + 1;
      end if;
   end loop;

   New_Line;

   -- Check the task are terminated
   for I in Jobs'Range loop
      if (not Jobs(I)'Terminated) then
         Put_Line ("FAILURE: Job #"
                   & Integer'Image(I)
                   & " still alive");
         Ok := False;
      end if;
   end loop;

   -- Now the queue should be empty
   if (not Queue.Is_Empty) then
      Put_Line ("FAILURE: Non-empty queue");
   end if;

   if (not Ok) then
      Put_Line ("FAILED");
      Set_Exit_Status (Failure);
   else
      Put_Line ("PASSED");
      Set_Exit_Status (Success);
   end if;
end Test_Shared_Queue;

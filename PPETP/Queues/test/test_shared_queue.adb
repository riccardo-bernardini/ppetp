with  Test_Report;            use Test_Report;
with Generic_Shared_Queue;

procedure Test_Shared_Queue is
   package Integer_Queue is
      new Generic_Shared_Queue(Integer);

   Reporter : Reporter_Type;

   Queue : Integer_Queue.Queue_Handler;
   Buf   : Integer;
   Excp_Raised : Boolean;
begin
   Reporter.New_Suite("Init to empty");

   Reporter.New_Result (Queue.Is_Empty);

   Reporter.New_Suite("Insert/extract");
   for I in 1..5 loop
      Queue.Insert(I);
   end loop;

   for I in 1..5 loop
      Queue.Extract(Buf);
      Reporter.New_Result(I = Buf);
   end loop;

   Reporter.New_Suite("Extract_Now");
   for I in 1..5 loop
      Queue.Insert(I);
   end loop;

   for I in 1..5 loop
      Queue.Extract_Now(Buf);
      Reporter.New_Result(I = Buf);
   end loop;

   Reporter.New_Suite("Is_Empty");
   Reporter.New_Result(Queue.Is_Empty);
   Queue.Insert(42);
   Reporter.New_Result(not Queue.Is_Empty);
   Queue.Extract(Buf);

   Reporter.New_Suite("Empty_Queue");

   begin
      Excp_Raised := False;
      Queue.Extract_Now(Buf);
   exception
      when Integer_Queue.Empty_Queue =>
         Excp_Raised := True;
   end;

   Reporter.New_Result(Excp_Raised);

   Reporter.New_Suite("Peek");

   Queue.Insert(21);
   Queue.Insert(33);

   Reporter.New_Result(Queue.Peek = 21);
   Reporter.New_Result(Queue.Peek = 21);
   Queue.Extract(Buf);
   Reporter.New_Result(Buf = 21);
   Reporter.New_Result(Queue.Peek = 33);
   Queue.Extract(Buf);
   Reporter.New_Result(Buf = 33);

   Reporter.New_Suite("Empty_Queue [peek]");

   begin
      Excp_Raised := False;
      Buf := Queue.Peek;
   exception
      when Integer_Queue.Empty_Queue =>
         Excp_Raised := True;
   end;

   Reporter.New_Result(Excp_Raised);

   Final (Reporter);
end Test_Shared_Queue;

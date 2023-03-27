with  Test_Report;            use Test_Report;
with Generic_Indefinite_Shared_Queue;

procedure Test_Shared_Indefinite_Queue is
   type My_Data (N : Integer) is null record;

   package My_Queue is
      new Generic_Indefinite_Shared_Queue(My_Data);

   Reporter : Reporter_Type;

   Queue : My_Queue.Queue_Handler;
   Buf   : My_Data(N => 12);
   Excp_Raised : Boolean;
begin
   Reporter.New_Suite("Init to empty");

   Reporter.New_Result (Queue.Is_Empty);

   Reporter.New_Suite("Insert/extract");
   for I in 1..5 loop
      Queue.Insert((N => I));
   end loop;

   for I in 1..5 loop
      Queue.Extract(Buf);
      Reporter.New_Result(I = Buf.N);
   end loop;

   -- Reporter.New_Suite("Extract_Now");
   -- for I in 1..5 loop
   --    Queue.Insert((N => I));
   -- end loop;
   --
   -- for I in 1..5 loop
   --    Queue.Extract_Now(Buf);
   --    Reporter.New_Result(I = Buf.N);
   -- end loop;

   Reporter.New_Suite("Is_Empty");
   Reporter.New_Result(Queue.Is_Empty);
   Queue.Insert((N => 42));
   Reporter.New_Result(not Queue.Is_Empty);
   Queue.Extract(Buf);

   -- Reporter.New_Suite("Empty_Queue");
   --
   -- begin
   --    Excp_Raised := False;
   --    Queue.Extract_Now(Buf);
   -- exception
   --    when My_Queue.Empty_Queue =>
   --       Excp_Raised := True;
   -- end;
   --
   -- Reporter.New_Result(Excp_Raised);

   Reporter.New_Suite("Peek");

   Queue.Insert((N => 21));
   Queue.Insert((N => 33));

   Reporter.New_Result(Queue.Peek.N = 21);
   Reporter.New_Result(Queue.Peek.N = 21);
   Queue.Extract(Buf);
   Reporter.New_Result(Buf.N = 21);
   Reporter.New_Result(Queue.Peek.N = 33);
   Queue.Extract(Buf);
   Reporter.New_Result(Buf.N = 33);

   Reporter.New_Suite("Empty_Queue [peek]");

   begin
      Excp_Raised := False;
      Buf := Queue.Peek;
   exception
      when My_Queue.Empty_Queue =>
         Excp_Raised := True;
   end;

   Reporter.New_Result(Excp_Raised);

   Final (Reporter);
end Test_Shared_Indefinite_Queue;


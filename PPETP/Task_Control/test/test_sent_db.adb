with Test_Report;        use Test_Report;
with Sent_Db;            use Sent_Db;
with PPETP;              use PPETP;
with Network;            use Network;
with Boolean_Mailboxes;  use Boolean_Mailboxes;
with Ada.Calendar;       use Ada.Calendar;
with Ada.Text_Io;        use Ada.Text_Io;

procedure Test_Sent_Db is
   Tbl : Table;
   subtype Test_Range is Integer range 1..3;

   Mbox_Array : array (Test_Range) of Mailbox_Access :=
     (new Mailbox, new Mailbox, new Mailbox);

   Pkt_Array : array (Test_Range)  of Packet_Info :=
     (1 => (Data       => null,
            Target     => (Family => Family_Inet,
                           Addr   => Inet_Addr("127.0.0.1"),
                           Port   => 42),
            Timestamp  => 123,
            Reply_Mbox => Mbox_Array (1)),
      2 => (Data       => null,
            Target     => (Family => Family_Inet,
                           Addr   => Inet_Addr("192.10.11.11"),
                           Port   => 84),
            Timestamp  => 214,
            Reply_Mbox => Mbox_Array (2)),
      3 => (Data       => null,
            Target     => (Family => Family_Inet,
                           Addr   => Inet_Addr("10.10.1.1"),
                           Port   => 21),
            Timestamp  => 521,
            Reply_Mbox => Mbox_Array (3)));

   Reporter : Reporter_Type;
   Timeout  : Time;
   Info     : Packet_Info;
   Ok       : Boolean;
   Raised   : Boolean;
begin
   -- Reporter.Be_Verbose;

   Tbl.Set_Timeout_Interval(5.0);
   Tbl.Set_Max_Trials(2);

   Reporter.New_Suite("Empty table");
   Reporter.New_Result (Tbl.Is_Empty);

   Reporter.New_Suite("Default timeout");

   Timeout := Tbl.Earliest_Timeout(30.0);
   Reporter.New_Result (Timeout > Clock+28.0);

   begin
      Raised := False;
      Timeout := Tbl.Earliest_Timeout;
   exception
      when Empty_Table =>
         Raised := True;
   end;
   Reporter.New_Result (Raised);

   Reporter.New_Suite("In/Out");

   for I in Pkt_Array'Range loop
      Tbl.Packet_Sent(Pkt_Array(I));
      delay 1.0;
   end loop;

   Reporter.New_Result (not Tbl.Is_Empty);

   for I in Pkt_Array'Range loop
      Timeout := Tbl.Earliest_Timeout(0.01);
      Reporter.New_Result (Timeout <= Clock+0.01);

      Timeout := Tbl.Earliest_Timeout(30.0);
      Reporter.New_Result (Timeout <= Clock+5.0);
      delay until (Timeout+0.01);

      Tbl.Next_Expired(Info);
      Reporter.New_Result (Info /= No_Info);
      Reporter.New_Result (Info.Timestamp = Pkt_Array(I).Timestamp);

      Tbl.Packet_Done (Info.Timestamp, True);
      Tbl.Packet_Done (Info.Timestamp, True);
      Mbox_Array(I).Wait(Ok);
      Reporter.New_Result (Ok);
   end loop;

   Reporter.New_Result (Tbl.Is_Empty);

   declare
      Mbox : Mailbox_Access := new Mailbox;
      Pkt  : Packet_Info    := (Data       => null,
                                Target     => (Family => Family_Inet,
                                               Addr   =>
                                                 Inet_Addr("127.0.0.1"),
                                               Port   => 111),
                                Timestamp  => 5490,
                                Reply_Mbox => Mbox);
   begin
      Reporter.New_Suite("Done packet");
      Tbl.Packet_Sent(Pkt);
      delay 1.0;
      Tbl.Packet_Done (Pkt.Timestamp, False);
      Mbox.Wait(Ok);
      Reporter.New_Result(not Ok);
      Reporter.New_Result(Tbl.Is_Empty);
   end;


   declare
      Mbox : Mailbox_Access := new Mailbox;
      Pkt  : Packet_Info    := (Data       => null,
                                Target     => (Family => Family_Inet,
                                               Addr   =>
                                                 Inet_Addr("10.0.0.1"),
                                               Port   => 16320),
                                Timestamp  => 6123,
                                Reply_Mbox => Mbox);
      Pkt_B : Packet_Info    := (Data       => null,
                                 Target     => (Family => Family_Inet,
                                                Addr   =>
                                                  Inet_Addr("100.10.0.1"),
                                                Port   => 12345),
                                 Timestamp  => 7555,
                                 Reply_Mbox => new Mailbox);

      Pkt_C : Packet_Info    := (Data       => null,
                                 Target     => (Family => Family_Inet,
                                                Addr   =>
                                                  Inet_Addr("10.0.0.1"),
                                                Port   => 2222),
                                 Timestamp  => 98710,
                                 Reply_Mbox => new Mailbox);
      Pkt2 : Packet_Info;
   begin
      Reporter.New_Suite("Find packet");
      Tbl.Packet_Sent(Pkt_C);
      Tbl.Packet_Sent(Pkt);
      Tbl.Packet_Sent(Pkt_B);
      delay 1.0;
      Pkt2 := Tbl.Find(Pkt.Timestamp);
      Reporter.new_result(Pkt2 /= No_Info);
      Reporter.new_result(Pkt2.Target.Port = Pkt.Target.Port and
                            Pkt2.Timestamp = Pkt.Timestamp);

      Pkt2 := Tbl.Find(1);
      Reporter.new_result(Pkt2 = No_Info);
      Pkt2 := Tbl.Find(Pkt_Array(1).Timestamp);
      Reporter.new_result(Pkt2 = No_Info);

      Tbl.Packet_Done(Pkt.Timestamp, True);
      Tbl.Packet_Done(Pkt_B.Timestamp, true);
      Tbl.Packet_Done(Pkt_C.Timestamp, true);
      Reporter.new_result(Tbl.Is_Empty);
   end;

   declare
      Mbox : Mailbox_Access := new Mailbox;
      Pkt  : Packet_Info    := (Data       => null,
                                Target     => (Family => Family_Inet,
                                               Addr   =>
                                                 Inet_Addr("10.0.0.1"),
                                               Port   => 33201),
                                Timestamp  => 555,
                                Reply_Mbox => Mbox);
      Pkt_Buf : Packet_Info;
   begin
      Reporter.New_Suite ("Max Trials");

      Tbl.Set_Max_Trials(3);
      Tbl.Set_Timeout_Interval(1.0);

      Pkt_Buf := Pkt;

      for I in 1..2 loop
         Tbl.Packet_Sent(Pkt_Buf);

         delay until Tbl.Earliest_Timeout;

         Tbl.Next_Expired (Pkt_Buf);

         Reporter.New_Result(Pkt_Buf /= No_Info);
         Reporter.New_Result(Pkt_Buf.Timestamp = Pkt.Timestamp);

         declare
            Busy : Boolean := False;
         begin
            select
               Mbox.Wait(Ok);
            else
               Busy := True;
            end select;
            Reporter.New_Result(Busy);
         end;
      end loop;

      Tbl.Packet_Sent(Pkt_Buf);
      Reporter.New_Result(not Tbl.Is_Empty);
      delay until Tbl.Earliest_Timeout;
      Tbl.Next_Expired (Pkt_Buf);
      Reporter.New_Result(Pkt_Buf = No_Info);

      declare
         Busy : Boolean := False;
      begin
         Ok := True;
         select
            Mbox.Wait(Ok);
         else
            Busy := True;
         end select;
         Reporter.New_Result(not Busy);
         Reporter.New_Result(not Ok);
      end;
   end;


   Reporter.Final;
end Test_Sent_Db;

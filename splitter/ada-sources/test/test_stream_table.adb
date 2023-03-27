--
with Ada.Calendar;                     use Ada.Calendar;
with Ada.Calendar.Formatting;          use Ada.Calendar.Formatting;
with Ada.Text_IO;                      use Ada.Text_IO;
with Test_Report;                      use Test_Report;
with Splitter_Lib.Stream_Table;        use Splitter_Lib;
with Splitter_Lib.RTP_Streams;         use Splitter_Lib.RTP_Streams;
with Splitter_Lib.RTP;                 use Splitter_Lib.RTP;
with Splitter_Lib.Players;             use Splitter_Lib.Players;

procedure Test_Stream_Table is
   type Datum is
      record
         Ssrc  : Ssrc_Type;
         Descr : Player_Descriptor;
      end record;

   type Data_Array is array (Natural range <>) of Datum;

   Data : Data_Array :=
            ((Ssrc  => 15,
              Descr => (ID => 7, Port => 3232)),
             (Ssrc  => 321,
              Descr => (Id => 21, Port => 4555)),
             (Ssrc  => 355,
              Descr => (Id => 121, Port => 555)));

   S : Stream;
begin
   Stream_Table.Use_Timeout (5.0);

   for I in Data'Range loop
      S := Create (Data (I).Ssrc, Data (I).Descr);
      delay 2.0;
      Stream_Table.Add(S);
   end loop;

   Put_Line ("Now: " & Image (Clock));
   Put_Line ("First timeout: " & Image (Stream_Table.Earliest_Timeout));
   delay 2.0;
   declare
      A : Stream_Table.Source_Array :=
            Stream_Table.Timed_Out_Sources (Clock);
   begin
      for I in A'Range loop
         Put_Line ("SSRC = " & Ssrc_Type'Image(SSRC(A(I))));
      end loop;
   end;
end Test_Stream_Table;

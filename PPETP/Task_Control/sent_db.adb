with Ada.Streams;        use Ada.Streams;
with Ada.Text_Io;        use Ada.Text_Io;

with Ada.Unchecked_Deallocation;
with PPETP;
with PPETP_Mail;	use PPETP_Mail;
with Packets.Protocol.Command;		use Packets.Protocol.Command ;
package body Sent_DB is
   use type PPETP.Command_Sequence_Number;

   ---------
   -- "<" --
   ---------

   function "<" (Left, Right : Timeout_Info) return Boolean
   is

   begin
      if (Left.Timeout = Right.Timeout) then
         return (Left.Sequence_Num < Right.Sequence_Num);
      else
         return (Left.Timeout < Right.Timeout);
      end if;
   end "<";

   -----------------
   -- Packet_Done --
   -----------------

   procedure Packet_Done (Container    : in out Table;
                          Sequence_Num : PPETP.Command_Sequence_Number;
                          Result       : PPETP_Mail_Ack_Type) is
      procedure Free is
        new Ada.Unchecked_Deallocation (Object => Stream_Element_Array,
                                        Name   => Stream_Array_Pt);
      procedure Free is
        new Ada.Unchecked_Deallocation (Object => Sent_Packet_Info,
                                        Name   => Packet_Info_Access);

      Pos      : PKT_Map.Cursor;
      Pkt_Info : Packet_Info_Access;


      use type PKT_Map.Cursor;
   begin
      Pos := Container.Packets.Find (Sequence_Num);
      if (Pos = PKT_Map.No_Element) then
         -- Why can this  (not finding the info in the packet
         -- map) happen?  Because it could be that we
         -- received more than one ACK for the same packet,
         -- for example, because the first one was late and
         -- we sent twice the same packet.  If this happen,
         -- we simply ignore the duplicate ACK.

         return;
      end if;

      -- If I am here, we found the entry in the packet map.
      -- First, remove the entry
      Pkt_Info := PKT_Map.Element (Pos);
      Container.Packets.Delete (Pos);

      Pkt_Info.Info.Reply_Mbox.Done (Result);

      -- Now we must delete the corresponding entry from
      -- the timeout table.  Note that the entry could have
      -- already been removed, for example when we are internally
      -- after an excessive number of trials.
      declare
         Timeout_Pos : Timeout_Map.Cursor;
         Timeout_Entry : Timeout_Info :=
              (Timeout      => Pkt_Info.Timeout,
               Sequence_Num => Pkt_Info.Info.Sequence_Num);

         use type Timeout_Map.Cursor;
      begin
         Timeout_Pos := Container.Timeouts.Find (Timeout_Entry);
         if (Timeout_Pos /= Timeout_Map.No_Element) then
            Container.Timeouts.Delete (Timeout_Pos);
         end if;
      end;

      Free (Pkt_Info.Info.Data);
      Free (Pkt_Info);
   end Packet_Done;

   --------------------------
   -- Set_Timeout_Interval --
   --------------------------

   procedure Set_Timeout_Interval (T : in out Table; X : Duration) is
   begin
      T.Timeout_Interval := X;
   end Set_Timeout_Interval;


   ---------------------------------
   -- Set_Routed_Timeout_Interval --
   ---------------------------------

   procedure Set_Routed_Timeout_Interval (T : in out Table; X : Duration) is
   begin
      T.Timeout_Routed_Interval := X;
   end Set_Routed_Timeout_Interval;

   --------------------
   -- Set_Max_Trials --
   --------------------

   procedure Set_Max_Trials (T : in out Table; N : Natural) is
   begin
      T.Max_Trials := N;
   end Set_Max_Trials;

   ----------------------
   -- Earliest_Timeout --
   ----------------------

   function Earliest_Timeout (Tbl      : Table;
                              Max_Wait : Duration) return Time is
      Default_Timeout : Time := Clock + Max_Wait;
   begin
      if (Tbl.Is_Empty) then
         return Default_Timeout;
      else
         declare
            Earliest : Time := Tbl.Timeouts.First_Element.Timeout;
         begin
            if (Earliest > Default_Timeout) then
               return Default_Timeout;
            else
               return Earliest;
            end if;
         end;
      end if;
   end Earliest_Timeout;

   ----------------------
   -- Earliest_Timeout --
   ----------------------

   function Earliest_Timeout (Tbl : Table) return Time is
   begin
      if (Tbl.Is_Empty) then
         -- raise Empty_Table;
         -- return a big interval
         return Time_Of(Year    => 2100,
                        Month   => 4,
                        Day     => 20);
      else
         return Tbl.Timeouts.First_Element.Timeout;
      end if;
   end Earliest_Timeout;

   function Is_Empty (Tbl : Table) return Boolean is
   begin
      return Tbl.Timeouts.Is_Empty;
   end Is_Empty;

   -----------------
   -- Packet_Sent --
   -----------------

   procedure Packet_Sent (Tbl    : in out Table;
                          Info   : in     Packet_Info;
                          Routed : in     Boolean) is

      Pos         : PKT_Map.Cursor;
      New_Timeout : Time;
      My_Info     : Packet_Info_Access;
      use type PKT_Map.Cursor;
   begin

      if Routed then
         New_Timeout := Tbl.Timeout_Routed_Interval + Clock;
      else

         New_Timeout := Tbl.Timeout_Interval + Clock;
      end if;
      Pos := Tbl.Packets.Find (Info.Sequence_Num);
      if (Pos /= PKT_Map.No_Element) then
         My_Info := PKT_Map.Element(Pos);

         if (My_Info.Remaining_Trials = 0) then

            declare
               Tmp: PPETP_Mail_Ack_Type := (Received => False,
                                            Reason   => No_Replay); -- this field don't matter, the
                                                                    -- ack packet is not received
            begin

               Tbl.Packet_Done(My_Info.Info.Sequence_Num, Tmp);
               return;
            end;

         else
            My_Info.Remaining_Trials := My_Info.Remaining_Trials - 1;
            My_Info.Timeout := New_Timeout;

            Tbl.Packets.Replace_Element (Position => Pos,
                                         New_Item => My_Info);
         end if;
      else
         My_Info := new Sent_Packet_Info'(Info       => Info,
                                          Remaining_Trials =>
                                            Tbl.Max_Trials - 1,
                                          Timeout    => New_Timeout);
         Tbl.Packets.Insert (Key      => My_Info.Info.Sequence_Num,
                             New_Item => My_Info);
      end if;

      Tbl.Timeouts.Insert ((Timeout      => New_Timeout,
                            Sequence_Num => My_Info.Info.Sequence_Num));

   end Packet_Sent;

   procedure Next_Expired (Tbl  : in out Table;
                           Info :    out Packet_Info) is
      Expired_Sequence_Num : PPETP.Command_Sequence_Number;
      Tmp_Info : Packet_Info_Access;
   begin
      while (not Tbl.Is_Empty and then Tbl.Earliest_Timeout < Clock) loop
         -- If I am here, the first timeout expired.
         -- First, remove the corresponding entry
         -- from the timeout map
         Expired_Sequence_Num := Tbl.Timeouts.First_Element.Sequence_Num;
         Tbl.Timeouts.Delete_First;

         -- Get the corresponding entry in the packet map
         Tmp_Info := Tbl.Packets.Element (Expired_Sequence_Num);
         if (Tmp_Info.Remaining_Trials = 0) then
            -- Packet Tmp_Info already had its share of trials
            declare
               Tmp: PPETP_Mail_Ack_Type := (Received => False,
                                            Reason   => No_Replay); -- this field don't matter, the
                                                                    -- ack packet is not received
            begin
               Tbl.Packet_Done (Tmp_Info.Info.Sequence_Num, Tmp);
            end;
         else
            -- Found an expired re-transmissible packet
            Info := Tmp_Info.Info;
            return;
         end if;
      end loop;

      -- If I am here, no expired re-transmissible packet
      -- was found
      Info := No_Info;
   end Next_Expired;

   function Find (Tbl          : Table;
                  Sequence_Num : PPETP.Command_Sequence_Number) return Packet_Info
   is
      Pos : PKT_Map.Cursor;
      use type PKT_Map.Cursor;
   begin
      Pos := Tbl.Packets.Find (Sequence_Num);
      if (Pos = PKT_Map.No_Element) then
         return No_Info;
      else
         return PKT_Map.Element (Pos).Info;
      end if;
   end Find;

end Sent_DB;


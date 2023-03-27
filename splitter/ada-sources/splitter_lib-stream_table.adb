--
with Ada.Text_IO;
with Splitter_Lib.RTP_Streams;
with Ada.Calendar.Formatting;

package body Splitter_Lib.Stream_Table is
   type Source_Descriptor is
      record
         Stream     : RTP_Streams.Stream;
         Expiration : Calendar.Time;
      end record;

   type Table_Cursor is new Integer;
   type Stream_Array is array (Table_Cursor range <>) of Source_Descriptor;



   Table           : Stream_Array (1 .. Table_Cursor(Granted_Table_Size));
   No_Cursor       : constant Table_Cursor := Table'First - 1;
   Next_Free_Entry : Table_Cursor := Table'First;
   Timeout         : Duration := 5.0;

   Forever         : Duration := 3600.0 * 24 * 365;

   ----------------------
   -- Timed_Out_Source --
   ----------------------

   function Timed_Out_Sources
     (Current_Time : Calendar.Time)
      return RTP_Streams.Stream_Array
   is
      use Ada.Calendar;
      use Ada.Calendar.Formatting;
      use Ada.Text_IO;

      Result : RTP_Streams.Stream_Array (1 .. Integer(Next_Free_Entry - Table'First));
      Index  : Integer;
   begin
--      Put_Line ("Ora:" & Image (Current_Time));

      Index := Result'First;
      for I in Table'First .. Next_Free_Entry - 1 loop
--           Put_Line ("Entry N. "
--                     & Table_Cursor'Image (I)
--                     & " expires at "
--                     & Image (Table (I).Expiration));

         if Table (I).Expiration < Current_Time then
            Put_Line("SCADUTA entry #" & i'img &
                     "SSRC=" & RTP.SSRC_Type'Image (rtp_streams.Ssrc(table(i).stream))&
                     "ID=" & integer(rtp_streams.player_id(table(i).stream))'img
                    );
            Result (Index) := Table (I).Stream;
            Index := Index + 1;
         end if;
      end loop;

      return Result (Result'First .. Index - 1);
   end Timed_Out_Sources;

   ----------------
   -- Find_Index --
   ----------------

   function Find_Index
     (SSRC : RTP.SSRC_Type)
      return Table_Cursor
   is
      use type RTP.SSRC_Type;
   begin
      for I in Table'First .. Next_Free_Entry - 1 loop
         if SSRC = RTP_Streams.SSRC (Table (I).Stream) then
            return I;
         end if;
      end loop;

      return No_Cursor;
   end Find_Index;



   ----------
   -- Find --
   ----------

   function Find
     (SSRC : RTP.SSRC_Type)
      return RTP_Streams.Stream
   is
      Idx : Table_Cursor := Find_Index(SSRC);
   begin
      if Idx = No_Cursor then
         return RTP_Streams.No_Stream;
      else
         return Table (Idx).stream;
      end if;
   end Find;


   -----------------
   -- Use_Timeout --
   -----------------

   procedure Use_Timeout (Amount : Duration) is
   begin
      Timeout := Amount;
   end Use_Timeout;

   ----------------------
   -- Earliest_Timeout --
   ----------------------

   function Earliest_Timeout return Calendar.Time is
      use type Calendar.Time;

      Result : Calendar.Time := Forever + Calendar.Clock;
   begin
      for I in Table'First .. Next_Free_Entry - 1 loop
         if Table (I).Expiration < Result then
            Result := Table (I).Expiration;
         end if;
      end loop;
      return Result;
   end Earliest_Timeout;

   ------------
   -- Remove --
   ------------

   procedure Remove (What : RTP_Streams.Stream) is
      Position : Table_Cursor;
   begin
      Position := Find_Index (RTP_Streams.SSRC (What));
      if Position /= No_Cursor then
         Table (Position) := Table (Next_Free_Entry - 1);
         Next_Free_Entry  := Next_Free_Entry - 1;
      end if;
   end Remove;

   ---------
   -- Add --
   ---------

   procedure Add (What : RTP_Streams.Stream) is
      use type Calendar.Time;

      Position : Table_Cursor;
   begin
      Position := Find_Index (RTP_Streams.SSRC (What));
      if Position /= No_Cursor then
         raise Program_Error;
      end if;

      if Next_Free_Entry > Table'Last then
         raise Program_Error;
      end if;

      Table (Next_Free_Entry) := (Stream     => What,
                                  Expiration => Calendar.Clock + Timeout);

      Next_Free_Entry  := Next_Free_Entry + 1;
   end Add;

   -------------------
   -- Reset_Timeout --
   -------------------

   procedure Reset_Timeout (SSRC : RTP.SSRC_Type)
   is
      use type Calendar.Time;
      use Text_IO;
      use Ada.Calendar.Formatting;
      Position : Table_Cursor := Find_Index(SSRC);
   begin
      -- Put_Line ("RRR");
      if Position /= No_Cursor then
         Table (Position).Expiration := Calendar.Clock + Timeout;
         -- Put_Line("Reset timeout to " & Image(Table (Position).Expiration));
      end if;
   end Reset_Timeout;

end Splitter_Lib.Stream_Table;

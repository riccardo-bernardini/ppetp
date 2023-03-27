--
--
-- ** What is this? **
-- *******************
--
-- The duty of the RTP-splitter is to dispatch RTP packets to different
-- UDP ports on the basis of the SSRC field.  This package implements
-- a "table of streams" used to map each SSRC to the corresponding
-- RTP stream.  The table keeps also track of the timeout associated
-- with a stream (if no packets are received before the timeout expiration,
-- the corresponding stream is removed from the table).  The table can
-- store at least Granted_Table_Size streams.
--
-- Please note that we are talking about a *single* table, built in
-- the package body.  Although this is not perfect from the point of view
-- of software reuse, we decided that this is a very specialized package
-- (so it is unlikely that it will be reused somewhere else )and that
-- this solution was a bit simpler to implement.
--
-- ** What kind of "actions" are possible? **
-- ******************************************
--
-- The Stream table allows for the following actions
--
--   * Stream-related:
--       + Add a new stream
--       + Remove a stream
--       + Search for a stream (via the SSRC)
--
--   * Timeout-related
--       + Set the timeout value
--       + Refresh the timeout of a stream
--       + Retrieve the earliest timeout
--       + Loop over timed out streams
--

with Ada.Calendar;
with Splitter_Lib.RTP_Streams;
with Splitter_Lib.RTP;

use Ada;

package Splitter_Lib.Stream_Table is
   Granted_Table_Size : constant Positive := 128;

   -- Add a new RTP stream to the table.  Raises an exception if a
   -- stream with the same SSRC is already present
   procedure Add (What : RTP_Streams.Stream);

   -- Remove an RTP stream.
   procedure Remove (What : RTP_Streams.Stream);

   -- Search for the stream with the give SSRC.  If no stream is found,
   -- return RTP_Streams.No_Stream
   function Find (SSRC : RTP.SSRC_Type)
                  return RTP_Streams.Stream;

   -- Set the timeout value (Amount is in seconds)
   procedure Use_Timeout (Amount : Duration);

   -- Return the earliest timeout
   function Earliest_Timeout return Calendar.Time;

   -- Reset the timeout associated with the stream with the given SSRC
   procedure Reset_Timeout (SSRC : RTP.SSRC_Type);



   -- Return an array of streams whose expiration date is antecedent the
   -- parameter given to the function.  Return an empty array if no such
   -- stream exists.
   function Timed_Out_Sources (Current_Time : Calendar.Time)
                              return RTP_Streams.Stream_Array;
end Splitter_Lib.Stream_Table;

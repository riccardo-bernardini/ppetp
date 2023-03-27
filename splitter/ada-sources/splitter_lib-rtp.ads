--
-- *******************
-- ** What is this? **
-- *******************
--
-- This is a tiny-weeny package to handle the SSRC field in RTP packets
--
with Ada.Streams;                            use Ada.Streams;

package Splitter_Lib.RTP is
   -- Type representing the 32-bit SSRC field
   type SSRC_Type is  mod 2 ** 32;

   No_SSRC : constant SSRC_Type := 0;

   -- Return the SSRC of an RTP packet.  This function raises
   -- Invalid_RTP_Packet if Packet has a structure that it is
   -- not compatible with an RTP packet (e.g., it is too short)
   function Get_SSRC (Packet : Stream_Element_Array)
                      return SSRC_Type;

   Invalid_RTP_Packet : exception;
end Splitter_Lib.RTP;

--
-- ** What is this? **
-- *******************
--
-- This package provides a type Stream representing an "RTP stream."  An RTP
-- stream represents a stream of packets with the same SSRC (see RTP RFC)
-- and it is characterized by the following attributes
--
--     + The SSRC
--     + The player where the packets are sent
--
-- A special constant No_Stream is provided.
--
-- ** What kind of "actions" are possible? **
-- ******************************************
--
--    + Create  an RTP stream with a given player and SSRC
--    + Destroy an RTP stream
--    + Send a packet to the player
--    + Retrieve the SSRC and the player ID associated with the stream
--
with Splitter_Lib.RTP;
with Splitter_Lib.Controller;
with Splitter_Lib.Network;
with Splitter_Lib.Players;

package Splitter_Lib.RTP_Streams is
   type Stream is private;

   No_Stream : constant Stream;

   type Stream_Array is array (Natural range <>) of RTP_Streams.Stream;

   -- Create a new RTP stream
   function Create (SSRC   : RTP.SSRC_Type;
                    Player : players.Player_Descriptor)
                    return Stream;

   -- Close an RTP stream and make it void
   procedure Close (S : in out Stream);

   -- Send a packet to the attached player
   procedure Send (What : Network.Packet_Buffer;
                   To   : Stream);

   -- Get the SSRC associated to the stream
   function SSRC (X : Stream) return RTP.SSRC_Type;
   pragma Inline (SSRC);

   -- Get the player ID associated to the stream
   function Player_Id (X : Stream) return players.Player_ID;
   pragma Inline (Player_ID);

   -- Return true if S is a "void" stream (i.e., S is equivalent to
   -- No_Stream)
   function Is_Void (S : Stream)
                     return Boolean;
   pragma Inline (Is_Void);
private
   type Stream is
      record
         SSRC   : RTP.SSRC_Type;
         Player : players.Player_Descriptor;
         Socket : Network.Output_UDP_Socket;
      end record;

   No_Stream : constant Stream := (SSRC   => 0,
                                   Player => players.No_Player,
                                   others => <>);
end Splitter_Lib.RTP_Streams;

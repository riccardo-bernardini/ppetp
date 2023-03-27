--
package body Splitter_Lib.RTP_Streams is

   -------------
   -- Is_Void --
   -------------

   function Is_Void
     (S : Stream)
      return Boolean
   is
   begin
      return S = No_Stream;
   end Is_Void;

   ------------
   -- Create --
   ------------

   function Create
     (SSRC   : RTP.SSRC_Type;
      Player : Players.Player_Descriptor)
      return Stream
   is
   begin
      return Stream'(SSRC => SSRC,
                     Player => Player,
                     Socket => Network.UDP_Output(Network.Localhost, Player.Port));
   end Create;



   -----------
   -- Close --
   -----------

   procedure Close (S : in out Stream) is
   begin
      Network.Close (S.Socket);
      S := No_Stream;
   end Close;

   ----------
   -- Send --
   ----------

   procedure Send
     (What : Network.Packet_Buffer;
      To   : Stream)
   is
   begin
      Network.Send (What, To.Socket);
   exception
      when Network.Socket_Error =>
         -- Ignore Socket_Error exception that could be raised
         -- if we send the packet when the player is still
         -- starting.  **TODO**: something more elaborate,
         -- for example, ignore the exception for at most K
         -- times or T seconds or, better yet, keep the packets
         -- in a queue and try again later. (The latter solution
         -- requires a task, so we keep it for later...)
         null;
   end Send;

   ----------
   -- SSRC --
   ----------

   function SSRC (X : Stream) return RTP.SSRC_Type is
   begin
      return X.SSRC;
   end SSRC;

   ---------------
   -- Player_Id --
   ---------------

   function Player_Id (X : Stream) return Players.Player_ID is
   begin
      return X.Player.ID;
   end Player_Id;

end Splitter_Lib.RTP_Streams;

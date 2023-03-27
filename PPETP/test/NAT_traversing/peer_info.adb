package body Peer_Info is

   function Image(Id: Peer_Identifier) return Unbounded_String is
   begin
      return To_Unbounded_String(Unsigned_32'image( Unsigned_32(Id)));
   end Image;


   function Image(Addr: Peer_Address) return Unbounded_String is
   begin
      return To_Unbounded_String(GNAT.Sockets.Image(Addr));
   end Image;


   -- Generate a random Peer_Identifier
   function Random_Peer_ID return Peer_Identifier is
   begin
      return Peer_Identifier(Random_Uns_32);
   end;


end Peer_Info;

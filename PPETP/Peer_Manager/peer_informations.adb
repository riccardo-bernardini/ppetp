with Network;
with network_utilities;			use network_utilities;
with PPETP;			use PPETP;



package body Peer_Informations is

   function "=" (Left, Right : Peer_Info) return Boolean is
   begin
      return (Left.PeerID = Right.PeerID);
   end  "=";

end Peer_Informations;

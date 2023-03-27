--                              -*- Mode: Ada -*-
--  Filename        : peer_informations.ads
--  Description     : Informations associated to a Peer
--  Author          : Roberto Cesco Fabbro
--  Created On      : Wed May 05 15:00:00 2010
--  Last Modified By: .
--  Last Modified On: .
--  Update Count    : 0
--  Status          : Untested

with Network;			use Network;

with PPETP;			use PPETP;
with PPETP.Attributes;		use PPETP.Attributes;

package Peer_Informations is

   type Peer_Kind is (Lower_Peer, Upper_Peer, Other_Peer);

   type Peer_Info is
      record
         PeerID       : Peer_ID;
         Address      : Sock_Addr_Type;
         PeerKind     : Peer_Kind;
         Peer_Cred    : Access_Attribute_Class;
         Puncturing   : Access_Attribute_Class;
         Routing_Prob : Access_Attribute_Class;
         --* TODO signer :  Access_Signature_Class;
      end record;

   function "=" (Left, Right : Peer_Info) return Boolean;

end Peer_Informations;

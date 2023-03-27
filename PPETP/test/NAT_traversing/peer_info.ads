--                              -*- Mode: Ada -*-
--  Filename        : peer_info.ads
--  Description     : Type for identify a peer
--  Author          : Roberto Cesco Fabbro
--  Created On      : Gen, 27 2009
--  Last Modified By: .
--  Last Modified On: .
--  Update Count    : 0
--  Status          :

-- This package define some peer type used in all the other packege such the
-- server_db

with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with GNAT.Sockets; use GNAT.Sockets;
with Interfaces; use Interfaces;
with Random_Generator; use Random_Generator;

package Peer_Info is


   type Peer_Identifier is new Unsigned_32;
   subtype Peer_Address is Sock_Addr_Type;

   -- Generate a random Peer_Identifier
   function Random_Peer_ID return Peer_Identifier;

   -- These two functions are used to return a String from the two type passed
   -- as parameters
   function Image(Id: Peer_Identifier) return Unbounded_String;
   function Image(Addr: Peer_Address) return Unbounded_String;

end Peer_Info;

--                              -*- Mode: Ada -*-
--  Filename        : GNAT.Sockets.ppetp.ads
--  Description     : Socket-level package
--  Author          : Roberto Cesco Fabbro
--  Created On      :
--  Last Modified By: .
--  Last Modified On: .
--  Update Count    : 0
--  Status          : <UNTESTED>

--
-- This package is simply  extends GNAT.Sockets to access to
-- internal, vectored, rappresentation of the Inet_Addr_Type
-- and convert it in a 32 bit number
--

with Interfaces; use Interfaces;

package GNAT.Sockets.ppetp is
   subtype byte is natural range 0..255;
   type Inet_Buffer is array(1..4) of byte;


   -- Return an array of 4 bytes from the IP address
   function Inet_To_Array(Addr: Inet_Addr_Type) return Inet_Buffer;

   -- Return a 32 bit number that correspond at the IP address
   function To_Int_32(Addr: Inet_Addr_Type) return Unsigned_32;

end GNAT.Sockets.ppetp;

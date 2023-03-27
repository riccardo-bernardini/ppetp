--                              -*- Mode: Ada -*-
--  Filename        : network_utilities.ads
--  Description     : Conversion between array of bytes and IP addresses
--  Author          :
--  Created On      : Mon Oct 20 11:21:48 2008
--  Last Modified By: .
--  Last Modified On: .
--  Update Count    : 0
--  Status          : <TESTED>

--
-- This package provides few functions useful to convert between
-- arrays of bytes and IP addresses.  It escapes my immagination
-- why the GNAT library already does not include these functions...
--
-- The functions can handle both IPv4 and IPv6 addresses types,
-- but currently GNAT does not support IPv6.
--

with Network;     use Network;
with Interfaces;  use Interfaces;

package Network_Utilities is
   Invalid_IP_Address : exception;

   function "<" (Left, Right : Sock_Addr_Type) return Boolean;
   function "=" (Left, Right : Sock_Addr_Type) return Boolean;

   subtype Byte is Unsigned_8;
   type Inet_Addr_VN_Buffer is array (Positive range <>) of Byte;

   subtype Inet_Addr_V4_Buffer is Inet_Addr_VN_Buffer (1 .. 4);
   subtype Inet_Addr_V6_Buffer is Inet_Addr_VN_Buffer (1 .. 16);

   -- Map a vector of byte to an IP address
   function Inet_Addr (Buffer : Inet_Addr_VN_Buffer)
                       return Inet_Addr_Type;

   -- Like Inet_Addr of GNAT.Sockets, but it converts
   -- the generic "Socket_Error" exception of the GNAT package
   -- to the more specific Invalid_IP_Address.  It is helpful
   -- in several situations.
   function To_Inet_Addr (S : String)
                         return Inet_Addr_Type;

   -- Map a vector of byte to a string in dot notation
   function To_Dot_Notation (Buf : Inet_Addr_VN_Buffer)
                             return String;

   -- Map an IP address to a vector of bytes
   function To_Array (Addr : Inet_Addr_Type)
                      return Inet_Addr_VN_Buffer;

end Network_Utilities;

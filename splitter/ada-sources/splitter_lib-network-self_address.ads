--
-- *******************
-- ** What is this? **
-- *******************
--
-- This package provides a function that returns the IP address associated
-- with a given interface
--
with Splitter_Lib.BSD_Sockets;
with Ada.Strings.Unbounded;                  use Ada.Strings.Unbounded;

package Splitter_Lib.Network.Self_Address is
   type IP_Interface is
      record
         Name : Unbounded_String;
         Addr : Inet_Addr_Type;
      end record;

   type IP_Configuration is array (Natural range <>) of IP_Interface;

   -- Return a vector representing the network configuration of the
   -- current host
   -- function IP_Config return IP_Configuration;

   --
   -- Return the IP address associated with the interface whose name
   -- is given as parameter. Special cases
   --
   --     * If Iface is "" (the default choice) the function loops over
   --       the available interface names and it returns the IP address
   --       of the first interface whose name is not "lo".
   --
   --     * If Iface is "localhost" or "lo", then 127.0.0.1 is returned.
   --
   function My_Address (Iface : String := "")
                        return BSD_Sockets.Inet_Addr_Type;

   function Addr_Inet_Of (Net_Interface : String)
                          return Inet_Addr_Type;

end Splitter_Lib.Network.Self_Address;

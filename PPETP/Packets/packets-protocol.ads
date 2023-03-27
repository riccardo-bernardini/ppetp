--                              -*- Mode: Ada -*-
--  Filename        : packets-protocol.ads
--  Description     : Basic definitions for protocol packets
--  Author          : Riccardo Bernardini
--  Created On      : Wed Nov  5 09:37:34 2008
--  Last Modified By: .
--  Last Modified On: .
--  Update Count    : 0
--  Status          : Unknown, Use with caution!

--
-- This package provides the definitions relative to "parsed packets,"
-- that is, the data structures obtained by parsing the packets received
-- over the network.  The parsed packets are organized in a hierarchy
-- so that it is possible to have, for example, "array of parsed
-- packets" by using class-wide types.
--
-- Currently, we have the following hierarchy
--
--    Protocol_Packet
--          |
--          |
--          +--- Data_Packet
--          |
--          +--- Control_Packet
--
--
--    * "Root" type Protocol_Packet which collects the characteristics
--       which are common to all the types of parsed packets, that is,
--       the profile, the timestamp and the address of the remote
--       peer which sent us the packet.
--
--   * Data_Packet is the internal correspondent of a... well...
--     data packet :-).  Besides the field of ancestor Protocol_Packet
--     it holds the Entangled data read from the packet
--
--   * Control_Packet is the internal correspondent of a command.  Besides
--     the fields inhereted from Protocol_Packet it has a field Command which
--     specifies the actual command and other fields which depends on
--     the value of Command.
--

with Network;                    use Network;
with Packets.Binary.Network;     use Packets.Binary.Network;
with PPETP;

package Packets.Protocol is
   -- Old stuff?
   -- ** MbO ** type Stream_Element_Array_Pt is
   -- ** MbO **  access Stream_Element_Array;
   -- ** MbO ** type Default_Array is array (Profile_Type) of Parameters_Class_Pt;

   -------------------------------------
   -- Root of every "protocol packet" --
   -------------------------------------

   type Protocol_Packet is abstract tagged
      record
--         Sequence_Num : PPETP.Sequence_Number;
         Address      : Network.Sock_Addr_Type;   --- ????????
      end record;

   -- Return true if Packet is a control packet, false if it
   -- is a data packet.
   function Is_Command (Packet : Network_Packet)
                       return Boolean;
   pragma Inline(Is_Command);


   -- Raised from parser when the parsed packet does not satisfy
   -- the required syntax
   Invalid_Packet        : exception;

   -- Raised when the parser for data packet is called with a
   -- command packet (and viceversa)
   Invalid_Packet_Type   : exception;

   -- Raised when trying to use a profile which is not implemented
   -- yet
   Unimplemented_Profile : exception;

   -- Some parsing/building can be done even if no profile has
   -- been specified.  If the packet requires a profile for its
   -- parsing/building, but no profile has been specified, this
   -- exception is raised.
   Profile_Required      : exception;

   Invalid_Profile : exception;
   -- Useful for debugging
   procedure Print (A : Protocol_Packet);
   procedure Free(Object: in out Protocol_Packet) is abstract;

end Packets.Protocol;

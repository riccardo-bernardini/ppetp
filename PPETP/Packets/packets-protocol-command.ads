--                              -*- Mode: Ada -*-
--  Filename        : packets-protocol-command.ads
--  Description     : Definition of command packet
--  Author          :
--  Created On      : Mon Nov  3 18:19:19 2008
--  Last Modified By: Roberto Cesco Fabbro
--  Last Modified On: Thu Feb  5 11:45:05 2009
--  Update Count    : 0
--  Status          : Unknown, Use with caution!

--
-- This package provides resource for building and parsing PPETP
-- control packets.  The most important provided resources are
--
--   * type Control_Packet: a record representing a control packet
--
--   * Parse_Command_Packet: to convert a received packet to a
--      Control_Packet (derived from Protocol_Packet)
--
--   * Make_Control_Packet: to convert a Control_Packet to a
--      bitstring.
--
-- All the knowledge about the internal structure of a control packet is
-- stored inside this package.
--

with Network;                   use Network;
with Packets.Binary.Network;    use Packets.Binary.Network;
with Auth.Credentials;          use Auth.Credentials;
with Byte_Arrays;               use Byte_Arrays;
with Profiles.Parameters;       use Profiles.Parameters;

with PPETP.Attributes;		use PPETP.Attributes;




package Packets.Protocol.Command is
   -- Internal correspondent of a Control packet --
   ------------------------------------------------

   type Request_Type is (Hello,
                         Set_Default,
                         Acknowledge,
                         Data_Control,
                         Forward); -- this is used for internal purpose

   for Request_Type'Size use 4;
   for Request_Type use (Hello                => 0,
                         Set_Default          => 1,
                         Acknowledge          => 2,
                         Data_Control         => 3,
                         Forward              => 15);

   type ACK_Reason_Type is (OK,
                            No_Resource,
                            No_Replay,
                            No_Target);
   for  ACK_Reason_Type'Size use 8;
   for  ACK_Reason_Type use (OK 	 => 0,
                             No_Resource => 1,
                             No_Replay	 => 2,
                             No_Target	 => 3);

   type Data_Control_Sub_Command is(Start,
                                    Stop,
                                    Redirect,
                                    Punch);
   for Data_Control_Sub_Command'Size use 8;
   for Data_Control_Sub_Command use (Start    => 0,
                                     Stop     => 1,
                                     Redirect => 2,
                                     Punch    => 3);


   type Request_Flags is new Natural range 0 .. 2**8-1;

   --
   -- Type of request in a Control packet.
   --

   type Control_Packet (Command : Request_Type) is
     new Protocol_Packet with
      record
         Sequence_Num : PPETP.Command_Sequence_Number;
         Sub_Seq_Num  : PPETP.Sub_Sequence_Number;

         -- This field is used only for Routed Packets
         ACK_Target:	  Sock_Addr_Type;


         case Command is
            when Hello =>
               H_Peer_Credential : Access_Attribute_Class;

            when Set_Default =>
               Chann_Def     : PPETP.Channel_ID;
               Default       : Byte_Array_Pt;

            when Acknowledge =>
               ACKed_Number     : PPETP.Command_Sequence_Number;
               ACKed_Sub_Number : PPETP.Sub_Sequence_Number;
               ACK_Reason       : ACK_Reason_Type;

            when Data_Control =>
               SC      : Data_Control_Sub_Command;
               Param_1 : Byte;
               Param_2 : Byte;
               Param_3 : Byte;
							   -- Used by: [optionally]
               D_New_Peer        : Access_Attribute_Class; -- Start, Redirect
               D_Peer_Credential : Access_Attribute_Class; -- [Start, Redirect]
               D_Puncturing      : Access_Attribute_Class; -- [Start, Redirect]
               D_Routing_Prob    : Access_Attribute_Class; -- [Start, Redirect]
               D_Old_Peer        : Access_Attribute_Class; -- Stop, Redirect
               D_NAT_Param       : Access_Attribute_Class; -- [Punch]
            when Forward =>
               SourceID : PPETP.Peer_ID;
               Data     : Byte_Array_Pt;
         end case;
      end record;

   type Control_Packet_Pt is access Control_Packet;

   overriding
   procedure Print (A : Control_Packet);

   function Is_Command (Packet : Network_Packet)
                        return Boolean;

   procedure Free(Object: in out Control_Packet);

end Packets.Protocol.Command;

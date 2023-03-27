--                              -*- Mode: Ada -*-
--  Filename        : input_data.ads
--  Description     : Type definitions related to the input command queue
--  Author          : Finta Tartaruga
--  Created On      : Thu Feb 14 21:06:52 2008
--  Last Modified By: .
--  Last Modified On: .
--  Update Count    : 0
--  Status          : Unknown, Use with caution!
with Generic_Protected_Queue;

--
-- This package provides the type Input_Packet which represents
-- the data that can be received from the network.
-- There are two types of packets that can be recived: commands
-- (such as: "send stream ... to ..." and network crumbs (with
-- multimedia data).
--
-- This package also provides the type of a queue of packets and
-- a "constructor" function which converts array of bytes into
-- Input_Packet.
--
package Input_Data is
   --
   -- As said, there are two possibile "classes" of
   -- Input_Packet. In order to avoid code duplications,
   -- we use a single type with a discriminant.
   --

   type Input_Packet_Class is (Command, Crumb);

   type Input_Packet(Class : Input_Packet_Class) is
      record
         case Class is
            when Command =>
              Command : Unbounded_String;
            when Crumb =>
               Data : Network_Crumb;
         end case;
      end record;

   type Input_Packet_Access is access Input_Packet;

   --
   -- The queue of command packets is a protected queue
   --
   package Packet_Protected_Queue is
      new Generic_Protected_Queue (Input_Packet);

   subtype  Command_Queue_Type is Packet_Protected_Queue.Queue_Type;
   use type Command_Queue_Type;

   type Command_Queue_Access is access all Command_Queue_Type;

   --
   -- Functions to convert from Stream_Element_Array's to Input_Packet's.
   -- Used in the instatation of Generic_Reader.
   --
   function To_Input_Packet (Data  : Ada.Streams.Stream_Element_Array;
                             Class : Input_Packet_Class)
     return Input_Packet_Array;
end Input_Data;


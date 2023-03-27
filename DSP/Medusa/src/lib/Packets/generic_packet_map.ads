--                              -*- Mode: Ada -*-
--  Filename        : generic_packet_map.ads
--  Description     : Generic map index <--> packets
--  Author          : Finta Tartaruga
--  Created On      : Sat Aug 25 18:46:25 2007
--  Last Modified By: .
--  Last Modified On: .
--  Update Count    : 0
--  Status          : Unknown, Use with caution!

--
-- The key operations in MEDUSA are packet splitting/merging.  More
-- precisely,
--
--    * Multimedia packets are split into uniform analogic/binary
--      packets
--
--    * Network packets are split into uniform coded packets
--
--    * Analogic and Binary packets are split into reduced
--      packets
--
-- With some ASCII art:
--
--                  Multimedia
--                      |
--                      V
--                      |
--                      S----------------------+
--                      |                      |
--                      V                      V
--                      |                      |
--                  Analogic                Binary
--                      |                      |
--              +-------S-----+         +------S------+
--              :       |     :         :      |      :
--              :   Reduced   :         :   Reduced   :
--                  Analogic                Binary
--                      |                      |
--                      |                      |
--                    Coded                  Coded
--                  Analogic                Binary
--             (Uniform Network)       (Uniform Network)
--                      |                      |
--                      |                      |
--                      +--->------M-----<-----+
--                                 |
--                                 |
--                              Network
--
-- The picture is to be read from top to bottom.  Each "S" node denotes a
-- splitting, each "M" node denotes a merge.  Of course, if the picture
-- is read bottom to top each "M" node becomes an "S" node and viceversa.
--
-- Each type of packet has an "index" which denotes its position in
-- the packet flow. Each time a packet is split, each piece carries
-- the information about its position in the "parent packet" (we
-- will call that information "portion").  Note that the reduced
-- packet have 3 indexes: the timestamp inherited by their multimedia
-- grandparent, the subtime inherited by their "oxidized" packet
-- and their own portion.
--
-- It is clear that in order to reconstruct the parent packet, the
-- merge node must receive all the original pieces. [Actually,
-- this is not enterely true since in some cases the analogic/binary
-- mergers (also known as "oxidizers") can reconstruct the original
-- packet without the whole set of pieces]
--
-- The procedures provided by this package handle a table which
-- can be used to hold the packets in the merge nodes.
-- The model of the tables defined in this package is the follow
--
--    * Each packet is uniquely determined by two values: an
--      _index_ and a _portion_
--
--    * The index is any type with a "<" relation, the portion
--      is a positive number
--
--    * The maximum number of packets with the same index is
--      fixed and given to the table at the initialization phase
--
--    * The table keeps track of how many packet with the same
--      index have been stored in the table
--
--    * It is an error to insert two packet with the same
--      pair (index, portion)
--
--
--

with Ada.Containers.Ordered_Maps;

generic
   type Index_Type  is private;
   -- The type of the "main index" of the packets

   with function "<" (Left, Right : Index_Type) return Boolean is <>;
   -- Index_Type must be an ordered type

   type Portion_Type is range <>;
   -- Type of the "portion index".  It must be a subset of Integer

   type Portion_Array is
     array(Positive range <>) of Packet_Type;

   type Packet_Type is private;
   -- The type of packets to be stored in the table

   type Packet_Vector is
     array(Portion_Type range <>) of Packet_Type;
package Generic_Packet_Map is
   type Packet_Table is private;


   -- Initialize the table
   procedure Init   (Table      : in out Packet_Table;
                     N_Portions : in     Portion_Type := Portion_Type'Last);

   -- Insert the packet relative to the pair (Index, Portion)
   -- in the table.
   procedure Insert (Table    : in out Packet_Table;
                     Index    : in     Index_Type;
                     Packet   : in     Packet_Type;
                     Portion  : in     Portion_Type;
                     Inserted :    out Positive);

   -- The previous version of Insert is called when each packet has
   -- a "natural" portion index associated with it.  Sometimes we
   -- are just interested in collecting N_Portions packets, but every
   -- packet has not a "natural" portion index.  In this case we can call
   -- this version of Insert which assigns the first unused portion index
   -- to the new packet.  Exception Table_Overflow is raised if more than
   -- N_Portions packets with the same index are inserted.
   procedure Insert (Table     : in out Packet_Table;
                     Index     : in     Index_Type;
                     Packet    : in     Packet_Type;
                     Inserted  :    out Positive);

   exception Table_Overflow;

   -- Return True if __at least__ one packet with index equal to
   -- Index was received.
   function Contains (Table : in Packet_Table;
                      Index : in Index_Type) return Boolean;

   -- Return True if the packet with index equal to Index
   -- and portion equal to Portion was received.
   function Contains (Table   : Packet_Table;
                      Index   : Index_Type;
                      Portion : Portion_Type) return Boolean;

   -- Return an array with the sub-indeces of the packets with
   -- index equal to Index which are yet to be received.
   function Missing (Table : Packet_Table;
                     Index : Index_Type) return Portion_Array;

   -- Return the number of packets with main index Index received
   function N_Stored (Table : in  Packet_Table;
                      Index : in Index_Type) return Natural;



   -- Return a vector of the received packets. Return an empty array
   -- if no packet with the given "main index" was received.
   function Get (Table : in Packet_Table;
                 Index : in Index_Type) return Packet_Vector;

   -- Return the received packets with given "index" and "portion"
   function Get (Table   : in Packet_Table;
                 Index   : in Index_Type;
                 Portion : in Portion_type) return Packet_Type;

   -- Remove from the table _all_ the packets with main index
   -- equal to Index.
   procedure Remove (Table : in out Packet_Table;
                     Index : in     Index_Type);

private
   --
   -- A Packet_Table is implemented as a table (currently implemented
   -- with an Ordered_Map) which associates each main index to a vector
   -- of received packets.  Actually, since the packets can arrive
   -- in any order, we need to keep track of which packets were received.
   -- We do that with an array of Boolean's.
   --
   type Flag_Vector    is array(Portion_Type range <>) of Boolean;
   type Flag_Vector_Pt is access Flag_Vector;

   type Packet_Vector_Pt is access Packet_Vector;

   --
   -- Entry of the Ordered_Map
   --
   type Packet_Buffer is record
      Packet_Stored  : Natural;           -- N. pacchetti ricevuti
      Buffer         : Packet_Vector_Pt;  -- Vettore pacchetti ricevuti
      Received       : Flag_Vector_Pt;
   end record;

   type Packet_Buffer_Pt is access Packet_Buffer;


   -- Ordered_Map used for the Index -> Packets association
   package Pkt_Maps is
      new Ada.Containers.Ordered_Maps(Key_Type     => Index_Type,
                                      Element_Type => Packet_Buffer_Pt);

   -- Finally, we can give the complete definition of
   -- the Packet_Table type
   type Packet_Table is record
      N_Packets : Integer := -1;
      Table : Pkt_Maps.Map;
   end record;

   subtype Packet_Cursor is Pkt_Maps.Cursor;
end Generic_Packet_Map;

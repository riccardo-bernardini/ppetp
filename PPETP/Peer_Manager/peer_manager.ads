--                              -*- Mode: Ada -*-
--  Filename        : peer_manager.ads
--  Description     : Manager of the Peers
--  Author          : Roberto Cesco Fabbro
--  Created On      : Wed May 05 14:00:00 2010
--  Last Modified By: .
--  Last Modified On: .
--  Update Count    : 0
--  Status          : Untested

with PPETP;		use PPETP;
with Network;		use Network;
with Ada.Containers.Indefinite_Ordered_Maps;

with Network_Utilities;		use Network_Utilities;

with PPETP.Attributes;	use PPETP.Attributes;
with Generic_Tables;
with Peer_Informations;		use Peer_Informations;

package Peer_Manager is


   type Peer_Manager_Handler is tagged limited private;

   type Peer_Manager_pt is access Peer_Manager_Handler;


   -- Add a new peer to the database
   procedure Add_Peer(PM:           in out Peer_Manager_pt;
                      PeerID:       in     Peer_ID;
                      Address:      in     Sock_Addr_Type;
                      PeerKind:     in     Peer_Kind;
                      Peer_Cred:    in     Access_Attribute_Class;
                      Puncturing:   in     Access_Attribute_Class;
                      Routing_Prob: in     Access_Attribute_Class;
                      --* TODO signer in  Access_Signature_Class;
                      Result:          out Boolean
                     ) ;

   -- Check if a peer is in the Peer_Manager
   function Is_Present(PM:     in Peer_Manager_pt;
                       PeerID: in Peer_ID) return Boolean;

   function Is_Present(PM:   in Peer_Manager_pt;
                       Addr: in Sock_Addr_Type) return Boolean;

   -- Remove a peer from the database
   procedure Remove_Peer(PM:      in out Peer_Manager_pt;
                         PeerID : in     Peer_ID;
                         Result :    out Boolean);

   -- Get the address of a peer
   function Get_Addr(PM:      Peer_Manager_pt;
                     PeerID : Peer_ID) return Sock_Addr_Type;


   -- Get the Peer_ID of a peer from its Address
   function Get_PeerID(PM:   Peer_Manager_pt;
                       Addr: Sock_Addr_Type) return Peer_ID;

   -- Get the credentials of a peer
   function Get_Credential(PM:      Peer_Manager_pt;
                           PeerID : Peer_ID) return Access_Attribute_Class;

   -- Get the puncturing informations of a peer
   function Get_Puncturing(PM:      Peer_Manager_pt;
                           PeerID : Peer_ID) return Access_Attribute_Class;

   -- Return True if a Control_Packet should be routed to a peer
   function Transmit_Routing_Packet(PM:      Peer_Manager_pt;
                                    PeerID : Peer_ID) return Boolean;


   -- Iteration functions for Routing Packets
   procedure Start_Iteration(PM: Peer_Manager_pt);
   function  Iterate_Again(PM: Peer_Manager_pt) return Boolean;
   procedure Next_Iteration(PM: Peer_Manager_pt;
                            Address : out Sock_Addr_Type;
                            Tx      : out Boolean);

   -- Call before use use the Peer Manager to set the Max number of peer
   procedure Initialize(PM: Peer_Manager_pt;
                        Max_Size : in Natural);


   procedure Image(PM: Peer_Manager_pt);
   --* TODO  funzione che firma direttamente i pacchetti

private

   package Peer_ID_Cursor_Map is
     new Ada.Containers.Indefinite_Ordered_Maps (Key_Type     => Peer_ID,
                                                 Element_Type => Natural);

   package Address_Cursor_Map is
     new Ada.Containers.Indefinite_Ordered_Maps (Key_Type     => Sock_Addr_Type,
                                                 Element_Type => Natural);


   package Peer_Info_Table is
     new  Generic_Tables(Element => Peer_Info,
                         Cursor  => Natural);

   type Peer_Manager_Handler is tagged limited
      record
         ID_Map     : Peer_ID_Cursor_Map.Map;
         Addr_Map   : Address_Cursor_Map.Map;
         Info_Table : Peer_Info_Table.Table;
      end record;

end Peer_Manager;

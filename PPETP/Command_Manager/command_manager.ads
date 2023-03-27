--                              -*- Mode: Ada -*-
--  Filename        : command_manager.ads
--  Description     : Database of the executed and ack-ed commands
--  Author          : Roberto Cesco Fabbro
--  Created On      : Fri May 21 09:24:00 2010
--  Last Modified By: .
--  Last Modified On: .
--  Update Count    : 0
--  Status          : Untested



with Ada.Containers.Indefinite_Ordered_Maps;
with Ada.Containers.Indefinite_Doubly_Linked_Lists;

with PPETP;		use PPETP;

package Command_Manager is

   type Command_Action is (Packet_In_Execution, Packet_Too_Old, Packet_Too_Acked, Send_Ack, Execute);


   type Packet_Info is
      record
         Seq_Num : Command_Sequence_Number;
         Num_Ack : Integer;
      end record;

   package Packet_Info_List is
     new Ada.Containers.Indefinite_Doubly_Linked_Lists(Element_Type => Packet_Info);

   use Packet_Info_List;


   type Peer_Packets_Info is
      record
         Exe_List : Packet_Info_List.List;
         Ack_List : Packet_Info_List.List;
      end record;



   package Peer_ID_Cursor_Map is
     new Ada.Containers.Indefinite_Ordered_Maps (Key_Type     => Peer_ID,
                                                 Element_Type => Peer_Packets_Info);

   use  Peer_ID_Cursor_Map;



   protected type Command_Manager_Handler is






      -- Return the state or the action for this packet
      function Process_Command(PeerID  : in Peer_ID;
                               Seq_Num : in Command_Sequence_Number) return Command_Action;

      -- Indicate to the Command Manager that an ACK was sent for this packet (i.e. elaboration concluded)
      procedure Ack_Sent(PeerID  : in     Peer_ID;
                         Seq_Num : in     Command_Sequence_Number);

      -- Indicate to the Command Manager that we are Executing this packet; when the execution
      -- is been finished we must send an ACK
      procedure Set_Packet_In_Execution(PeerID  : in     Peer_ID;
                                        Seq_Num : in     Command_Sequence_Number);


      -- Print informations about the Command Manager
      procedure Image;


      procedure Set_Re_Ack(Value: in     Natural := 20);

      procedure Set_Max_Oldest(Value: in     Natural := 50000);


      procedure Set_Max_ACK(Value: in     Natural := 8);



   private


      List    : Peer_ID_Cursor_Map.Map;
      Max_ACK : Natural := 8;
      Re_Ack  : Natural := 20;
      Oldest  : Natural := 50000;


   end Command_Manager_Handler;

   type Command_Manager_pt is access Command_Manager_Handler;


end Command_Manager;

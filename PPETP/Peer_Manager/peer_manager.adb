with Ada.Text_IO;	use Ada.Text_IO;
with Ada.Streams;	use Ada.Streams;
with Ada.Numerics.Discrete_Random;

with PPETP;		use PPETP;
with Network;		use Network;
with byte_arrays;	use byte_arrays;

with Peer_Informations;		use Peer_Informations;

with PPETP.Attributes;				use PPETP.Attributes;
with PPETP.Attributes.Routing_Probability;	use PPETP.Attributes.Routing_Probability;
with PPETP.Attributes.Puncturing;		use PPETP.Attributes.Puncturing;



package body Peer_Manager is

   -- here we can use what we want, then the probability is
   -- calculated with a proportion
   MAX_RANDOM : constant Natural := 1000;

   type Random_Type is new Natural range 1 .. MAX_RANDOM;

   package Random_Routing is new Ada.Numerics.Discrete_Random (Random_Type);

   G : Random_Routing.Generator;


   --------------
   -- Add_Peer --
   --------------
   procedure Add_Peer(PM:           in out Peer_Manager_pt;
                      PeerID:       in     Peer_ID;
                      Address:      in     Sock_Addr_Type;
                      PeerKind:     in     Peer_Kind;
                      Peer_Cred:    in     Access_Attribute_Class;
                      Puncturing:   in     Access_Attribute_Class;
                      Routing_Prob: in     Access_Attribute_Class;
                      Result:          out Boolean ) is

      Info : Peer_Info := (PeerID       => PeerID,
                           Address      => Address,
                           PeerKInd     => PeerKind,
                           Peer_Cred    => Peer_Cred,
                           Puncturing   => Puncturing,
                           Routing_Prob => Routing_Prob);
      Cursor : Natural := 0;
   begin

      -- yet present
      if PM.ID_Map.Contains(Key => PeerID) then
         Result := False;
         return;
      end if;


      PM.Info_Table.Insert(Item  => Info,
                           Index => Cursor);


      PM.ID_Map.Insert(Key      => PeerID,
                       New_Item => Cursor);

      PM.Addr_Map.Insert(Key      => Info.Address,
                         New_Item => Cursor);

      Result := True;

   end Add_Peer;


   ----------------
   -- Is_Present --
   ----------------
   function Is_Present(PM:     in Peer_Manager_pt;
                       PeerID: in Peer_ID) return Boolean is
   begin
      return PM.ID_Map.Contains(Key => PeerID);
   end Is_Present;

   ----------------
   -- Is_Present --
   ----------------
   function Is_Present(PM:   in Peer_Manager_pt;
                       Addr: in Sock_Addr_Type) return Boolean is
   begin
      return PM.Addr_Map.Contains(Key => Addr);
   end Is_Present;

   -----------------
   -- Remove_Peer --
   -----------------
   procedure Remove_Peer(PM:      in out Peer_Manager_pt;
                         PeerID : in     Peer_ID;
                         Result :    out Boolean) is
      Cursor : Natural;
      Addr   : Sock_Addr_Type;
   begin
      if PM.ID_Map.Contains(Key => PeerID) then

         Cursor := PM.ID_Map.Element(Key => PeerID);
         Addr := Get_Addr(PM     => PM,
                          PeerID => PeerID);

         PM.ID_Map.Delete(Key => PeerID);
         PM.Addr_Map.Delete(Key => Addr);
         PM.Info_Table.Delete(Index => Cursor);

         Result := True;
         return;
      end if;

      Result := False;

   end Remove_Peer;

   --------------
   -- Get_Addr --
   --------------
   function Get_Addr(PM:      Peer_Manager_pt;
                     PeerID : Peer_ID) return Sock_Addr_Type is
      Cursor : Natural;
   begin
      if PM.ID_Map.Contains(Key => PeerID) then
         Cursor := PM.ID_Map.Element( Key => PeerID);
         return PM.Info_Table.Get(Cursor).Address;
      else
         return No_Sock_Addr;
      end if;
   end Get_Addr;



   ----------------
   -- Get_PeerID --
   ----------------
   function Get_PeerID(PM:   Peer_Manager_pt;
                       Addr: Sock_Addr_Type) return Peer_ID is
      Cursor : Natural;
   begin
      Cursor := PM.Addr_Map.Element(Key => Addr);
      return PM.Info_Table.Get(Index => Cursor).PeerID;
   end Get_PeerID;

   --------------------
   -- Get_Credential --
   --------------------
   function Get_Credential(PM:      Peer_Manager_pt;
                           PeerID : Peer_ID) return Access_Attribute_Class is
      Cursor : Natural;
   begin
      if PM.ID_Map.Contains(Key => PeerID) then
         Cursor := PM.ID_Map.Element( Key => PeerID);
         return PM.Info_Table.Get(Cursor).Peer_Cred;
      else
         return null;
      end if;
   end Get_Credential;

   --------------------
   -- Get_Puncturing --
   --------------------
   function Get_Puncturing(PM:      Peer_Manager_pt;
                           PeerID : Peer_ID) return Access_Attribute_Class is
      Cursor : Natural;
   begin
      if PM.ID_Map.Contains(Key => PeerID) then
         Cursor := PM.ID_Map.Element( Key => PeerID);
         return PM.Info_Table.Get(Cursor).Puncturing;
      else
         return null;
      end if;
   end Get_Puncturing;


   ----------------------
   -- Transmit_Routing --
   ----------------------
   function Transmit_Routing (Info : Peer_Info) return Boolean is
      Num  : byte_arrays.Byte;
      Den  : byte_arrays.Byte;
   begin

      -- Default behaviour
      if Info.Routing_Prob = null then
         return True;
      end if;


      Get_Attribute(Object => ROUTING_PROBABILITY_Attribute(Info.Routing_Prob.all),
                    Num    => Num,
                    Den    => Den);

      if Num = 0 then
         return False;
      end if;


      if Natural(Num) > Natural(Den) then
         return True;
      end if;

      -- Proportion for calculation of transmission probability
      if float(Random_Routing.Random(G)) < (float(MAX_RANDOM) * float(Num) / (float(Den)+1.0)) then
         return True;
      else
         return False;
      end if;

   end Transmit_Routing;

   -----------------------------
   -- Transmit_Routing_Packet --
   -----------------------------
   function Transmit_Routing_Packet(PM:      Peer_Manager_pt;
                                    PeerID : Peer_ID) return Boolean is
      Cursor : Natural;
      Info : Peer_Info;

   begin
      if PM.ID_Map.Contains(Key => PeerID) then

         Cursor := PM.ID_Map.Element( Key => PeerID);
         Info := PM.Info_Table.Get(Cursor);

         return Transmit_Routing(Info);
      else
         return False;
      end if;

   end  Transmit_Routing_Packet;


   ---------------------
   -- Start_Iteration --
   ---------------------
   procedure Start_Iteration(PM: Peer_Manager_pt) is
   begin
      PM.Info_Table.Start_Iteration;
   end Start_Iteration;

   -------------------
   -- Iterate_Again --
   -------------------
   function Iterate_Again(PM: Peer_Manager_pt) return Boolean is
   begin
      return PM.Info_Table.Iterate_Again;
   end Iterate_Again;

   --------------------
   -- Next_Iteration --
   --------------------
   procedure Next_Iteration(PM: Peer_Manager_pt;
                            Address : out Sock_Addr_Type;
                            Tx      : out Boolean) is
      Info : Peer_Info;
   begin
      PM.Info_Table.Next_Iteration(Item => Info);
      Address := Info.Address;

      -- Transmit Routed Packet only to Lower_Peer
      if Info.PeerKind = Lower_Peer then
         Tx := Transmit_Routing(Info);
      else
         Tx := False;
      end if;
   end Next_Iteration;

   ----------------
   -- Initialize --
   ----------------
   procedure Initialize(PM: Peer_Manager_pt;
                        Max_Size : in Natural) is
   begin
      PM.Info_Table.Resize(Max_Size);
   end Initialize;


   -----------
   -- Image --
   -----------
   procedure Image(PM: Peer_Manager_pt) is
      Tmp: Peer_Info;
   begin
--      Put_Line("** Element: " & PM.Info_Table.Count'img);
      PM.Info_Table.Start_Iteration;

      while PM.Info_Table.Iterate_Again loop
         PM.Info_Table.Next_Iteration(Tmp);
         Put_Line("PeerID  : " & Tmp.PeerID'img);
         Put_Line("Address : " & Image(Tmp.Address));
         Put_Line("PeerKind: " & Tmp.PeerKind'img);
         New_Line;
      end loop;

   end Image;

begin
   Random_Routing.Reset(G);


end Peer_Manager;

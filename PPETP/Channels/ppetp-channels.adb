with Generic_Tables.Two_Params;

with Target_Tables;               use Target_Tables;
with Packets.Protocol.Building;   use Packets.Protocol.Building;
with Packets.Protocol;            use Packets.Protocol;
with Packets.Protocol.Data;       use Packets.Protocol.Data;
with Profiles.Entangled;          use Profiles.Entangled;
with Packets.Binary.Network;      use Packets.Binary.Network;


-- only for printing
with Ada.Text_IO;				use Ada.Text_IO;
with Ada.Characters.Latin_1;		use Ada.Characters.Latin_1;
with PPETP.Targets;				use PPETP.Targets;
with Ada.Exceptions; use Ada.Exceptions;

with Network;		use Network;


package body PPETP.Channels is

   -- Used for delete a Target
   Addr_To_Del: Network.Sock_Addr_Type;

   -- Used for delete a Target
   function Compare_Addr(X : PPETP.Targets.Target) return Boolean is
   begin
      if X.Address = Addr_To_Del then
         return true;
      else
         return false;
      end if;

   end Compare_Addr;



   -----------------
   -- New_Channel --
   -----------------

   function New_Channel (Channel_Num  : PPETP_Channel_ID;
                         Profile      : Profile_Type;
                         Parameters   : Parameters_Class_Pt;
                         Output_Queue : Network_Packet_Queues.Queue_Pt)
                        return Channel is
      Result : Channel;
   begin
      Result := new Channel_Handler'(Channel	  => Channel_Num,
                                     Profile      => Profile,
                                     Targets      => new Table,
                                     Entangler    =>
                                       New_Entangler (Profile, Parameters),
                                     Output_Queue => Output_Queue);

      Result.Targets.Resize(128);
      return Result;
   end New_Channel;

   ----------------
   -- New_Target --
   ----------------

   function New_Target (CH    : Channel_Handler;
                        Peer  : Network.Sock_Addr_Type;
                        Punct : Access_Attribute_Class)
                       return Target_Id
   is
      New_Id : Target_Id;
      Target : PPETP.Targets.Target;
      Present: Boolean := False;
   begin

      -- If the target is yet in the channel raise a Target_Yet_Present exception

      if not CH.Targets.Is_Empty then
         CH.Targets.Start_Iteration;

         Find_Target_Loop:
         while CH.Targets.Iterate_Again loop

            CH.Targets.Next_Iteration(Target);

            if Target.Address = Peer then
               Present := True;
            end if;

         end loop Find_Target_Loop;
      end if;

      if not Present then
         CH.Targets.Reserve(Index => New_ID);

         CH.Targets.Replace (Index    => New_ID,
                             New_Item => New_Target (Peer,CH.Output_Queue, Punct));
         return New_ID;
      else
         raise Target_Yet_Present;
      end if;

   end New_Target;

   -------------------
   -- Delete_Target --
   -------------------
   function Delete_Target(CH   : Channel_Handler;
                          Addr : Network.Sock_Addr_Type) return Boolean is

      Target : PPETP.Targets.Target;
      Found  : Boolean := False;
   begin

      -- set a package global variable used by the function
      -- Compare_Addr to comparate the target address and the
      -- passed address
      Addr_To_Del := Addr;

      CH.Targets.Start_Iteration;

      Find_Target_Loop:
      while CH.Targets.Iterate_Again loop

         CH.Targets.Next_Iteration(Target);

         if Target.Address = Addr then
            Target.Close; -- Close the target

            -- Delete the object from the table
            CH.Targets.Delete_If(Compare_Addr'Access);
            Found := True;
         end if;

      end loop Find_Target_Loop;

      return Found;
   end Delete_Target;


   ------------------
   -- Target_Exist --
   ------------------
   function Target_Exist(CH   : Channel_Handler;
                         Addr : Network.Sock_Addr_Type) return Boolean is
      Target : PPETP.Targets.Target;
   begin
      CH.Targets.Start_Iteration;

      Find_Target_Loop:
      while CH.Targets.Iterate_Again loop

         CH.Targets.Next_Iteration(Target);

         if Target.Address = Addr then
            return True;
         end if;

      end loop Find_Target_Loop;

      return False;
   end Target_Exist;


   ----------------------
   -- Set_Target_Ready --
   ----------------------

   procedure Set_Target_Ready (CH        : Channel_Handler;
                               Target    : Target_Id)
   is
      Tg : PPETP.Targets.Target := CH.Targets.Get (Target);
   begin
      Tg.Switch_To_Ready;
      CH.Targets.Replace (Index    => Target,
                          New_Item => Tg);
   end Set_Target_Ready;

   -----------------
   -- Actual_Send --
   -----------------

   procedure Actual_Send (Dst  : PPETP.Targets.Target;
                          Item : Network_Packet;
                          Seq_Num : Data_Sequence_Number) is
   begin
      Dst.Send(Item, Seq_Num);
   end Actual_Send;



   -----------------
   -- Send_Packet --
   -----------------

   procedure Send_Packet (CH     : Channel_Handler;
                          Packet : Application_Packet) is

      package Double is
        new Target_Tables.Two_Params (Network_Packet, Data_Sequence_Number);

      Ent_Buffer : Raw_Data;

   begin

      if CH.Targets.Count /= 0 then
         --  Put_Line("Channel: Send Data");
         CH.Entangler.Entangle (Input  => Packet,
                                Result => Ent_Buffer);




         -- L'entangler dovrebbe restituire un Raw_Data


         declare
            Data_Buffer : Data_Packet := (StreamID     => Packet.StreamID,
                                          Channel      => CH.Channel,
                                          Sequence_Num => Packet.Sequence_Num,
                                          Address      => Network.No_Sock_Addr,
                                          Payload      => Ent_Buffer);


            Bin_Data    : Network_Packet :=
              Make_Packet (Source  => Data_Buffer);
         begin
            Double.Iterate (T     => CH.Targets.all,
                            Proc  => Actual_Send'Access,
                            Aux   => Bin_Data,
                            Again => Packet.Sequence_Num);
         end;

      end if;
   end Send_Packet;

   -----------------
   -- Get_Profile --
   -----------------
   function Get_Profile (CH : Channel_Handler) return Profile_Type is
   begin
   	return CH.Profile;
   end Get_Profile;

   --------------------
   -- Get_Parameters --
   --------------------
   function Get_Parameters(CH : Channel_Handler) return Parameters_Class_Pt is
   begin
      return CH.Entangler.Get_Default;
   end Get_Parameters;


   -----------
   -- Print --
   -----------

   procedure Print (CH : in Channel_Handler;
                    Tab_Num : in Natural) is
      Tg : PPETP.Targets.Target;

      procedure Print_Tabs(Tab_Num : in Natural) is
      begin
         for i in Natural range 1 .. Tab_Num loop
            Put(Ada.Characters.Latin_1.HT);
         end loop;
      end Print_Tabs;

      Param : Parameters_Class_Pt;
   begin


      declare
      begin

         Print_Tabs(Tab_Num);
         Put_Line("Channel: " & Ch.Channel'img);
         Print_Tabs(Tab_Num);
         Put_Line("Profile: " & Ch.Profile'img);
         Print_Tabs(Tab_Num + 1);
         Param := Ch.Entangler.Get_Default;
         Put_Line("Parameters: " & Image(Param.all));


         Print_Tabs(Tab_Num);
         Put_Line("Targets ->");

         if not CH.Targets.Is_Empty then

            CH.Targets.Start_Iteration;

            Print_Target_Loop:
            while CH.Targets.Iterate_Again loop
               CH.Targets.Next_Iteration(Tg);
               PPETP.Targets.Print(Tg, Tab_Num + 1);
               New_Line;
            end loop Print_Target_Loop;
         else
            Print_Tabs(Tab_Num);
            Put_Line("Targets empty");
         end if;
      exception
         when e: others =>
            Put_Line("exception in Channels");
            Put_Line(Exception_Information(e));
      end;


   end Print;


end PPETP.Channels;

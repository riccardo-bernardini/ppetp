with PPETP.Channels;
with PPETP.Targets;
with Ada.Unchecked_Deallocation;
with Channel_Tables;  use Channel_Tables;
--with Generic_Tables.Two_Params;

-- for print only
with Ada.Text_IO;				use Ada.Text_IO;
with Ada.Characters.Latin_1;		use Ada.Characters.Latin_1;
with Ada.Exceptions; use Ada.Exceptions;

package body Transmitters is

   --------------------
   -- Output_Handler --
   --------------------

      protected body Output_Handler is
      -----------------
      -- New_Channel --
      -----------------

      procedure New_Channel (Profile : in     Profile_Type;
                             Details : in     Parameters_Class_Pt;
                             Output_Queue: in Network_Packet_Queues.Queue_Pt;
                             Channel :     in PPETP.PPETP_Channel_ID)
      is
         Internal_Channel : Channel_ID;
      begin
         Channel_Table.Reserve(Internal_Channel);
         Channel_Table.Replace (Index    => Internal_Channel,
                                New_Item =>
                                  Channels.New_Channel (Channel, Profile, Details, Output_Queue));

         Channel_List(Channel) := Internal_Channel;
      end New_Channel;

      ----------------
      -- New_Target --
      ----------------

      procedure New_Target (Channel : in     PPETP.PPETP_Channel_ID;
                            Address : in     Network.Sock_Addr_Type;
                            Punct   : in     Access_Attribute_Class;
                            Target  :    out Target_ID)
      is
         Internal_Channel : Channel_ID;
      begin

         Internal_Channel := Channel_List(Channel);
         Target := Channel_Table.Get(Internal_Channel).New_Target(Address, Punct);
      end New_Target;

      ------------------
      -- Target_Ready --
      ------------------
      procedure Target_Ready (Channel : PPETP.PPETP_Channel_ID;
                              Target  : Target_ID) is
         Internal_Channel : Channel_ID;
      begin
         Internal_Channel := Channel_List(Channel);
         Channel_Table.Get(Internal_Channel).Set_Target_Ready(Target);
      end Target_Ready;

      -------------------
      -- Delete_Target --
      -------------------
      function Delete_Target(Channel : PPETP.PPETP_Channel_ID;
                             Peer : Network.Sock_Addr_Type) return Boolean is
         Internal_Channel : Channel_ID;
      begin
         Internal_Channel := Channel_List(Channel);
         return Channel_Table.Get(Internal_Channel).Delete_Target(Peer);

      end Delete_Target;

      --------------
      -- Transmit --
      --------------

      procedure Transmit (Packet : Application_Packet) is
         Dst : PPETP.Channels.Channel;
      begin

    --     Put_Line("Transmitter: transmit");

         Channel_Table.Start_Iteration;

         while Channel_Table.Iterate_Again loop
            Channel_Table.Next_Iteration (Dst);
            Dst.Send_Packet(Packet);
         end loop;
      end Transmit;

      --------------
      -- Set_Size --
      --------------

      procedure Set_Size(Size: Natural) is
      begin
         Channel_Table.Resize(Size);
      end Set_Size;


      -----------------
      -- Get_Channel --
      -----------------
      procedure Get_Channel(Channel : in     PPETP.PPETP_Channel_ID;
                            Result  :    out PPETP.Channels.Channel) is
         Internal_Channel : Channel_ID;
      begin
         Internal_Channel := Channel_List(Channel);
         Result := Channel_Table.Get(Internal_Channel);
      end Get_Channel;

      -----------
      -- Print --
      -----------
      procedure Print(Tab_Num : in Natural) is
         procedure Print_Tabs(Tab_Num : in Natural) is
         begin
            for i in Natural range 1 .. Tab_Num loop
               Put(Ada.Characters.Latin_1.HT);
            end loop;
         end Print_Tabs;
      begin


         Print_Tabs(Tab_Num);
         Put_Line("Channels -> ");

         declare
            Ch: PPETP.Channels.Channel;
         begin
           if not Channel_Table.Is_Empty then
               Channel_Table.Start_Iteration;

               Print_Channel_Loop:
               while Channel_Table.Iterate_Again loop
                  Channel_Table.Next_Iteration(Ch);
                  PPETP.Channels.Print(Ch.all, Tab_Num + 1);
                  New_Line;
               end loop Print_Channel_Loop;

            else
               Print_Tabs(Tab_Num);
               Put_Line("Channels Empty");
            end if;

         exception
            when e: others =>
               Put_Line("exception in Transmitter");
               Put_Line(Exception_Information(e));
         end;

      end Print;


   end Output_Handler;


end Transmitters;

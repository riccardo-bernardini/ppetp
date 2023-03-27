with Input_Data;
with Generic_Reader;
with Medusa_State;
with Tcp_Server;
with Ada.Command_Line;
with Analog_Processor;
with Binary_Processor;
with Socket_Utility;

procedure Medusa is
   My_IP_Addresses : Socket_Utility.Inet_Addr_Array :=
     Socket_Utility.Get_My_IP_Addresses;

   package Command_Readers is
      new Generic_Reader(To_Command_Packet);

   package Data_Readers is
      new Generic_Reader(To_Crumb_Packet);

   type Reader_Table_Entry is record
      Free    : Boolean;
      Task_Pt : Command_Readers.Reader_Access;
   end record;

   Max_Readers  : constant Integer := 3;

   Reader_Table : array (1..Max_Readers) of Reader_Table_Entry :=
     (others => (Free => True, Task_Pt => null));

   procedure Process_Command(Command : Unbounded_String)
   is
   begin
      null;
   end Process_Command;

   -- Start_Command_Reader is called by the Command_Server when
   -- a new connection to the command port is received.  Its
   -- duty is to start a new task which will read commands
   -- from the new channel.
   procedure Start_Command_Reader (Socket       : Socket_Type;
                                   Peer_Address : Sock_Addr_Type)

   is
      -- Retur True if Addr is an IP addres which corresponds
      -- to localhost
      function Is_Address_Of_Mine(Addr : Sock_Addr_Type) return Boolean is
      begin
         for I in My_IP_Addresses'Range loop
            if (Addr = My_IP_Addresses(I)) then
               return True;
            end if;
         end loop;

         return False;
      end Is_Address_Of_Mine;

      Idx : Integer;
   begin
      -- Allow only connections from localhost
      if (not Is_Address_Of_Mine(Peer_Address)) then
         Shutdown_Socket(Socket);
         return;
      end if;

      -- Search for some room in the Reader Table.  Return to the caller
      -- as soon as you find it.
  Search_Free_Entry:
      for Idx in Reader_Table'Range loop
         if (Reader_Table(Idx).Free) then
            Reader_Table(Idx) := (Free    => False,
                                  Task_Pt => new Command_Readers.Reader
                                    (Socket, Medusa_State.Input_Queue'Access));
            return;
         end if;
      end loop Search_Free_Entry;

      -- If I am here,  Reader_Table is full
      Shutdown_Socket(Socket);
   end Start_Command_Reader;

   Command_Server : Server_Listener;
   Command_Port   : Port_Type := 54321;
   Data_Port      : Port_Type := 54322;

   Binary_Handler : Binary_Processor.Processor_Type;
   -- Analog_Handler : Analog_Processor.Processor_Type;

   DB_Port        : Port_Type;
   Central_DB     : DB_Connection;

   Data_Reader_Pt : Data_Readers.Reader_Access;
begin
   --
   -- Read the port number of the central DB from the command line
   -- and open the connection to the DB
   --
   if (Command_Line.Argument_Count /= 1) then
      Put_Line("Usage : " & Command_Line.Command_Name & " <db port>");
      Command_Line.Set_Exit_Status(Command_Line.Failure);
      return;
   end if;

   DB_Port := Port_Type'Value(Command_Line.Argument(1));
   Connect(Central_DB, Port);

   --
   -- Initialize the two processors (currently, only one)
   --
   Initialize (Processor       => Binary_Handler,
               Crumb_Dst       => To_Network'Access,
               Full_Dst        => To_Player'Access,
               Encode_Callback => Trivial_Encoder,
               Decode_Callback => Trivial_Decoder);

   --
   -- Start the command server.
   --
   Command_Server.Initialize (Port       => Command_Port,
                              Callback   => Start_Command_Reader,
                              Port_Fixed => False);

   --
   -- "Open" the UDP port used to receive the data from the
   -- others peers.
   --
   Open_Data_Port(Data_Socket, Data_Port);

   --
   -- Initialize the two synthesi queues
   --
   Medusa_State.Data_To_Peers.Initialize(3);
   Medusa_State.Data_To_Player.Initialize(3);

   declare
      --
      -- Read the command ports of the other modules
      --
      Player_Port : Integer := Get_Int(Connection => Central_DB,
                                       Var_Name   => "PLAYER.COMMAND_PORT");

      P2P_Port    : Integer := Get_Int(Connection => Central_DB,
                                       Var_Name   => "P2P.COMMAND_PORT");

      Root_Port   : Integer := Get_Int(Connection => Central_DB,
                                       Var_Name   => "ROOT.COMMAND_PORT");

      --
      -- Start the tasks which write to the other modules
      --
      Player_Writer : Command_Writers.Writer(Player_Port,
                                             Medusa_State.To_Player'Access);
      P2P_Writer    : Command_Writers.Writer(P2P_Port,
                                             Medusa_State.To_P2P'Access);
      Root_Writer   : Command_Writers.Writer(Root_Port,
                                             Medusa_State.To_Root'Access);

      --
      -- Start the taks which send packets to the peers
      --
      Data_Peer   : Output.Peer_Writer(Medusa_State.Data_To_Peers'Access);

      --
      -- Start the taks which send packets to the player
      --

      -- Note: player_data_port is a string and not an integer since
      -- it is possible that the player is listening on something
      -- different than an IP port, e.g., a named pipe.
      Player_Data_Port : String := Get(Connection => Central_DB,
                                       Var_Name   => "PLAYER.DATA_PORT");
      Data_Player : Output.Player_Writer(Medusa_State.Data_To_Player'Access,
                                         Player_Data_Port);

      --
      -- Start the taks which reads peer data.
      --
      Data_Reader : Data_Readers.Reader(Data_Socket,
                                        Medusa_State.Input_Queue'Access);
      Input : Input_Packet;
   begin
      --
      -- Write something about you to the central DB
      --
      Set(Connection => Central_DB,
          Var_Name   => "MMEDIA.COMMAND_PORT",
          Var_Value  => Integer(Command_Port));

      Set(Connection => Central_DB,
          Var_Name   => "MMEDIA.DATA_PORT",
          Var_Value  => Integer(Data_Port));

      --
      -- Ok, we are done with the initialization part.  Now we can begin
      -- receiving packets.
      --

  Main_Loop:
      loop
         Medusa_State.Input_Queue.Extract(Input);

         case (Input.Class) is
            when Command =>
               Process_Command(Input.Command);
            when Crumb =>
               case (Input.Data.Class) is
                  when Analog =>
                     Analog_Processor.Receive(Input.Data);
                  when Binary =>
                     Binary_Processor.Receive(Input.Data);
               end case;
         end case;
      end loop Main_Loop;
   end;
end Medusa;



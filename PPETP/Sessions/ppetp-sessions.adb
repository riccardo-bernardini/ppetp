
with Ada.Text_IO; 	use Ada.Text_IO;
with Text_IO;		use Text_IO;

with PPETP.Channels;
with PPETP.Targets;
with Transmitters;

with Command_Queues;   use Command_Queues;
with Network;          use Network;
with network_UDP;      use network_UDP;
with Channel_Tables;   use Channel_Tables;


with Main_Task;
with Control_Task;
with Output_Task;	use Output_Task;
with Ada.Unchecked_Deallocation;

with byte_arrays;	use byte_arrays;
with Ada.Streams;	use Ada.Streams;
with Common_Types; 	use Common_Types;

with Peer_Manager;	use Peer_Manager;
with Peer_Informations; use Peer_Informations;



with Ada.Numerics.Discrete_Random;




package body PPETP.Sessions is


   package Random_Sequence_Number is
     new Ada.Numerics.Discrete_Random (Data_Sequence_Number);

   --------------------
   -- Set_Ready_Flag --
   --------------------
   procedure Set_Ready_Flag(State : in out Sessions.State_Record) is
   begin

      Put_Line("*** Profile: " & State.Profile'img);
      Put_Line("*** Low_Level: " & State.Low_Level'img);

      State.Ready := State.Profile and State.Low_Level;
      Put_Line("*** Ready: " & State.Ready'img);
   end Set_Ready_Flag;


   -----------------------
   -- Set_ACK_Attribute --
   -----------------------
   procedure Set_ACK_Attribute(Parent : Session) is
   begin

      declare
         Addr: Sock_Addr_Type;
         Attribute: ACK_TARGET_Attribute;
      begin

         -- Ritorna l'indirizzo pubblico
--         Addr := Get_Socket_Name(Parent.In_Out_Socket.all);
         Addr.Addr := Parent.My_Address;
         Addr.Port := Get_Port(Parent.In_Out_Socket);

         Set_Attribute(Object     => Attribute,
                       Address    => Addr,
                       T_Protocol => 17,
                       PeerID     => Parent.My_PeerID.all);


         Parent.Control.Set_ACK_Target_Attribute(Attr => Attribute);

--           Put_Line("**** Addr: " & Image(Addr) & " *********");
--           Put_Line("**** PeerID: " & PeerID'img  & " *******");
      end;

   end Set_ACK_Attribute;


   --------------------------------
   -- Create_Input_Output_Socket --
   --------------------------------
   procedure Create_Input_Output_Sockets(Port          : in out Port_Type;
                                         In_Out_Socket : in out Socket_Access;
                                         Addr          : in     Inet_Addr_Type;
                                         N_Trials      : in     Natural;
                                         Success       :    out Boolean
                                        ) is
   begin

      if (Port = Any_Port) then

         -- in questa funzione restituisce la porta!
         Put_Line("** Session Search to: " & Image(Addr) & ": " & Port'img);
         Search_Port  (Port     => Port,
                       Input    => In_Out_Socket,
                       Success  => Success,
                       Addr     => Addr,
                       N_Trials => N_Trials);
      else

         Put_Line("** Session Bind to: " & Image(Addr) & ": " & Port'img);

         Bind_To_Port (Port     => Port,
                       Addr     => Addr,
                       Input    => In_Out_Socket,
                       Success  => Success);
      end if;

   end Create_Input_Output_Sockets;



   --------------------------------
   -- Close_Input_Output_Sockets --
   --------------------------------

   procedure Close_Input_Output_Sockets (In_Out_Socket : in out Socket_Access) is
   begin
      Close_Socket(In_Out_Socket.all);
   end Close_Input_Output_Sockets;









   -----------------
   -- New_Session --
   -----------------

   function New_Session (Ctl_Port     : in Port_Type := Any_Port;
                         My_Public_IP : in Inet_Addr_Type := No_Inet_Addr;
                         Profile      : in Profile_Type;
                         Prof_Parameters : in Parameters_Class_Pt;
                         PeerID          : in Peer_ID;
                         N_Trials        : in Natural := 1) return Session is

      Result        : Session;
      Commands      : Command_Queues.Queue_Pt := Command_Queues.New_Queue;
      Output_Queue  : Network_Packet_Queues.Queue_Pt := Network_Packet_Queues.New_Queue;

      In_out_Socket  : Socket_Access;
      Interprocess_Socket : Socket_Access;

      Success       : Boolean;
      Control_Port  : Port_Type := Ctl_Port;
      Internal_Port  : Port_Type := Any_Port;
      G		    : Random_Sequence_Number.Generator;



   begin


      Network.Initialize;


      -- Create the input/output socket
      Create_Input_Output_Sockets(Port          => Control_Port,
                                  In_Out_Socket => In_Out_Socket,
                                  Addr          => My_Public_IP,
                                  N_Trials      => N_Trials,
                                  Success       => Success);

      -- Create the Interprocess socket
      Create_Input_Output_Sockets(Port          => Internal_Port,
                                  In_Out_Socket => Interprocess_Socket,
                                  Addr          => Inet_Addr("127.0.0.1"),
                                  N_Trials      => N_Trials,
                                  Success       => Success);



      Result := new Session_Record'(Transmitter     => new Transmitters.Output_Handler,
                                    Commands        => Commands,
                                    Output_Queue    => Output_Queue,
                                    Main            => new Main_Task.Handler,
                                    Control         => <>,
                                    Output	    => <>,
                                    Input           => <>,
                                    Control_Port    => Control_Port,
                                    Profile         => Profile,
                                    Prof_Parameters => Prof_Parameters,
                                    Ready_Queue     => Application_Packet_Queues.New_Queue,
                                    In_Out_Socket   => In_Out_Socket,
                                    Sequence_Num    => <>,
                                    My_Address      => My_Public_IP,
                                    My_PeerID       => new Peer_ID'(PeerID),
                                    Peer_Manag      => new Peer_Manager.Peer_Manager_Handler,
                                    Inter_Proc_Socket => Interprocess_Socket,
                                    --My_Info_Attribute => <>,
                                    State           => <>,
                                    Default_StreamID => No_Stream_ID
                                   );


      --* TODO Set the right size
      Initialize(Result.Peer_Manag, 128);  --Max size of the Peer_Manager



      Random_Sequence_Number.Reset(G);
      -- Initialize a random Sequence_Number for every Stream_ID
      for i in Stream_ID'range loop
         Result.Sequence_Num(i) := 5000;--Random_Sequence_Number.Random(G);
      end loop;



--        --* TODO in futuro si modifica è usato solo se usato come server ?????
--        declare
--           Addr: Sock_Addr_Type;
--        begin
--
--           -- Ritorna l'indirizzo pubblico
--           Addr := Get_Socket_Name(In_Out_Socket.all);
--           Addr.Addr := Result.My_Address;
--           Addr.Port := Get_Port(In_Out_Socket);
--
--           Set_Attribute(Object     => Result.My_Info_Attribute,
--                         Address    => Addr,
--                         T_Protocol => 17,
--                         PeerID     => PeerID);
--
--           Put_Line("**** Addr: " & Image(Addr) & " *********");
--           Put_Line("**** PeerID: " & PeerID'img  & " *******");
--        end;





      --Put_Line("StreamID = 0; Start Sequence Number : " & Result.Sequence_Num(0)'img);


      Result.Input := new Reader(Commands, In_Out_Socket, Result.Peer_Manag, Result.Inter_Proc_Socket, Result.My_PeerID);
      Result.Output := new Output_Task.Writer(Output_Queue, Get_Port(Result.Inter_Proc_Socket));

      Result.Control := new Control_Task.Handler(Result.Output);

      --*** TODO *** set the right values
      Result.Transmitter.Set_Size(10); -- Max number of channels

      Result.Control.Init;
--      Result.Control.Set_ACK_Target_Attribute(Attr => Result.My_Info_Attribute);
      Result.Control.Set_Max_Trials(2);
      Result.Control.Set_Timeout(5.0);
      Result.Control.Set_Routed_Timeout(10.0);



      Result.Input.Start; -- *** non so se va qua
      Result.Input.Open; -- *** non so se va qua


      Result.Output.Start;

      Result.Main.Init (Queue       => Result.Commands,
                        Output      => Result.Transmitter,
                        Ctl         => Result.Control,
                        Prof_Type   => Result.Profile,
                        Prof_Param  => Result.Prof_Parameters,
                        Ready_Queue => Result.Ready_Queue,
                        Peer_Man    => Result.Peer_Manag);

      return Result;
   end New_Session;

   -------------
   -- Destroy --
   -------------

   procedure Destroy (What : in out Session) is
      procedure Free is
        new Ada.Unchecked_Deallocation (Object => Session_Record,
                                        Name   => Session);
      use Main_Task;
      use Control_Task;
      use Application_Packet_Queues;

      --Status : Boolean;
   begin



--      Put_Line("Destroy Output Task");
      Finalize(What.Output);


--      Put_Line("Destroy Input Task");
      Finalize (What.Input);


--      Put_Line("Destroy Control Task");
      Finalize (What.Control);



--      Put_Line("Destroy Main Task");
      Finalize (What.Main);



--      Put_Line("Close Sockets");
      Close_Input_Output_Sockets(In_Out_Socket  => What.In_Out_Socket);
                                 --Output_Socket => What.Output_Socket);
      Close_Input_Output_Sockets(In_Out_Socket  => What.Inter_Proc_Socket);
--      Free (What.Output_Socket);




--      Put_Line("Destroy Command Queue");
      Finalize (What.Commands);

--      Put_Line("Destroy Ready Queue");
      Finalize (What.Ready_Queue);

--      Put_Line("Destroy Output Queue");
      Finalize (What.Output_Queue);


      Free (What);
   end Destroy;




   --* I/O Functions

   ----------
   -- Send --
   ----------

   procedure Send (Parent   : in Session;
                   StreamID : in PPETP.Stream_ID;
                   Data     : in Stream_Element_Array) is

      Used_StreamID : Stream_ID;
   begin

      if StreamID = No_Stream_ID then
         Used_StreamID := Parent.Default_StreamID;
      else
         Used_StreamID := StreamID;
      end if;


      -- il parametro Byte_Array(Data) si dealloca da solo all'uscita della funzione?
      Parent.Transmitter.Transmit(New_Packet(Sequence_Num => Parent.Sequence_Num(Used_StreamID),
                                             StreamID     => Used_StreamID,
                                             Data         => Byte_Array(Data)));

      -- Update the Sequence_Number for the next packet
      Parent.Sequence_Num(Used_StreamID) := (Parent.Sequence_Num(Used_StreamID) +1) mod Data_Sequence_Number'Last;
   end Send;


   ----------
   -- Recv --
   ----------

   procedure Recv (Parent   : in     Session;
                   StreamID :    out PPETP.Stream_ID;
                   Data     :    out Stream_Element_Array;
                   Last     :    out Stream_Element_Offset) is
      App_Pkt : Application_Packet;

   begin
      Parent.Ready_Queue.Extract (App_Pkt);
      --      Put_line("Pkt length: " & App_Pkt.Buffer'length'img);


--      Put_Line("Session pkt'length: " & App_Pkt.Buffer'length'img);
      Last := Data'First+App_Pkt.Buffer'length-1;
  --    Put_Line("Session Data'First: "  & Data'First'img );
    --  Put_Line("Session Last : " & Last'img);
      Data(Data'First..Last) := Stream_Element_Array(App_Pkt.Buffer(App_Pkt.Buffer'range));

      StreamID := App_Pkt.StreamID;

      Finalize(App_Pkt); -- in teoria dealloca la memoria
   end Recv;





   -----------------------------------------------------------------------------
   --
   --	                     NEW FUNCTIONS
   --
   -----------------------------------------------------------------------------


   -----------------
   -- New_Session --
   -----------------
   function New_Session (Host_Port:      in Port_Type;
                         Host_IP :       in Inet_Addr_Type;  --*TODO MAYBE???
                         Internal_Port : in Port_Type;
                         PeerID :        in Peer_ID) return Session is

      Result        : Session;

      In_Out_Socket  : Socket_Access;
      Interprocess_Socket : Socket_Access;

      Success  : Boolean;
      Port     : Port_Type := Host_Port;
      Int_Port : Port_Type := Internal_Port;
      G	       : Random_Sequence_Number.Generator;
   begin



      Network.Initialize;

      -- Initialize the Sequence_Number random generator
      Random_Sequence_Number.Reset(G);

      -- Bind of the In-Out Socket
      Bind_To_Port (Port     => Port,
                    Addr     => Host_IP,
                    Input    => In_Out_Socket,
                    Success  => Success);


      if not Success then
         raise Program_Error;
      end if;


      Bind_To_Port (Port     => Int_Port,
                    Addr     => Inet_Addr("127.0.0.1"),
                    Input    => Interprocess_Socket,
                    Success  => Success);

      if not Success then
         raise Program_Error;
      end if;


      Result := new Session_Record'(Transmitter      => new Transmitters.Output_Handler,
                                    Commands         => Command_Queues.New_Queue,
                                    Output_Queue     => Network_Packet_Queues.New_Queue,
                                    Main             => new Main_Task.Handler,
                                    Control          => <>,
                                    Output	     => <>,
                                    Input            => <>,
                                    Control_Port     => Port,
                                    Profile          => <>,
                                    Prof_Parameters  => <>,
                                    Ready_Queue      => Application_Packet_Queues.New_Queue,
                                    In_Out_Socket    => In_Out_Socket,
                                    Sequence_Num     => <>,
                                    My_Address       => Host_IP,
                                    My_PeerID        => new Peer_ID'(PeerID),
                                    Peer_Manag       => new Peer_Manager.Peer_Manager_Handler,
                                    Inter_Proc_Socket => Interprocess_Socket,
                                    State            => <>,
                                    Default_StreamID => No_Stream_ID
                                   );



      Random_Sequence_Number.Reset(G);
      -- Initialize a random Sequence_Number for every Stream_ID
      for i in Stream_ID'range loop
         Result.Sequence_Num(i) := 5000;--Random_Sequence_Number.Random(G);
      end loop;

      Initialize(Result.Peer_Manag, 128);  --Max size of the Peer_Manager
      Result.Transmitter.Set_Size(16); -- Max number of channels  -- Non può essere modificata a runtime!


      -- Here the Peer_ID is a pointer, i can create the Input Task before that
      -- this field has a value (in this way the task can read non PPETP packet
      -- ie ICE packets)

      Result.Input   := new Reader(Result.Commands, In_Out_Socket, Result.Peer_Manag,
                                   Result.Inter_Proc_Socket, Result.My_PeerID);

      Result.Output  := new Output_Task.Writer(Result.Output_Queue, Get_Port(Result.Inter_Proc_Socket));

      Result.Control := new Control_Task.Handler(Result.Output);


      Result.Input.Open; --* Deve poter ricevere pacchetti ma non processare PPETP !!!
      Result.Control.Init;

      Set_ACK_Attribute(Result);

      return Result;



   end New_Session;


   --------------------------
   -- Set_Low_Level_Values --
   --------------------------
   procedure Set_Low_Level_Values(Parent :        in Session;
                                  Max_Trials:     in Natural;
                                  Timeout:        in Duration;
                                  Routed_Timeout: in Duration;
                                  Max_Sup:        in Natural := 100;
                                  Re_ACK:         in Natural := 20;
                                  Max_Oldest:     in Natural := 50_000;
                                  Max_ACK:        in Natural := 8) is
   begin


      Parent.Control.Set_Max_Trials(Max_Trials);
      Parent.Control.Set_Timeout(Timeout);
      Parent.Control.Set_Routed_Timeout(Routed_Timeout);

      Parent.Main.Set_Low_Level_Values(Max_Sup    => Max_Sup,
                                       Re_ACK     => Re_ACK,
                                       Max_Oldest => Max_Oldest,
                                       Max_ACK    => Max_ACK);

      Parent.State.Low_Level := True;

      Set_Ready_Flag(Parent.State);

   end Set_Low_Level_Values;


   ------------------
   -- Set_New_Port --
   ------------------
   procedure Set_New_Port(Parent: in Session;
                          Port:   in Port_Type) is
      Res: Boolean;
   begin

      Put_Line("Session: Set_New_Port");


      Parent.Input.Stop_Read;

      Close_Socket(Parent.In_Out_Socket.all);
      Free(Parent.In_Out_Socket);

      Bind_To_Port (Port     => Port,
                    Addr     => Parent.My_Address,
                    Input    => Parent.In_Out_Socket,
                    Success  => Res);

      Parent.Input.Change_In_Out_Socket(Parent.In_Out_Socket);

      Parent.Input.Start_Read;


      Parent.Control_Port := Port;


      Set_ACK_Attribute(Parent);


   end Set_New_Port;

   ---------------------
   -- Set_New_Address --
   ---------------------
   procedure Set_New_Address(Parent: in Session;
                             Addr:   in Inet_Addr_Type) is
      Res: Boolean;
   begin


      Put_Line("Session: Set_New_Address");


      Parent.Input.Stop_Read;

      Close_Socket(Parent.In_Out_Socket.all);
      Free(Parent.In_Out_Socket);

      Bind_To_Port (Port     => Parent.Control_Port,
                    Addr     => Addr,
                    Input    => Parent.In_Out_Socket,
                    Success  => Res);


      Parent.Input.Change_In_Out_Socket(Parent.In_Out_Socket);

      Parent.Input.Start_Read;


      Parent.My_Address := Addr;

      Set_ACK_Attribute(Parent);

   end Set_New_Address;


   ----------------
   -- Set_PeerID --
   ----------------
   procedure Set_PeerID(Parent: in Session;
                        PeerID: in Peer_ID) is
   begin

      Free(Parent.My_PeerID);
      Parent.My_PeerID := new Peer_ID'(PeerID);

      Set_ACK_Attribute(Parent);

   end Set_PeerID;


   -------------------
   -- Start_Session --
   -------------------
   procedure Start_Session(Parent : Session) is
   begin


      Parent.Input.Start;

      Parent.Main.Init (Queue       => Parent.Commands,
                        Output      => Parent.Transmitter,
                        Ctl         => Parent.Control,
                        Prof_Type   => Parent.Profile,
                        Prof_Param  => Parent.Prof_Parameters,
                        Ready_Queue => Parent.Ready_Queue,
                        Peer_Man    => Parent.Peer_Manag);

      Parent.Output.Start;

   end Start_Session;



end PPETP.Sessions;

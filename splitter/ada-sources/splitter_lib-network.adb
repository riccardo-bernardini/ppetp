--
with Ada.Unchecked_Deallocation;
with Ada.Text_Io;
with Ada.Exceptions;
with Ada.Characters.Latin_1;
with Ada.Unchecked_Conversion;
with Ada.Strings.Unbounded;                  use Ada.Strings.Unbounded;
with Ada.Numerics.Discrete_Random;
with Ada.Calendar.Formatting;
use Ada.Calendar.Formatting;

with Splitter_Lib.BSD_Sockets;
with Splitter_Lib.Network.Self_Address;

with Ada.Exceptions;	use Ada.Exceptions;

package body Splitter_Lib.Network is
   use BSD_Sockets;

   My_Addresses       : array (0 .. 128) of BSD_Sockets.Inet_Addr_Type;
   Last_Address_Index : Natural := 0;


   ----------
   -- Data --
   ----------

   function Data (X : Packet_Buffer) return Stream_Element_Array is
   begin
      return X.Data(X.Data'First .. X.Data'First + X.Size - 1);
   end Data;

   ---------------------
   -- Self_IP_Address --
   ---------------------

   function Self_IP_Address (Which : Natural := 1)
                           return  BSD_Sockets.Inet_Addr_Type
   is
   begin
      if Which > Last_Address_Index then
         raise Constraint_Error;
      end if;

      return My_Addresses(Which);
   end Self_IP_Address;


   -------------------
   -- Free_UDP_Port --
   -------------------

   function Free_UDP_Port (N_Trials : Natural := 1024)
                           return Natural
   is
      -- Range of port numbers that are granted to be non-reserved
      subtype Available_Port is Natural range 49_152 .. 61_000;

      package Random_Port is
        new Ada.Numerics.Discrete_Random (Available_Port);

      function Is_Free (Port : Available_Port)
                        return Boolean
      is
         Socket : Socket_Type;
         Result : Boolean;
         Addr   : Sock_Addr_Type := (Family => Family_Inet,
                                     Addr   => Localhost,
                                     Port   => Port_Type (Port));
      begin
         Result := True;
         Create_Socket (Socket => Socket,
                        Family => Family_Inet,
                        Mode   => Socket_Datagram);

         begin
            Bind_Socket (Socket, Addr);
         exception
            when Socket_Error =>
               Result := False;
         end;

         Close_Socket (Socket);
         return Result;
      end Is_Free;


      Gen      : Random_Port.Generator;
      Result   : Natural;
   begin
      Random_Port.Reset (Gen);
      for I in 1 .. N_Trials loop
         Result := Random_Port.Random (Gen);

         if (Result mod 2) = 1 then  -- use only even port for rtp
            null;
         else
            if Is_Free (Result) then
               return Result;
            end if;
         end if;

      end loop;

      raise Port_Unavailable;
   end Free_UDP_Port;

   ---------------
   -- UDP_Input --
   ---------------

   function UDP_Input
     (Host : BSD_Sockets.Inet_Addr_Type;
      Port : Natural)
      return Input_UDP_Socket
   is
      Result : Input_UDP_Socket;
      Addr   : Sock_Addr_Type := (Family => Family_Inet,
                                  Addr   => Host,
                                  Port   => Port_Type(Port));
   begin
      Create_Socket (Socket => Result.Socket,
                     Family => Family_Inet,
                     Mode   => Socket_Datagram);

      Bind_Socket (Result.socket, Addr);

      Result.Selector := new Selector_Type;
      Create_Selector (Result.Selector.all);

      return Result;
   end UDP_Input;

   --------------
   -- UDP_List --
   --------------

   function UDP_List
     return UDP_Socket_List is
      Result : UDP_Socket_List;
   begin
      Result := (Sockets    => <>,
                 First_Free => Socket_Array'First,
                 Selector   => new Selector_Type);

      Create_Selector (Result.Selector.all);
      return Result;
   end UDP_List;

   ------------
   -- Append --
   ------------

   procedure Append (List : in out UDP_Socket_List;
                     Host : in     Inet_Addr_Type;
                     Port : in     Natural;
                     Src  :    out Natural) is
      Addr   : Sock_Addr_Type := (Family => Family_Inet,
                                  Addr   => Host,
                                  Port   => Port_Type (Port));
      Sock : BSD_Sockets.Socket_Type;
   begin
      if List.First_Free = List.Sockets'Last + 1 then
         raise Constraint_Error;
      end if;

      Create_Socket (Socket => Sock,
                     Family => Family_Inet,
                     Mode   => Socket_Datagram);

      Bind_Socket (Sock, Addr);
      Src := List.First_Free;
      List.Sockets (List.First_Free) := Sock;
      List.First_Free := List.First_Free + 1;

      ada.Text_IO.put_line ("append: bound " & image(addr) & " " & "src=" & src'img);
   end Append;

   ----------------
   -- UDP_Output --
   ----------------

   function UDP_Output
     (Host : BSD_Sockets.Inet_Addr_Type;
      Port : Natural)
      return Output_UDP_Socket
   is
      Result : Socket_Type;
      Addr   : Sock_Addr_Type := (Family => Family_Inet,
                                  Addr   => Host,
                                  Port   => Port_Type(Port));
   begin


      Create_Socket (Socket => Result,
                     Family => Family_Inet,
                     Mode   => Socket_Datagram);

      Connect_Socket (Result, Addr);

      return Output_UDP_Socket(Result);
   end UDP_Output;

   -----------------------
   -- Read_With_Timeout --
   -----------------------

   procedure Read_With_Timeout (From    : in out UDP_Socket_List;
                                To      :    out Packet_Buffer;
                                Source  :    out Natural;
                                Timeout : in     Calendar.Time) is
      use type Ada.Calendar.Time;
      use Ada.Exceptions;

      R_Sock_Set : Socket_Set_Type;
      W_Sock_Set : Socket_Set_Type;
      Selector   : Selector_Type;
      Status     : Selector_Status;
      Peer_Addr  : Sock_Addr_Type;
      Last       : Stream_Element_Offset;
   begin
      -- Ada.Text_IO.Put_Line ("timeout : " & Image (Timeout));
      -- Ada.Text_IO.Put_Line ("now     : " & Image (Calendar.Clock));

      To.Size := 0;

      if (Timeout <= Calendar.Clock) then
         Source := 0;
         return;
      end if;


      --Put_Line ("read T=" & Duration'Image (Timeout));

      Empty (R_Sock_Set);
      Empty (W_Sock_Set);

      for I in From.Sockets'First .. From.First_Free - 1 loop
         --Ada.Text_IO.Put_Line ("add socket #" & Integer'Image (I)
         --                     & ":" & Image (Get_Socket_Name(From.Sockets(I))));
         Set (R_Sock_Set, From.Sockets (I));
      end loop;

      -- Ada.Text_IO.Put_Line ("check in");
      Check_Selector (From.Selector.all, R_Sock_Set, W_Sock_Set,
                      Status,  Timeout - Calendar.Clock);
      -- Put_Line ("check out");

      case Status is
         when Completed =>
            for I in From.Sockets'First .. From.First_Free - 1 loop
               if Is_Set (R_Sock_Set, From.Sockets (I)) then
                  Source := I;
                  exit;
               end if;
            end loop;

            pragma Assert (Is_Set (R_Sock_Set, From.Sockets (Source)));


            -- Ada.Text_IO.Put_Line ("completo");
            Receive_Socket (Socket => From.Sockets (Source),
                            From   => Peer_Addr,
                            Item   => To.Data,
                            Last   => Last);

            To.Size := Last - To.Data'First + 1;
         when Expired | Aborted =>
            null;
      end case;

      empty(R_Sock_Set);
      empty(W_Sock_Set);
   exception
      when e: others =>
         Text_Io.Put_Line ("timeout_socket: " & Exception_Information (e));
   end Read_With_Timeout;

   -----------
   -- Close --
   -----------

   procedure Close (S : in out Input_UDP_Socket) is
      procedure Free is
        new Ada.Unchecked_Deallocation (Object => Selector_Type,
                                        Name   => Selector_Access);
   begin
      Close_Socket (S.Socket);
      Close_Selector (S.Selector.all);
      Free(S.Selector);
   end;

   -----------
   -- Close --
   -----------

   procedure Close (S : in out UDP_Socket_List) is
      procedure Free is
        new Ada.Unchecked_Deallocation (Object => Selector_Type,
                                        Name   => Selector_Access);
   begin
      for I in S.Sockets'First .. S.First_Free - 1 loop
         Close_Socket (S.Sockets (I));
      end loop;

      S.First_Free := S.Sockets'First;

      Close_Selector (S.Selector.all);
      Free(S.Selector);
   end Close;

   --------------------
   -- New_TCP_Stream --
   --------------------

   function New_TCP_Stream (Host : BSD_Sockets.Inet_Addr_Type;
                            Port : Natural)
                            return TCP_Stream
   is
      Result : Socket_Type;
      Addr   : Sock_Addr_Type := (Family => Family_Inet,
                                  Addr   => Host,
                                  Port   => Port_Type(Port));
   begin
      Create_Socket (Socket => Result,
                     Family => Family_Inet,
                     Mode   => Socket_Stream);

      -- Text_Io.Put_Line ("connect =>" & Image (Addr));

      Connect_Socket (Result, Addr);

      return TCP_Stream(Result);
   end New_TCP_Stream;


   -----------
   -- Close --
   -----------

   procedure Close (S : Output_UDP_Socket) is
   begin
      Close_Socket (BSD_Sockets.Socket_Type (S));
   end;

   ----------
   -- Send --
   ----------

   procedure Send (What : Packet_Buffer;
                   Dst  : Output_UDP_Socket) is
      First : Stream_Element_Offset := What.data'First;
      Last  : Stream_Element_Offset := First + What.Size - 1;
      Sent  : Stream_Element_Offset;
   begin

     -- Ada.Text_Io.Put_Line("Send: First:Last" & First'img & ".." & Last'img );

      Send_Socket (Socket => BSD_Sockets.Socket_Type (Dst),
                   Item   => What.Data (First .. Last),
                   Last   => Sent);
   exception
      when e: others =>
         null;
         --Ada.Text_Io.Put_Line(Exception_Information(e));
   end Send;


   --------------
   -- Is_Empty --
   --------------

   function Is_Empty
     (Packet : Packet_Buffer)
      return Boolean
   is
   begin
      return Packet.Size = 0;
   end Is_Empty;

   -----------------------
   -- Read_With_Timeout --
   -----------------------

   procedure Read_With_Timeout
     (From    : in out Input_UDP_Socket;
      To      :    out Packet_Buffer;
      Timeout : in     Calendar.Time)
   is
      use type Ada.Calendar.Time;
      use Ada.Exceptions;

      R_Sock_Set : Socket_Set_Type;
      W_Sock_Set : Socket_Set_Type;
      Selector   : Selector_Type;
      Status     : Selector_Status;
      Peer_Addr  : Sock_Addr_Type;
      Last       : Stream_Element_Offset;
   begin
      To.Size := 0;

      if (Timeout <= Calendar.Clock) then
	return;
      end if;


      --Put_Line ("read T=" & Duration'Image (Timeout));

      Empty (R_Sock_Set);
      Empty (W_Sock_Set);
      Set (R_Sock_Set, From.Socket);

      -- Put_Line ("check in");
      Check_Selector (From.Selector.all, R_Sock_Set, W_Sock_Set,
                      Status,  Timeout - Calendar.Clock);
      -- Put_Line ("check out");

      case Status is
         when Completed =>
            -- Put_Line ("completo");
            Receive_Socket (Socket => From.Socket,
                            From   => Peer_Addr,
                            Item   => To.Data,
                            Last   => Last);

            To.Size := Last - To.Data'First + 1;
         when Expired | Aborted =>
            null;
      end case;

      empty(R_Sock_Set);
      empty(W_Sock_Set);
   exception
      when e: others =>
         Text_Io.Put_Line ("timeout_socket: " & Exception_Information (e));
   end Read_With_Timeout;


   ---------------
   -- Send_Line --
   ---------------

   procedure Send_Line (To   : TCP_Stream;
                        Line : String)
   is
      use Ada, Ada.Streams;

      CRLF : constant String := Characters.Latin_1.CR & Characters.Latin_1.LF;

      function To_Array (S : String)
                         return Stream_Element_Array is
      subtype Source is String (S'Range);
      subtype Result is Stream_Element_Array
           (Stream_Element_Offset (S'First) .. Stream_Element_Offset (S'Last));
      function To_A is new Ada.Unchecked_Conversion (Source, Result);
      begin
         return To_A (S);
      end To_Array;

      Last_Sent : Stream_Element_Offset;
   begin
      Send_Socket (Socket => Socket_Type (To),
                   Item   => To_Array (Line & CRLF),
                   Last   => Last_Sent);
   end Send_Line;

   --------------
   -- Get_Char --
   --------------

   function Get_Char (From : TCP_Stream)
                      return Character
   is
      function To_Char is
        new Ada.Unchecked_Conversion (Source => Stream_Element,
                                      Target => Character);

      Buffer : Stream_Element_Array (1 .. 1);
      Last   : Stream_Element_Offset;
   begin
      Receive_Socket (Socket => Socket_Type (From),
                      Item   => Buffer,
                      Last   => Last);

      if (Last = Buffer'First - 1) then
         raise IO_Exceptions.End_Error;
      else
         Text_Io.Put_Line("get_char:" & To_Char(Buffer(Buffer'First)));
         return To_Char(Buffer(Buffer'First));
      end if;
   end Get_Char;


   --------------
   -- Get_Line --
   --------------

   function Get_Line (From : TCP_Stream)
                      return String
   is
      Buffer : Unbounded_String := Null_Unbounded_String;
      Ch : Character;
   begin
      loop
         Ch := Get_Char (From);
         case Ch is
            when Characters.Latin_1.CR =>
               Ch := Get_Char (From);
               exit when Ch = Characters.Latin_1.LF;
               Buffer := Buffer & Characters.Latin_1.CR & Ch;
            when Characters.Latin_1.LF =>
               exit;
            when others =>
               Buffer := Buffer & Ch;
         end case;
      end loop;

      return To_String(Buffer);
   end Get_Line;

   function Size (X : Packet_Buffer) return Packet_Size
   is
   begin
      return X.Size;
   end Size;

   function To_Packet (X : Stream_Element_Array)
                       return Packet_Buffer
   is
      Result : Packet_Buffer;
      From   : Stream_Element_Offset := Result.Data'First;
      To     : Stream_Element_Offset := From + X'Length - 1;
   begin
      Result.Data (From .. To) := X;
      Result.Size := X'Length;
      return Result;
   end To_Packet;

   function Image (X : Packet_Buffer)
                   return String
   is
      Result : Unbounded_String := Null_Unbounded_String;
   begin
      Result := Result
        & "size: "
        & Stream_Element_Offset'Image (X.Size)
        & "[";

      for I in 0 .. X.Size - 1 loop
         Result := Result
           & Stream_Element'Image (X.Data (X.Data'First + I));
         if I /= X.Size - 1 then
            Result := Result & ", ";
         end if;
      end loop;

      Result := Result & "]";

      return To_String(Result);
   end Image;
begin
   My_Addresses (0) := Localhost;
   My_Addresses (1) := Self_Address.My_Address;
   Last_Address_Index := 1;
end Splitter_Lib.Network;

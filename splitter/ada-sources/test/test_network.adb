--
with Splitter_Lib.Network;    use Splitter_Lib.Network;
with Text_Io;                 use Text_Io;
with Test_Report;             use Test_Report;
with Ada.Streams;             use Ada.Streams;
with Ada.Calendar;            use Ada.Calendar;

procedure Test_Network is
   Server_Port : Natural := 4242;

   procedure Test_Tcp_Stream (Reporter : in out Reporter_Type) is
      T_Stream : TCP_Stream;
   begin
      T_Stream := New_TCP_Stream (Localhost, Server_Port);
      Send_Line (T_Stream, "3 4");
      declare
         Reply : String := Get_Line (T_Stream);
      begin
         New_Result(Reporter, Reply = "7");
      end;

      -- Close(T_Stream);
   end Test_Tcp_Stream;

   procedure UDP_Test (Reporter : in out Reporter_Type) is
      T_Stream : TCP_Stream;
      Input    : Input_UDP_Socket;
      Output   : Output_UDP_Socket;
      In_Port  : Natural;
      Out_Port : Natural;

      Value    : Stream_Element_Array  := (1 => 42, 2 => 33);
      Packet   : Packet_Buffer;
   begin
      In_Port := Free_UDP_Port;
      Input   := UDP_Input (Localhost, In_Port);

      Out_Port := Free_UDP_Port;
      Output   := UDP_Output (Localhost, Out_Port);

      T_Stream := New_TCP_Stream (Localhost, Server_Port);
      Send_Line (T_Stream, Integer'Image (Out_Port)
                 & " " & Integer'Image (In_Port));

      declare
         Reply : String := Get_Line (T_Stream);
         Tmp   : Integer := Integer'Value(Reply);
      begin
         New_Result(Reporter, Tmp = 2*Out_Port);
      end;

      delay 0.5;

      Send (To_Packet (Value), Output);
      Read_With_Timeout (From    => Input,
                         To      => Packet,
                         Timeout => Clock + 100.0);

      Put_Line ("read: " & Image (Packet));

      if (Size (Packet) /= Value'Length) then
         Failure (Reporter);
      else
         declare
            Result : Stream_Element_Array (1 .. 2) := Data (Packet);
         begin
            if Result (1) = Value (2) and
              Result (2) = Value (1) then
               Success (Reporter);
            else
               Failure (Reporter);
            end if;

            New_Result (Reporter, not Is_Empty(Packet));
         end;
      end if;

      Read_With_Timeout (From    => Input,
                         To      => Packet,
                         Timeout => Clock - 1.0);

      New_Result (Reporter, Is_Empty (Packet));
      -- Close (T_Stream);
      Close (Input);
      Close (Output);
   end UDP_Test;

   Reporter : Reporter_Type;
begin
   Set_Output (Reporter, Standard_Output);
   New_Suite (Reporter);
   Test_Tcp_Stream (Reporter);
   New_Suite (Reporter);
   UDP_Test (Reporter);
   Final (Reporter);
end Test_Network;

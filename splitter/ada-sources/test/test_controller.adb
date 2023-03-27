--
with Ada.Command_Line;                        use Ada.Command_Line;
with Ada.Text_Io;                             use Ada.Text_Io;
with Test_Report;                             use Test_Report;
with Splitter_Lib.Controller;                 use Splitter_Lib.Controller;
with Splitter_Lib.Players;                    use Splitter_Lib.Players;

procedure Test_Controller is
   ---------------------
   -- Check_Not_Found --
   ---------------------

   procedure Check_Use_Port (Reporter    : in out Reporter_Type;
                             Server_Port : in     Natural) is
      function Try_Port (Port : Natural) return Boolean is
      begin
         Use_Port (Port);
         return True;
      exception
         when Server_Not_Found =>
            return False;
      end;
      Ok : Boolean;
   begin
      New_Suite (Reporter);

      declare
         Ignored : Player_Descriptor;
      begin
         Ok := False;
         Ignored := Start_Player (3425);
      exception
         when Program_Error =>
            Ok := True;
      end;

      New_Result (Reporter, Ok);

      declare
         Ignored : Boolean;
      begin
         Ok := False;
         Ignored := Kill_Player (1);
      exception
         when Program_Error =>
            Ok := True;
      end;

      New_Result (Reporter, Ok);

      New_Result (Reporter, not Try_Port (Server_Port + 3));
      New_Result (Reporter, Try_Port (Server_Port));

      begin
         Ok := False;
         Use_Port (Server_Port);
      exception
         when Program_Error =>
            Ok := True;
      end;
      New_Result (Reporter, Ok);
   end Check_Use_Port;

   Reporter    : Reporter_Type;
   Server_Port : Natural;
   Descr : Player_Descriptor;
   Ok    : Boolean;
begin
   if Argument_Count < 1 then
      Put_Line (Standard_Error, "Usage: test_controller <port>");
      Set_Exit_Status (Ada.Command_Line.Failure);
      return;
   end if;

   Server_Port := Natural'Value(Argument(1));

   Set_Output(Reporter, Standard_Output);

   Check_Use_Port(Reporter, Server_Port);
   Descr := Start_Player (3245);
   Ok := Kill_Player(1);

   Final(Reporter);
end Test_Controller;

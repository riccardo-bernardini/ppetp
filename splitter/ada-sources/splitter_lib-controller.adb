--
with Splitter_Lib.Controller.Talker;
with Ada.Strings.Unbounded;

package body Splitter_Lib.Controller is
   Destination_Defined : Boolean := False;

   -----------------
   -- Kill_Player --
   -----------------

   function Kill_Player (ID : Players.Player_ID) return Boolean is
   begin
      if (not Destination_Defined) then
         raise Program_Error;
      end if;

      declare
         Reply : Talker.Reply :=
                   Talker.Send_Command ("kill", Natural (ID));
      begin
         return Talker.Ok (Reply);
      end;
   end Kill_Player;

   ------------------
   -- Start_Player --
   ------------------

   function Start_Player
     (Player_Port : Natural)
      return Players.Player_Descriptor
   is
      use Ada.Strings.Unbounded;
   begin
      if (not Destination_Defined) then
         raise Program_Error;
      end if;

      declare
         Reply : Talker.Reply :=
                   Talker.Send_Command ("start", Natural (Player_Port));
      begin
         if not Talker.Ok (Reply) then
            return Players.No_Player;
         elsif Reply.Words'Length < 1 then
            raise Program_Error;
         else
            return (ID   => Players.Player_ID'Value (To_String (Reply.Words (1))),
                    Port => Player_Port);
         end if;
      end;
   end Start_Player;

   --------------
   -- Use_Port --
   --------------

   procedure Use_Port (Port : Natural) is
   begin
      if (Destination_Defined) then
         raise Program_Error;
      end if;

      Talker.Use_Port (Port);
      Destination_Defined := True;
   end Use_Port;

end Splitter_Lib.Controller;

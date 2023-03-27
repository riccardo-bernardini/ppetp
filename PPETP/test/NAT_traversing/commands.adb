package body Commands is

   -- Functions for the type Command_Type_Set

   function "or"(L,R: Command_Type_Set) return Command_Type_Set is
   begin
      return Command_Type_Set( Unsigned_8(L) or Unsigned_8(R) );
   end;

   function "and"(L,R: Command_Type_Set) return Command_Type_Set is
   begin
      return Command_Type_Set( Unsigned_8(L) and Unsigned_8(R) );
   end;

   function "xor"(L,R: Command_Type_Set) return Command_Type_Set is
   begin
      return Command_Type_Set( Unsigned_8(L) xor Unsigned_8(R) );
   end;


   function In_Set(Element: in Command_Type_Set;
                   Set: in Command_Type_Set) return boolean is
   begin
      if (Unsigned_8(Element) and Unsigned_8(Set)) = Unsigned_8(Element) then
         return true;
      else
         return false;
      end if;

   end In_Set;


   function Value(Str: String) return Command_Type_Set is
   begin
      if Str = "PLAY" then
         return PLAY;
      elsif Str = "HELO" then
         return HELO;
      elsif Str = "ACK" then
         return ACK;
      elsif Str = "PUNCH" then
         return PUNCH;
      elsif Str = "UNKNOWN" then
         return UNKNOWN;
      else
         raise CONSTRAINT_ERROR;
      end if;

   end Value;


   -- Commands procedures

   procedure Send_Command(To: in Sock_Addr_Type;
                          Command: in Command_Type_Set;
                          Parameters: in Unbounded_String := NO_PARAMETERS;
                          From: in Sock_Addr_type) is
   begin


      case Command is

         when PLAY =>
            -- PLAY have not parameters to check
            if Parameters /= NO_PARAMETERS then
               raise command_format_error;
            end if;

            Send_String(To, To_Unbounded_String("PLAY"), From);

         when HELO =>
            -- HELO could have 1 or 0 parameters
            declare
               Msg: Unbounded_String;
            begin
               Msg := To_Unbounded_String("HELO ");

               if Parameters /= NO_PARAMETERS then
                  Msg := Msg & Parameters;
               end if;

               Send_String(To, Msg, From);
            end;

         when ACK =>
            -- ACK have not parameters to check
            if Parameters /= NO_PARAMETERS then
               raise command_format_error;
            end if;

            Send_String(To, To_Unbounded_String("ACK"), From);

         when PUNCH =>
            -- PUNCH must have 1 parameters
            if Parameters = NO_PARAMETERS then
               raise command_format_error;
            end if;

            declare
               msg: Unbounded_String;
            begin
               msg := "PUNCH " & Parameters;
               Send_String(To, Msg, From);
            end;

         when UNKNOWN =>
            -- Send a message, not a command
            Send_String(To, Parameters, From);

         when others =>
            raise command_not_yet_implemented;

      end case;

   end Send_Command;



   procedure Wait_For_Command(Listen_Addr: in Sock_Addr_Type;
                              Commands: in Command_Type_Set;
                              Parameters: in out Unbounded_String;
                              From: in out Sock_Addr_Type) is
      Msg: Unbounded_String;
      Sender_Addr: Sock_Addr_Type;
      Command_Received: Command_Type_Set;
      Msg_List: Token_List;
      Received_Parameters: Unbounded_String;
   begin

      -- Wait for a message
      Receive_String(Listen_Addr, Msg, Sender_Addr);

      -- Check the sender of the message
      if From /= No_Sock_Addr then
         if Sender_Addr /= From then
            raise command_wrong_sender;
         end if;
      end if;


      -- Retrive the message type
      Msg_List := Split(To_String(Msg));


      begin
         Command_Received := Value( To_String(Element(Msg_List,1)));
      exception

            -- If this exception append, means that an unknown command has been
            -- received
         when CONSTRAINT_ERROR =>
            Command_Received := UNKNOWN;
      end;

      Put("#");
      Put(To_String(Element(Msg_List,1)));
      Put("#");
      Put_Line(Command_Type_Set'Image(Command_Received));

      -- Check if the waited command is arrived
      if (not In_Set(Command_Received, Commands) ) then
         raise command_type_not_match;
      end if;

      -- If the received command is different from "Commands" parameter and is
      -- not UNKNOWN, than it means that more than a command is specified in the
      -- "Commands" parameter; for have no check and return the entire message,
      -- set as UNKNOWN command
      if ( Command_Received /= UNKNOWN and Command_Received /= Commands) then
         Command_Received := UNKNOWN;
      end if;



      -- *** TODO ****
      -- In this version check only the number of parameters, not the exact
      -- format

      -- Check the parameters
      case Command_Received is

         when PLAY =>
            -- PLAY have not parameters to check
            if Length(Msg_List) > 1 then
               raise command_format_error;
            end if;


         when HELO =>
            -- HELO could have 1 or 0 parameters
            null;

         when ACK =>
            -- ACK have not parameters to check
            if Length(Msg_List) > 1 then
               raise command_format_error;
            end if;

         when PUNCH =>
            -- PUNCH must have 1 parameters
            if Length(Msg_List) /= 1 then
               raise command_format_error;
            end if;


         when UNKNOWN =>
            null;

         when others =>
            null;

      end case;

      -- Set the output
      From := Sender_Addr;

      -- reconstruct the parameter string without the command; if the command
      -- is UNKNOWN, there is no command to skip
      declare
         Start: integer;
      begin
         if Command_Received = UNKNOWN then
            Start := 1;
         else
            Start := 2;
         end if;

         for i in integer range Start .. Length(Msg_List) loop
            Received_Parameters := Received_Parameters & " ";
            Received_Parameters := Received_Parameters &  Element(Msg_List,i);
         end loop;

      end;

      Parameters := Received_Parameters;

   end Wait_For_Command;


end Commands;

--
with Splitter_Lib.Network;
with Ada.Strings.Unbounded;                   use Ada.Strings.Unbounded;

package body Splitter_Lib.Controller.Talker is
   Server : Network.TCP_Stream;
   No_Error : constant Natural := 0;

   function Parse_Reply (S : String)
                         return Reply

   is
      function To_S (X : Unbounded_String) return String
                     renames To_String;

      function To_U (X : String) return Unbounded_String
                     renames To_Unbounded_String;

      -- The number of words in S cannot be larger than its
      -- length
      Buffer : Word_Array (1 .. S'Length);

      -- Force the last char to "space" (this makes the parsing
      -- simpler
      To_Be_Parsed : String := S & ' ';

      Word_Start : Natural;
      Cursor     : Natural;
      Word_Count : Natural;
   begin
      Cursor := To_Be_Parsed'First;
      Word_Count := 0;
      while Cursor <= To_Be_Parsed'Last loop
         while Cursor <= To_Be_Parsed'Last
           and then To_Be_Parsed (Cursor) = ' ' loop
            Cursor := Cursor + 1;
         end loop;

         exit when Cursor > To_Be_Parsed'Last;

         Word_Start := Cursor;
         -- We can avoid to test if Cursor is <= than To_Be_Parsed'Last
         -- since we forced the last character to be a space
         while To_Be_Parsed (Cursor) /= ' ' loop
            Cursor := Cursor + 1;
         end loop;

         -- # Cursor >= Word_Start + 1
         -- This happens since To_Be_Parsed(Word_Start) /= ' ' and
         -- To_Be_Parsed(Cursor) = ' '
         Word_Count := Word_Count + 1;
         Buffer(Word_Count) := To_U(To_Be_Parsed(Word_Start .. Cursor-1));
      end loop;

      return Reply'(N_Words    => Word_Count - 1,
                    Error_Code => Natural'Value (To_S(Buffer (1))),
                    Words      => Buffer (2 .. Word_Count));
   end Parse_Reply;

   ------------------
   -- Send_Command --
   ------------------

   procedure Send_Command
     (Command   : String;
      Parameter : Natural)
   is
      Ignored : Reply := Send_Command(Command, Parameter);
   begin
      null;
   end Send_Command;

   ------------------
   -- Send_Command --
   ------------------

   function Send_Command
     (Command   : String;
      Parameter : Natural)
      return Reply
   is
      To_Be_Sent : String := Command & ' ' & Natural'Image(Parameter);
   begin
      Network.Send_Line (Server, To_Be_Sent);
      return Parse_Reply(Network.Get_Line(Server));
   end Send_Command;

   --------
   -- Ok --
   --------

   function Ok
     (X : Reply)
      return Boolean
   is
   begin
      return X.Error_Code = No_Error;
   end Ok;

   --------------
   -- Use_Port --
   --------------

   procedure Use_Port (Port : Natural) is
   begin
      Server := Network.New_TCP_Stream (Network.Localhost, Port);
   exception
      when Network.Socket_Error =>
         raise Server_Not_Found;
   end Use_Port;

end Splitter_Lib.Controller.Talker;
